source(here::here("config.R"))

# %%
wrap_for_mitml <- function(fit) {
  fe <- fixef(fit)
  nms <- c(names(fe$cond), stringr::str_c("zi.", names(fe$zi)))

  if (any(duplicated(nms))) {
    nms <- make.unique(nms, sep = "_dup")
    warning("Duplicate parameter names; made unique.", call. = FALSE)
  }

  coefs <- setNames(c(fe$cond, fe$zi), nms)

  vc <- vcov(fit)
  mat <- as.matrix(Matrix::bdiag(vc$cond, vc$zi))
  rownames(mat) <- colnames(mat) <- nms

  stopifnot(length(coefs) == nrow(mat))

  structure(list(coefs = coefs, vcovs = mat), class = "wrapped_glmmTMB")
}

coef.wrapped_glmmTMB <- function(object, ...) object$coefs
vcov.wrapped_glmmTMB <- function(object, ...) object$vcovs

# %%
compare_models <- function(m_large, m_small, names = c("larger", "smaller")) {
  common_idx <- intersect(m_large$good_idx, m_small$good_idx)

  if (length(common_idx) < 2L) {
    stop(
      stringr::str_glue(
        "Fewer than 2 common imputations (large: {toString(m_large$good_idx)}, ",
        "small: {toString(m_small$good_idx)}); cannot compare."
      ),
      call. = FALSE
    )
  }

  if (
    length(common_idx) < length(m_large$good_idx) ||
      length(common_idx) < length(m_small$good_idx)
  ) {
    warning(
      stringr::str_glue(
        "Using {length(common_idx)} common imputations ",
        "(large had {length(m_large$good_idx)}, small had {length(m_small$good_idx)})."
      ),
      call. = FALSE
    )
  }

  pos_large <- match(common_idx, m_large$good_idx)
  fits_large <- purrr::map(m_large$good[pos_large], \(r) wrap_for_mitml(r$fit))

  added_terms <- setdiff(m_large$pooled$term, m_small$pooled$term)
  if (length(added_terms) == 0L) {
    stop(
      "No additional terms found in larger model. Check formula ordering.",
      call. = FALSE
    )
  }

  all_coef_names <- names(fits_large[[1]]$coefs)
  actual_diff <- purrr::map_chr(added_terms, \(term) {
    if (term %in% all_coef_names) {
      term
    } else if (stringr::str_c("zi.", term) %in% all_coef_names) {
      stringr::str_c("zi.", term)
    } else {
      NA_character_
    }
  }) %>%
    purrr::discard(is.na)

  cat(stringr::str_glue(
    "Nested Model Comparison: {names[1]} vs {names[2]}\n",
    "Additional terms: {toString(added_terms)}\n",
    "D1 parameters: {toString(actual_diff)} (n = {length(actual_diff)})\n\n"
  ))

  if (length(actual_diff) == 0L) {
    warning(
      "No matching parameters found; D1 test will be degenerate.",
      call. = FALSE
    )
  }

  mitml_result <- tryCatch(
    mitml::testConstraints(
      fits_large,
      constraints = actual_diff,
      method = "D1"
    ),
    error = \(e) {
      message(
        "D1 failed, falling back to per-parameter Wald: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  fallback_result <- NULL

  if (!is.null(mitml_result)) {
    print(mitml_result)
  } else {
    cat("FALLBACK: per-parameter Rubin t-tests (not a joint test)\n\n")

    rows <- dplyr::filter(m_large$pooled, term %in% added_terms)
    fallback_result <- dplyr::select(
      rows,
      component,
      term,
      estimate,
      std.error,
      statistic,
      df,
      p.value
    )
    print(fallback_result, row.names = FALSE)

    if (all(is.finite(rows$std.error)) && all(rows$std.error > 0)) {
      chi2 <- sum((rows$estimate / rows$std.error)^2)
      df_joint <- nrow(rows)
      cat(stringr::str_glue(
        "\nApprox joint Wald chi2 = {round(chi2, 3)}, df = {df_joint}, ",
        "p = {signif(pchisq(chi2, df_joint, lower.tail = FALSE), 4)} ",
        "(diagonal approx only)\n"
      ))
    } else {
      cat("\nCannot compute approximate joint Wald (non-finite SEs).\n")
    }
  }

  delta_AIC <- m_large$fit_stats$heuristic_AIC - m_small$fit_stats$heuristic_AIC
  delta_BIC <- m_large$fit_stats$heuristic_BIC - m_small$fit_stats$heuristic_BIC

  cat(stringr::str_glue(
    "\ndelta AIC = {round(delta_AIC, 2)}, delta BIC = {round(delta_BIC, 2)} ",
    "(negative favours {names[1]})\n"
  ))

  invisible(list(
    added_terms = added_terms,
    actual_diff = actual_diff,
    common_idx = common_idx,
    m_compared = length(common_idx),
    mitml_result = mitml_result,
    fallback_result = fallback_result,
    delta_AIC = delta_AIC,
    delta_BIC = delta_BIC
  ))
}
