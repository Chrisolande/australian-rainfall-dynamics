# %%
wrap_for_mitml <- function(fit) {
  fe <- fixef(fit)
  nms_raw <- c(names(fe$cond), str_c("zi.", names(fe$zi)))
  nms <- sanitize_coef_names(nms_raw)

  if (any(duplicated(nms))) {
    nms <- make.unique(nms, sep = "_dup")
    warning(
      "Duplicate parameter names after sanitizing; made unique.",
      call. = FALSE
    )
  }

  coefs <- setNames(c(fe$cond, fe$zi), nms)

  vc <- vcov(fit)
  mat <- as.matrix(Matrix::bdiag(vc$cond, vc$zi))
  rownames(mat) <- colnames(mat) <- nms

  stopifnot(length(coefs) == nrow(mat))

  structure(
    list(coefs = coefs, vcovs = mat, name_map = setNames(nms, nms_raw)),
    class = "wrapped_glmmTMB"
  )
}

coef.wrapped_glmmTMB <- function(object, ...) object$coefs
vcov.wrapped_glmmTMB <- function(object, ...) object$vcovs

# %%
compare_models <- function(m_large, m_small, names = c("larger", "smaller")) {
  common_idx <- intersect(m_large$good_idx, m_small$good_idx)

  if (length(common_idx) < 2L) {
    stop(
      str_glue(
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
      str_glue(
        "Using {length(common_idx)} common imputations ",
        "(large had {length(m_large$good_idx)}, small had {length(m_small$good_idx)})."
      ),
      call. = FALSE
    )
  }

  pos_large <- match(common_idx, m_large$good_idx)
  fits_large <- map(m_large$good[pos_large], \(r) wrap_for_mitml(r$fit))

  added_terms <- setdiff(m_large$pooled$term, m_small$pooled$term)
  if (length(added_terms) == 0L) {
    stop(
      "No additional terms found in larger model. Check formula ordering.",
      call. = FALSE
    )
  }

  # resolve actual_diff against raw (unsanitized) coef names
  name_map <- fits_large[[1]]$name_map
  all_coef_raw <- names(name_map)

  actual_diff <- map_chr(added_terms, \(term) {
    if (term %in% all_coef_raw) {
      term
    } else if (str_c("zi.", term) %in% all_coef_raw) {
      str_c("zi.", term)
    } else {
      NA_character_
    }
  }) %>%
    discard(is.na)

  # sanitized names to pass to mitml
  actual_diff_sanitized <- unname(name_map[actual_diff])

  mitml_result <- tryCatch(
    testConstraints(
      fits_large,
      constraints = actual_diff_sanitized,
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

  if (is.null(mitml_result)) {
    rows <- filter(m_large$pooled, term %in% added_terms)
    fallback_result <- select(
      rows,
      component,
      term,
      estimate,
      std.error,
      statistic,
      df,
      p.value
    )

    if (all(is.finite(rows$std.error)) && all(rows$std.error > 0)) {
      fallback_result <- mutate(
        fallback_result,
        approx_joint_chi2 = sum((estimate / std.error)^2),
        approx_joint_df = n(),
        approx_joint_p = pchisq(
          approx_joint_chi2,
          approx_joint_df,
          lower.tail = FALSE
        )
      )
    }
  }

  list(
    model_names = names,
    added_terms = added_terms,
    actual_diff = actual_diff,
    common_idx = common_idx,
    m_compared = length(common_idx),
    mitml_result = mitml_result,
    fallback_result = fallback_result,
    delta_AIC = m_large$fit_stats$heuristic_AIC -
      m_small$fit_stats$heuristic_AIC,
    delta_BIC = m_large$fit_stats$heuristic_BIC -
      m_small$fit_stats$heuristic_BIC
  )
}
