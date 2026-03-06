# %%
strip_glmmtmb <- function(fit) {
  if (!is.null(fit$frame)) {
    fit$frame <- fit$frame[0, , drop = FALSE]
  }
  fit
}

# %%
# strip = TRUE  -> drop frame to save memory (all models except the first good one)
# strip = FALSE -> keep full frame AND rebind 'dat' into the call environment
#  so that predict() can resolve it after the worker exits.
# required for simulateResiduals() and DHARMa to work.
fit_one_lean <- function(
  path,
  cond_formula,
  zi_formula,
  dispformula,
  control,
  strip = TRUE
) {
  blas_set_num_threads(1)
  omp_set_num_threads(1)

  dat <- readRDS(path)
  warns <- character()
  msgs <- character()

  fit_or_err <- withCallingHandlers(
    tryCatch(
      glmmTMB(
        formula = cond_formula,
        ziformula = zi_formula,
        dispformula = dispformula,
        family = ziGamma(link = "log"),
        control = control,
        data = dat
      ),
      error = \(e) e
    ),
    warning = \(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = \(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  if (inherits(fit_or_err, "error")) {
    rm(dat)
    gc()
    return(list(
      ok = FALSE,
      file = basename(path),
      reason = "error",
      error = conditionMessage(fit_or_err),
      convergence = NA_integer_,
      pdHess = FALSE,
      ll = NA_real_,
      k = NA_real_,
      n = NA_real_,
      warnings = warns,
      messages = msgs,
      fit = NULL
    ))
  }

  fit <- fit_or_err
  conv <- fit$fit$convergence %||% NA_integer_
  conv_ok <- isTRUE(conv == 0)
  hess_ok <- isTRUE(fit$sdr$pdHess)

  if (!conv_ok || !hess_ok) {
    rm(dat)
    gc()
    return(list(
      ok = FALSE,
      file = basename(path),
      reason = "bad_convergence",
      error = NA_character_,
      convergence = conv,
      pdHess = hess_ok,
      ll = NA_real_,
      k = NA_real_,
      n = NA_real_,
      warnings = warns,
      messages = msgs,
      fit = NULL
    ))
  }

  ll_obj <- logLik(fit)

  if (strip) {
    # Stripped model: free memory, drop frame
    rm(dat)
    gc()
    fit <- strip_glmmtmb(fit)
  } else {
    # Diagnostic model: rebind 'dat' into the call environment so
    # predict() can resolve it after the worker/function exits.
    # dat is now intentionally NOT rm()'d - it lives in the call environment below.
    environment(fit$call) <- list2env(list(dat = dat), parent = globalenv())
  }

  list(
    ok = TRUE,
    file = basename(path),
    reason = "ok",
    error = NA_character_,
    convergence = conv,
    pdHess = hess_ok,
    ll = as.numeric(ll_obj),
    k = as.numeric(attr(ll_obj, "df")),
    n = nrow(model.frame(fit)),
    warnings = warns,
    messages = msgs,
    fit = fit
  )
}

# %%
summarize_results <- function(results) {
  tibble(
    idx = seq_along(results),
    file = map_chr(results, "file"),
    ok = map_lgl(results, "ok"),
    reason = map_chr(results, "reason"),
    convergence = map_int(
      results,
      \(x) ifelse(is.na(x$convergence), NA_integer_, x$convergence)
    ),
    pdHess = map_lgl(results, \(x) isTRUE(x$pdHess)),
    error = map_chr(results, \(x) ifelse(is.na(x$error), "", x$error)),
    n_warnings = map_int(results, \(x) length(x$warnings)),
    n_messages = map_int(results, \(x) length(x$messages))
  )
}

# %%
validate_results <- function(results, M) {
  ok_idx <- which(map_lgl(results, "ok"))
  good <- map(results[ok_idx], \(x) {
    list(fit = x$fit, ll = x$ll, k = x$k, n = x$n)
  })

  if (length(good) == 0L) {
    stop("All models failed to fit, cannot pool.", call. = FALSE)
  }

  if (length(good) < 2L) {
    stop(
      str_glue(
        "Only {length(good)} model(s) converged, need >= 2 for Rubin's rules."
      ),
      call. = FALSE
    )
  }

  failed_idx <- setdiff(seq_len(M), ok_idx)
  if (length(failed_idx) > 0L) {
    warning(
      str_glue(
        "{length(failed_idx)} of {M} model(s) failed/dropped ",
        "(indices: {toString(failed_idx)}); pooling over {length(good)}."
      ),
      call. = FALSE
    )
  }

  list(good = good, ok_idx = ok_idx)
}

# %%
extract_fit_stats <- function(good) {
  ll <- mean(map_dbl(good, "ll"))
  k <- mean(map_dbl(good, "k"))
  n <- mean(map_dbl(good, "n"))

  tibble(
    mean_logLik = ll,
    heuristic_AIC = 2 * k - 2 * ll,
    heuristic_BIC = log(n) * k - 2 * ll,
    mean_k = k,
    mean_n = n
  )
}
