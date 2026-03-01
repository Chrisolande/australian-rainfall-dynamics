source(here::here("config.R"))

# %%
stage_datasets <- function(datasets) {
  run_dir <- tempfile(pattern = "mi_pool_")
  dir.create(run_dir, recursive = TRUE)

  paths <- imap_chr(datasets, \(dat, i) {
    path <- file.path(run_dir, sprintf("imp_dataset_%02d.rds", as.integer(i)))
    saveRDS(dat, path, compress = FALSE)
    path
  })

  attr(paths, "run_dir") <- run_dir
  paths
}

# %%
strip_glmmtmb <- function(fit) {
  if (!is.null(fit$frame)) {
    fit$frame <- fit$frame[0, , drop = FALSE]
  }
  fit
}

# %%
fit_one_lean <- function(path, cond_formula, zi_formula, control) {
  blas_set_num_threads(1)
  omp_set_num_threads(1)

  dat <- readRDS(path)

  fit <- tryCatch(
    glmmTMB(
      formula = cond_formula,
      ziformula = zi_formula,
      family = glmmTMB::ziGamma(link = "log"),
      control = control,
      data = dat
    ),
    error = \(e) {
      message(stringr::str_glue(
        "ERROR on {basename(path)}: {conditionMessage(e)}"
      ))
      NULL
    }
  )

  rm(dat)
  gc()

  if (is.null(fit)) {
    return(NULL)
  }

  conv_ok <- isTRUE(fit$fit$convergence == 0)
  hess_ok <- isTRUE(fit$sdr$pdHess)

  if (!conv_ok || !hess_ok) {
    warning(
      stringr::str_glue(
        "Imputation {basename(path)}: convergence={fit$fit$convergence}, ",
        "pdHess={hess_ok}, dropping from pool."
      ),
      call. = FALSE
    )
    return(NULL)
  }

  ll_obj <- logLik(fit)

  list(
    fit = strip_glmmtmb(fit),
    ll = as.numeric(ll_obj),
    k = as.numeric(attr(ll_obj, "df")),
    n = nrow(model.frame(fit))
  )
}

# %%
run_fits <- function(tmp_paths, cond_formula, zi_formula, control) {
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    BLIS_NUM_THREADS = "1"
  )

  imap(tmp_paths, \(path, i) {
    cat(stringr::str_glue("  [{i}/{length(tmp_paths)}] {basename(path)}\n"))
    result <- fit_one_lean(path, cond_formula, zi_formula, control)
    gc()
    result
  })
}

# %%
validate_results <- function(results, M) {
  good <- purrr::keep(results, Negate(is.null))
  failed <- purrr::discard(results, Negate(is.null))

  if (length(good) == 0L) {
    stop("All models failed to fit, cannot pool.", call. = FALSE)
  }

  if (length(good) < 2L) {
    stop(
      stringr::str_glue(
        "Only {length(good)} model(s) converged, need >= 2 for Rubin's rules."
      ),
      call. = FALSE
    )
  }

  if (length(failed) > 0L) {
    warning(
      stringr::str_glue(
        "{length(failed)} of {M} model(s) failed or dropped ",
        "(indices: {toString(which(purrr::map_lgl(results, is.null)))}); ",
        "pooling over {length(good)}."
      ),
      call. = FALSE
    )
  }

  good
}

# %%
extract_fit_stats <- function(good) {
  ll <- mean(purrr::map_dbl(good, "ll"))
  k <- mean(purrr::map_dbl(good, "k"))
  n <- mean(purrr::map_dbl(good, "n"))

  tibble::tibble(
    mean_logLik = ll,
    heuristic_AIC = 2 * k - 2 * ll,
    heuristic_BIC = log(n) * k - 2 * ll,
    mean_k = k,
    mean_n = n
  )
}

# %%
pool_zprob <- function(good, tmp_paths, good_idx) {
  zprobs <- imap(good, \(imp, i) {
    dat <- readRDS(tmp_paths[good_idx[i]])
    zp <- predict(imp$fit, type = "zprob", newdata = dat)
    rm(dat)
    gc()
    list(
      zprob = zp,
      zi_link = qlogis(pmin(
        pmax(zp, .Machine$double.eps),
        1 - .Machine$double.eps
      ))
    )
  })

  pooled_link <- purrr::map(zprobs, "zi_link") %>%
    purrr::reduce(cbind) %>%
    rowMeans()

  list(
    pooled_zprob = plogis(pooled_link),
    pooled_zi_link = pooled_link,
    per_imp_zprob = purrr::map(zprobs, "zprob"),
    per_imp_zi_link = purrr::map(zprobs, "zi_link")
  )
}

# Rubin's rules pooling per Van Buuren (FIMD, sec 2.3)
# https://stefvanbuuren.name/fimd/sec-whyandwhen.html
# %%
pool_rubin <- function(fit_list, component = c("cond", "zi"), dfcom = NULL) {
  component <- match.arg(component)
  m <- length(fit_list)

  Qhat_list <- purrr::map(fit_list, \(f) fixef(f)[[component]])
  U_list <- purrr::map(fit_list, \(f) as.matrix(vcov(f)[[component]]))

  Qbar <- purrr::reduce(Qhat_list, `+`) / m
  Ubar <- purrr::reduce(U_list, `+`) / m
  B <- purrr::map(Qhat_list, \(q) tcrossprod(q - Qbar)) %>%
    purrr::reduce(`+`) /
    (m - 1)

  # total variance (eq 2.20)
  T_var <- Ubar + (1 + 1 / m) * B
  se <- sqrt(diag(T_var))

  # relative increase in variance (eq 2.24)
  r <- (1 + 1 / m) * diag(B) / diag(Ubar)
  r[!is.finite(r)] <- Inf

  # lambda uses T in the denominator (eq 2.24)
  lambda <- (1 + 1 / m) * diag(B) / diag(T_var)
  lambda[!is.finite(lambda)] <- 1

  # old df (eq 2.30, Rubin 1987)
  nu_old <- (m - 1) * (1 + 1 / r)^2
  nu_old[!is.finite(nu_old)] <- Inf

  if (is.null(dfcom)) {
    dfcom_val <- tryCatch(df.residual(fit_list[[1]]), error = \(e) NULL)
    if (is.null(dfcom_val) || !is.finite(dfcom_val)) {
      warning("dfcom not available, large sample assumed (dfcom = 999999).")
      dfcom_val <- 999999
    }
  } else {
    dfcom_val <- dfcom
  }

  # Barnard-Rubin correction (eq 2.31-2.32, B&R 1999)
  nu_obs <- (dfcom_val + 1) / (dfcom_val + 3) * dfcom_val * (1 - lambda)
  nu <- nu_old * nu_obs / (nu_old + nu_obs)
  nu[!is.finite(nu_obs)] <- nu_old[!is.finite(nu_obs)]

  stat <- as.numeric(Qbar) / se

  tibble::tibble(
    component = component,
    term = names(Qbar),
    estimate = as.numeric(Qbar),
    std.error = se,
    statistic = stat,
    dfcom = dfcom_val,
    df = nu,
    r = r,
    lambda = lambda,
    p.value = 2 * pt(abs(stat), df = nu, lower.tail = FALSE),
    fmi = (r + 2 / (nu + 3)) / (r + 1)
  )
}

# %%
pool_rubin_full <- function(good) {
  fit_list <- purrr::map(good, "fit")
  has_zi <- all(purrr::map_lgl(fit_list, \(f) length(fixef(f)$zi) > 0))

  pooled <- if (has_zi) {
    dplyr::bind_rows(pool_rubin(fit_list, "cond"), pool_rubin(fit_list, "zi"))
  } else {
    pool_rubin(fit_list, "cond")
  }

  crit <- qt(0.975, df = pooled$df)

  dplyr::mutate(
    pooled,
    term_full = dplyr::if_else(component == "zi", paste0("zi_", term), term),
    conf.low = estimate - crit * std.error,
    conf.high = estimate + crit * std.error
  )
}

# %%
fit_and_pool_v3 <- function(
  cond_formula,
  zi_formula,
  datasets,
  control = glmmTMBControl()
) {
  M <- length(datasets)
  cat(stringr::str_glue("Fitting {M} models sequentially...\n"))

  if (!exists(".Random.seed", envir = globalenv())) {
    set.seed(NULL)
  }
  seed_state <- get(".Random.seed", envir = globalenv())

  tmp_paths <- stage_datasets(datasets)
  on.exit(unlink(attr(tmp_paths, "run_dir"), recursive = TRUE), add = TRUE)

  results <- run_fits(tmp_paths, cond_formula, zi_formula, control)
  good_idx <- which(purrr::map_lgl(results, Negate(is.null)))
  good <- validate_results(results, M)
  stopifnot(length(good_idx) == length(good))

  cat("Pooling via Rubin's rules...\n")
  pooled <- pool_rubin_full(good)

  cat("Computing zero-inflation probabilities...\n")
  zprobs_pooled <- pool_zprob(good, tmp_paths, good_idx)

  list(
    pooled = pooled,
    fit_stats = extract_fit_stats(good),
    zprobs_pooled = zprobs_pooled,
    good_idx = good_idx,
    good = good,
    seed_state = seed_state,
    m_used = length(good)
  )
}
