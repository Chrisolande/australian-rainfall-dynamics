# %%

pool_zprob <- function(good, tmp_paths, good_idx) {
  zprobs <- map(seq_along(good), \(i) {
    dat <- readRDS(tmp_paths[good_idx[[i]]])
    zp <- predict(good[[i]]$fit, type = "zprob", newdata = dat)
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

  pooled_link <- map(zprobs, "zi_link") %>%
    reduce(cbind) %>%
    rowMeans()

  list(
    pooled_zprob = plogis(pooled_link),
    pooled_zi_link = pooled_link,
    per_imp_zprob = map(zprobs, "zprob"),
    per_imp_zi_link = map(zprobs, "zi_link")
  )
}

# %%
# Rubin's rules pooling per Van Buuren (FIMD, sec 2.3)
# https://stefvanbuuren.name/fimd/sec-whyandwhen.html
# %%
pool_rubin <- function(
  fit_list,
  component = c("cond", "zi", "disp"),
  dfcom = NULL
) {
  component <- match.arg(component)
  m <- length(fit_list)

  Qhat_list <- map(fit_list, \(f) fixef(f)[[component]])
  U_list <- map(fit_list, \(f) as.matrix(vcov(f)[[component]]))

  Qbar <- reduce(Qhat_list, `+`) / m
  Ubar <- reduce(U_list, `+`) / m
  B <- map(Qhat_list, \(q) tcrossprod(q - Qbar)) %>%
    reduce(`+`) /
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

  tibble(
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
  fit_list <- map(good, "fit")
  has_zi <- all(map_lgl(fit_list, \(f) length(fixef(f)$zi) > 0))
  has_disp <- all(map_lgl(fit_list, \(f) length(fixef(f)$disp) > 0))

  pooled <- bind_rows(
    pool_rubin(fit_list, "cond"),
    if (has_zi) pool_rubin(fit_list, "zi"),
    if (has_disp) pool_rubin(fit_list, "disp")
  )

  crit <- qt(0.975, df = pooled$df)

  mutate(
    pooled,
    term_full = case_when(
      component == "zi" ~ paste0("zi_", term),
      component == "disp" ~ paste0("disp_", term),
      TRUE ~ term
    ),
    conf.low = estimate - crit * std.error,
    conf.high = estimate + crit * std.error
  )
}
