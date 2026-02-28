# %%
options(warn = -1)
librarian::shelf(
  tidyverse,
  tidymodels,
  kableExtra,
  patchwork,
  skimr,
  gridExtra,
  janitor,
  corrplot,
  scales,
  GGally,
  car,
  forcats,
  performance,
  glmmTMB,
  splines,
  mgcv,
  DHARMa,
  zoo,
  ggpubr,
  ggridges,
  caret,
  rstatix,
  Metrics,
  mice,
  missRanger,
  ranger,
  cocor,
  splines,
  multcompView,
  lmtest,
  parallel,
  gt,
  broom.mixed,
  RhpcBLASctl,
  mitml
)

blas_set_num_threads(1)
omp_set_num_threads(1)
# %%
df_final <- read_csv(here::here("data", "df_final.csv"))
imp_mids <- readRDS("data/imp_mids.rds")
# %%
stage_datasets <- function(datasets) {
  run_dir <- tempfile(pattern = "mi_pool_")
  dir.create(run_dir, recursive = TRUE)

  paths <- vapply(
    seq_along(datasets),
    function(i) {
      path <- file.path(run_dir, sprintf("imp_dataset_%02d.rds", i))
      saveRDS(datasets[[i]], path, compress = FALSE)
      path
    },
    character(1)
  )

  attr(paths, "run_dir") <- run_dir
  paths
}
# %%
strip_glmmtmb <- function(fit) {
  vc_before <- tryCatch(
    vcov(fit, full = TRUE),
    error = function(e) NULL
  )

  if (!is.null(fit$frame)) {
    fit$frame <- fit$frame[0, , drop = FALSE]
  }

  vc_after <- tryCatch(
    vcov(fit, full = TRUE),
    error = function(e) NULL
  )

  if (!is.null(vc_before) && !is.null(vc_after)) {
    if (!isTRUE(all.equal(vc_before, vc_after, tolerance = 1e-10))) {
      warning(
        "strip_glmmTMB: vcov changed after stripping; ",
        "returning unstripped fit",
        call. = FALSE
      )
    } # Restore the dataframe from the mdoel
  } else if (is.null(vc_after)) {
    warning(
      "strip_glmmTMB: vcv inaccessible after stripping",
      call. = FALSE
    )
  }

  fit
}

# %%
fit_one_lean <- function(path, cond_formula, zi_formula, control) {
  blas_set_num_threads(1)
  omp_set_num_threads(1)

  dat <- readRDS(path)
  gc()

  fit <- tryCatch(
    glmmTMB(
      formula = cond_formula,
      ziformula = zi_formula,
      family = glmmTMB::ziGamma(link = "log"),
      control = control,
      data = dat
    ),
    error = function(e) {
      message(sprintf("ERROR on %s: %s", basename(path), conditionMessage(e)))
      NULL
    }
  )
  # Clear out the memory
  rm(dat)
  gc()
  if (is.null(fit)) {
    return(NULL)
  }

  # Chck cnvergence
  conv_ok <- isTRUE(fit$fit$convergence == 0)
  hess_ok <- isTRUE(fit$sdr$pdHess)

  if (!conv_ok || !hess_ok) {
    warning(
      sprintf(
        "Imputation %s: convergence=%s, pdHess=%s, dropping from pool.",
        basename(path),
        as.character(fit$fit$convergence),
        as.character(hess_ok)
      ),
      call. = FALSE
    )
    return(NULL)
  }

  ll_obj <- logLik(fit)

  n_obs <- nrow(model.frame(fit))

  result <- list(
    fit = strip_glmmTMB(fit),
    ll = as.numeric(ll_obj),
    k = as.numeric(attr(ll_obj, "df")),
    n = n_obs
  )

  gc()
  result
}
# %%
run_fit <- function(tmp_paths, cond_formula, zi_formula, control) {
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    BLIS_NUM_THREADS = "1"
  )

  lapply(seq_along(tmp_paths), function(i) {
    cat(sprintf(
      "  [%d/%d] %s\n",
      i,
      length(tmp_paths),
      basename(tmp_paths[[i]])
    ))
    result <- fit_one_lean(tmp_paths[[i]], cond_formula, zi_formula, control)
    gc()
    result
  })
}
# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%
