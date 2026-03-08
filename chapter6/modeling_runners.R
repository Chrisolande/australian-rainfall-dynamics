# %%
run_fits_sequential <- function(
  tmp_paths,
  cond_formula,
  zi_formula,
  dispformula,
  control,
  offset = 0L
) {
  disable_multi_threading()
  total <- length(tmp_paths) + as.integer(offset)

  first_ok_done <- FALSE # flips TRUE after the first successful fit
  map(seq_along(tmp_paths), \(i) {
    path <- tmp_paths[[i]]
    global_i <- i + as.integer(offset)
    cat(str_glue(
      "Fitting imputation {global_i} of {total}: {basename(path)}..."
    ))
    t0 <- Sys.time()
    # strip = FALSE only until the first model converges successfully;
    # that model keeps its full frame + dat binding for DHARMa diagnostics
    res <- fit_one_lean(
      path,
      cond_formula,
      zi_formula,
      dispformula,
      control,
      strip = first_ok_done
    )
    dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

    if (isTRUE(res$ok)) {
      first_ok_done <<- TRUE
      cat(str_glue("Done! ({dt}s)\n\n"))
    } else {
      msg <- if (!is.na(res$error)) res$error else res$reason
      cat(str_glue("Failed after {dt}s. Reason: {msg}\n"))
    }
    gc()
    res
  })
}

# %%
run_fits_parallel <- function(
  tmp_paths,
  cond_formula,
  zi_formula,
  dispformula,
  control,
  workers = 4L,
  offset = 0L
) {
  if (workers == 1L) {
    message("Running in sequential mode.")
    return(run_fits_sequential(
      tmp_paths,
      cond_formula,
      zi_formula,
      dispformula,
      control
    ))
  }

  total <- length(tmp_paths)
  workers <- max(
    1L,
    min(as.integer(workers), length(tmp_paths), detectCores(logical = FALSE))
  )
  message(str_glue(
    "Parallel mode enabled: {workers} workers over {length(tmp_paths)} imputations."
  ))

  old_plan <- plan()
  on.exit(plan(old_plan), add = TRUE)

  plan(multisession, workers = workers)
  options(future.rng.onMisuse = "ignore")

  results <- with_progress(
    {
      p <- progressor(along = tmp_paths)

      future_lapply(
        seq_along(tmp_paths),
        FUN = function(i) {
          path <- tmp_paths[[i]]
          global_i <- i + as.integer(offset)

          disable_multi_threading()

          p(sprintf("[%d/%d] %s", global_i, total, basename(path)))
          # i == 1: keep full frame + rebind dat for DHARMa diagnostics
          # i  > 1: strip frame to save memory
          fit_one_lean(
            path,
            cond_formula,
            zi_formula,
            dispformula,
            control,
            strip = i != 1
          )
        },
        future.seed = TRUE
      )
    },
    handlers = handler_txtprogressbar()
  )

  # status summary
  map(seq_along(results), \(i) {
    res <- results[[i]]
    global_i <- i + as.integer(offset)
    status <- if (isTRUE(res$ok)) "COMPLETE" else "FAILED  "
    details <- if (!is.na(res$error)) {
      paste0(" (Error: ", res$error, ")")
    } else if (!isTRUE(res$ok)) {
      paste0(" (Reason: ", res$reason, ")")
    } else {
      ""
    }
    cat(str_glue(
      "Imputation {sprintf('%02d', global_i)}: {status} | {res$file}{details}\n"
    ))
  })

  results
}

# %%
run_fits <- function(
  tmp_paths,
  cond_formula,
  zi_formula,
  dispformula,
  control,
  parallel = FALSE,
  workers = 4L,
  offset = 0L
) {
  if (!isTRUE(parallel)) {
    message("Running in sequential mode.")
    return(run_fits_sequential(
      tmp_paths,
      cond_formula,
      zi_formula,
      dispformula,
      control,
      offset = offset
    ))
  }

  run_fits_parallel(
    tmp_paths = tmp_paths,
    cond_formula = cond_formula,
    zi_formula = zi_formula,
    dispformula = dispformula,
    control = control,
    workers = workers,
    offset = offset
  )
}

# %%
fit_and_pool <- function(
  cond_formula,
  zi_formula,
  datasets,
  dispformula,
  control = glmmTMBControl(),
  preflight_n = 2L,
  fail_fast = TRUE,
  parallel = FALSE,
  workers = 4L
) {
  M <- length(datasets)
  mode_txt <- if (isTRUE(parallel)) "parallel" else "sequential"
  cat(str_glue("Fitting {M} models ({mode_txt}, cold start)...\n"))

  tmp_paths <- stage_datasets(datasets)
  on.exit(unlink(attr(tmp_paths, "run_dir"), recursive = TRUE), add = TRUE)

  pre_results <- list()
  n_pre <- max(0L, min(as.integer(preflight_n), M))

  # Preflight fit and check
  if (n_pre > 0L) {
    cat(str_glue(
      "Running preflight on first {n_pre} imputations (sequential)...\n"
    ))
    pre_results <- run_fits_sequential(
      tmp_paths[seq_len(n_pre)],
      cond_formula,
      zi_formula,
      dispformula,
      control,
      offset = 0L
    )
    pre_diag <- summarize_results(pre_results)
    print(pre_diag)

    pre_ok <- sum(pre_diag$ok)
    if (fail_fast && pre_ok == 0L) {
      stop(
        "Preflight failed: 0 converged models in pilot run. Aborting early.",
        call. = FALSE
      )
    }
    if (fail_fast && n_pre >= 2L && pre_ok < 2L) {
      stop(
        "Preflight failed: fewer than 2 converged models in pilot run.",
        call. = FALSE
      )
    }
  }

  # Full run on the remaining datasets
  if (n_pre < M) {
    cat(str_glue(
      "Running remaining {M - n_pre} models ({mode_txt})...\n"
    ))
    rest_results <- run_fits(
      tmp_paths = tmp_paths[(n_pre + 1):M],
      cond_formula = cond_formula,
      zi_formula = zi_formula,
      dispformula = dispformula,
      control = control,
      parallel = parallel,
      workers = workers,
      offset = n_pre
    )
  } else {
    rest_results <- list()
  }

  # Combine preflight and remaining results
  results <- c(pre_results, rest_results)

  diagnostics <- summarize_results(results)
  validated <- validate_results(results, M)
  good <- validated$good
  good_idx <- validated$ok_idx
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
    diagnostics = diagnostics,
    good = good,
    m_used = length(good)
  )
}
