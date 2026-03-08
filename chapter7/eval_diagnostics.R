# eval_diagnostics.R
# DHARMa simulated residual diagnostics:
# run_dharma_diagnostics() - simulates residuals, plots, tests dispersion
# and zero-inflation, then evicts the model

run_dharma_diagnostics <- function(model = m6_mixed) {
  m_full <- get_diagnostic_model(model)
  sim <- simulateResiduals(m_full)

  plot(sim)
  testDispersion(sim)
  testZeroInflation(sim)

  evict_model("m6_mixed")

  invisible(sim)
}
