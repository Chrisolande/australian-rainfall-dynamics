# source(here::here("utils.R"))
options(warn = -1)
librarian::shelf(
  aod,
  broom.mixed,
  car,
  caret,
  cocor,
  corrplot,
  DHARMa,
  forcats,
  GGally,
  ggpubr,
  ggridges,
  glmmTMB,
  glue,
  gridExtra,
  gt,
  gtsummary,
  janitor,
  kableExtra,
  lmtest,
  Metrics,
  mgcv,
  mice,
  missRanger,
  mitml,
  multcompView,
  naniar,
  parallel,
  patchwork,
  performance,
  pROC,
  ranger,
  RhpcBLASctl,
  rstatix,
  scales,
  sjPlot,
  skimr,
  splines,
  tidymodels,
  tidyverse,
  viridis,
  zoo
)

report_theme <- theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    axis.line = element_line(colour = "grey40"),
    panel.grid.major = element_line(colour = "grey93", linewidth = 0.4),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50", hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  )

df <- read_csv(here::here("data", "weatherAUS.csv")) %>%
  janitor::clean_names()

disable_multi_threading <- function() {
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    BLIS_NUM_THREADS = "1"
  )
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    RhpcBLASctl::blas_set_num_threads(1)
    RhpcBLASctl::omp_set_num_threads(1)
  }
}

GHOST_THRESHOLD <- 0.90
MAXGAP <- 5
GHOST_THRESHOLD <- 0.90
M <- 10
