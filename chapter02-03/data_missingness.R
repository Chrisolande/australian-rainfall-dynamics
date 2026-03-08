# data_prep_missingness.R
# Five missingness diagnostic analyses that motivate the imputation pipeline.
# Each function returns a renderable kable or ggplot object and is called
# directly from the corresponding QMD chunk.

HIGH_MISS_VARS <- c("sunshine", "evaporation", "cloud3pm", "cloud9am")

# Co-missingness structure

#' Compute and render the conditional co-missingness matrix.
#' Prints a glue summary to the console and returns a styled kable.
render_comissingness_table <- function(dat = df) {
  high_miss <- HIGH_MISS_VARS[HIGH_MISS_VARS %in% names(dat)]

  cat("High missingness variables:", paste(high_miss, collapse = ", "), "\n\n")

  miss_mat <- dat %>%
    select(all_of(high_miss)) %>%
    mutate(across(everything(), is.na)) %>%
    as.matrix() %>%
    apply(2, as.integer)

  intersection_matrix <- crossprod(miss_mat)
  total_miss_counts <- diag(intersection_matrix)
  pct_matrix <- intersection_matrix / total_miss_counts * 100

  co_missing_stats <- as.data.frame(as.table(pct_matrix)) %>%
    rename(var1 = Var1, var2 = Var2, pct_co_miss = Freq) %>%
    filter(var1 != var2) %>%
    arrange(desc(pct_co_miss))

  co_missing_stats %>%
    kable(
      caption = paste0(
        "Conditional Co-missingness: when var1 is missing, ",
        "what percentage of the time is var2 also missing?"
      ),
      digits = 1,
      col.names = c("Variable 1 (Missing)", "Variable 2", "Co-missing (%)")
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
}


# Temporal structure and structural breaks

#' Plot monthly missingness rates over time for the four high-miss variables.
#' Returns a ggplot object.
plot_temporal_missingness <- function(dat = df) {
  high_miss <- HIGH_MISS_VARS[HIGH_MISS_VARS %in% names(dat)]

  temporal_trend <- dat %>%
    mutate(month_floor = floor_date(date, "month")) %>%
    select(month_floor, any_of(high_miss)) %>%
    pivot_longer(
      cols = -month_floor,
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(month_floor, variable) %>%
    summarise(pct_missing = mean(is.na(value)) * 100, .groups = "drop")

  ggplot(temporal_trend, aes(x = month_floor, y = pct_missing)) +
    geom_area(fill = "grey80", alpha = 0.5) +
    geom_line(colour = "#2C7BB6", linewidth = 0.7) +
    geom_hline(
      yintercept = 50,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.4
    ) +
    facet_wrap(~variable, ncol = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      labels = percent_format(scale = 1)
    ) +
    labs(
      title = "Temporal Structure of Missingness by Variable",
      subtitle = "Step changes indicate structural data collection breaks\nrather than random sensor failures",
      x = NULL,
      y = "Missing (%)",
      caption = "Dashed line at 50%. Monthly aggregation."
    ) +
    report_theme +
    theme(panel.spacing = unit(0.8, "lines"))
}


# Ghost sensor identification

#' Identify station-variable pairs with > 90% missingness.
#' Prints a count summary and returns a DT table.
render_ghost_sensor_table <- function(dat = df) {
  high_miss <- HIGH_MISS_VARS[HIGH_MISS_VARS %in% names(dat)]

  location_summary <- dat %>%
    group_by(location) %>%
    miss_var_summary()

  ghost_sensors <- location_summary %>%
    filter(variable %in% high_miss, pct_miss > GHOST_THRESHOLD * 100) %>%
    arrange(desc(pct_miss)) %>%
    rename(
      Location = location,
      Variable = variable,
      `Missing (n)` = n_miss,
      `Missing (%)` = pct_miss
    ) %>%
    mutate(
      `Missing (n)` = as.integer(`Missing (n)`),
      `Missing (%)` = as.numeric(round(`Missing (%)`, 1))
    )

  cat(sprintf(
    "\nTotal ghost sensor instances identified: %d across %d locations\n",
    nrow(ghost_sensors),
    n_distinct(ghost_sensors$Location)
  ))

  ghost_sensors %>%
    datatable(
      caption = "Ghost Sensor Instances: Location-Variable Pairs with >90% Missingness",
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = "Bfrtip",
        buttons = c("csv", "excel"),
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt-right", targets = c(2, 3))
        )
      )
    ) %>%
    formatStyle(
      "Missing (%)",
      background = styleColorBar(c(0, 100), "#f87171"),
      backgroundSize = "100% 80%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    )
}


# Weather-conditionality of sunshine missingness

#' Table: sunshine missing rate by rainy vs dry days.
render_sunshine_mcar_table <- function(dat = df) {
  if (!all(c("rainfall", "sunshine") %in% names(dat))) {
    message("rainfall or sunshine column not found; skipping MCAR test.")
    return(invisible(NULL))
  }

  weather_missing_stats <- dat %>%
    filter(!is.na(rainfall)) %>%
    mutate(is_rainy = if_else(rainfall > 1, "Rainy (>1mm)", "Dry (<=1mm)")) %>%
    group_by(is_rainy) %>%
    summarise(
      n_obs = n(),
      pct_sunshine_missing = mean(is.na(sunshine)) * 100,
      .groups = "drop"
    )

  weather_missing_stats %>%
    kable(
      caption = "Sunshine Missingness Rate by Daily Rainfall Status",
      digits = 1,
      col.names = c("Day Type", "Observations (n)", "Sunshine Missing (%)")
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
}

#' Density plot: rainfall distribution by sunshine presence / absence.
plot_sunshine_rainfall_density <- function(dat = df) {
  dat %>%
    bind_shadow() %>%
    filter(rainfall > 0 & rainfall < 50) %>%
    ggplot(aes(x = rainfall, fill = sunshine_NA, colour = sunshine_NA)) +
    geom_density(alpha = 0.35, linewidth = 0.6) +
    scale_fill_manual(
      values = c("!NA" = "#2C7BB6", "NA" = "#D7191C"),
      labels = c("!NA" = "Present", "NA" = "Missing")
    ) +
    scale_colour_manual(
      values = c("!NA" = "#2C7BB6", "NA" = "#D7191C"),
      labels = c("!NA" = "Present", "NA" = "Missing"),
      guide = "none"
    ) +
    scale_x_continuous(breaks = seq(0, 50, 10), expand = c(0.01, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      title = "Sunshine Missingness is Not Rainfall-Conditional",
      subtitle = paste0(
        "Rainfall distributions are near-identical whether sunshine data\n",
        "is present or absent, confirming missingness is driven by station\n",
        "instrumentation, not weather conditions"
      ),
      x = "Daily Rainfall (mm)",
      y = "Density",
      fill = "Sunshine Data",
      caption = "Restricted to rainfall > 0 and < 50 mm. Kernel density estimate."
    ) +
    report_theme
}


# Missing pattern structure

#' Table showing how many of the four variables are missing per observation.
render_miss_pattern_table <- function(dat = df) {
  high_miss <- HIGH_MISS_VARS[HIGH_MISS_VARS %in% names(dat)]

  dat %>%
    select(all_of(high_miss)) %>%
    miss_case_table() %>%
    kable(
      caption = "Distribution of Missing Variable Count per Observation",
      col.names = c(
        "Variables Missing (of 4)",
        "Observations (n)",
        "Percentage (%)"
      )
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
}
