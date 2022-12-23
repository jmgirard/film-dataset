# Convert seconds to MM:SS format -----------------------------------------

s_to_mmss <- function(s) {
  p <- lubridate::seconds_to_period(s)
  sprintf("%02d:%02d", lubridate::minute(p), lubridate::second(p))
}

# Get clip-specific information tibble ------------------------------------

get_info_df <- function(abbrev) {
  readxl::read_xlsx("./data/film_info.xlsx") |> 
    dplyr::filter(Abbrev == abbrev)
}

# Get clip-specific holistic ratings tibble -------------------------------

get_holistic_df <- function(abbrev) {
  readr::read_rds("./data/holistic_ratings.rds") |> 
    tidyr::drop_na(Rating) |> 
    dplyr::filter(Abbrev == abbrev)
}

# Get clip-specific valence ratings tibble --------------------------------

get_valence_df <- function(abbrev) {
  readr::read_rds("./data/valence_ratings.rds") |>
    tidyr::drop_na(Rating) |> 
    dplyr::filter(Abbrev == abbrev)
}


# Create clip-specific holistic ratings plot ------------------------------

create_holistic_plot <- function(holistic_df) {
  require("Hmisc")
  
  pa <- c("Inspired", "Excited", "Enthusiastic", "Determined", "Alert")
  na <- c("Nervous", "Afraid", "Distressed", "Scared", "Upset")
  
  holistic_df |> 
    dplyr::mutate(
      Valence = dplyr::case_when(
        Scale %in% pa ~ "Positive",
        Scale %in% na ~ "Negative"
      )
    ) |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = Rating, 
        y = forcats::fct_reorder(Scale, Rating, .fun = mean), 
        fill = Valence
      )
    ) + 
    ggplot2::stat_summary(
      geom = "col", 
      fun = mean, 
      orientation = "y"
    ) +
    ggplot2::stat_summary(
      geom = "pointrange", 
      fun.data = ggplot2::mean_cl_boot, 
      linewidth = 0.6
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 4)) +
    ggplot2::labs(
      y = NULL, 
      x = "Average Rating with 95% CI", 
      fill = "Scale"
    ) +
    ggplot2::theme(legend.position = "top")
}

# Estimate clip-specific valence ratings ICC ------------------------------

estimate_valence_icc_clip <- function(valence_df, info_df, iter = 10000) {
  varde::calc_icc(
    .data = valence_df, 
    subject = "Second", 
    rater = "Rater", 
    score = "Rating",
    iter = iter,
    file = paste0("icc_", info_df$Abbrev),
    silent = 2,
    subject_label = "Timepoint"
  )
}


# Create clip-specific time series plot -----------------------------------

create_valence_plot <- function(valence_df) {
  require("Hmisc")
  valence_df |> 
    ggplot2::ggplot(
      ggplot2::aes(x = Second, y = Rating)
    ) +
    ggplot2::stat_summary(
      geom = "ribbon",
      fun.data = ggplot2::median_hilow,
      fun.args = list(conf.int = .9),
      fill = "#440154"
    ) +
    ggplot2::stat_summary(
      geom = "ribbon",
      fun.data = ggplot2::median_hilow,
      fun.args = list(conf.int = .7),
      fill = "#21908c"
    ) +
    ggplot2::stat_summary(
      geom = "ribbon",
      fun.data = ggplot2::median_hilow,
      fun.args = list(conf.int = .5),
      fill = "#fde725"
    ) +
    ggplot2::geom_hline(
      yintercept = 0, 
      color = "grey", 
      linewidth = 3/4
    ) +
    ggplot2::stat_summary(
      geom = "line", 
      fun = mean, 
      na.rm = TRUE, 
      linewidth = 3/4
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 300, 30), 
      expand = c(0, 0),
      labels = s_to_mmss
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(
      ylim = c(-4, 4)
    ) +
    ggplot2::labs(
      y = "Valence Rating", 
      x = "Timestamp within Clip"
    )
}

