# Convert seconds to MM:SS format -----------------------------------------

s_to_mmss <- function(s) {
  p <- lubridate::seconds_to_period(s)
  sprintf("%02d:%02d", lubridate::minute(p), lubridate::second(p))
}

# Get clip-specific information tibble ------------------------------------

get_info_df <- function(abbrev) {
  df <- readxl::read_xlsx("./data/film_info.xlsx")
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df
}

# Get clip-specific holistic ratings tibble -------------------------------

get_holistic_df <- function(abbrev) {
  df <- readr::read_rds("./data/holistic_ratings.rds") |> 
    tidyr::drop_na(Rating)
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df 
}

# Get clip-specific valence ratings tibble --------------------------------

get_valence_df <- function(abbrev) {
  df <- 
    readr::read_rds("./data/valence_ratings.rds") |>
    dplyr::rename(Timepoint = Second) |> 
    tidyr::drop_na(Rating)
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df
}


# Create clip-specific holistic ratings plot ------------------------------

create_holistic_plot <- function(holistic_df) {

    holistic_df |> 
    tidyr::pivot_wider(
      names_from = Scale,
      values_from = Rating
    ) |> 
    dplyr::mutate(
      "Positive Affect" = rowMeans(
        dplyr::across(c(Alert, Determined, Enthusiastic, Excited, Inspired)), 
        na.rm = TRUE
      ),
      "Negative Affect" = rowMeans(
        dplyr::across(c(Afraid, Distressed, Nervous, Scared, Upset)), 
        na.rm = TRUE
      )
    ) |> 
    tidyr::pivot_longer(
      cols = c("Positive Affect", "Negative Affect"),
      names_to = "Scale", 
      values_to = "Mean"
    ) |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = Mean, 
        y = Scale
      )
    ) + 
    ggplot2::geom_boxplot() +
    ggbeeswarm::geom_quasirandom(color = "firebrick") +
    ggplot2::scale_x_continuous(limits = c(0, 4)) +
    ggplot2::labs(
      y = NULL, 
      x = NULL
    )
}

# Estimate clip-specific valence ratings ICC ------------------------------

estimate_valence_icc_clip <- function(valence_df, info_df, iter = 10000) {
  varde::calc_icc(
    .data = valence_df, 
    subject = "Timepoint", 
    rater = "Rater", 
    scores = "Rating",
    iter = iter,
    file = paste0("data/icc_", info_df$Abbrev),
    silent = 2
  )
}


# Create clip-specific time series plot -----------------------------------

create_valence_plot <- function(valence_df) {
  require("Hmisc")
  valence_df |> 
    ggplot2::ggplot(
      ggplot2::aes(x = Timepoint, y = Rating)
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
      breaks = seq(0, 10*2*30, 30), 
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

