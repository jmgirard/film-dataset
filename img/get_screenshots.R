library(tidymedia)
library(tidyverse)
library(glue)

source("./R/functions.R")
vid_dir <- "D:/OneDrive - University of Kansas/Research/Liebenthal/Video Clips/Optimized"
info_df <- get_info_df()

pull_screenshots <- function(info_df, n_images) {
  for (i in 1:nrow(info_df)) {
    info_i <- info_df[i, ]
    in_i <- file.path(vid_dir, glue('{info_i$Abbrev}_{info_i$Quality}.mp4'))
    ts_i <- seq(0, info_i$Duration, length.out = n_images + 2)[2:(n_images+1)]
    for (j in seq_along(ts_i)) {
      ts_ij <- ts_i[[j]]
      out_ij <- file.path("./img/screenshots", glue('{info_i$Abbrev}_{pad_integers(j, 2)}.jpg'))
      cmd_ij <- glue('-ss {ts_ij} -i "{in_i}" -qmin 1 -q:v 1 -qscale:v 2 -frames:v 1 -huffman optimal "{out_ij}"')
      ffmpeg(cmd_ij)
    }
  }
}

pull_screenshots(info_df, 12)
