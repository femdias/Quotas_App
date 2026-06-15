# Purpose: Plotting the evolution of AA seat shares at the microregion-year level

library(tidyverse)
library(panelView)

project_dir <- "G:/My Drive/Artigos/AA Brazil/Quotas_Calculation"

output_dir <- file.path(project_dir, "data/output")
figures_dir <- file.path(project_dir, "results/figures")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

microreg_df <- read_csv(
  file.path(output_dir, "quotas_SISU_microreg_year.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    micro_reg_code = as.character(micro_reg_code),
    year = as.integer(year),
    Share_Seats_AA = as.numeric(Share_Seats_AA)
  ) %>%
  arrange(micro_reg_code, year)

png(
  filename = file.path(figures_dir, "panelview_microreg_share_seats_aa.png"),
  width = 12,
  height = 8,
  units = "in",
  res = 300
)

panelview(
  Share_Seats_AA ~ 1,
  data = microreg_df,
  index = c("micro_reg_code", "year"),
  type = "outcome",
  ignore.treat = TRUE,
  xlab = "Year",
  ylab = "Share of AA seats",
  main = "Evolution of Share_Seats_AA by microregion"
)

dev.off()
