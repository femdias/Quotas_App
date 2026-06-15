# Purpose: Mapping Share_Seats_AA by immediate region in 2014

library(tidyverse)
library(haven)
library(sf)

project_dir <- "G:/My Drive/Artigos/AA Brazil/Quotas_Calculation"

output_dir <- file.path(project_dir, "data/output")
figures_dir <- file.path(project_dir, "results/figures")
shapefile_path <- file.path(
  project_dir,
  "data/raw/IBGE/BR_RG_Imediatas_2024/BR_RG_Imediatas_2024.shp"
)

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

regions_sf <- st_read(shapefile_path, quiet = TRUE) %>%
  mutate(micro_reg_code = as.character(CD_RGI))

prepare_2014_share <- function(file_name, aggregate_to_microregion = FALSE) {
  share_df <- read_dta(file.path(output_dir, file_name)) %>%
    mutate(
      micro_reg_code = as.character(micro_reg_code),
      year = as.integer(year)
    )

  if (aggregate_to_microregion) {
    share_df <- share_df %>%
      group_by(micro_reg_code, year) %>%
      summarize(
        Seats_Total = sum(Seats_Total, na.rm = TRUE),
        Seats_AA = sum(Seats_AA, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Share_Seats_AA = if_else(Seats_Total > 0, Seats_AA / Seats_Total, 0))
  }

  share_df %>%
    filter(year == 2014) %>%
    select(micro_reg_code, Share_Seats_AA)
}

save_2014_map <- function(file_name, output_name, map_title, aggregate_to_microregion = FALSE) {
  share_2014 <- prepare_2014_share(
    file_name = file_name,
    aggregate_to_microregion = aggregate_to_microregion
  )

  map_sf <- regions_sf %>%
    left_join(share_2014, by = "micro_reg_code") %>%
    mutate(Share_Seats_AA = replace_na(Share_Seats_AA, 0))

  map_plot <- ggplot(map_sf) +
    geom_sf(aes(fill = Share_Seats_AA), color = "white", linewidth = 0.05) +
    scale_fill_gradientn(
      colors = c("grey80", "blue", "yellow"),
      values = scales::rescale(c(0, 1e-8, 1), from = c(0, 1)),
      limits = c(0, 1),
      name = "Share_Seats_AA"
    ) +
    labs(title = map_title) +
    theme_void() +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(
    filename = file.path(figures_dir, paste0(output_name, ".png")),
    plot = map_plot,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

map_specs <- tribble(
  ~file_name,                                     ~output_name,            ~map_title,                              ~aggregate_to_microregion,
  "quotas_SISU_microreg_year.dta",               "Map_SISU_2014",         "SISU Share_Seats_AA, 2014",             FALSE,
  "quotas_SISU_both_editions_microreg_year.dta", "Map_SISU_both_2014",    "SISU both editions Share_Seats_AA, 2014", FALSE,
  "quotas_Sup_Census_univ_year.dta",             "Map_Sup_Census_2014",   "Superior Census Share_Seats_AA, 2014",  TRUE
)

for (i in seq_len(nrow(map_specs))) {
  save_2014_map(
    file_name = map_specs$file_name[i],
    output_name = map_specs$output_name[i],
    map_title = map_specs$map_title[i],
    aggregate_to_microregion = map_specs$aggregate_to_microregion[i]
  )
}
