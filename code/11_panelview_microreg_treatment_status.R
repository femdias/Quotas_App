# Purpose: Plotting continuous AA treatment intensity at the microregion-year level

library(tidyverse)
library(haven)
library(panelView)

project_dir <- "G:/My Drive/Artigos/AA Brazil/Quotas_Calculation"

output_dir <- file.path(project_dir, "data/output")
figures_dir <- file.path(project_dir, "results/figures")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

prepare_treatment_panel <- function(file_name, aggregate_to_microregion = FALSE) {
  treatment_df <- read_dta(file.path(output_dir, file_name)) %>%
    mutate(
      micro_reg_code = as.character(micro_reg_code),
      year = as.integer(year)
    )

  if (aggregate_to_microregion) {
    treatment_df <- treatment_df %>%
      group_by(micro_reg_code, year) %>%
      summarize(
        Seats_Total = sum(Seats_Total, na.rm = TRUE),
        Seats_AA = sum(Seats_AA, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Share_Seats_AA = if_else(Seats_Total > 0, Seats_AA / Seats_Total, 0))
  }

  year_range <- seq(min(treatment_df$year), max(treatment_df$year))

  treatment_panel <- treatment_df %>%
    select(micro_reg_code, year, Share_Seats_AA) %>%
    complete(
      micro_reg_code,
      year = year_range,
      fill = list(Share_Seats_AA = 0)
    ) %>%
    mutate(
      Share_Seats_AA_plot = case_when(
        Share_Seats_AA <= 0 ~ 0,
        Share_Seats_AA <= 0.1 ~ Share_Seats_AA / 0.1 * 0.25,
        Share_Seats_AA <= 0.3 ~ 0.25 + (Share_Seats_AA - 0.1) / 0.2 * 0.25,
        Share_Seats_AA <= 0.5 ~ 0.5 + (Share_Seats_AA - 0.3) / 0.2 * 0.25,
        TRUE ~ 0.75 + pmin(Share_Seats_AA - 0.5, 0.2) / 0.2 * 0.25
      ),
      outcome_placeholder = 0
    )

  microregion_order <- treatment_panel %>%
    group_by(micro_reg_code) %>%
    summarize(total_share_seats_aa = sum(Share_Seats_AA), .groups = "drop") %>%
    arrange(desc(total_share_seats_aa), micro_reg_code) %>%
    transmute(
      micro_reg_code,
      microregion_rank_total = row_number()
    )

  treatment_panel %>%
    left_join(microregion_order, by = "micro_reg_code") %>%
    arrange(microregion_rank_total, year)
}

save_treatment_outputs <- function(treatment_panel, output_name, plot_title) {
  n_microregions <- n_distinct(treatment_panel$micro_reg_code)

  panelview_plot <- panelview(
    outcome_placeholder ~ Share_Seats_AA_plot,
    data = treatment_panel,
    index = c("microregion_rank_total", "year"),
    type = "treat",
    treat.type = "continuous",
    gridOff = TRUE,
    color = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    xlab = "Year",
    ylab = "Microregion rank by total Share_Seats_AA",
    main = plot_title
  )

  panelview_plot <- panelview_plot +
    scale_fill_manual(
      values = c(
        "0" = "#F7FBFF",
        "0.25" = "#C6DBEF",
        "0.5" = "#6BAED6",
        "0.75" = "#2171B5",
        "1" = "#08306B"
      ),
      breaks = c("0", "0.25", "0.5", "0.75", "1"),
      labels = c("0", "0.1", "0.3", "0.5", "0.7+"),
      name = "Share_Seats_AA:"
    ) +
    labs(caption = paste0("Note: Balanced panel with ", n_microregions, " microregions.")) +
    theme(plot.caption = element_text(hjust = 0, size = 10))

  ggsave(
    filename = file.path(figures_dir, paste0(output_name, ".png")),
    plot = panelview_plot,
    width = 12,
    height = 8,
    units = "in",
    dpi = 300
  )
}

treatment_specs <- tribble(
  ~file_name,                                    ~output_name,                    ~plot_title,                                      ~aggregate_to_microregion,
  "quotas_SISU_microreg_year.dta",              "SISU_Treatment_Status",         "SISU treatment intensity: Share_Seats_AA",       FALSE,
  "quotas_Sup_Census_univ_year.dta",            "Sup_Census_Treatment_Status",   "Superior Census treatment intensity: Share_Seats_AA", TRUE,
  "quotas_SISU_both_editions_microreg_year.dta", "SISUS_both_Treatment_Status",   "SISU both editions treatment intensity",         FALSE
)

for (i in seq_len(nrow(treatment_specs))) {
  treatment_panel <- prepare_treatment_panel(
    file_name = treatment_specs$file_name[i],
    aggregate_to_microregion = treatment_specs$aggregate_to_microregion[i]
  )

  save_treatment_outputs(
    treatment_panel = treatment_panel,
    output_name = treatment_specs$output_name[i],
    plot_title = treatment_specs$plot_title[i]
  )
}
