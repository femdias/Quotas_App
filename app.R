library(shiny)
library(tidyverse)
library(haven)
library(panelView)
library(sf)

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(project_dir, "data/output")
shapefile_path <- file.path(
  project_dir,
  "data/raw/IBGE/BR_RG_Imediatas_2024/BR_RG_Imediatas_2024.shp"
)
simplified_map_path <- file.path(output_dir, "BR_RG_Imediatas_2024_simplified.rds")

dataset_specs <- tibble::tribble(
  ~dataset_id, ~dataset_label, ~file_name, ~org_var, ~title_label,
  "sisu", "SISU", "quotas_SISU_univ_year.dta", "DS_ORGANIZACAO_ACADEMICA", "SISU",
  "sisu_both", "SISU both editions", "quotas_SISU_both_editions_univ_year.dta", "DS_ORGANIZACAO_ACADEMICA", "SISU both editions",
  "sup_census", "Superior Census", "quotas_Sup_Census_univ_year.dta", "TP_ORGANIZACAO_ACADEMICA", "Superior Census"
)

share_specs <- c(
  "Share_Seats_AA" = "Share_Seats_AA",
  "Share_Seats_Public_School" = "Share_Seats_Public_School",
  "Share_Seats_Racial" = "Share_Seats_Racial",
  "Share_Seats_Low_Income" = "Share_Seats_Low_Income"
)

org_labels <- c(
  "1" = "Universidade",
  "3" = "Faculdade",
  "4" = "Instituto Federal de Educacao, Ciencia e Tecnologia",
  "5" = "Centro Federal de Educacao Tecnologica",
  "University" = "University",
  "College" = "College",
  "Federal Institute (IFs)" = "Federal Institute (IFs)",
  "Federal Technologic Center" = "Federal Technologic Center"
)

sisu_org_ids <- c(
  "Universidade" = "1",
  "Faculdade" = "3",
  "Instituto Federal de Educação, Ciência e Tecnologia" = "4",
  "Centro Federal de Educação Tecnológica" = "5"
)

add_share_columns <- function(df) {
  df %>%
    mutate(
      across(starts_with("Seats_"), as.numeric),
      Share_Seats_AA = if_else(Seats_Total > 0, Seats_AA / Seats_Total, 0),
      Share_Seats_Public_School = if_else(Seats_Total > 0, Seats_Public_School / Seats_Total, 0),
      Share_Seats_Racial = if_else(Seats_Total > 0, Seats_Racial / Seats_Total, 0),
      Share_Seats_Low_Income = if_else(Seats_Total > 0, Seats_Low_Income / Seats_Total, 0)
    )
}

load_dataset <- function(selected_dataset_id) {
  spec <- dataset_specs %>% filter(.data$dataset_id == selected_dataset_id)
  if (nrow(spec) != 1) {
    stop("Invalid dataset selection.")
  }

  df <- read_dta(file.path(output_dir, spec$file_name)) %>%
    mutate(
      year = as.integer(year),
      micro_reg_code = as.character(micro_reg_code)
    )

  if ("academic_organization" %in% names(df)) {
    df <- df %>%
      mutate(org_id = as.character(academic_organization))
  } else if (spec$org_var == "DS_ORGANIZACAO_ACADEMICA") {
    df <- df %>%
      mutate(org_id = unname(sisu_org_ids[DS_ORGANIZACAO_ACADEMICA]))
  } else {
    df <- df %>%
      mutate(org_id = as.character(as.integer(TP_ORGANIZACAO_ACADEMICA)))
  }

  df %>% add_share_columns()
}

org_display_label <- function(org_ids) {
  labels <- unname(org_labels[org_ids])
  if_else(is.na(labels), org_ids, labels)
}

available_org_choices <- function(dataset_id) {
  df <- load_dataset(dataset_id)
  available_ids <- sort(unique(df$org_id))
  labels <- org_display_label(available_ids)
  stats::setNames(available_ids, labels)
}

available_admin_choices <- function(df) {
  categories <- sort(unique(df$admin_category))
  stats::setNames(categories, categories)
}

available_year_choices <- function(df) {
  years <- sort(unique(df$year))
  stats::setNames(years, years)
}

share_numerator <- function(share_var) {
  switch(
    share_var,
    "Share_Seats_AA" = "Seats_AA",
    "Share_Seats_Public_School" = "Seats_Public_School",
    "Share_Seats_Racial" = "Seats_Racial",
    "Share_Seats_Low_Income" = "Seats_Low_Income",
    stop("Invalid share variable.")
  )
}

microregion_panel <- function(df, share_var) {
  numerator_var <- share_numerator(share_var)

  df %>%
    group_by(micro_reg_code, year) %>%
    summarize(
      Seats_Total = sum(Seats_Total, na.rm = TRUE),
      Seats_Selected = sum(.data[[numerator_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Share_Selected = if_else(Seats_Total > 0, Seats_Selected / Seats_Total, 0)) %>%
    arrange(micro_reg_code, year)
}

average_share_by_organization <- function(df, share_var) {
  numerator_var <- share_numerator(share_var)

  microregion_org_panel <- df %>%
    group_by(org_id, micro_reg_code, year) %>%
    summarize(
      Seats_Total = sum(Seats_Total, na.rm = TRUE),
      Seats_Selected = sum(.data[[numerator_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Share_Selected = if_else(Seats_Total > 0, Seats_Selected / Seats_Total, 0))

  org_counts <- microregion_org_panel %>%
    group_by(org_id) %>%
    summarize(n_microregions = n_distinct(micro_reg_code), .groups = "drop")

  microregion_org_panel %>%
    group_by(org_id, year) %>%
    summarize(
      Mean_Share_Selected = mean(Share_Selected, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(org_counts, by = "org_id") %>%
    mutate(
      org_label = paste0(
        org_display_label(org_id),
        " (n = ",
        n_microregions,
        ")"
      )
    ) %>%
    arrange(org_id, year)
}

prepare_treatment_panel <- function(df, share_var) {
  panel_df <- microregion_panel(df, share_var)
  year_range <- seq(min(panel_df$year), max(panel_df$year))

  treatment_panel <- panel_df %>%
    select(micro_reg_code, year, Share_Selected) %>%
    complete(
      micro_reg_code,
      year = year_range,
      fill = list(Share_Selected = 0)
    ) %>%
    mutate(
      Share_Selected_plot = case_when(
        Share_Selected <= 0 ~ 0,
        Share_Selected <= 0.1 ~ Share_Selected / 0.1 * 0.25,
        Share_Selected <= 0.3 ~ 0.25 + (Share_Selected - 0.1) / 0.2 * 0.25,
        Share_Selected <= 0.5 ~ 0.5 + (Share_Selected - 0.3) / 0.2 * 0.25,
        TRUE ~ 0.75 + pmin(Share_Selected - 0.5, 0.2) / 0.2 * 0.25
      ),
      outcome_placeholder = 0
    )

  microregion_order <- treatment_panel %>%
    group_by(micro_reg_code) %>%
    summarize(total_share = sum(Share_Selected), .groups = "drop") %>%
    arrange(desc(total_share), micro_reg_code) %>%
    transmute(micro_reg_code, microregion_rank_total = row_number())

  treatment_panel %>%
    left_join(microregion_order, by = "micro_reg_code") %>%
    arrange(microregion_rank_total, year)
}

if (file.exists(simplified_map_path)) {
  regions_sf_map <- readRDS(simplified_map_path)
} else {
  regions_sf <- st_read(shapefile_path, quiet = TRUE) %>%
    mutate(micro_reg_code = as.character(CD_RGI))

  regions_sf_map <- regions_sf %>%
    st_transform(5880) %>%
    st_simplify(dTolerance = 10000, preserveTopology = TRUE) %>%
    st_transform(st_crs(regions_sf))

  saveRDS(regions_sf_map, simplified_map_path)
}

ui <- fluidPage(
  titlePanel("AA Quotas Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dataset",
        "Dataset",
        choices = stats::setNames(dataset_specs$dataset_id, dataset_specs$dataset_label),
        selectize = FALSE
      ),
      selectInput(
        "organization",
        "Academic organization",
        choices = NULL,
        multiple = TRUE,
        selectize = FALSE
      ),
      selectInput(
        "admin_category",
        "Administrative category",
        choices = NULL,
        multiple = TRUE,
        selectize = FALSE
      ),
      selectInput(
        "share_var",
        "Share of affirmative action",
        choices = share_specs,
        selected = "Share_Seats_AA",
        selectize = FALSE
      ),
      selectInput("map_year", "Map year", choices = NULL, selectize = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Share of AA seats", plotOutput("share_plot", height = "720px")),
        tabPanel("Average by organization", plotOutput("average_share_plot", height = "640px")),
        tabPanel("Treatment status", plotOutput("treatment_plot", height = "720px")),
        tabPanel("Map", plotOutput("map_plot", height = "760px"))
      )
    )
  )
)

server <- function(input, output, session) {
  all_data <- reactive({
    req(input$dataset)
    load_dataset(input$dataset)
  })

  observeEvent(input$dataset, {
    choices <- available_org_choices(input$dataset)
    selected_org <- if ("University" %in% choices) "University" else "1"

    updateSelectInput(
      session,
      "organization",
      choices = choices,
      selected = selected_org
    )
  }, ignoreInit = FALSE)

  observeEvent(all_data(), {
    admin_choices <- available_admin_choices(all_data())

    updateSelectInput(
      session,
      "admin_category",
      choices = admin_choices,
      selected = admin_choices
    )
  }, ignoreInit = FALSE)

  observeEvent(all_data(), {
    years <- available_year_choices(all_data())

    updateSelectInput(
      session,
      "map_year",
      choices = years,
      selected = if (2014 %in% years) 2014 else min(years)
    )
  }, ignoreInit = FALSE)

  selected_spec <- reactive({
    dataset_specs %>% filter(.data$dataset_id == input$dataset)
  })

  selected_data <- reactive({
    req(input$organization, input$admin_category)
    all_data() %>%
      filter(
        .data$org_id %in% input$organization,
        .data$admin_category %in% input$admin_category
      )
  })

  selected_org_label <- reactive({
    req(input$organization)
    paste(org_display_label(input$organization), collapse = ", ")
  })

  output$share_plot <- renderPlot({
    req(input$share_var)
    panel_df <- microregion_panel(selected_data(), input$share_var)
    req(nrow(panel_df) > 0)

    panelview(
      Share_Selected ~ 1,
      data = panel_df,
      index = c("micro_reg_code", "year"),
      type = "outcome",
      ignore.treat = TRUE,
      xlab = "Year",
      ylab = input$share_var,
      main = paste0(
        "Evolution of ",
        input$share_var,
        " by microregion: ",
        selected_spec()$title_label,
        ", ",
        selected_org_label()
      )
    )
  })

  output$average_share_plot <- renderPlot({
    req(input$share_var)

    average_df <- average_share_by_organization(selected_data(), input$share_var)
    req(nrow(average_df) > 0)

    ggplot(
      average_df,
      aes(x = year, y = Mean_Share_Selected, color = org_label, group = org_label)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(
        title = paste0(
          "Average ",
          input$share_var,
          " across microregions: ",
          selected_spec()$title_label
        ),
        x = "Year",
        y = paste0("Average ", input$share_var),
        color = "Academic organization"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5)
      )
  })

  output$treatment_plot <- renderPlot({
    req(input$share_var)
    treatment_panel <- prepare_treatment_panel(selected_data(), input$share_var)
    req(nrow(treatment_panel) > 0)

    panelview_plot <- panelview(
      outcome_placeholder ~ Share_Selected_plot,
      data = treatment_panel,
      index = c("microregion_rank_total", "year"),
      type = "treat",
      treat.type = "continuous",
      gridOff = TRUE,
      color = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
      xlab = "Year",
      ylab = paste0("Microregion rank by total ", input$share_var),
      main = paste0(
        selected_spec()$title_label,
        " treatment intensity: ",
        selected_org_label()
      )
    )

    print(
      panelview_plot +
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
          name = paste0(input$share_var, ":")
        ) +
        labs(caption = paste0(
          "Note: Balanced panel with ",
          n_distinct(treatment_panel$micro_reg_code),
          " microregions."
        )) +
        theme(plot.caption = element_text(hjust = 0, size = 10))
    )
  })

  output$map_plot <- renderCachedPlot({
    req(input$share_var, input$map_year)

    share_map <- selected_data() %>%
      microregion_panel(input$share_var) %>%
      filter(year == as.integer(input$map_year)) %>%
      select(micro_reg_code, Share_Selected)

    req(nrow(share_map) > 0)

    map_sf <- regions_sf_map %>%
      left_join(share_map, by = "micro_reg_code") %>%
      mutate(Share_Selected = replace_na(Share_Selected, 0))

    ggplot(map_sf) +
      geom_sf(
        aes(fill = if_else(Share_Selected == 0, NA_real_, Share_Selected)),
        color = "white",
        linewidth = 0.05
      ) +
      scale_fill_gradient(
        low = "blue",
        high = "yellow",
        limits = c(0, 1),
        na.value = "grey80",
        name = input$share_var
      ) +
      labs(title = paste0(
        selected_spec()$title_label,
        " ",
        input$share_var,
        ", ",
        input$map_year,
        ": ",
        selected_org_label()
      )) +
      theme_void() +
      theme(
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }, cacheKeyExpr = {
    list(input$dataset, input$organization, input$admin_category, input$share_var, input$map_year)
  })
}

shinyApp(ui, server)
