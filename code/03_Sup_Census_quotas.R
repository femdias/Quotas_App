### Purpose: Getting quotas for public HEIs using Superior Census reservation variables

# Loading necessary packages
library(readxl)
library(tidyverse)
library(haven)

# Defining project directories
# Change only project_dir to switch between the test folder and the full project folder.
project_dir <- "G:/My Drive/Artigos/AA Brazil/Quotas_Calculation"
# project_dir <- "C:/Users/fm469/Box/Brazil Paper (Felipe Macedo Dias)"

raw_dir <- file.path(project_dir, "data/raw")
output_dir <- file.path(project_dir, "data/output")

setwd(project_dir)

# Inputs entering this script:
# 1. data/raw/Superior Census/**/**/*CURSOS*.csv, excluding files from 2023
# 2. data/raw/Universities_Geoinfo.csv
# 3. data/raw/IBGE/Municipality_Regions_Composition.xlsx

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Defining the directory containing the files
data_dir <- file.path(raw_dir, "Superior Census")

# Listing all CSV files that contain "CURSOS" (case-insensitive) in their names
file_list <- list.files(
  path = data_dir,
  pattern = "(?i)CURSOS.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE
) %>%
  sort()

# Removing 2023 explicitly
file_list <- file_list[!str_detect(basename(file_list), "2023")]

# Reading and appending
# Superior Census has reserved admission variables, not reserved offered-seat variables.
to_select <- c(
  "NU_ANO_CENSO", "CO_IES", "CO_CURSO", "NO_CURSO", "CO_MUNICIPIO",
  "TP_CATEGORIA_ADMINISTRATIVA", "TP_ORGANIZACAO_ACADEMICA", "QT_ING",
  "QT_ING_RESERVA_VAGA", "QT_ING_RVREDEPUBLICA", "QT_ING_RVETNICO",
  "QT_ING_RVPDEF", "QT_ING_RVSOCIAL_RF", "QT_ING_RVOUTROS"
)

required_census_cols <- to_select

# Initializing empty tibble
combined_data <- tibble()
for (file in file_list) {
  cat("\n### Reading:", file, "###\n")

  df <- read_delim(
    file,
    delim = ";",
    show_col_types = FALSE,
    locale = locale(encoding = "Latin1")
  )

  df <- df %>%
    filter(TP_CATEGORIA_ADMINISTRATIVA %in% c(1, 2, 3)) %>% # Select the public universities
    filter(TP_ORGANIZACAO_ACADEMICA %in% c(1, 3, 4, 5)) %>% # Select universities, faculties, IFs, and CEFETs
    select(all_of(to_select)) %>%
    mutate(
      TP_CATEGORIA_ADMINISTRATIVA = case_when(
        TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Federal",
        TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "State",
        TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Municipal"
      )
    )

  combined_data <- bind_rows(combined_data, df)
}

# Selecting column
selec_comb <- combined_data %>% select(all_of(to_select))

# Renaming
names(selec_comb) <- c(
  "year", "co_ies", "Course_Code", "Course_Name", "Code_Municipality",
  "admin_category", "academic_organization", "Seats_Total", "Seats_AA",
  "Seats_Public_School", "Seats_Racial", "Seats_Disability",
  "Seats_Low_Income", "Seats_Others"
)

selec_comb <- selec_comb %>%
  mutate(
    academic_organization = case_when(
      academic_organization == 1 ~ "University",
      academic_organization == 3 ~ "College",
      academic_organization == 4 ~ "Federal Institute (IFs)",
      academic_organization == 5 ~ "Federal Technologic Center",
      TRUE ~ NA_character_
    )
  )

if (any(is.na(selec_comb$admin_category))) {
  stop("Unexpected or missing TP_CATEGORIA_ADMINISTRATIVA values after standardization.")
}

if (any(is.na(selec_comb$academic_organization))) {
  stop("Unexpected or missing TP_ORGANIZACAO_ACADEMICA values after standardization.")
}

# The reservation categories are not mutually exclusive.
# Seats_AA uses QT_ING_RESERVA_VAGA directly to avoid double-counting categories.
selec_comb <- selec_comb %>%
  mutate(
    across(starts_with("Seats_"), ~replace_na(as.numeric(.), 0)),
    Seats_AA = pmin(Seats_AA, Seats_Total),
    Seats_Not_reserved = Seats_Total - Seats_AA
  )

# Dropping if total admissions == 0
selec_comb <- selec_comb %>% filter(Seats_Total != 0)

# Adding microreg and state (donwloaded form https://basedosdados.org/dataset/33b49786-fb5f-496f-bb7c-9811c985af8e?table=9c8ed04c-d617-4b5b-9e0c-ea4f9a945f04)
universities_geo <- read.csv(file.path(raw_dir, "Universities_Geoinfo.csv")) %>%
  select(id_ies, id_municipio) %>%
  distinct() %>%
  mutate(id_ies = as.numeric(id_ies), id_municipio = as.character(id_municipio))

microregion_df <- read_excel(file.path(raw_dir, "IBGE/Municipality_Regions_Composition.xlsx")) %>%
  select(Code_Municipality, Code_State, Code_Microregion) %>%
  mutate(Code_Municipality = as.character(Code_Municipality))

geo_info <- universities_geo %>%
  left_join(microregion_df, by = c("id_municipio" = "Code_Municipality")) %>%
  rename(
    co_ies = id_ies,
    co_municipio = id_municipio,
    micro_reg_code = Code_Microregion,
    state_code = Code_State
  )

# Joining the geo info
selec_comb <- selec_comb %>% left_join(geo_info, by = "co_ies")

# Selecting columns
selec_comb <- selec_comb %>%
  select(
    co_ies, year, admin_category, academic_organization, co_municipio, micro_reg_code, state_code,
    Seats_Total, Seats_AA, Seats_Not_reserved, Seats_Public_School,
    Seats_Racial, Seats_Low_Income, Seats_Disability, Seats_Others) %>%
  arrange(co_ies, year)

# Aggregating by University
Sup_Census_univ_level <- selec_comb %>%
  group_by(co_ies, year, admin_category, academic_organization, micro_reg_code, state_code) %>%
  summarise(
    Seats_Total = sum(Seats_Total, na.rm = TRUE),
    Seats_AA = sum(Seats_AA, na.rm = TRUE),
    Seats_Not_reserved = sum(Seats_Not_reserved, na.rm = TRUE),
    Seats_Public_School = sum(Seats_Public_School, na.rm = TRUE),
    Seats_Racial = sum(Seats_Racial, na.rm = TRUE),
    Seats_Low_Income = sum(Seats_Low_Income, na.rm = TRUE),
    Seats_Disability = sum(Seats_Disability, na.rm = TRUE),
    Seats_Others = sum(Seats_Others, na.rm = TRUE),
    .groups = "drop"
  )

#---------------------------------------------------------------------------
# University of Sao Paulo (USP, 55) Quotas are strange. It's not right.
# The quotas started in 2018 with 36.9% and went until 50% in 2021.
# 37% of reserved seats are racial quotas. There are no other quotas.
# https://g1.globo.com/educacao/noticia/usp-aprova-cotas-raciais-e-de-escola-publica-na-fuvest-pela-primeira-vez-na-historia.ghtml
#---------------------------------------------------------------------------

# Sup_Census_univ_level <- Sup_Census_univ_level %>%
#   mutate(
#     Seats_AA = case_when(
#       co_ies == 55 & year == 2018 ~ round(Seats_Total * 0.369),
#       co_ies == 55 & year == 2019 ~ round(Seats_Total * 0.40),
#       co_ies == 55 & year == 2020 ~ round(Seats_Total * 0.45),
#       co_ies == 55 & year >= 2021 ~ round(Seats_Total * 0.50),
#       TRUE ~ Seats_AA
#     ),
#     Seats_Public_School = case_when(
#       co_ies == 55 & year >= 2018 ~ Seats_AA,
#       TRUE ~ Seats_Public_School
#     ),
#     Seats_Racial = case_when(
#       co_ies == 55 & year >= 2018 ~ round(Seats_AA * 0.37),
#       TRUE ~ Seats_Racial
#     ),
#     Seats_Low_Income = if_else(co_ies == 55 & year >= 2018, 0, Seats_Low_Income),
#     Seats_Disability = if_else(co_ies == 55 & year >= 2018, 0, Seats_Disability),
#     Seats_Others = if_else(co_ies == 55 & year >= 2018, 0, Seats_Others),
#     Seats_Not_reserved = Seats_Total - Seats_AA
#   ) %>%
#   mutate(
#     Share_Seats_AA = Seats_AA / Seats_Total,
#     Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
#     Share_Seats_Public_School = Seats_Public_School / Seats_Total,
#     Share_Seats_Racial = Seats_Racial / Seats_Total,
#     Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
#     Share_Seats_Disability = Seats_Disability / Seats_Total,
#     Share_Seats_Others = Seats_Others / Seats_Total
#   ) %>%
#   arrange(co_ies, year)

write_dta(Sup_Census_univ_level, file.path(output_dir, "quotas_Sup_Census_univ_year.dta"))

Sup_Census_only_univ_level <- selec_comb %>%
  filter(academic_organization == "University") %>%
  group_by(co_ies, year, admin_category, academic_organization, micro_reg_code, state_code) %>%
  summarise(
    Seats_Total = sum(Seats_Total, na.rm = TRUE),
    Seats_AA = sum(Seats_AA, na.rm = TRUE),
    Seats_Not_reserved = sum(Seats_Not_reserved, na.rm = TRUE),
    Seats_Public_School = sum(Seats_Public_School, na.rm = TRUE),
    Seats_Racial = sum(Seats_Racial, na.rm = TRUE),
    Seats_Low_Income = sum(Seats_Low_Income, na.rm = TRUE),
    Seats_Disability = sum(Seats_Disability, na.rm = TRUE),
    Seats_Others = sum(Seats_Others, na.rm = TRUE),
    .groups = "drop"
  )

write_dta(Sup_Census_only_univ_level, file.path(output_dir, "quotas_Sup_Census_univ_year_only_univ.dta"))

# By Microregion
Sup_Census_microreg_level <- Sup_Census_univ_level %>%
  group_by(micro_reg_code, year) %>%
  summarise(
    Seats_Total = sum(Seats_Total, na.rm = TRUE),
    Seats_AA = sum(Seats_AA, na.rm = TRUE),
    Seats_Not_reserved = sum(Seats_Not_reserved, na.rm = TRUE),
    Seats_Public_School = sum(Seats_Public_School, na.rm = TRUE),
    Seats_Racial = sum(Seats_Racial, na.rm = TRUE),
    Seats_Low_Income = sum(Seats_Low_Income, na.rm = TRUE),
    Seats_Disability = sum(Seats_Disability, na.rm = TRUE),
    Seats_Others = sum(Seats_Others, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total
  ) %>%
  arrange(micro_reg_code, year)

write_dta(Sup_Census_microreg_level, file.path(output_dir, "quotas_Sup_Census_microreg_year.dta"))

Sup_Census_microreg_only_univ_level <- Sup_Census_only_univ_level %>%
  group_by(micro_reg_code, year) %>%
  summarise(
    Seats_Total = sum(Seats_Total, na.rm = TRUE),
    Seats_AA = sum(Seats_AA, na.rm = TRUE),
    Seats_Not_reserved = sum(Seats_Not_reserved, na.rm = TRUE),
    Seats_Public_School = sum(Seats_Public_School, na.rm = TRUE),
    Seats_Racial = sum(Seats_Racial, na.rm = TRUE),
    Seats_Low_Income = sum(Seats_Low_Income, na.rm = TRUE),
    Seats_Disability = sum(Seats_Disability, na.rm = TRUE),
    Seats_Others = sum(Seats_Others, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total
  ) %>%
  arrange(micro_reg_code, year)

write_dta(Sup_Census_microreg_only_univ_level, file.path(output_dir, "quotas_Sup_Census_microreg_year_only_univ.dta"))
