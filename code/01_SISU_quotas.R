# Purpose: Building SISU quota shares using only the first SISU edition
#          at the university-year and microregion-year levels

# Loading necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)

# Defining project directories
# Change only project_dir to switch between the test folder and the full project folder.
project_dir <- "G:/My Drive/Artigos/AA Brazil/Quotas_Calculation"
# project_dir <- "C:/Users/fm469/Box/Brazil Paper (Felipe Macedo Dias)"

raw_dir <- file.path(project_dir, "data/raw")
intermediate_dir <- file.path(project_dir, "data/intermediate")
output_dir <- file.path(project_dir, "data/output")

setwd(project_dir)



# Function to standardize column names
standardize_column_names <- function(df, mapping) {
  header_key <- names(df) %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_to_upper() %>%
    str_squish()

  normalized_rename_map <- c(
    "EDICAO" = "EDICAO",
    "COD. IES" = "CO_IES",
    "NOME IES" = "NO_IES",
    "SIGLA IES" = "SG_IES",
    "CATEGORIA ADMINISTRATIVA" = "DS_CATEGORIA_ADM",
    "ORGANIZACAO ACADEMICA" = "DS_ORGANIZACAO_ACADEMICA",
    "CAMPUS" = "NO_CAMPUS",
    "REGIAO CAMPUS" = "DS_REGIAO",
    "SIGLA UF CAMPUS" = "SG_UF_CAMPUS",
    "MUNICIPIO CAMPUS" = "NO_MUNICIPIO_CAMPUS",
    "COD CURSO" = "CO_IES_CURSO",
    "NOME CURSO" = "NO_CURSO",
    "GRAU" = "DS_GRAU",
    "TURNO" = "DS_TURNO",
    "TIPO MODALIDADE" = "TP_MODALIDADE",
    "MODALIDADE CONCORRENCIA" = "DS_MOD_CONCORRENCIA",
    "PERCENTUAL DE BONUS" = "NU_PERCENTUAL_BONUS",
    "QT. VAGAS" = "QT_VAGAS_OFERTADAS",
    "NOTA_MINIMA_REDACAO" = "NOTA_MINIMA_REDACAO"
  )

  new_names <- unname(normalized_rename_map[header_key])
  names(df) <- ifelse(is.na(new_names), names(df), new_names)

  for (old_name in names(mapping)) {
    if (old_name %in% names(df)) {
      names(df)[names(df) == old_name] <- mapping[[old_name]]
    }}
  df
}

# Defining column name mappings for 'Vagas ofertadas' files
vagas_rename_map <- c(
  "EDIÇÃO" = "EDICAO",
  "CÓD. IES" = "CO_IES",
  "NOME IES" = "NO_IES",
  "SIGLA IES" = "SG_IES",
  "CATEGORIA ADMINISTRATIVA" = "DS_CATEGORIA_ADM",
  "ORGANIZAÇÃO ACADÊMICA" = "DS_ORGANIZACAO_ACADEMICA",
  "CAMPUS" = "NO_CAMPUS",
  "REGIÃO CAMPUS" = "DS_REGIAO",
  "SIGLA UF CAMPUS" = "SG_UF_CAMPUS",
  "MUNICÍPIO CAMPUS" = "NO_MUNICIPIO_CAMPUS",
  "CÓD CURSO" = "CO_IES_CURSO",
  "NOME CURSO" = "NO_CURSO",
  "GRAU" = "DS_GRAU",
  "TURNO" = "DS_TURNO",
  "TIPO MODALIDADE" = "TP_MODALIDADE",
  "MODALIDADE CONCORRÊNCIA" = "DS_MOD_CONCORRENCIA",
  "PERCENTUAL DE BÔNUS" = "NU_PERCENTUAL_BONUS",
  "QT. VAGAS" = "QT_VAGAS_OFERTADAS",
  "NOTA_MÍNIMA_REDACAO" = "NOTA_MINIMA_REDACAO"
)

# Listing all Excel files in the input directory
all_files <- list.files(
  path = file.path(raw_dir, "SISU/SISU aggregated"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Initializing empty data frame for appending
vagas_ofertadas_df <- tibble()

# Looping through each file to read and append data
for (file in all_files) {
  file_name <- basename(file)

  if (!str_detect(file_name, regex("Vagas ofertadas", ignore_case = TRUE))) {
    next
  }

  # Reading files based on naming convention.
  if (str_detect(file_name, fixed("PORTAL_Sisu 2010 a 2018"))) {
    temp_df <- read_excel(file, sheet = 1, skip = 4, col_types = "text")
  } else {
    temp_df <- read_excel(file, sheet = 2, col_types = "text")
  }

  vagas_ofertadas_df <- bind_rows(
    vagas_ofertadas_df,
    standardize_column_names(temp_df, vagas_rename_map)
  )
}


# Splitting 'EDICAO' into 'NU_ANO' and 'NU_EDICAO'
if ("EDICAO" %in% names(vagas_ofertadas_df)) {
  vagas_ofertadas_df <- vagas_ofertadas_df %>%
    mutate(
      temp_NU_ANO = if_else(str_detect(EDICAO, "/"), sub("/.*", "", EDICAO), NA_character_),
      temp_NU_EDICAO = if_else(str_detect(EDICAO, "/"), sub(".*/", "", EDICAO), NA_character_)
    ) %>%
    mutate(
      NU_ANO = coalesce(NU_ANO, temp_NU_ANO),
      NU_EDICAO = coalesce(NU_EDICAO, temp_NU_EDICAO)
    ) %>%
    select(-EDICAO, -starts_with("temp_"))
}

# Unifying QT_VAGAS_OFERTADAS and QT_VAGAS_CONCORRENCIA in vagas_ofertadas_df
if ("QT_VAGAS_CONCORRENCIA" %in% names(vagas_ofertadas_df)) {
  vagas_ofertadas_df <- vagas_ofertadas_df %>%
    mutate(QT_VAGAS_OFERTADAS = coalesce(QT_VAGAS_OFERTADAS, QT_VAGAS_CONCORRENCIA)) %>%
    select(-QT_VAGAS_CONCORRENCIA)
}

# Converting numeric columns
numeric_vagas_cols <- c("QT_VAGAS_OFERTADAS", "NU_PERCENTUAL_BONUS", "NU_ANO", "NU_EDICAO")
for (col in numeric_vagas_cols) {
  vagas_ofertadas_df[[col]] <- as.numeric(vagas_ofertadas_df[[col]])
}

# Keeping only the first SISU edition 
vagas_ofertadas_df <- vagas_ofertadas_df %>% filter(NU_EDICAO == 1)

# Sorting by year, edition, university code and major code
vagas_ofertadas_df <- vagas_ofertadas_df %>% arrange(NU_ANO, NU_EDICAO, CO_IES, CO_IES_CURSO)

# Importing dataset manually classified
classified_concorrencia_df <- read_excel(file.path(raw_dir, "SISU/DS_MOD_CONCORRENCIA_Unique_Values_CLASSIFIED.xlsx")) %>%
  select(Unique_DS_MOD_CONCORRENCIA_Values, Classification) %>%
  distinct()

# Cleaning the DS_MOD_CONCORRENCIA column
vagas_ofertadas_class <- vagas_ofertadas_df %>%
  mutate(
    DS_MOD_CONCORRENCIA_cleaned = iconv(DS_MOD_CONCORRENCIA, from = "UTF-8", to = "ASCII//TRANSLIT"),
    DS_MOD_CONCORRENCIA_cleaned = str_to_sentence(DS_MOD_CONCORRENCIA_cleaned)) %>%
  left_join(classified_concorrencia_df,
    by = c("DS_MOD_CONCORRENCIA_cleaned" = "Unique_DS_MOD_CONCORRENCIA_Values")) %>%
  mutate(Classification = if_else(is.na(Classification), "Others", Classification))

# Importing dataset manually classified of Courses
classified_major_df <- read_excel(file.path(raw_dir, "SISU/NO_CURSO_Unique_Values_CLASSIFIED_v2.xlsx")) %>%
  select(Unique_NO_CURSO_Values, Major) %>%
  distinct()

# Cleaning the NO_CURSO column
vagas_ofertadas_final <- vagas_ofertadas_class %>%
  mutate(NO_CURSO_cleaned = iconv(NO_CURSO, from = "UTF-8", to = "ASCII//TRANSLIT"),
         NO_CURSO_cleaned = str_to_sentence(NO_CURSO_cleaned)) %>%
  left_join(classified_major_df, by = c("NO_CURSO_cleaned" = "Unique_NO_CURSO_Values")) %>%
  mutate(Major = if_else(is.na(Major), "Others", Major)) %>%
  relocate(Major, .after = NO_CURSO) %>%
  relocate(Classification, .after = DS_MOD_CONCORRENCIA)

# Saving Courses "Majors"
Majors_course_df <- vagas_ofertadas_final %>%
  select(CO_IES, CO_IES_CURSO, Major) %>%
  distinct() %>%
  mutate(CO_IES_CURSO = as.numeric(CO_IES_CURSO)) %>%
  arrange(CO_IES, CO_IES_CURSO, Major)

#write.csv(Majors_course_df, file.path(intermediate_dir, "Majors_SISU.csv"), row.names = FALSE)
#write_dta(Majors_course_df, file.path(intermediate_dir, "Majors_SISU.dta"))

#### Selecting only "University" and Federal Centers/Institutes (removing State Centers)

# First, filling and standardizing institution-level classifications
vagas_ofertadas_final <- vagas_ofertadas_final %>%
  group_by(CO_IES) %>%
  fill(DS_ORGANIZACAO_ACADEMICA, DS_CATEGORIA_ADM, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    admin_category = case_when(
      str_detect(iconv(DS_CATEGORIA_ADM, from = "", to = "ASCII//TRANSLIT"), regex("federal", ignore_case = TRUE)) ~ "Federal",
      str_detect(iconv(DS_CATEGORIA_ADM, from = "", to = "ASCII//TRANSLIT"), regex("estadual|state", ignore_case = TRUE)) ~ "State",
      str_detect(iconv(DS_CATEGORIA_ADM, from = "", to = "ASCII//TRANSLIT"), regex("municipal", ignore_case = TRUE)) ~ "Municipal",
      TRUE ~ NA_character_
    ),
    academic_organization = case_when(
      str_detect(iconv(DS_ORGANIZACAO_ACADEMICA, from = "", to = "ASCII//TRANSLIT"), regex("^Universidade$", ignore_case = TRUE)) ~ "University",
      str_detect(iconv(DS_ORGANIZACAO_ACADEMICA, from = "", to = "ASCII//TRANSLIT"), regex("^Faculdade$", ignore_case = TRUE)) ~ "College",
      str_detect(iconv(DS_ORGANIZACAO_ACADEMICA, from = "", to = "ASCII//TRANSLIT"), regex("Instituto Federal", ignore_case = TRUE)) ~ "Federal Institute (IFs)",
      str_detect(iconv(DS_ORGANIZACAO_ACADEMICA, from = "", to = "ASCII//TRANSLIT"), regex("Centro Federal", ignore_case = TRUE)) ~ "Federal Technologic Center",
      TRUE ~ NA_character_
    )
  )

if (any(is.na(vagas_ofertadas_final$admin_category))) {
  stop("Unexpected or missing admin_category values after standardization.")
}

# Now, safely applying your original filter to the filled data
vagas_ofertadas_final <- vagas_ofertadas_final %>%
  filter(academic_organization %in% c(
    "University", "College", "Federal Institute (IFs)", "Federal Technologic Center"))

if (any(is.na(vagas_ofertadas_final$academic_organization))) {
  stop("Unexpected or missing academic_organization values after standardization.")
}


#### Affirmative action classification

# Aggregating TP_MODALIDADE into a yes/no dummy for Affirmative_Action
vagas_ofertadas_final <- vagas_ofertadas_final %>%
  mutate(Affirmative_Action = case_when(
    str_detect(TP_MODALIDADE, regex("Ampla Concorr", ignore_case = TRUE)) ~ 0,
    str_detect(TP_MODALIDADE, regex("Ações Afirmativas|Lei de Cotas|Lei n|Ação afirmativa", ignore_case = TRUE)) ~ 1,
    TRUE ~ if_else(Classification == "No Reserved", 0, 1)
  ))

# Aggregating Quotas Classifications into dummies.
# These category dummies are not mutually exclusive.
vagas_ofertadas_final <- vagas_ofertadas_final %>%
  mutate(
    Not_reserved = if_else(Classification == "No Reserved", 1, 0),
    Public_School = if_else(Classification %in% c(
      "Public School", "Public School non-white",
      "Public School low-income", "Public School non-white low-income"
    ), 1, 0),
    Racial = if_else(Classification %in% c(
      "Non-white", "Public School non-white",
      "Non-white low-income", "Public School non-white low-income"
    ), 1, 0),
    Low_Income = if_else(Classification %in% c(
      "Low-income", "Non-white low-income",
      "Public School low-income", "Public School non-white low-income"
    ), 1, 0),
    Disability = if_else(Classification %in% c(
      "Special Needs", "Low-income special needs", "Non-white special needs",
      "Public School special needs", "Public School low-income special needs",
      "Public School non-white special needs",
      "Public School non-white low-income special needs"
    ), 1, 0),
    Others = if_else(Classification %in% c("Others", "LGBT", "Regional"), 1, 0)
  )

# University localization (donwloaded form https://basedosdados.org/dataset/33b49786-fb5f-496f-bb7c-9811c985af8e?table=9c8ed04c-d617-4b5b-9e0c-ea4f9a945f04)
universities_geo <- read.csv(file.path(raw_dir, "Universities_Geoinfo.csv")) %>%
  select(id_ies, id_municipio) %>%
  distinct() %>%
  mutate(id_ies = as.character(id_ies), id_municipio = as.character(id_municipio))

# IBGE's table relating municipality code and microregion
microregion_df <- read_excel(file.path(raw_dir, "IBGE/Municipality_Regions_Composition.xlsx")) %>%
  select(Code_Municipality, Code_State, Code_Microregion) %>%
  mutate(Code_Municipality = as.character(Code_Municipality))

# Merging Microregion with university localization
geo_info <- universities_geo %>%
  left_join(microregion_df, by = c("id_municipio" = "Code_Municipality")) %>%
  rename(micro_reg_code = Code_Microregion, state_code = Code_State) %>%
  rename(co_ies = id_ies, co_municipio = id_municipio)

# Merging Localization with the Seats dataset
SISU_seats <- vagas_ofertadas_final %>%
  left_join(geo_info, by = c("CO_IES" = "co_ies")) %>%
  mutate(co_ies = as.numeric(CO_IES)) %>%
  rename(year = NU_ANO)

# Aggregated number of quota/non-quotas spots per university per year
SISU_univ_level <- SISU_seats %>%
  group_by(year, co_ies, admin_category, academic_organization, micro_reg_code, state_code) %>%
  summarise(
    Seats_Total = sum(QT_VAGAS_OFERTADAS, na.rm = TRUE),
    Seats_AA = sum(QT_VAGAS_OFERTADAS * Affirmative_Action, na.rm = TRUE),
    Seats_Not_reserved = sum(QT_VAGAS_OFERTADAS * Not_reserved, na.rm = TRUE),
    Seats_Public_School = sum(QT_VAGAS_OFERTADAS * Public_School, na.rm = TRUE),
    Seats_Racial = sum(QT_VAGAS_OFERTADAS * Racial, na.rm = TRUE),
    Seats_Low_Income = sum(QT_VAGAS_OFERTADAS * Low_Income, na.rm = TRUE),
    Seats_Disability = sum(QT_VAGAS_OFERTADAS * Disability, na.rm = TRUE),
    Seats_Others = sum(QT_VAGAS_OFERTADAS * Others, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Seats_Not_reserved = Seats_Total - Seats_AA,
    Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total
  ) %>%
  arrange(co_ies, year, micro_reg_code)


write_dta(SISU_univ_level, file.path(output_dir, "quotas_SISU_univ_year.dta"))

SISU_only_univ_level <- SISU_seats %>%
  filter(academic_organization == "University") %>%
  group_by(year, co_ies, admin_category, academic_organization, micro_reg_code, state_code) %>%
  summarise(
    Seats_Total = sum(QT_VAGAS_OFERTADAS, na.rm = TRUE),
    Seats_AA = sum(QT_VAGAS_OFERTADAS * Affirmative_Action, na.rm = TRUE),
    Seats_Not_reserved = sum(QT_VAGAS_OFERTADAS * Not_reserved, na.rm = TRUE),
    Seats_Public_School = sum(QT_VAGAS_OFERTADAS * Public_School, na.rm = TRUE),
    Seats_Racial = sum(QT_VAGAS_OFERTADAS * Racial, na.rm = TRUE),
    Seats_Low_Income = sum(QT_VAGAS_OFERTADAS * Low_Income, na.rm = TRUE),
    Seats_Disability = sum(QT_VAGAS_OFERTADAS * Disability, na.rm = TRUE),
    Seats_Others = sum(QT_VAGAS_OFERTADAS * Others, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Seats_Not_reserved = Seats_Total - Seats_AA,
    Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total
  ) %>%
  arrange(co_ies, year, micro_reg_code)

write_dta(SISU_only_univ_level, file.path(output_dir, "quotas_SISU_univ_year_only_univ.dta"))

# By Microregion
SISU_microreg_level <- SISU_univ_level %>%
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

write_dta(SISU_microreg_level, file.path(output_dir, "quotas_SISU_microreg_year.dta"))

SISU_microreg_only_univ_level <- SISU_only_univ_level %>%
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

write_dta(SISU_microreg_only_univ_level, file.path(output_dir, "quotas_SISU_microreg_year_only_univ.dta"))
