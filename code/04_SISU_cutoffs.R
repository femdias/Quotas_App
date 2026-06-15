# Purpose: Extracting SISU cutoff scores and aggregating them by university-major-quota type

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
output_dir <- file.path(project_dir, "data/output")

setwd(project_dir)



# Function to standardize column names
standardize_column_names <- function(df, mapping) {
  header_key <- names(df) %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_to_upper() %>%
    str_squish()

  normalized_names <- unname(mapping[header_key])
  names(df) <- ifelse(is.na(normalized_names), names(df), normalized_names)
  df
}

# Function to convert numeric columns imported as text
parse_numeric_column <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "")
  x <- str_replace_all(x, ",", ".")
  as.numeric(x)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

weighted_mean_or_na <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0

  if (!any(keep)) {
    return(NA_real_)
  }

  weighted.mean(x[keep], w[keep])
}

make_join_key <- function(x) {
  x %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_to_upper() %>%
    str_squish()
}

rename_map <- c(
  "EDICAO" = "EDICAO",
  "NU_ANO" = "NU_ANO",
  "NU_EDICAO" = "NU_EDICAO",
  "COD. IES" = "CO_IES",
  "CO_IES" = "CO_IES",
  "NOME IES" = "NO_IES",
  "NO_IES" = "NO_IES",
  "SIGLA IES" = "SG_IES",
  "SG_IES" = "SG_IES",
  "CATEGORIA ADMINISTRATIVA" = "DS_CATEGORIA_ADM",
  "DS_CATEGORIA_ADM" = "DS_CATEGORIA_ADM",
  "ORGANIZACAO ACADEMICA" = "DS_ORGANIZACAO_ACADEMICA",
  "DS_ORGANIZACAO_ACADEMICA" = "DS_ORGANIZACAO_ACADEMICA",
  "COD CAMPUS" = "CO_CAMPUS",
  "CO_CAMPUS" = "CO_CAMPUS",
  "CAMPUS" = "NO_CAMPUS",
  "NO_CAMPUS" = "NO_CAMPUS",
  "REGIAO CAMPUS" = "DS_REGIAO_CAMPUS",
  "DS_REGIAO_CAMPUS" = "DS_REGIAO_CAMPUS",
  "SIGLA UF CAMPUS" = "SG_UF_CAMPUS",
  "SG_UF_CAMPUS" = "SG_UF_CAMPUS",
  "MUNICIPIO CAMPUS" = "NO_MUNICIPIO_CAMPUS",
  "NO_MUNICIPIO_CAMPUS" = "NO_MUNICIPIO_CAMPUS",
  "COD CURSO" = "CO_IES_CURSO",
  "CO_IES_CURSO" = "CO_IES_CURSO",
  "NOME CURSO" = "NO_CURSO",
  "NO_CURSO" = "NO_CURSO",
  "GRAU" = "DS_GRAU",
  "DS_GRAU" = "DS_GRAU",
  "TURNO" = "DS_TURNO",
  "DS_TURNO" = "DS_TURNO",
  "TIPO MODALIDADE" = "TP_MOD_CONCORRENCIA",
  "TP MODALIDADE" = "TP_MOD_CONCORRENCIA",
  "TP_MOD_CONCORRENCIA" = "TP_MOD_CONCORRENCIA",
  "TP_MODALIDADE" = "TP_MOD_CONCORRENCIA",
  "MODALIDADE CONCORRENCIA" = "DS_MOD_CONCORRENCIA",
  "DS_MOD_CONCORRENCIA" = "DS_MOD_CONCORRENCIA",
  "PERCENTUAL DE BONUS" = "NU_PERCENTUAL_BONUS",
  "NU_PERCENTUAL_BONUS" = "NU_PERCENTUAL_BONUS",
  "QT. VAGAS" = "QT_VAGAS_CONCORRENCIA",
  "QT VAGAS" = "QT_VAGAS_CONCORRENCIA",
  "QT_VAGAS_CONCORRENCIA" = "QT_VAGAS_CONCORRENCIA",
  "QT_INSCRICAO" = "QT_INSCRICAO",
  "NOTA DE CORTE" = "NU_NOTACORTE",
  "NU_NOTACORTE" = "NU_NOTACORTE"
)

all_files <- list.files(
  path = file.path(raw_dir, "SISU/SISU aggregated"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Reading cutoff files
cutoff_df <- tibble()

for (file in all_files) {
  file_name <- basename(file)

  if (!str_detect(file_name, regex("Inscri|Nota de corte", ignore_case = TRUE))) {
    next
  }

  if (str_detect(file_name, fixed("PORTAL_Sisu 2010 a 2018"))) {
    temp_df <- read_excel(file, sheet = 1, skip = 4, col_types = "text")
  } else {
    temp_df <- read_excel(file, sheet = 2, col_types = "text")
  }

  cutoff_df <- bind_rows(cutoff_df, standardize_column_names(temp_df, rename_map))
}

if (nrow(cutoff_df) == 0) {
  stop("No SISU cutoff files were found.")
}

# Reading seats files
vagas_df <- tibble()

for (file in all_files) {
  file_name <- basename(file)

  if (!str_detect(file_name, regex("Vagas ofertadas", ignore_case = TRUE))) {
    next
  }

  if (str_detect(file_name, fixed("PORTAL_Sisu 2010 a 2018"))) {
    temp_df <- read_excel(file, sheet = 1, skip = 4, col_types = "text")
  } else {
    temp_df <- read_excel(file, sheet = 2, col_types = "text")
  }

  vagas_df <- bind_rows(vagas_df, standardize_column_names(temp_df, rename_map))
}

if (nrow(vagas_df) == 0) {
  stop("No SISU seat files were found.")
}

# Splitting 'EDICAO' into 'NU_ANO' and 'NU_EDICAO'
if ("EDICAO" %in% names(cutoff_df)) {
  cutoff_df <- cutoff_df %>%
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

if ("EDICAO" %in% names(vagas_df)) {
  vagas_df <- vagas_df %>%
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

numeric_cols <- c(
  "NU_ANO", "NU_EDICAO", "CO_IES", "CO_CAMPUS", "CO_IES_CURSO",
  "QT_VAGAS_CONCORRENCIA", "QT_INSCRICAO", "NU_NOTACORTE"
)

for (col in numeric_cols) {
  if (col %in% names(cutoff_df)) {
    original_non_missing <- !is.na(cutoff_df[[col]]) & str_squish(as.character(cutoff_df[[col]])) != ""
    cutoff_df[[col]] <- parse_numeric_column(cutoff_df[[col]])

    if (any(original_non_missing & is.na(cutoff_df[[col]]))) {
      stop(paste("Unexpected non-numeric values in cutoff column", col))
    }
  }

  if (col %in% names(vagas_df)) {
    original_non_missing <- !is.na(vagas_df[[col]]) & str_squish(as.character(vagas_df[[col]])) != ""
    vagas_df[[col]] <- parse_numeric_column(vagas_df[[col]])

    if (any(original_non_missing & is.na(vagas_df[[col]]))) {
      stop(paste("Unexpected non-numeric values in seats column", col))
    }
  }
}

text_join_cols <- intersect(
  c("NO_CAMPUS", "NO_CURSO", "DS_GRAU", "DS_TURNO", "TP_MOD_CONCORRENCIA", "DS_MOD_CONCORRENCIA"),
  intersect(names(cutoff_df), names(vagas_df))
)

for (col in text_join_cols) {
  cutoff_df[[paste0(col, "_join")]] <- make_join_key(cutoff_df[[col]])
  vagas_df[[paste0(col, "_join")]] <- make_join_key(vagas_df[[col]])
}

required_cutoff_cols <- c(
  "NU_ANO", "NU_EDICAO", "CO_IES", "CO_IES_CURSO", "NO_CURSO",
  "DS_GRAU", "DS_TURNO", "TP_MOD_CONCORRENCIA", "DS_MOD_CONCORRENCIA", "NU_NOTACORTE"
)
missing_cutoff_cols <- setdiff(required_cutoff_cols, names(cutoff_df))

if (length(missing_cutoff_cols) > 0) {
  stop(paste("Missing required cutoff columns:", paste(missing_cutoff_cols, collapse = ", ")))
}

required_vagas_cols <- c(
  "NU_ANO", "NU_EDICAO", "CO_IES", "CO_IES_CURSO", "NO_CURSO",
  "DS_GRAU", "DS_TURNO", "TP_MOD_CONCORRENCIA", "DS_MOD_CONCORRENCIA", "QT_VAGAS_CONCORRENCIA"
)
missing_vagas_cols <- setdiff(required_vagas_cols, names(vagas_df))

if (length(missing_vagas_cols) > 0) {
  stop(paste("Missing required seats columns:", paste(missing_vagas_cols, collapse = ", ")))
}

seat_join_keys <- c(
  "NU_ANO", "NU_EDICAO", "CO_IES", "CO_IES_CURSO",
  paste0(text_join_cols, "_join")
)

vagas_lookup <- vagas_df %>%
  group_by(across(all_of(intersect(seat_join_keys, names(vagas_df))))) %>%
  summarise(
    QT_VAGAS_CONCORRENCIA_vagas = sum(QT_VAGAS_CONCORRENCIA, na.rm = TRUE),
    DS_ORGANIZACAO_ACADEMICA_vagas = first(na.omit(DS_ORGANIZACAO_ACADEMICA)),
    DS_CATEGORIA_ADM_vagas = first(na.omit(DS_CATEGORIA_ADM)),
    SG_UF_CAMPUS_vagas = first(na.omit(SG_UF_CAMPUS)),
    NO_MUNICIPIO_CAMPUS_vagas = first(na.omit(NO_MUNICIPIO_CAMPUS)),
    DS_REGIAO_CAMPUS_vagas = first(na.omit(DS_REGIAO_CAMPUS)),
    .groups = "drop"
  )

cutoff_df <- cutoff_df %>%
  left_join(vagas_lookup, by = intersect(seat_join_keys, names(cutoff_df))) %>%
  mutate(
    QT_VAGAS_CONCORRENCIA = coalesce(QT_VAGAS_CONCORRENCIA, QT_VAGAS_CONCORRENCIA_vagas),
    DS_ORGANIZACAO_ACADEMICA = coalesce(DS_ORGANIZACAO_ACADEMICA, DS_ORGANIZACAO_ACADEMICA_vagas),
    DS_CATEGORIA_ADM = coalesce(DS_CATEGORIA_ADM, DS_CATEGORIA_ADM_vagas),
    SG_UF_CAMPUS = coalesce(SG_UF_CAMPUS, SG_UF_CAMPUS_vagas),
    NO_MUNICIPIO_CAMPUS = coalesce(NO_MUNICIPIO_CAMPUS, NO_MUNICIPIO_CAMPUS_vagas),
    DS_REGIAO_CAMPUS = coalesce(DS_REGIAO_CAMPUS, DS_REGIAO_CAMPUS_vagas)
  ) %>%
  select(-ends_with("_vagas"))

fallback_seat_join_keys <- setdiff(seat_join_keys, "CO_IES_CURSO")

vagas_lookup_fallback <- vagas_df %>%
  group_by(across(all_of(intersect(fallback_seat_join_keys, names(vagas_df))))) %>%
  summarise(
    QT_VAGAS_CONCORRENCIA_fallback = sum(QT_VAGAS_CONCORRENCIA, na.rm = TRUE),
    DS_ORGANIZACAO_ACADEMICA_fallback = first(na.omit(DS_ORGANIZACAO_ACADEMICA)),
    DS_CATEGORIA_ADM_fallback = first(na.omit(DS_CATEGORIA_ADM)),
    SG_UF_CAMPUS_fallback = first(na.omit(SG_UF_CAMPUS)),
    NO_MUNICIPIO_CAMPUS_fallback = first(na.omit(NO_MUNICIPIO_CAMPUS)),
    DS_REGIAO_CAMPUS_fallback = first(na.omit(DS_REGIAO_CAMPUS)),
    .groups = "drop"
  )

cutoff_df <- cutoff_df %>%
  left_join(vagas_lookup_fallback, by = intersect(fallback_seat_join_keys, names(cutoff_df))) %>%
  mutate(
    QT_VAGAS_CONCORRENCIA = coalesce(QT_VAGAS_CONCORRENCIA, QT_VAGAS_CONCORRENCIA_fallback),
    DS_ORGANIZACAO_ACADEMICA = coalesce(DS_ORGANIZACAO_ACADEMICA, DS_ORGANIZACAO_ACADEMICA_fallback),
    DS_CATEGORIA_ADM = coalesce(DS_CATEGORIA_ADM, DS_CATEGORIA_ADM_fallback),
    SG_UF_CAMPUS = coalesce(SG_UF_CAMPUS, SG_UF_CAMPUS_fallback),
    NO_MUNICIPIO_CAMPUS = coalesce(NO_MUNICIPIO_CAMPUS, NO_MUNICIPIO_CAMPUS_fallback),
    DS_REGIAO_CAMPUS = coalesce(DS_REGIAO_CAMPUS, DS_REGIAO_CAMPUS_fallback)
  ) %>%
  select(-ends_with("_fallback"), -ends_with("_join"))

# Importing dataset manually classified
classified_concorrencia_df <- read_excel(file.path(raw_dir, "SISU/DS_MOD_CONCORRENCIA_Unique_Values_CLASSIFIED.xlsx")) %>%
  select(Unique_DS_MOD_CONCORRENCIA_Values, Classification) %>%
  distinct()

cutoff_df <- cutoff_df %>%
  mutate(
    DS_MOD_CONCORRENCIA_cleaned = iconv(DS_MOD_CONCORRENCIA, from = "UTF-8", to = "ASCII//TRANSLIT"),
    DS_MOD_CONCORRENCIA_cleaned = str_to_sentence(DS_MOD_CONCORRENCIA_cleaned)
  ) %>%
  left_join(classified_concorrencia_df,
    by = c("DS_MOD_CONCORRENCIA_cleaned" = "Unique_DS_MOD_CONCORRENCIA_Values")) %>%
  mutate(Classification = if_else(is.na(Classification), "Others", Classification))

# Importing dataset manually classified of Courses
classified_major_df <- read_excel(file.path(raw_dir, "SISU/NO_CURSO_Unique_Values_CLASSIFIED_v2.xlsx")) %>%
  select(Unique_NO_CURSO_Values, Major) %>%
  distinct()

cutoff_df <- cutoff_df %>%
  mutate(
    NO_CURSO_cleaned = iconv(NO_CURSO, from = "UTF-8", to = "ASCII//TRANSLIT"),
    NO_CURSO_cleaned = str_to_sentence(NO_CURSO_cleaned)
  ) %>%
  left_join(classified_major_df, by = c("NO_CURSO_cleaned" = "Unique_NO_CURSO_Values")) %>%
  mutate(Major = if_else(is.na(Major), "Others", Major))

# Affirmative action classification
cutoff_df <- cutoff_df %>%
  mutate(
    TP_MOD_CONCORRENCIA_cleaned = iconv(TP_MOD_CONCORRENCIA, from = "", to = "ASCII//TRANSLIT"),
    Affirmative_Action = case_when(
      str_detect(TP_MOD_CONCORRENCIA_cleaned, regex("Ampla Concorr", ignore_case = TRUE)) ~ 0,
      str_detect(TP_MOD_CONCORRENCIA_cleaned, regex("Acoes Afirmativas|Lei de Cotas|Lei n|Acao afirmativa", ignore_case = TRUE)) ~ 1,
      TRUE ~ if_else(Classification == "No Reserved", 0, 1)
    ),
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

# University localization
universities_geo <- read.csv(file.path(raw_dir, "Universities_Geoinfo.csv")) %>%
  select(id_ies, id_municipio) %>%
  distinct() %>%
  mutate(id_ies = as.character(id_ies), id_municipio = as.character(id_municipio))

microregion_df <- read_excel(file.path(raw_dir, "IBGE/Municipality_Regions_Composition.xlsx")) %>%
  select(Code_Municipality, Code_State, Code_Microregion) %>%
  mutate(Code_Municipality = as.character(Code_Municipality))

geo_info <- universities_geo %>%
  left_join(microregion_df, by = c("id_municipio" = "Code_Municipality")) %>%
  rename(micro_reg_code = Code_Microregion, state_code = Code_State) %>%
  rename(co_ies = id_ies, co_municipio = id_municipio)

cutoff_df <- cutoff_df %>%
  mutate(CO_IES_join = as.character(CO_IES)) %>%
  left_join(geo_info, by = c("CO_IES_join" = "co_ies")) %>%
  select(-CO_IES_join) %>%
  rename(year = NU_ANO, edition = NU_EDICAO, co_ies = CO_IES) %>%
  relocate(year, edition, co_ies, micro_reg_code, state_code) %>%
  relocate(Major, .after = NO_CURSO) %>%
  relocate(Classification, .after = DS_MOD_CONCORRENCIA) %>%
  select(-DS_MOD_CONCORRENCIA_cleaned, -NO_CURSO_cleaned, -TP_MOD_CONCORRENCIA_cleaned) %>%
  arrange(year, edition, co_ies, CO_IES_CURSO, TP_MOD_CONCORRENCIA, DS_MOD_CONCORRENCIA)

missing_cutoffs <- sum(is.na(cutoff_df$NU_NOTACORTE))
missing_seats <- sum(is.na(cutoff_df$QT_VAGAS_CONCORRENCIA))
missing_applications <- sum(is.na(cutoff_df$QT_INSCRICAO))

if (missing_cutoffs > 0) {
  warning(paste("Rows with missing cutoff scores in NU_NOTACORTE:", missing_cutoffs))
}

if (missing_seats > 0) {
  warning(paste("Rows with missing seats in QT_VAGAS_CONCORRENCIA:", missing_seats))
}

if (missing_applications > 0) {
  warning(paste("Rows with missing applications in QT_INSCRICAO:", missing_applications))
}

write_dta(cutoff_df, file.path(output_dir, "cutoffs_SISU.dta"))

# Aggregating cutoffs by year, university, major and quota classification
cutoff_turn_level <- cutoff_df %>%
  filter(DS_GRAU == "Bacharelado") %>%
  group_by(year, co_ies, micro_reg_code, Major, Classification, DS_TURNO) %>%
  summarise(
    Cutoff_Score_Turno = weighted_mean_or_na(NU_NOTACORTE, QT_VAGAS_CONCORRENCIA),
    Seats_Total_Turno = sum_or_na(QT_VAGAS_CONCORRENCIA),
    Applications_Total_Turno = sum_or_na(QT_INSCRICAO),
    .groups = "drop"
  )

cutoff_univ_major_aa <- cutoff_turn_level %>%
  group_by(year, co_ies, micro_reg_code, Major, Classification) %>%
  summarise(
    Cutoff_Score = weighted_mean_or_na(Cutoff_Score_Turno, Seats_Total_Turno),
    Seats_Total = sum_or_na(Seats_Total_Turno),
    Applications_Total = sum_or_na(Applications_Total_Turno),
    Number_Turnos = n_distinct(DS_TURNO),
    .groups = "drop"
  )

quota_seats_univ_major_aa <- cutoff_df %>%
  filter(DS_GRAU == "Bacharelado") %>%
  group_by(year, co_ies, micro_reg_code, Major, Classification) %>%
  summarise(
    Seats_AA = sum(QT_VAGAS_CONCORRENCIA * Affirmative_Action, na.rm = TRUE),
    Seats_Not_reserved = sum(QT_VAGAS_CONCORRENCIA * Not_reserved, na.rm = TRUE),
    Seats_Public_School = sum(QT_VAGAS_CONCORRENCIA * Public_School, na.rm = TRUE),
    Seats_Racial = sum(QT_VAGAS_CONCORRENCIA * Racial, na.rm = TRUE),
    Seats_Low_Income = sum(QT_VAGAS_CONCORRENCIA * Low_Income, na.rm = TRUE),
    Seats_Disability = sum(QT_VAGAS_CONCORRENCIA * Disability, na.rm = TRUE),
    Seats_Others = sum(QT_VAGAS_CONCORRENCIA * Others, na.rm = TRUE),
    .groups = "drop"
  )

cutoff_univ_major_aa <- cutoff_univ_major_aa %>%
  left_join(quota_seats_univ_major_aa, by = c("year", "co_ies", "micro_reg_code", "Major", "Classification")) %>%
  mutate(
    Seats_Not_reserved = if_else(Classification == "No Reserved", Seats_Total, Seats_Not_reserved),
    Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total
  ) %>%
  arrange(co_ies, micro_reg_code, Major, Classification, year)

write_dta(cutoff_univ_major_aa, file.path(output_dir, "cutoffs_SISU_univ_major_aa_year.dta"))
