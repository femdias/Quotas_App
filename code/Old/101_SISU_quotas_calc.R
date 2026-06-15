# Loading necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx) 
library(writexl) 
library(ggplot2)
library(haven)
library(stringr)

# Setting the working directory
setwd("C:/Users/fm469/Box/Brazil Paper (Felipe Macedo Dias)")

# Function to create dummy for a variation of more than 25% in the Shares of AA between 2012 and 2016
create_treatment_dummies <- function(df, group_var) {
  group_var_sym <- sym(group_var)

  # Filtering for relevant years and share columns
  df_shares_2012_2016 <- df %>% filter(year %in% c(2012, 2016)) %>%
    select(!!group_var_sym, year, Share_Seats_AA, Share_Seats_Public_School, Share_Seats_Racial, Share_Seats_Low_Income)

  # Pivoting to compare 2012 and 2016 side-by-side
  treatment_df <- df_shares_2012_2016 %>%
    pivot_wider(names_from = year,
      values_from = c(Share_Seats_AA, Share_Seats_Public_School, Share_Seats_Racial, Share_Seats_Low_Income) ) %>%
    # Removing units that don't have data for both years
    filter(!is.na(Share_Seats_AA_2012) & !is.na(Share_Seats_AA_2016)) %>%
    # Calculating the increase in shares
    mutate(
      Increase_AA = Share_Seats_AA_2016 - Share_Seats_AA_2012,
      Increase_Public_School = Share_Seats_Public_School_2016 - Share_Seats_Public_School_2012,
      Increase_Racial = Share_Seats_Racial_2016 - Share_Seats_Racial_2012,
      Increase_Low_Income = Share_Seats_Low_Income_2016 - Share_Seats_Low_Income_2012) %>%
    # Creating the treatment dummies
    mutate(Treated_AA = ifelse(Increase_AA > 0.25, 1, 0),
      Treated_Public_School = ifelse(Increase_Public_School > 0.25, 1, 0),
      Treated_Racial = ifelse(Increase_Racial > 0.25, 1, 0),
      Treated_Low_Income = ifelse(Increase_Low_Income > 0.25, 1, 0)) %>%
    # Selecting the identifier and the new dummy variables
    select(!!group_var_sym, starts_with("Treated_"))

  return(treatment_df)
}


# Listing all Excel files in the input directory
all_files <- list.files(path = "Data//raw//SISU//SISU aggregated", pattern = "\\.xlsx$", full.names = TRUE)

# Defining column name mappings for 'Inscrições e notas de corte' files
inscricoes_rename_map <- c(
  "EDIÇÃO" = "EDICAO",
  "CÓD. IES" = "CO_IES",
  "NOME IES" = "NO_IES",
  "SIGLA IES" = "SG_IES",
  "CAMPUS" = "NO_CAMPUS",
  "CÓD CURSO" = "CO_IES_CURSO",
  "NOME CURSO" = "NO_CURSO",
  "GRAU" = "DS_GRAU",
  "TURNO" = "DS_TURNO",
  "TIPO MODALIDADE" = "TP_MOD_CONCORRENCIA",
  "TP_MODALIDADE" = "TP_MOD_CONCORRENCIA",
  "MODALIDADE CONCORRÊNCIA" = "DS_MOD_CONCORRENCIA",
  "PERCENTUAL DE BÔNUS" = "NU_PERCENTUAL_BONUS",
  "NOTA DE CORTE" = "NU_NOTACORTE")

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
  "NOTA_MI\u008dNIMA_REDACAO" = "NOTA_MINIMA_REDACAO"
)

# Function to standardize column names
standardize_column_names <- function(df, mapping) {
  # Looping aroung mapping to rename column
  for (old_name in names(mapping)) {
    new_name <- mapping[[old_name]]
    # Checking if the old column name exists
    if (old_name %in% names(df)) {
      # Renaming the column
      names(df)[names(df) == old_name] <- new_name
    }
  }
  return(df)
}

# Initializing empty data frames for appending
vagas_ofertadas_df <- tibble()
inscricoes_notas_corte_df <- tibble()

# Looping through each file to read and append data
for (file in all_files) {

  file_name <- basename(file)

  # Reading files based on naming convention.
  if (grepl("PORTAL_Sisu 2010 a 2018", file_name)) {
    temp_df <- read_excel(file, sheet = 1, skip = 4, col_types = "text")
  } else {
    temp_df <- read_excel(file, sheet = 2, col_types = "text")
  }

  # Appending data based on file type.
  if (grepl("Vagas ofertadas", file_name)) {
    vagas_ofertadas_df <- bind_rows(vagas_ofertadas_df, standardize_column_names(temp_df, vagas_rename_map))
  } else if (grepl("Inscrições e notas de corte", file_name) || grepl("Nota de corte", file_name)) {
    inscricoes_notas_corte_df <- bind_rows(inscricoes_notas_corte_df, standardize_column_names(temp_df, inscricoes_rename_map))
  }
}

# Splitting 'EDICAO' into 'NU_ANO' and 'NU_EDICAO', for inscricoes_notas_corte_df
inscricoes_notas_corte_df <- inscricoes_notas_corte_df %>%
  mutate(temp_NU_ANO = ifelse(grepl("/", EDICAO), sub("/.*", "", EDICAO), NA_character_),
        temp_NU_EDICAO = ifelse(grepl("/", EDICAO), sub(".*/", "", EDICAO), NA_character_)) %>%
  mutate(NU_ANO = coalesce(NU_ANO, temp_NU_ANO), 
        NU_EDICAO = coalesce(NU_EDICAO, temp_NU_EDICAO) ) %>%
  select(-EDICAO, -starts_with("temp_"))


# Splitting 'EDICAO' into 'NU_ANO' and 'NU_EDICAO', for vagas_ofertadas_df
vagas_ofertadas_df <- vagas_ofertadas_df %>%
  mutate(temp_NU_ANO = ifelse(grepl("/", EDICAO), sub("/.*", "", EDICAO), NA_character_),
        temp_NU_EDICAO = ifelse(grepl("/", EDICAO), sub(".*/", "", EDICAO), NA_character_)) %>%
  mutate(NU_ANO = coalesce(NU_ANO, temp_NU_ANO),
        NU_EDICAO = coalesce(NU_EDICAO, temp_NU_EDICAO)) %>%
  select(-EDICAO, -starts_with("temp_"))

# Unifying QT_VAGAS_OFERTADAS and QT_VAGAS_CONCORRENCIA in vagas_ofertadas_df
vagas_ofertadas_df <- vagas_ofertadas_df %>%
    mutate(QT_VAGAS_OFERTADAS = coalesce(QT_VAGAS_OFERTADAS, QT_VAGAS_CONCORRENCIA)) %>%
    select(-QT_VAGAS_CONCORRENCIA)

# Defining common numeric columns for 'vagas_ofertadas_df'
numeric_vagas_cols <- c(
  "QT_SEMESTRE", "NU_VAGAS_AUTORIZADAS", "QT_VAGAS_OFERTADAS", "NU_PERCENTUAL_BONUS", "PESO_REDACAO",
  "NOTA_MINIMA_REDACAO", "PESO_LINGUAGENS", "NOTA_MINIMA_LINGUAGENS", "PESO_MATEMATICA",
  "NOTA_MINIMA_MATEMATICA", "PESO_CIENCIAS_HUMANAS", "NOTA_MINIMA_CIENCIAS_HUMANAS",
  "PESO_CIENCIAS_NATUREZA", "NOTA_MINIMA_CIENCIAS_NATUREZA", "NU_MEDIA_MINIMA_ENEM",
  "PERC_UF_PRE_PPI", "PERC_UF_PPID", "PERC_UF_PP", "PERC_UF_I", "NU_PERC_LEI",
  "NU_PERC_PPI", "NU_PERC_PP", "NU_PERC_I", "NU_PERC_PPI_DEF","NU_ANO", "NU_EDICAO"
)

# Converting these columns to numeric
for (col in numeric_vagas_cols) {
  if (col %in% names(vagas_ofertadas_df)) {
    vagas_ofertadas_df[[col]] <- as.numeric(vagas_ofertadas_df[[col]])
  }
}

# Defining common numeric columns for 'inscricoes_notas_corte_df'
numeric_inscricoes_cols <- c(
  "QT_VAGAS_CONCORRENCIA", "QT_INSCRICAO", "NU_PERCENTUAL_BONUS", "NU_NOTACORTE",
  "NU_ANO", "NU_EDICAO"
)
# Converting these columns to numeric
#for (col in numeric_inscricoes_cols) {
#  if (col %in% names(inscricoes_notas_corte_df)) {
#    inscricoes_notas_corte_df[[col]] <- as.numeric(inscricoes_notas_corte_df[[col]])
#  }
#}

# Sorting by year, edition, university code and major code
vagas_ofertadas_df <- vagas_ofertadas_df %>% arrange(NU_ANO, NU_EDICAO, CO_IES, CO_IES_CURSO)
#inscricoes_notas_corte_df <- inscricoes_notas_corte_df %>% arrange(NU_ANO, NU_EDICAO, CO_IES, CO_IES_CURSO)


# # selecting unique DS_MOD_CONCORRENCIA
# concorrencia_types_with_freq <- vagas_ofertadas_df %>%
#   mutate(DS_MOD_CONCORRENCIA_cleaned = iconv(DS_MOD_CONCORRENCIA, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
#   mutate(DS_MOD_CONCORRENCIA_cleaned = str_to_sentence(DS_MOD_CONCORRENCIA_cleaned)) %>%
#   count(DS_MOD_CONCORRENCIA_cleaned, name = "Frequency") %>%
#   arrange(desc(Frequency)) %>%
#   rename(Unique_DS_MOD_CONCORRENCIA_Values = DS_MOD_CONCORRENCIA_cleaned)

# # Exporting the list of unique values to an XLSX file
# write_xlsx(concorrencia_types_with_freq, "Data\\intermediate\\temp\\DS_MOD_CONCORRENCIA_Unique_Values.xlsx")

# # selecting unique NO_CURSO
# course_types_with_freq <- vagas_ofertadas_df %>%
#   mutate(NO_CURSO_cleaned = iconv(NO_CURSO, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
#   mutate(NO_CURSO_cleaned = str_to_sentence(NO_CURSO_cleaned)) %>%
#   count(NO_CURSO_cleaned, name = "Frequency") %>%
#   arrange(desc(Frequency)) %>%
#   rename(Unique_NO_CURSO_Values = NO_CURSO_cleaned)

# # Exporting the list of unique values to an XLSX file
# write_xlsx(course_types_with_freq, "Data\\intermediate\\temp\\NO_CURSO_Unique_Values.xlsx")





# Importing dataset manually classified
classified_concorrencia_df <- read_excel("Data\\intermediate\\temp\\DS_MOD_CONCORRENCIA_Unique_Values_CLASSIFIED.xlsx")
classified_concorrencia_df <- classified_concorrencia_df %>% select(Unique_DS_MOD_CONCORRENCIA_Values, Classification)

# Cleaning the DS_MOD_CONCORRENCIA column (removing accent marks from the text and making only the first letter uppercase for consistency)
vagas_ofertadas_df_cleaned <- vagas_ofertadas_df %>%
  mutate(DS_MOD_CONCORRENCIA_cleaned = iconv(DS_MOD_CONCORRENCIA, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(DS_MOD_CONCORRENCIA_cleaned = str_to_sentence(DS_MOD_CONCORRENCIA_cleaned))

# Merging the classified data into the original dataset, by the cleaned competition mode string
vagas_ofertadas_class <- vagas_ofertadas_df_cleaned %>%
  left_join(classified_concorrencia_df, by = c("DS_MOD_CONCORRENCIA_cleaned" = "Unique_DS_MOD_CONCORRENCIA_Values"))

# Handling missing classifications
# Replacing NA values in the new 'Manual_Classification' column with "Others"
vagas_ofertadas_class <- vagas_ofertadas_class %>% mutate(Classification = ifelse(is.na(Classification), "Others", Classification))


# Importing dataset manually classified of Courses
classified_major_df <- read_excel("Data\\intermediate\\temp\\NO_CURSO_Unique_Values_CLASSIFIED.xlsx")
classified_major_df <- classified_major_df %>% select(Unique_NO_CURSO_Values, Major)

# Cleaning the NO_CURSO column (removing accent marks from the text and making only the first letter uppercase for consistency)
vagas_ofertadas_class_cleaned <- vagas_ofertadas_class %>%
  mutate(NO_CURSO_cleaned = iconv(NO_CURSO, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(NO_CURSO_cleaned = str_to_sentence(NO_CURSO_cleaned))

# Merging the classified data into the original dataset, by the cleaned competition mode string
vagas_ofertadas_class_cleaned <- vagas_ofertadas_class_cleaned %>%
  left_join(classified_major_df, by = c("NO_CURSO_cleaned" = "Unique_NO_CURSO_Values"))

# Replacing NA values in the new 'Major' column with "Others"
vagas_ofertadas_final <- vagas_ofertadas_class_cleaned %>% mutate(Major = ifelse(is.na(Major), "Others", Major))

# Ordering
vagas_ofertadas_final <- vagas_ofertadas_final %>% relocate(Major, .after = NO_CURSO) %>% relocate(Classification, .after = DS_MOD_CONCORRENCIA)

# Saving Courses "Majors"
Majors_course_df <- vagas_ofertadas_final %>%  select(c("CO_IES", "CO_IES_CURSO", "Major"))  %>% 
  distinct() %>% mutate(CO_IES_CURSO = as.numeric(CO_IES_CURSO)) %>% 
  arrange(CO_IES, CO_IES_CURSO, Major) 


write.csv(Majors_course_df, "Data/intermediate/Majors_SISU.csv", row.names = FALSE)
write_dta(Majors_course_df, "Data/intermediate/Majors_SISU.dta")


#### Selecting only "University" and Federal Centers/Institutes (removing State Centers)

# First, filling the missing values of DS_ORGANIZACAO_ACADEMICA (idk why there are some missings)
vagas_ofertadas_filled <- vagas_ofertadas_final %>% group_by(CO_IES) %>%
                        fill(DS_ORGANIZACAO_ACADEMICA, .direction = "downup") %>% ungroup()

# Now, safely applying your original filter to the filled data
vagas_ofertadas_universities <- vagas_ofertadas_filled %>% filter(DS_ORGANIZACAO_ACADEMICA %in% c("Universidade",
                              "Faculdade", "Instituto Federal de Educação, Ciência e Tecnologia", "Centro Federal de Educação Tecnológica"))

# Also drop this one that have different classifications through time
vagas_ofertadas_final <- vagas_ofertadas_final %>% filter(NO_IES != "CENTRO UNIVERSITÁRIO ESTADUAL DA ZONA OESTE")



#### Affirmative action classification 

# Aggregating TP_MODALIDADE into a yes/no dummy for Affirmative_Action 
vagas_ofertadas_final <- vagas_ofertadas_final %>%
  mutate(Affirmative_Action = case_when(
      grepl("Ampla Concorrência", TP_MODALIDADE, ignore.case = TRUE) ~ 0,
      grepl("Ações Afirmativas|Lei de Cotas|Lei nº 12.711/2012 - Lei de cotas|Ação afirmativa própria da instituição - tipo rese|Ação afirmativa própria da instituição - tipo bôn", TP_MODALIDADE, ignore.case = TRUE) ~ 1))


# Aggregating Quotas Classifications into dummies
vagas_ofertadas_final <- vagas_ofertadas_final %>%
  mutate(
    # Dummy for seats not reserved
    Not_reserved = if_else(Classification == "No Reserved", 1, 0),

    # Dummy for public school quotas
    Public_School = if_else(Classification %in% c("Public School", 
                                                "Public School non-white",
                                                "Public School low-income",
                                                "Public School non-white low-income"), 1, 0),

    # Creating a dummy for racial quotas
    Racial = if_else(Classification %in% c("Non-white",
                                         "Public School non-white",
                                         "Non-white low-income",
                                         "Public School non-white low-income"), 1, 0),

    # Creating a dummy for low-income quotas
    Low_Income = if_else(Classification %in% c("Low-income",
                                             "Non-white low-income",
                                             "Public School low-income",
                                             "Public School non-white low-income"), 1, 0),

    # Creating a dummy for disability quotas
    Disability = if_else(Classification %in% c("Special Needs",
                                             "Low-income special needs",
                                             "Non-white special needs",
                                             "Public School special needs",
                                             "Public School low-income special needs",
                                             "Public School non-white special needs",
                                             "Public School non-white low-income special needs"), 1, 0),

    # Creating a dummy for other categories
    Others = if_else(Classification %in% c("Others", "LGBT", "Regional"), 1, 0)
  )




# Summarizing the number of observations and total seats for each dummy variable

summaries_list <- list()

# Looping through each dummy column
for (col in c("Not_reserved", "Public_School", "Racial", "Low_Income", "Disability", "Others")) {
  # Creating a summary for the current dummy column
  summary_df <- vagas_ofertadas_final %>% filter(!!sym(col) == 1) %>% 
                                          summarise(Count_Observations = n(), Total_Seats = sum(QT_VAGAS_OFERTADAS, na.rm = TRUE) )
                                          summaries_list[[col]] <- summary_df
}

# Combining all summaries into a single data frame for a clean display
final_summary <- bind_rows(summaries_list, .id = "Dummy_Variable")
print(final_summary)

# Dummy_Variable      Count_Observations Total_Seats
# 1 Not_reserved                73246     1459676
# 2 Public_School              228075     1073594
# 3 Racial                     131920      656913
# 4 Low_Income                 111633      515407
# 5 Disability                  80952      103281
# 6 Others                       8319       12165




#---------------------------------------------------------------------------
# Adding Municipality and microregion
#---------------------------------------------------------------------------
 
# Superior Census (multiple, so we have basically all the university/ courses/ campus)
superior_census_2010 <- read_dta("Data//raw//Superior Census//DTA files//HEI_2010.dta")
superior_census_2013 <- read_dta("Data//raw//Superior Census//DTA files//HEI_2013.dta")
superior_census_2016 <- read_dta("Data//raw//Superior Census//DTA files//HEI_2016.dta")
superior_census_2019 <- read_dta("Data//raw//Superior Census//DTA files//HEI_2019.dta")
superior_census_2022 <- read_dta("Data//raw//Superior Census//DTA files//HEI_2022.dta")

# Appending 
superior_census1 <- superior_census_2010 %>% bind_rows(superior_census_2013, superior_census_2016, 
                                                   superior_census_2019, superior_census_2022)

# Selecting only Presencial courses (not Remote), grouping by University and Municiaplity, summing number of seats
superior_census2 <- superior_census1 %>% filter(tp_modalidade_ensino == 1)  %>%
                                        group_by(co_ies, co_municipio) %>% 
                                        summarise(qt_vg_total = sum(qt_vg_total, na.rm = TRUE))  %>% 
                                        arrange(co_ies, desc(qt_vg_total))
  
# Selecting the first appearance (the campus that have the largest # of students) and selecting important columns 
superior_census2 <- superior_census2 %>%  distinct(co_ies, .keep_all = TRUE) %>%
                                          select(co_ies, co_municipio) %>%
                                          mutate(co_ies = as.character(co_ies), co_municipio = as.character(co_municipio))
                                  
# IBGE's table relating municipality code and microregion
microregion_df <- read_excel("Data//raw//IBGE//Municipality_Regions_Composition.xlsx")
microregion_df <- microregion_df %>% select(Code_Municipality, Code_State, Code_Microregion)

# Merging Microregion with Superior Census localization
geo_info <- superior_census2 %>% left_join(microregion_df, by = c("co_municipio" = "Code_Municipality")) %>%
                                  rename(micro_reg_code = Code_Microregion, state_code = Code_State) %>% 
                                  mutate(co_ies = as.character(co_ies))

# Saving this dataset for later
write.csv(geo_info, "Data/intermediate/Universities_Geoinfo.csv", row.names = FALSE)


# Merging Localization with the Seats  dataset
vagas_ofertadas_final_geo <- vagas_ofertadas_final %>% left_join(geo_info, by = c("CO_IES" = "co_ies"))


# Sorting by university and year 
vagas_ofertadas_final_geo <- vagas_ofertadas_final_geo %>% mutate(CO_IES = as.numeric(CO_IES)) %>% arrange(CO_IES, NU_ANO)

# Dropping unecessaries variables 
to_keep <- c("NU_ANO", "NU_EDICAO", "CO_IES", "NO_IES", "SG_IES", "DS_ORGANIZACAO_ACADEMICA", 
            "DS_CATEGORIA_ADM", "NO_CAMPUS", "NO_MUNICIPIO_CAMPUS", "SG_UF_CAMPUS", "CO_IES_CURSO",
            "NO_CURSO", "Major", "DS_GRAU",  "NU_PERCENTUAL_BONUS", "Classification", "QT_VAGAS_OFERTADAS",
            "Affirmative_Action",  "Not_reserved", "Public_School", "Racial", "Low_Income", "Disability", "Others",
             "micro_reg_code", "state_code")

vagas_ofertadas_final_geo <- vagas_ofertadas_final_geo %>% select(all_of(to_keep))

# Saving
write.csv(vagas_ofertadas_final_geo, "Data/intermediate/seats_SISU.csv", row.names = FALSE)
#write.csv(inscricoes_notas_corte_df, "Data/intermediate/cut_off_SISU.csv", row.names = FALSE)



vagas_ofertadas_final_geo <- read.csv("Data/intermediate/seats_SISU.csv")


#---------------------------------------------------------------------------
# Aggregated number of quota/non-quotas spot per MAJOR and university per year
#---------------------------------------------------------------------------

# Generating Total dummy, equal to 1 in every obs
SISU_seats <- vagas_ofertadas_final_geo %>% mutate(Total = 1)  %>% rename(year = NU_ANO)

# Creating a list of the dummy column names to loop through
dummy_columns <- c("Total", "Affirmative_Action","Not_reserved", "Public_School", 
                  "Racial", "Low_Income", "Disability", "Others")

# Initializing an empty dataframe to store the final combined pivoted data
major_pivoted_df <- SISU_seats %>% distinct(year, CO_IES, Major, micro_reg_code, state_code) %>% 
                                         arrange(year, CO_IES, Major, micro_reg_code, state_code)

# Looping through each dummy variable to create a separate pivot and then joining
for (col in dummy_columns) {
  # Aggregating seats for the current dummy variable
  temp_seats_df <- SISU_seats %>% group_by(year, CO_IES, Major, micro_reg_code, state_code) %>%
    # Summing QT_VAGAS_OFERTADAS only where the dummy is 1
    summarise(Seats_Dummy = sum(QT_VAGAS_OFERTADAS * !!sym(col), na.rm = TRUE), .groups = 'drop') %>%
    # Renaming the Seats_Dummy column to reflect the current dummy's name
    rename(!!sym(paste0("Seats_", col)) := Seats_Dummy)

  # Joining the temporary dataframe to the final dataframe
  major_pivoted_df <- major_pivoted_df %>% left_join(temp_seats_df, by = c("year", "CO_IES", "Major", "micro_reg_code", "state_code"))
}

# Filling any NA values (where a university had no seats for a specific dummy type) with 0
major_pivoted_df <- major_pivoted_df %>% mutate(across(starts_with("Seats_"), ~replace_na(., 0))) %>%
                                        rename(Seats_AA = Seats_Affirmative_Action)

# Calculating the shares of seats by category
major_pivoted_df2 <- major_pivoted_df %>% mutate(Share_Seats_AA = Seats_AA / Seats_Total,
                                                Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
                                                Share_Seats_Public_School = Seats_Public_School / Seats_Total,
                                                Share_Seats_Racial = Seats_Racial / Seats_Total,
                                                Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
                                                Share_Seats_Disability = Seats_Disability / Seats_Total,
                                                Share_Seats_Others = Seats_Others / Seats_Total)

# Adding treatment dummies (+30% variation between 2012 and 2016)
dummies <- create_treatment_dummies(major_pivoted_df2, "Major")
major_pivoted_df2 <- major_pivoted_df2 %>% left_join(dummies, by = "Major") %>%
  mutate(across(starts_with("Treated_"), ~replace_na(., 0)))

# Sorting and 
major_pivoted_df2 <- major_pivoted_df2 %>% arrange(CO_IES, Major, year)

# Saving
write.csv(major_pivoted_df2, "Data//clean//Quotas_Share//quotas_SISU_major_univ_level.csv", row.names = FALSE)
write_dta(major_pivoted_df2, "Data//clean//Quotas_Share//quotas_SISU_major_univ_level.dta")







#---------------------------------------------------------------------------
# Aggregated number of quota/non-quotas spot per university per year
#---------------------------------------------------------------------------

# Initializing an empty dataframe to store the final combined pivoted data
uni_pivoted_df <- SISU_seats %>% distinct(year, CO_IES, micro_reg_code, state_code) %>% arrange(year, CO_IES, micro_reg_code, state_code)

# Looping through each dummy variable to create a separate pivot and then joining
for (col in dummy_columns) {
  # Aggregating seats for the current dummy variable
  temp_seats_df_uni <- SISU_seats %>% group_by(year, CO_IES) %>%
    # Summing QT_VAGAS_OFERTADAS only where the dummy is 1
    summarise(Seats_Dummy = sum(QT_VAGAS_OFERTADAS * !!sym(col), na.rm = TRUE), .groups = 'drop') %>%
    # Renaming the Seats_Dummy column to reflect the current dummy's name
    rename(!!sym(paste0("Seats_", col)) := Seats_Dummy)

  # Joining the temporary dataframe to the final dataframe
  uni_pivoted_df <- uni_pivoted_df %>% left_join(temp_seats_df_uni, by = c("year", "CO_IES"))
}

# Filling any NA values (where a university had no seats for a specific dummy type) with 0
uni_pivoted_df <- uni_pivoted_df %>% mutate(across(starts_with("Seats_"), ~replace_na(., 0))) %>%
                                        rename(Seats_AA = Seats_Affirmative_Action)

# Calculating the shares of seats by category
uni_pivoted_df2 <- uni_pivoted_df %>% mutate(Share_Seats_AA = Seats_AA / Seats_Total,
                                                Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
                                                Share_Seats_Public_School = Seats_Public_School / Seats_Total,
                                                Share_Seats_Racial = Seats_Racial / Seats_Total,
                                                Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
                                                Share_Seats_Disability = Seats_Disability / Seats_Total,
                                                Share_Seats_Others = Seats_Others / Seats_Total) %>% arrange(CO_IES, year, micro_reg_code)

write.csv(uni_pivoted_df2, "Data//clean//Quotas_Share//quotas_SISU_univ_level.csv", row.names = FALSE)
write_dta(uni_pivoted_df2, "Data//clean//Quotas_Share//quotas_SISU_univ_level.dta")









#------------------#
#  By Microregion  #
#------------------#


# Grouping by microregion and year
SISU_df_microreg <- uni_pivoted_df2 %>% group_by(micro_reg_code, year)  %>%
  summarise(Seats_Total = sum(Seats_Total, na.rm = TRUE),
            Seats_AA = sum(Seats_AA, na.rm = TRUE),
            Seats_Not_reserved = sum(Seats_Not_reserved, na.rm = TRUE),
            Seats_Public_School = sum(Seats_Public_School, na.rm = TRUE),
            Seats_Racial = sum(Seats_Racial, na.rm = TRUE),
            Seats_Low_Income = sum(Seats_Low_Income, na.rm = TRUE),
            Seats_Disability = sum(Seats_Disability, na.rm = TRUE),
            Seats_Others = sum(Seats_Others, na.rm = TRUE),
            .groups = 'drop')

# Calculating the Shares
SISU_df_microreg_shares <- SISU_df_microreg %>%
  mutate(Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total) %>%
  arrange(micro_reg_code, year) # Arranging the final data


# Saving
write.csv(SISU_df_microreg_shares, "Data//clean//Quotas_Share//quotas_SISU_microreg_level.csv", row.names = FALSE)
write_dta(SISU_df_microreg_shares, "Data//clean//Quotas_Share//quotas_SISU_microreg_level.dta")





#------------------#
#     By State     #
#------------------#

# Grouping by state and year
SISU_df_state <- uni_pivoted_df2 %>% group_by(state_code, year)  %>%
  summarise(Seats_Total = sum(Seats_Total, na.rm = TRUE),
            Seats_AA = sum(Seats_AA, na.rm = TRUE),
            Seats_Not_reserved = sum(Seats_Not_reserved, na.rm = TRUE),
            Seats_Public_School = sum(Seats_Public_School, na.rm = TRUE),
            Seats_Racial = sum(Seats_Racial, na.rm = TRUE),
            Seats_Low_Income = sum(Seats_Low_Income, na.rm = TRUE),
            Seats_Disability = sum(Seats_Disability, na.rm = TRUE),
            Seats_Others = sum(Seats_Others, na.rm = TRUE),
            .groups = 'drop')

# Calculating the shares for ALL categories
SISU_df_state_shares <- SISU_df_state %>%
  mutate(Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total) %>%
  arrange(state_code, year) # Arranging the final data

# Saving the new, complete state-level dataset
write.csv(SISU_df_state_shares, "Data//clean//Quotas_Share//quotas_SISU_state_level.csv", row.names = FALSE)
write_dta(SISU_df_state_shares, "Data//clean//Quotas_Share//quotas_SISU_state_level.dta")


