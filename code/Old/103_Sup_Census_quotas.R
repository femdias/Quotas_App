### Purpose: Getting Quotas for Universities with own admission test, that are not present in SISU
library(readxl)
library(data.table)
library(writexl)
library(tidyverse)
library(dplyr)
library(stringi)
library(haven)

# Defining work directory  
setwd('C:\\Users\\fm469\\Box\\Brazil Paper (Felipe Macedo Dias)')

# Defining the directory containing the files
data_dir <- "Data\\raw\\Superior Census"

# Listing all CSV files that contain "CURSOS" (case-insensitive) in their names
file_list <- list.files(path = data_dir, pattern = "(?i)CURSOS.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Removing 2023
file_list <- file_list[1:(length(file_list)-1)]


# Reading and appending
to_select <- c("NU_ANO_CENSO", "CO_IES", "CO_CURSO","NO_CURSO", "CO_MUNICIPIO", "TP_CATEGORIA_ADMINISTRATIVA", "QT_ING",# "QT_VG_NOVA",
               "QT_ING_RVREDEPUBLICA","QT_ING_RVETNICO", "QT_ING_RVPDEF", "QT_ING_RVSOCIAL_RF", "QT_ING_RVOUTROS")



# Looping through each file in file_list

# Initializing empty tibble
combined_data <- tibble()
for (file in file_list) {
  
  cat("\n### Reading:", file, "###\n")
  
  df <- read_delim(file, delim = ";", show_col_types = FALSE, locale = locale(encoding = "Latin1"))  %>%
                    filter(TP_CATEGORIA_ADMINISTRATIVA %in% c(1,2,3)) %>% # Select the public universities
                    mutate(TP_CATEGORIA_ADMINISTRATIVA = ifelse(TP_CATEGORIA_ADMINISTRATIVA == 1, "Federal",
                                                                ifelse(TP_CATEGORIA_ADMINISTRATIVA == 2, "State", "Municipal"))) # %>% select(-c(CO_CINE_AREA_GERAL, CO_CINE_AREA_ESPECIFICA))
                    
  combined_data <- bind_rows(combined_data, df)  # Appending data
}



# Selecting only "University" and Federal Centers/Institutes (removing State Centers)

# TP_ORGANIZACAO_ACADEMICA= 1. Universidade 2. Centro Universitário 3. Faculdade 4. Instituto Federal de Educação, Ciência e Tecnologia # 5. Centro Federal de Educação Tecnológica
combined_data <- combined_data %>% filter(TP_ORGANIZACAO_ACADEMICA %in% c(1,3, 4, 5))



################################## Dataset to cassify courses by Major

# # Processing and exporting unique combinations
# major_combinations <- combined_data %>%
#   # Grouping by university and course codes
#   group_by(CO_IES, CO_CURSO) %>%
#   # Summarizing data for each group
#   summarise(
#     # Calculating the frequency of each combination
#     freq = n(),
#     # Selecting the most frequent course name as the representative one
#     NO_CURSO = names(which.max(table(NO_CURSO, useNA = "ifany"))),
#     # Dropping the grouping structure
#     .groups = 'drop'
#   ) %>%
#   # Sorting results by frequency in descending order
#   arrange(desc(freq))


# # Reading the dataset with classified majors
# majors_classified <- read_csv("Data/intermediate/Majors_SISU.csv")

# # Merging the datasets by CO_IES and CO_CURSO
# # Keeping all combinations from the census data
# merged_data <- major_combinations %>%
#   left_join(majors_classified, by = c("CO_IES", "CO_CURSO" = "CO_IES_CURSO"))

# # Overwriting the old file with the new merged data
# write_xlsx(merged_data, "Data/intermediate/temp/Sup_Census_Major_to_classify.xlsx")


# write_xlsx(merged_data, "Data/intermediate/temp/Sup_Census_Major_to_classify.xlsx")

##################################

# Reading the Major classified excel file
classified_data <- read_excel("Data/intermediate/temp/Sup_Census_Major_CLASSIFIED.xlsx")

# Removing duplicate entries by university and course codes
deduplicated_data <- classified_data %>% distinct(CO_IES, CO_CURSO, .keep_all = TRUE)

# Saving 
write_xlsx(deduplicated_data, "Data/intermediate/Sup_Census_Majors.xlsx")
write_dta(deduplicated_data, "Data/intermediate/Sup_Census_Majors.dta")






# Selecting column
selec_comb <- combined_data %>% select(to_select)

# Renaming 
names(selec_comb) <- c('year', 'co_ies', "Course_Code", "Course_Name",  "Code_Municipality",
                          "Adm_Category", "Seats_Total", "Seats_Public_School", "Seats_Racial", 
                          "Seats_Disability", "Seats_Low_Income", "Seats_Others")

# Extra Quota variables
selec_comb <- selec_comb %>% mutate(Seats_AA = Seats_Public_School + Seats_Racial + Seats_Low_Income + Seats_Disability + Seats_Others,
                                    Seats_Not_reserved = Seats_Total - Seats_AA)

# Dropping if total seats == 0
selec_comb <- selec_comb %>% filter(Seats_Total != 0)

# Adding microreg and state
geo_info <- read.csv("Data/intermediate/Universities_Geoinfo.csv") %>%
                      select(co_ies, co_municipio, micro_reg_code, state_code)

# Joining the geo info and filling NAs
selec_comb <- selec_comb %>% left_join(geo_info, by = "co_ies") 

# Adding major dataset 
#majors <- read.csv("Data/intermediate/Majors_SISU.csv") %>% mutate(CO_IES_CURSO = as.numeric(CO_IES_CURSO)) %>% 
#                                                            arrange(CO_IES_CURSO) 

# Merging 
#selec_comb <- selec_comb %>% left_join(majors, by = c("Course_Code" = "CO_IES_CURSO", "co_ies" = "CO_IES"))
 
# Grouping by majors
#own_test_majors <- selec_comb %>% group_by(Major)

# Selecting columns
selec_comb <- selec_comb %>% select(co_ies, year, co_municipio, micro_reg_code, state_code, Seats_Total, Seats_AA, Seats_Not_reserved, 
               Seats_Public_School, Seats_Racial, Seats_Low_Income, Seats_Disability, Seats_Others) %>%  
              arrange(co_ies, year)

# Aggregating by University
selec_comb_uni <- selec_comb %>% group_by(co_ies, year, micro_reg_code, state_code)  %>%
  summarise(Seats_Total = sum(Seats_Total, na.rm = TRUE),
            Seats_AA = sum(Seats_AA, na.rm = TRUE),
            Seats_Not_reserved = sum(Seats_Not_reserved, na.rm = TRUE),
            Seats_Public_School = sum(Seats_Public_School, na.rm = TRUE),
            Seats_Racial = sum(Seats_Racial, na.rm = TRUE),
            Seats_Low_Income = sum(Seats_Low_Income, na.rm = TRUE),
            Seats_Disability = sum(Seats_Disability, na.rm = TRUE),
            Seats_Others = sum(Seats_Others, na.rm = TRUE),
            .groups = 'drop')


#---------------------------------------------------------------------------
# University of Sao Paulo (USP, 55) Quotas are strange. It's not right.
# The quotas started in 2018 with 36.9% and went until 50% in 2021. 37% are racial quotas. There are no other quotas.
# https://g1.globo.com/educacao/noticia/usp-aprova-cotas-raciais-e-de-escola-publica-na-fuvest-pela-primeira-vez-na-historia.ghtml
#---------------------------------------------------------------------------

selec_comb_uni <- selec_comb_uni %>%
  mutate(
    # Applying rules only for USP from 2018
    Seats_Public_School = case_when(
      co_ies == 55 & year == 2018 ~ round(Seats_Total * 0.369),
      co_ies == 55 & year == 2019 ~ round(Seats_Total * 0.40),
      co_ies == 55 & year == 2020 ~ round(Seats_Total * 0.45),
      co_ies == 55 & year >= 2021 ~ round(Seats_Total * 0.50),
      co_ies == 55 & year >= 2022 ~ round(Seats_Total * 0.50),
      co_ies == 55 & year >= 2023 ~ round(Seats_Total * 0.50),
      TRUE ~ Seats_Public_School),
    # Calculating racial quotas as 37% of the new public school seats for USP
    Seats_Racial = case_when(
      co_ies == 55 & year >= 2018 ~ round(Seats_Public_School * 0.37),
      TRUE ~ Seats_Racial),
    # Setting other quotas to zero for USP
    Seats_Low_Income = ifelse(co_ies == 55 & year >= 2018, 0, Seats_Low_Income),
    Seats_Disability = ifelse(co_ies == 55 & year >= 2018, 0, Seats_Disability),
    Seats_Others = ifelse(co_ies == 55 & year >= 2018, 0, Seats_Others)) %>%
  # Recalculating total AA and non-reserved seats after the correction
  mutate(Seats_AA = Seats_Public_School + Seats_Racial + Seats_Low_Income + Seats_Disability + Seats_Others,
    Seats_Not_reserved = Seats_Total - Seats_AA)

# Save
write.csv(selec_comb_uni, "Data//clean//Quotas_Share//quotas_Sch_Census_univ_level.csv", row.names = FALSE)
write_dta(selec_comb_uni, "Data//clean//Quotas_Share//quotas_Sch_Census_univ_level.dta")



#------------------#
#  By Microregion  #
#------------------#


# Grouping by microregion and year
Sup_Census_df_microreg <- selec_comb_uni %>% group_by(micro_reg_code, year)  %>%
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
Sup_Census_df_microreg_shares <- Sup_Census_df_microreg %>%
  mutate(Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total) %>%
  arrange(micro_reg_code, year) # Arranging the final data


# Saving
write.csv(Sup_Census_df_microreg_shares, "Data//clean//Quotas_Share//quotas_Sch_Census_microreg_level.csv", row.names = FALSE)
write_dta(Sup_Census_df_microreg_shares, "Data//clean//Quotas_Share//quotas_Sch_Census_microreg_level.dta")





#------------------#
#     By State     #
#------------------#

# Grouping by state and year
Sup_Census_df_state <- selec_comb_uni %>% group_by(state_code, year)  %>%
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
Sup_Census_df_state_shares <- Sup_Census_df_state %>%
  mutate(Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total) %>%
  arrange(state_code, year) # Arranging the final data

# Saving the new, complete state-level dataset
write.csv(Sup_Census_df_state_shares, "Data//clean//Quotas_Share//quotas_Sch_Census_state_level.csv", row.names = FALSE)
write_dta(Sup_Census_df_state_shares, "Data//clean//Quotas_Share//quotas_Sch_Census_state_level.dta")
