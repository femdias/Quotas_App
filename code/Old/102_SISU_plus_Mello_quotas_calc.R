# Loading necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx) 
library(writexl) 
library(ggplot2)
library(haven)
library(stringr)
library(haven)

# Setting the working directory
setwd("C:/Users/fm469/Box/Brazil Paper (Felipe Macedo Dias)")



#---------------------------------------------------------------------------
# Comparing SISU with Mello (2022) files
#---------------------------------------------------------------------------

# SISU dataframe
SISU_df <- read.csv("Data//clean//Quotas_Share//quotas_SISU_univ_level.csv")

# Mello (2022) dataset
mello_df <- read_stata("Data\\raw\\Mello (2022) files\\quotas.dta")

# Selecting variables
mello_df2 <- mello_df %>% select(c(co_ies, year, total, reserved)) 

# Calculating percentage of Affirmative Action Seats
mello_df2 <- mello_df2 %>% mutate(Perc_AA_Mello = reserved / total )
   
merged <- SISU_df %>% left_join(mello_df2, by = c("year" = "year", "CO_IES" = "co_ies"))
 
ggplot(merged, aes(x = Share_Seats_AA, y = Perc_AA_Mello)) + geom_point(alpha = 0.6) + theme_minimal() 

# Calculating the correlation between Perc_AA and Perc_AA_Mello
correlation_value <- cor(merged$Share_Seats_AA, merged$Perc_AA_Mello, use = "pairwise.complete.obs")
print(paste("Correlation between Share_Seats_AA and Perc_AA_Mello:", correlation_value))
# 0.8533


# Now, aggregating per year to see how different it is
merged2 <- merged %>% group_by(year) %>% 
  summarise(reserved = sum(reserved, na.rm = TRUE), total = sum(total, na.rm = TRUE),
            Seats_AA = sum(Seats_AA), Seats_Total = sum(Seats_Total))

merged2 <- merged2 %>% mutate(Perc_AA_Mello = reserved / total,
                              Share_Seats_AA = Seats_AA/ Seats_Total)

correlation_value <- cor(merged2$Share_Seats_AA, merged2$Perc_AA_Mello, use = "pairwise.complete.obs")
print(paste("Correlation between Perc_AA and Perc_AA_Mello:", correlation_value))
# 0.9968




#---------------------------------------------------------------------------
# Merging new SISU data with Mello (2022) file to create final AA dataset
#---------------------------------------------------------------------------

# Making Mello quotas aggregation into our aggregations
mello_df2 <- mello_df %>% mutate(Total = total,
                                Affirmative_Action = reserved,
                                Not_reserved = total - reserved,
                                Public_School = ps + ps_nw + ps_li + ps_nw_li,
                                Racial = other_ethnic + ps_nw_li + ps_nw,
                                Low_Income = ps_nw_li + ps_li,
                                Disability = other_specialneeds,
                                Others = other_specialneeds + other_ethnic)

# Selecting columns
mello_df3 <- mello_df2 %>% select(co_ies, no_ies, year, Total, Affirmative_Action,
                                  Not_reserved, Public_School, Racial, Low_Income, Disability, Others)

# Merging with the SISU univeristy level dataset
merged2  <- mello_df3 %>% full_join(uni_pivoted_df, by = c("year" = "year", "co_ies" = "CO_IES"))

# Filling missing university names 

# Creating a clean lookup table for names from the Mello dataset
mello_names <- mello_df %>%  select(co_ies, no_ies) %>% distinct(co_ies, .keep_all = TRUE)

# Joining the lookup table and filling NAs
merged3 <- merged2 %>% left_join(mello_names, by = "co_ies", suffix = c("", "_mello")) %>%
                                mutate(no_ies = coalesce(no_ies, no_ies_mello),
                                      micro_reg_code = as.numeric(micro_reg_code)) %>%
                                select(-no_ies_mello) 

# Filling missing microregion codes

# Loading the new university geoinfo file
geo_info <- read.csv("Data/intermediate/Universities_Geoinfo.csv") %>% select(co_ies, co_municipio, micro_reg_code, state_code)

# Joining the geo info and filling NAs
merged3 <- merged3 %>% left_join(geo_info, by = "co_ies", suffix = c("", "_new")) %>%
                      mutate(micro_reg_code = coalesce(micro_reg_code, micro_reg_code_new)) %>%
                        select(-micro_reg_code_new)

# Sorting the data and dropping the universities without name (created only in > 2020, so they dont matter)
merged3 <- merged3 %>% arrange(co_ies, year) %>%  filter(!is.na(no_ies)) # 16 dropped

# Using the new SISU dataset after 2015
merged4 <- merged3 %>% mutate(Seats_Total         = case_when(year <= 2015 ~ Total, year > 2015 ~ Seats_Total), 
                              Seats_AA            = case_when(year <= 2015 ~ Affirmative_Action, year > 2015 ~ Seats_AA), 
                              Seats_Not_reserved  = case_when(year <= 2015 ~ Not_reserved, year > 2015 ~ Seats_Not_reserved), 
                              Seats_Public_School = case_when(year <= 2015 ~ Public_School, year > 2015 ~ Seats_Public_School), 
                              Seats_Racial        = case_when(year <= 2015 ~ Racial, year > 2015 ~ Seats_Racial), 
                              Seats_Low_Income    = case_when(year <= 2015 ~ Low_Income, year > 2015 ~ Seats_Low_Income), 
                              Seats_Disability    = case_when(year <= 2015 ~ Disability, year > 2015 ~ Seats_Disability), 
                              Seats_Others        = case_when(year <= 2015 ~ Others, year > 2015 ~ Seats_Others), 
                              Seats_Total         = case_when(year <= 2015 ~ Total, year > 2015 ~ Seats_Total)) %>%
                       select(co_ies, no_ies, year, micro_reg_code, state_code, Seats_Total, Seats_AA, Seats_Not_reserved, 
                              Seats_Public_School, Seats_Racial, Seats_Low_Income, Seats_Disability, Seats_Others)

# Calculating the shares of seats by category
merged5 <- merged4 %>% mutate(Share_Seats_AA = Seats_AA / Seats_Total,
                              Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
                              Share_Seats_Public_School = Seats_Public_School / Seats_Total,
                              Share_Seats_Racial = Seats_Racial / Seats_Total,
                              Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
                              Share_Seats_Disability = Seats_Disability / Seats_Total,
                              Share_Seats_Others = Seats_Others / Seats_Total)

# Selecting and Ordering
merged6 = merged5 %>% select(co_ies, no_ies, year, micro_reg_code, state_code, Seats_Total, Seats_AA, Seats_Not_reserved, 
                            Seats_Public_School, Seats_Racial, Seats_Low_Income, Seats_Disability, 
                            Seats_Others, Share_Seats_AA, Share_Seats_Not_reserved, 
                            Share_Seats_Public_School, Share_Seats_Racial, 
                            Share_Seats_Low_Income, Share_Seats_Disability, Share_Seats_Others)

# Filtering rows with 0 seats (drop 22)
merged7 <- merged6 %>%  filter(Seats_Total > 0)


# Saving Mello (2022) + SISU 2016-2023
write.csv(merged7, "Data//clean//Quotas_Share//quotas_Mello_plus_SISU_univ_level.csv", row.names = FALSE)
write_dta(merged7, "Data//clean//Quotas_Share//quotas_Mello_plus_SISU_univ_level.dta")





#------------------#
#  By Microregion  #
#------------------#


# Grouping by microregion and year
quotas_df_microreg <- merged7 %>% group_by(micro_reg_code, year)  %>%
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
quotas_df_microreg_shares <- quotas_df_microreg %>%
  mutate(Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total) %>%
  arrange(micro_reg_code, year) # Arranging the final data


# Saving
write.csv(quotas_df_microreg_shares, "Data//clean//Quotas_Share//quotas_Mello_plus_SISU_microreg_level.csv", row.names = FALSE)
write_dta(quotas_df_microreg_shares, "Data//clean//Quotas_Share//quotas_Mello_plus_SISU_microreg_level.dta")





#------------------#
#     By State     #
#------------------#

# Grouping by state and year
quotas_df_state <- merged7 %>% group_by(state_code, year)  %>%
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
quotas_df_state_shares <- quotas_df_state %>%
  mutate(Share_Seats_AA = Seats_AA / Seats_Total,
    Share_Seats_Not_reserved = Seats_Not_reserved / Seats_Total,
    Share_Seats_Public_School = Seats_Public_School / Seats_Total,
    Share_Seats_Racial = Seats_Racial / Seats_Total,
    Share_Seats_Low_Income = Seats_Low_Income / Seats_Total,
    Share_Seats_Disability = Seats_Disability / Seats_Total,
    Share_Seats_Others = Seats_Others / Seats_Total) %>%
  arrange(state_code, year) # Arranging the final data

# Saving the new, complete state-level dataset
write.csv(quotas_df_state_shares, "Data//clean//Quotas_Share//quotas_Mello_plus_SISU_state_level.csv", row.names = FALSE)
write_dta(quotas_df_state_shares, "Data//clean//Quotas_Share//quotas_Mello_plus_SISU_state_level.dta")












#------------------------------------------------------------------------------------------
# Identifying "errors"/ discontinuities to be fixed with Superior Census dataset
#------------------------------------------------------------------------------------------

# Loading the dataset
df <- read.csv("Data/clean/Quotas_Share/quotas_Mello_plus_SISU_univ_level.csv")
#df <- read.csv("Data/clean/Quotas_Share/quotas_Mello_SISU_Sup_Census_univ_level.csv")

# --- 1. Identifying universities with no data after 2015 ---

# Finding the last year of data for each university
last_year_df <- df %>%
  group_by(co_ies, no_ies) %>%
  summarise(last_observation_year = max(year), .groups = 'drop')

# Filtering for universities that end in 2015
discontinued_unis <- last_year_df %>%
  filter(last_observation_year == 2015)

# --- 2. Identifying universities with a break in seats between 2015 and 2016 ---

# Filtering for the transition years and pivoting
seat_break_df <- df %>%
  filter(year %in% c(2015, 2016)) %>%
  select(co_ies, no_ies, year, Seats_Total) %>%
  pivot_wider(names_from = year, values_from = Seats_Total, names_prefix = "seats_") %>%
  # Removing universities that don't exist in both years or had zero seats in 2015
  filter(!is.na(seats_2015) & !is.na(seats_2016) & seats_2015 > 0) %>%
  # Calculating the percentage change from 2015 to 2016
  mutate(pct_change = (seats_2016 - seats_2015) / seats_2015) %>%
  # Filtering for changes greater than 50% drop
  filter(pct_change < -0.50) %>% arrange(pct_change)

# --- 3. Printing the results ---

# Printing the first list
cat("--- Universities with no observations after 2015 ---\n")
print(discontinued_unis)

# Printing the second list
cat("\n--- Universities with a break in total seats between 2015 and 2016 ---\n")
print(seat_break_df)








