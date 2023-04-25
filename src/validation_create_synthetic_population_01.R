#### VALIDATION
rm(list = ls());gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(xlsx)
library(dplyr)
library(ipumsr)
library(survey)
library(data.table)
library(docstring)
library(tidyverse)
source(file = "00_functions.R")
options(scipen = 10)

inputs_path_step1 <- "../results/Final_Results/Analysis/step 1 - data processing/"
inputs_path_step2 <- "../results/Final_Results/Analysis/step 2 - estimations/"
outputs_path <- "../results/Final_Results/Validation/01_create_synthetic_population"
############################################################################################
#### 1.1) READING THE INPUTS FROM PREVIOUS STEPS
############################################################################################
# # Reading objects from Step: Modelling  
# # Reading parameters Triangular
# # Reading start and stop times for each respondent 

load(file.path(inputs_path_step1, "activities_vector.Rdata"))
load(file.path(inputs_path_step1, "df_days.Rdata")) 
load(file.path(inputs_path_step1, "df_combined_target_profiles.Rdata"))

df_days <- rename(df_days, days_code = code)
v_activs <- unique(v_activs)

############################################################################################
#### 1.2) READING THE CENSUS & CREATE COLUMNS WITH PROFILES
############################################################################################

ddi <- read_ipums_ddi("../data/raw/CPS 2019/usa_00003.xml")
data_US <- read_ipums_micro(ddi)
data_US_Texas <- data_US %>% filter(STATEFIP == 48)
sum(data_US_Texas$PERWT)
data_US_Texas$People <- 1

##### calculate and create age cohorts Census ####################################################################
data_US_Texas$CohAge <- cut(data_US_Texas$AGE,
                            breaks = c(14, 25, 44, 64, 85))
df_ATUS_Texas_Heros <- data_US_Texas %>% filter(!is.na(CohAge)) 

##### calculate and create sex the Census ####################################################################
df_ATUS_Texas_Heros$SEX2 <- as_factor(df_ATUS_Texas_Heros$SEX) 
df_ATUS_Texas_Heros$SEX2 <- as_factor(df_ATUS_Texas_Heros$SEX) 

##### calculate and create income  the Census ####################################################################
df_ATUS_Texas_Heros$FAMINCOME2 <- df_ATUS_Texas_Heros$HHINCOME 
df_ATUS_Texas_Heros$FAMINCOME2 <- with(df_ATUS_Texas_Heros,
                                 ifelse(FAMINCOME2 == 9999999 |
                                          FAMINCOME2 < 0, NA, FAMINCOME2))
df_ATUS_Texas_Heros$IncomeNew <- cut(df_ATUS_Texas_Heros$FAMINCOME2, 
                                c(0, 29999, 59999, 99999, Inf), 
                               labels = c("Less than $29,999", "$30,000 to $59,999", "$60,000 to $99,999", "$100,000 and over"))
##### calculate and create household size in the Census ####################################################################
household_size <- df_ATUS_Texas_Heros %>%
  group_by(SERIAL) %>% 
  summarise(nHousehold = n())
df_ATUS_Texas_Heros <-  merge(df_ATUS_Texas_Heros, household_size, all.x = T)

df_ATUS_Texas_Heros$HH_SIZE2 <- with(df_ATUS_Texas_Heros, 
                               ifelse(nHousehold == 1, "HS_1", 
                                      ifelse(nHousehold == 2, "HS_2", 
                                             ifelse(nHousehold == 3, "HS_3", 
                                                    ifelse(nHousehold > 3, "HS_>3", NA)))))
df_ATUS_Texas_Heros$HH_SIZE2 <- factor(df_ATUS_Texas_Heros$HH_SIZE2,  
                                 levels = c("HS_1", "HS_2", "HS_3", "HS_>3")) 
##### calculate and create employment occupationin the Census ####################################################################
df_ATUS_Texas_Heros$Occup_HEROS_Employed <- with(df_ATUS_Texas_Heros,
                                           ifelse(EMPSTAT == 1, "worker", 
                                                  ifelse(EMPSTAT == 2, "unemployed", 
                                                         ifelse(EMPSTAT == 3, "not in a labor force", NA))))
df_ATUS_Texas_Heros$Occup_HEROS_Employed <- factor(df_ATUS_Texas_Heros$Occup_HEROS_Employed,
                                             levels = c("not in a labor force", "unemployed", "worker")) 
##### calculate and create student occupation in the Census ####################################################################
df_ATUS_Texas_Heros$Occup_HEROS_Student <- with(df_ATUS_Texas_Heros, 
                                          ifelse(SCHOOL == 1, "not enrolled", 
                                                 ifelse(SCHOOL %in% 2:5, "college or university student", NA)))
df_ATUS_Texas_Heros$Occup_HEROS_Student <- factor(df_ATUS_Texas_Heros$Occup_HEROS_Student,  
                                            levels = c("not enrolled", "college or university student")) 

############################################################################################
#### 2) GENERATE SYNTHETIC POPULATION ACCORDING TO ESTIMATIONS
############################################################################################
# # Create survey design to estimate the Total and Standard Deviation from the Totals 

df_GeneratePopulation_List <- list()
for( ll in 1:nrow(df_combined_target_profiles)){
  profile <- df_combined_target_profiles[ll, ]
  profile_formula <- paste("~", paste(profile[1:2], collapse = " + "))
  cat(profile_formula, "\n")
  profile_formula <- formula(profile_formula)
  
  ####################################
  
  svy_Texas <- svydesign( id =~ 1 , data = df_ATUS_Texas_Heros , weights =~ PERWT)
  est_Pop <- svyby(~People, profile_formula, svy_Texas, na.rm = T, svytotal)
  sum(est_Pop$People)
  colnames(est_Pop)[3] <- "nPeople"
  colnames(est_Pop)[4] <- "se.nPeople"
  
  #### The size follows a Normal Distribution
  n_Pop <- sum(df_ATUS_Texas_Heros$PERWT)
  list_pop <- list()
  pSamp <- 0.0001
  est_Pop$nSample <- est_Pop$nPeople*pSamp
  est_Pop$se.nSample <- est_Pop$se.nPeople*pSamp
  est_Pop <- rename(est_Pop, Cat_1 =  profile$Var1, Cat_2 = profile$Var2)
  for(ii in 1:nrow(est_Pop)){
    n_PopSim <- rnorm(1, 
                      mean = est_Pop$nSample[ii], 
                      sd = est_Pop$se.nSample[ii])
    df_TempSim <- data.frame(Person = 1:n_PopSim, 
                             Cat_1 = est_Pop$Cat_1[ii], 
                             Cat_2 = est_Pop$Cat_2[ii], 
                             Var_1 = profile$Var1, 
                             Var_2 = profile$Var2,    
                             Weights = 1/pSamp)
    list_pop[[ii]] <- df_TempSim
  }
  df_temp_Pop <- do.call("rbind", list_pop)
  df_tempGenPop <- NULL
  df_GeneratePopulation <-NULL
  for(jj in 1:7){
    df_tempGenPop <- df_temp_Pop
    df_tempGenPop$days_code <- jj  
    df_tempGenPop$ID <- 1:nrow(df_tempGenPop)
    df_GeneratePopulation <- rbind(df_GeneratePopulation, df_tempGenPop)
  }
  df_GeneratePopulation <- data.table(df_GeneratePopulation)
  
  df_GeneratePopulation <- merge(df_GeneratePopulation, df_days, 
                                 by = "days_code", all.x = T)
  df_GeneratePopulation_List[[ll]] <- df_GeneratePopulation
}

save(df_GeneratePopulation_List, file = file.path(outputs_path, "df_GeneratePopulation_List.Rdata"))



