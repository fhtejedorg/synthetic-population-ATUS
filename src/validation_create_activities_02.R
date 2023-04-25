#### VALIDATION --- Acaa voy
rm(list = ls());gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(extraDistr)
library(splitstackshape)
library(xlsx)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(data.table)
library(docstring)
library(truncnorm)
library(tidyverse)
source(file = "00_functions.R")
options(scipen = 10)

inputs_path_step1 <- "../results/Final_Results/Analysis/step 1 - data processing/"
inputs_path_step2 <- "../results/Final_Results/Analysis/step 2 - estimations/"
outputs_path <- "../results/Final_Results/Validation/02_create_activities"
############################################################################################
#### 1) READING THE INPUTS FROM PREVIOUS STEPS
############################################################################################
# # Reading objects from Step: Modelling  
# # Reading parameters Triangular
# # Reading start and stop times for each respondent 

load(file.path("../results/Final_Results/Validation/01_create_synthetic_population", "df_GeneratePopulation_List.Rdata"))

load(file.path(inputs_path_step1, "activities_vector.Rdata"))
load(file.path(inputs_path_step1, "df_days.Rdata")) 
load(file.path(inputs_path_step1, "df_combined_target_profiles.Rdata"))

load(file.path(inputs_path_step2, "Proportion_StartTimes_Combined.Rdata")) 
load(file.path(inputs_path_step2, "Weekly_Probability_Activity_Combined.Rdata"))
load(file.path(inputs_path_step2, "Triangular_StartTimes_Combined.Rdata")) 
load(file.path(inputs_path_step2, "df_Consolidate_StartTime.Rdata")) ## ATUS Start times Consolidated File
load(file.path(inputs_path_step2, "df_Consolidate_StopTime.Rdata")) ### ATUS Stop times Consolidated File



############################################################################################
##### Consolidated data frames and rename columns to input datasets 
df_days <- rename(df_days, days_code = code)
v_activs <- unique(v_activs)

Proportion_StartTimes_Combined_Width <- Proportion_StartTimes_Combined %>% 
  group_by(days, Cat_1, Cat_2, Activity) %>% 
  summarise(StartTime_Vector = paste(StartTime, collapse = ", "), 
            proportion_StartTime_Vector = paste(proportion_StartTime, collapse = ", "), 
            average_duration_Vector = paste(average_duration, collapse = ", "), 
            std_duration_Vector = paste(std_duration, collapse = ", "), 
  )
############################################################################################
#### 2) CREATE ACTIVITIES FOR EACH RESPONDENT AND IDENTIFY HOUR-DAY OF THE SIMULATED ACTIVITY
############################################################################################
list_indicator_consolidated_long_profiles <- list()
for( ll in 1:nrow(df_combined_target_profiles)){
  profile <- df_combined_target_profiles[ll, ]
  profile_formula <- paste("~", paste(profile[1:2], collapse = " + "))
  cat(profile_formula, "\n")
  profile_formula <- formula(profile_formula)
  
  df_GeneratePopulation_reduced <- df_GeneratePopulation_List[[ll]] 
  list_proportion_hourly_consolidated <- list()
  list_indicator_consolidated_long <- list()
  v_days <- 1:7
  L = "L_Start"; M = "M_Start"; H = "H_Start"
  for(kk in 1:length(v_activs)){
    ##############################################################################################################################
    # # 2.1 Create activities randomly  
    ##############################################################################################################################
    activity <-  v_activs[kk]
    cat(activity, "\n")
    df_GeneratePopulation <- df_GeneratePopulation_reduced
    ### Merging with probability data set
    df_GeneratePopulation$Activity <- activity
    df_GeneratePopulation <- merge(df_GeneratePopulation, 
                                   Weekly_Probability_Activity_Combined[, c("Cat_1", "Cat_2", "days","Activity", "Probability", "nTimesDay")],
                                   by.x = c("Cat_1", "Cat_2", "days", "Activity"),
                                   by.y = c("Cat_1", "Cat_2", "days", "Activity"),
                                   all.x = T)
    ### Indicator variable: Activity is performed or not. Bernoulli distribution with parameter (Probability of performing the activity)
    set.seed(100L)
    df_GeneratePopulation$Indicator_Activity <- sapply(df_GeneratePopulation$Probability, rbinom, n = 1, size = 1)
    
    ### Frequency variable: It indicates the number of times the activity is performed per day. Truncated poisson because the frequency cannot be zero. 
    set.seed(100L)
    df_GeneratePopulation$Frequency_Activity <- sapply(df_GeneratePopulation$nTimesDay, rtpois, n = 1, a = 0, b = df_GeneratePopulation$nTimesDay + 1) # Truncated Poisson distribution
    
    df_population_simulation_activity <- df_GeneratePopulation %>% filter(Indicator_Activity > 0)
    df_population_simulation_activity <- merge(df_population_simulation_activity, 
                                               Triangular_StartTimes_Combined[, c("Cat_1","Cat_2", "days", 
                                                                                  "Activity", "L_Start","M_Start","H_Start", 
                                                                                  "average_duration", "std_duration")], 
                                               by.x = c("Cat_1", "Cat_2", "days", "Activity"), 
                                               by.y = c("Cat_1","Cat_2", "days", "Activity"), 
                                               all.x =T)
    df_population_simulation_activity <- na.omit(df_population_simulation_activity)
    
    ### The table format changes according to the number of times each respondent performs the activity
    df_activity_schedules_simulation_long <- expandRows(df_population_simulation_activity, "Frequency_Activity") # #
    df_activity_schedules_simulation_long <- merge(df_activity_schedules_simulation_long, 
                                                   Proportion_StartTimes_Combined_Width,
                                                   by = c("days", "Cat_1", "Cat_2", "Activity"), 
                                                   all.x = T)
    StartTime_Vector <- apply(df_activity_schedules_simulation_long, 1, get_start_time, type = "II")
    StartTime_Vector_Long <- do.call("rbind", (strsplit(StartTime_Vector, ", ")))
    StartTime_Vector_Long <- apply(StartTime_Vector_Long, 2, as.numeric)
    StartTime_Vector_Long <- data.frame(StartTime_Vector_Long)
    colnames(StartTime_Vector_Long) <- c("StartTime", "DURATION_EXT")
    df_activity_schedules_simulation_long <- cbind(df_activity_schedules_simulation_long, 
                                                   StartTime_Vector_Long)
    
    ### Assign start and stop times according to the estimations found at previous steps
    df_activity_schedules_simulation_long <- data.frame(df_activity_schedules_simulation_long)
    
    df_activity_schedules_simulation_long$StopTime <- with(df_activity_schedules_simulation_long, 
                                                           round(StartTime + round(DURATION_EXT/60)))
    df_activity_schedules_simulation_long$StopTime <- df_activity_schedules_simulation_long$StopTime %%24
    
    ##############################################################################################################################
    # # 2.2 This part recreates the sections A and B from file 01_ATUS_Analysis.R
    ##############################################################################################################################
    #### A) Create list of matrices
    var_hours_day <- paste("H", 0:23, sep = "_")
    id_people <- unique(df_activity_schedules_simulation_long$ID)
    df_Hour <- as_tibble(matrix(NA, ncol = 24, nrow = length(id_people)))
    colnames(df_Hour) <- var_hours_day
    df_Hour$ID <- id_people
    df_Hour$nTimesDay <- 0
    df_Hour$StartTime <- -99
    df_Hour$StopTime <- -99
    indicator_hour_activity <- list()
    for(ii in 1:7){
      df_Hour$days <- df_days[df_days$days_code == ii, "days"]
      indicator_hour_activity[[v_days[ii]]]  <- df_Hour
    }
    
    df_activity = df_activity_schedules_simulation_long
    target_profiles <- c("Cat_1", "Cat_2")
    

    df_indicator_consolidated <- fun_indicator_activity(df_activity = df_activity_schedules_simulation_long, 
                                                        indicator_hour_activity = indicator_hour_activity)
    
    df_weights <- df_activity %>% select(ID, Weights)
    df_weights <- unique(df_weights)
    df_profiles_original <- unique(df_activity[, c("ID", target_profiles)])
    df_indicator_consolidated <- merge(df_indicator_consolidated, 
                                       df_weights, by = "ID", all.x = T)
    df_indicator_consolidated$doneActivity <-  apply(df_indicator_consolidated[, var_hours_day], 1, any, na.rm = T)
    df_indicator_consolidated$doneActivity <-  as.numeric(df_indicator_consolidated$doneActivity)
    
    df_indicator_consolidated[, var_hours_day] <- apply(df_indicator_consolidated[, var_hours_day], 2, as.numeric)
    df_indicator_consolidated <- data.frame(df_indicator_consolidated)
    df_indicator_consolidated <- merge(df_indicator_consolidated, 
                                       df_profiles_original, 
                                       by = "ID", all.x = T)
    
    df_indicator_consolidated$TotalPP <- 1
    df_indicator_consolidated$Activity <- activity
    df_indicator_consolidated <- df_indicator_consolidated %>% filter(doneActivity == 1)
    df_indicator_consolidated_long <- melt(df_indicator_consolidated, 
                                           measure.vars = var_hours_day, 
                                           value.name = "Indicator_Activity", 
                                           variable.name = "H0_23")
    list_indicator_consolidated_long[[activity]] <- df_indicator_consolidated_long
  }
  list_indicator_consolidated_long_profiles[[ll]] <- list_indicator_consolidated_long
}

save(list_indicator_consolidated_long_profiles, file = file.path(outputs_path, "list_indicator_consolidated_long_profiles.Rdata"))
