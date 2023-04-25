rm(list = ls());gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(xlsx)
library(dplyr)
library(ipumsr)
library(labelled)
library(survey)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(data.table)
library(remotes)
remotes::install_github("marberts/smart")
library(smart)
library(docstring)
library(Hmisc)
library(dvmisc)

source(file = "00_functions.R")
input_path <- "../results/Final_Results/Analysis/step 1 - data processing"
output_path <- "../results/Final_Results/Analysis/step 2 - estimations"
############################################################################################
#### 1) Loading data
############################################################################################
load(file.path(input_path, "df_ATUS_Texas_Heros.Rdata"))
load(file.path(input_path, "df_combined_target_profiles.Rdata"))
load(file.path(input_path, "target_profiles.Rdata"))
load(file.path(input_path, "activities_vector.Rdata"))
load(file.path(input_path, "df_days.Rdata"))

############################################################################################
#### 2) Calculating the estimations to each combination (Activity, Profile) 
############################################################################################
v_days <- 1:7
list_estimates_daily <- list(list(Single = list(), Combined = list()))
list_estimates_hourly <- list(list(Single = list(), Combined = list()))
# # lists that will contain the survey design for the estimations for each activity 
list_indicator_consolidated_long <- list()
list_indicator_consolidated <- list()

df_ATUS_Texas_Heros <- rename(df_ATUS_Texas_Heros, ID = CASEID, Weights = WT06, days_code = DAY)

start.time.perf <- Sys.time()
for(kk in 1:length(v_activs)){ 
  activity <- as.character(v_activs[kk])
  cat("Activity:",  activity, "\n")
  if(activity == "Total"){
    df_activity <- df_ATUS_Texas_Heros %>% 
      filter(YEAR == 2019)
  }else{
    df_activity <- df_ATUS_Texas_Heros %>% 
      filter(Subcategory_ModifiedHEROS == activity & YEAR == 2019)
  }
  
  df_activity$days <- as_factor(df_activity$days_code)
  df_activity$StartTime <- format(strptime(df_activity$START, "%H:%M:%S"), "%H")
  df_activity$StopTime <- format(strptime(df_activity$STOP, "%H:%M:%S"), "%H")
  df_activity$StartTime <- as.numeric(df_activity$StartTime)
  df_activity$StopTime <- as.numeric(df_activity$StopTime)
  
  #### A) Create list of matrices
  var_hours_day <- paste("H", 0:23, sep = "_")
  id_people <- unique(df_activity$ID)
  df_Hour <- as_tibble(matrix(NA, ncol = 24, nrow = length(id_people)))
  colnames(df_Hour) <- var_hours_day
  df_Hour$ID <- id_people
  df_Hour$nTimesDay <- 0
  df_Hour$StartTime <- -99
  df_Hour$StopTime <- -99
  indicator_hour_activity <- list()
  for(ii in 1:7){
    df_Hour$days <- df_days[df_days$code == ii, "days"]
    indicator_hour_activity[[v_days[ii]]]  <- df_Hour
  }
  #### B) Identify when the activity was performed and the duration
  df_indicator_consolidated <- fun_indicator_activity(df_activity = df_activity, 
                                                      indicator_hour_activity = indicator_hour_activity)
  
  temp_duration <- df_activity %>%
    group_by(ID, days) %>% 
    summarise(DURATION_EXT = paste(DURATION_EXT, collapse = ",")) %>% 
    rename(days = days) 
  df_indicator_consolidated <- merge(df_indicator_consolidated, temp_duration, by = c("ID", "days"), all.x = T) ## merging with DURATION
  
  df_weights <- df_activity %>% select(ID, Weights)
  df_weights <- unique(df_weights)
  df_profiles_original <- unique(df_activity[, c("ID", target_profiles)])
  
  df_indicator_consolidated <- merge(df_indicator_consolidated, 
                                     df_weights, by = "ID", all.x = T)
  df_indicator_consolidated$doneActivity <-  apply(df_indicator_consolidated[, var_hours_day], 1, any, na.rm = T) ## do the activity during the day
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
  list_indicator_consolidated[[activity]] <- df_indicator_consolidated
  ### C) Calculate estimations: hourly and daily for each activity and profile 
  for(ll in 1:nrow(df_combined_target_profiles)){
    profile_iteration <- as.character(unlist(df_combined_target_profiles[ll, 1:2], use.names = FALSE))
    profile_type <- as.character(unlist(df_combined_target_profiles[ll, 3], use.names = FALSE))
    estimates_hourly <- fun_HourEstim(data = df_indicator_consolidated_long, profile = profile_iteration, activity=activity)
    estimates_daily <- fun_DayEstim(data = df_indicator_consolidated, profile = profile_iteration, activity=activity)
    
    list_estimates_hourly[[activity]][[profile_type]][[ll]] <- estimates_hourly
    list_estimates_daily[[activity]][[profile_type]][[ll]] <- estimates_daily
  }
}
difftime(Sys.time(), start.time.perf)

names(list_indicator_consolidated_long) <- v_activs
names(list_indicator_consolidated) <- v_activs

############################################################################################
#### 3) Calculating the probability (numerator = list_estimates_daily), denominator (estim_total) 
############################################################################################

## list_estimates_daily contains totals which is the numerator. 
## In order to obtain the denominator the total for the Sleeping activity is used. 

list_estimates_daily_all <- list(Single = list(), Combined = list())
output_activity_single <- NULL
output_activity_combined <- NULL
for(ll in 1:nrow(df_combined_target_profiles)){
  profile_iteration <- as.character(unlist(df_combined_target_profiles[ll, 1:2], use.names = FALSE))
  profile_type <- as.character(unlist(df_combined_target_profiles[ll, 3], use.names = FALSE))
  estim_total <- list_estimates_daily[["Sleeping"]][[profile_type]][[ll]]
  colnames(estim_total) <- gsub("EstimPP", "EstimPP_Total", colnames(estim_total))
  estim_total <- estim_total %>% select_at(c("days", profile_iteration, "EstimPP_Total"))
  
  list_estimates_daily_all[[profile_type]][[ll]] <- estim_total    
  for(kk in 1:length(v_activs)){
    activity <- v_activs[kk]
    estim_activity <- list_estimates_daily[[activity]][[profile_type]][[ll]]
    estim_activity_probability <- merge(estim_total, estim_activity, by = c("days", profile_iteration), all.x = T)
    estim_activity_probability <- estim_activity_probability %>% arrange_(.dots = c(profile_iteration, "days"))
    estim_activity_probability <- estim_activity_probability %>% mutate(Probability = EstimPP/EstimPP_Total)
    estim_activity_probability$Activity <- activity
    estim_activity_probability$Probability <- with(estim_activity_probability, 
                                                   ifelse(is.na(Probability), 0 , Probability))
    estim_activity_probability$nTimesDay <- with(estim_activity_probability, 
                                                 ifelse(is.na(nTimesDay), 0 , nTimesDay))
    for(jj in 1:length(unique(profile_iteration))){
      tt_Var <- paste("Var", jj, sep = "_")
      tt_Cat <- paste("Cat", jj, sep = "_")
      colnames(estim_activity_probability) <- gsub(profile_iteration[jj], tt_Cat, colnames(estim_activity_probability)) 
      estim_activity_probability[, tt_Var] <- profile_iteration[jj]
    } 
    list_estimates_daily[[activity]][[profile_type]][[ll]] <- estim_activity_probability
    if(profile_type == "Single"){
      estim_activity_single <- estim_activity_probability %>% 
        select_at(c("days", "Cat_1",  "Var_1", "nTimesDay", "Probability", "Activity"))
      output_activity_single <- rbind(output_activity_single, estim_activity_single)
    }else{
      estim_activity_combined <- estim_activity_probability %>% 
        select_at(c("days", "Cat_1", "Cat_2", "Var_1", "Var_2", "nTimesDay", "Probability", "Activity"))
      output_activity_combined <- rbind(output_activity_combined, estim_activity_combined)
    }
  }
}


fileName <- "Weekly_Probability_Activity_Combined.csv"
dim(output_activity_combined)
Weekly_Probability_Activity_Combined <- na.omit(output_activity_combined)
dim(Weekly_Probability_Activity_Combined)
Weekly_Probability_Activity_Combined$Probability <- ifelse(Weekly_Probability_Activity_Combined$Probability > 1, 1, Weekly_Probability_Activity_Combined$Probability)
write.table(Weekly_Probability_Activity_Combined , file = file.path(output_path, fileName), row.names = F)
save(Weekly_Probability_Activity_Combined , file = file.path(output_path, "Weekly_Probability_Activity_Combined.Rdata")) 

fileName <- "Weekly_Probability_Activity_Single.csv"
dim(output_activity_single)
Weekly_Probability_Activity_Single <- na.omit(output_activity_single)
dim(Weekly_Probability_Activity_Single)
Weekly_Probability_Activity_Single$Probability <- ifelse(Weekly_Probability_Activity_Single$Probability > 1, 1, Weekly_Probability_Activity_Single$Probability)
write.table(Weekly_Probability_Activity_Single, file = file.path(output_path, fileName), row.names = F)
save(Weekly_Probability_Activity_Single, file = file.path(output_path, "Weekly_Probability_Activity_Single.Rdata")) 

############################################################################################
#### 4) Estimating the proportion of people per hour and profile

# 1 <- Consolidate the indicators activity list in a data.frame
# 2 <- loop through the profile data.frame (df_combined_target_profiles) 
# 3 <- Get the estimate number of people per (Profile, Day, Hour)
# 4 <- Obtain the proportion of people per day-hour and consolidate in Daily_Probability_Activity_Combined 
############################################################################################

Daily_Probability_Activity_Combined_List <- list()

for( ii in 1:nrow(df_combined_target_profiles)){
  profile <- df_combined_target_profiles[ii, ]
  df_all_indicators_consolidated <- do.call("rbind", list_indicator_consolidated_long)
  df_hourly_profile <- df_all_indicators_consolidated %>%
    filter(Indicator_Activity == 1) %>% # performing the activity
    select_at(c(profile$Var1, profile$Var2, "days", "H0_23", "Weights")) %>%
    group_by_at(c("H0_23", "days", profile$Var1, profile$Var2)) %>%
    summarise(Total_People_Hour = sum(Weights)/(91*4))
  
  Daily_Probability_Activity_Combined <- list()
  for (kk in 1:length(v_activs)){
    activity <- v_activs[kk]
    tab_activity_hourly <- list_estimates_hourly[[activity]][["Combined"]][[ii]]
    df_hourly_profile_activity <- merge(df_hourly_profile, tab_activity_hourly, all = T)  
    df_hourly_profile_activity$probability_hourly <- with(df_hourly_profile_activity, EstimPP/Total_People_Hour)
    df_hourly_profile_activity$probability_hourly <- ifelse(is.na(df_hourly_profile_activity$probability_hourly), 0, df_hourly_profile_activity$probability_hourly )
    lll <- apply(df_hourly_profile_activity[, c("H0_23", "days", profile$Var1, profile$Var2)], 1, paste, collapse ="_")
    if(any(duplicated(lll)))stop("")
    Daily_Probability_Activity_Combined[[activity]] <- df_hourly_profile_activity
  }
  Daily_Probability_Activity_Combined_List[[ii]] <- Daily_Probability_Activity_Combined
}

save(Daily_Probability_Activity_Combined_List , file = file.path(output_path, "Daily_Probability_Activity_Combined_List.Rdata"))

# # # Saving  additional objects 
save(list_estimates_hourly, file = file.path(output_path, "list_estimates_hourly.Rdata"))
save(list_estimates_daily, file = file.path(output_path, "list_estimates_daily.Rdata"))
save(list_indicator_consolidated_long, file = file.path(output_path, "list_indicator_consolidated_long.Rdata"))
save(list_indicator_consolidated, file = file.path(output_path, "list_indicator_consolidated.Rdata"))



  
