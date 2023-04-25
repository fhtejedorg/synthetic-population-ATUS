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
library(RJSONIO)
library(remotes)
remotes::install_github("marberts/smart")
library(smart)
library(docstring)
library(Hmisc)
source(file = "00_functions.R")

inputs_path_step1 <- "../results/Final_Results/Analysis/step 1 - data processing/"
inputs_path_step2 <- "../results/Final_Results/Analysis/step 2 - estimations/"
output_path <- "../results/Final_Results/Analysis/step 2 - estimations/"
######################################################################
#####  6) Obtaining the triangular distribution for each activity 
######################################################################
load(file.path(inputs_path_step1, "activities_vector.Rdata"))
load(file.path(inputs_path_step1, "target_profiles.Rdata"))
load(file.path(inputs_path_step1, "df_combined_target_profiles.Rdata"))
load(file.path(inputs_path_step2, "list_indicator_consolidated.Rdata"))

df_Start_Out_1 <- NULL
df_Stop_Out_1 <- NULL
df_Start_Out_2 <- NULL
df_Stop_Out_2 <- NULL
df_StartProportion_Out_1 <- NULL
df_StartProportion_Out_2 <- NULL
df_Consolidate_StartTime <- NULL
df_Consolidate_StopTime <- NULL
v_activs <- unique(v_activs)

for(kk in 1:length(v_activs)){
  activity <- as.character(v_activs[kk])
  cat(activity, "\n")
  www <- list_indicator_consolidated[[activity]]
  www$StartTime_C <- gsub("-99,|,NA,|,NA|NA,|NA", "", www$StartTime)
  www$StopTime_C <- gsub("-99,|,NA,|,NA|NA,|NA", "", www$StopTime)
  
  df_StartTime <- NULL
  df_StopTime <- NULL
  for(ii in 1:nrow(www)){
    qqq1 <- unlist(strsplit(www$StartTime_C[ii], ","))
    if(length(qqq1) == 0){
      qqq1 <- NA
    }else{
      qqq1 <- as.numeric(qqq1)
      if(any(qqq1>23 | qqq1<0))stop("Non-conformable dataset")
    }
    
    qqq2 <- unlist(strsplit(www$StopTime_C[ii], ","))
    if(length(qqq2) == 0){
      qqq2 <- NA
    }else{
      qqq2 <- as.numeric(qqq2)
    }
    
    qqq3 <- unlist(strsplit(www$DURATION_EXT[ii], ","))
    if(length(qqq3) == 0){
      qqq2 <- NA
    }else{
      qqq3 <- as.numeric(qqq3)
    }  
    
    ppp1 <- data.frame(ID = www$ID[ii], 
                       days = www$days[ii],
                       StartTime = qqq1, 
                       DURATION_EXT = qqq3)
    ppp1 <- ppp1 %>% filter(!is.na(StartTime) & !is.na(DURATION_EXT))
    ppp2 <- data.frame(ID = www$ID[ii], 
                       days = www$days[ii],
                       StopTime = qqq2)
    
    df_StartTime <- rbind(df_StartTime, ppp1)
    df_StopTime  <- rbind(df_StopTime, ppp2)
  }
  
  uuu <- unique(www %>% select_at(c("ID", "days", "Weights", target_profiles, "Activity"))) 
  df_StartTime <- merge(df_StartTime, uuu, by = c("ID", "days"), all.x = T) 
  df_StartTime <- df_StartTime %>% filter(!is.na(StartTime))
  df_Consolidate_StartTime <- rbind(df_Consolidate_StartTime, df_StartTime)
  
  df_StopTime <- merge(df_StopTime, uuu, by = c("ID", "days"), all.x = T) 
  df_StopTime <- df_StopTime %>% filter(!is.na(StopTime))
  df_Consolidate_StopTime <- rbind(df_Consolidate_StopTime, df_StopTime)
  
  for(ll in 1:nrow(df_combined_target_profiles)){
    profile_iteration <- as.character(unlist(df_combined_target_profiles[ll, 1:2], use.names = FALSE))
    profile_type <- as.character(unlist(df_combined_target_profiles[ll, 3], use.names = FALSE))    
      subStart <- df_StartTime %>%
        group_by_at(c("days", profile_iteration)) %>%
        summarise(L_Start = min(StartTime),
                  M_Start = ifelse(length(weighted_mode(StartTime, Weights)) > 1, median(StartTime), weighted_mode(StartTime, Weights)),
                  H_Start = max(StartTime),
                  average_duration = weighted.mean(DURATION_EXT, Weights),
                  std_duration = sqrt(wtd.var(DURATION_EXT, Weights)))
      if(any(is.na(subStart$days))) stop()
      subStart$Activity <- activity
      position_columns <- unique(match(profile_iteration, colnames(subStart)))
      new_columns <- paste("Cat_", 1:length(position_columns), sep = "")
      colnames(subStart)[position_columns] <- new_columns
      
      ### Proportion per hour 
      subStart_proportion <- df_StartTime %>%
        group_by_at(c("days", profile_iteration, "StartTime")) %>%
        summarise(total_StartTime = sum(Weights)/(91*4), 
                  average_duration = weighted.mean(DURATION_EXT, Weights),
                  std_duration = sqrt(wtd.var(DURATION_EXT, Weights))) %>%
        mutate(proportion_StartTime = total_StartTime/sum(total_StartTime))
      position_columns <- unique(match(profile_iteration, colnames(subStart_proportion)))
      new_columns <- paste("Cat_", 1:length(position_columns), sep = "")
      colnames(subStart_proportion)[position_columns] <- new_columns
      subStart_proportion$Activity <- activity

    # }
    
    if(profile_type == "Combined"){
      subStart[, "Var_1"] <- profile_iteration[1]
      subStart[, "Var_2"] <- profile_iteration[2]
      df_Start_Out_2 <- rbind(df_Start_Out_2, subStart)
      
      subStart_proportion[, "Var_1"] <- profile_iteration[1]
      subStart_proportion[, "Var_2"] <- profile_iteration[2]
      df_StartProportion_Out_2 <- rbind(df_StartProportion_Out_2, subStart_proportion)
      
    }else{
      subStart[, "Var_1"] <- profile_iteration[1]
      df_Start_Out_1 <- rbind(df_Start_Out_1, subStart)
      
      subStart_proportion[, "Var_1"] <- profile_iteration[1]
      df_StartProportion_Out_1 <- rbind(df_StartProportion_Out_1, subStart_proportion)
    }
  }
}

dim(df_Start_Out_1)
Triangular_StartTimes_Single <- unique(na.omit(df_Start_Out_1))
dim(Triangular_StartTimes_Single)

dim(df_Start_Out_2)
Triangular_StartTimes_Combined <- unique(na.omit(df_Start_Out_2))
dim(Triangular_StartTimes_Combined)

dim(df_StartProportion_Out_1)
Proportion_StartTimes_Single <- unique(na.omit(df_StartProportion_Out_1))
dim(Proportion_StartTimes_Single)

dim(df_StartProportion_Out_2)
Proportion_StartTimes_Combined <- unique(na.omit(df_StartProportion_Out_2))
dim(Proportion_StartTimes_Combined)

### Main outputs

write.csv(data.frame(Triangular_StartTimes_Single), file = file.path(output_path, "Triangular_StartTimes_Single.csv"), row.names = F)
write.csv(data.frame(Triangular_StartTimes_Combined), file = file.path(output_path, "Triangular_StartTimes_Combined.csv"), row.names = F)
save(Triangular_StartTimes_Single, file = file.path(output_path, "Triangular_StartTimes_Single.Rdata")) # # Single variables
save(Triangular_StartTimes_Combined, file = file.path(output_path, "Triangular_StartTimes_Combined.Rdata")) # # Single variables

write.csv(data.frame(Proportion_StartTimes_Combined), file = file.path(output_path, "Proportion_StartTimes_Combined.csv"), row.names = F)
save(Proportion_StartTimes_Combined, file = file.path(output_path, "Proportion_StartTimes_Combined.Rdata")) # # Single variables

### Other outputs for Validation
save(df_Consolidate_StartTime, file = file.path(output_path, "df_Consolidate_StartTime.Rdata")) 
save(df_Consolidate_StopTime, file = file.path(output_path, "df_Consolidate_StopTime.Rdata"))  

