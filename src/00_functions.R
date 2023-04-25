############################################################################################
#### FUNCTIONS 
############################################################################################
fun_indicator_activity <- function(df_activity, indicator_hour_activity){
  #' Indicator activity
  #'
  #' @description This function identifies daily and weekly activities for each individual.
  #'
  #' @param df_activity data.frame. It corresponds to the ATUS data frame. 
  #' @param indicator_hour_activity list. The elements are data frames with all the respondents and daily hours.¡
  #' @param X_ACT character. character with the name of the Activity 
  #' @return an object of class "data.frame" with all the combinations: profiles and activities.
  #' @details The function is divided in two parts: activities performed on the same day and activities that starts on day
  #' before and ends day after. Additionally, fhe function consolidates the frequency times, and start and end times that 
  #' corresponds to the individual response. 
   

  ### when the activity starts day before, and stop on the same day 
  df_activity_1 <-  df_activity %>% filter(StartTime <= StopTime)   
  ## analysis for all days 
  for( ii in 1:7){
    temp_df <- df_activity_1 %>% filter(days_code == ii) %>% select(ID, StartTime, StopTime)
    if(nrow(temp_df) == 0) next
    temp_hour <- indicator_hour_activity[[ii]]
    for(jj in 1:nrow(temp_df)){
      repo_hour <- paste("H", temp_df$StartTime[jj]:temp_df$StopTime[jj], sep = "_")  
      temp_hour[temp_hour$ID == temp_df$ID[jj], repo_hour] <- TRUE
    }
    
    temp_nTimDay <- temp_df %>% 
      group_by(ID) %>% 
      summarise(nTimeD = n(), # # Counting number of times per day
                StartTime_D = paste(StartTime, collapse = ","),  # # Collapsing all starting times to estimate afterwards 
                StopTime_D = paste(StopTime, collapse = ",")) # # Collapsing all stop times to estimate afterwards 
    temp_hour <- merge(temp_hour, temp_nTimDay, by = "ID", all.x = T) 
    temp_hour$nTimeD <- ifelse(is.na(temp_hour$nTimeD), 0, temp_hour$nTimeD)
    # # Adding the number of times doing that activity from the previous one
    temp_hour$nTimesDay <- with(temp_hour, nTimesDay + nTimeD)
    # # Collapsing the start and stop times reported in one string by adding to the previous one  
    temp_hour$StartTime <- with(temp_hour, paste(StartTime, StartTime_D, sep = ","))
    temp_hour$StopTime <- with(temp_hour, paste(StopTime, StopTime_D, sep = ","))
    temp_hour$nTimeD <- NULL
    temp_hour$StartTime_D <- NULL
    temp_hour$StopTime_D <- NULL
    indicator_hour_activity[[ii]] <- temp_hour
  }
  
  ### when the activity starts day before, and stop on day after 
  df_activity_2 <- df_activity %>% filter(StartTime > StopTime)   
  for (ii in 1:7){
    temp_df <- df_activity_2 %>% filter(days_code == ii) %>% select(ID, StartTime, StopTime)
    if(nrow(temp_df) == 0) next
    ## Activities that starts day before and ends on the day after.
    # Example: Saturday (7) day before, Sunday (1) day after 
    if(ii < 7){
      temp_hour_dB <- indicator_hour_activity[[ii]] ## day before 
      temp_hour_dA <- indicator_hour_activity[[ii+1]] ## day After    
    }else{
      temp_hour_dB <- indicator_hour_activity[[7]] ## day before 
      temp_hour_dA <- indicator_hour_activity[[1]] ## day After    
    }
    
    for(jj in 1:nrow(temp_df)){
      repo_hour_dB <- paste("H",  temp_df$StartTime[jj]:23, sep = "_")  
      repo_hour_dA <- paste("H", 0:temp_df$StopTime[jj], sep = "_")  
      temp_hour_dB[temp_hour_dB$ID == temp_df$ID[jj], repo_hour_dB] <- TRUE
      temp_hour_dA[temp_hour_dA$ID == temp_df$ID[jj], repo_hour_dA] <- TRUE
    }
    temp_nTimDay <- temp_df %>% 
      group_by(ID) %>% 
      summarise(nTimeD = n(), # # Counting number of times per day
                StartTime_D = paste(StartTime, collapse = ", "),  # # Collapsing all starting times to estimate afterwards 
                StopTime_D = paste(StopTime, collapse = ", ")) # # Collapsing all stop times to estimate afterwards )
    
    if(ii < 7){
      temp_hour_dB <- merge(temp_hour_dB, temp_nTimDay, by = "ID", all.x = T) 
      temp_hour_dB$nTimeD <- ifelse(is.na(temp_hour_dB$nTimeD), 0, temp_hour_dB$nTimeD)
      temp_hour_dB$nTimesDay <- with(temp_hour_dB, nTimesDay + nTimeD)
      temp_hour_dB$StartTime <- with(temp_hour_dB, paste(StartTime, StartTime_D, sep = ","))
      temp_hour_dB$nTimeD <- NULL
      temp_hour_dB$StartTime_D <- NULL
      temp_hour_dB$StopTime_D <- NULL
      
      temp_hour_dA <- merge(temp_hour_dA, temp_nTimDay, by = "ID", all.x = T) 
      temp_hour_dA$nTimeD <- ifelse(is.na(temp_hour_dA$nTimeD), 0, temp_hour_dA$nTimeD)      
      temp_hour_dA$nTimesDay <- with(temp_hour_dA, nTimesDay + nTimeD)
      temp_hour_dA$StopTime <- with(temp_hour_dA, paste(StopTime, StopTime_D, sep = ","))
      temp_hour_dA$nTimeD <- NULL
      temp_hour_dA$StartTime_D <- NULL
      temp_hour_dA$StopTime_D <- NULL
      
      indicator_hour_activity[[ii]] <- temp_hour_dB
      indicator_hour_activity[[ii+1]] <- temp_hour_dA    
    }else{
      temp_hour_dB <- merge(temp_hour_dB, temp_nTimDay, by = "ID", all.x = T) 
      temp_hour_dB$nTimeD <- ifelse(is.na(temp_hour_dB$nTimeD), 0, temp_hour_dB$nTimeD)
      temp_hour_dB$nTimesDay <- with(temp_hour_dB, nTimesDay + nTimeD)
      temp_hour_dB$StartTime <- with(temp_hour_dB, paste(StartTime, StartTime_D, sep = ","))
      temp_hour_dB$nTimeD <- NULL
      temp_hour_dB$StartTime_D <- NULL
      temp_hour_dB$StopTime_D <- NULL
      
      temp_hour_dA <- merge(temp_hour_dA, temp_nTimDay, by = "ID", all.x = T) 
      temp_hour_dA$nTimeD <- ifelse(is.na(temp_hour_dA$nTimeD), 0, temp_hour_dA$nTimeD)      
      temp_hour_dA$nTimesDay <- with(temp_hour_dA, nTimesDay + nTimeD)
      temp_hour_dA$StopTime <- with(temp_hour_dA, paste(StopTime, StopTime_D, sep = ","))
      temp_hour_dA$nTimeD <- NULL
      temp_hour_dA$StartTime_D <- NULL
      temp_hour_dA$StopTime_D <- NULL
      
      indicator_hour_activity[[7]] <- temp_hour_dB
      indicator_hour_activity[[1]] <- temp_hour_dA        
    }
  }
  df_consolidated <- do.call("rbind", indicator_hour_activity)
  return(df_consolidated)
}

fun_HourEstim <- function(data = df_indicator_consolidated_long, profile = profile_iteration , days = df_days, activity = activity){
  #' Calculate hourly estimations according to ATUS weighting scheme. 
  #'
  #' @description The function rescales the estimation of totals according to the ATUS weights (WT06), and return estimations according to the hour of the day.
  #'
  #' @param data data.frame. It corresponds to a data frame in long format obtained from the function fun_indicator_activity.
  #' @param profile vector. It contains the profile characteristics to be measured. 
  #' @param days data.frame. Data with the list of days of the week. 
  #' @param activity character. character with the name of the Activity 
  #' @return data.frame with estimations per each profile
  #' 
  
  estimates <- data %>%
    group_by_at(c(paste(c("H0_23", "days", profile)))) %>%
    summarise(Estim = sum(Weights * Indicator_Activity, na.rm = T))
  estimates$days <- ordered(estimates$days, levels = days$days[-8])
  estimates <- estimates %>% arrange(days)
  estimates$EstimPP <- estimates$Estim/(91*4)
  estimates$Activity <- activity
  return(estimates)  
}


fun_DayEstim <- function(data = df_indicator_consolidated, profile = profile_iteration , days = df_days, activity = activity){
  #' Calculate daily estimations according to ATUS weighting scheme. 
  #'
  #' @description The function rescales the estimation of totals according to the ATUS weights (WT06), and return estimations according to the day of the week.
  #'
  #' @param data data.frame. It corresponds to a data frame in long format obtained from the function fun_indicator_activity.
  #' @param profile vector. It contains the profile characteristics to be measured. 
  #' @param days data.frame. Data with the list of days of the week. 
  #' @param activity character. character with the name of the Activity 
  #' @return data.frame with estimations per each profile
  #' 
  estimates <- data %>%
    group_by_at(c(paste(c("days", profile)))) %>%
    summarise(Estim = sum(Weights * doneActivity, na.rm = T), 
              nTimesDay = weighted.mean(nTimesDay, Weights,na.rm = TRUE))
  estimates$days <- ordered(estimates$days, levels = days$days)
  estimates$EstimPP <- estimates$Estim/(91*4)
  estimates <- estimates %>% arrange(days)
  estimates$Activity <- activity
  return(estimates)  
}

fun_plotHeatmap <- function(list_estimates_hourly, 
                            df_combined_target_profiles,
                            profile_1,
                            profile_2,
                            days_class = week_day, 
                            week_title = "WeekDay", 
                            path){

  #' Plotting activities performed
  #'
  #' @description This function is used to visualize the activities during one typical day. 
  #'
  #' @param list_estimates_hourly data.frame. It corresponds to a data frame in long format obtained from the function fun_indicator_activity.
  #' @param df_combined_target_profiles vector. It contains the profile characteristics to be measured. 
  #' @param profile_1 character. It indicates the first characteristic in the profile.
  #' @param profile_2 character. It indicates the second characteristic in the profile.
  #' @param days_class vector. It has the group of days to be visualized. 
  #' @param week_title character. It is the main title that corresponds to the days_class
  #' @param path character. It has the path route to the output folder.
  #' @return It returns the plots in png format at the specified folder. 
  #'   
  profile_type <- df_combined_target_profiles %>% filter(Var1 == profile_1 & Var2 == profile_2) %>% select(typeEstim)
  profile_type <- as.character(profile_type)
  profile_number <- with(df_combined_target_profiles, Var1 == profile_1 & Var2 == profile_2)
  profile_number <- which(profile_number)
  list_activities <- lapply(list_estimates_hourly, function(x, profile_type)x[[profile_type]], profile_type)
  to_delete <- sapply(list_activities, length)
  to_delete <- which(to_delete == 0)
  list_activities[[to_delete]] <- NULL
  list_activities <- lapply(list_activities, function(x, profile_number)x[[profile_number]], profile_number)
  
  activities_binded <- do.call("rbind", list_activities)
  position_columns <- unique(match(c(profile_1, profile_2), colnames(activities_binded)))
  new_columns <- paste("Var_", 1:length(position_columns), sep = "")
  colnames(activities_binded)[position_columns] <- new_columns
  categories_activities <- unique(activities_binded[, new_columns])
  categories_activities <- data.frame(categories_activities)
  for(ll in 1:nrow(categories_activities)){
    
    if(profile_type == "Combined"){
      forVar_1 <- as.character(categories_activities[ll, 1])
      forVar_2 <- as.character(categories_activities[ll, 2])
      activities_binded_subset <- activities_binded %>% filter(days %in% days_class &
                                                                 Var_1 == forVar_1 &
                                                                 Var_2 == forVar_2)
      forTitle <- paste(forVar_1, forVar_2, sep = "-")
    }else{
      forVar_1 <- as.character(categories_activities[ll, 1])
      activities_binded_subset <- activities_binded %>% filter(days %in% days_class &
                                                                 Var_1 == forVar_1)
      forTitle <- forVar_1
    }
    
    activities_binded_grouped <- activities_binded_subset %>% 
      group_by(H0_23, Activity) %>% 
      summarise(Tot_Act = sum(EstimPP)) 
    activities_weekly_proportion <- activities_binded_grouped %>%
      group_by(Activity) %>% 
      summarise(Hour = H0_23, 
                PropPP = Tot_Act/sum(Tot_Act)) 
    
    activities_weekly_proportion$Activity <- factor(activities_weekly_proportion$Activity, levels = v_activs) 
    activities_weekly_proportion <- activities_weekly_proportion %>% arrange(Activity)
    activities_weekly_proportion$Hour <- gsub("H_", "", activities_weekly_proportion$Hour)
    activities_weekly_proportion$Hour <- as.numeric(as.character(activities_weekly_proportion$Hour))
    f_title <- paste(week_title, forTitle, sep = ":")
    f_name <- gsub(" ", "", f_title)
    f_name <- gsub("\\(|\\,|\\]|\\[|\\)|\\:|\\-", "_", f_name)
    gPlot_C <- ggplot(data = activities_weekly_proportion, aes(x=Hour, y=Activity, fill=PropPP)) + 
      geom_tile() +  
      scale_fill_gradient(low = "white", high = "red") +
      theme_ipsum() + 
      scale_x_continuous(breaks = seq(0, 23, 1)) + 
      ggtitle(f_title)
    ggsave(gPlot_C, file=paste0(path, f_name,".png"), 
           width = 12, height = 8)
  }
}

fn_rtriangle_mod <- function(df_data){
  L_Triangular =  df_data[1] # Minimum (Low)
  M_Triangular =  df_data[2] # Mode 
  H_Triangular =  df_data[3] # Minimum (High)
  if(L_Triangular == M_Triangular & M_Triangular == H_Triangular){
    random_triang = L_Triangular
    return(random_triang) 
  }else{
    if(L_Triangular == M_Triangular & M_Triangular < H_Triangular){
      random_triang = rtriangle(n = 1, L_Triangular, H_Triangular, M_Triangular + 0.1)
      return(random_triang) 
    }
    if(L_Triangular < H_Triangular & M_Triangular == H_Triangular){
      random_triang = rtriangle(n = 1, L_Triangular, H_Triangular + 0.1, M_Triangular)
      return(random_triang) 
    }
    if(L_Triangular < H_Triangular & M_Triangular < H_Triangular){
      random_triang = rtriangle(n = 1, L_Triangular, H_Triangular, M_Triangular)
      return(random_triang) 
    }
  }
}

fun_MSE <- function(data_frame, original, simulation){
  data_frame <- data.frame(data_frame)
  data_frame[, "original"] <- as.numeric(data_frame[, original] )
  data_frame[, "simulation"] <- as.numeric(data_frame[, simulation] )
  MSE_out <- data_frame %>%
    group_by(Cat_1, Cat_2, days) %>%
    summarise(mse_TypeI = sqrt(sum((original- simulation)**2/length(original))), 
              mse_TypeII = sum(abs(original- simulation)/simulation), 
              std_residual = mean(original- simulation)/mse_TypeI)
  return(MSE_out)
}

get_start_time <- function(x, type = "I"){
  if(type == "I"){
    x <- data.frame(t(x))
    x[,] <- lapply(x, as.character)
    subset_start <- subset(Proportion_StartTimes_Combined,
                           Activity == x$Activity & days == x$days &
                             Cat_1 == x$Cat_1 & Cat_2==x$Cat_2 )
    sample_profile <- sample(x = subset_start$StartTime, size = 1, prob = subset_start$proportion_StartTime)
    sample_profile <- as.numeric(sample_profile)
    return(sample_profile)
  }
  if(type == "II"){
    start_vector <- x["StartTime_Vector"]
    probs_vector <- x["proportion_StartTime_Vector"]
    start_vector <- as.numeric(unlist(strsplit(start_vector, ", ")))
    probs_vector <- as.numeric(unlist(strsplit(probs_vector, ", ")))
    if(length(start_vector) > 1){
      sample_profile <- sample(x = start_vector, size = 1, prob = probs_vector)
    }else{
      sample_profile <- start_vector
    }
    which_position <- which(sample_profile %in% start_vector)
    av_duration_vector <- x["average_duration_Vector"]
    std_duration_vector <- x["std_duration_Vector"]
    av_duration_vector <- as.numeric(unlist(strsplit(av_duration_vector, ", ")))
    std_duration_vector <- as.numeric(unlist(strsplit(std_duration_vector, ", ")))
    sample_duration <- rtruncnorm(1, a= 0, 
                                  mean = av_duration_vector[which_position], 
                                  sd = std_duration_vector[which_position])
    output <- paste(sample_profile, sample_duration, sep = ", ")
    return(output)
  }
}
