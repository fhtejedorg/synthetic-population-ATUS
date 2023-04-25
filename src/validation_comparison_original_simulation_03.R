#### VALIDATION
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

outputs_path <- "../results/Final_Results/Validation/03_comparison_original_simulation/"
############################################################################################
#### 1.1) READING THE INPUTS FROM PREVIOUS STEPS
############################################################################################
# # Reading objects from Step: Modelling  
# # Reading parameters Triangular
# # Reading start and stop times for each respondent 

load(file.path("../results/Final_Results/Validation/02_create_activities", "list_indicator_consolidated_long_profiles.Rdata"))
load(file.path(inputs_path_step1, "df_combined_target_profiles.Rdata"))
load(file.path(inputs_path_step1, "activities_vector.Rdata"))
load(file.path(inputs_path_step1, "df_days.Rdata")) 
load(file.path(inputs_path_step2, "Daily_Probability_Activity_Combined_List.Rdata"))

############################################################################################
##### Consolidated data frames and rename columns to input datasets 
df_days <- rename(df_days, days_code = code)
v_activs <- unique(v_activs)

### Consolidate population doing at least one activity in a given day, hour 
for( ll in 1:nrow(df_combined_target_profiles)){
  profile <- df_combined_target_profiles[ll, ]
  Daily_Probability_Activity_Combined <- Daily_Probability_Activity_Combined_List[[ll]]
  list_indicator_consolidated_long <- list_indicator_consolidated_long_profiles[[ll]]
  
  profile_formula <- paste("~", paste(profile[1:2], collapse = " + "))
  cat(profile_formula, "\n")
  profile_formula <- formula(profile_formula)
  
  ### Create folder per profile 
  folder_new <- paste(profile[1:2], collapse ="_")
  temp_output_path <- file.path(outputs_path, folder_new) 
  if(!dir.exists(temp_output_path)) dir.create(temp_output_path)

  df_all_indicators_consolidated_simulation <- do.call("rbind", list_indicator_consolidated_long)
  df_hourly_profile_simulation <- df_all_indicators_consolidated_simulation %>%
    filter(Indicator_Activity == 1) %>% # performing the activity
    select(Cat_1, Cat_2, days, H0_23, Weights) %>%
    group_by(H0_23, days, Cat_1, Cat_2) %>%
    summarise(Total_People_Hour_Simulation = sum(Weights))
  
  df_hourly_profile_original <- Daily_Probability_Activity_Combined$Sleeping ### take any of the list, just to extract totals 
  df_hourly_profile_original <- rename(df_hourly_profile_original, Cat_1 = profile$Var1,  Cat_2 = profile$Var2)
  df_hourly_profile_original <- merge(df_hourly_profile_original, df_hourly_profile_simulation)
  
  correlation <- cor(df_hourly_profile_original$Total_People_Hour, df_hourly_profile_original$Total_People_Hour_Simulation)
  max_xy <- max(df_hourly_profile_original$Total_People_Hour, 
                df_hourly_profile_original$Total_People_Hour_Simulation)
  
  gg_general <- ggplot(df_hourly_profile_original, aes(x =Total_People_Hour, y = Total_People_Hour_Simulation))+
    geom_point() +
    xlab("Original") + ylab("Simulation") + 
    ggtitle(paste("Comparison_Simulation_Original ", folder_new," \nStartTime--> Probabilities\nCorrelation:", round(correlation, 3), sep = ""))
  ggsave(filename = file.path(temp_output_path, "Comparison_Simulation_Original.png"), plot = gg_general)
  
  list_comparison_simulation_original <- list()
  for(kk in 1:length(v_activs)){
    activity <-  v_activs[kk]
    cat(activity, "\n")
    df_indicator_consolidated_long <-  list_indicator_consolidated_long[[activity]] 
    ### C) Calculate estimations: hourly and weekly for each activity and profile 
    df_estimate_hourly_simulation <- fun_HourEstim(data = df_indicator_consolidated_long, 
                                                   profile = c("Cat_1", "Cat_2"), 
                                                   activity = activity)
    df_estimate_hourly_simulation <- merge(df_estimate_hourly_simulation, 
                                           df_hourly_profile_simulation, 
                                           by = c("H0_23", "days", "Cat_1", "Cat_2"), all.x = T)
    ### proportion given profile and day 
    df_estimate_hourly_simulation <- df_estimate_hourly_simulation %>% 
      mutate(proportion_simulation = Estim / Total_People_Hour_Simulation)
    df_estimate_hourly_simulation <- rename(df_estimate_hourly_simulation, 
                                            Total_People_Hour_Activity_Simulation = Estim)
    ##############################################################################################################################
    # # 3.3 Calculate comparison measures: original versus sumulated 
    ##############################################################################################################################
    ### comparison per hour given a day  
    df_estimate_hourly_original <- data.table(Daily_Probability_Activity_Combined[[activity]])
    df_estimate_hourly_original <- dplyr:::rename(df_estimate_hourly_original, Cat_1 = profile$Var1, Cat_2 = profile$Var2)
    
    if(nrow(df_estimate_hourly_original) >= nrow(df_estimate_hourly_simulation)){
      df_estimate_hourly_original <- merge(df_estimate_hourly_original, 
                                           df_hourly_profile_simulation, 
                                           by = c("Cat_1", "Cat_2", "days", "H0_23"), all.x = T)
    }
    df_estimate_hourly_original <- df_estimate_hourly_original %>%
      mutate(Total_People_Hour_Activity_Original = Total_People_Hour_Simulation * probability_hourly)
    
    df_comparison <- merge(df_estimate_hourly_original[, c("Cat_1", "Cat_2", "days", "H0_23", "Total_People_Hour_Activity_Original", "Total_People_Hour")], 
                           df_estimate_hourly_simulation[, c("Cat_1", "Cat_2", "days", "H0_23", "Total_People_Hour_Activity_Simulation")], 
                           by = c("Cat_1", "Cat_2", "days", "H0_23"))
    df_comparison$proportion_deviation <- with(df_comparison, 
                                               (Total_People_Hour_Activity_Simulation - Total_People_Hour_Activity_Original)/Total_People_Hour)
    list_comparison_simulation_original[[activity]] <- df_comparison
  }
  
  
  for(kk in 1:length(v_activs)){
    activity <- v_activs[kk]
    comparison_simulation_original <- list_comparison_simulation_original[[activity]] %>% select(c("Cat_1", "Cat_2", "days","H0_23", "Total_People_Hour_Activity_Original", "Total_People_Hour_Activity_Simulation"))
    comparison_simulation_original_long <- melt(comparison_simulation_original, id.vars=c("Cat_1", "Cat_2", "days","H0_23"))
    plot_gg_densities <- ggplot(comparison_simulation_original_long, aes(x = value, color = variable)) + 
      geom_density() +
      scale_color_manual(labels = c("Original", "Simulation"), values = c("blue", "red")) + 
      facet_wrap(~ Cat_1 + Cat_2 )+
      ggtitle(paste("Activity: " ,activity, ", Profile: ", folder_new, sep = "")) 
    ggname_densities <- file.path(temp_output_path, paste("/Densities_", gsub("\\/", "_", activity), ".png", sep = ""))
    ggsave(filename = ggname_densities, plot = plot_gg_densities)
    
    max_xy <- max(comparison_simulation_original$Total_People_Hour_Activity_Original, 
                  comparison_simulation_original$Total_People_Hour_Activity_Simulation)
    plot_gg_points <- ggplot(comparison_simulation_original, aes(x = Total_People_Hour_Activity_Original,
                                                                 y = Total_People_Hour_Activity_Simulation)) + 
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color="red", 
                  linetype="dashed", size=1, alpha = 0.3) + 
      ggtitle(paste("Activity: " ,activity, ", Profile: ", folder_new, sep = "")) + 
      xlim(c(0, max_xy)) + ylim(c(0, max_xy)) + 
      facet_wrap(~ Cat_1 + Cat_2 )
    ggname_points <- file.path(temp_output_path, paste("/Scatterplot_", gsub("\\/", "_", activity), ".png", sep = ""))
    ggsave(filename = ggname_points, plot = plot_gg_points)
  }
  ##################################################################################################################################
  #### Measures ####################################################################################################################
  ##################################################################################################################################
  list_MSE <- list()
  for(kk in 1:length(v_activs)){
    activity <- v_activs[kk]
    data_setMSE <- list_comparison_simulation_original[[activity]]
    list_MSE[[activity]] <- fun_MSE(data_setMSE, "Total_People_Hour_Activity_Original", "Total_People_Hour_Activity_Simulation")
  }
  
  activity_MSE_Consolidated <- NULL
  for(kk in 1:length(v_activs)){
    activity <- v_activs[kk]
    activity_MSE <- list_MSE[[activity]]
    activity_MSE$days <- factor(activity_MSE$days, levels = df_days$days[-8])
    activity_MSE$profile <- with(activity_MSE, paste(Cat_1, Cat_2, "_"))
    plot_gg_tiles <- ggplot(activity_MSE, aes(days, profile, fill= std_residual)) + 
      geom_tile() +
      scale_fill_gradient2(limits = c(-1,1)) +
      ggtitle(paste("Activity: " ,activity, ", Profile: ", folder_new, sep = "")) 
    ggname_tiles <- file.path(temp_output_path,  paste("Validation_Std_Residual", gsub("\\/", "_", activity), ".png", sep = ""))
    ggsave(filename = ggname_tiles, plot = plot_gg_tiles)
    
    activity_MSE_Grouped <- activity_MSE %>% group_by(Cat_1, Cat_2) %>% summarise(mean(std_residual))
    activity_MSE_Grouped$Activity <- activity 
    activity_MSE_Consolidated <- rbind(activity_MSE_Consolidated, activity_MSE_Grouped)
  }
}
