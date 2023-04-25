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
output_path <- "../results/Final_Results/Analysis/step 1 - data processing"
############################################################################################
#### 1) DATA SOURCES: ATUS +  ACTIVITY_CODES (ACTIVITIES and OCCUPATIONS)
############################################################################################

# # ATUS 
ddi <- read_ipums_ddi("../data/raw/IPUMS ATUS 2019/atus_00009.xml")
df_ATUS <- read_ipums_micro(ddi)
dat_County <- read.xlsx2("../data/external/Activity Codes ATUS.xlsx", sheetName = "County_Texas")

# # ACTIVITY CODES 
df_ActHeros <- read.xlsx2("../data/external/Activity Codes ATUS.xlsx", sheetName = "Activities_ATUS")
head(df_ActHeros)
df_OccupHeros <- read.xlsx2("../data/external/Activity Codes ATUS.xlsx", sheetName = "Occupation_ATUS")

############################################################################################
#### 2) FILTERING for TEXAS State (sample is not representative at the county level).
####    VARIABLE SELECTION and TRANSFORMATION
############################################################################################
### Creating Texas data.frame
df_ATUS_Texas <- df_ATUS[grep("^48", df_ATUS$COUNTY, ignore.case=F), ]

### Changing the type of object inside R 
df_ATUS_Texas$ACTIVITY2 <- as.numeric(as.character(df_ATUS_Texas$ACTIVITY))
df_ATUS_Texas$SCHLCOLL_2 <- as.numeric(as.character(df_ATUS_Texas$SCHLCOLL))
df_ATUS_Texas$EMPSTAT_2 <- as.numeric(as.character(df_ATUS_Texas$EMPSTAT))
df_ActHeros$Code_ATUS <- as.numeric(as.character(df_ActHeros$Code_ATUS))
df_OccupHeros$Code_ATUS <- as.numeric(as.character(df_OccupHeros$Code_ATUS))
df_OccupHeros$OccupationHEROS <- as.character(df_OccupHeros$OccupationHEROS)
df_OccupHeros$OccupationHEROS <- ifelse(df_OccupHeros$OccupationHEROS == "", NA, df_OccupHeros$OccupationHEROS)

############################################################################################
#### 3) INTEGRATION: ATUS dataset with Activity codes and Occupations
####    RECATEGORIZATION: new columns are created to define each category for each characteristic of interest
############################################################################################
df_ATUS_Texas <-  left_join(df_ATUS_Texas, df_ActHeros, by = c("ACTIVITY2" = "Code_ATUS"))

df_OccupHeros_SCHLCOLL<- df_OccupHeros %>% filter(Column == "SCHLCOLL") %>% select(Code_ATUS, OccupationHEROS)
colnames(df_OccupHeros_SCHLCOLL)[2] <- "Occup_HEROS_Student" 
df_OccupHeros_EMPLOYED <- df_OccupHeros %>% filter(Column == "EMPSTAT") %>% select(Code_ATUS, OccupationHEROS)
colnames(df_OccupHeros_EMPLOYED)[2] <- "Occup_HEROS_Employed" 

# # INTEGRATION 
df_ATUS_Texas <-  left_join(df_ATUS_Texas, df_OccupHeros_SCHLCOLL, by = c("SCHLCOLL_2" = "Code_ATUS"))
df_ATUS_Texas <-  left_join(df_ATUS_Texas, df_OccupHeros_EMPLOYED, by = c("EMPSTAT_2" = "Code_ATUS"))

# # Filtering dataset to individuals who have any HERoS activity associated. Data for 2019 
df_ATUS_Texas_Heros <- df_ATUS_Texas %>% filter(CategoryHEROS != "" & YEAR == 2019)

# # RECATEGORIZATION 
df_ATUS_Texas_Heros$CohAge <- cut(df_ATUS_Texas_Heros$AGE, 
                                  breaks = c(14, 25, 44, 64, max(df_ATUS_Texas_Heros$AGE)))
df_ATUS_Texas_Heros$SEX2 <- as_factor(df_ATUS_Texas_Heros$SEX)
df_ATUS_Texas_Heros$FAMINCOME2 <- as_factor(df_ATUS_Texas_Heros$FAMINCOME)
df_ATUS_Texas_Heros$IncomeNew <- with(df_ATUS_Texas_Heros, 
                                      ifelse(FAMINCOME %in% 1:8, "Less than $29,999", 
                                             ifelse(FAMINCOME %in% 9:12, "$30,000 to $59,999",
                                                    ifelse(FAMINCOME %in% 13:14, "$60,000 to $99,999",
                                                           ifelse(FAMINCOME %in% 15:16, "$100,000 and over", NA)))))
label_income <- c("Less than $29,999", "$30,000 to $59,999", "$60,000 to $99,999", "$100,000 and over")
df_ATUS_Texas_Heros$IncomeNew <- factor(df_ATUS_Texas_Heros$IncomeNew, levels = label_income)
df_ATUS_Texas_Heros$HH_SIZE2 <- with(df_ATUS_Texas_Heros, 
                                     ifelse(HH_SIZE %in% 4:8, 5, HH_SIZE))
df_ATUS_Texas_Heros$HH_SIZE2 <- factor(df_ATUS_Texas_Heros$HH_SIZE2,  
                                       labels = c("HS_1", "HS_2", "HS_3", "HS_>3")) 

df_ATUS_Texas_Heros$Occup_HEROS_Student <-factor(df_ATUS_Texas_Heros$Occup_HEROS_Student, 
                                                 levels= unique(df_ATUS_Texas_Heros$Occup_HEROS_Student))
df_ATUS_Texas_Heros$Occup_HEROS_Employed <-factor(df_ATUS_Texas_Heros$Occup_HEROS_Employed, 
                                                  levels= unique(df_ATUS_Texas_Heros$Occup_HEROS_Employed))

df_ATUS_Texas_Heros$Subcategory_ModifiedHEROS <- as.character(df_ATUS_Texas_Heros$Subcategory_ModifiedHEROS)

############################################################################################
#### 4) DEFINING ESTIMATION OBJECTS: list that will contain the estimation for each combination of characteristics
############################################################################################
# # Data frame with days of the week according to the ATUs survey 
df_days <- data.frame(val_labels(df_ATUS_Texas_Heros$DAY))
df_days$days <- rownames(df_days)
colnames(df_days) <- c("code", "days")

# # Vector with all the activities. Total is added in order to calculate 
# # estimations for all the activities in the dataset

v_activs <- unique(df_ATUS_Texas_Heros$Subcategory_ModifiedHEROS)
v_activs <- as.character(v_activs)
v_activs <- ifelse(v_activs =="", NA, v_activs)
v_activs <- v_activs[!is.na(v_activs)]

v_activs <- unique(v_activs)

target_profiles <- c("CohAge", "SEX2", "IncomeNew", 
                     "Occup_HEROS_Student", "Occup_HEROS_Employed", 
                     "HH_SIZE2")
df_combined_target_profiles <- expand.grid(target_profiles[1], target_profiles[-1])
df_combined_target_profiles <- data.frame(df_combined_target_profiles)
colnames(df_combined_target_profiles) <- c("Var1", "Var2")
df_combined_target_profiles[,] <- apply(df_combined_target_profiles, 2, as.character)
df_combined_target_profiles$typeEstim <- with(df_combined_target_profiles, 
                                              ifelse(Var1 == Var2, "Single", "Combined"))
#### Export objects 
save(df_ATUS_Texas_Heros, file = file.path(output_path, "df_ATUS_Texas_Heros.Rdata"))
save(df_combined_target_profiles, file = file.path(output_path, "df_combined_target_profiles.Rdata"))
save(target_profiles , file = file.path(output_path, "target_profiles.Rdata"))
save(v_activs, file = file.path(output_path, "activities_vector.Rdata"))
save(df_days , file = file.path(output_path, "df_days.Rdata"))

