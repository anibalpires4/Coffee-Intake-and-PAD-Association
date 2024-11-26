library("tidyverse")
library("haven")
library("dplyr")

library(factoextra)
library(vcd)
library(reshape2)
library(gtsummary)
library(ggeffects)
library(rrcov)
library(MASS)
library(survey)
library(magrittr)
library(rcompanion)
library(gt)
library(RColorBrewer)
library(ggpubr)
library(plotRCS)
library(pscl)
devtools::install_github("kunhuo/plotRCS")
library(ResourceSelection)
library(performance)
library(DescTools)
library(vcdExtra)
devtools::install_github("gnattino/largesamplehl")
library(devtools)
library(sjPlot)
library(car)

########################################################## CREATE THE DATABASE ###############################################################

rm(list = ls())

# Load Examination data (1999-2004) for Ankle Brachial Index (ABI) 
df_lexab_99_00=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/1999-00/Examination/LEXABPI.XPT")

df_lexab_01_02=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/2001-02/Examination/LEXAB_B.XPT")

df_lexab_03_04=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/2003-04/Examination/LEXAB_C.XPT")

# Bind Examination data (bind_rows merges all rows and keeps the union of the columns - https://www.statology.org/dplyr-bind_rows-bind_cols/)
df_lexab <- bind_rows(df_lexab_99_00, df_lexab_01_02,df_lexab_03_04)

# Remove rows with missing values on LEXLABPI or on LEXRABPI -> 7571
df_lexab <- df_lexab %>% filter(!is.na(LEXLABPI) | !is.na(LEXRABPI))
dim(df_lexab)[1]==7571

# Exclude the ones with LEXLABPI or LEXRABPI > 1.40
df_lexab <- df_lexab %>% filter(LEXLABPI<1.40 & LEXRABPI<1.40)
dim(df_lexab)[1]==6716

# Create PAD variable
df_lexab$PAD <- ifelse(df_lexab$LEXLABPI<=0.9 | df_lexab$LEXRABPI<=0.9,1,0)
#Count how many rows with PAD=1
table(df_lexab$PAD)

# Create a new dataset with only the variables we need
df_lexab <- df_lexab %>% dplyr::select(SEQN, LEXLABPI,LEXRABPI,PAD)

# Save the dataset
write.csv(df_lexab, "C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/df_lexab.csv", row.names = FALSE)
#Check the dataset size and PAD distribution
dim(df_lexab)
table(df_lexab$PAD)


# Load Dietary Data (1999-2004)
df_indiv_foods_99_00=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/1999-00/Dietary/DRXIFF.XPT")
df_indiv_foods_01_02=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/2001-02/Dietary/DRXIFF_B.XPT")
df_indiv_foods_03_04_1=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/2003-04/Dietary/DR1IFF_C.XPT") #there are two files for 2003-04 (2 days of dietary data)
df_indiv_foods_03_04_2=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/2003-04/Dietary/DR2IFF_C.XPT")

# For the same patient (SEQN) merge the rows on USDA food code by adding the grams consumed
df_indiv_foods_99_00=df_indiv_foods_99_00 %>% group_by(SEQN,DRDIFDCD) %>% dplyr::summarise(DRXIGRMS=sum(DRXIGRMS,na.rm=TRUE))
df_indiv_foods_01_02=df_indiv_foods_01_02 %>% group_by(SEQN,DRDIFDCD) %>% dplyr::summarise(DRXIGRMS=sum(DRXIGRMS,na.rm=TRUE))
df_indiv_foods_03_04_1=df_indiv_foods_03_04_1 %>% group_by(SEQN,DR1IFDCD) %>% dplyr::summarise(DR1IGRMS=sum(DR1IGRMS,na.rm=TRUE))
df_indiv_foods_03_04_2=df_indiv_foods_03_04_2 %>% group_by(SEQN,DR2IFDCD) %>% dplyr::summarise(DR2IGRMS=sum(DR2IGRMS,na.rm=TRUE))

# Merge the two 2003-04 files by doing the average of the grams per day
df_indiv_foods_03_04=full_join(df_indiv_foods_03_04_1,df_indiv_foods_03_04_2,by=c("SEQN"="SEQN","DR1IFDCD"="DR2IFDCD"))
df_indiv_foods_03_04$DRXIGRMS <- rowMeans(df_indiv_foods_03_04[,c("DR1IGRMS","DR2IGRMS")], na.rm = TRUE)
df_indiv_foods_03_04 <- df_indiv_foods_03_04 %>% dplyr::select(SEQN,DRDIFDCD=DR1IFDCD,DRXIGRMS)

# Bind Dietary data
df_indiv_foods <- bind_rows(df_indiv_foods_99_00, df_indiv_foods_01_02,df_indiv_foods_03_04)

# Count how many distinct SEQN are in the df_indiv_foods dataset
length(unique(df_indiv_foods$SEQN))

# Load the USDA food codes for coffee types
coffee_types <- read.delim("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/coffee_types.tsv", header = TRUE, sep = "\t")

coffee_types <- coffee_types[!duplicated(coffee_types$FNDDS.food.code),] #Remove the coffee types with the same code (duplicates)

# Rename the column in coffee_types to match DRDIFDCD
coffee_types <- coffee_types %>% rename('DRDIFDCD' = 'FNDDS.food.code')

#Inner join the dietary data with the coffee types data to obtain the patients that consumed coffee
df_indiv_foods_with_coffee_types <- df_indiv_foods %>% inner_join(coffee_types, by = "DRDIFDCD")

#for each seqn we choose the DRDIFDCD with the highest grams consumed
df_indiv_foods_with_coffee_types <- df_indiv_foods_with_coffee_types %>% group_by(SEQN) %>% slice(which.max(DRXIGRMS)) #7735 rows


dim(df_indiv_foods_with_coffee_types) #needs to be 7735 which are the rows without duplicates
df_indiv_foods_with_coffee_types <- df_indiv_foods_with_coffee_types %>%
mutate(
        TotalCoffeeIntake=DRXIGRMS,
        CaffeinatedStatus=Caffeinated.status,
        SugaryStatus=Sugary.status,
        FattyStatus=Fatty.status,
        MilkContainingStatus=Milk.containing.status,
    )


df_indiv_foods_with_coffee_types <- df_indiv_foods_with_coffee_types %>% 
    dplyr::select(SEQN, DRDIFDCD, TotalCoffeeIntake, CaffeinatedStatus, SugaryStatus, FattyStatus, MilkContainingStatus)

# Replace - with NA
df_indiv_foods_with_coffee_types[df_indiv_foods_with_coffee_types == "-"] <- NA

df_indiv_foods_with_coffee_types

df_lexab <- read.csv("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/df_lexab.csv")

# Select only the SEQN are present in the df_lexab dataset
df_lexab <- df_lexab %>% filter(SEQN %in% df_indiv_foods$SEQN)
dim(df_lexab)[1] == 6565

# Merge the dietary data with the lexab data (Left join because we only want the patients that have PAD)
df_pad_coffee_types <- left_join(df_lexab, df_indiv_foods_with_coffee_types, by = "SEQN")

# Create a new column with 1 for coffee consumers
df_pad_coffee_types$Coffee = ifelse(is.na(df_pad_coffee_types$TotalCoffeeIntake),0,1)

#Save the dataset
write.csv(df_pad_coffee_types, "C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/df_pad_coffee_types.csv", row.names = FALSE)

# Load Demographics Data (1999-2004)
df_demo_99_00=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/1999-00/Demographics/DEMO.XPT")
df_demo_01_02=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/2001-02/Demographics/DEMO_B.XPT")
df_demo_03_04=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/2003-04/Demographics/DEMO_C.XPT")

# Bind Demographics data
df_demo <- bind_rows(df_demo_99_00, df_demo_01_02,df_demo_03_04)

df_demo <- df_demo %>% dplyr::select(SEQN,RIAGENDR, RIDAGEYR, RIDRETH1)

#Merge the demographics data with the PAD and coffee types data (Left join because we only want the patients that have PAD variable)
df_final <- inner_join(df_pad_coffee_types, df_demo, by = "SEQN")

#Save the dataset
write.csv(df_final, "C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/data/df_final.csv", row.names = FALSE)

# Cholesterol - LDL & Triglycerides --------------------------
col_99_00=read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/LAB13AM.XPT")
col_01_02 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/L13AM_B.XPT")
col_03_04 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/L13AM_C.XPT")
col_df <- bind_rows(col_99_00, col_01_02,col_03_04)
col_df <- col_df %>% dplyr::select(SEQN, LBXTR, LBDLDL) # triglicéridos, colesterol LDL

# blood pressure --------------------------
blood_99_00 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/blood_pressure.XPT")
blood_01_02 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/blood_pressure01-02.XPT")
blood_03_04 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/blood_pressure03-04.XPT")
blood_df <- bind_rows(blood_99_00, blood_01_02,blood_03_04)
blood_df <- blood_df %>% dplyr::select(SEQN,BPXPLS,BPXML1,BPXSY1,BPXDI1,BPXSY2,BPXDI2) # 60 sec pulse, maximum inflation levels, systolic and diastolic blood pressure

# body measures --------------------------
body_99_00 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/BMX.XPT")
body_01_02 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/BMX_B.XPT")
body_03_04 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/BMX_C.XPT")
body_df <- bind_rows(body_99_00, body_01_02,body_03_04)
body_df <- body_df %>% dplyr::select(SEQN,BMXWT,BMXHT) # weigth, heigth, body mass index

# alcohol drinking --------------------------
alcohol_99_00 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/ALQ.XPT")
alcohol_01_02 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/ALQ_B.XPT")
alcohol_03_04 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/ALQ_C.XPT")
alcohol_df <- bind_rows(alcohol_99_00, alcohol_01_02, alcohol_03_04)
alcohol_df <- alcohol_df %>% dplyr::select(SEQN,ALQ120Q) # How often drink alcohol over past 12 months (number of days)

# Smoking - Cigarette/Tobacco Use - Adult --------------------------
smoking_99_00 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/SMQ.XPT")
smoking_01_02 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/SMQ_B.XPT")
smoking_03_04 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/SMQ_C.XPT")
smoking_df <- bind_rows(smoking_99_00, smoking_01_02, smoking_03_04)
smoking_df <- smoking_df %>% dplyr::select(SEQN, SMD030, SMQ040) # Age started smoking cigarets regularly, Do you now smoke cigarettes

# physical activity --------------------------
phy_99_00 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/PAQ.XPT")
phy_01_02 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/PAQ_B.XPT")
phy_03_04 <- read_xpt("C:/Users/ricar/Desktop/uni/MECD/1º Ano 2º Semestre/Bioestatística/Projeto/joao/PAQ_C.XPT")
phy_df <- bind_rows(phy_99_00, phy_01_02,phy_03_04)
phy_df <- phy_df %>% dplyr::select(SEQN, PAQ180, PAD200, PAD320) # Avg level of physical activity each day, Vigorous activity over past 30 days, Moderate activity over past 30 days

df_final <- inner_join(df_final, col_df, by = "SEQN")
df_final <- inner_join(df_final, blood_df, by = "SEQN")
df_final <- inner_join(df_final, body_df, by = "SEQN")
df_final <- inner_join(df_final, alcohol_df, by = "SEQN")
df_final <- inner_join(df_final, smoking_df, by = "SEQN")
df_final <- inner_join(df_final, phy_df, by = "SEQN")

# Move the 'PAD' column to the end of df_final_clean
df_final <- df_final[c(setdiff(names(df_final), "PAD"), "PAD")]

# Remove certain variables
df_final <- subset(df_final, select = -c(SEQN,LEXLABPI,BPXSY1,BPXDI1,BPXSY2,BPXDI2,
                                         LEXRABPI,SugaryStatus,FattyStatus,MilkContainingStatus))

# Transform types of some variables
df_final$DRDIFDCD <- as.character(df_final$DRDIFDCD)
df_final$CaffeinatedStatus <- as.character(df_final$CaffeinatedStatus)
df_final$Coffee <- as.character(df_final$Coffee)
df_final$RIAGENDR <- as.character(df_final$RIAGENDR)
df_final$RIDRETH1 <- as.character(df_final$RIDRETH1)
df_final$SMQ040 <- as.character(df_final$SMQ040)
df_final$PAQ180 <- as.character(df_final$PAQ180)
df_final$PAD200 <- as.character(df_final$PAD200)
df_final$PAD320 <- as.character(df_final$PAD320)
df_final$PAD <- as.factor(df_final$PAD)

####################### HANDLING MISSING DATA ##################################

# Missing values NA
colSums(is.na(df_final))

# Custom function to calculate the mode
calculate_mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]  # Remove NA values if na.rm is TRUE
  }
  ux <- unique(x)  # Find unique values
  ux[which.max(tabulate(match(x, ux)))]  # Return the value with the highest frequency
}

# We replace the NA values of 'TotalCoffeeIntake' with 0
df_final$TotalCoffeeIntake <- replace(df_final$TotalCoffeeIntake,is.na(df_final$TotalCoffeeIntake),0)
#We replace the NA values of 'DRDIFDCD' by a code to identify non-caffeine beverages
df_final$DRDIFDCD <- replace(df_final$DRDIFDCD,is.na(df_final$DRDIFDCD), 99999999)
#We replace the NA values of 'LBXTR' with the median
df_final$LBXTR <- replace(df_final$LBXTR,is.na(df_final$LBXTR),median(df_final$LBXTR,na.rm=TRUE))
#We replace the NA values of 'LBDLDL' with the median
df_final$LBDLDL <- replace(df_final$LBDLDL,is.na(df_final$LBDLDL),median(df_final$LBDLDL,na.rm=TRUE))
#We replace the NA values of 'BPXPLS' with the median
df_final$BPXPLS <- replace(df_final$BPXPLS,is.na(df_final$BPXPLS),median(df_final$BPXPLS,na.rm=TRUE))
#We replace the NA values of 'BPXML1' with the median
df_final$BPXML1 <- replace(df_final$BPXML1,is.na(df_final$BPXML1),median(df_final$BPXML1,na.rm=TRUE))
#We replace the NA values of 'BPXSY1' with the median
df_final$BPXSY1 <- replace(df_final$BPXSY1,is.na(df_final$BPXSY1),median(df_final$BPXSY1,na.rm=TRUE))
#We replace the NA values of 'BPXDI1' with the median
df_final$BPXDI1 <- replace(df_final$BPXDI1,is.na(df_final$BPXDI1),median(df_final$BPXDI1,na.rm=TRUE))
#We replace the NA values of 'BPXSY2' with the median
df_final$BPXSY2 <- replace(df_final$BPXSY2,is.na(df_final$BPXSY2),median(df_final$BPXSY2,na.rm=TRUE))
#We replace the NA values of 'BPXDI2' with the median
df_final$BPXDI2 <- replace(df_final$BPXDI2,is.na(df_final$BPXDI2),median(df_final$BPXDI2,na.rm=TRUE))
#We replace the NA values of 'BMXWT' with the median
df_final$BMXWT <- replace(df_final$BMXWT,is.na(df_final$BMXWT),median(df_final$BMXWT,na.rm=TRUE))
#We replace the NA values of 'BMXHT' with the median
df_final$BMXHT <- replace(df_final$BMXHT,is.na(df_final$BMXHT),median(df_final$BMXHT,na.rm=TRUE))
# Replace 999 values of 'ALQ120Q' with NA
df_final$ALQ120Q[df_final$ALQ120Q %in% 999] <- NA
#We replace the NA values of 'ALQ120Q' with the median
df_final$ALQ120Q <- replace(df_final$ALQ120Q,is.na(df_final$ALQ120Q),median(df_final$ALQ120Q,na.rm=TRUE))
# Replace 777 and 999 values of 'SMD030' with NA
df_final$SMD030[df_final$SMD030 %in% c(777, 999)] <- NA
#We replace the NA values of 'SMD030' with the median
df_final$SMD030 <- replace(df_final$SMD030,is.na(df_final$SMD030),median(df_final$SMD030,na.rm=TRUE))
# Replace NA values in SMQ040 column with the mode
df_final <- df_final %>% mutate(SMQ040 = ifelse(is.na(SMQ040), calculate_mode(SMQ040, na.rm = TRUE), SMQ040))
# Replace 9 values of 'PAQ180' with NA
df_final$PAQ180[df_final$PAQ180 %in% 9] <- NA
#We replace the NA values of 'PAQ180' with the mode
df_final <- df_final %>% mutate(PAQ180 = ifelse(is.na(PAQ180), calculate_mode(PAQ180, na.rm = TRUE), PAQ180))
# Replace 9 values of 'PAD200' with NA
df_final$PAD200[df_final$PAD200 %in% 9] <- NA
# Replace NA values in PAD200 column with the mode
df_final <- df_final %>% mutate(PAD200 = ifelse(is.na(PAD200), calculate_mode(PAD200, na.rm = TRUE), PAD200))
# Replace 7 and 9 values of 'PAD320' with NA
df_final$PAD320[df_final$PAD320 %in% c(7, 9)] <- NA
# Replace NA values in PAD320 column with the mode
df_final <- df_final %>% mutate(PAD320 = ifelse(is.na(PAD320), calculate_mode(PAD320, na.rm = TRUE), PAD320))

# Substitute NA values with -1 for observations with the 99999999 food code
df_final$CaffeinatedStatus[is.na(df_final$CaffeinatedStatus)] <- -1

# Remove the DRDIFDCD variable
df_final <- subset(df_final, select = -DRDIFDCD)

# Replace some char values with labels

######################################## CHANGE THE VALUES OF SOME VARIABLES ################################################

df_final <- df_final %>% mutate(CaffeinatedStatus = dplyr::recode(CaffeinatedStatus, 
                                                                  '0' = "Decaffeinateted Coffee", '1' = "Caffeinated Coffee"))
df_final <- df_final %>% mutate(Coffee = dplyr::recode(Coffee, '1' = "Consumed", '0' = "Not Consumed"))
df_final <- df_final %>% mutate(RIAGENDR = dplyr::recode(RIAGENDR, '1' = "Male", '2' = "Female"))
df_final <- df_final %>% mutate(RIDRETH1 = dplyr::recode(RIDRETH1, '1' = "Mexican American", 
                                                  '2' = "Other Hispanic", '3' = "Non-Hispanic White",
                                                  '4' = "Non-Hispanic Black", '5' = "Other Race - Including Multi-Racial"))
df_final <- df_final %>% mutate(SMQ040 = dplyr::recode(SMQ040, '1' = "Every day", '2' = "Some days", '3' = "Not at all"))
df_final <- df_final %>% mutate(PAD200 = dplyr::recode(PAD200, '1' = "Yes", '2' = "No", '3' = "Unable to do activity"))
df_final <- df_final %>% mutate(PAD320 = dplyr::recode(PAD320, '1' = "Yes", '2' = "No", '3' = "Unable to do activity"))
df_final <- df_final %>% mutate(PAQ180 = dplyr::recode(PAQ180, '1' = "Sit during the day and don't walk very much", 
                                                '2' = "Stand/walk a lot during the day",
                                                '3' = "Lift light load or have to climb stairs or hills often",
                                                '4' = "Do heavy work or carry heavy loads"))

########################### OUTLIERS ###########################################

# dataframe with only the numeric variables for outlier analysis
outlier_data <- subset(df_final, select = -c(CaffeinatedStatus,Coffee,RIAGENDR,
                                             RIDRETH1,SMQ040,PAQ180,PAD200,PAD320))

classes <- unique(df_final[,"PAD"])

outliers <- c()

for (i in classes) {
  d <- subset(outlier_data[outlier_data$PAD == i,], select = -c(PAD))
  pcH <- PcaHubert(d, k=0,alpha=0.75)
  distances <- pcH$od
  outliers <- append(outliers, as.integer(names(distances[distances>pcH$cutoff.od])))
}

# Function that generates the outlier map
for (i in classes) {
  d <- subset(outlier_data[outlier_data$PAD == i,], select = -c(PAD)) 
  pcH <- PcaHubert(d, k=0, alpha=0.75)
  plot(pcH, main = i)
}

df_final <- df_final[-c(outliers),]

################### SCALING OF THE VARIABLES ###########################

df_final_stand <- df_final

for (i in names(df_final_stand)) {
  if (is.numeric(df_final_stand[[i]])) {
    df_final_stand[[i]] <- scale(df_final_stand[[i]])
  }
}

##################################### EDA ######################################

# Bar Plots for Categorical Variables

# Coffee

data_coffee <- table(df_final_stand$Coffee, df_final_stand$PAD)
data_coffee <- data.frame(data_coffee)
names(data_coffee) <- c("Coffee","PAD","Frequency")
ggplot(data_coffee, aes(fill=PAD, y=Frequency, x=Coffee)) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() + theme(legend.position = "top")

# Caffeinated Status

# Filter out the -1 values of the Caffeinated Status variable
df_filtered <- df_final_stand[df_final_stand$CaffeinatedStatus != -1, ]

data_caffeinated <- table(df_filtered$CaffeinatedStatus, df_filtered$PAD)
data_caffeinated <- data.frame(data_caffeinated)
names(data_caffeinated) <- c("Caffeinated Status","PAD","Frequency")
ggplot(data_caffeinated, aes(fill=PAD, y=Frequency, x=`Caffeinated Status`)) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() + theme(legend.position = "top")

# Histograms for Numerical Variables
for (i in names(df_final_stand)) {
  if (typeof(df_final_stand[[i]]) != "character" && typeof(df_final_stand[[i]]) != "integer") {
    h <- ggplot(df_final_stand, aes(x = .data[[i]])) +
      geom_histogram(fill = "grey", color = "white", bins = 20) +
      labs(x = i, y = "Frequency") +
      theme_minimal()
    print(h)
  }
}

# Cramer's V matrix between categorical features

# Select columns that are categorical
char_and_factor_df_final_stand <- df_final_stand[, sapply(df_final_stand, function(x) is.character(x))]

cramer_mat <- matrix(nrow=8,ncol=8)

for (i in 1:8) {
  for (j in 1:8) {
    cramer_mat[i,j]=cramerV(char_and_factor_df_final_stand[[i]],char_and_factor_df_final_stand[[j]])
  }
}

# Assign row and column names to the matrix
colnames(cramer_mat) <- colnames(char_and_factor_df_final_stand)
rownames(cramer_mat) <- colnames(char_and_factor_df_final_stand)

cramer_mat=gt(as.data.frame(cramer_mat))
data_color(cramer_mat,palette = "Blues")

# Compute Pearson's correlation matrix

# Compute Pearson's correlation matrix
numeric_df_final_stand <- df_final_stand[sapply(df_final_stand, is.numeric)]
cormat <- round(cor(numeric_df_final_stand, use = "complete.obs"), 2)

# Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Reorder the correlation matrix
reorder_cormat <- function(cormat) {
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
  return(cormat)
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1, 1), space = "Lab", 
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() + # minimal theme
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.box = "horizontal",  # Place legend box horizontally
    legend.box.just = "top",  # Justify legend box at the top
    legend.position = c(0.6, 0.7)  # Position legend inside the plot with numeric coordinates
  ) +
  coord_fixed()

# Adds correlation coefficients on the heatmap
ggheatmap <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5
    )
  )

# Print the heatmap
print(ggheatmap)

#Boxplots
boxplot(df_final$TotalCoffeeIntake,
        ylab = "Amount of coffee (g)",
        main = "Distribution of Total Coffee Intake",
        col = "grey")

boxplot(TotalCoffeeIntake ~ RIAGENDR, data = df_final,
        xlab = "Gender", ylab = "Amount of coffee (g)",
        main = "Difference in coffee intake by gender",
        col = c("grey","grey"))

########################### DATA SUMMARIZATION #################################

df_final_stand %>% 
  dplyr::select(PAD,TotalCoffeeIntake,CaffeinatedStatus,Coffee,RIAGENDR,RIDAGEYR,
                RIDRETH1,LBXTR,LBDLDL,BMXWT,BMXHT,ALQ120Q,SMD030,PAQ180) %>%
  tbl_summary(
    by = PAD,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(2, 2),
                  all_categorical() ~ c(0, 1)),
    type = list(TotalCoffeeIntake ~ "continuous",
                CaffeinatedStatus ~ "categorical",
                Coffee ~ "categorical",
                RIAGENDR ~ "categorical",
                RIDAGEYR ~ "continuous",
                RIDRETH1 ~ "categorical",
                LBXTR ~ "continuous",
                LBDLDL ~ "continuous",
                BMXWT ~ "continuous",
                BMXHT ~ "continuous",
                ALQ120Q ~ "continuous",
                SMD030 ~ "continuous",
                PAQ180 ~ "categorical"),
    label  = list(TotalCoffeeIntake ~ "Total Coffee Intake (g/day)",
                  CaffeinatedStatus ~ "Caffeinated Coffee",
                  Coffee ~ "Coffee",
                  RIAGENDR ~ "Gender",
                  RIDAGEYR ~ "Age",
                  RIDRETH1 ~ "Race",
                  LBXTR ~ "Triglyceride (mg/dL)",
                  LBDLDL ~ "LDL-cholesterol (mg/dL)",
                  BMXWT ~ "Weight (kg)",
                  BMXHT ~ "Height (cm)",
                  ALQ120Q ~ "How often drink alcohol over past 12 months",
                  SMD030 ~ "Age started smoking cigarets regularly",
                  PAQ180 ~ "Avg level of physical activity each day")
  ) %>%
  modify_header(
    label = "**Variable**",
    # The following adds the % to the column total label
    # <br> is the location of a line break
    all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p, digits=1)}%)"
  ) %>%
  add_overall(
    last = FALSE,
    # The ** make it bold
    col_label = "**All participants**<br>N = {N}"
  ) %>%
  add_p(
    test = list(all_continuous()  ~ "t.test",
                all_categorical() ~ "chisq.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  as_gt() %>% 
  tab_options(column_labels.background.color = "lightgrey",
              table.background.color = "white")

############################### CHI-SQUARE TEST ################################

# Create a contingency table
contingency_table <- table(df_final_stand$Coffee, df_final_stand$PAD)

# Perform the Chi-Square test
chi_square_result <- chisq.test(contingency_table)

print(chi_square_result)

contingency_table2 <- table(df_final_stand$CaffeinatedStatus, df_final_stand$PAD)

chi_square_result2 <- chisq.test(contingency_table2)

print(chi_square_result2)

##################### WEIGHTED BINARY LOGISTIC REGRESSION ######################

# Fit initial logistic regression model excluding 'Coffee' variable
final_model <- glm(PAD ~ . - Coffee, family = binomial, data = df_final_stand)

# Generate residual plots for diagnostic purposes
residualPlots(final_model)

# Fit the model again for backward stepwise selection
model <- glm(PAD ~ . - Coffee, data = df_final_stand, family = binomial(link = "logit"))
summary(model)  # Display summary of the initial model

# Perform backward stepwise model selection
new <- step(model, direction = "backward")
summary(new)  # Display summary of the selected model

# Calculate Pearson residuals and their variance and mean
resi_new <- residuals(new, type = "pearson")
var(resi_new)
mean(resi_new)

# Calculate Cook's distance to identify influential observations
cooks <- cooks.distance(new)
plot(cooks, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 8/(((new$df.null) + 1) - 2 * (new$rank + 1)), col = "red")  # Threshold line for Cook's distance

# Identify influential observations
influential <- as.numeric(names(cooks)[cooks > 8/(((new$df.null) + 1) - 2 * (new$rank + 1))])

# Remove influential observations from the dataset
final_no <- df_final_stand[-influential, ]

# Create a new dataframe with selected variables for the final model
final_no_ds <- data.frame(
  final_no$PAD, final_no$TotalCoffeeIntake, final_no$CaffeinatedStatus,
  final_no$RIDAGEYR, final_no$RIDRETH1, final_no$SMQ040,
  final_no$LBXTR, final_no$LBDLDL, final_no$BPXML1,
  final_no$SMQ040, final_no$PAD200
)

# Fit final logistic regression model using the cleaned dataset
final_lm <- glm(final_no.PAD ~ ., family = binomial(link = "logit"), data = final_no_ds)

# Perform Hosmer-Lemeshow goodness-of-fit test
hl_gof <- hoslem.test(final_lm$y, fitted(final_lm))
hl_gof

# Plot Pearson residuals for the final model
pearson_residuals <- residuals(final_lm, type = "pearson")
plot(pearson_residuals, main = "Pearson Residuals", ylab = "Pearson Residuals", xlab = "Index")
abline(h = 0, col = "red")
var(pearson_residuals)
mean(pearson_residuals)

# Plot deviance residuals for the final model
deviance_residuals <- residuals(final_lm, type = "deviance")
plot(deviance_residuals, main = "Deviance Residuals", ylab = "Deviance Residuals", xlab = "Index")
var(deviance_residuals)

summary(final_lm)
plot(final_lm)

# Fit Model 1 with selected covariates
fit_model1 <- glm(final_no.PAD ~ . - final_no.SMQ040, data = final_no_ds, family = binomial(link = "logit"))

# Compute confidence intervals for the odds ratios of the covariates
CI <- exp(confint(fit_model1))[-1, ]

# Plot odds ratios with their confidence intervals using sjPlot
plot_model(fit_model1,
           axis.lim = c(min(CI), max(CI)),
           auto.label = FALSE)
