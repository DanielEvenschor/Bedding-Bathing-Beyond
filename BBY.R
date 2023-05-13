#' Author: Daniel Evenschor
#' Date: March 24,2023
#' Purpose: A1 

# set my working directory
setwd("C:/Users/danie/OneDrive - Hult Students/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Libraries
library(ggplot2) 
library(dplyr)
library(tidyr)
library(gridExtra)

# Read all of the files
consumerData <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv")
donationsData <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/DonationsData_training15K_studentVersion.csv")
inHouseData <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv")
magazineData <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/magazineData_training15K_studentVersion.csv")
politicalData <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/politicalData_training15K_studentVersion.csv")

# Basic EDA before joining to get information about each individual data set
# Structure gives insights on the datatypes and general content of each column
# Just want to have a look at which columns come from which data set a reference
str(consumerData)
str(donationsData)
str(inHouseData)
str(magazineData)
str(politicalData)
# Takeaways: looks like a lot of string type data 
# A lot of missing values
# Boolean columns are Yes/ empty? 

# Looking at na columns
colSums(is.na(consumerData))
colSums(is.na(donationsData))
colSums(is.na(inHouseData))
colSums(is.na(magazineData))
colSums(is.na(politicalData))
#Takeaways: N/a's are not showing accurately because empty string columns are "" instead of n/a

# There doesn't seem to be a problem with one of the individual data sets that needs to be addressed before joining

# Before joining, making sure that there are no duplicates in tmpID
sum(duplicated(consumerData$tmpID))
sum(duplicated(donationsData$tmpID))
sum(duplicated(inHouseData$tmpID))
sum(duplicated(magazineData$tmpID))
sum(duplicated(politicalData$tmpID))

# Use dplyr library left_join to accurately join the data. 
# inHouseData is the most relevant so it's the left Table
alldata <- inHouseData %>%
  left_join(donationsData, by = "tmpID") %>%
  left_join(consumerData, by = "tmpID") %>%
  left_join(magazineData, by = "tmpID") %>%
  left_join(politicalData, by = "tmpID")

# solve the problem identified earlier so we can properly assess nulls
alldata[alldata == ""] <- NA
colSums(is.na(alldata))

##########################################################################################################

# After carefully reading the Data dictionary provided, and taking into consideration the nulls 
# Decision to remove the following columns:

columns_to_drop <- c("lat", "lon", # manufactured
                     "FirstName", "LastName", # no value to identify individuals, keep them anonymous
                     "TelephonesFullPhone", # fictitious and individual to each person, no patterns
                     "fips", "stateFips", # already have state, county, city information
                     "HomePurchasePrice", # more missing values than "EstHomeValue" almost same information
                     "EthnicDescription", # More missing values than "BroadEthnicGroupings", basically same information
                     "LikelyUnionMember","supportsAffordableCareAct","supportsGayMarriage",
                     "supportsGunControl","supportsTaxesRaise","overallsocialviews")
                    # these columns have a lot of missing rows and cannot be directly grouped
                    # They also represent general political viewpoints which are not relevant for the end-goal of this analysis

# use the select function from the dplyr library
# since i have the column names in a vector use helper function one_of
# matches the names in the drop_list with names in alldata and if they match then remove - 
# Result will be alldata_cleaned without the columns specified in columns_to_drop
alldata_1 <- alldata %>% 
  select(-one_of(columns_to_drop))

##########################################################################################################


alldata_1
# Didn't remove some columns due to the ability to group them into one column
# This will reduce column clutter and also help with filling in the missing values
# Loose specificity but better than removing those rows completely

# First grouping can be done with the data coming from donationsData 
# -> all of the columns that indicate donations
# Create new column "is_donator" Yes/No using the following columns (excluding "tmpID"):
column_names <- colnames(donationsData)
print(column_names)

# PoliticalContributerInHome and ReligiosContributorInHome have to be treated differently
# They don't have "Yes" indicating a true value, like all of the other columns
# instead the rows contain "1-9 x Contribution in home" -> find "in home" and declare "yes" 
# ASSUMPTION :: all missing values are seen as a "No" 

column_names <- column_names[-1] # Remove the tmpID
# There are two columns coming from the politicalData dataset so add them
column_names <- c(column_names, "DonatestoConservativeCauses", "DonatestoLiberalCauses")

# Replace na's with "No" in the specified columns using tidyr library for "replace_na"
alldata_1[column_names] <- alldata_1[column_names] %>% 
  replace_na(list(ReligiousContributorInHome = "No",
                  PoliticalContributerInHome = "No",
                  DonatesEnvironmentCauseInHome = "No",
                  DonatesToCharityInHome = "No",
                  DonatestoAnimalWelfare = "No",
                  DonatestoArtsandCulture = "No",
                  DonatestoChildrensCauses = "No",
                  DonatestoHealthcare = "No",
                  DonatestoInternationalAidCauses = "No",
                  DonatestoVeteransCauses = "No",
                  DonatestoHealthcare1 = "No",
                  DonatestoInternationalAidCauses1 = "No",
                  DonatestoWildlifePreservation = "No",
                  DonatestoLocalCommunity = "No",
                  DonatestoConservativeCauses = "No",
                  DonatestoLiberalCauses = "No"))


# This function goes through each row and it returns either 1 or 0 
# grepl looks for "in home" and if that pattern is found it returns 1
is_donatorf <- function(row, column_names) {
  for (col in column_names)
    if (grepl("in home", row[col], ignore.case = TRUE))
      return(1)
  else if (row[col] == "Yes")
    return(1)
  return(0)
}

# now apply the function to the alldata_1 dataframe 
alldata_1$is_donator <- apply(alldata_1[column_names], 1, is_donatorf, column_names = column_names)

# Since the old columns are now useless, i remove them using the same method as before
columns_to_remove <- c("ReligiousContributorInHome", "PoliticalContributerInHome",
                       "DonatesEnvironmentCauseInHome", "DonatesToCharityInHome",
                       "DonatestoAnimalWelfare", "DonatestoArtsandCulture",
                       "DonatestoChildrensCauses", "DonatestoHealthcare",
                       "DonatestoInternationalAidCauses", "DonatestoVeteransCauses",
                       "DonatestoHealthcare1", "DonatestoInternationalAidCauses1",
                       "DonatestoWildlifePreservation", "DonatestoLocalCommunity",
                       "DonatestoLiberalCauses", "DonatestoConservativeCauses")
# apply
alldata_2 <- alldata_1 %>%
  select(-one_of(columns_to_remove))

table(alldata_2$is_donator) # Looks much better! and also no more missing values 

# Now same process for the magazine data 
# create reads_magazine

# columns of interest
column_names_1 <- c("FamilyMagazineInHome", "FemaleOrientedMagazineInHome",
                    "ReligiousMagazineInHome", "GardeningMagazineInHome",
                    "CulinaryInterestMagazineInHome", "HealthFitnessMagazineInHome",
                    "DoItYourselfMagazineInHome", "FinancialMagazineInHome", 
                    "InterestinCurrentAffairsPoliticsInHousehold")

# Replace na's with "No" in the specified columns using tidyr library for "replace_na"
alldata_2[column_names_1] <- alldata_2[column_names_1] %>% 
  replace_na(list(FamilyMagazineInHome = "No",
                  FemaleOrientedMagazineInHome = "No",
                  ReligiousMagazineInHome = "No",
                  GardeningMagazineInHome = "No",
                  CulinaryInterestMagazineInHome = "No",
                  HealthFitnessMagazineInHome = "No",
                  DoItYourselfMagazineInHome = "No",
                  FinancialMagazineInHome = "No", 
                  InterestinCurrentAffairsPoliticsInHousehold = "No"
                  ))

# This function goes through each row and it returns either 1 or 0 
# grepl looks for "magazine" and if that pattern is found it returns 1
magazinef <- function(row, column_names_1) {
  for (col in column_names_1)
    if (!is.na(row[col]) && (grepl("magazine", row[col], ignore.case = TRUE) || row[col] == "Yes"))
      return(1)
  return(0)
}

# now apply the function to the alldata_2 dataframe 
alldata_2$reads_magazine <- apply(alldata_2[column_names_1], 1, magazinef, column_names_1 = column_names_1)

# Since the old columns are now useless, i remove them using the same method as before
columns_to_remove_1 <- c("FamilyMagazineInHome", "FemaleOrientedMagazineInHome",
                         "ReligiousMagazineInHome", "GardeningMagazineInHome",
                         "CulinaryInterestMagazineInHome", "HealthFitnessMagazineInHome",
                         "DoItYourselfMagazineInHome", "FinancialMagazineInHome", 
                         "InterestinCurrentAffairsPoliticsInHousehold")
# apply
alldata_3 <- alldata_2 %>%
  select(-one_of(columns_to_remove_1))

table(alldata_3$reads_magazine) # looks good! 

colSums(is.na(alldata_3))# Look at remaining columns and their nulls

# Lets group the HorseOwner, CatOwner, DogOwner and OtherPetOwner into one column 
# has_pet 
# Less commenting because i will use the same methods for this as above 

column_names_2 <- c("CatOwner", "DogOwner", "OtherPetOwner", "HorseOwner")

alldata_3[column_names_2] <- alldata_3[column_names_2] %>% 
  replace_na(list(CatOwner = "No", 
                  DogOwner = "No", 
                  OtherPetOwner = "No",
                  HorseOwner = "No"))

petf <- function(row, column_names_2) {
  for (col in column_names_2)
    if (row[col] == "Yes")
      return(1)
  return(0)
}

alldata_3$has_pet <- apply(alldata_3[column_names_2], 1, petf, column_names_2 = column_names_2)

columns_to_remove_2 <- c("CatOwner", "DogOwner", "OtherPetOwner", "HorseOwner")

alldata_4 <- alldata_3 %>%
  select(-one_of(columns_to_remove_2))

table(alldata_4$has_pet) # better!

colSums(is.na(alldata_4))

# same for collectors and book interested columns

column_names_3 <- c("BuyerofAntiquesinHousehold", "BuyerofArtinHousehold",
                    "GeneralCollectorinHousehold")
column_names_4 <- c("BooksAudioReadinginHousehold", "BookBuyerInHome")

alldata_4[column_names_3] <- alldata_4[column_names_3] %>% 
  replace_na(list(BuyerofAntiquesinHousehold = "No", 
                  BuyerofArtinHousehold = "No", 
                  GeneralCollectorinHousehold = "No"))

alldata_4[column_names_4] <- alldata_4[column_names_4] %>% 
  replace_na(list(BooksAudioReadinginHousehold = "No", 
                  BookBuyerInHome = "No"))

collectorf <- function(row, column_names_3) {
  for (col in column_names_3)
    if (row[col] == "Yes")
      return(1)
  return(0)
}

booksf <- function(row, column_names_4) {
  for (col in column_names_4)
    if (!is.na(row[col]) && (grepl("in home", row[col], ignore.case = TRUE) || row[col] == "Yes"))
      return(1)
  return(0)
}

alldata_4$has_collector <- apply(alldata_4[column_names_3], 1, collectorf, column_names_3 = column_names_3)
alldata_4$reads_booksoraudiob <- apply(alldata_4[column_names_4], 1, booksf, column_names_4 = column_names_4)

columns_to_remove_3 <- c("BuyerofAntiquesinHousehold", "BuyerofArtinHousehold",
                         "GeneralCollectorinHousehold","BooksAudioReadinginHousehold",
                         "BookBuyerInHome")

alldata_5 <- alldata_4 %>%
  select(-one_of(columns_to_remove_3))

colSums(is.na(alldata_5))

# notes for further working tomorrow 
# fill broadethnic grouping missing values iwth ethnic description that you removed
# upscale yes/no 

# now go one by one and look at the values -> fill in NULLS using logic 

table(alldata_5$Gender) # Male is "M" and Female is "F" 

null_gender_rows <- alldata_5[is.na(alldata_5$Gender), ]
print(null_gender_rows) # There is only one missing value in Gender column 
# Luckily the column ResidenceHHGenderDescription is not null for this row
# Female only Household indicates that the gender should be "F"
# FILL MISSING VALUE 
alldata_5$Gender <- ifelse(is.na(alldata_5$Gender), "F", alldata_5$Gender)
# Change into a numeric column with 1 and 0 where 1 represents Male and 0 represents Female 
alldata_5 <- alldata_5 %>%
  mutate(is_male = ifelse(Gender == "M", 1, 0)) %>%
  select(-Gender)

# Now lets look at Age column 
# First round all numbers to full, no more decimals 
alldata_5$Age <- round(alldata_5$Age)

summary(alldata_5$Age)
# lets visualize the distribution to get a better understanding 
hist(alldata_5$Age, main = "Age Distribution",
     xlab = "Age", col = "orange", border = "black")
# very symmetrical distribution
# Calculate the median age
median_age <- median(alldata_5$Age, na.rm = TRUE)
# Impute missing values in the Age column with the median age
alldata_5$Age <- ifelse(is.na(alldata_5$Age), median_age, alldata_5$Age)

colSums(is.na(alldata_5))
# Drop some more Columns that I deem to be irrelevant for this analysis 
# City and County are too detailed, we are looking for trends so detail is not necessary 
# Can always come back to detail with tmpID 
# DwellingUnitSize has 16% missing values and almost the same information is found in PropertyType
# LandValue does not seem very insightful, already have HomeValue wich seems more interesting
# Also LandValue has a lot of missing values that will be dificult to fill in 
alldata_6 <- alldata_5 %>%
  select(-city, -county, -DwellingUnitSize, -LandValue)

table(alldata_6$state) # look at the content of state column
# Washington, D.C. and Washington indicate the same so change state to Washington for all outliers
# There is a value "NULL" wich are tricky, could impute them correctly going back to city and county 
# then finding the correct state etc but in this case i decided to drop them since its only 18 rows 
alldata_7 <- alldata_6 %>%
  filter(state != "NULL") %>%
  mutate(state = ifelse(state == "Washington, D.C.", "Washington", state))

table(alldata_7$PropertyType)# a lot of Unknowns, could minimize using DwellingUnitSize
# but decision to ignore for now 


# EstHomeValue has 130 nulls, since HomePurchasePrice gives us similar data, I decided to 
# demonstrate how to fill in nulls using a "twin" 
# first loop over all the rows in alldata_7 to find nulls 
# if it is then look up the tmp_id and store it 
# now use that tmpID to go to the original dataset "alldata" and lookup those tmpID's 
# store the value of those rows into home_purchase_price 
# if that value is NOT na then assign the value from home_purchasing_price 
# to the corresponding row in alldata_7 
# Done !
for (i in 1:nrow(alldata_7)) {
  if (is.na(alldata_7$EstHomeValue[i])) {
    tmp_id <- alldata_7$tmpID[i]
    home_purchase_price <- alldata[alldata$tmpID == tmp_id, "HomePurchasePrice"]
    
    if (!is.na(home_purchase_price)) {
      alldata_7$EstHomeValue[i] <- home_purchase_price
    }
  }
}
# Check if nulls are reduced and remove the rest of the rows that couldnt be filled
colSums(is.na(alldata_7)) # only 4 rows were filled in ... that was a waste of time
# clean the values by removing the "$" and replacing it with empty string 
# allows for creation of histogram
alldata_7$EstHomeValue <- as.numeric(gsub("\\$", "", alldata_7$EstHomeValue))
#create histogram using ggplot2
ggplot(alldata_7, aes(x = EstHomeValue)) +
  geom_histogram(binwidth = 50000, color = "black", fill = "blue") +
  theme_minimal() +
  labs(title = "Distribution of Estimated Home Value",
       x = "Estimated Home Value",
       y = "Frequency")
summary(alldata_7$EstHomeValue)
# looking at the data it seems to be positively skewed with some outliers
# decision to impute the missing values with the median 
# median is less sensitive to outliers and keeps the central tendency of the graph
alldata_7$EstHomeValue[is.na(alldata_7$EstHomeValue)] <- median(alldata_7$EstHomeValue, na.rm = TRUE)

# Round the y_HouseholdSpend 
alldata_7$y_householdSpend <- round(alldata_7$y_householdSpend, 2)

# ResidenceHHGenderDescription has 2 "Cannot Determine" -> Drop those rows 
table(alldata_7$ResidenceHHGenderDescription) # get insigth
# use subset to drop
alldata_8 <- subset(alldata_7, ResidenceHHGenderDescription != "Cannot Determine")

# BroadEthnicGrouping is next, has a lot of missing values
# attempt to use twin: EthnicDescription; I use the same code as earlier so no explanation
for (i in 1:nrow(alldata_8)) {
  if (is.na(alldata_8$BroadEthnicGroupings[i])) {
    tmp_id <- alldata_8$tmpID[i]
    ethnicity <- alldata[alldata$tmpID == tmp_id, "EthnicDescription"]
    
    if (!is.na(ethnicity)) {
      alldata_8$BroadEthnicGroupings[i] <- ethnicity
    }
  }
}
# after this only 71 missing values are left!
colSums(is.na(alldata_8))
# Drop the remaining rows using subset
alldata_9 <- subset(alldata_8, !is.na(BroadEthnicGroupings))
# look at the contents. Looks good, move on 
table(alldata_9$BroadEthnicGroupings)


table(alldata_9$PresenceOfChildrenCode)
# PresenceOfChildrenCode can be simplified into 1 and 0 so thats what i will do
# Create has_child column based on PresenceofChildrenCode column
# if it contains "Not" it will be 0
# every other string will be 1 
# The nulls are kept nulls for now 
alldata_9 <- alldata_9 %>% 
  mutate(has_child = ifelse(grepl("Not", PresenceOfChildrenCode, ignore.case = TRUE), 0, 
                            ifelse(is.na(PresenceOfChildrenCode), NA, 1)))

# drop the old PresenceofChildrenCode column
alldata_10 <- alldata_9 %>% select(-PresenceOfChildrenCode)

# Now lets take a look at the distribution in the has_child column
table(alldata_10$has_child, useNA = "ifany")

# calculate the proportion of 1 and 0 
prop_table <- prop.table(table(alldata_10$has_child, useNA = "no"))

# Replace NA values with a random choice based on the distribution
# this is not ideal but it keeps the proportion of this column 
# could introduce bias since its 1k columns 
# still better than removing the rows in my opinion
alldata_10$has_child <- ifelse(is.na(alldata_10$has_child),
                               sample(c(0, 1), size = sum(is.na(alldata_10$has_child)), 
                                      replace = TRUE, prob = c(prop_table[1], prop_table[2])),
                               alldata_10$has_child)


# I will use the same method for HomeOwnerRenter as for has_child so less commenting here
# Also will fill in the missing values, keeping the distribution
# Create new is_homeowner column
alldata_10 <- alldata_10 %>%
  mutate(is_homeowner = ifelse(grepl("Homeowner", HomeOwnerRenter, ignore.case = TRUE), 1,
                               ifelse(grepl("Renter", HomeOwnerRenter, ignore.case = TRUE), 0, NA)))

table(alldata_10$is_homeowner, useNA = "ifany")
# proportion table
prop_table_homeowner <- prop.table(table(alldata_10$is_homeowner), 1)
# Impute missing values 
alldata_10$is_homeowner <- ifelse(is.na(alldata_10$is_homeowner),
                                  sample(c(0, 1), size = sum(is.na(alldata_10$is_homeowner)), 
                                         replace = TRUE, prob = c(prop_table_homeowner[1], prop_table_homeowner[2])),
                                  alldata_10$is_homeowner)
# Check the distribution 
table(alldata_10$is_homeowner)

# Drop HomeOwnerRenter (old column)
# Decision to drop three more columns 
alldata_11 <- alldata_10 %>% select(-HomeOwnerRenter, -ISPSA, -UpscaleBuyerInHome, -ComputerOwnerInHome, -ResidenceHHGenderDescription)
# ISPSA is very interesting but hard to interpret also we have the information unpivoted already
# UpscaleBuyer and ComputerOwner can be looked at but there are already a lot of columns 
# ResidenceHHGender because it is inconsistent with the Gender column from inhousedata
# If building a model, i wouldn't remove them but here they provide very little value for insights 

# A lot of nulls in the MosaicZ4 column 
# for now i dont know what to do with it so impute "Unkown" 
# it might be interesting for personas later so i keep it and didnt delete it 
alldata_12 <- alldata_11 %>%
  mutate(MosaicZ4 = ifelse(is.na(MosaicZ4), "Unknown", MosaicZ4))


# lets look at the summary for the MedianEducationYears column
summary(alldata_12$MedianEducationYears)
#Mean and median seem very similar so probably symmetrical distr similar to age 
# Visualize the distribution of MedianEducationYears to make sure
ggplot(alldata_12, aes(x = MedianEducationYears)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribution of Median Education Years",
       x = "Median Education Years",
       y = "Count")
# Warning is just related to the nulls that are still there
# Will impute them with median here 
# Calculate median
median_education_years <- median(alldata_12$MedianEducationYears, na.rm = TRUE)
# Impute missing values 
alldata_12$MedianEducationYears[is.na(alldata_12$MedianEducationYears)] <- median_education_years

# NetWorth has a bunch of missing values
# cant really impute them with anything right now 
# label them as Unkown 
alldata_12 <- alldata_12 %>%
  mutate(NetWorth = ifelse(is.na(NetWorth), "Unknown", NetWorth))

# The columns "Investor", "BusinessOwner" and "HomeOffice" have the same problem
# They only contain "Yes" or null 
# I make the assumption that null is "No" 
# So impute all of them with 1 and 0 accordingly 
# use across to perform mutate for several columns 
alldata_12 <- alldata_12 %>%
  mutate(across(c(Investor, BusinessOwner, HomeOffice),
                ~replace_na(ifelse(grepl("Yes", ., ignore.case = TRUE), 1, 0), 0),
                .names = "{col}"))

table(alldata_12$HomeOffice)
table(alldata_12$Investor)
table(alldata_12$BusinessOwner)
# Looks good !


table(alldata_12$PartiesDescription)
# looks like we have 4 "Green" this doesnt make a lot of sense when analyzing America
# Decision to drop those 4 columns 
alldata_13 <- alldata_12 %>%
  filter(!grepl("Green", PartiesDescription, ignore.case = TRUE))

# ReligionsDescription has a bunch of Nulls, can't fill them so declare as "Unknown"
alldata_14 <- alldata_13 %>%
  mutate(ReligionsDescription = ifelse(is.na(ReligionsDescription), "Unknown", ReligionsDescription))

# The education column can be simplified 
table(alldata_14$Education)

# use mutate to overwrite the values according to the specified cases 
alldata_15 <- alldata_14 %>%
  mutate(Education = case_when(
    grepl("Bach Degree", Education, ignore.case = TRUE) ~ "Bachelors Degree",
    grepl("Grad Degree", Education, ignore.case = TRUE) ~ "Graduate Degree",
    grepl("Less than", Education, ignore.case = TRUE) ~ "Less than High School",
    grepl("HS Diploma", Education, ignore.case = TRUE) ~ "High School Diploma",
    grepl("College", Education, ignore.case = TRUE) ~ "College",
    grepl("unknown", Education, ignore.case = TRUE) ~ "Unknown",
    grepl("Vocational", Education, ignore.case = TRUE) ~ "Vocational Degree",
    TRUE ~ Education
  ))

# All the null values have been dealt with 
colSums(is.na(alldata_15)) 
data <- alldata_15
# Its time to get some insights!


#########################################################################################################


str(data)# looking at datatypes 

# I want to convert all of the categorical columns that are still numerical after preprocessing
columns_to_convert <- c("Investor", "HomeOffice", "reads_magazine", "has_collector", "is_male", "is_homeowner",
                        "BusinessOwner", "is_donator", "has_pet", "reads_booksoraudiob", "has_child")

# Loop through the columns and convert them to categoric (Factors)
for (col in columns_to_convert) {
  data[[col]] <- factor(data[[col]], levels = c(0, 1))
}

# Select all numeric columns only
numeric_columns <- names(data)[sapply(data, is.numeric)]

# Select all character columns
char_cols <- names(data)[sapply(data, is.character)]

# Select all categorical columns  
categorical_columns <- data %>% select_if(is.factor) %>% names()

# Now I want to loop through all of the different columns and look at their distribution

# Creating a list to store the plots
plot_list <- list()

# Looping through all of the numeric columns and creating a histogram for each of them
for (col_name in numeric_columns) {
  col_sym <- sym(col_name)
  plot <- ggplot(data = data, aes(x = !!col_sym)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Distribution of", col_name),
         x = col_name,
         y = "Frequency")
  
  plot_list[[col_name]] <- plot
}

# Combine all plots into a grid
grid.arrange(grobs = plot_list, ncol = 2)
# Now export the output to a pdf so i can look at it properly (manually using RStudio)


# Same for Character columns 
plot_list_1 <- list()

for (i in seq_along(char_cols)) {
  col <- char_cols[i]
  
  p <- ggplot(data, aes_string(col)) +
    geom_bar() +
    theme_minimal() +
    labs(title = col, x = "", y = "Count")
  
  plot_list_1[[i]] <- p
}

grid.arrange(grobs = plot_list_1, ncol = 2) 

# Same for the categorical columns
plot_list_2 <- list()

for (i in seq_along(categorical_columns)) {
  col <- categorical_columns[i]
  
  dist_table <- data %>% 
    group_by_at(col) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = (Count / sum(Count)) * 100)
  
  p <- ggplot(dist_table, aes_string(x = col, y = "Percentage")) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Distribution of", col), x = col, y = "Percentage")
  
  plot_list_2[[i]] <- p
}

grid.arrange(grobs = plot_list_2, ncol = 2)



# After looking at the distribution of all of the columns 
# I decided to look at the exact % distribution for categoric and character columns 
# I excluded numeric because first of all i would need to bin them 
# And secondly their exact distribution % even for bins is not really interesting for me at this point

# print the % distribution for each character column 
# Loop through the character columns
for (col in char_cols) {
  cat("Column:", col, "\n")
  
  # Calculate the percentage distribution of each value in the column
  percent_distribution <- round(prop.table(table(data[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}
#Same for categorical columns 
for (col in categorical_columns) {
  cat("Column:", col, "\n")
  percent_distribution <- round(prop.table(table(data[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}
###########################################################################################################

# After looking at the data in detail I was able to narrow down a typical buyer persona of someone
# already in the loyalty program
# Significance is determined with 60% majority for categoric columns 

# Age: Very evenly distributed no clear trend
# State: Top 5 states are California, Illinois, New York, Pennsylvania and Texas
# Store visit frequency is quite balanced no clear trend
# PropertyType: Typical member lives in a Residential home 
# EstHomeValue: The typical member lives in a home estimated to be worth between 150-200 thousand $ 
# HouseholdSpend: Typically between 300-400 $ 
# BroadEthnicGroupings: Typically English/Welsh but also German and Irish 
# MosaicZ4: No place like home, Rooted Flower Power
# MedianEducationYears & Education: Most likely a Graduate or Bachelors degree with 15 years worth of education
# NetWorth: Typically 100-250 thousand $ and upwards 
# Investor: Not significant
# BusinessOwner: NOT a business owner
# OccupationIndustry: Typically work in management or medical industry
# HomeOffice: NOT in homeoffice
# PartiesDescription: Not significant 
# ReligionsDescription: Most likely a Protestant
# GunOwner: Unknown/ probably NOT typically a gun Owner
# Veteran: Unknown/ probably NOT typically a veteran 
# is_donator: Typically donates to some cause 
# reads_magazine: Not significant
# has_pet: Typically NOT a pet owner
# has_collector: Typically does NOT collect something
# reads_booksoraudiob: Typically does NOT read books or listens to audiobooks
# is_male: no trend, not significant
# has_child: Typically does NOT have a child 
# is_homeowner: Typically is a HOMEOWNER

#######################################################################################################

# For the second persona i decided to look at the top spenders for y_householdspend 
# First step is to create bins for this numeric column 
# Second step filter all the columns according to the top bin in householdspend 
# Third, look at trends in the persona, and understand what persona a typical top spender has 
# This will be interesting for the business since these customers are very valuable 

# Create bins for y_householdSpend
data$y_householdSpend_bins <- cut(data$y_householdSpend, breaks = 8, include.lowest = TRUE, labels = FALSE)

# Visualize the distribution of y_householdSpend_bins
ggplot(data, aes(x = y_householdSpend_bins)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of y_householdSpend",
       x = "y_householdSpend Bins",
       y = "Frequency")

# Filter rows where y_householdSpend_bins is 7/8 (Top spenders) (136) rows --> top 1% spenders
filtered_data <- data %>%
  filter(y_householdSpend_bins %in% c(7, 8))   

# now i repeat the same as before to look at the different distributions for all the columns 

plot_list <- list()

for (col_name in numeric_columns) {
  col_sym <- sym(col_name)
  plot <- ggplot(data = filtered_data, aes(x = !!col_sym)) + # the only change is that the data is now filtered_data
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Distribution of", col_name),
         x = col_name,
         y = "Frequency")
  
  plot_list[[col_name]] <- plot
}

grid.arrange(grobs = plot_list, ncol = 2)


# Character columns 
plot_list_1 <- list()

for (i in seq_along(char_cols)) {
  col <- char_cols[i]
  
  p <- ggplot(data= filtered_data, aes_string(col)) +
    geom_bar() +
    theme_minimal() +
    labs(title = col, x = "", y = "Count")
  
  plot_list_1[[i]] <- p
}

grid.arrange(grobs = plot_list_1, ncol = 2) 



# Categorical columns
plot_list_2 <- list()

for (i in seq_along(categorical_columns)) {
  col <- categorical_columns[i]
  
  dist_table <- filtered_data %>% 
    group_by_at(col) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = (Count / sum(Count)) * 100)
  
  p <- ggplot(dist_table, aes_string(x = col, y = "Percentage")) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Distribution of", col), x = col, y = "Percentage")
  
  plot_list_2[[i]] <- p
}

grid.arrange(grobs = plot_list_2, ncol = 2)

# Exact % distributions 
# Character columns 
for (col in char_cols) {
  cat("Column:", col, "\n")
  
  percent_distribution <- round(prop.table(table(filtered_data[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}

# Categorical columns 
for (col in categorical_columns) {
  cat("Column:", col, "\n")
  percent_distribution <- round(prop.table(table(filtered_data[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}
########################################################################################################

# After looking at the data in detail I was able to narrow down a typical buyer persona of someone
# that is in the top 1% of spenders 
# Significance is determined with 60% majority for categoric columns 

# Age: Very evenly distributed no clear trend
# State: Texas overwhelmingly, then California, Maryland and New Mexico 
# Store visit frequency: More likely 6 or more times 
# PropertyType: Typical 1% spender lives in a Residential home 
# EstHomeValue: The typical 1% member lives in a home estimated to be worth between 150-200 thousand $ 
# HouseholdSpend: Typically around 600 $ 
# BroadEthnicGroupings: Typically English/Welsh but also German and AFRO Americans
# MosaicZ4: No place like Home, Silver Sophisticates
# MedianEducationYears & Education: Most likely a Graduate or Bachelors degree with 15 years worth of education
# NetWorth: Typically 50-100 thousand $ and upwards 
# Investor: Not significant
# BusinessOwner: NOT a business owner
# OccupationIndustry: Typically works in  medical industry or Skilled Trades 
# HomeOffice: NOT in homeoffice
# PartiesDescription: More likely to be democratic 
# ReligionsDescription: Most likely a Protestant
# GunOwner: Unknown/ probably NOT typically a gun Owner
# Veteran: Unknown/ probably NOT typically a veteran 
# is_donator: Typically donates to some cause 
# reads_magazine: Not significant
# has_pet: Typically NOT a pet owner
# has_collector: Typically does NOT collect something
# reads_booksoraudiob: Typically does NOT read books or listens to audiobooks
# is_male: no trend, not significant
# has_child: Typically does NOT have a child 
# is_homeowner: Typically is a HOMEOWNER

########################################################################################################
# Now i decided to look at the personas for the top visitors 
# This might be a good indicator since they are very loyal members

#First step:
# Visualize the distribution of storeVisitFrequency
ggplot(data, aes(x = storeVisitFrequency)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of storeVisitFrequency",
       x = "storeVisitFrequency",
       y = "Frequency")
# Decision to look at only the top that visit the store 8 times 
# Filter rows where storeVisitFrequency_bins is 8 (Top frequent visitors) 2766 rows 
filtered_data_1 <- data %>%
  filter(storeVisitFrequency %in% c(8))


# now i repeat the same as before to look at the different distributions for all the columns 

plot_list <- list()

for (col_name in numeric_columns) {
  col_sym <- sym(col_name)
  plot <- ggplot(data = filtered_data_1, aes(x = !!col_sym)) + # the only change is that the data is now filtered_data
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Distribution of", col_name),
         x = col_name,
         y = "Frequency")
  
  plot_list[[col_name]] <- plot
}

grid.arrange(grobs = plot_list, ncol = 2)


# Character columns 
plot_list_1 <- list()

for (i in seq_along(char_cols)) {
  col <- char_cols[i]
  
  p <- ggplot(data= filtered_data_1, aes_string(col)) +
    geom_bar() +
    theme_minimal() +
    labs(title = col, x = "", y = "Count")
  
  plot_list_1[[i]] <- p
}

grid.arrange(grobs = plot_list_1, ncol = 2) 



# Categorical columns
plot_list_2 <- list()

for (i in seq_along(categorical_columns)) {
  col <- categorical_columns[i]
  
  dist_table <- filtered_data_1 %>% 
    group_by_at(col) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = (Count / sum(Count)) * 100)
  
  p <- ggplot(dist_table, aes_string(x = col, y = "Percentage")) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Distribution of", col), x = col, y = "Percentage")
  
  plot_list_2[[i]] <- p
}

grid.arrange(grobs = plot_list_2, ncol = 2)

# Exact % distributions 
# Character columns 
for (col in char_cols) {
  cat("Column:", col, "\n")
  
  percent_distribution <- round(prop.table(table(filtered_data_1[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}

# Categorical columns 
for (col in categorical_columns) {
  cat("Column:", col, "\n")
  percent_distribution <- round(prop.table(table(filtered_data_1[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}

########################################################################################################

# After looking at the data in detail I was able to narrow down a typical buyer persona of someone
# that vitist the store more than the other members
# Significance is determined with 60% majority for categoric columns 

# Age: Very evenly distributed no clear trend
# State: Texas overwhelmingly, then California, Maryland and New Mexico 
# Store visit frequency: More likely 6 or more times 
# PropertyType: Typical 1% spender lives in a Residential home 
# EstHomeValue: The typical 1% member lives in a home estimated to be worth between 150-200 thousand $ 
# HouseholdSpend: Typically around 600 $ 
# BroadEthnicGroupings: Typically English/Welsh but also German and AFRO Americans
# MosaicZ4: No place like Home, Silver Sophisticates
# MedianEducationYears & Education: Most likely a Graduate or Bachelors degree with 15 years worth of education
# NetWorth: Typically 50-100 thousand $ and upwards 
# Investor: Not significant
# BusinessOwner: NOT a business owner
# OccupationIndustry: Typically works in  medical industry or Skilled Trades 
# HomeOffice: NOT in homeoffice
# PartiesDescription: More likely to be democratic 
# ReligionsDescription: Most likely a Protestant
# GunOwner: Unknown/ probably NOT typically a gun Owner
# Veteran: Unknown/ probably NOT typically a veteran 
# is_donator: Typically donates to some cause 
# reads_magazine: Not significant
# has_pet: Typically NOT a pet owner
# has_collector: Typically does NOT collect something
# reads_booksoraudiob: Typically does NOT read books or listens to audiobooks
# is_male: no trend, not significant
# has_child: Typically does NOT have a child 
# is_homeowner: Typically is a HOMEOWNER

########################################################################################################
# Combine the two filters for further insights

# Filter rows where storeVisitFrequency is 8 and y_householdSpend_bins is 7 or 8 (Top frequent visitors and top spenders)
filtered_data_2 <- data %>%
  filter(storeVisitFrequency == 8, y_householdSpend_bins %in% c(7, 8))

plot_list <- list()

for (col_name in numeric_columns) {
  col_sym <- sym(col_name)
  plot <- ggplot(data = filtered_data_2, aes(x = !!col_sym)) + 
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Distribution of", col_name),
         x = col_name,
         y = "Frequency")
  
  plot_list[[col_name]] <- plot
}

grid.arrange(grobs = plot_list, ncol = 2)


# Character columns 
plot_list_1 <- list()

for (i in seq_along(char_cols)) {
  col <- char_cols[i]
  
  p <- ggplot(data= filtered_data_2, aes_string(col)) +
    geom_bar() +
    theme_minimal() +
    labs(title = col, x = "", y = "Count")
  
  plot_list_1[[i]] <- p
}

grid.arrange(grobs = plot_list_2, ncol = 2) 



# Categorical columns
plot_list_2 <- list()

for (i in seq_along(categorical_columns)) {
  col <- categorical_columns[i]
  
  dist_table <- filtered_data_2 %>% 
    group_by_at(col) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = (Count / sum(Count)) * 100)
  
  p <- ggplot(dist_table, aes_string(x = col, y = "Percentage")) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Distribution of", col), x = col, y = "Percentage")
  
  plot_list_2[[i]] <- p
}

grid.arrange(grobs = plot_list_2, ncol = 2)

# Exact % distributions 
# Character columns 
for (col in char_cols) {
  cat("Column:", col, "\n")
  
  percent_distribution <- round(prop.table(table(filtered_data_2[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}

# Categorical columns 
for (col in categorical_columns) {
  cat("Column:", col, "\n")
  percent_distribution <- round(prop.table(table(filtered_data_2[[col]])) * 100, 2)
  print(percent_distribution)
  cat("\n")
}

######################################################################################################
# Here no detailed insights- too time consuming but used for reference


#######################################################################################################
# I now want to look at the change of the distr for the categorical columns 
# This changes for each of the filters
# I include firstly the data df that contains all customers
# Secondly the filtered_data containing the top spenders
# filtered_data_1 containing the customers with 8 visits 
# filtered_data_2 containing both of those specific filters meaning top 1% AND 8 store visits

# Calculate the percentage distribution for the categorical columns
get_percentage_distribution <- function(data, col) {
  perc_dist <- data %>%
    group_by_at(col) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / sum(Count)) * 100)
  return(perc_dist)
}

# Change the name to get different graphs !!!!!!!!!!!!!!!!!! I iterated manualy over the categoric columns
categories <- c("has_collector") 

percentage_distribution <- list() # store in list 

# loop over the 4 filters
for (col in categories) {
  alldata_perc <- get_percentage_distribution(data, col)
  filtered_data_perc <- get_percentage_distribution(filtered_data, col)
  filtered_data_1_perc <- get_percentage_distribution(filtered_data_1, col)
  filtered_data_2_perc <- get_percentage_distribution(filtered_data_2, col)
  combined <- bind_rows( # combine them
    mutate(alldata_perc, Dataset = "data"),
    mutate(filtered_data_perc, Dataset = "filtered_data"),
    mutate(filtered_data_1_perc, Dataset = "filtered_data_1"),
    mutate(filtered_data_2_perc, Dataset = "filtered_data_2")
  )
  
  percentage_distribution[[col]] <- combined # assign 
}

# Create a new dataframe to store the calculated percentage distributions
perc_dist_df <- bind_rows(percentage_distribution, .id = "column_name")

# Create separate dataframes for category values 0 and 1
perc_dist_df_0 <- perc_dist_df %>%
  filter(!!sym(col) == 0)

perc_dist_df_1 <- perc_dist_df %>%
  filter(!!sym(col) == 1)

# Generate a line chart with two lines for each categorical column
# credits to GPT 
ggplot() +
  geom_line(data = perc_dist_df_0, aes(x = Dataset, y = Percentage, group = column_name), color = "red", size = 1) +
  geom_point(data = perc_dist_df_0, aes(x = Dataset, y = Percentage), color = "red", size = 2) +
  geom_text(data = perc_dist_df_0, aes(x = Dataset, y = Percentage, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
  geom_line(data = perc_dist_df_1, aes(x = Dataset, y = Percentage, group = column_name), color = "blue", size = 1) +
  geom_point(data = perc_dist_df_1, aes(x = Dataset, y = Percentage), color = "blue", size = 2) +
  geom_text(data = perc_dist_df_1, aes(x = Dataset, y = Percentage, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
  facet_wrap(~ column_name, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Percentage Probability Change for Categorical Columns",
       x = "Dataset",
       y = "Percentage Distribution") +
  ylim(0, 100) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(fill = c("red", "blue"))), linetype = "none") +
  scale_color_manual(values = c("red", "blue"), labels = c("0", "1"), name = "Category")

#########################################################################################################
# Data viz for the Presentation
# Distribution of the age column using bins 

ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Frequency")


# HOUSEHOLD SPEND 

# Calculate the median and IQR
median_y_householdSpend <- median(data$y_householdSpend)
IQR_y_householdSpend <- IQR(data$y_householdSpend)

# Calculate the IQR bounds (Q1 and Q3)
Q1 <- quantile(data$y_householdSpend, 0.25)
Q3 <- quantile(data$y_householdSpend, 0.75)

# Create a histogram of y_householdSpend
hist_plot <- ggplot(data, aes(x = y_householdSpend)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of y_householdSpend",
       x = "y_householdSpend",
       y = "Frequency")

# Calculate the median and IQR
y_householdSpend_median <- median(data$y_householdSpend)
y_householdSpend_IQR <- IQR(data$y_householdSpend)
lower_bound <- y_householdSpend_median - 0.5 * y_householdSpend_IQR
upper_bound <- y_householdSpend_median + 0.5 * y_householdSpend_IQR

# Create a histogram of y_householdSpend with legend and connecting dots
hist_plot <- ggplot(data, aes(x = y_householdSpend)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of y_householdSpend",
       x = "y_householdSpend",
       y = "Frequency") +
  geom_vline(aes(xintercept = y_householdSpend_median, color = "Median"), linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = lower_bound, color = "IQR Range"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = upper_bound, color = "IQR Range"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Median" = "red", "IQR Range" = "green"),
                     labels = c("Median", "IQR Range")) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank())

hist_plot # cred to GPT


# STATES

# Filter states with more than 100 mentions
filtered_states <- data %>%
  count(state) %>%
  filter(n > 100)

# Determine the top 4 states
top_4_states <- filtered_states %>%
  top_n(4, wt = n) %>%
  pull(state)

# Add the is_top_4 variable
filtered_states <- filtered_states %>%
  mutate(is_top_4 = ifelse(state %in% top_4_states, "Top 4", "Other"))

# Horizontal bar plot with the top 4 states highlighted in green
ggplot(filtered_states, aes(x = state, y = n, fill = is_top_4)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution of States (More than 100 mentions)",
       x = "State",
       y = "Frequency") +
  scale_fill_manual(values = c("Top 4" = "darkgreen", "Other" = "steelblue"),
                    guide = guide_legend(title = NULL, reverse = TRUE)) +
  theme(legend.position = "top")


# Store visit frequency 

# Histogram of store visit frequency
ggplot(data, aes(x = storeVisitFrequency)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Store Visit Frequency",
       x = "Store Visit Frequency",
       y = "Frequency")

