library(tidyverse)
library(xgboost)
library(lubridate)

#Read data
#Data <- read_csv("2024_05_28 atlas_antibiotics.csv")

#Select Africa countries from the merged data
#Africa
merged_dataA <- Data %>%
  filter(Country %in% c("Cameroon", "Egypt", "Ghana", "Ivory Coast", "Kenya",
                        "Malawi", "Mauritius", "Morocco", "Namibia", "Nigeria",
                        "South Africa", "Tunisia", "Uganda"))

#Asia
merged_dataASIA <- Data %>%
  filter(Country %in% c("Australia", "China", "Thailand", "Hong Kong", "India", "Indonesia", "Japan", "Korea South",
                        "Malaysia", "New Zealand", "Philippines", "Singapore", "Taiwan", "Vietnam"
  ))


# North America
merged_dataNAm <- Data %>%
  filter(Country %in% c("Canada", "United States"
  ))

#Europe
merged_dataEuro <- Data %>%
  filter(Country %in% c("Austria",
                        "Belgium",
                        "Bulgaria",
                        "Croatia",
                        "Czech Republic",
                        "Denmark",
                        "Estonia",
                        "Finland",
                        "France",
                        "Germany",
                        "Greece",
                        "Hungary",
                        "Ireland",
                        "Italy",
                        "Latvia",
                        "Lithuania",
                        "Netherland",
                        "Norway",
                        "Poland",
                        "Portugal",
                        "Romania",
                        "Russia",
                        "Serbia",
                        "Slovak",
                        "Slovenia",
                        "Spain",
                        "Sweden",
                        "Switzerland",
                        "Turkey",
                        "Ukraine",
                        "United Kingdom"
  ))

#Latin America
merged_dataLAme<- Data %>%
  filter(Country %in% c(
    "Argentina",
    "Brazil",
    "Chile",
    "Colombia",
    "Costa Rica",
    "Dominican Republic",
    "El Salvador",
    "Guatemala",
    "Honduras",
    "Jamaica",
    "Mexico",
    "Nicaragua",
    "Panama",
    "Puerto Rico",
    "Venezuela"
  ))


#Middle East
merged_dataMEast<- Data %>%
  filter(Country %in% c(
    "Oman",
    "Israel",
    "Jordan",
    "Kuwait",
    "Lebanon",
    "Pakistan",
    "Qatar",
    "Saudi Arabia"
  ))

#All ATLAS Data
Actino_B_AData <- subset(Data, Species == "Acinetobacter baumannii")

#Select Carbapenem
Actino_B_CarbData <- Actino_B_AData %>%
              select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Imipenem_I, Meropenem_I, Doripenem_I)
#Save data
write_csv(Actino_B_CarbData, "Actino_B_CarbData.csv")

#Africa 
Actino_B_AF <- subset(merged_dataA, Species == "Acinetobacter baumannii")

#Select Carbapenem
Actino_B_CarbAF <- Actino_B_AF %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Imipenem_I, Meropenem_I, Doripenem_I)

#Save data
write_csv(Actino_B_CarbAF, "Actino_B_CarbAF.csv")

#Asia
Actino_B_AS <- subset(merged_dataASIA, Species == "Acinetobacter baumannii")

#Select Carbapenem
Actino_B_CarbAS <- Actino_B_AS %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Imipenem_I, Meropenem_I, Doripenem_I)

#Save data
write_csv(Actino_B_CarbAS, "Actino_B_CarbAS.csv")

#North America
Actino_B_NA <- subset(merged_dataNAm, Species == "Acinetobacter baumannii")

#Select Carbapenem
Actino_B_CarbNA <- Actino_B_NA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Imipenem_I, Meropenem_I, Doripenem_I)

#Save data
write_csv(Actino_B_CarbNA, "Actino_B_CarbNA.csv")

#Europe
Actino_B_EU <- subset(merged_dataEuro, Species == "Acinetobacter baumannii")

#Select Carbapenem
Actino_B_CarbEU <- Actino_B_EU %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Imipenem_I, Meropenem_I, Doripenem_I)

#Save data
write_csv(Actino_B_CarbEU, "Actino_B_CarbEU.csv")

#Latin America

Actino_B_LA <- subset(merged_dataLAme, Species == "Acinetobacter baumannii")

#Select Carbapenem
Actino_B_CarbLA <- Actino_B_LA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Imipenem_I, Meropenem_I, Doripenem_I)

#Save data
write_csv(Actino_B_CarbLA, "Actino_B_CarbLA.csv")

#Middle East
Actino_B_ME <- subset(merged_dataMEast, Species == "Acinetobacter baumannii")

#Select Carbapenem
Actino_B_CarbME <- Actino_B_ME %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Imipenem_I, Meropenem_I, Doripenem_I)

#Save data
write_csv(Actino_B_CarbME, "Actino_B_CarbME.csv")

#Enterococcus faecium
#All ATLAS Data
Entero_B_AData <- subset(Data, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_CarbData <- Entero_B_AData %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)
#Save data
write_csv(Entero_B_CarbData, "Entero_B_CarbData.csv")


# Percent of Resistant after filtering
# Create a data frame with the provided data
# Function to calculate the percentage of "Resistant" cases for each year
calculate_resistant_percentage <- function(df) {
  # Filter the data for "Resistant" entries
  resistant_data <- df[df == "Resistant"]
  
  # Calculate the percentage for each year
  df %>%
    group_by(Year) %>%
    summarise(
      Erythromycin_Resistant = sum(Erythromycin_I == "Resistant", na.rm = TRUE),
      Ampicillin_Resistant = sum(Ampicillin_I == "Resistant", na.rm = TRUE),
      Levofloxacin_Resistant = sum(Levofloxacin_I == "Resistant", na.rm = TRUE),
      Total_Entries = n()
    ) %>%
    mutate(
      Erythromycin_Percentage = (Erythromycin_Resistant / Total_Entries) * 100,
      Ampicillin_Percentage = (Ampicillin_Resistant / Total_Entries) * 100,
      Levofloxacin_Percentage = (Levofloxacin_Resistant / Total_Entries) * 100
    ) %>%
    select(Year, Erythromycin_Percentage, Ampicillin_Percentage, Levofloxacin_Percentage)
}

# Calculate the resistant percentage by year
resistant_percentages <- calculate_resistant_percentage(Entero_B_CarbData)

# Print the result
print("Resistant Percentage by Year:")
print(resistant_percentages)


#Acinetobacter baumannii (Final)
#All ATLAS Data
Acin_B_AData <- subset(Data, Species == "Acinetobacter baumannii")

#Select Carbapenem
Acin_B_CarbData <- Acin_B_AData %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Cefepime_I, Ceftazidime_I, Levofloxacin_I)
#Save data
write_csv(Acin_B_CarbData, "Acin_B_CarbData.csv")

#Africa 
Acin_B_AF <- subset(merged_dataA, Species == "Acinetobacter baumannii")

#Select Carbapenem
Acin_B_CarbAF <- Acin_B_AF %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Cefepime_I, Ceftazidime_I, Levofloxacin_I)

#Save data
write_csv(Acin_B_CarbAF, "Acin_B_CarbAF.csv")

#Asia
Acin_B_AS <- subset(merged_dataASIA, Species == "Acinetobacter baumannii")

#Select Carbapenem
Acin_B_CarbAS <- Acin_B_AS %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Cefepime_I, Ceftazidime_I, Levofloxacin_I)

#Save data
write_csv(Acin_B_CarbAS, "Acin_B_CarbAS.csv")

#North America
Acin_B_NA <- subset(merged_dataNAm, Species == "Acinetobacter baumannii")

#Select Carbapenem
Acin_B_CarbNA <- Acin_B_NA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Cefepime_I, Ceftazidime_I, Levofloxacin_I)

#Save data
write_csv(Acin_B_CarbNA, "Acin_B_CarbNA.csv")

#Europe
Acin_B_EU <- subset(merged_dataEuro, Species == "Acinetobacter baumannii")

#Select Carbapenem
Acin_B_CarbEU <- Acin_B_EU %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Cefepime_I, Ceftazidime_I, Levofloxacin_I)

#Save data
write_csv(Acin_B_CarbEU, "Acin_B_CarbEU.csv")

#Latin America

Acin_B_LA <- subset(merged_dataLAme, Species == "Acinetobacter baumannii")

#Select Carbapenem
Acin_B_CarbLA <- Acin_B_LA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Cefepime_I, Ceftazidime_I, Levofloxacin_I)

#Save data
write_csv(Acin_B_CarbLA, "Acin_B_CarbLA.csv")

#Middle East
Acin_B_ME <- subset(merged_dataMEast, Species == "Acinetobacter baumannii")

#Select Carbapenem
Acin_B_CarbME <- Acin_B_ME %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Cefepime_I, Ceftazidime_I, Levofloxacin_I)

#Save data
write_csv(Acin_B_CarbME, "Acin_B_CarbME.csv")

# Create a data frame with the provided data
# Function to calculate the percentage of "Resistant" cases for each year
calculate_resistant_percentage <- function(df) {
  # Filter the data for "Resistant" entries
  resistant_data <- df[df == "Resistant"]
  
  # Calculate the percentage for each year
  df %>%
    group_by(Year) %>%
    summarise(
      Cefepime_Resistant = sum(Cefepime_I == "Resistant", na.rm = TRUE),
      Levofloxacin_Resistant = sum(Levofloxacin_I == "Resistant", na.rm = TRUE),
      Ceftazidime_Resistant = sum(Ceftazidime_I == "Resistant", na.rm = TRUE),
      Total_Entries = n()
    ) %>%
    mutate(
      Cefepime_Percentage = (Cefepime_Resistant / Total_Entries) * 100,
      Levofloxacin_Percentage = (Levofloxacin_Resistant / Total_Entries) * 100,
      Ceftazidime_Percentage = (Ceftazidime_Resistant / Total_Entries) * 100
    ) %>%
    select(Year, Cefepime_Percentage, Levofloxacin_Percentage, Ceftazidime_Percentage)
}

# Calculate the resistant percentage by year
resistant_percentages <- calculate_resistant_percentage(Acin_B_CarbData)

# Print the result
print("Resistant Percentage by Year:")
print(resistant_percentages)


#Enterococcus faecium

#ATLAS
Entero_B_Data <- subset(Data, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_DataS <- Entero_B_Data %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)

#Save data
#write_csv(Entero_B_CarbAF, "Entero_B_CarbAF.csv")

#Africa 
Entero_B_AF <- subset(merged_dataA, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_CarbAF <- Entero_B_AF %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)

#Save data
write_csv(Entero_B_CarbAF, "Entero_B_CarbAF.csv")

#Asia
Entero_B_AS <- subset(merged_dataASIA, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_CarbAS <- Entero_B_AS %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)

#Save data
write_csv(Entero_B_CarbAS, "Entero_B_CarbAS.csv")

#North America
Entero_B_NA <- subset(merged_dataNAm, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_CarbNA <- Entero_B_NA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)

#Save data
write_csv(Entero_B_CarbNA, "Entero_B_CarbNA.csv")

#Europe
Entero_B_EU <- subset(merged_dataEuro, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_CarbEU <- Entero_B_EU %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)

#Save data
write_csv(Entero_B_CarbEU, "Entero_B_CarbEU.csv")

#Latin America

Entero_B_LA <- subset(merged_dataLAme, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_CarbLA <- Entero_B_LA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)

#Save data
write_csv(Entero_B_CarbLA, "Entero_B_CarbLA.csv")

#Middle East
Entero_B_ME <- subset(merged_dataMEast, Species == "Enterococcus faecium")

#Select Carbapenem
Entero_B_CarbME <- Entero_B_ME %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Erythromycin_I, Ampicillin_I, Levofloxacin_I)

#Save data
write_csv(Entero_B_CarbME, "Entero_B_CarbME.csv")

# Create a data frame with the provided data
# Function to calculate the percentage of "Resistant" cases for each year
calculate_resistant_percentage <- function(df) {
  # Filter the data for "Resistant" entries
  resistant_data <- df[df == "Resistant"]
  
  # Calculate the percentage for each year
  df %>%
    group_by(Year) %>%
    summarise(
      Erythromycin_Resistant = sum(Erythromycin_I == "Resistant", na.rm = TRUE),
      Levofloxacin_Resistant = sum(Levofloxacin_I == "Resistant", na.rm = TRUE),
      Ampicillin_I_Resistant = sum(Ampicillin_I == "Resistant", na.rm = TRUE),
      Total_Entries = n()
    ) %>%
    mutate(
      Erythromycin_Percentage = (Erythromycin_Resistant / Total_Entries) * 100,
      Levofloxacin_Percentage = (Levofloxacin_Resistant / Total_Entries) * 100,
      Ampicillin_I_Percentage = (Ampicillin_I_Resistant / Total_Entries) * 100
    ) %>%
    select(Year, Erythromycin_Percentage, Levofloxacin_Percentage, Ampicillin_I_Percentage)
}

# Calculate the resistant percentage by year
resistant_percentages <- calculate_resistant_percentage(Entero_B_CarbAF)

# Print the result
print("Resistant Percentage by Year:")
print(resistant_percentages)


#Streptococcus pneumoniae
#All ATLAS Data
Staphy_B_AData <- subset(Data, Species == "Streptococcus pneumoniae")

#Select Carbapenem
Staphy_B_CarbData <- Staphy_B_AData %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Penicillin_I, Erythromycin_I, Meropenem_I)
#Save data
write_csv(Staphy_B_CarbData, "Staphy_B_CarbData.csv")

#Africa 
Staphy_B_AF <- subset(merged_dataA, Species == "Streptococcus pneumoniae")

#Select Carbapenem
Staphy_B_CarbAF <- Staphy_B_AF %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Penicillin_I, Erythromycin_I, Meropenem_I)

#Save data
write_csv(Staphy_B_CarbAF, "Staphy_B_CarbAF.csv")

#Asia
Staphy_B_AS <- subset(merged_dataASIA, Species == "Streptococcus pneumoniae")

#Select Carbapenem
Staphy_B_CarbAS <- Staphy_B_AS %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Penicillin_I, Erythromycin_I, Meropenem_I)

#Save data
write_csv(Staphy_B_CarbAS, "Staphy_B_CarbAS.csv")

#North America
Staphy_B_NA <- subset(merged_dataNAm, Species == "Streptococcus pneumoniae")

#Select Carbapenem
Staphy_B_CarbNA <- Staphy_B_NA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Penicillin_I, Erythromycin_I, Meropenem_I)

#Save data
write_csv(Staphy_B_CarbNA, "Staphy_B_CarbNA.csv")

#Europe
Staphy_B_EU <- subset(merged_dataEuro, Species == "Streptococcus pneumoniae")

#Select Carbapenem
Staphy_B_CarbEU <- Staphy_B_EU %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Penicillin_I, Erythromycin_I, Meropenem_I)

#Save data
write_csv(Staphy_B_CarbEU, "Staphy_B_CarbEU.csv")

#Latin America

Staphy_B_LA <- subset(merged_dataLAme, Species == "Streptococcus pneumoniae")

#Select Carbapenem
Staphy_B_CarbLA <- Staphy_B_LA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Penicillin_I, Erythromycin_I, Meropenem_I)

#Save data
write_csv(Staphy_B_CarbLA, "Staphy_B_CarbLA.csv")

#Middle East
Staphy_B_ME <- subset(merged_dataMEast, Species == "Streptococcus pneumoniae")

#Select Carbapenem
Staphy_B_CarbME <- Staphy_B_ME %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Penicillin_I, Erythromycin_I, Meropenem_I)

#Save data
write_csv(Staphy_B_CarbME, "Staphy_B_CarbME.csv")

# Create a data frame with the provided data
# Function to calculate the percentage of "Resistant" cases for each year
calculate_resistant_percentage <- function(df) {
  # Filter the data for "Resistant" entries
  resistant_data <- df[df == "Resistant"]
  
  # Calculate the percentage for each year
  df %>%
    group_by(Year) %>%
    summarise(
      Penicillin_Resistant = sum(Penicillin_I == "Resistant", na.rm = TRUE),
      Meropenem_Resistant = sum(Meropenem_I == "Resistant", na.rm = TRUE),
      Erythromycin_Resistant = sum(Erythromycin_I == "Resistant", na.rm = TRUE),
      Total_Entries = n()
    ) %>%
    mutate(
      Penicillin_Percentage = (Penicillin_Resistant / Total_Entries) * 100,
      Meropenem_Percentage = (Meropenem_Resistant / Total_Entries) * 100,
      Erythromycin_Percentage = (Erythromycin_Resistant / Total_Entries) * 100
    ) %>%
    select(Year, Penicillin_Percentage, Meropenem_Percentage, Erythromycin_Percentage)
}

# Calculate the resistant percentage by year
resistant_percentages <- calculate_resistant_percentage(Staphy_B_CarbEU)

# Print the result
print("Resistant Percentage by Year:")
print(resistant_percentages)


