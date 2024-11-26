setwd("/Users/morganhawthorne/Downloads/RDS-2016-0005/Data")
data <- read.csv("TS3_Raw_tree_data.csv")
rm(list=ls())

1. 
# Load libraries
library(dplyr)
library(tidyr)

# Split the city column into cityname and stateabbreviation
data <- data %>%
  mutate(mCityName = sub(",.*$", "", City),
    StateAbbreviation = sub("^.*,\\s*", "", City))

# show first few rows to show the changes
head(data)


# 1. List the states present in the dataset
seperatecitystate <- unique(data$StateAbbreviation)
print(seperatecitystate)

# 2. Count the number of rows for each state
statecount <- data %>%
  group_by(StateAbbreviation) %>%
  summarize(Count = n())

print(statecount)

2. 
# Load libraries
library(stringr)

# Filter the dataset for the different carolinas, NC and SC 
carolinas <- data %>%
  filter(str_detect(City, ",\\s*(NC|SC)$"))

# filter cities
carolinas <- carolinas %>%
  mutate(CityName = str_extract(City, "^[^,]+"))

carolinacities <- unique(carolinas$CityName)

# Display the cities
print(carolinacities)


3. 

# take the genus from the scientificname column
carolinas <- carolinas %>%
  mutate(Genus = str_extract(ScientificName, "^[^\\s]+"))

# Calculate the average crown diameter for each genus
carolinalgcrown <- carolinas %>%
  group_by(Genus) %>%
  summarize(AverageCrownDiameter = mean(AvgCdia..m., na.rm = TRUE)) %>%
  arrange(desc(AverageCrownDiameter))

# Identify the genus with the largest crown diameter
largestcrowngenus <- carolinalgcrown[1, ]

# Display the result
print(largestcrowngenus)

