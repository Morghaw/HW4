CHAT GPY CLASS CODE 

library(tidyverse)
install.packages("tidycensus")
library(tidycensus)
library(dplyr)
library(ggplot2)

library(sf)  # The sf package provides functions for working with spatial data
rm(list=ls())

setwd("/Users/morganhawthorne/Downloads")
data <- read_csv("airport_pairs (1).csv")

1. 
#BRING THE DATA IN FROM THE CSV FILE 
### AND THEN FILTER IT SO ANY FLIGHT TO OR FROM RALEIGH AND ANY AIRPORT OVER10,000 PEOPLE COMES 

```{r}

RDU_airport <- data %>%
  filter((origin == "RDU" | dest == "RDU") & passengers > 10000) %>%
  select(origin, dest, passengers)

###re-run the table 
RDU_airport 

```

2. # Load necessary libraries
library(tidycensus)
library(dplyr)
library(ggplot2)
library(readr)

# Set  Census API key 
census_api_key("142c5231f642cee1c4ee24f1ffca0bba6e1c50d7", install = TRUE)
airport_data <- read_csv("airport_pairs (1).csv")

# Retrieve CBSA-level population data
cbsa_data <- get_acs(geography = "cbsa",
                     variables = c(total_population = "B01003_001"),
                     year = 2022,
                     survey = "acs5") %>%
  select(GEOID, total_population = estimate) %>%
  rename(cbsa_code = GEOID) %>%
  mutate(cbsa_code = as.numeric(cbsa_code))

# Join CBSA population data for origin and destination
# Create separate origin and destination population data for easier joining
origin_population <- cbsa_data %>%
  rename(origin_cbsa = cbsa_code, origin_pop = total_population)

dest_population <- cbsa_data %>%
  rename(dest_cbsa = cbsa_code, dest_pop = total_population)

# Join population data with airport data
airport_data <- airport_data %>%
  left_join(origin_population, by = c("origin_cbsa" = "origin_cbsa")) %>%
  left_join(dest_population, by = c("dest_cbsa" = "dest_cbsa"))

# Filter out rows with missing CBSA population data
airport_data <- airport_data %>%
  filter(!is.na(origin_pop), !is.na(dest_pop))

# Aggregate by CBSA-to-CBSA routes to get total passenger volumes
cbsa_to_cbsa <- airport_data %>%
  group_by(origin_cbsa, dest_cbsa, origin_pop, dest_pop) %>%
  summarize(total_passengers = sum(passengers), avg_distance = mean(distancemiles), .groups = 'drop')

# Scatterplots
# 1. Origin population vs. Total passengers
ggplot(cbsa_to_cbsa, aes(x = origin_pop, y = total_passengers)) +
  geom_point(alpha = 0.5) +
  labs(title = "Origin Population vs Total Passengers",
       x = "Origin Population",
       y = "Total Passengers") +
  theme_minimal()

# 2. Destination population vs. Total passengers
ggplot(cbsa_to_cbsa, aes(x = dest_pop, y = total_passengers)) +
  geom_point(alpha = 0.5) +
  labs(title = "Destination Population vs Total Passengers",
       x = "Destination Population",
       y = "Total Passengers") +
  theme_minimal()

# 3. Flight distance vs. Total passengers
ggplot(cbsa_to_cbsa, aes(x = avg_distance, y = total_passengers)) +
  geom_point(alpha = 0.5) +
  labs(title = "Flight Distance vs Total Passengers",
       x = "Average Distance (miles)",
       y = "Total Passengers") +
  theme_minimal()

3.


# Run the regression model
single_variable_model = lm(total_passengers ~ origin_pop + dest_pop + avg_distance, data = cbsa_to_cbsa)

# to see the results of our model, we can run summary()
summary(single_variable_model)


4. 

RDU -> PDX 
  origin_pop.  dest_pop. avg_distance
  39580.        38900.     2,363


RDU -> ElP
  origin_pop.  dest_pop. avg_distance
   39570.         21340.    1606 

RDU -> TLH 
  origin_pop.  dest_pop. avg_distance
    45220.       45300.     496

RDU-> SMF
  origin_pop.  dest_pop. avg_distance
  39580.         40900         2345

   
   # Actual population values for each city
   RDU_population <- 1350000  # Example population for RDU
   PDX_population <- 2150000  # Example population for Portland, OR
   ELP_population <- 840000   # Example population for El Paso, TX
   TLH_population <- 390000   # Example population for Tallahassee, FL
   SMF_population <- 2400000  # Example population for Sacramento, CA
   
   # Create the data frame with actual populations for each route
   new_routes <- data.frame(
     origin_pop = c(RDU_population, RDU_population, RDU_population, RDU_population,
                    PDX_population, ELP_population, TLH_population, SMF_population),
     dest_pop = c(PDX_population, ELP_population, TLH_population, SMF_population,
                  RDU_population, RDU_population, RDU_population, RDU_population),
     avg_distance = c(2363, 1606, 496, 2345, 2363, 1606, 496, 2345),
     route = c("RDU to PDX", "RDU to ELP", "RDU to TLH", "RDU to SMF", 
               "PDX to RDU", "ELP to RDU", "TLH to RDU", "SMF to RDU")
   )
   
   # Use the model to predict passenger demand for each route
   new_routes$predicted_passengers <- predict(single_variable_model, newdata = new_routes)
   
   # Display the forecasted demand for each route
   library(dplyr)
   new_routes %>%
     select(route, origin_pop, dest_pop, avg_distance, predicted_passengers)
   
   