Final Project PLAN 372 

setwd("/Users/morganhawthorne/Downloads")
rm(list=ls())

# Load necessary library
library(dplyr)


##Load data in 
data <- read.csv("Schools-2.csv")

###Filter data so it focuses only on public schools 
### it is a public school if it has a school code over 0 
# Filter rows where SCHL_CODE > 0
publicschools <- data %>% 
  filter(SCHL_CODE > 0)

###filter the data now to see only elementary schools 
elementaryschools <- publicschools %>% 
  filter(SCHL_TYPE == "ES")

##now find the average population for elementary schools in New Hanover County 

sum(elementaryschools$ELEM_)

##9939 is the total number of students in public elementary schools in New Hanover county 


###run this to convert the numbers in the data set to numeric 
elementaryschools$CAPACITY <- as.numeric(elementaryschools$CAPACITY)

###now find the max capactiy of public elementary schools in New Hanover County 

sum(elementaryschools$CAPACITY)


###10101 is the MAX CAPACITY number of students in public elementary schools in New Hanover county

sum(elementaryschools$ELEM_)/ sum(elementaryschools$CAPACITY)
###  0.983962 not overcrowded 
# Load necessary library

# Add the overcrowded column
elementaryschools <- elementaryschools %>%
  mutate(overcrowded = ifelse(ELEM_ / CAPACITY > 1, "Yes", "No"))

table(elementaryschools$overcrowded)

###make a table displayin the name of the high school and if its overcrowded

elementary_table <- elementaryschools %>%
  select(NAME, overcrowded)
###filter the data now to see only middle schools 
middleschools <- publicschools %>% 
  filter(SCHL_TYPE == "MS")

##now find the average population for middle schools in New Hanover County 

sum(middleschools$INT_)

##4720 is the total number of students in public elementary schools in New Hanover county 

###run this to convert the numbers in the data set to numeric 
middleschools$CAPACITY <- as.numeric(middleschools$CAPACITY)

###now find the max capactiy of public middle schools in New Hanover County 

sum(middleschools$CAPACITY)

##5241 total capactiy of new hanover county middle schools 

sum(middleschools$INT_)/sum(middleschools$CAPACITY)
###.9005 not overcrowded 

middleschools <- middleschools %>%
  mutate(overcrowded = ifelse(INT_ / CAPACITY > 1, "Yes", "No"))
###make a table displayin the name of the middle school and if its overcrowded

middleschool_table <- middleschools %>%
  select(NAME, overcrowded)
###filter the data now to see only High schools schools 
highschools <- publicschools %>% 
  filter(SCHL_TYPE == "HS")

##now find the average population for high schools in New Hanover County 

sum(highschools$HIGH_)


##7471 is the total number of students in public high schools in New Hanover county 


###run this to convert the numbers in the data set to numeric 



###now find the max capacity of public high schools in New Hanover County 
sum(highschools$CAPACITY)



##7548 is the total number of students  public high schools in New Hanover county can hold

sum(highschools$HIGH_)/sum(highschools$CAPACITY)
###.9897
# Add the overcrowded column
highschools <- highschools %>%
  mutate(overcrowded = ifelse(HIGH_ / CAPACITY > 1, "Yes", "No"))
###make a table displayin the name of the high school and if its overcrowded

highschool_table <- highschools %>%
  select(NAME, overcrowded)
###now I add all the totals and divide them by the total capacity 

(sum(highschools$HIGH_) + sum(middleschools$INT_) + sum(elementaryschools$ELEM_)) / (sum(highschools$CAPACITY) + sum(middleschools$CAPACITY) + sum(elementaryschools$CAPACITY))

###.9667


###load in libraries 
library(ggplot2)
library(dplyr)

# Combine the data into one data frame for plotting
all_schools <- bind_rows(
  elementaryschools %>% mutate(SchoolType = "Elementary"),
  middleschools %>% mutate(SchoolType = "Middle"),
  highschools %>% mutate(SchoolType = "High")
)

# Summarize the data by school type and overcrowded status
summary_data <- all_schools %>%
  group_by(SchoolType, overcrowded) %>%
  summarize(Count = n(), .groups = "drop")

# Create the graph
ggplot(summary_data, aes(x = SchoolType, y = Count, fill = overcrowded)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Overcrowding Status by School Type in New Hanover County",
    x = "School Type",
    y = "Number of Schools",
    fill = "Overcrowded"
  ) +
  theme_minimal()


