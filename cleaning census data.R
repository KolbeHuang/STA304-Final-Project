#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from GSS 
# for post stratification.
# Author: Jihong Huang
# Data: 19 December 2020
# Contact: kolbe.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)


# Read in the raw data.
census_raw_data <- read_csv("gss.csv")


# Keep some variables of interest
census_reduced_data <- 
  census_raw_data %>% 
  select(age, 
         sex, 
         province, 
         place_birth_canada,
         education, 
         religion_importance,
         income_family) 



#### Data Cleaning ####
census_reduced_data <- na.omit(census_reduced_data)


# remove the terms that say "do not know"
census_reduced_data <- census_reduced_data %>%
    filter(religion_importance != "Don't know")

census_reduced_data <- census_reduced_data %>%
    filter(place_birth_canada != "Don't know")


# The sex, province, place_birth_canada stay the same.


# 1. regroup education variable for easier prediction
census_reduced_data$education <- 
  ifelse(census_reduced_data$education == "High school diploma or a high school equivalency certificate" |
           census_reduced_data$education == "Less than high school diploma or its equivalent" |
           census_reduced_data$education == "Trade certificate or diploma", 
         "Not attended university", "Attended university")


# 2. regroup religion_importance variable for easier prediction
census_reduced_data$religion_importance <- 
  ifelse(census_reduced_data$religion_importance == "Don't know" |
           census_reduced_data$religion_importance == "Not at all important" |
           census_reduced_data$religion_importance == "Not very important", 
         "Not important religion", "Important religion")


# 3. filter the valid age and group into different ranges
census_reduced_data <- census_reduced_data %>%
  filter(age >= 18)

census_reduced_data$age <-
  ifelse(census_reduced_data$age < 30, "younger than 30 years old",
         ifelse(census_reduced_data$age < 50, "30 - 50 years old",
                ifelse(census_reduced_data$age < 70, "50 - 70 years old",
                       "older than 70 years old")))


# Saving the census data as a csv file in working directory
write_csv(census_reduced_data, "census_data.csv")


# group the census data into cells -- using all the attributes
census_celled_data <- census_reduced_data %>%
  count(age,
        sex,
        province,
        place_birth_canada,
        education,
        religion_importance,
        income_family) %>%
  group_by(age,
           sex,
           province,
           place_birth_canada,
           education,
           religion_importance,
           income_family) 

# view(census_reduced_data)

# Saving the celled census data as a csv file in working directory
write_csv(census_celled_data, "census_data_celled.csv")



         
         