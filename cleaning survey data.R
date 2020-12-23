#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund website.
# Author: Jihong Huang, Wingyan Lee, Yi Qin, Shang Wu
# Data: 22 October 2020
# Contact: shang.wu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
knitr::opts_chunk$set(echo = TRUE)
# install.packages("tidyverse")
# install.packages("devtools")
library(tidyverse)
library(visdat)
library(cesR)
library(skimr)


get_ces("ces2019_phone")


# Just keep some variables of interest
survey_reduced_data <- 
  ces2019_phone %>% 
  select(q11, # vote
         q2,  # yob
         q3,  # gender
         q4,  # province
         q61, # education
         q63, # religion importance
         q64, # born place
         q69) # household income
    



#### Data Cleaning ####
survey_reduced_data <- na.omit(survey_reduced_data)


# 1. calculate the age to be correspondent to census data
survey_reduced_data <- survey_reduced_data %>%
  mutate(age = 2019 - q2)

# 2. categorize the education 
survey_reduced_data <- survey_reduced_data %>%
  filter(q61 != -8 & q61 != -9) %>%
  mutate(education = ifelse(q61 < 5.5, 
                            "Not attended university", 
                            "Attended university"))

# 3. categorize the importance of religion
survey_reduced_data <- survey_reduced_data %>%
  filter(q63 != -8 & q63 != -9) %>%
  mutate(religion_importance = ifelse(q63 > 2.5, 
                                      "Not important religion", 
                                      "Important religion"))

# 4. categorize the birth place
survey_reduced_data <- survey_reduced_data %>%
  filter(q64 != -8 & q64 != -9) %>% 
  mutate(place_birth_canada = ifelse(q64 == 1 | q64 == 2, "Born in Canada", 
                                     "Born outside Canada"))

# 5. categorize the vote
survey_reduced_data <- survey_reduced_data %>%
  filter(q11 != -8 & q11 != -9 & q11 != 7 & q11 != 8 & q11 != 9 & q11 != 10) %>%
  mutate(vote = q11)

# 6. rename the gender
survey_reduced_data <- survey_reduced_data %>%
  filter(q3 != -8 & q3 != -9) %>%
  mutate(sex = q3)

# 6. rename the gender
survey_reduced_data <- survey_reduced_data %>%
  filter(q4 != -8 & q4 != -9) %>%
  mutate(province = q4)

# 7. rename the household income
survey_reduced_data <- survey_reduced_data %>%
  filter(q69 != -8 & q69 != -9) %>%
  mutate(household_income = q69)





# delete extra variables
survey_reduced_data <- survey_reduced_data %>%
  select(vote, age, sex, province, education, religion_importance,
         place_birth_canada, household_income)




# substituting the values
# 3. filter the valid age and group into different ranges
survey_reduced_data <- survey_reduced_data %>%
  filter(age >= 18)

survey_reduced_data$age <-
  ifelse(survey_reduced_data$age < 30, "younger than 30 years old",
         ifelse(survey_reduced_data$age < 50, "30 - 50 years old",
                ifelse(survey_reduced_data$age < 70, "50 - 70 years old",
                       "older than 70 years old")))


survey_reduced_data$sex <-
  ifelse(survey_reduced_data$sex == 1, "Male", 
         ifelse(survey_reduced_data$sex == 2, "Female", 
                "Other"))

survey_reduced_data$province <-
  ifelse(survey_reduced_data$province == 1, "Newfoundland and Labrador", 
         ifelse(survey_reduced_data$province == 2, "Prince Edward Island",
                ifelse(survey_reduced_data$province == 3, "Nova Scotia", 
                       ifelse(survey_reduced_data$province == 4, "New Brunswick",
                              ifelse(survey_reduced_data$province == 5, "Quebec",
                                     ifelse(survey_reduced_data$province == 6, "Ontario",
                                            ifelse(survey_reduced_data$province == 7, "Manitoba",
                                                   ifelse(survey_reduced_data$province == 8, "Saskatchewan",
                                                          ifelse(survey_reduced_data$province == 9, "Alberta", 
                                                                 ifelse(survey_reduced_data$province == 10, "British Columbia",
                                                                        ifelse(survey_reduced_data$province == 11, "Northwest Territories",
                                                                               ifelse(survey_reduced_data$province == 12, "Yukon",
                                                                                      "Nunavut"))))))))))))


survey_reduced_data <- survey_reduced_data %>%
  mutate(income_family = ifelse(household_income < 25000, "Less than $25,000",
                                ifelse(household_income < 50000, "$25,000 to $49,999",
                                       ifelse(household_income < 75000, "$50,000 to $74,999",
                                              ifelse(household_income < 100000, "$75,000 to $99,999",
                                                     ifelse(household_income < 125000, "$100,000 to $ 124,999",
                                                            "$125,000 and more"))))))


survey_reduced_data <- survey_reduced_data %>%
  mutate(vote_party = ifelse(vote == 1, "Liberal",
                             ifelse(vote == 2, "Conservatives",
                                    ifelse(vote == 3, "NDP",
                                           ifelse(vote == 4, "Bloc Québécois",
                                                  ifelse(vote == 5, "Green Party",
                                                         "People’s Party"))))))


# for the purpose of separate predictions
survey_reduced_data <- survey_reduced_data %>%
  mutate(vote_liberal = ifelse(vote == 1, 1, 0)) %>%
  mutate(vote_conservatives = ifelse(vote == 2, 1, 0)) %>%
  mutate(vote_NDP = ifelse(vote == 3, 1, 0)) %>%
  mutate(vote_bloc_québécois = ifelse(vote == 4, 1, 0)) %>%
  mutate(vote_green = ifelse(vote == 5, 1, 0)) %>% 
  mutate(vote_people = ifelse(vote == 6, 1, 0))


# delete extra variables
survey_reduced_data <- survey_reduced_data %>%
  select(vote_party, age, sex, province, education, religion_importance,
         place_birth_canada, income_family, vote_liberal, vote_conservatives,
         vote_NDP, vote_bloc_québécois, vote_green, vote_people)


survey_reduced_data %>%
  group_by(income_family) %>%
  summarise(n=n())



# Saving the survey/sample data as a csv file in the working directory
write_csv(survey_reduced_data, "survey_data.csv")

