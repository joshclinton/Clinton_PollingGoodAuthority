###########################################################################
###########################################################################
#   Weight YouGov Survey to CPS Targets
#   Josh Clinton
#   Vanderbilt University
#   October 2024
###########################################################################
###########################################################################

set.seed(42)
library(tidyverse)
library(haven)
library(ggplot2)
library(pewmethods)

# Create Weighting Targets
# Need to run every time to load into environment
source("ClintonCreateCPSTargets.R")

# Clean Data to make variables and values match Weighting Targets
# source("RecodeYouGov.R")

# load cleaned data
survey_data = readRDS(file = "ClintonGoodAuthorityData.Rds")
nrow(survey_data)

# Filter to Respondents Who Express an Opinion in 2024

survey_data = survey_data %>%
  filter(vote2024 == "Harris" | vote2024 == "Trump" | vote2024 == "Other")
nrow(survey_data)

# And Provide Demographics
# Note: pid3 codes "Not Sure" (N=153) as "independent" and codes "Other" (N=107), "Skipped", and "Not Asked" as missing

survey_data = survey_data %>%
  drop_na(pid3,vote2020,vote2024,educ_grp,age_grp,sex,race_grp,educ_sex,census_region,educ_race)
nrow(survey_data)


get_totals(var="vote2024",
           df=survey_data,
           digits=1) 

get_totals(var="vote2020",
           df=survey_data,
           digits=1) 

get_totals(var="pid3",
           df=survey_data,
           digits=1) 

get_totals(var="educ_grp",
           df=survey_data,
           digits=1) 

get_totals(var="age_grp",
           df=survey_data,
           digits=1) 

get_totals(var="race_grp",
           df=survey_data,
           digits=1) 

get_totals(var="sex",
           df=survey_data,
           digits=1) 



# Since All are registered voters, no need to filter

##########################################################################################################################################
# 2022
# NOTE: Weighting tolerances changed to 0.01 because non-convergence under default when using partisan leaning measures
# NOTE: Weighting to Gallup PID requires changing the tolerance to 0.1
##########################################################################################################################################

source(file = "ClintonWeightingCode2022.R")

margins = get_totals(var = "vote2024",
                     df = survey_data,
                     wt = c("nowgt","dem2022ni","dem2022","dem2022.pewpid3","dem2022.pewpid3nolean","dem2022.galluppid3","dem2022.2020.0","dem2022.2020.5","dem2022.2020.10","dem2022.2020.15","dem2022.2020.20","dem2022.2020.25"),
                     digits = 1) %>%
  filter(vote2024 == "Harris" | vote2024 == "Trump") %>%
  pivot_longer(cols = c("nowgt", starts_with("dem")),
               names_to = "weight",
               values_to = "value") %>%
  group_by(weight) %>%
  summarize(
    harris = sum(value[vote2024 == "Harris"], na.rm = TRUE),
    trump = sum(value[vote2024 == "Trump"], na.rm = TRUE),
    margin = harris - trump  # Calculate the margin
  ) 

# Merge in the Design Effect for each weight

margins = left_join(margins,DesignEffect,
                    by = "weight")

# Clean Up for display
# Note moe is for a proportion, not a difference of proportions
# Demographics with Interaction Effect (Demographics: 2022) is used in all PID/Past vote weightings 

margins %>%
  mutate(weight_label = case_when(
    weight == "nowgt" ~ "Raw Data",
    weight == "dem2022ni" ~ "Demographics (Only Marginals): 2022",
    weight == "dem2022" ~ "Demographics: 2022",
    weight == "dem2022.pewpid3" ~ "Demographics: Pew PID3",
    weight == "dem2022.pewpid3nolean" ~ "Demographics: Pew PID3 incl. leaners",
    weight == "dem2022.2020.0" ~ "Past Vote - 0% New",
    weight == "dem2022.2020.5" ~ "Past Vote - 5% New",
    weight == "dem2022.2020.10" ~ "Past Vote - 10% New",
    weight == "dem2022.2020.15" ~ "Past Vote - 15% New",
    weight == "dem2022.2020.20" ~ "Past Vote - 20% New",
    weight == "dem2022.2020.25" ~ "Past Vote - 25% New",
    TRUE ~ weight  # Default case
  )) %>%
  arrange(match(weight, c("nowgt", "dem2022ni","dem2022", "dem2022.pewpid3", "dem2022.pewpid3nolean","dem2022.2020.0", "dem2022.2020.5", "dem2022.2020.10", "dem2022.2020.15", "dem2022.2020.20", "dem2022.2020.25"))) %>%
  select(weight_label, harris, trump, margin, deff, moe)

##########################################################################################################################################
# 2020
# NOTE: Weighting tolerances changed to 0.01 because non-convergence under default when using partisan leaning measures
##########################################################################################################################################

source(file = "ClintonWeightingCode2020.R")

margins = get_totals(var = "vote2024",
                     df = survey_data,
                     wt = c("nowgt","dem2020ni","dem2020","dem2020.pewpid3","dem2020.pewpid3nolean","dem2020.galluppid3","dem2020.2020.0","dem2020.2020.5","dem2020.2020.10","dem2020.2020.15","dem2020.2020.20","dem2020.2020.25"),
                     digits = 1) %>%
  filter(vote2024 == "Harris" | vote2024 == "Trump") %>%
  pivot_longer(cols = c("nowgt", starts_with("dem")),
               names_to = "weight",
               values_to = "value") %>%
  group_by(weight) %>%
  summarize(
    harris = sum(value[vote2024 == "Harris"], na.rm = TRUE),
    trump = sum(value[vote2024 == "Trump"], na.rm = TRUE),
    margin = harris - trump  # Calculate the margin
  ) 

# Merge in the Design Effect for each weight

margins = left_join(margins,DesignEffect,
                    by = "weight")

# Clean Up for display
# Note moe is for a proportion, not a difference of proportions

margins %>%
  mutate(weight_label = case_when(
    weight == "nowgt" ~ "Raw Data",
    weight == "dem2020ni" ~ "Demographics (Only Marginals): 2020",
    weight == "dem2020" ~ "Demographics: 2020",
    weight == "dem2020.pewpid3" ~ "Demographics: Pew PID3",
    weight == "dem2020.pewpid3nolean" ~ "Demographics: Pew PID3 incl. leaners",
    weight == "dem2020.galluppid3" ~ "Demographics: Gallup PID3",
    weight == "dem2020.2020.0" ~ "Past Vote - 0% New",
    weight == "dem2020.2020.5" ~ "Past Vote - 5% New",
    weight == "dem2020.2020.10" ~ "Past Vote - 10% New",
    weight == "dem2020.2020.15" ~ "Past Vote - 15% New",
    weight == "dem2020.2020.20" ~ "Past Vote - 20% New",
    weight == "dem2020.2020.25" ~ "Past Vote - 25% New",
    TRUE ~ weight  # Default case
  )) %>%
  arrange(match(weight, c("nowgt", "dem2020ni","dem2020", "dem2020.pewpid3","dem2020.pewpid3nolean","dem2020.galluppid3", "dem2020.2020.0", "dem2020.2020.5", "dem2020.2020.10", "dem2020.2020.15", "dem2020.2020.20", "dem2020.2020.25"))) %>%
  select(weight_label, harris, trump, margin, deff, moe)

# What does PID look like across weightings?

get_totals(var="pid3",
           df=survey_data,
           wt = c("nowgt","dem2020ni","dem2020","dem2020.pewpid3","dem2020.pewpid3nolean","dem2020.galluppid3","dem2020.2020.0","dem2020.2020.5","dem2020.2020.10","dem2020.2020.15","dem2020.2020.20","dem2020.2020.25"),
           digits=1) 

##########################################################################################################################################
#   2016
# NOTE: Weighting tolerances changed to 0.01 because non-convergence under default when using partisan leaning measures
##########################################################################################################################################

source(file = "ClintonWeightingCode2016.R")

margins = get_totals(var = "vote2024",
                     df = survey_data,
                     wt = c("nowgt","dem2016ni","dem2016","dem2016.pewpid3","dem2016.pewpid3nolean","dem2016.galluppid3","dem2016.2020.0","dem2016.2020.5","dem2016.2020.10","dem2016.2020.15","dem2016.2020.20","dem2016.2020.25"),
                     digits = 1) %>%
  filter(vote2024 == "Harris" | vote2024 == "Trump") %>%
  pivot_longer(cols = c("nowgt", starts_with("dem")),
               names_to = "weight",
               values_to = "value") %>%
  group_by(weight) %>%
  summarize(
    harris = sum(value[vote2024 == "Harris"], na.rm = TRUE),
    trump = sum(value[vote2024 == "Trump"], na.rm = TRUE),
    margin = harris - trump)  # Calculate the margin

margins = left_join(margins,DesignEffect,
                    by = "weight")

margins %>%
  mutate(weight_label = case_when(
    weight == "nowgt" ~ "Raw Data",
    weight == "dem2016ni" ~ "Demographics (Only Marginals): 2016",
    weight == "dem2016" ~ "Demographics: 2016",
    weight == "dem2016.pewpid3" ~ "Demographics: Pew PID3",
    weight == "dem2016.pewpid3nolean" ~ "Demographics: Pew PID3 incl. leaner",
    weight == "dem2016.galluppid3" ~ "Demographics: Gallup PID3",
    weight == "dem2016.2020.0" ~ "Past Vote - 0% New",
    weight == "dem2016.2020.5" ~ "Past Vote - 5% New",
    weight == "dem2016.2020.10" ~ "Past Vote - 10% New",
    weight == "dem2016.2020.15" ~ "Past Vote - 15% New",
    weight == "dem2016.2020.20" ~ "Past Vote - 20% New",
    weight == "dem2016.2020.25" ~ "Past Vote - 25% New",
    TRUE ~ weight  # Default case
  )) %>%
  arrange(match(weight, c("nowgt", "dem2016ni","dem2016", "dem2016.pewpid3","dem2016.pewpid3nolean","dem2016.galluppid3", "dem2016.2020.0", "dem2016.2020.5", "dem2016.2020.10", "dem2016.2020.15", "dem2016.2020.20", "dem2016.2020.25"))) %>%
  select(weight_label, harris, trump, margin, deff, moe)

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
##  By Enthusiasm 2020
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

# Use Enthusiasm Scale as a Percentage to Apply to weighted Data
# First Weight As Above, Then compute compound weight by multiplying by scaled enthusiasm ranging from 0 to 1

survey_data_lv = survey_data %>%
  mutate(votelikely = if_else(votelikely == 11,10,votelikely)) %>%  # recode already voted to a 10
  filter(votelikely <= 10) %>%                                      # Select respondents with a scale from 1-10
  mutate(votelikely = votelikely - 1) %>%                           # rescale from 0 to 9 so will get a probabilty of 0
  mutate(votepct = votelikely/9)

survey_data_lv %>%
  group_by(vote2020) %>%
  summarize(AvgEnthusiasm = mean(votepct,na.rm=TRUE))

survey_data_lv %>%
  group_by(pid3) %>%
  summarize(AvgEnthusiasm = mean(votepct,na.rm=TRUE))

survey_data_lv %>%
  group_by(vote2024) %>%
  summarize(AvgEnthusiasm = mean(votepct,na.rm=TRUE))

# Margins before likely voter weights -- note the filter above
get_totals(var = "vote2024",
           df = survey_data_lv,
           wt = c("nowgt", "dem2020ni", "dem2020", "dem2020.pewpid3","dem2020.pewpid3nolean","dem2020.galluppid3", "dem2020.2020.0", "dem2020.2020.5", "dem2020.2020.10", "dem2020.2020.15", "dem2020.2020.20", "dem2020.2020.25"),
           digits = 1) 

# Adjust the weights by votepct
survey_data_lv <- survey_data_lv %>%
  mutate(
    nowgt = nowgt * votepct,
    dem2020ni = dem2020ni * votepct,
    dem2020 = dem2020 * votepct,
    dem2020_pewpid3 = dem2020.pewpid3 * votepct,
    dem2020_pewpid3nolean = dem2020.pewpid3nolean * votepct,
    dem2020_galluppid3 = dem2020.galluppid3 * votepct,
    dem2020_2020_0 = dem2020.2020.0 * votepct,
    dem2020_2020_5 = dem2020.2020.5 * votepct,
    dem2020_2020_10 = dem2020.2020.10 * votepct,
    dem2020_2020_15 = dem2020.2020.15 * votepct,
    dem2020_2020_20 = dem2020.2020.20 * votepct,
    dem2020_2020_25 = dem2020.2020.25 * votepct
  )

total = get_totals(var = "vote2024",
                   df = survey_data_lv,
                   wt = c("nowgt", "dem2020ni", "dem2020", "dem2020.pewpid3","dem2020.pewpid3nolean","dem2020.galluppid3", "dem2020.2020.0", "dem2020.2020.5", "dem2020.2020.10", "dem2020.2020.15", "dem2020.2020.20", "dem2020.2020.25"),
                   digits = 1) 
total = total %>%
  pivot_longer(cols = c("nowgt", starts_with("dem")),
               names_to = "weight",
               values_to = "value") 
prob = total %>%
  group_by(weight) %>%
  summarize(
    harris = sum(value[vote2024 == "Harris"], na.rm = TRUE),
    trump = sum(value[vote2024 == "Trump"], na.rm = TRUE),
    margin = harris - trump  # Calculate the margin
  ) %>%
  mutate(weight_label = case_when(
    weight == "nowgt" ~ "Raw Data",
    weight == "dem2020ni" ~ "Demographics (Only Marginals): 2020",
    weight == "dem2020" ~ "Demographics: 2020",
    weight == "dem2020.pewpid3" ~ "Demographics: Pew PID3",
    weight == "dem2020.pewpid3nolean" ~ "Demographics: Pew PID3 incl. leaners",
    weight == "dem2020.galluppid3" ~ "Demographics: Gallup PID3",
    weight == "dem2020.2020.0" ~ "Past Vote - 0% New",
    weight == "dem2020.2020.5" ~ "Past Vote - 5% New",
    weight == "dem2020.2020.10" ~ "Past Vote - 10% New",
    weight == "dem2020.2020.15" ~ "Past Vote - 15% New",
    weight == "dem2020.2020.20" ~ "Past Vote - 20% New",
    weight == "dem2020.2020.25" ~ "Past Vote - 25% New",
    TRUE ~ weight  # Default case
  )) %>%
  arrange(match(weight, c("nowgt", "dem2020ni", "dem2020", "dem2020.pewpid3","dem2020.pewpid3nolean","dem2020.galluppid3", "dem2020.2020.0", "dem2020.2020.5", "dem2020.2020.10", "dem2020.2020.15", "dem2020.2020.20", "dem2020.2020.25"))) %>%
  select(weight_label, harris, trump, margin) %>%
  mutate(votelikely_level = "Prob Vote")

prob
