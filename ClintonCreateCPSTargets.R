###########################################################################
###########################################################################
# Create Weighting Targets
# 2016, 2020, 2022 CPS Voter Supplement
# Josh Clinton
# Vanderbilt University
###########################################################################
###########################################################################

library(haven)
library(tidyverse)
library(pewmethods)
library(readstata13)

##  Uses the STATA file created using "ReadCPS.do"
##  Run that first to create "CPSVoter.dta"

cps = read.dta13(file = "CPSVoter.dta")


#########################################################################
##    2020
#########################################################################

cps20 = cps %>%
  filter(year == 2020)

race_grp = get_totals(var = "race_grp",
                      df = cps20,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(race_grp = as.factor(race_grp))

age_grp = get_totals(var = "age_grp",
                     df = cps20,
                     wt = c("vosuppwt"),
                     digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(age_grp = as.factor(age_grp))

sex = get_totals(var = "sex",
                 df = cps20,
                 wt = c("vosuppwt"),
                 digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(sex = as.factor(sex)) 

educ_sex = get_totals(var = "educ_sex",
                      df = cps20,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_sex = as.factor(educ_sex)) 

educ_race = get_totals(var = "educ_race",
                       df = cps20,
                       wt = c("vosuppwt"),
                       digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_race = as.factor(educ_race)) 

census_region = get_totals(var = "census_region",
                           df = cps20,
                           wt = c("vosuppwt"),
                           digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(census_region = as.factor(census_region)) 

educ_grp = get_totals(var = "educ_grp",
                      df = cps20,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_grp = as.factor(educ_grp)) 

# Interaction with Education, Sex and Race

Targets2020 = list()
Targets2020[[1]] = age_grp
Targets2020[[2]] = race_grp
Targets2020[[3]] = educ_race
Targets2020[[4]] = census_region
Targets2020[[5]] = sex
Targets2020[[6]] = educ_grp
Targets2020[[7]] = educ_sex

# No Interaction with Education - just marginal

Targets2020ni = list()
Targets2020ni[[1]] = age_grp
Targets2020ni[[2]] = race_grp
Targets2020ni[[3]] = educ_grp
Targets2020ni[[4]] = census_region
Targets2020ni[[5]] = sex


#########################################################################
##    2016
#########################################################################

cps16 = cps %>%
  filter(year == 2016)

race_grp = get_totals(var = "race_grp",
                      df = cps16,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(race_grp = as.factor(race_grp))

age_grp = get_totals(var = "age_grp",
                     df = cps16,
                     wt = c("vosuppwt"),
                     digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(age_grp = as.factor(age_grp))

sex = get_totals(var = "sex",
                 df = cps16,
                 wt = c("vosuppwt"),
                 digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(sex = as.factor(sex)) 

educ_race = get_totals(var = "educ_race",
                       df = cps16,
                       wt = c("vosuppwt"),
                       digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_race = as.factor(educ_race)) 

educ_sex = get_totals(var = "educ_sex",
                      df = cps16,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_sex = as.factor(educ_sex)) 

census_region = get_totals(var = "census_region",
                           df = cps16,
                           wt = c("vosuppwt"),
                           digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(census_region = as.factor(census_region)) 

educ_grp = get_totals(var = "educ_grp",
                      df = cps16,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_grp = as.factor(educ_grp)) 

# Interaction with Education, Sex and Race

Targets2016 = list()
Targets2016[[1]] = age_grp
Targets2016[[2]] = race_grp
Targets2016[[3]] = educ_race
Targets2016[[4]] = census_region
Targets2016[[5]] = sex
Targets2016[[6]] = educ_grp
Targets2016[[7]] = educ_sex

# No Interaction

Targets2016ni = list()
Targets2016ni[[1]] = age_grp
Targets2016ni[[2]] = race_grp
Targets2016ni[[3]] = educ_grp
Targets2016ni[[4]] = census_region
Targets2016ni[[5]] = sex


#########################################################################
##    2022
#########################################################################

cps22 = cps %>%
  filter(year == 2022)

race_grp = get_totals(var = "race_grp",
                      df = cps22,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(race_grp = as.factor(race_grp))

age_grp = get_totals(var = "age_grp",
                     df = cps22,
                     wt = c("vosuppwt"),
                     digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(age_grp = as.factor(age_grp))

sex = get_totals(var = "sex",
                 df = cps22,
                 wt = c("vosuppwt"),
                 digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(sex = as.factor(sex)) 

educ_race = get_totals(var = "educ_race",
                       df = cps22,
                       wt = c("vosuppwt"),
                       digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_race = as.factor(educ_race)) 

educ_sex = get_totals(var = "educ_sex",
                      df = cps16,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_sex = as.factor(educ_sex)) 

census_region = get_totals(var = "census_region",
                           df = cps22,
                           wt = c("vosuppwt"),
                           digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(census_region = as.factor(census_region)) 

educ_grp = get_totals(var = "educ_grp",
                      df = cps22,
                      wt = c("vosuppwt"),
                      digits = 1) %>%
  rename(Freq = vosuppwt) %>%
  mutate(educ_grp = as.factor(educ_grp)) 

# Interaction with Education, Sex and Race

Targets2022 = list()
Targets2022[[1]] = age_grp
Targets2022[[2]] = race_grp
Targets2022[[3]] = educ_race
Targets2022[[4]] = census_region
Targets2022[[5]] = sex
Targets2022[[6]] = educ_grp
Targets2022[[7]] = educ_sex

# No Interaction

Targets2022ni = list()
Targets2022ni[[1]] = age_grp
Targets2022ni[[2]] = race_grp
Targets2022ni[[3]] = educ_grp
Targets2022ni[[4]] = census_region
Targets2022ni[[5]] = sex

# Clean up environment
rm(cps,cps16,cps20,cps22,educ_grp,educ_race,educ_sex,race_grp,sex,age_grp,census_region)