###########################################################################
###########################################################################
#   Wrangle YouGov Survey to Match CPS Targets
#   Josh Clinton
#   Vanderbilt University
###########################################################################
###########################################################################

# Get YouGov Data
# dat <- read_sav("VAND0046_W1_interim.sav")

### Recode so same variables as Weighting Targets used in CreateCPSTargets.R

dat$sex = NA
dat$sex[dat$gender==1] = "Male"
dat$sex[dat$gender==2] = "Female"

dat$educ_grp = NA
dat$educ_grp[dat$educ == 1 | dat$educ == 2] = "Hs Or Less"
dat$educ_grp[dat$educ == 3 | dat$educ == 4] = "Some College"
dat$educ_grp[dat$educ == 5] = "College"
dat$educ_grp[dat$educ == 6] = "Postgrad"

dat$race_grp = NA
dat$race_grp[dat$race == 1] = "White"
dat$race_grp[dat$race == 2] = "Black"
dat$race_grp[dat$race == 3] = "Hispanic"
dat$race_grp[dat$race == 4] = "Asian"
dat$race_grp[dat$race == 5 | dat$race == 6 | dat$race == 7 | dat$race == 8] = "Other"

dat$age = 2024 - dat$birthyr

dat$age_grp = NA
dat$age_grp[dat$age < 30] = "18-29"
dat$age_grp[dat$age >= 30 & dat$age < 45] = "30-44"
dat$age_grp[dat$age >= 45 & dat$age < 65] = "45-64"
dat$age_grp[dat$age >= 65] = "65+"

dat = dat %>%
  mutate(educ_race = case_when(
    race_grp != "White" & educ_grp %in% c("College", "Postgrad") ~ "Non-White College",
    race_grp != "White" & educ_grp %in% c("Hs Or Less", "Some College") ~ "Non-White Non-College",
    race_grp == "White" & educ_grp %in% c("College", "Postgrad") ~ "White College",
    race_grp == "White" & educ_grp %in% c("Hs Or Less", "Some College") ~ "White Non-College"
  ),
  educ_sex = case_when(
    sex != "Male" & educ_grp %in% c("College", "Postgrad") ~ "Female College",
    sex != "Male" & educ_grp %in% c("Hs Or Less", "Some College") ~ "Female Non-College",
    sex == "Male" & educ_grp %in% c("College", "Postgrad") ~ "Male College",
    sex == "Male" & educ_grp %in% c("Hs Or Less", "Some College") ~ "Male Non-College"
  ))

dat$npid7 = NA
dat$npid7[dat$pid7==1] = "Democrat"
dat$npid7[dat$pid7==2] = "Democrat"
dat$npid7[dat$pid7==3] = "Democrat"
dat$npid7[dat$pid7==5] = "Republican"
dat$npid7[dat$pid7==6] = "Republican"
dat$npid7[dat$pid7==7] = "Republican"
dat$npid7[dat$pid7==4] = "Independent"
dat$npid7[dat$pid7==8] = "Independent"
dat$npid7[dat$pid7==9] = "Independent"

# Drop Other, Skipped, Not Asked
# Recode Not sure as Independent

dat$npid3 = NA
dat$npid3[dat$pid3==1] = "Democrat"
dat$npid3[dat$pid3==2] = "Republican"
dat$npid3[dat$pid3==3] = "Independent"
dat$npid3[dat$pid3==5] = "Independent"

dat$vote2020 = NA
dat$vote2020[dat$presvote20post == 1] = "Biden"
dat$vote2020[dat$presvote20post == 2] = "Trump"
dat$vote2020[dat$presvote20post == 3] = "Other"
dat$vote2020[dat$presvote20post == 4] = "Other"
dat$vote2020[dat$presvote20post == 5] = "Other"
dat$vote2020[dat$presvote20post == 6] = "DNV"

dat$census_region = NA
dat$census_region[dat$region == 1] = "Northeast" 
dat$census_region[dat$region == 2] = "Midwest"
dat$census_region[dat$region == 3] = "South"
dat$census_region[dat$region == 4] = "West"

dat$vote2024 = NA
dat$vote2024[dat$V3 == 1] = "Trump"
dat$vote2024[dat$V3 == 2] = "Harris"
dat$vote2024[dat$V3 == 3] = "Other"
dat$vote2024[dat$V3 == 4] = "Not Sure"

dat_small = dat %>%
  select(sex,age_grp,educ_grp,race_grp,educ_race,educ_sex,votereg,vote2020,npid3,npid7,census_region,V1,vote2024) %>%
  filter(votereg == 1) %>%
  rename(votelikely = V1)

# Get rid of haven labels...

write_csv(dat_small,
          file="dat_small.csv")

dat_small = read_csv(file = "dat_small.csv")

dat_small = dat_small %>%
  rename(pid3=npid3) %>%
  rename(pid3nolean=npid7) %>%
  mutate(sex = as.factor(sex),
         age_grp = as.factor(age_grp),
         race_grp = as.factor(race_grp),
         educ_grp = as.factor(educ_grp),
         educ_race = as.factor(educ_race),
         educ_sex = as.factor(educ_sex),
         pid3 = as.factor(pid3),
         pid3nolean = as.factor(pid3nolean),
         vote2020 = as.factor(vote2020),
         census_region = as.factor(census_region))

write_rds(dat_small,
          file="ClintonGoodAuthorityData.Rds")