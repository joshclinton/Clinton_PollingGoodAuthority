
# Raw Data Weight
survey_data$nowgt = 1

#####################################################################################################################
#   Now weight to 2016 electorate using demographics alone - no education interaction
#####################################################################################################################
dem2016ni <- rake_survey(survey_data,
                         epsilon = .01,
                         pop_margins = Targets2016ni)
survey_data$dem2016ni = dem2016ni

# What % are Did not vote after weighting?
get_totals(var = "vote2020",
           df = survey_data,
           wt = c("nowgt","dem2016ni"),
           digits = 1)

#####################################################################################################################
#   Now weight to 2020 electorate using demographics alone - education X White/Non-White and Education X Sex
#####################################################################################################################

dem2016 <- rake_survey(survey_data,
                       epsilon = .01,
                       pop_margins = Targets2016)
survey_data$dem2016 = dem2016

# What % are Did not vote after weighting?
get_totals(var = "vote2020",
           df = survey_data,
           wt = c("nowgt","dem2016"),
           digits = 1)

#####################################################################################################################
# Add pew pid3
#####################################################################################################################

# https://www.pewresearch.org/politics/2024/04/09/the-partisanship-and-ideology-of-american-voters/
# https://www.pewresearch.org/methods/fact-sheet/national-public-opinion-reference-survey-npors/

pid3nolean <- tibble::tibble(
  pid3nolean = as.factor(c("Democrat", "Republican", "Independent")),
  Freq = c(46, 47, 7)
)
pid3 <- tibble::tibble(
  pid3 = as.factor(c("Democrat", "Republican", "Independent")),
  Freq = c(33, 32, 35)
)
Targets2016[[8]] = pid3
dem2016.pewpid3 <- rake_survey(survey_data,
                               epsilon = .01,
                               pop_margins = Targets2016)
survey_data$dem2016.pewpid3 = dem2016.pewpid3

# No leaners

pid3nolean <- tibble::tibble(
  pid3nolean = as.factor(c("Democrat", "Republican", "Independent")),
  Freq = c(46, 47, 7))

Targets2016[[8]] = pid3nolean
dem2016.pewpid3nolean <- rake_survey(survey_data,
                                     epsilon = .1,
                                     pop_margins = Targets2016)
survey_data$dem2016.pewpid3nolean = dem2016.pewpid3nolean


#####################################################################################################################
# Add Gallup pid3
#####################################################################################################################

#https://news.gallup.com/poll/15370/party-affiliation.aspx


pid3 <- tibble::tibble(
  pid3 = as.factor(c("Democrat", "Republican", "Independent")),
  Freq = c(28, 31, 41)
)
Targets2016[[8]] = pid3
dem2016.galluppid3 <- rake_survey(survey_data,
                               epsilon = .01,
                               pop_margins = Targets2016)
survey_data$dem2016.galluppid3 = dem2016.galluppid3

#####################################################################################################################
# 2016 vote - 0% new voters
#####################################################################################################################

# p is the 2016 national popular vote for: Biden, Trump, Other
p = c(51.3,46.8,1.9)

# Add the value for DNV - here 0
vote2020 = tibble::tibble(
  vote2020 = as.factor(c("Biden", "Trump", "Other","DNV")),
  Freq = c(51.3,46.8,1.9,0)
)
Targets2016[[8]] = vote2020
targets_vote2016 <- rake_survey(survey_data,
                                epsilon = .01,
                                pop_margins = Targets2016) 
survey_data$dem2016.2020.0 = targets_vote2016

#####################################################################################################################
# 2016 vote - 5% new voters
#####################################################################################################################

p*.95
vote2020 = tibble::tibble(
  vote2020 = as.factor(c("Biden", "Trump", "Other","DNV")),
  Freq = c(48.7,44.5,1.8,5)
)
Targets2016[[8]] = vote2020
targets_vote2016 <- rake_survey(survey_data,
                                epsilon = .01,
                                pop_margins = Targets2016) 
survey_data$dem2016.2020.5 = targets_vote2016

#####################################################################################################################
# 2016 Vote - 10% new voters
#####################################################################################################################

p*.9
vote2020 = tibble::tibble(
  vote2020 = as.factor(c("Biden", "Trump", "Other","DNV")),
  Freq = c(46.2, 42.1, 1.7, 10)
)
Targets2016[[8]] = vote2020
targets_vote2016 <- rake_survey(survey_data,
                                epsilon = .01,
                                pop_margins = Targets2016) 
survey_data$dem2016.2020.10 = targets_vote2016

#####################################################################################################################
# 2016 vote - 15% new voters
#####################################################################################################################

p*.85
vote2020 = tibble::tibble(
  vote2020 = as.factor(c("Biden", "Trump", "Other","DNV")),
  Freq = c(43.6,39.8,1.6,15)
)
Targets2016[[8]] = vote2020
targets_vote2016 <- rake_survey(survey_data,
                                epsilon = .01,
                                pop_margins = Targets2016) 
survey_data$dem2016.2020.15 = targets_vote2016

#####################################################################################################################
# 2016 Vote - 20% new voters
#####################################################################################################################

p*.8
vote2020 = tibble::tibble(
  vote2020 = as.factor(c("Biden", "Trump", "Other","DNV")),
  Freq = c(41, 37, 2, 20)
)
Targets2016[[8]] = vote2020
targets_vote2016 <- rake_survey(survey_data,
                                epsilon = .01,
                                pop_margins = Targets2016) 
survey_data$dem2016.2020.20 = targets_vote2016

#####################################################################################################################
# 2016 vote -25% new voters
#####################################################################################################################

p*.75
vote2020 = tibble::tibble(
  vote2020 = as.factor(c("Biden", "Trump", "Other","DNV")),
  Freq = c(38.5, 35, 1.4, 25)
)
Targets2016[[8]] = vote2020
targets_vote2016 <- rake_survey(survey_data,
                                epsilon = .01,
                                pop_margins = Targets2016) 
survey_data$dem2016.2020.25 = targets_vote2016

#####################################################################################################################
# Save Design Effects for each Weighting Scheme
#####################################################################################################################

DesignEffect = NULL
# Bind rows with an additional column for the weight name
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016) %>% mutate(weight = "dem2016ni"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016) %>% mutate(weight = "dem2016"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.pewpid3) %>% mutate(weight = "dem2016.pewpid3"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.pewpid3nolean) %>% mutate(weight = "dem2016.pewpid3nolean"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.galluppid3) %>% mutate(weight = "dem2016.galluppid3"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.2020.0) %>% mutate(weight = "dem2016.2020.0"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.2020.5) %>% mutate(weight = "dem2016.2020.5"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.2020.10) %>% mutate(weight = "dem2016.2020.10"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.2020.15) %>% mutate(weight = "dem2016.2020.15"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.2020.20) %>% mutate(weight = "dem2016.2020.20"))
DesignEffect = bind_rows(DesignEffect, calculate_deff(survey_data$dem2016.2020.25) %>% mutate(weight = "dem2016.2020.25"))

rm(pid3,pid3nolean,vote2020)

