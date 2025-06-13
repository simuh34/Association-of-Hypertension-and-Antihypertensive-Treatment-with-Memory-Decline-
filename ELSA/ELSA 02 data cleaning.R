#' elsa
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load packages
library(haven)
library(naniar)
library(psych)
library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(survey)
library(mice)
library(VIM)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis <- read.csv("D:/hypertension/ELSA/elsa_analysis.csv")
#n=19802
sum(!is.na(elsa_analysis$age))
#n=10601

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#define the sample
#1. must participate wave 6 and be aged 50+ at Wave 6
summary(elsa_analysis$age)
elsa_analysis <- elsa_analysis[which(elsa_analysis$age>=50), ]
hist(elsa_analysis$age)
#10372

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2.do not have NA in sex
#table(elsa_analysis$h6rural, exclude = NULL)
table(elsa_analysis$gender, exclude = NULL)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#3.check the NA and subset sample
md1 <- elsa_analysis %>%
  miss_var_summary()
md1

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(elsa_analysis$r6hibpe)
summary(elsa_analysis$r6systo)
summary(elsa_analysis$r6diasto)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#but mass nA in cognition
#6.46% NA in cognition6
#35.5% NA in cognition8

#have both blood pressure and hypertension question at wave 6
elsa_analysis <- elsa_analysis[-which(is.na(elsa_analysis$r6systo) |
                                          is.na(elsa_analysis$r6diasto)), ]
#n=7902

#' 
#' 
#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#4.receode variabels
#gender:1 is men; 2 is women
table(elsa_analysis$gender, exclude = NULL)
elsa_analysis$gender <- ifelse(elsa_analysis$gender==1, "men", "women")
elsa_analysis$gender <- factor(elsa_analysis$gender)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#age group
elsa_analysis$agecat <-ifelse(elsa_analysis$age >= 45 & 
                                  elsa_analysis$age <55, "age1",
                                ifelse(elsa_analysis$age >= 55 & 
                                         elsa_analysis$age <65, "age2", "age3"))
elsa_analysis$agecat <- factor(elsa_analysis$agecat, levels = c("age1", "age2", "age3"))
table(elsa_analysis$agecat, exclude = NULL)

#' 
#' self edu
#' 1.less than upper secondary education
#' 2.upper secondary and vocational training
#' 3.tertiary education
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(elsa_analysis$education, exclude = NULL)
elsa_analysis$education <- factor(elsa_analysis$education, levels = c(1, 2, 3))
table(elsa_analysis$education, exclude = NULL)

#' 
#' 
#' 
#' 1.never went to school
#' 2.age 14 or under
#' 3.at age 15
#' 4.at age 16
#' 5.at age 17
#' 6.at age 18
#' 7.age 19 or over
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(elsa_analysis$mother_edu, elsa_analysis$father_edu)
elsa_analysis$highest_education_parent<- ifelse(is.na(elsa_analysis$father_edu), elsa_analysis$mother_edu,
ifelse(
  is.na(elsa_analysis$mother_edu), elsa_analysis$father_edu,
  ifelse(elsa_analysis$father_edu> elsa_analysis$mother_edu, elsa_analysis$father_edu, elsa_analysis$mother_edu)
))
# Recategorize parental education based on the new categories
elsa_analysis$highest_education_parent <- ifelse(elsa_analysis$highest_education_parent %in% c(1, 2, 3, 4), 1,
                                   ifelse(elsa_analysis$highest_education_parent %in% c(5, 6), 2,
                                          ifelse(elsa_analysis$highest_education_parent == 7, 3, NA)))

# Convert the recategorized variable to a factor with specified levels
elsa_analysis$highest_education_parent <- factor(elsa_analysis$highest_education_parent, 
                                   levels = c(1, 2, 3))
table(elsa_analysis$highest_education_parent, exclude = NULL)

#' 
#' 
#' 
#' 
#' 1.married
#' 3.partnered
#' 4.separated
#' 5.divorced
#' 7.widowed
#' 8.never married
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
table(elsa_analysis$marital_status, exclude = NULL)
elsa_analysis[, 18:21] <- lapply(elsa_analysis[, 18:21], function(x) ifelse(x==1|x==3, "married/partnered", "other"))
elsa_analysis[, 18:21] <- lapply(elsa_analysis[, 18:21], function(x) factor(x, levels = c("married/partnered", "other")))
table(elsa_analysis$marital_status, exclude = NULL)

#' 
#' birthplace
#' 1.uk
#' 11.elsewhere outside of uk
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#birth place: 1.uk, 11.elsewhere outside of uk
table(elsa_analysis$birthplace, exclude = NULL)
elsa_analysis$birthplace <- lapply(elsa_analysis$birthplace, function(x) ifelse(x==1, "a", "b"))
elsa_analysis$birthplace <- lapply(elsa_analysis$birthplace, function(x) factor(x, levels = c("a", "b")))
elsa_analysis$birthplace[is.na(elsa_analysis$birthplace)] <- "a"
elsa_analysis$birthplace <- unlist(elsa_analysis$birthplace)
table(elsa_analysis$birthplace, exclude = NULL)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#race/ethnicity: 1.white, 4.non-white
table(elsa_analysis$race, exclude = NULL)
elsa_analysis$race <- lapply(elsa_analysis$race, function(x) ifelse(x==1, "a", "b"))
elsa_analysis$race <- lapply(elsa_analysis$race, function(x) factor(x, levels = c("a", "b")))
elsa_analysis$race[is.na(elsa_analysis$race)] <- "a"
elsa_analysis$race <- unlist(elsa_analysis$race)
table(elsa_analysis$race, exclude = NULL)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#hhold total monthly consumption
summary(elsa_analysis$consumption)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis$consumption[elsa_analysis$consumption == 0] <- NA
elsa_analysis$hh7ctot1m[elsa_analysis$hh7ctot1m == 0] <- NA
elsa_analysis$hh8ctot1m[elsa_analysis$hh8ctot1m == 0] <- NA
elsa_analysis[, c(24:26)] <- lapply(elsa_analysis[, c(24:26)], function(x) log(x))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(elsa_analysis$consumption)

#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#bmi, smoking, and drinking
summary(elsa_analysis$bmi)
elsa_analysis$bmi[elsa_analysis$bmi > 60] <- NA

#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(elsa_analysis$r6smoken, exclude = NULL)
elsa_analysis$r6smoken[is.na(elsa_analysis$r6smoken)] <- 0
table(elsa_analysis$r6smokev, exclude = NULL)
elsa_analysis$r6smokev[is.na(elsa_analysis$r6smokev)] <- 0
table(elsa_analysis$r6smoken, elsa_analysis$r6smokev)
elsa_analysis$smoke_2012 <-ifelse(elsa_analysis$r6smoken == 1, "current smoker",
                                  ifelse(elsa_analysis$r6smoken == 0 &
                                         elsa_analysis$r6smokev == 1, "past smoker", "non smoker"))
elsa_analysis$smoke_2012 <- factor(elsa_analysis$smoke_2012, levels = c("non smoker", "past smoker", "current smoker"))
table(elsa_analysis$smoke_2012)


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(elsa_analysis$r6drink)
elsa_analysis$drink <- elsa_analysis$r6drink
table(elsa_analysis$drink, exclude = NULL)
prop.table(table(elsa_analysis$drink))

#' 
#' table(elsa_analysis$r1drinkr_c)
#' elsa_analysis$drink_heavy <- ifelse(elsa_analysis$r1drinkr_c>3, 1, 0)
#' table(elsa_analysis$drink_heavy, exclude = NULL)
#' prop.table(table(elsa_analysis$drink_heavy))
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#sleep was restless
table(elsa_analysis$r6sleepr, exclude = NULL)
elsa_analysis$r6sleepr <- ifelse(elsa_analysis$r6sleepr == 0, "no",
                                 ifelse(elsa_analysis$r6sleepr == 1, "yes", NA))
elsa_analysis$r6sleepr <- factor(elsa_analysis$r6sleepr, 
                                   levels = c("no", "yes"))

# Display a table of the recategorized variable
table(elsa_analysis$r6sleepr, exclude = NULL)
elsa_analysis$restless <- elsa_analysis$r6sleepr
table(elsa_analysis$restless, exclude = NULL)

#' 
#' 2.> 1 per week
#' 3.1 per week
#' 4.1-3 per mon
#' 5.hardly ever or never
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#physical activity
table(elsa_analysis$r6vgactx_e, exclude = NULL)
table(elsa_analysis$r6mdactx_e, exclude = NULL)
elsa_analysis$phy_act <- ifelse(elsa_analysis$r6vgactx_e == 2 & elsa_analysis$r6mdactx_e == 2, 1, 0)
elsa_analysis$phy_act <- ifelse(elsa_analysis$phy_act == 0, "no",
                                 ifelse(elsa_analysis$phy_act == 1, "yes", NA))
elsa_analysis$phy_act <- factor(elsa_analysis$phy_act, 
                                   levels = c("no", "yes"))
table(elsa_analysis$phy_act, exclude = NULL)

#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#social activity (yearly)
table(elsa_analysis$r6socyr, exclude = NULL)
elsa_analysis$r6socyr <- ifelse(elsa_analysis$r6socyr == 0, "no",
                                 ifelse(elsa_analysis$r6socyr == 1, "yes", NA))
elsa_analysis$r6socyr <- factor(elsa_analysis$r6socyr, 
                                   levels = c("no", "yes"))
elsa_analysis$social_act <- elsa_analysis$r6socyr
# Display a table of the recategorized variable
table(elsa_analysis$r6socyr, exclude = NULL)
table(elsa_analysis$social_act, exclude = NULL)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(elsa_analysis$wealth)
summary(elsa_analysis$hh6hhresp)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#equivalized_wealth
elsa_analysis$hh6hhresp <- as.numeric(as.character(elsa_analysis$hh6hhresp))
elsa_analysis$equivalized_wealth <- elsa_analysis$wealth/sqrt(elsa_analysis$hh6hhresp)
elsa_analysis$equivalized_wealth <- round(elsa_analysis$equivalized_wealth/1000, 2)
summary(elsa_analysis$equivalized_wealth)
elsa_analysis$wealth_quartile <- cut(
  elsa_analysis$equivalized_wealth,
  breaks = quantile(elsa_analysis$equivalized_wealth, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("btm", "lower", "upper", "top")
)
table(elsa_analysis$wealth_quartile)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#5.code exposure: blood pressure, hypertension, and treatment
names(elsa_analysis)
table(elsa_analysis$r6rxhibp)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#sbp, dbp, and taking medication
#elsa_analysis$htn_2012 <- ifelse(elsa_analysis$r6systo >= 140 | 
#                                   elsa_analysis$r6diasto >= 90 | 
#                                   elsa_analysis$r6rxhibp == 1, 1, 0)
elsa_analysis$htn_2012 <- ifelse(
  elsa_analysis$r6systo >= 140 | 
  elsa_analysis$r6diasto >= 90 | 
  (!is.na(elsa_analysis$r6rxhibp) & elsa_analysis$r6rxhibp == 1), 
  1, 0)


table(elsa_analysis$htn_2012, exclude = NULL)

elsa_analysis$htn_2016 <- ifelse(
  elsa_analysis$r8systo >= 140 | 
  elsa_analysis$r8diasto >= 90 | 
  (!is.na(elsa_analysis$r8rxhibp) & elsa_analysis$r8rxhibp == 1), 
  1, 0)
table(elsa_analysis$htn_2016, exclude = NULL)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis$r6rxhibp[is.na(elsa_analysis$r6rxhibp)] <- 0
elsa_analysis$ht_status2012 <- ifelse(elsa_analysis$htn_2012 == 0, "non hypertensive",
                                        ifelse(elsa_analysis$htn_2012 == 1 &
                                               elsa_analysis$r6rxhibp == 0, "hypertensive and untreated",
                                               ifelse(elsa_analysis$htn_2012 == 1 &
                                                      elsa_analysis$r6rxhibp == 1 &
                                                      elsa_analysis$r6systo < 140 &
                                                      elsa_analysis$r6diasto < 90, "hypertensive, treated and controlled",
                                                      "hypertensive, treated but uncontrolled")))
elsa_analysis$ht_status2012 <- factor(elsa_analysis$ht_status2012, levels = c("hypertensive and untreated", 
                                                                                  "hypertensive, treated but uncontrolled", 
                                                                                  "hypertensive, treated and controlled",
                                                                                  "non hypertensive"))
table(elsa_analysis$ht_status2012, exclude = NULL)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis$ht_status2016 <- ifelse(elsa_analysis$htn_2016 == 0, "non hypertensive",
                                        ifelse(elsa_analysis$htn_2016 == 1 &
                                               elsa_analysis$r8rxhibp == 0, "hypertensive and untreated",
                                               ifelse(elsa_analysis$htn_2016 == 1 &
                                                      elsa_analysis$r8rxhibp == 1 &
                                                      elsa_analysis$r8systo < 140 &
                                                      elsa_analysis$r8diasto < 90, "hypertensive, treated and controlled",
                                                      "hypertensive, treated but uncontrolled")))
elsa_analysis$ht_status2016 <- factor(elsa_analysis$ht_status2016, levels = c("hypertensive and untreated", 
                                                                                  "hypertensive, treated but uncontrolled", 
                                                                                  "hypertensive, treated and controlled",
                                                                                  "non hypertensive"))
table(elsa_analysis$ht_status2016)

#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#6. cognition difference 
# standardize cognition score
elsa_analysis$wave6_z <- scale(elsa_analysis$cognition6)
mean_baseline <- mean(elsa_analysis$cognition6, na.rm = TRUE)
sd_baseline <- sd(elsa_analysis$cognition6, na.rm = TRUE)
# cognition2 z-score
elsa_analysis$wave7_z <- (elsa_analysis$cognition7 - mean_baseline) / sd_baseline
# cognition3 z-score
elsa_analysis$wave8_z <- (elsa_analysis$cognition8 - mean_baseline) / sd_baseline
# cognition4 z-score
elsa_analysis$wave9_z <- (elsa_analysis$cognition9 - mean_baseline) / sd_baseline

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####covariates missingness####
# Step 1: Define covariates
covariates <- c("age", "gender", "education","marital_status", "birthplace", "race", "bmi", "smoke_2012", "drink", "restless", "phy_act", "social_act", "wealth_quartile")

# Step 2: Check the missing data pattern
md.pattern(elsa_analysis[covariates])

# Step 3: Simple missing data visualization

aggr_plot <- aggr(elsa_analysis[covariates], col=c('navyblue','red3'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(elsa_analysis[covariates]),
                  cex.axis=.7, gap=3,
                  ylab=c("Missing data","Pattern"))

#Step 4: Get missingness percentage
missing_summary <- sapply(elsa_analysis[covariates], function(x) mean(is.na(x)) * 100)
print(round(missing_summary, 2))


#' 
#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#extract complete data
write.csv(elsa_analysis, "elsa_df_complete.csv")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#impute covariates
elsa_analysis_converted <- elsa_analysis[, c("age", "gender","education","highest_education_parent","marital_status","birthplace","race","consumption" ,"bmi","smoke_2012" ,"drink","restless","phy_act","social_act","wealth_quartile")]
classes <- sapply(elsa_analysis_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
elsa_analysis_converted <- elsa_analysis_converted %>%
  mutate_if(names(.) %in% c("gender","education","highest_education_parent","marital_status","birthplace","race","smoke_2012" ,"drink_heavy","restless","phy_act","social_act","wealth_quartile"), as.factor)
set.seed(1005)
mice_mod <- mice(elsa_analysis_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(elsa_analysis), names(imputed_data))
elsa_analysis[common_cols] <- imputed_data[common_cols]

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write.csv(elsa_analysis, "ELSA/elsa_df.csv")


#' 
