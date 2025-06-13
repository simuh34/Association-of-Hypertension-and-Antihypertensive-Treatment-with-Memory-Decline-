#define the sample
#1. must participate wave 6 and be aged 50+ at Wave 6
summary(elsa_analysis$age)
elsa_analysis <- elsa_analysis[which(elsa_analysis$age>=50), ]

#have both blood pressure and hypertension question at wave 6
elsa_analysis <- elsa_analysis[-which(is.na(elsa_analysis$r6systo) |
                                          is.na(elsa_analysis$r6diasto)), ]

#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
table(elsa_analysis$marital_status, exclude = NULL)
elsa_analysis[, 18:21] <- lapply(elsa_analysis[, 18:21], function(x) ifelse(x==1|x==3, "married/partnered", "other"))
elsa_analysis[, 18:21] <- lapply(elsa_analysis[, 18:21], function(x) factor(x, levels = c("married/partnered", "other")))
table(elsa_analysis$marital_status, exclude = NULL)

elsa_analysis$birthplace <- lapply(elsa_analysis$birthplace, function(x) ifelse(x==1, "a", "b"))
elsa_analysis$birthplace <- lapply(elsa_analysis$birthplace, function(x) factor(x, levels = c("a", "b")))
elsa_analysis$birthplace[is.na(elsa_analysis$birthplace)] <- "a"
elsa_analysis$birthplace <- unlist(elsa_analysis$birthplace)
table(elsa_analysis$birthplace, exclude = NULL)

elsa_analysis$race <- lapply(elsa_analysis$race, function(x) ifelse(x==1, "a", "b"))
elsa_analysis$race <- lapply(elsa_analysis$race, function(x) factor(x, levels = c("a", "b")))
elsa_analysis$race[is.na(elsa_analysis$race)] <- "a"
elsa_analysis$race <- unlist(elsa_analysis$race)

elsa_analysis$consumption[elsa_analysis$consumption == 0] <- NA
elsa_analysis$hh7ctot1m[elsa_analysis$hh7ctot1m == 0] <- NA
elsa_analysis$hh8ctot1m[elsa_analysis$hh8ctot1m == 0] <- NA
elsa_analysis[, c(24:26)] <- lapply(elsa_analysis[, c(24:26)], function(x) log(x))

elsa_analysis$smoke_2012 <-ifelse(elsa_analysis$r6smoken == 1, "current smoker",
                                  ifelse(elsa_analysis$r6smoken == 0 &
                                         elsa_analysis$r6smokev == 1, "past smoker", "non smoker"))
elsa_analysis$smoke_2012 <- factor(elsa_analysis$smoke_2012, levels = c("non smoker", "past smoker", "current smoker"))

#sleep was restless
table(elsa_analysis$r6sleepr, exclude = NULL)
elsa_analysis$r6sleepr <- ifelse(elsa_analysis$r6sleepr == 0, "no",
                                 ifelse(elsa_analysis$r6sleepr == 1, "yes", NA))
elsa_analysis$r6sleepr <- factor(elsa_analysis$r6sleepr, 
                                   levels = c("no", "yes"))

table(elsa_analysis$r6socyr, exclude = NULL)
elsa_analysis$r6socyr <- ifelse(elsa_analysis$r6socyr == 0, "no",
                                 ifelse(elsa_analysis$r6socyr == 1, "yes", NA))
elsa_analysis$r6socyr <- factor(elsa_analysis$r6socyr, 
                                   levels = c("no", "yes"))
elsa_analysis$social_act <- elsa_analysis$r6socyr

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

elsa_analysis$htn_2012 <- ifelse(
  elsa_analysis$r6systo >= 140 | 
  elsa_analysis$r6diasto >= 90 | 
  (!is.na(elsa_analysis$r6rxhibp) & elsa_analysis$r6rxhibp == 1), 
  1, 0)

elsa_analysis$htn_2016 <- ifelse(
  elsa_analysis$r8systo >= 140 | 
  elsa_analysis$r8diasto >= 90 | 
  (!is.na(elsa_analysis$r8rxhibp) & elsa_analysis$r8rxhibp == 1), 
  1, 0)

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

elsa_analysis_converted <- elsa_analysis[, c("age", "gender","education","highest_education_parent","marital_status","birthplace","race","consumption" ,"bmi","smoke_2012" ,"drink","restless","phy_act","social_act","wealth_quartile")]
set.seed(1005)
mice_mod <- mice(elsa_analysis_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(elsa_analysis), names(imputed_data))
elsa_analysis[common_cols] <- imputed_data[common_cols]
