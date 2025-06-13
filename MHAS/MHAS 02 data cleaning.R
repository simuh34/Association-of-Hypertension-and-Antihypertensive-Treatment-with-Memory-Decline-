summary(MHAS_analysis_1$age)
MHAS_analysis<- MHAS_analysis_1[which(MHAS_analysis_1$age>=50), ]

#have both blood pressure and hypertension question at wave 3
MHAS_analysis <- MHAS_analysis[-which(is.na(MHAS_analysis$r3hibpe) &
                                            is.na(MHAS_analysis$r3systo) &
                                            is.na(MHAS_analysis$r3diasto)), ]

#self edu:education, 
## 1.Less than lower secondary, 2.upper secondary & vocational training, 3.tertiary
table(MHAS_analysis$education, exclude = NULL)

#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
MHAS_analysis[, 13:15] <- lapply(MHAS_analysis[, 13:15], function(x) ifelse(x==1|x==3, "married/partnered", "other"))
MHAS_analysis[, 13:15] <- lapply(MHAS_analysis[, 13:15], function(x) factor(x, levels = c("married/partnered", "other")))
table(MHAS_analysis$marital_status, exclude = NULL)

#residence
MHAS_analysis[, c(16:18)] <- lapply(MHAS_analysis[, c(16:18)], function(x) ifelse(x==1, "rural", "urban"))
MHAS_analysis[, c(16:18)] <- lapply(MHAS_analysis[, c(16:18)], function(x) factor(x, levels = c("rural", "urban")))
table(MHAS_analysis$urbanicity)

#yearly household consumption per capita
summary(MHAS_analysis$consumption)
MHAS_analysis$consumption[MHAS_analysis$consumption == 0] <- NA
MHAS_analysis$hh4ctot1m[MHAS_analysis$hh4ctot1m == 0] <- NA
MHAS_analysis$hh5ctot1m[MHAS_analysis$hh5ctot1m == 0] <- NA
MHAS_analysis[, c(19:21)] <- lapply(MHAS_analysis[, c(19:21)], function(x) log(x*12))

#bmi, smoking
summary(MHAS_analysis$bmi)
MHAS_analysis$bmi[MHAS_analysis$bmi > 60] <- NA
hist(MHAS_analysis$bmi)
summary(MHAS_analysis$bmi)

##smoking
table(MHAS_analysis$r3smoken, exclude = NULL)
MHAS_analysis$r3smoken[is.na(MHAS_analysis$r3smoken)] <- 0
table(MHAS_analysis$r3smokev, exclude = NULL)
MHAS_analysis$r3smokev[is.na(MHAS_analysis$r3smokev)] <- 0
table(MHAS_analysis$r3smoken, MHAS_analysis$r3smokev)
MHAS_analysis$smoke_2012 <-ifelse(MHAS_analysis$r3smoken == 1, "current smoker",
                                    ifelse(MHAS_analysis$r3smoken == 0 &
                                             MHAS_analysis$r3smokev == 1, "past smoker", "non smoker"))
MHAS_analysis$smoke_2012 <- factor(MHAS_analysis$smoke_2012, levels = c("non smoker", "past smoker", "current smoker"))
table(MHAS_analysis$smoke_2012)

#if drink
table(MHAS_analysis$r3drinkd)
MHAS_analysis$drink_ever <- ifelse(MHAS_analysis$r3drinkd>0, "yes", "no")
table(MHAS_analysis$drink_ever, exclude = NULL)
prop.table(table(MHAS_analysis$drink_ever))

#sleep 1.Rarely or none of the time < 1 day 2.Some or a little of the time 1-2 days 3.Occasionally or a moderate amount of 4.Most or all of the time 5-7 days
table(MHAS_analysis$r3sleepr)
MHAS_analysis$restless <- factor(ifelse(MHAS_analysis$r3sleepr == 1, "no", ifelse(is.na(MHAS_analysis$r3sleepr), MHAS_analysis$r3sleepr, "yes")))

#social activity
table(MHAS_analysis$r3socact_m, exclude = NULL)
MHAS_analysis$r3socact_m <- ifelse(MHAS_analysis$r3socact_m == 9 , "no", "yes")
MHAS_analysis$r3socact_m <- factor(MHAS_analysis$r3socact_m, 
                                levels = c("yes", "no"))
MHAS_analysis$social_act <- MHAS_analysis$r3socact_m
## Display a table of the recategorized variable
table(MHAS_analysis$r3socact_m, exclude = NULL)
table(MHAS_analysis$social_act, exclude = NULL)

#equivalized_wealth
MHAS_analysis$h3hhres <- as.numeric(as.character(MHAS_analysis$h3hhres))
MHAS_analysis$equivalized_wealth <- MHAS_analysis$wealth/sqrt(MHAS_analysis$h3hhres)
MHAS_analysis$equivalized_wealth <- round(MHAS_analysis$equivalized_wealth/1000, 2)
summary(MHAS_analysis$equivalized_wealth)
MHAS_analysis$wealth_quartile <- cut(
  MHAS_analysis$equivalized_wealth,
  breaks = quantile(MHAS_analysis$equivalized_wealth, probs = c(0, 0.25, 0.5,0.75, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("1", "2", "3","4")
)
table(MHAS_analysis$wealth_quartile)

#sbp, dbp, and taking medication
MHAS_analysis$htn_2012 <- ifelse(MHAS_analysis$r3systo >= 140 | 
                                     MHAS_analysis$r3diasto >= 90 | 
                                     MHAS_analysis$r3rxhibp == 1, 1, 0)
table(MHAS_analysis$htn_2012, exclude = NULL)


MHAS_analysis$r3rxhibp[is.na(MHAS_analysis$r3rxhibp)] <- 0
MHAS_analysis$ht_status2012 <- ifelse(MHAS_analysis$htn_2012 == 0, "non hypertensive",
                                        ifelse(MHAS_analysis$htn_2012 == 1 &
                                                 MHAS_analysis$r3rxhibp == 0, "hypertensive and untreated",
                                               ifelse(MHAS_analysis$htn_2012 == 1 &
                                                        MHAS_analysis$r3rxhibp == 1 &
                                                        MHAS_analysis$r3systo < 140 &
                                                        MHAS_analysis$r3diasto < 90, "hypertensive, treated and controlled",
                                                      "hypertensive, treated but uncontrolled")))
MHAS_analysis$ht_status2012 <- factor(MHAS_analysis$ht_status2012, levels = c("non hypertensive", 
                                                                              "hypertensive, treated and controlled", 
                                                                              "hypertensive, treated but uncontrolled", 
                                                                              "hypertensive and untreated"))
# standardize cognition score
MHAS_analysis$wave3_z <- scale(MHAS_analysis$cognition1)
mean_baseline <- mean(MHAS_analysis$cognition1, na.rm = TRUE)
sd_baseline <- sd(MHAS_analysis$cognition1, na.rm = TRUE)
# cognition2 z-score
MHAS_analysis$wave4_z <- (MHAS_analysis$cognition2 - mean_baseline) / sd_baseline
# cognition3 z-score
MHAS_analysis$wave5_z <- (MHAS_analysis$cognition3 - mean_baseline) / sd_baseline

names(MHAS_analysis)
                                    
MHAS_analysis_converted <- MHAS_analysis[, c("age", "gender","education","highest_education_parent","marital_status","urbanicity","wealth_quartile" ,"bmi","smoke_2012" ,"drink_ever","restless","phy_act","social_act")]
set.seed(1005)
mice_mod <- mice(MHAS_analysis_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(MHAS_analysis), names(imputed_data))
MHAS_analysis[common_cols] <- imputed_data[common_cols]
