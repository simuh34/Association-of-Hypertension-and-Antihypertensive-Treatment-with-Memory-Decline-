#must participate wave 1 and be aged 45+ at Wave 1
summary(CHARLS_analysis$age)
CHARLS_analysis <- CHARLS_analysis[which(CHARLS_analysis$age>=50), ]

#have both blood pressure and hypertension question at wave 1
CHARLS_analysis <- CHARLS_analysis[-which(is.na(CHARLS_analysis$r1hibpe) |
                                          is.na(CHARLS_analysis$r1systo) |
                                          is.na(CHARLS_analysis$r1diasto)), ]

#self edu:education, #1.Less than lower secondary, 2.upper secondary & vocational training, 3.tertiary
table(CHARLS_analysis$education, exclude = NULL)

#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
CHARLS_analysis[, 16:19] <- lapply(CHARLS_analysis[, 16:19], function(x) ifelse(x==1|x==3, "married/partnered", "other"))
CHARLS_analysis[, 16:19] <- lapply(CHARLS_analysis[, 16:19], function(x) factor(x, levels = c("married/partnered", "other")))
table(CHARLS_analysis$marital_status, exclude = NULL)

#hukou: 1.Agricultual hukou; 2.Non-agricultural hukou; 3.Unified residence hukou; 4.Do not have hukou
table(CHARLS_analysis$hukou, exclude = NULL)
CHARLS_analysis[, c(20:23)] <- lapply(CHARLS_analysis[, c(20:23)], function(x) ifelse(x==1 | x==4, "agricultural", "non agricultural"))
CHARLS_analysis[, c(20:23)] <- lapply(CHARLS_analysis[, c(20:23)], function(x) factor(x, levels = c("agricultural", "non agricultural")))

#residence
CHARLS_analysis[, c(24:27)] <- lapply(CHARLS_analysis[, c(24:27)], function(x) ifelse(x==1, "rural", "urban"))
CHARLS_analysis[, c(24:27)] <- lapply(CHARLS_analysis[, c(24:27)], function(x) factor(x, levels = c("rural", "urban")))
table(CHARLS_analysis$urbanicity)

#bmi, smoking
summary(CHARLS_analysis$bmi)
CHARLS_analysis$bmi[CHARLS_analysis$bmi > 60] <- NA 
hist(CHARLS_analysis$bmi)

table(CHARLS_analysis$r1smoken, exclude = NULL)
CHARLS_analysis$r1smoken[is.na(CHARLS_analysis$r1smoken)] <- 0
table(CHARLS_analysis$r1smokev, exclude = NULL)
CHARLS_analysis$r1smokev[is.na(CHARLS_analysis$r1smokev)] <- 0
table(CHARLS_analysis$r1smoken, CHARLS_analysis$r1smokev)
CHARLS_analysis$smoke_2011 <-ifelse(CHARLS_analysis$r1smoken == 1, "current smoker",
                                  ifelse(CHARLS_analysis$r1smoken == 0 &
                                         CHARLS_analysis$r1smokev == 1, "past smoker", "non smoker"))
CHARLS_analysis$smoke_2011 <- factor(CHARLS_analysis$smoke_2011, levels = c("non smoker", "past smoker", "current smoker"))
table(CHARLS_analysis$smoke_2011)

#if drinking
table(CHARLS_analysis$r1drinkr_c)
CHARLS_analysis$drink_ever <- ifelse(CHARLS_analysis$r1drinkr_c>0, 1, 0)
table(CHARLS_analysis$drink_ever, exclude = NULL)
prop.table(table(CHARLS_analysis$drink_ever))

#sleep 1.Rarely or none of the time < 1 day 2.Some or a little of the time 1-2 days 3.Occasionally or a moderate amount of 4.Most or all of the time 5-7 days
table(CHARLS_analysis$r1sleeprl)
CHARLS_analysis$restless <- factor(ifelse(CHARLS_analysis$r1sleeprl == 1, 0, ifelse(is.na(CHARLS_analysis$r1sleeprl), CHARLS_analysis$r1sleeprl, 1)))

#social activities
table(CHARLS_analysis$r1socwk)
CHARLS_analysis <- CHARLS_analysis %>% 
  rename(social_act = r1socwk) %>%
  mutate(social_act = as.factor(social_act))

#equivalized_wealth
CHARLS_analysis$h1hhres <- as.numeric(as.character(CHARLS_analysis$h1hhres))
CHARLS_analysis$equivalized_wealth <- CHARLS_analysis$wealth/sqrt(CHARLS_analysis$h1hhres)
CHARLS_analysis$equivalized_wealth <- round(CHARLS_analysis$equivalized_wealth/1000, 2)
summary(CHARLS_analysis$equivalized_wealth)
CHARLS_analysis$wealth_quartile <- cut(
  CHARLS_analysis$equivalized_wealth,
  breaks = quantile(CHARLS_analysis$equivalized_wealth, probs = c(0,0.25, 0.5,0.75, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("1", "2","3","4")
)
table(CHARLS_analysis$wealth_quartile)

CHARLS_analysis$htn_2011<- ifelse(CHARLS_analysis$r1systo >= 140|
                                  CHARLS_analysis$r1diasto>=90|
                                (!is.na(CHARLS_analysis$r1rxhibp)& CHARLS_analysis$r1rxhibp == 1),
                                1,0)

CHARLS_analysis$r1rxhibp_c[is.na(CHARLS_analysis$r1rxhibp_c)] <- 0
CHARLS_analysis$ht_status2011 <- ifelse(CHARLS_analysis$htn_2011 == 0, "non hypertensive",
                                        ifelse(CHARLS_analysis$htn_2011 == 1 &
                                               CHARLS_analysis$r1rxhibp_c == 0, "hypertensive and untreated",
                                               ifelse(CHARLS_analysis$htn_2011 == 1 &
                                                      CHARLS_analysis$r1rxhibp_c == 1 &
                                                      CHARLS_analysis$r1systo < 140 &
                                                      CHARLS_analysis$r1diasto < 90, "hypertensive, treated and controlled",
                                                      "hypertensive, treated but uncontrolled")))
CHARLS_analysis$ht_status2011 <- factor(CHARLS_analysis$ht_status2011, levels = c("hypertensive and untreated", 
                                                                                  "hypertensive, treated but uncontrolled", 
                                                                                  "hypertensive, treated and controlled",
                                                                                  "non hypertensive"))

CHARLS_analysis$ht_status2013 <- ifelse(CHARLS_analysis$htn_2013 == 0, "non hypertensive",
                                        ifelse(CHARLS_analysis$htn_2013 == 1 &
                                               CHARLS_analysis$r2rxhibp_c == 0, "hypertensive and untreated",
                                               ifelse(CHARLS_analysis$htn_2013 == 1 &
                                                      CHARLS_analysis$r2rxhibp_c == 1 &
                                                      CHARLS_analysis$r2systo < 140 &
                                                      CHARLS_analysis$r2diasto < 90, "hypertensive, treated and controlled",
                                                      "hypertensive, treated but uncontrolled")))
CHARLS_analysis$ht_status2013 <- factor(CHARLS_analysis$ht_status2013, levels = c("hypertensive and untreated", 
                                                                                  "hypertensive, treated but uncontrolled", 
                                                                                  "hypertensive, treated and controlled",
                                                                                  "non hypertensive"))
                                      
CHARLS_analysis$ht_status2015 <- ifelse(CHARLS_analysis$htn_2015 == 0, "non hypertensive",
                                        ifelse(CHARLS_analysis$htn_2015 == 1 &
                                               CHARLS_analysis$r3rxhibp_c == 0, "hypertensive and untreated",
                                               ifelse(CHARLS_analysis$htn_2015 == 1 &
                                                      CHARLS_analysis$r3rxhibp_c == 1 &
                                                      CHARLS_analysis$r3systo < 140 &
                                                      CHARLS_analysis$r3diasto < 90, "hypertensive, treated and controlled",
                                                      "hypertensive, treated but uncontrolled")))
CHARLS_analysis$ht_status2015 <- factor(CHARLS_analysis$ht_status2015, levels = c("hypertensive and untreated", 
                                                                                  "hypertensive, treated but uncontrolled", 
                                                                                  "hypertensive, treated and controlled",
                                                                                  "non hypertensive"))
                                      
CHARLS_analysis$wave1_z <- scale(CHARLS_analysis$cognition1)
mean_baseline <- mean(CHARLS_analysis$cognition1, na.rm = TRUE)
sd_baseline <- sd(CHARLS_analysis$cognition1, na.rm = TRUE)
# cognition2 z-score
CHARLS_analysis$wave2_z <- (CHARLS_analysis$cognition2 - mean_baseline) / sd_baseline
# cognition3 z-score
CHARLS_analysis$wave3_z <- (CHARLS_analysis$cognition3 - mean_baseline) / sd_baseline
# cognition4 z-score
CHARLS_analysis$wave4_z <- (CHARLS_analysis$cognition4 - mean_baseline) / sd_baseline

CHARLS_analysis_converted <- CHARLS_analysis[, c("age", "gender","education","highest_education_parent","marital_status","hukou","urbanicity","consumption" ,"bmi","smoke_2011" ,"drink_ever","restless","phy_act","social_act","wealth_quartile")]
classes <- sapply(CHARLS_analysis_converted, class)
mice_mod <- mice(CHARLS_analysis_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(CHARLS_analysis), names(imputed_data))
CHARLS_analysis[common_cols] <- imputed_data[common_cols]
