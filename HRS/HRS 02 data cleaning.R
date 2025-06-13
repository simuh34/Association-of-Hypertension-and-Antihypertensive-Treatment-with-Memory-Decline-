library(psych)
library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(survey)
library(mice)
library(tidyr)

HRS_analysis <- read.csv("HRS_analysis.csv")
HRS_analysis <- filter(HRS_analysis,r10iwstat == 1 & r11iwstat == 1)
#n=19783

#0.combine wave 10 and 11
wave_10_clean <- HRS_analysis %>%
  filter(!is.na(r10systo)|!is.na(r10diasto))  %>%
  rename(hibpe = r10hibpe, systo = r10systo, diasto = r10diasto,age = r10agey_m,bmi = r10mbmi,urbanicity = h10rural,marital_st=r10mstat,wealth=h10atotb,people=h10hhres,smokev = r10smokev,smoken = r10smoken,drinkv = r10drink,restless = r10sleepr,vigorous = r10vgactx, moderate = r10mdactx,social = r10socwk) %>%
  mutate(wave = "r10")
length(wave_10_clean$hhidpn)

wave_11_clean <- HRS_analysis %>%
  filter(!is.na(r11systo)|!is.na(r11diasto)) %>%  
  rename(hibpe = r11hibpe, systo = r11systo, diasto = r11diasto,age = r11agey_m,bmi = r11mbmi,urbanicity = h11rural,marital_st=r11mstat,wealth=h11atotb,people=h11hhres,smokev = r11smokev,smoken = r11smoken,drinkv = r10drink,restless = r11sleepr,vigorous = r11vgactx, moderate = r11mdactx,social = r11socwk) %>%
  mutate(wave = "r11")
length(wave_11_clean$hhidpn)

combined_data <- bind_rows(wave_10_clean, wave_11_clean)
HRS_analysis <- combined_data

#define the sample
#1. must participate wave 10 and be r10agey_md 50+
summary(HRS_analysis$age)
HRS_analysis <- HRS_analysis[which(HRS_analysis$age>=50), ]
#hist(HRS_analysis$age)
#n=15886 in 2010

#2.do not have NA in gender
table(HRS_analysis$urbanicity, exclude = NULL)
table(HRS_analysis$ragender, exclude = NULL)

#3.check the NA and subset sample
library(naniar)
md1 <- HRS_analysis %>%
  miss_var_summary()
print(md1)

#have both blood pressure and hypertension question at wave 10
HRS_analysis <- HRS_analysis %>%
  filter(!is.na(hibpe) & # takes meds for high blood pressure 
           !is.na(systo) & 
           !is.na(diasto))
#n = 15886

#4.receode variabels
#ragender:1 is men; 2 is women
table(HRS_analysis$ragender, exclude = NULL)
HRS_analysis$ragender <- ifelse(HRS_analysis$ragender==1, "men", "women")
HRS_analysis$ragender <- factor(HRS_analysis$ragender)

#r10agey_m group
HRS_analysis$age_mcat <-ifelse(HRS_analysis$age >= 45 & 
                                  HRS_analysis$age <55, "age_m1",
                                ifelse(HRS_analysis$age >= 55 & 
                                         HRS_analysis$age <65, "age_m2", "age_m3"))
HRS_analysis$age_mcat <- factor(HRS_analysis$age_mcat, levels = c("age_m1", "age_m2", "age_m3"))
table(HRS_analysis$age_mcat, exclude = NULL)

#self edu:education, #1.Less than lower secondary, 2.upper secondary & vocational training, 3.tertiary
table(HRS_analysis$raeducl, exclude = NULL)

#highest parent edu: radadeducl,#radadeducl
table(HRS_analysis$radadeducl, HRS_analysis$radadeducl)
HRS_analysis$highest_education_parent<- ifelse(is.na(HRS_analysis$radadeducl), HRS_analysis$radadeducl,
                                                  ifelse(
                                                    is.na(HRS_analysis$radadeducl), HRS_analysis$radadeducl,
                                                    ifelse(HRS_analysis$radadeducl> HRS_analysis$radadeducl, HRS_analysis$radadeducl, HRS_analysis$radadeducl)
                                                  ))
table(HRS_analysis$highest_education_parent)

#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
HRS_analysis$marital_status<- ifelse(HRS_analysis$marital_st==1|HRS_analysis$marital_st==3, "married/partnered", "other")
table(HRS_analysis$marital_status, exclude = NULL)

#raracem and rahispan
#Generate a combined raracem: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
HRS_analysis$Race <- ifelse(HRS_analysis$raracem == 1 & 
                    HRS_analysis$rahispan == 0, 0,
                  ifelse(HRS_analysis$raracem == 2 & 
                           HRS_analysis$rahispan == 0, 1,
                         ifelse(HRS_analysis$rahispan == 1, 2, 3)))

HRS_analysis$Race <- ifelse(HRS_analysis$Race == 0, "Non-Hispanic White" , 
                  ifelse(HRS_analysis$Race == 1, "Non-Hispanic Black",
                         ifelse(HRS_analysis$Race == 2, "Hispanic","Other")))

table(HRS_analysis$Race)

#residence
HRS_analysis <- HRS_analysis %>%
  mutate(urbanicity = case_when(
    urbanicity == 0 ~ "urban",
    urbanicity == 1 ~ "rural",
    TRUE ~ NA_character_  # NA
  ))
table(HRS_analysis$urbanicity)
HRS_analysis$urbanicity <- factor(HRS_analysis$urbanicity)

#bmi, smoking
summary(HRS_analysis$bmi)
HRS_analysis$bmi[HRS_analysis$bmi > 60] <- NA 
#hist(HRS_analysis$bmi)

table(HRS_analysis$smoken, exclude = NULL)
table(HRS_analysis$smokev, exclude = NULL)
table(HRS_analysis$smoken, HRS_analysis$smokev)
HRS_analysis$smoke_2010 <-ifelse(HRS_analysis$smoken == 1, "current smoker",
                                    ifelse(HRS_analysis$smoken == 0 &
                                             HRS_analysis$smokev == 1, "past smoker", "non smoker"))
HRS_analysis$smoke_2010 <- factor(HRS_analysis$smoke_2010, levels = c("non smoker", "past smoker", "current smoker"))
table(HRS_analysis$smoke_2010)

#if drinking
table(HRS_analysis$drinkv)
HRS_analysis$drink_ever <- ifelse(HRS_analysis$drinkv == 1, 1, 0)
table(HRS_analysis$drink_ever, exclude = NULL)
prop.table(table(HRS_analysis$drink_ever))

#sleep 1.Rarely or none of the time < 1 day 2.Some or a little of the time 1-2 days 3.Occasionally or a moderate amount of 4.Most or all of the time 5-7 days
table(HRS_analysis$restless)
HRS_analysis$restless <- factor(ifelse(HRS_analysis$restless == 1, 0, ifelse(is.na(HRS_analysis$restless), HRS_analysis$restless, 1)))

#physical activity
#1.Every day 2.>1 per week 3.1 per week 4.l-3 per mon 5.Never 
table(HRS_analysis$vigorous, exclude = NULL)
table(HRS_analysis$moderate, exclude = NULL)
freq_map <- c("1" = 7,  # Every day = 7 days per week
              "2" = 2,  # >1 per week = >1 times per week, here considered as 2
              "3" = 1,  # 1 per week = 1 time per week
              "4" = 0.25, # l-3 per mon = 1-3 times per month, roughly 0.25 times per week
              "5" = 0)   # Never = 0 times per week
freq_moderate <- freq_map[as.character(HRS_analysis$moderate)]
freq_vigorous <- freq_map[as.character(HRS_analysis$vigorous)]
HRS_analysis <- HRS_analysis %>%
  mutate(phy_act = ifelse(freq_moderate + freq_vigorous > 2, 1, 0))
table(HRS_analysis$phy_act)

#social activities
table(HRS_analysis$social)
HRS_analysis <- HRS_analysis %>% 
  rename(social_act = social) %>%
  mutate(social_act = as.factor(social_act))

#wealth
#equivalized_wealth
HRS_analysis$people <- as.numeric(as.character(HRS_analysis$people))
HRS_analysis$equivalized_wealth <- HRS_analysis$wealth/sqrt(HRS_analysis$people)
HRS_analysis$equivalized_wealth <- round(HRS_analysis$equivalized_wealth/1000, 2)
summary(HRS_analysis$equivalized_wealth)
HRS_analysis$wealth_quartile <- cut(
  HRS_analysis$equivalized_wealth,
  breaks = quantile(HRS_analysis$equivalized_wealth, probs = c(0, 0.25,0.5, 0.75, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("1", "2", "3", "4")
)
table(HRS_analysis$wealth_quartile)

#5.code exposure: blood pressure, hypertension, and treatment
names(HRS_analysis)
table(HRS_analysis$hibpe)
#sbp, dbp, and taking medication
HRS_analysis$htn_2010 <- ifelse(HRS_analysis$systo >= 140 | 
                                  HRS_analysis$diasto >= 90 | 
                                  HRS_analysis$hibpe == 1, 1, 0)
table(HRS_analysis$htn_2010)

HRS_analysis$htn_2012 <- ifelse(HRS_analysis$r11systo >= 140 | 
                                     HRS_analysis$r11diasto >= 90 | 
                                     HRS_analysis$r11rxhibp == 1, 1, 0)
table(HRS_analysis$htn_2012, exclude = NULL)

HRS_analysis$htn_2014 <- ifelse(HRS_analysis$r12systo >= 140 | 
                                     HRS_analysis$r12diasto >= 90 | 
                                     HRS_analysis$r12rxhibp == 1, 1, 0)
table(HRS_analysis$htn_2014)

HRS_analysis$htn_2016 <- ifelse(HRS_analysis$r13systo >= 140 | 
                                     HRS_analysis$r13diasto >= 90 | 
                                     HRS_analysis$r13rxhibp == 1, 1, 0)
table(HRS_analysis$htn_2016)

HRS_analysis$hibpe[is.na(HRS_analysis$hibpe)] <- 0
HRS_analysis$ht_status2010 <- ifelse(HRS_analysis$htn_2010 == 0, "non hypertensive",
                                        ifelse(HRS_analysis$htn_2010 == 1 &
                                                 HRS_analysis$hibpe == 0, "hypertensive and untreated",
                                               ifelse(HRS_analysis$htn_2010 == 1 &
                                                        HRS_analysis$hibpe== 1 &
                                                        HRS_analysis$systo < 140 &
                                                        HRS_analysis$diasto < 90, "hypertensive, treated and controlled",
                                                      "hypertensive, treated but uncontrolled")))
HRS_analysis$ht_status2010 <- factor(HRS_analysis$ht_status2010, levels = c("hypertensive and untreated", 
                                                                                  "hypertensive, treated but uncontrolled", 
                                                                                  "hypertensive, treated and controlled",
                                                                                  "non hypertensive"))
table(HRS_analysis$ht_status2010, exclude = NULL)
HRS_analysis <- HRS_analysis[complete.cases(HRS_analysis$htn_2010), ]
# n = 15221

#6. cognition difference 
# standardize cognition score
HRS_analysis$wave1_z <- scale(HRS_analysis$r10tr20)
mean_baseline <- mean(HRS_analysis$r10tr20, na.rm = TRUE)
sd_baseline <- sd(HRS_analysis$r10tr20, na.rm = TRUE)
# cognition2 z-score
HRS_analysis$wave2_z <- (HRS_analysis$r11tr20 - mean_baseline) / sd_baseline
# cognition3 z-score
HRS_analysis$wave3_z <- (HRS_analysis$r12tr20 - mean_baseline) / sd_baseline
# cognition4 z-score
HRS_analysis$wave4_z <- (HRS_analysis$r13tr20 - mean_baseline) / sd_baseline
# cognition5 z-score
HRS_analysis$wave5_z <- (HRS_analysis$r14tr20 - mean_baseline) / sd_baseline

#extract complete data
write.csv(HRS_analysis, "HRS_df_complete.csv")

####covariates missingness####
# Step 1: Define covariates
covariates <- c("age","ragender","raeducl","marital_status", "urbanicity","Race","bmi", "smoke_2010", "drink_ever",'restless',"phy_act","social_act", "wealth_quartile")

# Step 2: Check the missing data pattern
md.pattern(HRS_analysis[covariates])

# Step 3: Simple missing data visualization

aggr_plot <- aggr(HRS_analysis[covariates], col=c('navyblue','red3'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(HRS_analysis[covariates]),
                  cex.axis=.7, gap=3,
                  ylab=c("Missing data","Pattern"))

#Step 4: Get missingness percentage
missing_summary <- sapply(HRS_analysis[covariates], function(x) mean(is.na(x)) * 100)
print(round(missing_summary, 2))

#impute covariates
HRS_analysis_converted <- HRS_analysis[, c("ragender","age","bmi","raeducl","highest_education_parent","marital_status", "Race", "urbanicity","wealth_quartile", "smoke_2010", "drink_ever",'restless',"phy_act","social_act")]
classes <- sapply(HRS_analysis_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
HRS_analysis_converted <- HRS_analysis_converted %>%
  mutate_if(names(.) %in% c("ragender","raeducl","highest_education_parent","marital_status","Race","urbanicity","smoke_2010" ,"drink_ever","restless","social_act","wealth_quartile","phy_act"), as.factor)
set.seed(1005)
mice_mod <- mice(HRS_analysis_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(HRS_analysis), names(imputed_data))
HRS_analysis[common_cols] <- imputed_data[common_cols]

#extract data
write.csv(HRS_analysis, "HRS_analysis_df.csv")
