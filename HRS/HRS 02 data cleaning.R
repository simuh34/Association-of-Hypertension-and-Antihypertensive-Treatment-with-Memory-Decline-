HRS_analysis <- HRS_analysis[which(HRS_analysis$age>=50), ]

HRS_analysis <- HRS_analysis %>%
  filter(!is.na(hibpe) & # takes meds for high blood pressure 
           !is.na(systo) & 
           !is.na(diasto))

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

HRS_analysis$smoke_2010 <-ifelse(HRS_analysis$smoken == 1, "current smoker",
                                    ifelse(HRS_analysis$smoken == 0 &
                                             HRS_analysis$smokev == 1, "past smoker", "non smoker"))
#wealth
#equivalized_wealth
HRS_analysis$people <- as.numeric(as.character(HRS_analysis$people))
HRS_analysis$equivalized_wealth <- HRS_analysis$wealth/sqrt(HRS_analysis$people)
HRS_analysis$equivalized_wealth <- round(HRS_analysis$equivalized_wealth/1000, 2)
summary(HRS_analysis$equivalized_wealth)
HRS_analysis$wealth_quartile <- cut(
  HRS_analysis$equivalized_wealth,
  breaks = quantile(HRS_analysis$equivalized_wealth, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("1", "2", "3", "4")
)
table(HRS_analysis$wealth_quartile)

#sbp, dbp, and taking medication
HRS_analysis$htn_2010 <- ifelse(HRS_analysis$systo >= 140 | 
                                  HRS_analysis$diasto >= 90 | 
                                  HRS_analysis$hibpe == 1, 1, 0)

HRS_analysis$wave1_z <- scale(HRS_analysis$r10tr20)
mean_baseline <- mean(HRS_analysis$r10tr20, na.rm = TRUE)
sd_baseline <- sd(HRS_analysis$r10tr20, na.rm = TRUE)
HRS_analysis$wave2_z <- (HRS_analysis$r11tr20 - mean_baseline) / sd_baseline
HRS_analysis$wave3_z <- (HRS_analysis$r12tr20 - mean_baseline) / sd_baseline
HRS_analysis$wave4_z <- (HRS_analysis$r13tr20 - mean_baseline) / sd_baseline
HRS_analysis$wave5_z <- (HRS_analysis$r14tr20 - mean_baseline) / sd_baseline

HRS_analysis_converted <- HRS_analysis[, c("ragender","age","bmi","raeducl","highest_education_parent","marital_status", "Race", "urbanicity","wealth_quartile", "smoke_2010", "drink_ever",'restless',"phy_act","social_act")]
set.seed(1005)
mice_mod <- mice(HRS_analysis_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(HRS_analysis), names(imputed_data))
HRS_analysis[common_cols] <- imputed_data[common_cols]
