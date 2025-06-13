CHARLS_analysis_long  <- CHARLS_analysis %>%
  pivot_longer(cols = c("wave1_z", "wave2_z","wave3_z","wave4_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave1_z" ~ 0,
    time == "wave2_z" ~ riwmid_w2 - riwmid_w1,
    time == "wave3_z" ~ riwmid_w3 - riwmid_w1,
    time == "wave4_z" ~ riwmid_w4 - riwmid_w1,
    TRUE ~ NA_real_  
  ))

#hypertension and treatment
mixed1 <- lmer(cognition ~ ht_status2011*time + age + gender + education +
                 marital_status + hukou + urbanicity + wealth_quartile + 
                 bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                 (1|ID), data = CHARLS_analysis_long)
summary(mixed1)

CHARLS_analysis_long$if_hyper <- ifelse(CHARLS_analysis_long$ht_status2011 == "non hypertensive", "non hypertensive", "hypertensive")
mixed2 <- lmer(cognition ~ if_hyper*time + age + gender + education +
                 marital_status + hukou + urbanicity + wealth_quartile + 
                 bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                 (1|ID), data = CHARLS_analysis_long)
summary(mixed2)

CHARLS_analysis_long$if_treat <- ifelse(CHARLS_analysis_long$ht_status2011 == "non hypertensive", "non hypertensive", 
                                     ifelse(CHARLS_analysis_long$ht_status2011 == "hypertensive, treated and controlled"|CHARLS_analysis_long$ht_status2011 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))
CHARLS_analysis_long$if_treat <- factor(CHARLS_analysis_long$if_treat,c("non hypertensive","hypertensive and treated","hypertensive and untreated"))
mixed3 <- lmer(cognition ~ if_treat*time + age + gender + education +
                 marital_status + hukou + urbanicity + wealth_quartile + 
                 bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                 (1|ID), data = CHARLS_analysis_long)
summary(mixed3)
