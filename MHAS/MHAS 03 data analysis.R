MHAS_analysis_long <- MHAS_analysis %>%
  pivot_longer(cols = c("wave3_z", "wave4_z","wave5_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave3_z" ~ 0,
    time == "wave4_z" ~ riwmid_w4 - riwmid_w3,
    time == "wave5_z" ~ riwmid_w5 - riwmid_w3,
    TRUE ~ NA_real_  
  ))

## hypertension 
MHAS_analysis_long$if_hyper <- ifelse(MHAS_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", "hypertensive")
set.seed(1005)
mixed2 <- lmer(cognition ~ if_hyper*time + age + gender + education +
                 marital_status  + urbanicity + wealth_quartile + 
                 bmi + smoke_2012 + drink_ever + restless + phy_act+social_act+
                 (1|np), data = MHAS_analysis_long)

## hypertension & anti treatment
MHAS_analysis_long$if_treat <- ifelse(MHAS_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", 
                                     ifelse(MHAS_analysis_long$ht_status2012 == "hypertensive, treated and controlled"|MHAS_analysis_long$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))
MHAS_analysis_long$if_treat <- factor(MHAS_analysis_long$if_treat, levels = c("non hypertensive","hypertensive and treated","hypertensive and untreated"))
MHAS_analysis_long$if_treat <- factor(MHAS_analysis_long$if_treat)

set.seed(1005)
mixed3 <- lmer(cognition ~ if_treat*time + age + gender + education +
                 marital_status + urbanicity + wealth_quartile + 
                 bmi + smoke_2012 + drink_ever + restless + phy_act +social_act+
                 (1|np), data = MHAS_analysis_long)


