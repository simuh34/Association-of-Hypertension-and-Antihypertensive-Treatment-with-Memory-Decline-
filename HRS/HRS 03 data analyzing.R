HRS_analysis_long  <- HRS_analysis %>%
  pivot_longer(cols = c("wave1_z", "wave2_z","wave3_z","wave4_z","wave5_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave1_z" ~ 0,
    time == "wave2_z" ~ riwmid_w2 - riwmid_w1,
    time == "wave3_z" ~ riwmid_w3 - riwmid_w1,
    time == "wave4_z" ~ riwmid_w4 - riwmid_w1,
    time == "wave5_z" ~ riwmid_w5 - riwmid_w1,
    TRUE ~ NA_real_  
  ))

mixed1 <- lmer(cognition ~ ht_status2010*time + age + ragender + raeducl +
                 marital_status + Race + urbanicity  + wealth_quartile+social_act+
                 bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)
summary(mixed1)

mixed1 <- lmer(cognition ~ if_treat*time + age + ragender + raeducl +highest_education_parent+
                 marital_status + Race + urbanicity  + wealth_quartile+social_act+
                 bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)
summary(mixed1)





