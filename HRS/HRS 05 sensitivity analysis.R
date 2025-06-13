#1.ipcw
model_ipcw_1 <- lmer(cognition ~ if_hyper*time + age + ragender + raeducl +highest_education_parent+
                       marital_status + Race + urbanicity  + wealth_quartile+social_act+
                       bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), 
                       weights = ipcw_cum_inw13,
                       data = HRS_analysis_long)
model_ipcw_2 <- lmer(cognition ~ if_treat*time + age + ragender + raeducl +highest_education_parent+
                       marital_status + Race + urbanicity  + wealth_quartile+social_act+
                       bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), 
                       weights = ipcw_cum_inw13,
                       data = HRS_analysis_long)
model_ipcw_3 <- lmer(cognition ~ ht_status2010*time + age + ragender + raeducl +highest_education_parent+
                       marital_status + Race + urbanicity  + wealth_quartile+social_act+
                       bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), 
                       weights = ipcw_cum_inw13,
                       data = HRS_analysis_long)

#2. complete case analysis
HRS_analysis_complete_long  <- HRS_analysis_complete %>%
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

HRS_analysis_complete_long$if_hyper <- ifelse(HRS_analysis_complete_long$ht_status2010 == 
                                       "non hypertensive", 
                                     "non hypertensive", 
                                     "hypertensive")

model_1 <- lmer(cognition ~ if_hyper*time + age + ragender + raeducl +highest_education_parent+
                  marital_status + Race + urbanicity  + wealth_quartile+social_act+
                  bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), 
                  data = HRS_analysis_complete_long)

model_2 <- lmer(cognition ~ if_treat*time + age + ragender + raeducl +highest_education_parent+marital_status + Race + urbanicity  + wealth_quartile+social_act+
                  bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), 
                data = HRS_analysis_complete_long)

#3. sampling weighting
model_1 <- lmer(cognition ~ if_hyper*time + age + ragender + raeducl +highest_education_parent+
                  marital_status + Race + urbanicity  + wealth_quartile+social_act+
                  bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn),  
                     weights = r10wtresp,
                     data = HRS_analysis_long)
model_2 <- lmer(cognition ~ if_treat*time + age + ragender + raeducl +highest_education_parent+
                  marital_status + Race + urbanicity  + wealth_quartile+social_act+
                  bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), 
                     weights = r10wtresp,
                     data = HRS_analysis_long)
