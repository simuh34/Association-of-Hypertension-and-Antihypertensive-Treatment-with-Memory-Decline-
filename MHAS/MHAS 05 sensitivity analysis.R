#1.ipcw
model_ipcw_1 <- lmer(cognition ~ if_hyper*time + gender + age + education +high_edu_parent+
                                       marital_status + urbanicity + wealth_quartile + 
                                       bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                                       (1|np), weights = ipcw_cum_inw4, data = MHASc_analysis_long)
model_ipcw_2 <- lmer(cognition ~ if_treat*time + gender + age + education +high_edu_parent+
                                      marital_status + urbanicity + wealth_quartile + 
                                      bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                                     (1|np), weights = ipcw_cum_inw4, data = MHASc_analysis_long)

#2. complete case analysis
MHAS_analysis_complete_long  <- MHAS_analysis_complete %>%
  pivot_longer(cols = c("wave3_z", "wave4_z","wave5_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave3_z" ~ 0,
    time == "wave4_z" ~ riwmid_w4 - riwmid_w3,
    time == "wave5_z" ~ riwmid_w5 - riwmid_w3,
    TRUE ~ NA_real_  
  ))

#hypertension status
model_1_complete_case <- lmer(cognition ~ if_hyper*time + gender + age + high_edu +high_edu_parent+
                         marital_status + urbanicity + wealth_quartile + 
                         bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                         (1|np), data = MHAS_analysis_complete_long)

#treatment
model_2_complete_case <- lmer(cognition ~ if_treat*time + gender + age + high_edu +high_edu_parent+
                         marital_status + urbanicity + wealth_quartile + 
                         bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                         (1|np), data = MHAS_analysis_complete_long)

#3.sampling weight
MHASc_analysis_long <- MHASc_analysis_long[which(!is.na(MHASc_analysis_long$r3wtresp)), ]
MHASc_analysis_long$r3wtresp <- MHASc_analysis_long$r3wtresp/sum(MHASc_analysis_long[, 'r3wtresp'])*5766

#1. hypertension status
model_1_sampling_weight <- lmer(cognition ~ if_hyper*time + gender + age +education +high_edu_parent+
                  marital_status + urbanicity + wealth_quartile + 
                  bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                  (1|np), weights = r3wtresp,  data = MHASc_analysis_long, control = lmerControl(optCtrl = list(maxfun = 100000)))

#2. treatment
model_2_sampling_weight <- lmer(cognition ~ if_treat*time + gender + age + education +high_edu_parent+
                  marital_status + urbanicity + wealth_quartile + 
                  bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                  (1|np), weights = r3wtresp, data = MHASc_analysis_long)
