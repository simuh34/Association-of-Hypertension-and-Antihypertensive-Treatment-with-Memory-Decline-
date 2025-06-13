#1.ipcw
model_ipcw_1 <- lmer(cognition ~ if_hyper*time + gender + age + high_edu +
                                       marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                                       (1|idauniqc), weights = ipcw_cum_inw9, data = elsa_analysis_long)
model_ipcw_2 <- lmer(cognition ~ if_treat*time + gender + age + high_edu +
                                      marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                                       (1|idauniqc), weights = ipcw_cum_inw9, data = elsa_analysis_long)

# need to extract complete data before covariates imputation (in file 2 data cleaning (around elsa 02 - line 215))
elsa_analysis_complete_long  <- elsa_analysis_complete %>%
  pivot_longer(cols = c("wave6_z", "wave7_z","wave8_z","wave9_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave6_z" ~ 0,
    time == "wave7_z" ~ riwmid_w7 - riwmid_w6,
    time == "wave8_z" ~ riwmid_w8 - riwmid_w6,
    time == "wave9_z" ~ riwmid_w9 - riwmid_w6,
    TRUE ~ NA_real_  
  ))

elsa_analysis_complete_long$if_hyper <- ifelse(elsa_analysis_complete_long$ht_status2012 == "non hypertensive", 
                                                                              "non hypertensive","hypertensive")
elsa_analysis_complete_long$if_hyper <- factor(elsa_analysis_complete_long$if_hyper,c("non hypertensive",
                                                                        "hypertensive"))

elsa_analysis_complete_long$if_treat <- ifelse(elsa_analysis_complete_long$ht_status2012 == "non hypertensive", 
                                                                              "non hypertensive", 
                  ifelse(elsa_analysis_complete_long$ht_status2012 == "hypertensive, treated and controlled"|
                           elsa_analysis_complete_long$ht_status2012 == "hypertensive, 
                                                                  treated but uncontrolled", 
                                                                  "hypertensive and treated","hypertensive and untreated"))
elsa_analysis_complete_long$if_treat <- factor(elsa_analysis_complete_long$if_treat,c("non hypertensive",
                                                                        "hypertensive and treated",
                                                                        "hypertensive and untreated"))

#1. hypertension status
model_1 <- lmer(cognition ~ if_hyper*time + gender + age + high_edu +
                         marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                                       (1|idauniqc), data = elsa_analysis_complete_long)

#2. treatment
model_2 <- lmer(cognition ~ if_treat*time + gender + age + high_edu +
                         marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                                       (1|idauniqc), data = elsa_analysis_complete_long)

#3.sampling weight
model_1 <- lmer(cognition ~ if_hyper*time + gender + age + high_edu +
                  marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                  (1|idauniqc), weights = r6lwtresp,  data = elsa_analysis_long)

#2. treatment
model_2 <- lmer(cognition ~ if_treat*time + gender + age + high_edu +
                  marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                  (1|idauniqc), weights = r6lwtresp, data = elsa_analysis_long)

