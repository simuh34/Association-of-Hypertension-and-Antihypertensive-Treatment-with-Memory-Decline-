#1.ipcw-censoring
#censoring selection bias 
#use interview status variables in each wave
CHARLS_analysis_long <- CHARLS_analysis_long %>%
  mutate(
    
    death_wave2 = ifelse(r2iwstat == 5, 1, ifelse(r2iwstat < 5, 0, NA)),
    death_wave3 = ifelse(r3iwstat == 5, 1, ifelse(r3iwstat < 5, 0, NA)),
    death_wave4 = ifelse(r4iwstat == 5, 1, ifelse(r4iwstat < 5, 0, NA)),
    
    lost_wave2 = ifelse(r2iwstat == 9 | r2iwstat == 7 | r2iwstat == 4, 1, ifelse(r2iwstat == 1, 0, NA)),
    lost_wave3 = ifelse(r3iwstat == 9 | r3iwstat == 7 | r3iwstat == 4, 1, ifelse(r3iwstat == 1, 0, NA)),
    lost_wave4 = ifelse(r4iwstat == 9 | r4iwstat == 7 | r4iwstat == 4, 1, ifelse(r4iwstat == 1, 0, NA))
  )

for (wave in 2:4) {
  death_var <- paste0("death_wave", wave)
  lost_var <- paste0("lost_wave", wave)
  
  model_death <- glm(as.formula(paste(death_var, "~ age + education + highest_education_parent+
                       marital_status + hukou + urbanicity + wealth_quartile + 
                       bmi + smoke_2011 + drink_ever + restless + phy_act + social_act")),
                     data = CHARLS_analysis_long, 
                     family = binomial, 
                     control = glm.control(maxit = 1000), 
                     na.action = na.exclude)  
  
  model_lost <- glm(as.formula(paste(lost_var, "~ age + education + highest_education_parent+
                       marital_status + hukou + urbanicity + wealth_quartile + 
                       bmi + smoke_2011 + drink_ever + restless + phy_act + social_act")),
                    data = CHARLS_analysis_long, 
                    family = binomial, 
                    control = glm.control(maxit = 1000),
                    na.action = na.exclude)  
  
  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")
  
  if (length(pred_death) == nrow(CHARLS_analysis_long) && length(pred_lost) == nrow(CHARLS_analysis_long)) {
    CHARLS_analysis_long <- CHARLS_analysis_long %>%
      mutate(!!paste0("prob_alive_inw", wave) := (1 - pred_death) * (1 - pred_lost))
  } else {
    warning("Prediction length mismatch for wave", wave)
  }
}

for (wave in 2:4) {
  prob_var <- paste0("prob_alive_inw", wave)
  ipcw_var <- paste0("ipcw_inw", wave)
  
  CHARLS_analysis_long <- CHARLS_analysis_long %>%
    mutate(!!ipcw_var := 1 / get(prob_var))  
}

#cumulative censoring weight
CHARLS_analysis_long <- CHARLS_analysis_long %>%
  mutate(
    ipcw_cum_inw2 = ipcw_inw2,
    ipcw_cum_inw3 = ifelse(is.na(ipcw_inw2), NA, ipcw_inw3 * ipcw_inw2),
    ipcw_cum_inw4 = ifelse(is.na(ipcw_cum_inw3), NA, ipcw_inw4 * ipcw_cum_inw3)
  )

CHARLS_analysis_long %>%
  select(ipcw_cum_inw2, ipcw_cum_inw3, ipcw_cum_inw4) %>%
  summary()

model_ipcw_1 <- lmer(cognition ~ if_hyper*time + gender + age + education + highest_education_parent+
                                       marital_status + hukou + urbanicity + wealth_quartile + 
                                       bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                                       (1|ID), weights = ipcw_cum_inw4, data = CHARLS_analysis_long)
model_ipcw_2 <- lmer(cognition ~ if_treat*time + gender + age + education + highest_education_parent+
                                      marital_status + hukou + urbanicity + wealth_quartile + 
                                      bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                                     (1|ID), weights = ipcw_cum_inw4, data = CHARLS_analysis_long)

#2. complete case analysis
CHARLS_analysis_complete_long  <- CHARLS_analysis_complete %>%
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

#1. hypertension status
model_1 <- lmer(cognition ~ if_hyper*time + gender + age + education + highest_education_parent+
                         marital_status + hukou + urbanicity + wealth_quartile + 
                         bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                         (1|ID), data = CHARLS_analysis_complete_long)

#2. treatment
model_2 <- lmer(cognition ~ if_treat*time + gender + age + education + highest_education_parent+
                         marital_status + hukou + urbanicity + wealth_quartile + 
                         bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                         (1|ID), data = CHARLS_analysis_complete_long)

#3.sampling weight
#1. hypertension status
model_1 <- lmer(cognition ~ if_hyper*time + gender + age + education + highest_education_parent+
                  marital_status + hukou + urbanicity + wealth_quartile + 
                  bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                  (1|ID), weights = r1wtresp,  data = CHARLS_analysis_long)

#2. treatment
model_2 <- lmer(cognition ~ if_treat*time + gender + age + education + highest_education_parent+
                  marital_status + hukou + urbanicity + wealth_quartile + 
                  bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                  (1|ID), weights = r1wtresp, data = CHARLS_analysis_long)

