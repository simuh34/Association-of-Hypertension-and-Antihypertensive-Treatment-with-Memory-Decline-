elsa_analysis_long  <- elsa_analysis %>%
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

## hypertension 
elsa_analysis_long$if_hyper <- ifelse(elsa_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", "hypertensive")
table(elsa_analysis_long$if_hyper)
elsa_analysis_long$if_hyper <- factor(elsa_analysis_long$if_hyper)
elsa_analysis_long$if_hyper<- relevel(factor(elsa_analysis_long$if_hyper), ref = "non hypertensive")
mixed2 <- lmer(cognition ~ if_hyper*time + age + gender + high_edu +
                 marital_status + birthplace + race + wealth_quartile + 
                 bmi + smoke_2012 + drink + restless + phy_act+
                 (1|idauniqc), data = elsa_analysis_long)

summary(mixed2)

## hypertension & anti treatment
elsa_analysis_long$if_treat <- ifelse(elsa_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", 
                                     ifelse(elsa_analysis_long$ht_status2012 == "hypertensive, treated and controlled"|elsa_analysis_long$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))

elsa_analysis_long$if_treat <- factor(elsa_analysis_long$if_treat)
elsa_analysis_long$if_treat<- relevel(factor(elsa_analysis_long$if_treat), ref = "non hypertensive")
mixed3 <- lmer(cognition ~ if_treat*time + age + gender + high_edu +
                 marital_status + birthplace + race + wealth_quartile + 
                 bmi + smoke_2012 + drink + restless + phy_act+
                 (1|idauniqc), data = elsa_analysis_long)
summary(mixed3)
