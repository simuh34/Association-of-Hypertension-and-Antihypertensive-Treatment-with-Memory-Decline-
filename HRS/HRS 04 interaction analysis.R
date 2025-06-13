inter_gender_1 <- lmer(cognition ~ if_hyper*time*ragender + age + raeducl +highest_education_parent+
                         marital_status + Race + urbanicity  + wealth_quartile+social_act+
                         bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)

inter_gender_2 <- lmer(cognition ~ if_treat*time*ragender + age + raeducl +highest_education_parent+
                         marital_status + Race + urbanicity  + wealth_quartile+social_act+
                         bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)






