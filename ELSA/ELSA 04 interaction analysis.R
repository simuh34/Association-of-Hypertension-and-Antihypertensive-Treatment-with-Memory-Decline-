inter_gender_1 <- lmer(cognition ~ if_hyper*time*gender + age + high_edu +
                       marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                       (1|idauniqc), data = elsa_analysis_long)

inter_gender_2 <- lmer(cognition ~ if_treat*time*gender + age + high_edu +
                         marital_status + race + birthplace + wealth_quartile + 
                         bmi + smoke_2012 + drink + restless + phy_act +
                         (1|idauniqc), data = elsa_analysis_long)
