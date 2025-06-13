library(sjPlot)
library(lme4)

CHARLS_analysis_long <- read.csv("CHARLS_analysis_long.csv")
CHARLS_analysis_long$ht_status2011 <- factor(CHARLS_analysis_long$ht_status2011, levels = c("non hypertensive",
                                                                                            "hypertensive, treated and controlled",
                                                                                            "hypertensive, treated but uncontrolled", 
                                                                                            "hypertensive and untreated" ))
CHARLS_analysis_long$if_hyper <- factor(CHARLS_analysis_long$if_hyper, levels = c("non hypertensive","hypertensive" ))
CHARLS_analysis_long$if_treat <- factor(CHARLS_analysis_long$if_treat, levels = c("non hypertensive", 
                                                                                  "hypertensive and treated",
                                                                                  "hypertensive and untreated" ))
####interaction####
#1. hypertension status
inter_gender_1 <- lmer(cognition ~ if_hyper*time*gender + age + education + highest_education_parent+
                       marital_status + hukou + urbanicity + wealth_quartile + 
                       bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                       (1|ID), data = CHARLS_analysis_long)

#2. treatment
inter_gender_2 <- lmer(cognition ~ if_treat*time*gender + age + education + highest_education_parent+
                         marital_status + hukou + urbanicity + wealth_quartile + 
                         bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                         (1|ID), data = CHARLS_analysis_long)

tab_model(inter_gender_1, inter_gender_2)

