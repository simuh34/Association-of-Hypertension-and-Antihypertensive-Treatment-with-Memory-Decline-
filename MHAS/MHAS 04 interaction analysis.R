library(sjPlot)
library(lme4)

MHAS_analysis_long <- read.csv("MHAS_long.csv")
MHAS_analysis_long$ht_status2012 <- factor(MHAS_analysis_long$ht_status2012, levels = c("non hypertensive",
                                                                                            "hypertensive, treated and controlled",
                                                                                            "hypertensive, treated but uncontrolled", 
                                                                                            "hypertensive and untreated" ))
MHAS_analysis_long$if_hyper <- factor(MHAS_analysis_long$if_hyper, levels = c("non hypertensive","hypertensive" ))
MHAS_analysis_long$if_treat <- factor(MHAS_analysis_long$if_treat, levels = c("non hypertensive", 
                                                                                  "hypertensive and treated",
                                                                                  "hypertensive and untreated" ))
####interaction####
#1. hypertension status
inter_gender_1 <- lmer(cognition ~ if_hyper*time*gender + age + education +highest_education_parent+
                       marital_status + urbanicity + wealth_quartile + 
                       bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                       (1|np), data = MHAS_analysis_long)

#2. treatment
inter_gender_2 <- lmer(cognition ~ if_treat*time*gender + age + education +highest_education_parent+
                         marital_status + urbanicity + wealth_quartile + 
                         bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                         (1|np), data = MHAS_analysis_long)

tab_model(inter_gender_1, inter_gender_2)


