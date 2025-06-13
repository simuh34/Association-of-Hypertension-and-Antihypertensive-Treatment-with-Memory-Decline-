library(sjPlot)
library(lme4)
library(patchwork)
library(ggplot2)

HRS_analysis_long <- read.csv("D:\\R project\\Hypertension\\HRS\\HRS_analysis_long.csv")
HRS_analysis_long$ht_status2010 <- factor(HRS_analysis_long$ht_status2010, levels = c("non hypertensive",
                                                                                      "hypertensive, treated and controlled",
                                                                                      "hypertensive, treated but uncontrolled",
                                                                                      "hypertensive and untreated" ))
HRS_analysis_long$if_hyper <- factor(HRS_analysis_long$if_hyper, levels = c("non hypertensive",
                                                                            "hypertensive" ))
HRS_analysis_long$if_treat <- factor(HRS_analysis_long$if_treat, levels = c("non hypertensive", 
                                                                            "hypertensive and treated",
                                                                            "hypertensive and untreated" ))

####interaction####
#1. hypertension status
inter_gender_1 <- lmer(cognition ~ if_hyper*time*ragender + age + raeducl +highest_education_parent+
                         marital_status + Race + urbanicity  + wealth_quartile+social_act+
                         bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)
#2. treatment
inter_gender_2 <- lmer(cognition ~ if_treat*time*ragender + age + raeducl +highest_education_parent+
                         marital_status + Race + urbanicity  + wealth_quartile+social_act+
                         bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)

tab_model(inter_gender_1, inter_gender_2)





