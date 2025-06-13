#' elsa
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(sjPlot)
library(lme4)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis_long <- read.csv("elsa_analysis_long.csv")

#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis_long$ht_status2012 <- factor(elsa_analysis_long$ht_status2012, levels = c("non hypertensive",
                                                                                            "hypertensive, treated and controlled",
                                                                                            "hypertensive, treated but uncontrolled", 
                                                                                            "hypertensive and untreated" ))
elsa_analysis_long$if_hyper <- factor(elsa_analysis_long$if_hyper, levels = c("non hypertensive","hypertensive" ))
elsa_analysis_long$if_treat <- factor(elsa_analysis_long$if_treat, levels = c("non hypertensive", 
                                                                                  "hypertensive and treated",
                                                                                  "hypertensive and untreated" ))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####interaction####
#1. hypertension status
inter_gender_1 <- lmer(cognition ~ if_hyper*time*gender + age + high_edu +
                       marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                       (1|idauniqc), data = elsa_analysis_long)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2. treatment
inter_gender_2 <- lmer(cognition ~ if_treat*time*gender + age + high_edu +
                         marital_status + race + birthplace + wealth_quartile + 
                         bmi + smoke_2012 + drink + restless + phy_act +
                         (1|idauniqc), data = elsa_analysis_long)

#' 
#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tab_model(inter_gender_1, inter_gender_2)

#' 
#' 
#' 
