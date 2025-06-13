#' elsa
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(sjPlot)
library(lme4)
library(tidyr)
library(glm2)
library(dplyr)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis_long <- read.csv("elsa_analysis_long.csv")

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis_long$ht_status2012 <- factor(elsa_analysis_long$ht_status2012, levels = c("non hypertensive",
                                                                                            "hypertensive, treated and controlled",
                                                                                            "hypertensive, treated but uncontrolled", 
                                                                                            "hypertensive and untreated" ))
elsa_analysis_long$if_hyper <- factor(elsa_analysis_long$if_hyper, levels = c("non hypertensive",
                                                                                  "hypertensive" ))
elsa_analysis_long$if_treat <- factor(elsa_analysis_long$if_treat, levels = c("non hypertensive", "hypertensive and treated",
                                                                                  "hypertensive and untreated" ))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis_long$high_edu <- factor(elsa_analysis_long$high_edu, levels = c("1", "2", "3" ))
elsa_analysis_long$high_edu <- relevel(elsa_analysis_long$high_edu, ref = "1")

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####sensitivity analysis####
#1.ipcw
#censoring selection bias 
#use interview status variables in each wave
elsa_analysis_long <- elsa_analysis_long %>%
  mutate(
    
    death_wave7 = ifelse(r7iwstat == 5, 1, ifelse(r7iwstat < 5, 0, NA)),
    death_wave8 = ifelse(r8iwstat == 5, 1, ifelse(r8iwstat < 5, 0, NA)),
    death_wave9 = ifelse(r9iwstat == 5, 1, ifelse(r9iwstat < 5, 0, NA)),
    
    lost_wave7 = ifelse(r7iwstat == 9 | r7iwstat == 7 | r7iwstat == 4, 1, ifelse(r7iwstat == 1, 0, NA)),
    lost_wave8 = ifelse(r8iwstat == 9 | r8iwstat == 7 | r8iwstat == 4, 1, ifelse(r8iwstat == 1, 0, NA)),
    lost_wave9 = ifelse(r9iwstat == 9 | r9iwstat == 7 | r9iwstat == 4, 1, ifelse(r9iwstat == 1, 0, NA))
  )

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (wave in 7:9) {
  death_var <- paste0("death_wave", wave)
  lost_var <- paste0("lost_wave", wave)
  
  model_death <- glm(as.formula(paste(death_var, "~ age + high_edu +
                       marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act")),
                     data = elsa_analysis_long, 
                     family = binomial, 
                     control = glm.control(maxit = 1000), 
                     na.action = na.exclude)  
  
  model_lost <- glm(as.formula(paste(lost_var, "~ age + high_edu +
                       marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act")),
                    data = elsa_analysis_long, 
                    family = binomial, 
                    control = glm.control(maxit = 1000),
                    na.action = na.exclude)  
  
  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")
  
  if (length(pred_death) == nrow(elsa_analysis_long) && length(pred_lost) == nrow(elsa_analysis_long)) {
    elsa_analysis_long <- elsa_analysis_long %>%
      mutate(!!paste0("prob_alive_inw", wave) := (1 - pred_death) * (1 - pred_lost))
  } else {
    warning("Prediction length mismatch for wave", wave)
  }
}

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (wave in 7:9) {
  prob_var <- paste0("prob_alive_inw", wave)
  ipcw_var <- paste0("ipcw_inw", wave)
  
  elsa_analysis_long <- elsa_analysis_long %>%
    mutate(!!ipcw_var := 1 / get(prob_var))  
}

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#cumulative censoring weight
elsa_analysis_long <- elsa_analysis_long %>%
  mutate(
    ipcw_cum_inw7 = ipcw_inw7,
    ipcw_cum_inw8 = ifelse(is.na(ipcw_inw7), NA, ipcw_inw8 * ipcw_inw7),
    ipcw_cum_inw9 = ifelse(is.na(ipcw_cum_inw8), NA, ipcw_inw9 * ipcw_cum_inw8)
  )

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis_long %>%
  select(ipcw_cum_inw7, ipcw_cum_inw8, ipcw_cum_inw9) %>%
  summary()

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_ipcw_1 <- lmer(cognition ~ if_hyper*time + gender + age + high_edu +
                                       marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                                       (1|idauniqc), weights = ipcw_cum_inw9, data = elsa_analysis_long)
model_ipcw_2 <- lmer(cognition ~ if_treat*time + gender + age + high_edu +
                                      marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                                       (1|idauniqc), weights = ipcw_cum_inw9, data = elsa_analysis_long)
model_ipcw_3 <- lmer(cognition ~ ht_status2012*time + gender + age + high_edu +
                                      marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                                       (1|idauniqc), weights = ipcw_cum_inw9, data = elsa_analysis_long)
tab_model(model_ipcw_1,model_ipcw_2,model_ipcw_3)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2. complete case analysis
# need to extract complete data before covariates imputation (in file 2 data cleaning (around elsa 02 - line 215))
elsa_analysis_complete <- read.csv("elsa_df_complete.csv")

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#recode education in complete data
#elsa_analysis_complete$high_edu[elsa_analysis_complete$education == 2 | #elsa_analysis_complete$education == 3] <- "high"
#elsa_analysis_complete$high_edu[elsa_analysis_complete$education == 1] <- "low"
elsa_analysis_complete$high_edu <- elsa_analysis_complete$education
elsa_analysis_complete$high_edu <- factor(elsa_analysis_complete$high_edu)
elsa_analysis_complete$high_edu <- relevel(elsa_analysis_complete$high_edu, ref = "1")

#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# reconvert complete data into long data
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

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#reconstruct outcome in complete data
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

elsa_analysis_complete_long$ht_status2012 <- factor(elsa_analysis_complete_long$ht_status2012,c("non hypertensive",
                                                                        "hypertensive, treated and controlled",
                                                                        "hypertensive, treated but uncontrolled",
                                                                        "hypertensive and untreated"))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#modeling
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

#extract result table in supplementary table
tab_model(model_1, model_2)


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#3.sampling weight
elsa_analysis_long <- elsa_analysis_long[which(!is.na(elsa_analysis_long$r6lwtresp)), ]
elsa_analysis_long$r6lwtresp <- elsa_analysis_long$r6lwtresp/sum(elsa_analysis_long[, 'r6lwtresp'])*31608

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1. hypertension status
model_1 <- lmer(cognition ~ if_hyper*time + gender + age + high_edu +
                  marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                  (1|idauniqc), weights = r6lwtresp,  data = elsa_analysis_long)

#2. treatment
model_2 <- lmer(cognition ~ if_treat*time + gender + age + high_edu +
                  marital_status + race + birthplace + wealth_quartile + 
                       bmi + smoke_2012 + drink + restless + phy_act +
                  (1|idauniqc), weights = r6lwtresp, data = elsa_analysis_long)


#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#extract result table in supplementary table
tab_model(model_1, model_2)



#' 
