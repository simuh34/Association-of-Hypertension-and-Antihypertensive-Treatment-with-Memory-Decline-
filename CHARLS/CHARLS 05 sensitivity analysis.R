library(sjPlot)
library(lme4)
library(tidyr)
library(glm2)
library(dplyr)
library(ggplot2)

CHARLS_analysis_long <- read.csv("CHARLS_analysis_long.csv")
CHARLS_analysis_long$ht_status2011 <- factor(CHARLS_analysis_long$ht_status2011, levels = c("non hypertensive",
                                                                                            "hypertensive, treated and controlled",
                                                                                            "hypertensive, treated but uncontrolled", 
                                                                                            "hypertensive and untreated" ))
CHARLS_analysis_long$if_hyper <- factor(CHARLS_analysis_long$if_hyper, levels = c("non hypertensive",
                                                                                  "hypertensive" ))
CHARLS_analysis_long$if_treat <- factor(CHARLS_analysis_long$if_treat, levels = c("non hypertensive", "hypertensive and treated",
                                                                                  "hypertensive and untreated" ))
cols_to_factor <- c("if_hyper", "if_treat", "education", "highest_education_parent",
                    "marital_status", "wealth_quartile", "smoke_2011", "drink_ever",
                    "restless", "phy_act", "social_act","gender","hukou")
CHARLS_analysis_long <- CHARLS_analysis_long %>%
  mutate(across(all_of(cols_to_factor), as.factor))

cols_to_numeric <- c("age", "time", "bmi")
CHARLS_analysis_long <- CHARLS_analysis_long %>%
  mutate(across(all_of(cols_to_numeric), as.numeric))

####sensitivity analysis####
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
model_ipcw_3 <- lmer(cognition ~ ht_status2011*time + gender + age + education + highest_education_parent+
                                      marital_status + hukou + urbanicity + wealth_quartile + 
                                       bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                                       (1|ID), weights = ipcw_cum_inw4, data = CHARLS_analysis_long)
tab_model(model_ipcw_1,model_ipcw_2)


total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("low", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("lower", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  high_edu_parent <- rep("low", total_rows),
  if_hyper = rep(c("non hypertensive", 
                   "hypertensive"), 
                 times = c(1800, 1800)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)


yhat1 <- predict(model_ipcw_1, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_hyper, 
                               levels = c("non hypertensive", 
                                          "hypertensive"))


jpeg("charls_ipcw_if_hyper.jpeg", width =7, height = 5, units = 'in', res = 600)
hrs_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 1.2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0)) +
  scale_color_manual(values = c("#861BCA","#122F84"),  
                     labels = c("Non hypertensive", 
                                "Hypertensive")) +
  ggtitle(" ") +
  ylab(" ") + 
  xlab(" ") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  guides(shape = "none", color = "none") +
  theme(legend.position = c(0.80, 0.85)) 
print(hrs_mixed_if_hyper)
dev.off()



total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("low", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("lower", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  high_edu_parent <- rep("low", total_rows),
  if_treat = rep(c("non hypertensive", "hypertensive and treated",
                   "hypertensive and untreated"), 
                 times = c(1200, 1200, 1200)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)

yhat1 <- predict(model_ipcw_2, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_treat, 
                               levels = c("non hypertensive", "hypertensive and treated",
                                          "hypertensive and untreated"))


jpeg("CHARLS_ipcw_if_treat.jpeg", width =7, height = 5, units = 'in', res = 600)

hrs_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 1.2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0)) +
  scale_color_manual(values = c("#861BCA", "#1EBCE1","#1E6AE1"),  
                     labels = c("Non hypertensive", "Hypertensive and treated",
                                "Hypertensive and untreated")) +
  ggtitle(" ") +
  ylab(" ") + 
  xlab(" ") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  guides(shape = "none", color = "none") +
  theme(legend.position = c(0.80, 0.85)) 
print(hrs_mixed_if_hyper)
dev.off()



#2. complete case analysis
# need to extract complete data before covariates imputation (in file 2 data cleaning (around charls 02 - line 215))
CHARLS_analysis_complete <- read.csv("charls_df_complete.csv")



# reconvert complete data into long data
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

#reconstruct outcome in complete data
CHARLS_analysis_complete_long$if_hyper <- ifelse(CHARLS_analysis_complete_long$ht_status2011 == "non hypertensive", 
                                                                              "non hypertensive","hypertensive")
CHARLS_analysis_complete_long$if_hyper <- factor(CHARLS_analysis_complete_long$if_hyper,c("non hypertensive",
                                                                        "hypertensive"))

CHARLS_analysis_complete_long$if_treat <- ifelse(CHARLS_analysis_complete_long$ht_status2011 == "non hypertensive", 
                                                                              "non hypertensive", 
                  ifelse(CHARLS_analysis_complete_long$ht_status2011 == "hypertensive, treated and controlled"|
                           CHARLS_analysis_complete_long$ht_status2011 == "hypertensive, 
                                                                  treated but uncontrolled", 
                                                                  "hypertensive and treated","hypertensive and untreated"))
CHARLS_analysis_complete_long$if_treat <- factor(CHARLS_analysis_complete_long$if_treat,c("non hypertensive",
                                                                        "hypertensive and treated",
                                                                        "hypertensive and untreated"))

CHARLS_analysis_complete_long$ht_status2011 <- factor(CHARLS_analysis_complete_long$ht_status2011,c("non hypertensive",
                                                                        "hypertensive, treated and controlled",
                                                                        "hypertensive, treated but uncontrolled",
                                                                        "hypertensive and untreated"))

cols_to_factor <- c("if_hyper", "if_treat", "education", "highest_education_parent",
                    "marital_status", "wealth_quartile", "smoke_2011", "drink_ever",
                    "restless", "phy_act", "social_act","gender","hukou")
CHARLS_analysis_complete_long <- CHARLS_analysis_complete_long %>%
  mutate(across(all_of(cols_to_factor), as.factor))

cols_to_numeric <- c("age", "time", "bmi")
CHARLS_analysis_complete_long <- CHARLS_analysis_complete_long %>%
  mutate(across(all_of(cols_to_numeric), as.numeric))

#modeling
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

#extract result table in supplementary table
tab_model(model_1, model_2)

total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("low", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("lower", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  high_edu_parent <- rep("low", total_rows),
  if_hyper = rep(c("non hypertensive", 
                   "hypertensive"), 
                 times = c(1800, 1800)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)

yhat1 <- predict(model_1, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_hyper, 
                               levels = c("non hypertensive", 
                                          "hypertensive"))


jpeg("CHARLS_complete_if_hyper.jpeg", width =7, height = 5, units = 'in', res = 600)
hrs_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3,0.25),xlim = c(2010, 2020)) +
  geom_line(size = 1.2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0)) +
  scale_color_manual(values = c("#861BCA","#122F84"),  
                     labels = c("Non hypertensive", 
                                "Hypertensive")) +
  ggtitle(" ") +
  ylab(" ") + 
  xlab(" ") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  guides(shape = "none", color = "none") +
  theme(legend.position = c(0.80, 0.85)) 
print(hrs_mixed_if_hyper)
dev.off()


total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("low", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("lower", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  high_edu_parent <- rep("low", total_rows),
  if_treat = rep(c("non hypertensive", "hypertensive and treated",
                   "hypertensive and untreated"), 
                 times = c(1200,1200, 1200)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)

yhat1 <- predict(model_2, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_treat, 
                               levels = c("non hypertensive", "hypertensive and treated",
                                          "hypertensive and untreated"))


jpeg("CHARLS_complete_if_treat.jpeg", width =7, height = 5, units = 'in', res = 600)

hrs_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3,0.25),xlim = c(2010, 2020)) +
  geom_line(size = 1.2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0)) +
  scale_color_manual(values = c("#861BCA", "#1EBCE1","#1E6AE1"),  
                     labels = c("Non hypertensive", "Hypertensive and treated",
                                "Hypertensive and untreated")) +
  ggtitle(" ") +
  ylab(" ") + 
  xlab(" ") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  guides(shape = "none", color = "none") +
  theme(legend.position = c(0.80, 0.85)) 
print(hrs_mixed_if_hyper)
dev.off()


#3.sampling weight
CHARLS_analysis_long <- CHARLS_analysis_long[which(!is.na(CHARLS_analysis_long$r1wtresp)), ]
CHARLS_analysis_long$r1wtresp <- CHARLS_analysis_long$r1wtresp/sum(CHARLS_analysis_long[, 'r1wtresp'])*42356

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

#extract result table in supplementary table
tab_model(model_1, model_2)

total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("low", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("lower", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  high_edu_parent <- rep("low", total_rows),
  if_hyper = rep(c("non hypertensive", 
                   "hypertensive"), 
                 times = c(1800, 1800)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)

yhat1 <- predict(model_1, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_hyper, 
                               levels = c("non hypertensive", 
                                          "hypertensive"))


jpeg("CHARLS_sampling_if_hyper.jpeg", width =7, height = 5, units = 'in', res = 600)

hrs_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 1.2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0)) +
  scale_color_manual(values = c("#861BCA","#122F84"),  
                     labels = c("Non hypertensive", 
                                "Hypertensive")) +
  ggtitle(" ") +
  ylab(" ") + 
  xlab(" ") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  guides(shape = "none", color = "none") +
  theme(legend.position = c(0.80, 0.85)) 
print(hrs_mixed_if_hyper)
dev.off()


total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("low", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("lower", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  high_edu_parent <- rep("low", total_rows),
  if_treat = rep(c("non hypertensive", "hypertensive and treated",
                   "hypertensive and untreated"), 
                 times = c(1200,1200, 1200)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)

yhat1 <- predict(model_2, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_treat, 
                               levels = c("non hypertensive", "hypertensive and treated",
                                          "hypertensive and untreated"))


jpeg("CHARLS_sampling_if_treat.jpeg", width =7, height = 5, units = 'in', res = 600)

hrs_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 1.2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0)) +
  scale_color_manual(values = c("#861BCA", "#1EBCE1","#1E6AE1"),  
                     labels = c("Non hypertensive", "Hypertensive and treated",
                                "Hypertensive and untreated")) +
  ggtitle(" ") +
  ylab(" ") + 
  xlab(" ") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  guides(shape = "none", color = "none") +
  theme(legend.position = c(0.80, 0.85)) 
print(hrs_mixed_if_hyper)
dev.off()
