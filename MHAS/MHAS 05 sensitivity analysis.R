library(sjPlot)
library(lme4)
library(tidyr)
library(glm2)
library(dplyr)

MHASc_analysis_long <- read.csv("MHAS_long.csv")
MHASc_analysis_long$ht_status2012 <- factor(MHASc_analysis_long$ht_status2012, levels = c("non hypertensive",
                                                                                            "hypertensive, treated and controlled",
                                                                                            "hypertensive, treated but uncontrolled", 
                                                                                            "hypertensive and untreated" ))
MHASc_analysis_long$if_hyper <- factor(MHASc_analysis_long$if_hyper, levels = c("non hypertensive",
                                                                                  "hypertensive" ))
MHASc_analysis_long$if_treat <- factor(MHASc_analysis_long$if_treat, levels = c("non hypertensive", "hypertensive and treated",
                                                                                  "hypertensive and untreated" ))

####sensitivity analysis####
#1.ipcw
#censoring selection bias 
#use interview status variables in each wave
#4.NR, alive;5.NR, died this wave;6.NR, died prev wave;9.NR, dk if alive or died
# 循环代码
# 打印原始数据框的列名和基本信息
# 1. 确保数据预处理正确（无缺失值，变量名正确）
library(dplyr)
library(rlang)  # 确保加载，用于符号解析

MHASc_analysis_long <- MHASc_analysis_long %>%
  mutate(
    death_wave4 = ifelse(r4iwstat == 5, 1, ifelse(r4iwstat < 5, 0, NA)),
    death_wave5 = ifelse(r5iwstat == 5, 1, ifelse(r5iwstat < 5, 0, NA)),
    lost_wave4 = ifelse(r4iwstat %in% c(4, 9), 1, ifelse(r4iwstat == 1, 0, NA)),
    lost_wave5 = ifelse(r5iwstat %in% c(4, 9), 1, ifelse(r5iwstat == 1, 0, NA))
  )

library(dplyr)
library(rlang)  # 用于符号解析

# ------------------------
# 第一步：处理 wave=4
# ------------------------
wave <- 4  # 手动指定 wave
death_var <- paste0("death_wave", wave)
lost_var <- paste0("lost_wave", wave)

model_death <- glm(
  formula = as.formula(paste(death_var, "~ age + education + high_edu_parent +
                             marital_status + urbanicity + wealth_quartile + 
                             bmi + smoke_2012 + drink_ever + restless + phy_act + social_act")),
  data = MHASc_analysis_long,
  family = binomial,
  control = glm.control(maxit = 1000),
  na.action = na.exclude
)

model_lost <- glm(
  formula = as.formula(paste(lost_var, "~ age + education + high_edu_parent +
                             marital_status + urbanicity + wealth_quartile + 
                             bmi + smoke_2012 + drink_ever + restless + phy_act + social_act")),
  data = MHASc_analysis_long,
  family = binomial,
  control = glm.control(maxit = 1000),
  na.action = na.exclude
)

pred_death <- predict(model_death, newdata = MHASc_analysis_long, type = "response")
pred_lost <- predict(model_lost, newdata = MHASc_analysis_long, type = "response")
prob_alive_wave4 <- (1 - pred_death) * (1 - pred_lost)  # 明确变量名，方便检查

MHASc_analysis_long$prob_alive_inw4 <- prob_alive_wave4



# ------------------------
# 第二步：处理 wave=5
# ------------------------
wave <- 5  # 手动指定 wave
death_var <- paste0("death_wave", wave)
lost_var <- paste0("lost_wave", wave)

model_death <- glm(
  formula = as.formula(paste(death_var, "~ age + education + high_edu_parent +
                             marital_status + urbanicity + wealth_quartile + 
                             bmi + smoke_2012 + drink_ever + restless + phy_act + social_act")),
  data = MHASc_analysis_long,
  family = binomial,
  control = glm.control(maxit = 1000),
  na.action = na.exclude
)

model_lost <- glm(
  formula = as.formula(paste(lost_var, "~ age + education + high_edu_parent +
                             marital_status + urbanicity + wealth_quartile + 
                             bmi + smoke_2012 + drink_ever + restless + phy_act + social_act")),
  data = MHASc_analysis_long,
  family = binomial,
  control = glm.control(maxit = 1000),
  na.action = na.exclude
)

pred_death <- predict(model_death, newdata = MHASc_analysis_long, type = "response")
pred_lost <- predict(model_lost, newdata = MHASc_analysis_long, type = "response")
prob_alive_wave5 <- (1 - pred_death) * (1 - pred_lost)  # 明确变量名，方便检查

MHASc_analysis_long$prob_alive_inw5 <- prob_alive_wave5

# ------------------------
# 第一步：计算 wave4 的 IPCW（ipcw_inw4）
# ------------------------
MHASc_analysis_long <- MHASc_analysis_long %>%
  mutate(ipcw_inw4 = 1 / prob_alive_inw4)
names(MHASc_analysis_long)

# ------------------------
# 第二步：计算 wave5 的 IPCW（ipcw_inw5）
# ------------------------
MHASc_analysis_long <- MHASc_analysis_long %>%
  mutate(ipcw_inw5 = 1 / prob_alive_inw5)
names(MHASc_analysis_long)

#cumulative censoring weight
MHASc_analysis_long <- MHASc_analysis_long %>%
  mutate(
    ipcw_cum_inw4 = ipcw_inw4,
    ipcw_cum_inw5 = ifelse(is.na(ipcw_inw4), NA, ipcw_inw5 * ipcw_inw4)
  )

MHASc_analysis_long %>%
  select(ipcw_cum_inw4, ipcw_cum_inw5) %>%
  summary()

model_ipcw_1 <- lmer(cognition ~ if_hyper*time + gender + age + education +high_edu_parent+
                                       marital_status + urbanicity + wealth_quartile + 
                                       bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                                       (1|np), weights = ipcw_cum_inw4, data = MHASc_analysis_long)
model_ipcw_2 <- lmer(cognition ~ if_treat*time + gender + age + education +high_edu_parent+
                                      marital_status + urbanicity + wealth_quartile + 
                                      bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                                     (1|np), weights = ipcw_cum_inw4, data = MHASc_analysis_long)
model_ipcw_3 <- lmer(cognition ~ ht_status2012*time + gender + age + education +high_edu_parent+
                                      marital_status + urbanicity + wealth_quartile + 
                                       bmi + smoke_2012+ drink_ever + restless + phy_act + social_act+
                                       (1|np), weights = ipcw_cum_inw4, data = MHASc_analysis_long)
tab_model(model_ipcw_1,model_ipcw_2)

#2. complete case analysis
# need to extract complete data before covariates imputation (in file 2 data cleaning (around MHAS 02 - line 215))
MHAS_analysis_complete <- read.csv("MHAS_df_complete.csv")

#recode education in complete data
#1.Less than upper secondary 2.Upper secondary and vocational 3.Tertiary
# MHAS_analysis_complete$high_edu[MHAS_analysis_complete$education == 2 | MHAS_analysis_complete$education == 3] <- "high"
# MHAS_analysis_complete$high_edu[MHAS_analysis_complete$education == 1] <- "low"
MHAS_analysis_complete$high_edu <- factor(MHAS_analysis_complete$education)
# MHAS_analysis_complete$high_edu <- relevel(MHAS_analysis_complete$high_edu, ref = "low")

#parental education
#1.None 2.Some primary 3.Primary 4.More than primary
MHAS_analysis_complete$high_edu_parent <- ifelse(MHAS_analysis_complete$highest_education_parent == 1|MHAS_analysis_complete$highest_education_parent == 2, 1, ifelse(MHAS_analysis_complete$highest_education_parent == 3, 2, 3))
MHAS_analysis_complete$high_edu_parent <- factor(MHAS_analysis_complete$high_edu_parent)
table(MHAS_analysis_complete$high_edu_parent)


# reconvert complete data into long data
MHAS_analysis_complete_long  <- MHAS_analysis_complete %>%
  pivot_longer(cols = c("wave3_z", "wave4_z","wave5_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave3_z" ~ 0,
    time == "wave4_z" ~ riwmid_w4 - riwmid_w3,
    time == "wave5_z" ~ riwmid_w5 - riwmid_w3,
    TRUE ~ NA_real_  
  ))

#reconstruct outcome in complete data
MHAS_analysis_complete_long$if_hyper <- ifelse(MHAS_analysis_complete_long$ht_status2012 == "non hypertensive", 
                                                                              "non hypertensive","hypertensive")
MHAS_analysis_complete_long$if_hyper <- factor(MHAS_analysis_complete_long$if_hyper,c("non hypertensive",
                                                                        "hypertensive"))

MHAS_analysis_complete_long$if_treat <- ifelse(MHAS_analysis_complete_long$ht_status2012 == "non hypertensive", 
                                                                              "non hypertensive", 
                  ifelse(MHAS_analysis_complete_long$ht_status2012 == "hypertensive, treated and controlled"|
                           MHAS_analysis_complete_long$ht_status2012 == "hypertensive, 
                                                                  treated but uncontrolled", 
                                                                  "hypertensive and treated","hypertensive and untreated"))
MHAS_analysis_complete_long$if_treat <- factor(MHAS_analysis_complete_long$if_treat,c("non hypertensive",
                                                                        "hypertensive and treated",
                                                                        "hypertensive and untreated"))

MHAS_analysis_complete_long$ht_status2012 <- factor(MHAS_analysis_complete_long$ht_status2012,c("non hypertensive",
                                                                        "hypertensive, treated and controlled",
                                                                        "hypertensive, treated but uncontrolled",
                                                                        "hypertensive and untreated"))

#modeling
#1. hypertension status
model_1_complete_case <- lmer(cognition ~ if_hyper*time + gender + age + high_edu +high_edu_parent+
                         marital_status + urbanicity + wealth_quartile + 
                         bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                         (1|np), data = MHAS_analysis_complete_long)

#2. treatment
model_2_complete_case <- lmer(cognition ~ if_treat*time + gender + age + high_edu +high_edu_parent+
                         marital_status + urbanicity + wealth_quartile + 
                         bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                         (1|np), data = MHAS_analysis_complete_long)

#extract result table in supplementary table
tab_model(model_1_complete_case, model_2_complete_case)

#3.sampling weight
MHASc_analysis_long <- MHASc_analysis_long[which(!is.na(MHASc_analysis_long$r3wtresp)), ]
MHASc_analysis_long$r3wtresp <- MHASc_analysis_long$r3wtresp/sum(MHASc_analysis_long[, 'r3wtresp'])*5766

#1. hypertension status
model_1_sampling_weight <- lmer(cognition ~ if_hyper*time + gender + age +education +high_edu_parent+
                  marital_status + urbanicity + wealth_quartile + 
                  bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                  (1|np), weights = r3wtresp,  data = MHASc_analysis_long, control = lmerControl(optCtrl = list(maxfun = 100000)))

#2. treatment
model_2_sampling_weight <- lmer(cognition ~ if_treat*time + gender + age + education +high_edu_parent+
                  marital_status + urbanicity + wealth_quartile + 
                  bmi + smoke_2012 + drink_ever + restless + phy_act + social_act+
                  (1|np), weights = r3wtresp, data = MHASc_analysis_long)

#extract result table in supplementary table
tab_model(model_1_sampling_weight, model_2_sampling_weight)
