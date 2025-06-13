
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lme4)
library(lmerTest)
library(tableone)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

MHAS_analysis <- read.csv("MHAS_df.csv")
MHAS_analysis <- MHAS_analysis[,-1:-2]

MHAS_analysis$education <- relevel(factor(MHAS_analysis$education), ref = "1")
MHAS_analysis$smoke_2012 <- relevel(factor(MHAS_analysis$smoke_2012), ref = "non smoker")
MHAS_analysis$highest_Harmonized_education_parent <- relevel(factor(MHAS_analysis$highest_education_parent), ref = "1")
MHAS_analysis$restless <- factor(MHAS_analysis$restless )
MHAS_analysis$drink_ever <- factor(MHAS_analysis$drink_ever)
MHAS_analysis$wealth_quartile <- factor(MHAS_analysis$wealth_quartile)

#recode education as 1=high vs 0=low education
table(MHAS_analysis$education)


#parental education
MHAS_analysis$high_edu_parent <- ifelse(MHAS_analysis$highest_education_parent == 1|MHAS_analysis$highest_education_parent == 2, 1, ifelse(MHAS_analysis$highest_education_parent == 3, 2, 3))
MHAS_analysis$high_edu_parent <- factor(MHAS_analysis$high_edu_parent)
table(MHAS_analysis$high_edu_parent)

#anova test 
anova_cog <- aov(age ~ ht_status2012, data = MHAS_analysis)
summary(anova_cog)
#p<2e-16

#1.descriptive analysis 
names(MHAS_analysis)

## Descriptive charateristics by hypertension status
t1 = CreateTableOne(vars = c("gender","age", "education", "marital_status",  "urbanicity","wealth_quartile","high_edu_parent","bmi", "smoke_2012", "drink_ever",'restless',"phy_act","social_act","r3systo", "r3diasto","cognition1", "cognition2", "cognition3"), 
                    data=MHAS_analysis, strata = "htn_2012") 
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

## table 2 adjusted hypertension & anti treatment
MHAS_analysis$if_treated <- ifelse(MHAS_analysis$ht_status2012 == "non hypertensive", "non hypertensive", 
                                   ifelse(MHAS_analysis$ht_status2012 == "hypertensive, treated and controlled"|MHAS_analysis$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))
MHAS_analysis$if_treated <- relevel(factor(MHAS_analysis$if_treated), ref = "non hypertensive")

## Descriptive charateristics by hypertension and treatment status
t2 = CreateTableOne(vars = c("gender","age", "education", "marital_status","urbanicity","wealth_quartile","high_edu_parent","bmi", "smoke_2012", "drink_ever",'restless',"phy_act","social_act","r3systo", "r3diasto","wave3_z", "wave4_z", "wave5_z","if_treated"),   
                    data=MHAS_analysis)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#extract table
table_data <- as.data.frame(print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE))
table_data <- cbind(Variable = rownames(table_data), table_data)  
rownames(table_data) <- NULL  
#write.csv(table_data, "D:\\0\\hypertension and cog\\descriptive_table_MHAS.csv", row.names = FALSE)

#2.Mixed regression models
names(MHAS_analysis)
MHAS_analysis_long <- MHAS_analysis %>%
  pivot_longer(cols = c("wave3_z", "wave4_z","wave5_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave3_z" ~ 0,
    time == "wave4_z" ~ riwmid_w4 - riwmid_w3,
    time == "wave5_z" ~ riwmid_w5 - riwmid_w3,
    TRUE ~ NA_real_  
  ))

MHAS_analysis_long$ht_status2012 <- relevel(factor(MHAS_analysis_long$ht_status2012), ref = "non hypertensive")
MHAS_analysis_long$time <- as.numeric(MHAS_analysis_long$time)
MHAS_analysis_long$np <- as.factor(MHAS_analysis_long$np)
factor_vars <- c("gender", "education", "marital_status",  "urbanicity", 
                 "smoke_2012", "drink_ever", "highest_education_parent","restless","phy_act","social_act")
MHAS_analysis_long[factor_vars] <- lapply(MHAS_analysis_long[factor_vars], as.factor)

anova_cog <- aov(cognition ~ ht_status2012, data = MHAS_analysis_long)
summary(anova_cog)
##P：4.24e-08

####Attention! add this line before construct newdata and replace time in newdata with this variable
MHAS_analysis_long$wave1_date <- ISOdate(MHAS_analysis_long$r3iwy, MHAS_analysis_long$r3iwm, 1)
MHAS_analysis_long$wave1_date <- as.Date(MHAS_analysis_long$wave1_date)
month_diff <- (as.numeric(format(MHAS_analysis_long$wave1_date, "%Y")) - 2012) * 12 + 
  as.numeric(format(MHAS_analysis_long$wave1_date, "%m")) - 1
time_1 <- round(min(month_diff, na.rm = TRUE) / 12, 2)
time_2 <- round(max(MHAS_analysis_long$time,na.rm = TRUE),2) 
####Attention! add this line before construct newdata and replace this in newdata with this variable

#hypertension and treatment
set.seed(1005)
mixed <- lmer(cognition ~ ht_status2012*time +
                (1|np), data = MHAS_analysis_long)
summary(mixed)

set.seed(1005)
mixed1 <- lmer(cognition ~ ht_status2012*time + age + gender + education +
                 marital_status + urbanicity + wealth_quartile + 
                 bmi + smoke_2012 + drink_ever + highest_education_parent + restless + phy_act +social_act+
                 (1|np), data = MHAS_analysis_long)
summary(mixed1)
tab_model(mixed, mixed1)

MHAS_analysis_long$wave1_date <- ISOdate(MHAS_analysis_long$r3iwy, MHAS_analysis_long$r3iwm, 1)
MHAS_analysis_long$wave1_date <- as.Date(MHAS_analysis_long$wave1_date)
month_diff <- (as.numeric(format(MHAS_analysis_long$wave1_date, "%Y")) - 2012) * 12 + 
  as.numeric(format(MHAS_analysis_long$wave1_date, "%m")) - 1
time_1 <- round(min(month_diff, na.rm = TRUE) / 12, 2)
time_2 <- round(max(MHAS_analysis_long$time,na.rm = TRUE),2) 


# intermediate analysis
## hypertension 
MHAS_analysis_long$if_hyper <- ifelse(MHAS_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", "hypertensive")
table(MHAS_analysis_long$if_hyper)
MHAS_analysis_long$if_hyper <- factor(MHAS_analysis_long$if_hyper)

MHAS_analysis_long$if_hyper <- factor(MHAS_analysis_long$if_hyper, ordered = FALSE)

MHAS_analysis_long$if_hyper <- relevel(MHAS_analysis_long$if_hyper, ref = "non hypertensive")

set.seed(1005)
mixed <- lmer(cognition ~ if_hyper*time +
                (1|np), data = MHAS_analysis_long)
summary(mixed)

set.seed(1005)
mixed2 <- lmer(cognition ~ if_hyper*time + age + gender + education +
                 marital_status  + urbanicity + wealth_quartile + 
                 bmi + smoke_2012 + drink_ever + restless + phy_act+social_act+
                 (1|np), data = MHAS_analysis_long)
summary(mixed2)
str(MHAS_analysis$wealth_quartile)
tab_model(mixed, mixed2)



total_rows <- 4923  
newdata2 <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  education = rep("1", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("1", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2012 = rep("non smoker", total_rows),
  drink_ever = rep("no", total_rows),
  restless = rep("no", total_rows),
  phy_act = rep("no", total_rows),
  social_act=rep("no",total_rows),
  if_hyper = rep(c("non hypertensive", 
                   "hypertensive"), 
                 times = c(2461, 2462)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  np = rep(1, total_rows))

newdata2[, c(2,3,4,5,6,8,9,10,11,12,13)] <- lapply(newdata2[, c(2,3,4,5,6,8,9,10,11,12,13)], as.factor)

newdata2$age <- as.integer(newdata2$age)
newdata2$wealth_quartile <- as.factor(newdata2$wealth_quartile)
newdata2$np <- as.factor(newdata2$np)
newdata2$bmi <- as.numeric(newdata2$bmi)

str(newdata2)
str(model.frame(mixed2))

set.seed(1005)
yhat2 <- predict(mixed2, newdata2, allow.new.levels=TRUE)
yhat2
yhat2 <- as.data.frame(yhat2)
names(yhat2)[1] <- "yhat"
predictmemory <- cbind(newdata2[, c(13,14)], yhat2)

predictmemory$groups <- factor(predictmemory$if_hyper, 
                               levels = c("non hypertensive", 
                                          "hypertensive"))


# Create the plot
MHAS_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2012 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0), 
             aes(x = 2012 + time),size = 3) +
  scale_color_manual(values = c("#f7b91c","#1E6AE1"),  # Adjust colors to match the order
                     labels = c("Non hypertensive", 
                                "Hypertensive")) +
  #ggtitle("Mexico") +
  ylab("Memory Z-score") + 
  xlab("Year") +
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018", "2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  # Draws x and y axis line
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  # Remove the shape legend and move color legend to top right corner
  #guides(shape = "none", color = guide_legend(title = "Hypertension Status")) +
  theme(legend.position = "none")  # Position legend at top right corner
jpeg("MHAS_mixed_if_hyper.jpeg", width =7, height = 5, units = 'in', res = 600)
print(MHAS_mixed_if_hyper)
dev.off()


## hypertension & anti treatment
MHAS_analysis_long$if_treat <- ifelse(MHAS_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", 
                                     ifelse(MHAS_analysis_long$ht_status2012 == "hypertensive, treated and controlled"|MHAS_analysis_long$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))
MHAS_analysis_long$if_treat <- factor(MHAS_analysis_long$if_treat, levels = c("non hypertensive","hypertensive and treated","hypertensive and untreated"))

table(MHAS_analysis_long$if_treat)
MHAS_analysis_long$if_treat <- factor(MHAS_analysis_long$if_treat)

summary(mixed)

set.seed(1005)
mixed3 <- lmer(cognition ~ if_treat*time + age + gender + education +
                 marital_status + urbanicity + wealth_quartile + 
                 bmi + smoke_2012 + drink_ever + restless + phy_act +social_act+
                 (1|np), data = MHAS_analysis_long)

summary(mixed3)
str(MHAS_analysis$wealth_quartile)
tab_model(mixed, mixed3)

total_rows <- 4923  
newdata3 <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  education = rep("1", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("1", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2012 = rep("non smoker", total_rows),
  drink_ever = rep("no", total_rows),
  restless = rep("no", total_rows),
  phy_act = rep("no", total_rows),
  social_act=rep("no",total_rows),
  if_treat = rep(c("non hypertensive", "hypertensive and treated",
                   "hypertensive and untreated"), 
                 times = c(1641,1641,1641)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  np = rep(1, total_rows)
)
str(newdata3)
str(model.frame(mixed3))

newdata3[, c(2,3,4,5,6,8,9,10,11,12,13)] <- lapply(newdata3[, c(2,3,4,5,6,8,9,10,11,12,13)], as.factor)

newdata3$age <- as.integer(newdata3$age)
newdata3$wealth_quartile <- as.factor(newdata3$wealth_quartile)
newdata3$np <- as.factor(newdata3$np)
newdata3$bmi <- as.numeric(newdata3$bmi)

set.seed(1005)
yhat3 <- predict(mixed3, newdata3, allow.new.levels=TRUE)
yhat3
yhat3 <- as.data.frame(yhat3)
names(yhat3)[1] <- "yhat"
predictmemory <- cbind(newdata3[, c(13,14)], yhat3)

predictmemory$groups <- factor(predictmemory$if_treat, 
                               levels = c("non hypertensive", "hypertensive and treated","hypertensive and untreated"))

# Create the plot
MHAS_mixed_if_treat = ggplot(predictmemory, aes(x = 2012 + time, y = yhat, color = groups, linetype = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0),size = 3) +
  scale_color_manual(values = c("#f7b91c","#122F84", "#1EBCE1"),  # Adjust colors to match the order
                     labels = c("Non hypertensive", "Hypertensive and treated",
                                "Hypertensive and untreated")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid"),
                        labels = c("Non hypertensive", "Hypertensive and treated",
                                   "Hypertensive and untreated")) +
  #ggtitle("Mexico") +
  ylab("Marginal trajectory memory") + 
  xlab("Year") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018", "2020")) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  # Draws x and y axis line
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text.x = element_text(color = "black", hjust = 1.0, vjust = 1.0, size = 10.0)) +
  # Remove the shape legend and move color legend to top right corner
  #guides(shape = "none", color = guide_legend(title = "Hypertension Status"), linetype = guide_legend(title = "Hypertension Status")) +
  theme(legend.position = "none")

jpeg("MHAS_mixed_if_treat.jpeg", width =7, height = 5, units = 'in', res = 600)
print(MHAS_mixed_if_treat)
dev.off()
jpeg("MHAS_mixed.jpg",res = 600, width = 7, height = 10, units = "in")
MHAS_mixed_if_hyper+MHAS_mixed_if_treat+plot_layout(ncol =  1)
dev.off()



#stratified by age
mixed_age1 <- lmer(cognition ~ ht_status2012*time + age + gender + high_edu +
                     marital_status  + urbanicity + wealth_quartile + 
                     bmi + smoke_2012 + drink_ever + high_edu_parent + 
                     (1|np), subset(MHAS_analysis_long, agecat == "age1"))
summary(mixed_age1)

mixed_age2 <- lmer(cognition ~ ht_status2012*time + age + gender + high_edu +
                     marital_status  + urbanicity + wealth_quartile + 
                     bmi + smoke_2012 + drink_ever + high_edu_parent + 
                     (1|np), subset(MHAS_analysis_long, agecat == "age2"))
summary(mixed_age2)

mixed_age3 <- lmer(cognition ~ ht_status2012*time + age + gender + high_edu +
                     marital_status  + urbanicity + wealth_quartile + 
                     bmi + smoke_2012 + drink_ever + high_edu_parent + 
                     (1|np), subset(MHAS_analysis_long, agecat == "age3"))
summary(mixed_age3)
tab_model(mixed_age1, mixed_age2, mixed_age3)

write.csv(MHAS_analysis_long,"MHAS_analysis_long.csv")
