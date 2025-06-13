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

CHARLS_analysis <- read.csv("charls_df.csv")
CHARLS_analysis <- CHARLS_analysis[,-1:-2]
CHARLS_analysis$education <- relevel(factor(CHARLS_analysis$education), ref = "1")
CHARLS_analysis$smoke_2011 <- factor(CHARLS_analysis$smoke_2011, c("non smoker","past smoker","current smoker"))
CHARLS_analysis$highest_Harmonized_education_parent <- relevel(factor(CHARLS_analysis$highest_education_parent), ref = "1")
CHARLS_analysis$restless <- factor(CHARLS_analysis$restless )
CHARLS_analysis$social_act <- factor(CHARLS_analysis$social_act)
CHARLS_analysis$drink_ever <- factor(CHARLS_analysis$drink_ever)
CHARLS_analysis$hukou <- factor(CHARLS_analysis$hukou)
CHARLS_analysis$wealth_quartile <- factor(CHARLS_analysis$wealth_quartile)
CHARLS_analysis$phy_act <- factor(CHARLS_analysis$phy_act)

#recode education as 1=high vs 0=low education
# table(CHARLS_analysis$education)
# CHARLS_analysis$high_edu[CHARLS_analysis$education == 2 | CHARLS_analysis$education == 3] <- "high"
# CHARLS_analysis$high_edu[CHARLS_analysis$education == 1] <- "low"
# CHARLS_analysis$high_edu <- factor(CHARLS_analysis$high_edu)
# CHARLS_analysis$highest_Harmonized_education_parent[CHARLS_analysis$highest_Harmonized_education_parent == 2 | CHARLS_analysis$highest_Harmonized_education_parent == 3] <- "high"
# CHARLS_analysis$high_edu_parent[CHARLS_analysis$highest_Harmonized_education_parent == 1] <- "low"
# CHARLS_analysis$high_edu_parent <- factor(CHARLS_analysis$high_edu_parent)
CHARLS_analysis$ht_status2011 <- factor(CHARLS_analysis$ht_status2011,c("non hypertensive","hypertensive, treated and controlled","hypertensive, treated but uncontrolled","hypertensive and untreated"))

#CHARLS_analysis$high_edu <- relevel(CHARLS_analysis$high_edu, ref = "low")
t#able(CHARLS_analysis$high_edu)

#parental education
#CHARLS_analysis$high_edu_parent <- ifelse(CHARLS_analysis$highest_education_parent >= 2, "high", "low")
#table(CHARLS_analysis$high_edu_parent)
CHARLS_analysis$highest_education_parent <- factor(CHARLS_analysis$highest_education_parent)
#CHARLS_analysis$high_edu_parent<- relevel(CHARLS_analysis$high_edu_parent, ref = "low")

#anova test
anova_cog <- aov(age ~ ht_status2011, data = CHARLS_analysis)
summary(anova_cog)
#p<2e-16

CHARLS_analysis$if_hyper <- ifelse(CHARLS_analysis$ht_status2011 == "non hypertensive", "non hypertensive", "hypertensive")
table(CHARLS_analysis$if_hyper)

#1.descriptive analysis
names(CHARLS_analysis)
t1 = CreateTableOne(vars = c("if_hyper","age", "education", "marital_status", "hukou", "urbanicity","wealth_quartile","bmi", "smoke_2011", "drink_ever",'restless',"phy_act",'social_act',"r1systo", "r1diasto","cognition1", "cognition2", "cognition3","cognition4"), 
                     data=CHARLS_analysis)
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

CHARLS_analysis$if_treat <- ifelse(CHARLS_analysis$ht_status2011 == "non hypertensive", "non hypertensive", 
                                ifelse(CHARLS_analysis$ht_status2011 == "hypertensive, treated and controlled"|CHARLS_analysis$ht_status2011 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))

table(CHARLS_analysis$if_treat)
CHARLS_analysis$if_treat <- factor(CHARLS_analysis$if_treat,c("non hypertensive","hypertensive and treated","hypertensive and untreated"))

t2 = CreateTableOne(vars = c("gender","age","if_treat", "education", "marital_status", "hukou", "urbanicity","wealth_quartile","highest_education_parent","bmi", "smoke_2011", "drink_ever",'restless',"phy_act",'social_act',"r1systo", "r1diasto","wave1_z", "wave2_z", "wave3_z","wave4_z"),   
                     data=CHARLS_analysis)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#extract table
table_data <- as.data.frame(print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE))
table_data <- cbind(Variable = rownames(table_data), table_data)  
rownames(table_data) <- NULL  
#write.csv(table_data, "D:/hypertension/CHARLS/descriptive_table.csv", row.names = FALSE)

df_subset <- CHARLS_analysis[CHARLS_analysis$if_treat == "hypertensive and treated"|CHARLS_analysis$if_treat == "hypertensive and untreated",]
table(df_subset$if_treat)

#2.Mixed regression models
names(CHARLS_analysis)
CHARLS_analysis_long  <- CHARLS_analysis %>%
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

CHARLS_analysis_long$ht_status2011 <- as.factor(CHARLS_analysis_long$ht_status2011)
CHARLS_analysis_long$time <- as.numeric(CHARLS_analysis_long$time)
CHARLS_analysis_long$ID <- as.factor(CHARLS_analysis_long$ID)
factor_vars <- c("gender", "education", "highest_education_parent","marital_status", "hukou", "urbanicity", 
                 "smoke_2011", "drink_ever", 'restless',"phy_act",'social_act',"wealth_quartile")
CHARLS_analysis_long[factor_vars] <- lapply(CHARLS_analysis_long[factor_vars], as.factor)


anova_cog <- aov(cognition ~ ht_status2011, data = CHARLS_analysis_long)
summary(anova_cog)

#hypertension and treatment
mixed <- lmer(cognition ~ ht_status2011*time +
                 (1|ID), data = CHARLS_analysis_long)
summary(mixed)

mixed1 <- lmer(cognition ~ ht_status2011*time + age + gender + education +
                 marital_status + hukou + urbanicity + wealth_quartile + 
                 bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                 (1|ID), data = CHARLS_analysis_long)
summary(mixed1)
tab_model(mixed, mixed1)


####Attention! add this line before construct newdata and replace time in newdata with this variable
CHARLS_analysis_long$wave1_date <- ISOdate(CHARLS_analysis_long$r1iwy, CHARLS_analysis_long$r1iwm, 1)
CHARLS_analysis_long$wave1_date <- as.Date(CHARLS_analysis_long$wave1_date)
month_diff <- (as.numeric(format(CHARLS_analysis_long$wave1_date, "%Y")) - 2011) * 12 + 
  as.numeric(format(CHARLS_analysis_long$wave1_date, "%m")) - 1
time_1 <- round(min(month_diff / 12),2)
time_2 <- round(max(CHARLS_analysis_long$time,na.rm = TRUE),2) 


# intermediate analysis
## hypertension 
CHARLS_analysis_long$if_hyper <- ifelse(CHARLS_analysis_long$ht_status2011 == "non hypertensive", "non hypertensive", "hypertensive")
table(CHARLS_analysis_long$if_hyper)
CHARLS_analysis_long$if_hyper <- factor(CHARLS_analysis_long$if_hyper,c("non hypertensive","hypertensive"))

mixed <- lmer(cognition ~ if_hyper*time + (1|ID), data = CHARLS_analysis_long)
summary(mixed)

mixed2 <- lmer(cognition ~ if_hyper*time + age + gender + education +
                 marital_status + hukou + urbanicity + wealth_quartile + 
                 bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                 (1|ID), data = CHARLS_analysis_long)
summary(mixed2)
tab_model(mixed, mixed2)
tab_model(mixed2)

total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  education = rep("1", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("1", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  if_hyper = rep(c("non hypertensive", 
                   "hypertensive"), 
                 times = c(1800, 1800)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows))
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14)], as.factor)

yhat1 <- predict(mixed2, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(14,15)], yhat1)

predictmemory$groups <- factor(predictmemory$if_hyper, 
                               levels = c("non hypertensive", 
                                          "hypertensive"))

# Set the color scale according to the correct factor levels
#jpeg("D:/hypertension/CHARLS/plot\\CHARLS_mixed_if_hyper.jpeg", width = 7, height = 5, units = 'in', res = 600)
# Create the plot
charls_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2011 + time, y = yhat, color = groups)) + 
        coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
        geom_line(size = 2) + 
        geom_point(data = subset(predictmemory, time %% 0.5 == 0),size = 3) +
        scale_color_manual(values = c("#f7b91c","#1E6AE1"),  # Adjust colors to match the order
                           labels = c("Non hypertensive", 
                                      "Hypertensive")) +
        #ggtitle("China") +
        ylab("Memory Z-score") + 
        xlab("Year") + 
        scale_x_continuous(breaks = seq(2010, 2020, 2), 
                           labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
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
jpeg("CHARLS_mixed_if_hyper.jpeg", width =7, height = 5, units = 'in', res = 600)
print(charls_mixed_if_hyper)
dev.off()



## hypertension & anti treatment
CHARLS_analysis_long$if_treat <- ifelse(CHARLS_analysis_long$ht_status2011 == "non hypertensive", "non hypertensive", 
                                     ifelse(CHARLS_analysis_long$ht_status2011 == "hypertensive, treated and controlled"|CHARLS_analysis_long$ht_status2011 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))
CHARLS_analysis_long$if_treat <- factor(CHARLS_analysis_long$if_treat,c("non hypertensive","hypertensive and treated","hypertensive and untreated"))

table(CHARLS_analysis_long$if_treat)

mixed <- lmer(cognition ~ if_treat*time +
                (1|ID), data = CHARLS_analysis_long)
summary(mixed)

mixed3 <- lmer(cognition ~ if_treat*time + age + gender + education +
                 marital_status + hukou + urbanicity + wealth_quartile + 
                 bmi + smoke_2011 + drink_ever + restless + phy_act + social_act+
                 (1|ID), data = CHARLS_analysis_long)
summary(mixed3)
tab_model(mixed, mixed3)

total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  education = rep("1", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("1", total_rows),
  hukou = rep("agricultural", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2011 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  if_treat = rep(c("non hypertensive", "hypertensive and treated",
                   "hypertensive and untreated"), 
                 times = c(1200,1200,1200)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  ID = rep(1, total_rows))
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14)], as.factor)

yhat1 <- predict(mixed3, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(14,15)], yhat1)

predictmemory$groups <- factor(predictmemory$if_treat, 
                               levels = c("non hypertensive", "hypertensive and treated",
                                          "hypertensive and untreated"))

# Set the color scale according to the correct factor levels
#jpeg("D:/hypertension/CHARLS/plot\\CHARLS_mixed_if_treat.jpeg", width = 7, height = 5, units = 'in', res = 600)
# Create the plot
charls_mixed_if_treat <- ggplot(predictmemory, aes(x = 2011 + time, y = yhat, color = groups, linetype = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0),size = 3) +
  scale_color_manual(values = c("#f7b91c", "#122F84","#1EBCE1"),  # Adjust colors to match the order
                     labels = c("Non hypertensive", "Hypertensive and treated",
                                "Hypertensive and untreated")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid"),
                        labels =  c("Non hypertensive", "Hypertensive and treated",
                                    "Hypertensive and untreated")) +
  #ggtitle("China") +
  ylab("Memory Z-score") + 
  xlab("Year") + 
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "2012", "2014", "2016", "2018","2020")) +
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
  theme(legend.position = "none")  # Position legend at top right corner
jpeg("CHARLS_mixed_if_treat.jpeg", width =7, height = 5, units = 'in', res = 600)
print(charls_mixed_if_treat)
dev.off()
jpeg("CHARLS_mixed.jpeg", res = 600, width = 7, height = 10, units = "in")
charls_mixed_if_hyper+charls_mixed_if_treat+plot_layout(ncol = 1)
dev.off()


write.csv(CHARLS_analysis_long,"CHARLS_analysis_long.csv")



