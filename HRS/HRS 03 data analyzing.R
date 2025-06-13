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

HRS_analysis <- read.csv("HRS_analysis_df.csv")
HRS_analysis <- HRS_analysis[,-1:-2]
HRS_analysis$raeducl <- relevel(factor(HRS_analysis$raeducl), ref = "1")
HRS_analysis$smoke_2010 <- relevel(factor(HRS_analysis$smoke_2010), ref = "non smoker")
HRS_analysis$highest_education_parent <- relevel(factor(HRS_analysis$highest_education_parent), ref = "1")
HRS_analysis$restless <- factor(HRS_analysis$restless )
HRS_analysis$drink_ever <- factor(HRS_analysis$drink_ever)
HRS_analysis$Race <- factor(HRS_analysis$Race)
HRS_analysis$wealth_quartile <- factor(HRS_analysis$wealth_quartile, c("1","2","3","4"))
HRS_analysis$phy_act <- factor(HRS_analysis$phy_act)
HRS_analysis$social_act <- factor(HRS_analysis$social_act)
HRS_analysis$Race <- factor(ifelse(HRS_analysis$Race == "Non-Hispanic White", "Non-Hispanic White", "Others"))

#recode raeducl as 1=high vs 0=low raeducl
table(HRS_analysis$raeducl)
HRS_analysis$high_edu[HRS_analysis$raeducl == 2 | HRS_analysis$raeducl == 3] <- "high"
HRS_analysis$high_edu[HRS_analysis$raeducl == 1] <- "low"
HRS_analysis$high_edu <- factor(HRS_analysis$high_edu)
HRS_analysis$high_edu <- relevel(HRS_analysis$high_edu, ref = "low")
table(HRS_analysis$high_edu)
HRS_analysis$ht_status2010 <- factor(HRS_analysis$ht_status2010, levels = c("non hypertensive","hypertensive, treated and controlled","hypertensive, treated but uncontrolled", "hypertensive and untreated" ))

#parental raeducl
HRS_analysis$high_edu_parent <- ifelse(HRS_analysis$highest_education_parent == 2|HRS_analysis$highest_education_parent == 3, "high", "low")
table(HRS_analysis$high_edu_parent)
HRS_analysis$highest_education_parent  <- factor(HRS_analysis$highest_education_parent )
#HRS_analysis$high_edu_parent<- relevel(HRS_analysis$high_edu_parent, ref = "low")

#anova test
# anova_cog <- aov(age ~ ht_status2011, data = HRS_analysis)
# summary(anova_cog)
#p<2e-16

#1.descriptive analysis
names(HRS_analysis)

# 
HRS_analysis$if_treat <- ifelse(HRS_analysis$ht_status2010 == "non hypertensive", "non hypertensive",
                                     ifelse(HRS_analysis$ht_status2010 == "hypertensive, treated and controlled"|HRS_analysis$ht_status2010 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))

table(HRS_analysis$if_treat)
HRS_analysis$if_treat <- factor(HRS_analysis$if_treat,c("non hypertensive","hypertensive and treated","hypertensive and untreated"))

t2 = CreateTableOne(vars = c("ragender","age","if_treat","raeducl", "highest_education_parent","marital_status", "Race", "urbanicity","wealth_quartile","bmi", "smoke_2010", "drink_ever",'restless',"phy_act","social_act","systo", "diasto","wave1_z","wave2_z", "wave3_z", "wave4_z","wave5_z"),   
                    data=HRS_analysis)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#extract table
table_data <- as.data.frame(print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE))
table_data <- cbind(Variable = rownames(table_data), table_data)  
rownames(table_data) <- NULL  
#write.csv(table_data, "descriptive_table.csv", row.names = FALSE)

df_subset <- HRS_analysis[HRS_analysis$if_treat == "hypertensive and treated"|HRS_analysis$if_treat == "hypertensive and untreated",]

t3 = CreateTableOne(vars = c("ragender","age","if_treat","raeducl", "highest_education_parent","marital_status", "Race", "urbanicity","wealth_quartile","bmi", "smoke_2010", "drink_ever",'restless',"phy_act","social_act","systo", "diasto","wave1_z","wave2_z", "wave3_z", "wave4_z","wave5_z"),   
                    data=HRS_analysis)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#extract table
table_data <- as.data.frame(print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE))
table_data <- cbind(Variable = rownames(table_data), table_data)  
rownames(table_data) <- NULL  
#write.csv(table_data, "descriptive_table.csv", row.names = FALSE)

#2.Mixed regression models
names(HRS_analysis)
HRS_analysis_long  <- HRS_analysis %>%
  pivot_longer(cols = c("wave1_z", "wave2_z","wave3_z","wave4_z","wave5_z"), 
               names_to = "time", 
               values_to = "cognition") %>%
  mutate(time = case_when(
    time == "wave1_z" ~ 0,
    time == "wave2_z" ~ riwmid_w2 - riwmid_w1,
    time == "wave3_z" ~ riwmid_w3 - riwmid_w1,
    time == "wave4_z" ~ riwmid_w4 - riwmid_w1,
    time == "wave5_z" ~ riwmid_w5 - riwmid_w1,
    TRUE ~ NA_real_  
  ))

HRS_analysis_long$ht_status2010 <- as.factor(HRS_analysis_long$ht_status2010)
HRS_analysis_long$time <- as.numeric(HRS_analysis_long$time)
HRS_analysis_long$hhidpn <- as.numeric(HRS_analysis_long$hhidpn)
factor_vars <- c("ragender", "high_edu", "marital_status", "Race", "urbanicity", 
                 "smoke_2010", "drink_ever", "wealth_quartile",'restless',"phy_act")
HRS_analysis_long[factor_vars] <- lapply(HRS_analysis_long[factor_vars], as.factor)
HRS_analysis_long$bmi <- as.numeric(HRS_analysis_long$bmi)
HRS_analysis_long$age <- as.numeric(HRS_analysis_long$age)
anova_cog <- aov(cognition ~ ht_status2010, data = HRS_analysis_long)
summary(anova_cog)
str(HRS_analysis_long[c("ht_status2010","time" , "age","ragender", "raeducl",
                          "marital_status" , "Race" ,"urbanicity" , "highest_education_parent",
                          "bmi" , "smoke_2010" ,"drink_ever" ,"wealth_quartile", "restless" ,"phy_act" ,"hhidpn")])
#hypertension and treatment
mixed <- lmer(cognition ~ ht_status2010*time +
                (1|hhidpn), data = HRS_analysis_long)
summary(mixed)

mixed1 <- lmer(cognition ~ ht_status2010*time + age + ragender + raeducl +
                 marital_status + Race + urbanicity  + wealth_quartile+social_act+
                 bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)
summary(mixed1)
tab_model(mixed, mixed1)

####Attention! add this line before construct newdata and replace time in newdata with this variable
HRS_analysis_long$wave1_date <- ISOdate(HRS_analysis_long$r10iwmid_year, HRS_analysis_long$r10iwmid_month, 1)
HRS_analysis_long$wave1_date <- as.Date(HRS_analysis_long$wave1_date)
month_diff <- (as.numeric(format(HRS_analysis_long$wave1_date, "%Y")) - 2010) * 12 + 
  as.numeric(format(HRS_analysis_long$wave1_date, "%m")) - 1
time_1 <- round(min(month_diff / 12),2)
time_2 <- round(max(HRS_analysis_long$time,na.rm = TRUE),2) 
####Attention! add this line before construct newdata and replace this in newdata with this variable



# intermediate analysis
## hypertension 
HRS_analysis_long$if_hyper <- ifelse(HRS_analysis_long$ht_status2010 == "non hypertensive", "non hypertensive", "hypertensive")
table(HRS_analysis_long$if_hyper)
HRS_analysis_long$if_hyper <- factor(HRS_analysis_long$if_hyper,c("non hypertensive", "hypertensive"))

mixed <- lmer(cognition ~ if_hyper*time +
                (1|hhidpn), data = HRS_analysis_long)
summary(mixed)

mixed1 <- lmer(cognition ~ if_hyper*time + age + ragender + raeducl + 
                 marital_status + Race + urbanicity  + wealth_quartile+social_act+
                 bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)
summary(mixed1)
tab_model(mixed1)

total_rows <- 3600  
newdata <- data.frame(
  age = rep(65, total_rows),
  ragender = rep("men", total_rows),
  raeducl = rep("1", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("1", total_rows),
  Race = rep("Non-Hispanic White", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2010 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  highest_education_parent <- rep("1", total_rows),
  if_hyper = rep(c("non hypertensive", 
                        "hypertensive"), 
                      times = c(1800, 1800)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  hhidpn = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)

yhat1 <- predict(mixed1, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_hyper, 
                               levels = c("non hypertensive", 
                                          "hypertensive"))


#jpeg("HRS_mixed_if_hyper.jpeg", width =7, height = 5, units = 'in', res = 600)

hrs_mixed_if_hyper <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0),size = 3) +
  scale_color_manual(values = c("#f7b91c","#1E6AE1"),  # Adjust colors to match the order
                     labels = c("Non hypertensive", 
                                "Hypertensive")) +
  #ggtitle("The United States") +
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
  # Remove the shape legend and hide the color legend
  guides(shape = "none", color = FALSE) +
  # Remove the shape legend and move color legend to top right corner
  #guides(shape = "none", color = FALSE) +
  theme(legend.position = "none")
jpeg("HRS_mixed_if_hyper.jpeg", width =7, height = 5, units = 'in', res = 600)
print(hrs_mixed_if_hyper)
dev.off()

## hypertension & anti treatment
HRS_analysis_long$if_treat <- ifelse(HRS_analysis_long$ht_status2010 == "non hypertensive", "non hypertensive", 
                                     ifelse(HRS_analysis_long$ht_status2010 == "hypertensive, treated and controlled"|HRS_analysis_long$ht_status2010 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))
                                     
table(HRS_analysis_long$if_treat)
HRS_analysis_long$if_treat <- factor(HRS_analysis_long$if_treat,c("non hypertensive","hypertensive and treated","hypertensive and untreated"))

mixed <- lmer(cognition ~ if_treat*time +
                (1|hhidpn), data = HRS_analysis_long)
summary(mixed)

mixed1 <- lmer(cognition ~ if_treat*time + age + ragender + raeducl +highest_education_parent+
                 marital_status + Race + urbanicity  + wealth_quartile+social_act+
                 bmi + smoke_2010 + drink_ever + restless + phy_act  + (1|hhidpn), data = HRS_analysis_long)
summary(mixed1)
tab_model(mixed, mixed1)

newdata <- data.frame(
  age = rep(65, total_rows),
  ragender = rep("men", total_rows),
  raeducl = rep("1", total_rows),
  urbanicity = rep("rural", total_rows),
  wealth_quartile = rep("2", total_rows),
  Race = rep("Non-Hispanic White", total_rows),
  marital_status = rep("married/partnered", total_rows),
  bmi = rep(25, total_rows),
  smoke_2010 = rep("non smoker", total_rows),
  drink_ever = rep(0, total_rows),
  restless = rep(0, total_rows),
  phy_act = rep(0, total_rows),
  social_act = rep(0, total_rows),
  highest_education_parent <- rep("1", total_rows),
  if_treat = rep(c("non hypertensive", "hypertensive and treated",
                   "hypertensive and untreated"), 
                 times = c(1200,1200,1200)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  hhidpn = rep(1, total_rows)
)
str(newdata)

newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)] <- lapply(newdata[, c(2,3,4,5,6,7,9,10,11,12,13,14,15)], as.factor)

yhat1 <- predict(mixed1, newdata, allow.new.levels=TRUE)
yhat1
yhat1 <- as.data.frame(yhat1)
names(yhat1)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(15,16)], yhat1)

predictmemory$groups <- factor(predictmemory$if_treat, 
                               levels = c("non hypertensive", "hypertensive and treated",
                                          "hypertensive and untreated"))

# Set the color scale according to the correct factor levels
#jpeg("plot\\HRS_mixed_if_treat.jpeg", width =7, height = 5, units = 'in', res = 600)
# Create the plot
hrs_mixed_if_treat <- ggplot(predictmemory, aes(x = 2010 + time, y = yhat, color = groups)) + 
        coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
        geom_line(size = 2) + 
        geom_point(data = subset(predictmemory, time %% 0.5 == 0),size = 3) +
        scale_color_manual(values = c("#f7b91c","#122F84", "#1EBCE1"),  # Adjust colors to match the order
                           labels = c("Non hypertensive", "Hypertensive and treated",
                                      "Hypertensive and untreated")) +
        #ggtitle("The United States") +
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
        #guides(shape = "none", color = FALSE) +
        theme(legend.position = "none")
jpeg("HRS_mixed_if_treat.jpeg", width =7, height = 5, units = "in", res = 600)
print(hrs_mixed_if_treat)
#dev.off()

dev.off()

write.csv(HRS_analysis_long,"HRS_analysis_long.csv")



