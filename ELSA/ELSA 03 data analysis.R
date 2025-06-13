#' elsa
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis <- read.csv("ELSA/elsa_df.csv")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis <- elsa_analysis[,-1:-2]
elsa_analysis$ht_status2012 <- relevel(factor(elsa_analysis$ht_status2012), ref = "non hypertensive")
elsa_analysis$education <- relevel(factor(elsa_analysis$education), ref = "1")
elsa_analysis$smoke_2012 <- relevel(factor(elsa_analysis$smoke_2012), ref = "non smoker")
elsa_analysis$highest_education_parent <- relevel(factor(elsa_analysis$highest_education_parent), ref = "1")
elsa_analysis$restless <- factor(elsa_analysis$restless )
elsa_analysis$social_act <- factor(elsa_analysis$social_act)
elsa_analysis$drink <- factor(elsa_analysis$drink)
elsa_analysis$birthplace <- factor(elsa_analysis$birthplace)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#education
elsa_analysis$high_edu <- elsa_analysis$education
elsa_analysis$high_edu <- factor(elsa_analysis$high_edu)
elsa_analysis$high_edu <- relevel(elsa_analysis$high_edu, ref = "1")
table(elsa_analysis$high_edu)

#parental education
elsa_analysis$high_edu_parent <- elsa_analysis$highest_education_parent
elsa_analysis$high_edu_parent <- factor(elsa_analysis$high_edu_parent)
elsa_analysis$high_edu_parent<- relevel(elsa_analysis$high_edu_parent, ref = "1")
table(elsa_analysis$high_edu_parent)






## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#anova test
anova_cog <- aov(age ~ ht_status2012, data = elsa_analysis)
summary(anova_cog)
#p<2e-16


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1.descriptive analysis
names(elsa_analysis)
t1 = CreateTableOne(vars = c("gender","age", "high_edu", "marital_status", "birthplace", "race","wealth_quartile","high_edu_parent","bmi", "smoke_2012", "drink",'restless',"phy_act",'social_act',"r6systo", "r6diasto","cognition6", "cognition7", "cognition8","cognition9"), 
                     data=elsa_analysis, strata = "htn_2012")
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")


#' 
#' last version
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## table 2 adjusted hypertension & anti treatment
elsa_analysis$if_treated <- ifelse(elsa_analysis$ht_status2012 == "non hypertensive", "non hypertensive", 
                                     ifelse(elsa_analysis$ht_status2012 == "hypertensive, treated and controlled"|elsa_analysis$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))
elsa_analysis$if_treated <- relevel(factor(elsa_analysis$if_treated), ref = "non hypertensive")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t2 = CreateTableOne(vars = c("gender","age", "high_edu", "marital_status", "birthplace", "race","wealth_quartile","high_edu_parent","bmi", "smoke_2012", "drink",'restless',"phy_act",'social_act',"r6systo", "r6diasto", "wave6_z", "wave7_z","wave8_z", "wave9_z"),   
                     data=elsa_analysis, strata = "if_treated")
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")


#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#extract table
table_data <- as.data.frame(print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE))
table_data <- cbind(Variable = rownames(table_data), table_data)  
rownames(table_data) <- NULL  
write.csv(table_data, "D:/hypertension/ELSA/elsa_descriptive_table.csv", row.names = FALSE)


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# intermediate analysis
## hypertension 
elsa_analysis$if_hyper <- ifelse(elsa_analysis$ht_status2012 == "non hypertensive", "non hypertensive", "hypertensive")
table(elsa_analysis$if_hyper)
elsa_analysis$if_hyper <- factor(elsa_analysis$if_hyper)
elsa_analysis$if_hyper<- relevel(factor(elsa_analysis$if_hyper), ref = "non hypertensive")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## hypertension & anti treatment
#old version
#elsa_analysis$if_treat <- ifelse(elsa_analysis$ht_status2012 == "non hypertensive", "non hypertensive", 
#                                     ifelse(elsa_analysis$ht_status2012 == "hypertensive, treated and controlled"|elsa_analysis$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))

#new version on 0424
elsa_analysis$if_treat <- ifelse(elsa_analysis$ht_status2012 == "hypertensive, treated and controlled"|elsa_analysis$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated",ifelse(elsa_analysis$ht_status2012 == "hypertensive and untreated","hypertensive and untreated",NA))
elsa_analysis$if_treat <- factor(elsa_analysis$if_treat)
elsa_analysis$if_treat<- relevel(factor(elsa_analysis$if_treat), ref = "hypertensive and treated")
table(elsa_analysis$if_treat)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t3 = CreateTableOne(vars = c("gender","age","if_hyper","if_treat", "high_edu", "marital_status","wealth_quartile","bmi", "smoke_2012", "drink",'restless',"phy_act", "race", "birthplace","r6systo", "r6diasto", "wave6_z", "wave7_z","wave8_z", "wave9_z"),   
                     data=elsa_analysis)
print(t3, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#extract table
table_data <- as.data.frame(print(t3, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE))
table_data <- cbind(Variable = rownames(table_data), table_data)  
rownames(table_data) <- NULL  
write.csv(table_data, "D:/hypertension/ELSA/elsa_table_1.csv", row.names = FALSE)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2.Mixed regression models
names(elsa_analysis)
elsa_analysis_long  <- elsa_analysis %>%
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
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_analysis_long$ht_status2012 <- as.factor(elsa_analysis_long$ht_status2012)
elsa_analysis_long$time <- as.numeric(elsa_analysis_long$time)
#elsa_analysis_long$idauniqc <- as.factor(elsa_analysis_long$idauniqc)
factor_vars <- c("gender", "high_edu", "marital_status", "birthplace", "race", 
                 "smoke_2012", "drink",'restless',"phy_act",'social_act','wealth_quartile')
elsa_analysis_long[factor_vars] <- lapply(elsa_analysis_long[factor_vars], as.factor)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
anova_cog <- aov(cognition ~ ht_status2012, data = elsa_analysis_long)
summary(anova_cog)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)

#hypertension and treatment
mixed <- lmer(cognition ~ ht_status2012*time +
                 (1|idauniqc), data = elsa_analysis_long)
summary(mixed)

#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)

#test mixed
mixed1 <- lmer(cognition ~ ht_status2012*time + age + gender + high_edu +
                 marital_status + birthplace + race + wealth_quartile + 
                 bmi + smoke_2012 + drink + restless + phy_act +
                 (1|idauniqc), data = elsa_analysis_long)
summary(mixed1)
tab_model(mixed, mixed1)#, show.se = T, show.ci = F)

#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Attention! add this line before construct newdata and replace time in newdata with this variable
elsa_analysis_long$wave6_date <- ISOdate(elsa_analysis_long$r6iwindy, elsa_analysis_long$r6iwindm, 1)
elsa_analysis_long$wave6_date <- as.Date(elsa_analysis_long$wave6_date)
month_diff <- (as.numeric(format(elsa_analysis_long$wave6_date, "%Y")) - 2012) * 12 + 
  as.numeric(format(elsa_analysis_long$wave6_date, "%m")) - 1
time_1 <- round(min(month_diff / 12),2)
time_2 <- round(max(elsa_analysis_long$time,na.rm = TRUE),2) 
####Attention! add this line before construct newdata and replace this in newdata with this variable


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# intermediate analysis
## hypertension 
elsa_analysis_long$if_hyper <- ifelse(elsa_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", "hypertensive")
table(elsa_analysis_long$if_hyper)
elsa_analysis_long$if_hyper <- factor(elsa_analysis_long$if_hyper)
elsa_analysis_long$if_hyper<- relevel(factor(elsa_analysis_long$if_hyper), ref = "non hypertensive")


#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)
mixed <- lmer(cognition ~ if_hyper*time +
                (1|idauniqc), data = elsa_analysis_long)
summary(mixed)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)
mixed2 <- lmer(cognition ~ if_hyper*time + age + gender + high_edu +
                 marital_status + birthplace + race + wealth_quartile + 
                 bmi + smoke_2012 + drink + restless + phy_act+
                 (1|idauniqc), data = elsa_analysis_long)

summary(mixed2)

tab_model(mixed, mixed2)#, show.se = T, show.ci = F)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
total_rows <- 3600  
newdata <- data.frame(
  if_hyper = rep(c("non hypertensive", 
                   "hypertensive"), 
                 times = c(1800, 1800)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("1", total_rows),
  marital_status = rep("married/partnered", total_rows),
  birthplace = rep("a", total_rows),
  race = rep("a", total_rows),
  wealth_quartile = rep("btm", total_rows),
  bmi = rep(25, total_rows),
  smoke_2012 = rep("non smoker", total_rows),
  drink = rep(0, total_rows),
  restless = rep("no", total_rows),
  phy_act = rep("no", total_rows),
  idauniqc = rep(1, total_rows)
)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(model.frame(mixed2))

#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(newdata)

newdata[, c(4,5,6,7,8,9,11,12,13,14)] <- lapply(newdata[, c(4,5,6,7,8,9,11,12,13,14)], as.factor)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(newdata)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)
yhat2 <- predict(mixed2, newdata, allow.new.levels=TRUE)
yhat2
yhat2 <- as.data.frame(yhat2)
names(yhat2)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(1,2)], yhat2)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_hyper, 
                               levels = c("non hypertensive", 
                                          "hypertensive"))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_mixed_if_hyper=ggplot(predictmemory, aes(x = 2012 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0),size = 3) +
  scale_color_manual(values = c("#f7b91c", "#1E6AE1"),  # Adjust colors to match the order
                     labels = c("Non hypertensive", 
                                "Hypertensive")) +
  #ggtitle("England") +
  ylab("Memory Z-score") + 
  xlab("Years") + 
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

# Set the color scale according to the correct factor levels
jpeg("elsa_mixed_if_hyper.jpeg", width = 7, height = 5, units = 'in', res = 600)
# Create the plot
print(elsa_mixed_if_hyper)

#dev.off()

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## hypertension & anti treatment
elsa_analysis_long$if_treat <- ifelse(elsa_analysis_long$ht_status2012 == "non hypertensive", "non hypertensive", 
                                     ifelse(elsa_analysis_long$ht_status2012 == "hypertensive, treated and controlled"|elsa_analysis_long$ht_status2012 == "hypertensive, treated but uncontrolled", "hypertensive and treated","hypertensive and untreated"))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(elsa_analysis_long$if_treat)
elsa_analysis_long$if_treat <- factor(elsa_analysis_long$if_treat)
elsa_analysis_long$if_treat<- relevel(factor(elsa_analysis_long$if_treat), ref = "non hypertensive")

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)
mixed <- lmer(cognition ~ if_treat*time +
                (1|idauniqc), data = elsa_analysis_long)
summary(mixed)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)
mixed3 <- lmer(cognition ~ if_treat*time + age + gender + high_edu +
                 marital_status + birthplace + race + wealth_quartile + 
                 bmi + smoke_2012 + drink + restless + phy_act+
                 (1|idauniqc), data = elsa_analysis_long)
summary(mixed3)
#str(elsa_analysis$wealth_quartile)
tab_model(mixed, mixed3)#, show.se = T, show.ci = F)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
total_rows <- 3600  
newdata <- data.frame(
  if_treat = rep(c("non hypertensive", "hypertensive and treated",
                   "hypertensive and untreated"), 
                 times = c(1200,1200,1200)),
  time = rep(seq(time_1, time_2, 0.01), length.out = total_rows),
  age = rep(65, total_rows),
  gender = rep("men", total_rows),
  high_edu = rep("1", total_rows),
  marital_status = rep("married/partnered", total_rows),
  birthplace = rep("a", total_rows),
  race = rep("a", total_rows),
  wealth_quartile = rep("btm", total_rows),
  bmi = rep(25, total_rows),
  smoke_2012 = rep("non smoker", total_rows),
  drink = rep(0, total_rows),
  restless = rep("no", total_rows),
  phy_act = rep("no", total_rows),
  idauniqc = rep(1, total_rows)
)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(newdata)

newdata[, c(4,5,6,7,8,9,11,12,13,14)] <- lapply(newdata[, c(4,5,6,7,8,9,11,12,13,14)], as.factor)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1005)
yhat3 <- predict(mixed3, newdata, allow.new.levels=TRUE)
yhat3
yhat3 <- as.data.frame(yhat3)
names(yhat3)[1] <- "yhat"
predictmemory <- cbind(newdata[, c(1,2)], yhat3)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Modify the legend order and labels
predictmemory$groups <- factor(predictmemory$if_treat, 
                               levels = c("non hypertensive", "hypertensive and treated",
                                          "hypertensive and untreated"))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa_mixed_if_treat=ggplot(predictmemory, aes(x = 2012 + time, y = yhat, color = groups)) + 
  coord_cartesian(ylim = c(-1.3, -0.2), xlim = c(2010, 2020)) +
  geom_line(size = 2) + 
  geom_point(data = subset(predictmemory, time %% 0.5 == 0), aes(x = 2012 + time),size = 3) +
  scale_color_manual(values = c("#f7b91c", "#122F84", "#1EBCE1"),  # Adjust colors to match the order
                     labels = c("Non hypertensive", "Hypertensive and treated",
                                "Hypertensive and untreated")) +
  #ggtitle("Memory trajectory over time - UK") +
  ylab("Marginal trajectory memory") + 
  xlab("Years") + 
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

# Set the color scale according to the correct factor levels
jpeg("elsa_mixed_if_treat.jpeg", width = 7, height = 5, units = 'in', res = 600)
# Create the plot
print(elsa_mixed_if_treat)

#dev.off()

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jpeg("elsa_plots.jpg",res = 600, width = 7, height = 10, units = "in")
elsa_mixed_if_hyper+elsa_mixed_if_treat+plot_layout(ncol =  1)
dev.off()

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class(elsa_mixed_if_hyper)
class(elsa_mixed_if_treat)
class(elsa_mixed_plot)


#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#stratified by age
mixed_age1 <- lmer(cognition ~ ht_status2012*time + age + gender + high_edu +
                     marital_status + birthplace + race + wealth_quartile + 
                     bmi + smoke_2012 + drink + 
                     (1|idauniqc), subset(elsa_analysis_long, agecat == "age1"))
summary(mixed_age1)

mixed_age2 <- lmer(cognition ~ ht_status2012*time + age + gender + high_edu +
                     marital_status + birthplace + race + wealth_quartile + 
                     bmi + smoke_2012 + drink + 
                     (1|idauniqc), subset(elsa_analysis_long, agecat == "age2"))
summary(mixed_age2)

mixed_age3 <- lmer(cognition ~ ht_status2012*time + age + gender + high_edu +
                     marital_status + birthplace + race + wealth_quartile + 
                     bmi + smoke_2012 + drink + 
                     (1|idauniqc), subset(elsa_analysis_long, agecat == "age3"))
summary(mixed_age3)
tab_model(mixed_age1, mixed_age2, mixed_age3, show.se = T, show.ci = F)



#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(elsa_analysis_long,"elsa_analysis_long.csv")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(elsa_analysis_long)

#' 
#' Gender
#' Age
#' Education
#' Parental education
#' Marital status
#' Wealth
#' BMI
#' Smoking
#' Drinking
#' Sleep was restless
#' Physical activity
#' SBP
#' DBP
#' Cognition 2012
#' Cognition 2014
#' Cognition 2016
#' Cognition 2018
#' Race
#' Birthplace
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(elsa_analysis_long) <- c(
  "id","r5iwstat","r6iwstat","r7iwstat","r8iwstat","r9iwstat","r5agey","age","r7agey","r8agey","r9agey",
  "gender","raeduc_e","edu","mother_edu","father_edu","marital_status","r7mstat","r8mstat","r9mstat","birthplace","race","hh5ctot1m","consumption","hh7ctot1m","hh8ctot1m","hh9ctot1m","bmi","r8mbmi","r6mbmicat","r8mbmicat","r5hibpe","r6hibpe","r7hibpe","r8hibpe","r9hibpe","r5rxhibp","r6rxhibp","r7rxhibp","r8rxhibp","r9rxhibp","sbp","r8systo","dbp","r8diato","r5smokev","r5smoken","r6smokev","r6smoken","r7smokev","r7smoken","r8smokev","r8smoken","r9smokev","r9smoken","r5drink","r5drinkwn_e","r6drink","r6drinkwn_e","r7drink","r7drinkwn_e","r8drink","r8drinkwn_e","r9drink","r9drinkwn_e","sleepr","r6vgactx_e","r6mdactx_e","r6ltactx_e","r6socyr","r5iwindm","r6iwindm","r7iwindm","r8iwindm","r9iwindm","r5iwindy","r6iwindy","r7iwindy","r8iwindy","r9iwindy","h5atotb","h6atotb","h7atotb","h8atotb","h9atotb","hh5hhresp","hh6hhresp","hh7hhresp","hh8hhresp","hh9hhresp","r6lwtresp","cognition5","cognition6","cognition7","cognition8","cognition9","riwmid_w5","riwmid_w6","riwmid_w7","riwmid_w8","riwmid_w9","agecat","parental_high_education","smoking","drinking","sleep_was_restless","physical_activity","social","equivalized_wealth","wealth","htn_2012","htn_2016","ht_status2012","ht_status2016","education","parental_education","if_treated","if_hyper","if_treat","time","cognition","wave6_date")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(elsa_analysis_long, "elsa.csv")

#' 
