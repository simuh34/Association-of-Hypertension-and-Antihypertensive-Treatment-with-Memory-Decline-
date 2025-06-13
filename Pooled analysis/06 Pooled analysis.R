library(dplyr)
library(data.table)
library(lme4)
library(sjPlot)
library(lmerTest)
library(broom.mixed)
library(tableone)
library(kableExtra)
library(ggplot2)
library(readxl)

#load data
file_list <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE)
for (file in file_list) {
  data_name <- tools::file_path_sans_ext(tolower(basename(file)))
  assign(data_name, read.csv(file))
}

hrs <- hrs %>%
  select(ID = hhidpn,
         age = age,   
         gender = ragender,
         education = raeducl,
         marital_status = marital_status,
         wealth = wealth_quartile,
         bmi = bmi,
         smoking = smoke_2010,
         drinking = drink_ever,
         sleep_was_restless = restless,
         physical_activity = phy_act,
         social_activity = social_act,
         if_hyper = if_hyper,
         if_treat = if_treat,
         ht_status = ht_status2010,
         time = time,
         cognition = cognition)

elsa <- elsa %>%
  select(ID = id,
         age = age,
         gender = gender,
         education = education,
         marital_status = marital_status,
         wealth = wealth,
         bmi = bmi,
         smoking = smoking,
         drinking = drinking,
         sleep_was_restless = sleep_was_restless,
         physical_activity = physical_activity,
         social_activity = social,
         ht_status = htn_2012,
         if_hyper = if_hyper,
         if_treat = if_treat,
         time = time,
         cognition = cognition)

table(elsa$if_hyper)
elsa$if_hyper <- factor(elsa$if_hyper,c("non hypertensive","hypertensive"))
table(elsa$if_treat)

mhas <- mhas %>%
    select(ID = np,
           age = age,
           gender = gender,
           education = education,
           marital_status = marital_status,
           wealth = wealth_quartile,
           bmi = bmi,
           smoking = smoke_2012,
           drinking = drink_ever,
           sleep_was_restless = restless,
           physical_activity = phy_act,
           social_activity = social_act,
           if_hyper = if_hyper,
           if_treat = if_treat,
           ht_status = ht_status2012,
           time = time,
           cognition = cognition)

charls <- charls %>%
  select(ID = ID,
         age = age,
         gender = gender,
         education = education,
         marital_status = marital_status,
         wealth = wealth_quartile,
         bmi = bmi,
         smoking = smoke_2011,
         drinking = drink_ever,
         sleep_was_restless = restless,
         physical_activity = phy_act,
         social_activity = social_act,
         if_hyper = if_hyper,
         if_treat = if_treat,
         ht_status = ht_status2011,
         time = time,
         cognition = cognition)


#group the data
elsa$country <- "England"
hrs$country <- "USA"
mhas$country <- "Mexico"
charls$country <- "China"

pooled_data <- rbindlist(
  list(share, elsa, hrs, mhas, charls),
  use.names = TRUE,  
  fill = TRUE        
)

pooled_data <- pooled_data %>%
  mutate(
    wealth = case_when(
      wealth == 1 | wealth == "low"  ~ "lower",   
      wealth == 2 | wealth == "upper" ~ "higher", 
      wealth == "high" ~ "higher",                
      TRUE ~ as.character(wealth)                       
    )
  )

pooled_data <- pooled_data %>%
  mutate(
    drinking = case_when(
      drinking == 0   ~ "no",   
      drinking == 1   ~ "yes", 
      TRUE ~ as.character(drinking)                       
    )
  )

pooled_data <- pooled_data %>%
  mutate(
    sleep_was_restless = case_when(
      sleep_was_restless == 0   ~ "no",   
      sleep_was_restless == 1   ~ "yes", 
      TRUE ~ as.character(sleep_was_restless)                       
    )
  )

pooled_data <- pooled_data %>%
  mutate(
    social_activity = case_when(
      social_activity == 0   ~ "no",   
      social_activity == 1   ~ "yes", 
      TRUE ~ as.character(social_activity)                       
    )
  )

pooled_data <- pooled_data %>%
  mutate(
    physical_activity = case_when(
      physical_activity == 0   ~ "no",   
      physical_activity == 1   ~ "yes", 
      TRUE ~ as.character(physical_activity)                       
    )
  )

lapply(pooled_data, function(x) {
  if (is.factor(x) || is.character(x)) {
    table(x, useNA = "ifany")  
  } else {
    summary(x)  
  }
})

write.csv(pooled_data, "pooled_analysis.csv")
# pooled_data <- read.csv("pooled_analysis.csv")
pooled_data$country <- factor(pooled_data$country, levels = c("USA","England","Mexico","China"))
pooled_data$if_treat <- factor(pooled_data$if_treat, levels = c("non hypertensive","hypertensive and treated","hypertensive and untreated"))

#Table
t1 = CreateTableOne(vars = c("age","gender","if_treat","education","parental_education","marital_status","wealth","bmi","smoking","drinking","sleep_was_restless","physical_activity","social_activity"), 
                    data=pooled_data, strata = "country")
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

table_data <- print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE)
table_df <- as.data.frame(table_data)
write.csv(table_df, file = "descriptive_characteristics.csv", row.names = TRUE)

#Pooled Analysis in Regression Models
cols_to_factor <- c("if_hyper", "country", "education", 
                    "marital_status", "wealth", "smoking", "drinking",
                    "sleep_was_restless", "physical_activity", "social_activity","gender")
pooled_data <- pooled_data %>%
  mutate(across(all_of(cols_to_factor), as.factor))

cols_to_numeric <- c("age", "time", "bmi")
pooled_data <- pooled_data %>%
  mutate(across(all_of(cols_to_numeric), as.numeric))

pooled_data$if_hyper <- factor(pooled_data$if_hyper, levels = c("non hypertensive", "hypertensive"))
pooled_data$if_treat <- factor(pooled_data$if_treat, levels = c("non hypertensive", "hypertensive and treated","hypertensive and untreated"))

model_1 <- lmer(cognition ~ if_hyper*time *country + age + gender + education +
                marital_status + wealth + 
                bmi + smoking + drinking + sleep_was_restless + physical_activity + social_activity +
                (1|ID), data = pooled_data)
summary(model_1)

library(broom.mixed)
t1 <- tidy(model_1, conf.int = TRUE) %>% 
  filter(grepl("if_hyper.*time.*country", term)) 
tab_model(model_1)

coef_p <- coef(summary(model_1))
coef_p_df <- as.data.frame(coef_p)
colnames(coef_p_df)[5] <- "p.value"
coef_p_df$term <- rownames(coef_p_df)
numeric_cols <- sapply(coef_p_df, is.numeric)
coef_p_df[numeric_cols] <- round(coef_p_df[numeric_cols], 2)
conf_int <- confint(model_1, method = "Wald", level = 0.95)
conf_int_df <- as.data.frame(conf_int)
conf_int_df$term <- rownames(conf_int_df)
colnames(conf_int_df) <- c("conf.low", "conf.high", "term")
result <- merge(coef_p_df, conf_int_df, by = "term")
result <- result[, c("term", "Estimate", "conf.low", "conf.high", "p.value")]
colnames(result)[2] <- "estimate"
numeric_cols_result <- sapply(result, is.numeric)
result[numeric_cols_result] <- round(result[numeric_cols_result], 2)

print(result)

model_2 <- lmer(cognition ~ if_treat*time *country + age + gender + education +
                  marital_status + wealth + 
                  bmi + smoking + drinking + sleep_was_restless + physical_activity + social_activity +
                  (1|ID), data = pooled_data)
library(broom.mixed)
result <- tidy(model_2, conf.int = TRUE) %>% 
  filter(grepl("if_treat.*time.*country", term))
View(result)
tab_model(model_2)


coef_p <- coef(summary(model_2))
coef_p_df <- as.data.frame(coef_p)
colnames(coef_p_df)[5] <- "p.value"
coef_p_df$term <- rownames(coef_p_df)
numeric_cols <- sapply(coef_p_df, is.numeric)
coef_p_df[numeric_cols] <- round(coef_p_df[numeric_cols], 2)
conf_int <- confint(model_2, method = "Wald", level = 0.95)
conf_int_df <- as.data.frame(conf_int)
conf_int_df$term <- rownames(conf_int_df)
colnames(conf_int_df) <- c("conf.low", "conf.high", "term")
result <- merge(coef_p_df, conf_int_df, by = "term")
result <- result[, c("term", "Estimate", "conf.low", "conf.high", "p.value")]
colnames(result)[2] <- "estimate"
numeric_cols_result <- sapply(result, is.numeric)
result[numeric_cols_result] <- round(result[numeric_cols_result], 2)

print(result)

####country difference plot####


library(readxl)
library(ggplot2)
df <- read_excel('Model results.xlsx')
levels <- c("US", "England", "Mexico", "China")
df$COUNTRY <- factor(df$COUNTRY, levels = levels) 

jpeg("if_hyper_countryname.jpeg", width = 10, height = 6, units = 'in', res = 250)
fig <- ggplot(data = df,
              aes(x = Estimates, y = COUNTRY,
                  xmin = lci, xmax = uci, color = STATUS)) +
  geom_point(position = position_dodge(width = 1.0), size = 5) +
  geom_errorbar(
    position = position_dodge(width = 1.0), 
    width = 0.2,
    size = 1.5
  ) + 
  scale_y_discrete(limits = rev(levels(df$COUNTRY))) + 
  scale_x_continuous(limits = c(-0.04, 0.04), breaks =  c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04) ) + 
  geom_vline(xintercept = 0.0, color = "black", linetype = 2) +
  theme_bw() +
  xlab("Estimates (95% CI)") +
  ylab("") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  labs(color = "HT status") +
  scale_color_manual(values = rep("#1E6AE1", length(unique(df$STATUS))))

print(fig) 
dev.off()

####if treat####
df <- read_excel('Model results.xlsx', sheet = 'intermediate')
# levels <- c("US * Non hypertensive","US * Hypertensive and treated","US * Hypertensive and untreated","China * Non hypertensive","China * Hypertensive and treated","China * Hypertensive and untreated","Mexico * Non hypertensive","Mexico * Hypertensive and treated","Mexico * Hypertensive and untreated","Europe * Non hypertensive","Europe * Hypertensive and treated","Europe * Hypertensive and untreated","England * Non hypertensive","England * Hypertensive and treated" ,"England * Hypertensive and untreated")
df$COUNTRY <- factor(df$COUNTRY) 

jpeg("if_treat.jpeg", width =10, height = 6, units = 'in', res = 250)
levels <- c("US", "England", "Mexico", "China")
df$COUNTRY <- factor(df$COUNTRY, levels = rev(levels))  
df$STATUS <- factor(df$STATUS, levels = c("Untreated HT", "Treated HT"))  

fig <- df %>% 
  ggplot(aes(x = Estimates, y = COUNTRY, xmin = lci, xmax = uci, color = STATUS)) + 
  geom_point(position = position_dodge(width = 1.0), size = 5, alpha = 1.0) +
  geom_errorbar(position = position_dodge(width = 1.0), width = 0.4, size = 1.5) + 
  scale_x_continuous(limits = c(-0.04, 0.04), breaks =  c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04) )+
  geom_vline(xintercept = 0.0, color = "black", linetype = 2) +
  theme_bw() +
  xlab("Estimates (95% CI)") +
  ylab("") +
  scale_color_manual(
    name = "Antihypertensive treatment status",
    values = c(
      "Untreated HT" = "#1EBCE1",  
      "Treated HT" = "#122F84"
    )
  ) +
  theme(
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.position = "bottom"  
  )



fig
dev.off()



