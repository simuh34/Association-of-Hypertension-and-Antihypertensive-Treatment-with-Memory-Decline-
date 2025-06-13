model_1 <- lmer(cognition ~ if_hyper*time *country + age + gender + education +
                marital_status + wealth + 
                bmi + smoking + drinking + sleep_was_restless + physical_activity + social_activity +
                (1|ID), data = pooled_data)

model_2 <- lmer(cognition ~ if_treat*time *country + age + gender + education +
                  marital_status + wealth + 
                  bmi + smoking + drinking + sleep_was_restless + physical_activity + social_activity +
                  (1|ID), data = pooled_data)



