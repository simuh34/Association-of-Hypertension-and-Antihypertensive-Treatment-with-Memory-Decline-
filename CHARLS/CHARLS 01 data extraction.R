####1. Load package####
library(haven)
library(dplyr)

####2. Import CHARLS harmonized data####
HCHARLS_D <- haven::read_dta("H_CHARLS_D_Data.dta")

####3. Extract relevant variables for analysis####
CHARLS_analysis <- subset(HCHARLS_D, select = c(ID, #personel id
                                      r1iwstat, r2iwstat, r3iwstat, r4iwstat,#wave status, interview status
                                      r1agey, r2agey, r3agey, r4agey,#age at interview
                                      ragender,#sex
                                      raeduc_c, raeducl,#edu
                                      ramomeducl,
                                      radadeducl,#parental edu
                                      r1mstat, r2mstat, r3mstat, r4mstat,#marital status
                                      r1hukou, r2hukou, r3hukou, r4hukou,#hukou
                                      h1rural, h2rural, h3rural, h4rural,#live in rural/urban
                                      hh1cperc, hh2cperc, hh3cperc,# household expenditure,
                                      r1mbmi, r2mbmi, r3mbmi,#bmi
                                      r1mbmicata, r2mbmicata, r3mbmicata,#bmi categories
                                      r1hibpe, r2hibpe, r3hibpe, r4hibpe,#ever hypertension
                                      r1rxhibp, r2rxhibp, r3rxhibp, r4rxhibp,#any hp modern medicine
                                      r1rxhibp_c, r2rxhibp_c, r3rxhibp_c, r4rxhibp_c,#any hp medicine
                                      r1systo, r2systo, r3systo,#average blood pressure measure (systolic) 2 & 3
                                      r1diasto, r2diasto, r3diasto, #average blood pressure measure (diastolic) 2 & 3
                                      r1smokev, r1smoken,#ever and current smoking
                                      r1drinkl, r1drinkr_c,#drinking last year and number of drinking per day
                                      r1sleeprl,#sleep was restless
                                      r1mdactx_c,#days/wk moderate physical activity or exercise 
                                      r1vgactx_c,# days/wk light physical activity or exercise
                                      r1socwk,#r participate in social activities last month
                                      r1iwm,r2iwm,r3iwm,r4iwm,r1iwy,r2iwy,r3iwy,r4iwy,#interview time
                                      hh1atotb, #HHold total wealth
                                      h1hhresp, #respondents in household
                                      r1wtresp #weight
                                      ))

#outcome: memory
#only use memory (20 points)-rwtr20
#The tr20 is the sum of delay and immediate, 
cognition <- subset(HCHARLS_D, select = c(ID,
                                          r1tr20, 
                                          #total recall, serial subtraction, orientation, drawing
                                          r2tr20,
                                          r3tr20))

#impute cognition score of 2018
cognition_test <- read.csv("D:/hypertension/CHARLS/charls_2018_cognition.csv")
cognition_test$r4tr20 <- cognition_test$r4imrc_eqt + cognition_test$r4dlrc_eqt
cognition$ID <- as.numeric(cognition$ID)
cognition_test$ID <- as.numeric(cognition_test$ID)
cognition <- inner_join(cognition, cognition_test, by =  "ID")

#check the NA in each cognition measures
library(naniar)
mdcog <- cognition %>%
  miss_var_summary()
#Note: there are more NAs in delay recall than immediate recall. 
#The tr20 is the sum of delay and immediate, which makes its N is close to the N of delayed.
#Alternatively, we can take the average of two recall measures if both have no NA; 
#if one has NA, especially delay, we use the immediate as the memory measure. 
#By doing this, it can slightly ease the NA issue

#check the summary of outcome memory score
sapply(cognition[, c("r1tr20", "r2tr20", "r3tr20", "r4tr20")], summary)

#select columns containing "tr" and dynamically rename them
cognition <- cognition %>%
  select(contains("tr")) %>%
  rename_with(~ paste0("cognition", seq_along(.)))

summary(cognition$cognition1)
hist(cognition$cognition1)
CHARLS_analysis <- cbind(CHARLS_analysis, cognition)

#rename variables
CHARLS_analysis <- CHARLS_analysis %>%
                    rename(age = r1agey, 
                           gender = ragender, 
                           education = raeducl, 
                           mother_edu = ramomeducl, 
                           father_edu = radadeducl, 
                           marital_status = r1mstat, 
                           hukou = r1hukou, 
                           urbanicity = h1rural, 
                           consumption = hh1cperc, 
                           bmi = r1mbmi,
                           wealth = hh1atotb)

#interview time
#calculate the precise interview time for each wave in years
#combine the interview year (rXiwy) and the interview month (rXiwm) 
#by converting the month to a fraction of a year (month / 12)
#this results in a more accurate representation of the interview date.
CHARLS_analysis$riwmid_w1<-  CHARLS_analysis$r1iwy + CHARLS_analysis$r1iwm/12
CHARLS_analysis$riwmid_w2<-  CHARLS_analysis$r2iwy + CHARLS_analysis$r2iwm/12
CHARLS_analysis$riwmid_w3<-  CHARLS_analysis$r3iwy + CHARLS_analysis$r3iwm/12
CHARLS_analysis$riwmid_w4<-  CHARLS_analysis$r4iwy + CHARLS_analysis$r4iwm/12



####4. Extract data for next steps####
write.csv(CHARLS_analysis, "CHARLS_analysis.csv")

