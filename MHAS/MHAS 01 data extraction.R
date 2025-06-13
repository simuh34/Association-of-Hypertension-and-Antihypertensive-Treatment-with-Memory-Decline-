####1. Load package####
library(haven)
library(dplyr)

####2. Import MHAS harmonized data####
HMHAS_D <- haven::read_dta("H_MHAS_c.dta")

####3. Extract relevant variables for analysis
MHAS_analysis <- subset(HMHAS_D, select = c(np, #personel id
                                            r3iwstat, r4iwstat, r5iwstat,#wave status, interview status
                                            r3agey, r4agey, r5agey,#age at interview
                                            ragender,#sex
                                            raeducl,#edu
                                            rameduc_m,
                                            rafeduc_m,#parental edu
                                            r3mstat,r4mstat,r5mstath,#marital status
                                            h3rural,h4rural,h5rural,#live in rural/urban
                                            hh3ctot1m,hh4ctot1m,hh5ctot1m,# household expenditure,
                                            r3mbmi,#bmi##Measured Body Mass Index=kg/m2##only wave3
                                            r3bmi,r4bmi,r5bmi,#bmi##Body Mass Index=kg/m2（the respondent's self-reported）
                                            r3mbmicat,#bmi categories##Measured Body Mass Index Categorization
                                            r3hibpe, r4hibpe, r5hibpe,#ever hypertension
                                            r3rxhibp, r4rxhibp, r5rxhibp,#takes meds for high blood pressure
                                            r3systo,#average blood pressure measure (systolic) 1 & 2
                                            r3diasto,#average blood pressure measure (diastolic) 1 & 2
                                            r3smokev, r4smokev,r5smokev,#smoke ever
                                            r3smoken, r4smoken,r5smoken,#current smoking
                                            r3drink,r3drinkd,r4drink,r4drinkd,r5drink,r5drinkd,#ever drinks any alcohol and Number of days/week drinks
                                            r3sleepr,r4sleepr,r5sleepr,#CESD-Sleep was restless
                                            r3rested,r4rested,r5rested,#feels rested when wakes up
                                            r3vigact,r4vigact,r5vigact,#Wtr vigorus phys act 3+/wk 
                                            r3socact_m,r4socact_m,r5socact_m,#social activities
                                            r3iwm,r4iwm,r5iwm,r3iwy,r4iwy,r5iwy,#interview time
                                            h3atotb, #HHold total wealth
                                            h3hhresp,#respondents in household
                                            h3hhres,#people living with
                                            r3wtresp)) #weight

#outcome
cognition <- subset(HMHAS_D, select = c(r3tr8_m,
                                        r4tr8_m,
                                        r5tr8_m))

##Serial 7’s starts from 4 wave

#check the NA in each cognition measures
library(naniar)
mdcog <- cognition %>%
  miss_var_summary()
head(mdcog)

#check the summary of outcome memory score
sapply(cognition[, c("r3tr8_m", "r4tr8_m", "r5tr8_m")], summary)

#select columns containing "tr" and dynamically rename them
cognition <- cognition %>%
  select(contains("tr")) %>%
  rename_with(~ paste0("cognition", seq_along(.)))

summary(cognition$cognition3)
hist(cognition$cognition1)#15716
MHAS_analysis <- cbind(MHAS_analysis, cognition)


prcl <- MHAS_analysis %>%
  miss_var_summary()

nrow(MHAS_analysis)#26839
nrow(cognition)#26839


#rename variables
MHAS_analysis <- MHAS_analysis %>%
  rename(age = r3agey, 
         gender = ragender, 
         education = raeducl, 
         mother_edu = rameduc_m, 
         father_edu = rafeduc_m, 
         marital_status = r3mstat, 
         urbanicity = h3rural, 
         consumption = hh3ctot1m, 
         bmi = r3mbmi,
         wealth = h3atotb)

#interview time
#calculate the precise interview time for each wave in years
#combine the interview year (rXiwy) and the interview month (rXiwm) 
#by converting the month to a fraction of a year (month / 12)
#this results in a more accurate representation of the interview date.
MHAS_analysis$riwmid_w3<-  MHAS_analysis$r3iwy + MHAS_analysis$r3iwm/12
MHAS_analysis$riwmid_w4<-  MHAS_analysis$r4iwy + MHAS_analysis$r4iwm/12
MHAS_analysis$riwmid_w5<-  MHAS_analysis$r5iwy + MHAS_analysis$r5iwm/12


####4. Extract data for next steps####
write.csv(MHAS_analysis, "MHAS_analysis_extract.csv")

summary(MHAS_analysis)

