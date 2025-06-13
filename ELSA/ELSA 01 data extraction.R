#' hypertension and cognition
#' data cleaning
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(haven)
library(naniar)
library(dplyr)

#' ELSA: wave5-9 (2010-2018)
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
elsa <- haven::read_dta("H_ELSA_g3.dta")

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#demographic info
demog <- subset(elsa, select = c(idauniqc, # unique individual serial number /6-char
                                    r5iwstat, r6iwstat, r7iwstat, r8iwstat, r9iwstat,#wave status, interview status
                                    r5agey, r6agey, r7agey, r8agey, r9agey,#age (years) at interview (midmon)
                                    ragender,#sex
                                    raeduc_e, raeducl,#edu
                                    ramomeduage, radadeduage,#parental edu
                                    r6mstat, r7mstat, r8mstat, r9mstat,#marital status
                                    rabplace,# place of birth
                                    raracem,#race (white/non-white)
                                    hh5ctot1m, hh6ctot1m, hh7ctot1m, hh8ctot1m, hh9ctot1m,# household expenditure,
                                    r6mbmi, r8mbmi,#bmi (only wave 6 & 8 available)
                                    r6mbmicat, r8mbmicat,#bmi categories
                                    r5hibpe, r6hibpe, r7hibpe,r8hibpe, r9hibpe,#ever hypertension
                                    r5rxhibp, r6rxhibp, r7rxhibp,r8rxhibp, r9rxhibp, #any hp modern medicine
                                    r6systo, r8systo,#average blood pressure measure (systolic) only wave 6 and 8
                                    r6diasto, r8diasto,#average blood pressure measure (diastolic) only wave 6 and 8
                                    r5smokev, r5smoken,r6smokev, r6smoken,r7smokev, r7smoken,r8smokev, r8smoken,r9smokev, r9smoken,#ever and current smoking
                                    r5drink, r5drinkwn_e,r6drink, r6drinkwn_e,r7drink, r7drinkwn_e,r8drink, r8drinkwn_e,r9drink, r9drinkwn_e,#currently drinks, and number of day/week drinks
                                    r6sleepr,#sleep was restless
                                    r6vgactx_e,#vigorous physical activity
                                    r6mdactx_e,#moderate physical activity
                                    r6ltactx_e,#light physical activity
                                    r6socyr,#yearly social activity
                                 r5iwindm,r6iwindm,r7iwindm,r8iwindm,r9iwindm,r5iwindy,r6iwindy,r7iwindy,r8iwindy,r9iwindy,#interview time
                                 h5atotb,h6atotb,h7atotb,h8atotb,h9atotb,##HHold total wealth
                                 hh5hhresp,hh6hhresp,hh7hhresp,hh8hhresp,hh9hhresp,#respondents in household
                                 r6lwtresp#weight
))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#outcome: memory (20 points) + mental status (10 points)
#RwBWC20: R able to count backwards from 20
cognition <- subset(elsa, select = c(r5tr20, #r5orient, #ser7 and bwc20 are not provided in wave 5 and 6
                                        r6tr20, #r6orient,
                                        r7tr20, #r7orient, #r7ser7, r7bwc20,
                                        r8tr20, #r8orient, #r8ser7, r8bwc20,
                                        r9tr20#, r9orient#, r9ser7, r9bwc20
))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#check the NA in each cognition measures
library(naniar)
mdcog <- cognition %>%
  miss_var_summary()
mdcog
#Note: there are more NAs in delay recall than immediate recall. 
#The tr20 is the sum of delay and immediate, which makes its N is close to the N of delayed.
#Alternatively, we can take the average of two recall measures if both have no NA; 
#if one has NA, especially delay, we use the immediate as the memory measure. 
#By doing this, it can slightly ease the NA issue

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#check summary
summary(cognition$r6tr20)#0-20
#summary(cognition$r7ser7)#0-5
#summary(cognition$r7orient)#0-4
#summary(cognition$r7bwc20)#0-2

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##covert draw from charater to numeric
#cognition[, c(4, 8, 12, 16)] <- lapply(cognition[, c(4, 8, 12, 16)], as.numeric)
#summary(cognition$r1draw)

#' 
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cognition$cognition5 <- cognition$r5tr20
summary(cognition$cognition5)

cognition$cognition6 <- cognition$r6tr20
summary(cognition$cognition6)
hist(cognition$cognition6)

cognition$cognition7 <- cognition$r7tr20
summary(cognition$cognition7)

cognition$cognition8 <- cognition$r8tr20
summary(cognition$cognition8)

cognition$cognition9 <- cognition$r9tr20
summary(cognition$cognition9)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#only keep the total cognition score we need
cognition <- cognition[, 6:10]

elsa_harmonized <- cbind(demog, cognition)

elsa_analysis <- elsa_harmonized

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#rename variables
elsa_analysis <- elsa_analysis %>%
  rename(age = r6agey, gender = ragender, education = raeducl, mother_edu = ramomeduage, father_edu = radadeduage, marital_status = r6mstat, birthplace = rabplace, race = raracem, consumption = hh6ctot1m, bmi = r6mbmi,wealth = h6atotb)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#interview time
elsa_analysis$riwmid_w5<-  elsa_analysis$r5iwindy + elsa_analysis$r5iwindm/12
elsa_analysis$riwmid_w6<-  elsa_analysis$r6iwindy + elsa_analysis$r6iwindm/12
elsa_analysis$riwmid_w7<-  elsa_analysis$r7iwindy + elsa_analysis$r7iwindm/12
elsa_analysis$riwmid_w8<-  elsa_analysis$r8iwindy + elsa_analysis$r8iwindm/12
elsa_analysis$riwmid_w9<-  elsa_analysis$r9iwindy + elsa_analysis$r9iwindm/12

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Save data
write.csv(elsa_analysis, "elsa_analysis.csv")

