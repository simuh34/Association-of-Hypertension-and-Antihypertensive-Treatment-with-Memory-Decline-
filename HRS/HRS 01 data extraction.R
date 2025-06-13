####1. Load package####
library(haven)
library(dplyr)

randhrs<- haven::read_dta("randhrs1992_2020v2.dta")
hhrs <- haven::read_dta("H_HRS_c.dta")
#langa <- haven::read_dta("hrs\\cogfinalimp_9520wide.dta")
  



####2. Import HRS harmonized data####
subdat_demo <- subset(randhrs,
                      select = c(hhid,hhidpn, #household id + person number
                                 r10iwstat, r11iwstat,r12iwstat,r13iwstat,
                                 r10iwmid,r11iwmid,r12iwmid,r13iwmid,r14iwmid,r15iwmid,
                                 r10agey_m, r11agey_m,#age (years) at interview (midmon), RAND
                                 ragender,#sex, RAND
                                 raracem,rahispan,
                                 raeduc,#edu, RAND
                                 rameduc, rafeduc,#parental edu, RAND
                                 r10mstat, r11mstat,
                                 r10cenreg, r11cenreg,
                                 r10hibpe, r11hibpe,r12hibpe, r13hibpe,r14hibpe,r15hibpe,
                                 r10smokev, r10smoken,r11smokev, r11smoken,#ever and current smoking, RAND
                                 r10drink, r10drinkd,r11drink, r11drinkd,
                                 #currently drinks, and number of day/week drinks, RAND
                                 r10drinkn, #drinks/day when drinks
                                 r10sleepr,r11sleepr,
                                 r10vgactx,r11vgactx,
                                 r10mdactx,r11mdactx,
                                 h10atotb, h11atotb,#household wealth
                                 h10hhres, h11hhres,#people living with
                                 r10wtresp, r11wtresp #sampling weight
                                 ))

subdat_h <- subset(hhrs,
                   select = c(hhid,hhidpn, #household id + person number
                              raeducl,#edu, 
                              ramomeducl,radadeducl,
                              h10rural, h11rural, 
                              r10mbmi, r11mbmi,#bmi, HHC
                              r10bmicat, r11bmicat, 
                              r10rxhibp, r11rxhibp, r12rxhibp, r13rxhibp,r14rxhibp,  #any hp modern medicine. HHC
                              r10systo,  r11systo, r12systo, r13systo,r14systo,  #average blood pressure measure (systolic), HHC
                              r10diasto, r11diasto,r12diasto,r13diasto,r14diasto, #average blood pressure measure (diastolic)
                              r10socwk,r11socwk #social avtivity
                   ))                                

data01 <- merge(subdat_demo, subdat_h,by = c("hhid","hhidpn"))
# data01 <- merge(data01,langa, by = c("hhidpn"))

cognition <- subset(randhrs, select = c(hhid, hhidpn,r10tr20,
                                                     r11tr20,
                                                     r12tr20, 
                                                     r13tr20,
                                                     r14tr20p
                                                    ))
names(cognition) <- c("hhid","hhidpn","r10tr20","r11tr20","r12tr20","r13tr20","r14tr20")
data01 <- merge(data01, cognition, by = c("hhid","hhidpn"))

#interview time
base_date <- as.Date("1960-01-01") 
date_from_r10iwmid <- base_date + data01$r10iwmid 
date_from_r11iwmid <- base_date + data01$r11iwmid 
date_from_r12iwmid <- base_date + data01$r12iwmid 
date_from_r13iwmid <- base_date + data01$r13iwmid 
date_from_r14iwmid <- base_date + data01$r14iwmid 

data01$r10iwmid_year <- as.numeric(format(date_from_r10iwmid, "%Y"))
data01$r10iwmid_month <- as.numeric(format(date_from_r10iwmid, "%m"))

data01$r11iwmid_year <- as.numeric(format(date_from_r11iwmid, "%Y"))
data01$r11iwmid_month <- as.numeric(format(date_from_r11iwmid, "%m"))

data01$r12iwmid_year <- as.numeric(format(date_from_r12iwmid, "%Y"))
data01$r12iwmid_month <- as.numeric(format(date_from_r12iwmid, "%m"))

data01$r13iwmid_year <- as.numeric(format(date_from_r13iwmid, "%Y"))
data01$r13iwmid_month <- as.numeric(format(date_from_r13iwmid, "%m"))

data01$r14iwmid_year <- as.numeric(format(date_from_r14iwmid, "%Y"))
data01$r14iwmid_month <- as.numeric(format(date_from_r14iwmid, "%m"))

data01$riwmid_w1<-  data01$r10iwmid_year + data01$r10iwmid_month/12
data01$riwmid_w2<-  data01$r11iwmid_year + data01$r11iwmid_month/12
data01$riwmid_w3<-  data01$r12iwmid_year + data01$r12iwmid_month/12
data01$riwmid_w4<-  data01$r13iwmid_year + data01$r13iwmid_month/12
data01$riwmid_w5<-  data01$r14iwmid_year + data01$r14iwmid_month/12

####4. Extract data for next steps####
write.csv(data01, "HRS_analysis.csv")

