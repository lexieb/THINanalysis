install.packages("devtools") 
devtools::install_git('https://gitlab.phe.gov.uk/packages/DataLakeR') 

library(DataLakeR)


#Get data back from the lake
#test <- get_datalake("SELECT * FROM THIN_Analysis.dbo.AleksandraBlawat_patients")

#get summary statistics 

install.packages("tidyverse")
library(tidyverse)

#code for testing
patid <- c(1, 2, 3, 4, 4, 5)
condition <- c("breastcancer", "diabetes", "chd", "stroke", "diabetes", "none")
duration <- c(40, 5, 5, 4, 3, 5)
age <- c(3, 5, 6, 7, 90, 90)
medcode <- c(534, 623, 432, 564, 231, 222)
medflag <- c("R", "S", "S", "R", "S", "S")
patflag <- c("A", "C", "A", "A", "A", "A")
category <- c(1, 2, 3, 4, 1, 1)
regstat <- c(01, 01, 02, 05, 99, 99)
xferdate <- c(00000000, 00000000, 20020220, 00000000, 00000000, 00000000)
sex <- c(1, 2, 1, 2, 1, 1)
regdate <- c(20000220, 20010212, 19990212, 19930212, 19930212, 19930212)
evdatereal <- c(20050821, 20100212, 20090113, 20080101, 20100503, 20110402)
deathdate <- c(00000000, 00000000, 00000000, 00000000, 20150603, 20150603)
health <- tibble(age, condition, duration, patid, medcode, medflag, patflag, category, regstat, xferdate, sex, regdate, evdatereal)

#need to calculate whether or not a patient has condition - create variable 'diseases' to separate patients into which condition/MM they have as not each of their constulations will be for the named conditions 
health <- health %>%
  group_by(patid) %>%
  mutate(has_diabetes = max((condition  == "diabetes"))) %>% 
  mutate(has_breastcancer = max((condition == "breastcancer"))) %>%
  mutate(has_chd = max((condition == "chd"))) %>%
  mutate(has_stroke = max((condition == "stroke")))

health$diseases2 <- case_when((health$has_diabetes * health$has_stroke * health$has_breastcancer * health$has_chd) == 1 ~ 1, 
                          (health$has_diabetes * health$has_stroke * health$has_chd) == 1 ~ 2, 
                          (health$has_diabetes * health$has_stroke * health$has_breastcancer) == 1 ~ 3,
                          (health$has_diabetes * health$has_breastcancer * health$has_chd) == 1 ~ 4,
                          (health$has_stroke * health$has_breastcancer * health$has_chd) == 1 ~ 5, 
                          (health$has_diabetes * health$has_chd) == 1 ~ 6,
                          (health$has_diabetes * health$has_breastcancer) == 1 ~ 7,
                          (health$has_diabetes * health$has_stroke) == 1 ~ 8,
                          health$has_diabetes == 1 ~ 9,
                          (health$has_chd * health$has_breastcancer) == 1 ~ 10,
                          (health$has_stroke * health$has_breastcancer) == 1 ~ 11, 
                          (health$has_chd * health$has_stroke) == 1 ~ 12,
                          health$has_chd == 1 ~ 13, 
                          health$has_breastcancer == 1 ~ 14, 
                          TRUE ~ 0)
                        

 # to get the number of different patients in each disease group
health %>% 
   unique() %>%
  group_by(diseases2) %>% 
  summarize(count = n_distinct(patid))

#format evdatereal, regdate, deathdate, and xferdate
health$evdatereal[health$evdatereal == 0] <- NA
health$xferdate[health$xferdate == 0] <- NA
health$regdate[health$regdate == 0] <- NA
health$deathdate[health$deathdate ==0] <- NA
health$evdaterealdate <- as.Date(strptime(health$evdatereal,format='%Y%m%d', tz = "GMT"))
health$deathdaterealdate <- as.Date(strptime(health$deathdate,format='%Y%m%d', tz = "GMT"))
health$xferrealdate <- as.Date(strptime(health$xferdate,format='%Y%m%d', tz = "GMT"))
health$regdatereal <- as.Date(strptime(health$regdate,format='%Y%m%d', tz = "GMT"))


#calculate difference between registation date and earliest of xferdate or deatdate 
leaving_date <- min(health$deathdaterealdate, health$xferrealdate)


#drop patients without any of the specified diseases/MM
health <- subset(health, diseases != 0)

# earliest consultation per patient per disease
health %>% 
  group_by(patid, condition) %>%
  summarize(earliest_flagged_visit = min(evdaterealdate)) 



