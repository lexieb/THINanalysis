If (!require(devtools)) install.packages("devtools") 
Devtools::install_git('https://gitlab.phe.gov.uk/packages/DataLakeR') 

library(DataLakeR)


#Get data back from the lake
test <- get_datalake("SELECT * FROM THIN_Analysis.dbo.AleksandraBlawat_patients")

#get summary statistics 

install.packages("tidyverse")
library(tidyverse)

#code for testing
patid <- c(1, 2, 3, 4, 4, 5)
condition <- c("breastcancer", "diabetes", "chd", "stroke", "diabetes", "none")
duration <- c(40, 5, 5, 4, 3, 5)
age <- c(3, 5, 6, 7, 8, 9)
health <- tibble(age, condition, duration, patid)

health <- data_frame(age, condition, duration, patid, realconsultationdate)
health$patid <- as.numeric(patid)
length(unique(health$patid))

#need to calculate whether or not a patient has condition - create variable 'diseases' to separate patients into which condition/MM they have as not each of their constulations will be for the named conditions 
health <- health %>%
  group_by(patid) %>%
  mutate(has_diabetes = max((condition  == "diabetes"))) %>% 
  mutate(has_breastcancer = max((condition == "breastcancer"))) %>%
  mutate(has_chd = max((condition == "chd"))) %>%
  mutate(has_stroke = max((condition == "stroke")))

health$diseases <- ifelse((health$has_diabetes * health$has_stroke * health$has_breastcancer * health$has_chd) == 1, 1, 
                         + ifelse((health$has_diabetes * health$has_stroke * health$has_chd) == 1, 2, 
                         + ifelse((health$has_diabetes * health$has_stroke * health$has_breastcancer) == 1, 3,
                         + ifelse((health$has_diabetes * health$has_breastcancer * health$has_chd) == 1, 4,
                         + ifelse((health$has_stroke * health$has_breastcancer * health$has_chd) == 1, 5, ifelse((health$has_diabetes * health$has_chd) == 1, 6,
                         + ifelse((health$has_diabetes * health$has_breastcancer) == 1, 7,
                         + ifelse((health$has_diabetes * health$has_stroke) == 1, 8,
                         + ifelse(health$has_diabetes == 1, 9,
                         + ifelse((health$has_chd * health$has_breastcancer) == 1, 10,
                         + ifelse((health$has_stroke * health$has_breastcancer) == 1, 11, 
                         + ifelse((health$has_chd * health$has_stroke) == 1, 12,
                         + ifelse(health$has_chd == 1, 13, 
                         + ifelse(health$has_breastcancer == 1, 14, 0))))))))))))))
                        

 # to get the number of different patients in each disease group
health %>% 
   unique() %>%
  group_by(diseases) %>% 
  summarize(count = n_distinct(patid))

#drop patients without any of the specified diseases/MM
health <- subset(health, diseases != 0)

# haven't tested this bit yet
health %>% 
  group_by(patid, diseases) %>%
  mutate(earliest_flagged_visit = min(realconsultationdate)) 



