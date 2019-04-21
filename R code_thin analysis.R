
If (!require(devtools)) install.packages("devtools") 
Devtools::install_git('https://gitlab.phe.gov.uk/packages/DataLakeR') 

library(DataLakeR)


#Get data back from the lake
test <- get_datalake("SELECT * FROM THIN_Analysis.dbo.AleksandraBlawat_patients")

#get summary statistics 

install.packages("tidyverse")
library(tidyverse)

health <- data_frame(age, condition, duration, patid, realconsultationdate)
health$patid <- as.numeric(patid)
length(unique(health$patid))
health %>% 
  group_by(condition) %>% 
  summarize(mean_duration = mean(duration), number_patients = length(unique(patid))) %>% 
  mutate(number_visits = length(duration) / length(unique(patid)))




