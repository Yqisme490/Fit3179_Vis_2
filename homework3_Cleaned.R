setwd("C:/Users/user/Desktop")
rm(list=ls())

library(tidyverse)
library(fs)

#Remove colname special char
ausDs <- read.csv("AustraliaDataScienceJobs.csv")
colnames(ausDs) <- gsub('[^[:alnum:] ]', '', colnames(ausDs))
colnames(ausDs)[26:53] <- substr(colnames(ausDs)[26:53],1,nchar(colnames(ausDs)[26:53])-2) #remove yn
colnames(ausDs)[33] <- "c++"
write.csv(ausDs,"AustraliaDataScience.csv", row.names = FALSE)

id <- c(0:7)
State <- c("New South Wales","Victoria","Queensland","South Australia","Western Australia","Tasmania","Northern Territory","Australian Capital Territory")
keyPair <- data.frame(id, State)

ausDs <- ausDs %>% group_by(State) %>% summarise(Estimate.Base.Salary = mean(Estimate.Base.Salary))
write.csv(ausDs,"AustraliaDataScienceEBS.csv", row.names = FALSE)
#-------------------------------------------------------------------
countDs <- ausDs %>% group_by(JobTitle) %>% summarise(countVal = length(JobTitle)) %>% arrange(desc(countVal))
top10Popular <- head(countDs,5)
top10Popular <- inner_join(ausDs, top10Popular, by = "JobTitle")
skillRequired <- pivot_longer(top10Popular, cols=26:53, names_to = "skill", values_to = "required")
skillRequired <- skillRequired[c(1,28,29)]
#skillRequired  <- skillRequired  %>% group_by(JobTitle,skill) %>% summarise(countVal = sum(required))
skillRequired <- subset(skillRequired, required != 0)
write.csv(skillRequired,"top5Skills_2.csv", row.names = FALSE)
#-------------------------------------------------------------------
countDs <- ausDs %>% group_by(CompanyIndustry) %>% summarise(countVal = length(CompanyIndustry)) %>% arrange(desc(countVal))
countDs <- subset(countDs, CompanyIndustry != '')
#-------------------------------------------------------------------
countDs <- ausDs %>% group_by(Job.Title) %>% summarise(countVal = length(Job.Title)) %>% arrange(desc(countVal))
skillRequired <- pivot_longer(countDs, cols=26:53, names_to = "skill", values_to = "required")
skillRequired  <- skillRequired  %>% group_by(skill) %>% summarise(countVal = sum(required))
skillRequired <- subset(skillRequired, countVal != 0)
colnames(skillRequired) <- c("category","amount")
write.csv(skillRequired,"importantSkills.csv", row.names = FALSE)

#-------------------------------------------------------------------
ausDs2 <- read.csv("listings2019_2022.csv")
ausDs2 <- pivot_longer(ausDs2, cols=25:49, names_to = "skill", values_to = "required")
ausDs2 <- ausDs2 %>% group_by(jobTitle,skill) %>% summarise(countVal = sum(required))
write.csv(ausDs2,"listings2019_2022_2.csv", row.names = FALSE)