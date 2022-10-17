setwd("C:/Users/user/Desktop")
rm(list=ls())

library(tidyverse)
library(fs)
library(tibble)


#Remove colname special char
ausDs <- read.csv("AustraliaDataScienceJobs.csv")
colnames(ausDs) <- gsub('[^[:alnum:] ]', '', colnames(ausDs))
colnames(ausDs)[26:53] <- substr(colnames(ausDs)[26:53],1,nchar(colnames(ausDs)[26:53])-2) #remove yn
colnames(ausDs)[33] <- "c++"
write.csv(ausDs,"AustraliaDataScience.csv", row.names = FALSE)

# Australia Map
stateEBS <- ausDs %>% group_by(State) %>% summarise(Estimate.Base.Salary = mean(EstimateBaseSalary))
write.csv(stateEBS,"AustraliaDataScienceEBS.csv", row.names = FALSE)

#-------------------------------------------------------------------
countDs <- ausDs %>% group_by(JobTitle) %>% summarise(countVal = length(JobTitle)) %>% arrange(desc(countVal))
top10Popular <- head(countDs,5)
top10Popular <- inner_join(ausDs, top10Popular, by = "JobTitle")
skillRequired <- pivot_longer(top10Popular, cols=26:53, names_to = "skill", values_to = "required")
skillRequired <- skillRequired[c(1,28,29)]
skillRequired <- subset(skillRequired, required != 0)
write.csv(skillRequired,"top5Skills.csv", row.names = FALSE)

#-------------------------------------------------------------------
countDs <- ausDs %>% group_by(CompanyIndustry) %>% summarise(countVal = length(CompanyIndustry)) %>% arrange(desc(countVal))
countDs <- subset(countDs, CompanyIndustry != '')
countDs <- head(countDs,10)
countDs <- inner_join(ausDs, countDs, by = "CompanyIndustry")
write.csv(countDs,"top10industry.csv", row.names = FALSE)

#-------------------------------------------------------------------
ds1 <- ausDs[ausDs$CompanyType == 'Company - Public',]
ds2 <- ausDs[ausDs$CompanyType == 'Company - Private',]
companyType <- rbind(ds1,ds2)
companyType <- na.omit(companyType)
companyType <- companyType %>% group_by(CompanyType) %>% summarise("Company Rating" = mean(CompanyRating),
                                                                   "Career Opportunities" = mean(CompanyCareerOpportinities),
                                                                   "Senior Management" = mean(CompanySeniorManagement),
                                                                   "Compensation and Benefits" = mean(CompensationandBenefits),
                                                                   "Work Life Balance" = mean(CompanyWorkLifeBalance),
                                                                   "Culture and Values" = mean(CompanyCultureandValues))
companyType <- pivot_longer(companyType, cols=2:7, names_to = "category", values_to = "value")
colnames(companyType)[1] <- "key"
write.csv(companyType,"companyTypeRating.csv", row.names = FALSE)
