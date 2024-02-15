# Loading DF & Libraries
library("readxl")
library("dplyr")
CustDF <- read_excel("Merr.Customer.Survey (2).xlsx")

#Adding AgeGrp to de-identify/generalize 
CustDF$AgeGrp <- cut(CustDF$Age, breaks=c(0,19,29,39,49,59,69,80), 
                     labels=c(">20","20s","30s","40s","50s","60s","70s"))

#Variable Breakdowns
summary(CustDF$Age)
summary(CustDF$EducationYears)
summary(CustDF$HouseholdIncome)
summary(CustDF$HouseholdSize)
summary(CustDF$DebtToIncomeRatio)
summary(CustDF$PhoneCoTenure)

#Separating quasi-identifiers
QuasiCustDF <- CustDF %>% 
  select(Age, AgeGrp, EducationYears, HouseholdIncome, HouseholdSize, DebtToIncomeRatio, PhoneCoTenure)

unique_combos <- unique(QuasiCustDF)
#All rows are completely unique, generalizing variables
max(QuasiCustDF$HouseholdIncome)
QuasiCustDF$HHIncome10Ks <- 
  cut(QuasiCustDF$HouseholdIncome, 
      breaks=c(0,49,99,249,499,1100), 
      labels=c(">50k","50-99k","100-249k","250-499k", "500k+"))
table(QuasiCustDF$HHIncome10Ks)

max(QuasiCustDF$DebtToIncomeRatio)
QuasiCustDF$DebtToIncomeGrp <- 
  cut(QuasiCustDF$DebtToIncomeRatio, 
      breaks=c(0, 9.9, 19.9, 29.9, 44), 
      labels=c(">10","10-19.9","20-29.9", "30+"))
table(QuasiCustDF$DebtToIncomeGrp)

max(QuasiCustDF$PhoneCoTenure)
QuasiCustDF$PhoneCoYrs <- 
  cut(QuasiCustDF$PhoneCoTenure, 
      breaks=c(0, 11, 23, 35, 47, 59, 71, 83), 
      labels=c(">1","1","2","3","4","5","6"))
table(QuasiCustDF$PhoneCoYrs)

max(QuasiCustDF$EducationYears)
QuasiCustDF$EducationLevel <- 
  cut(QuasiCustDF$EducationYears, 
      breaks=c(0, 11, 15, 19, 24), 
      labels=c(">12","12-15","16-19","20+"))
table(QuasiCustDF$EducationLevel)

QuasiCustDF2 <- QuasiCustDF %>% 
  select(AgeGrp, EducationLevel, HHIncome10Ks , HouseholdSize, DebtToIncomeGrp, PhoneCoYrs)
unique_combos <- unique(QuasiCustDF2)

# #Creating equivalence classes
eqclasses <- QuasiCustDF2[order(QuasiCustDF2$AgeGrp, QuasiCustDF2$EducationLevel, QuasiCustDF2$HHIncome10Ks, QuasiCustDF2$HouseholdSize,
                                QuasiCustDF2$DebtToIncomeGrp, QuasiCustDF2$PhoneCoYrs), ]

equivalence_id <- 1
eqclasses$EquivalenceClass <- equivalence_id

for (i in 2:nrow(eqclasses)) {
  if (is.na(eqclasses[i, "AgeGrp"]) || is.na(eqclasses[i - 1, "AgeGrp"]) ||
      is.na(eqclasses[i, "EducationLevel"]) || is.na(eqclasses[i - 1, "EducationLevel"]) ||
      is.na(eqclasses[i, "HHIncome10Ks"]) || is.na(eqclasses[i - 1, "HHIncome10Ks"]) ||
      is.na(eqclasses[i, "HouseholdSize"]) || is.na(eqclasses[i - 1, "HouseholdSize"]) ||
      is.na(eqclasses[i, "DebtToIncomeGrp"]) || is.na(eqclasses[i - 1, "DebtToIncomeGrp"]) ||
      is.na(eqclasses[i, "PhoneCoYrs"]) || is.na(eqclasses[i - 1, "PhoneCoYrs"]) ||
      eqclasses[i, "AgeGrp"] != eqclasses[i - 1, "AgeGrp"] || 
      eqclasses[i, "EducationLevel"] != eqclasses[i - 1, "EducationLevel"] ||
      eqclasses[i, "HHIncome10Ks"] != eqclasses[i - 1, "HHIncome10Ks"] ||
      eqclasses[i, "HouseholdSize"] != eqclasses[i - 1, "HouseholdSize"] ||
      eqclasses[i, "DebtToIncomeGrp"] != eqclasses[i - 1, "DebtToIncomeGrp"] ||
      eqclasses[i, "PhoneCoYrs"] != eqclasses[i - 1, "PhoneCoYrs"]) {
    equivalence_id <- equivalence_id + 1
  }
  eqclasses[i, "EquivalenceClass"] <- equivalence_id
}

#counting occurrences and sizes of each equivalence class
class_sizes <- table(eqclasses$EquivalenceClass)
class_count <- table(class_sizes)

print(class_sizes)
print(class_count)

#Calculating re-identification risk
ReIDProbDF <- data.frame (
  ClassSize = c(1:22,30),
  Classes = c(class_count)
)
ReIDProbDF$TotalRecords <- ReIDProbDF$ClassSize*ReIDProbDF$Classes
ReIDProbDF$RiskReID <- 1/ReIDProbDF$ClassSize
ReIDProbDF$PercOfRecords <- ReIDProbDF$TotalRecords/sum(ReIDProbDF$TotalRecords)
ReIDProbDF$RiskS1 <- (((ReIDProbDF$RiskReID*.2)/.2)*.2)
ReIDProbDF$RiskS2 <- (((ReIDProbDF$RiskReID*1)/1)*1)
ReIDProbDF$RiskS3 <- (((ReIDProbDF$RiskReID*.37)/.37)*.37)
ReIDProbDF$RiskS4 <- (((ReIDProbDF$RiskReID*1)/1)*1)

#Calculating Average Risk
ReIDProbDF$S1Avg <-  ReIDProbDF$TotalRecords*ReIDProbDF$RiskS1
ReIDProbDF$S2Avg <-  ReIDProbDF$TotalRecords*ReIDProbDF$RiskS2
ReIDProbDF$S3Avg <-  ReIDProbDF$TotalRecords*ReIDProbDF$RiskS3
ReIDProbDF$S4Avg <-  ReIDProbDF$TotalRecords*ReIDProbDF$RiskS4

ReIDRiskFinal <- data.frame (
  S1RiskAvg = c(sum(ReIDProbDF$S1Avg)/5000),
  S2RiskAvg = c(sum(ReIDProbDF$S2Avg)/5000),
  S3RiskAvg = c(sum(ReIDProbDF$S3Avg)/5000),
  S4RiskAvg = c(sum(ReIDProbDF$S4Avg)/5000)
)