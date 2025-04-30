library(readxl)
library(tidyverse)


RawData <- read_excel("Data/DATA_CLEANING-296820004 (1).xlsx", sheet = "Raw Data")
RevenueData <- read_excel("Data/DATA_CLEANING-296820004 (1).xlsx", sheet = "Group 5")

RawData <- RawData %>%
  select(Identifier, Submitter, `Submission Date`, `315330058|What is the size of the farm dedicated to Sunflower?`,
         `315330060|What unit of land measurement do you use?`) %>%
  filter(`315330058|What is the size of the farm dedicated to Sunflower?` < 9999) %>%
  mutate(SunflowerFarmSize=
           ifelse(`315330060|What unit of land measurement do you use?`=="Hectares",`315330058|What is the size of the farm dedicated to Sunflower?`*2.47,
                  `315330058|What is the size of the farm dedicated to Sunflower?`))
  

RevenueData <- RevenueData %>%
  select(Identifier, Submitter, `304750087|We will now ask you a couple of questions about the production and revenues of sunflower. Please indicate for which period you know the production level, quantity sold, revenues received, price received etc.`,
         `306790027|Using what measurement unit did you measure the amount of Sunflower that you produced during this period?`,
         `304750086|How much of the Sunflower did you produce during this period?`) %>%
  filter(`304750086|How much of the Sunflower did you produce during this period?` != 9999 & `304750086|How much of the Sunflower did you produce during this period?` != 9998) %>%
  rename(MeasurementUnit = `306790027|Using what measurement unit did you measure the amount of Sunflower that you produced during this period?`,
         Period=`304750087|We will now ask you a couple of questions about the production and revenues of sunflower. Please indicate for which period you know the production level, quantity sold, revenues received, price received etc.`,
         SunflowerProduced = `304750086|How much of the Sunflower did you produce during this period?`) %>%
  mutate(`Sunflower produced (Kgs)` = 
           ifelse(MeasurementUnit == "Debes", SunflowerProduced* 10,
                  ifelse(MeasurementUnit=="gunia", SunflowerProduced * 70, SunflowerProduced)))

# FinalRecipients <- full_join(SurveyAttempts, Recipients, by="recipient_id")

FinalRevenueData <- full_join(RawData, RevenueData, by="Identifier") 


ggplot(FinalRevenueData) +
  geom_point(aes(x = SunflowerFarmSize, y = `Sunflower produced (Kgs)`))

g