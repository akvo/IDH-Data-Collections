##  ---- readdata ----
# Data delivery bare bones

# Packages used
library(here); library(readxl);
library(plyr); library(dplyr); library(tidyr); library(tidylog); library(tidyselect)
library(stringr); library(zoo);library(splitstackshape); library(reshape2); library(openxlsx)
here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Simple transformations.R")

#CASES
cases <- c("2021-06-29_Alluvial_anom.xlsx",
          "2021-06-29_Coscharis_anom.xlsx",
          "12122019_Alluvial.xlsx",
          "20190904 Bulamu Anom.xlsx",
          "17102019_NFC.xlsx",
          "12122019_USOMI.xlsx",
          "2019_pilot_syngenta_kenya_raw.xlsx")
          

surveys <- c("survey alluvial.xlsx",
           "survey coscharis.xlsx",
           "survey Alluvial 2019.xlsx",
           "SURVEY_FORM_BULAMU.xlsx",
           "survey_nfc.xlsx",
           "survey_usomi_kenya.xlsx",
           "survey_pilot_syngenta_kenya.xlsx")
          
for(i in 1:length(cases)){
for(j in 1:length(surveys)) {


# Read file with survey structure
survey_questions <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", surveys[i]),
  sheet ="Full Survey", skip=2) %>%
  # Fill excel merged cells from the section
  mutate(Title = na.locf(Title, na.rm = FALSE)) %>%
  # Select right columns
  select("Title", "Variable name", "Text",
         "Question type", "Options",
         "Allow multiple") %>%
  # Trim trailing spaces
  mutate_if(is.character, str_trim) 

survey_questions <- survey_questions %>%
  # Rename columns
  dplyr::rename("section" = Title, 
         "variable" = `Variable name`, 
         "question" = Text,
         "type" = `Question type`, 
         "options" = Options,
         "multiple" = `Allow multiple`) %>%
  # All variables to lowercase
  mutate_all(tolower) %>%
  # Filter the empty variables
  filter(!is.na("variable"))




# Read cleaned data from output folder
cleaned <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Need transformation", cases[i]))



## WRITE data to excel
sets <- list(
  "Code book" = survey_questions,
  "Cleaned Data" = cleaned)

write.xlsx (sets, file = here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]))

}
}
