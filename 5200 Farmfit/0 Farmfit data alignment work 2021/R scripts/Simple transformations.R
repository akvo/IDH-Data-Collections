##  ---- readdata ----
# Data delivery bare bones

# Packages used
library(here); library(readxl);
library(plyr); library(dplyr); library(tidyr); library(tidylog); library(tidyselect)
library(stringr); library(zoo);library(splitstackshape); library(reshape2); library(openxlsx)

#CASES
#04102019_AGRI_WALLET_anonymized.xlsx / survey agri wallet.xlsx
#12122019_Alluvial.xlsx / survey Alluvial 2019.xlsx
#11122019 Coscharis Anom.xlsx / SURVEY_FORM_nigeria_rice_coscharis_14052021.xlsx

here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Simple transformations.R")


# Read cleaned data from output folder
cleaned <- read_excel("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original/11122019 Coscharis Anom.xlsx")

# Read file with survey structure
survey_questions <- read_excel(
  here::here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original/SURVEY_FORM_nigeria_rice_coscharis_14052021.xlsx"),
  sheet ="Full Survey", skip=2)

# SURVEY ADJUSTMENTS
survey_questions %<>% 
  # Fill excel merged cells from the section
  mutate(Title = na.locf(Title, na.rm = FALSE)) %>%
  # Select right columns
  select("Title", "Variable name", "Text",
         "Question type", "Options",
         "Allow multiple") %>%
  # Trim trailing spaces
  mutate_if(is.character, str_trim) %>%
  # Rename columns
  rename(section = "Title", 
         variable = "Variable name", 
         question = "Text",
         type = "Question type", 
         options = "Options",
         multiple = "Allow multiple") %>%
  # All variables to lowercase
  mutate_all(tolower) %>%
  # Filter the empty variables
  filter(!is.na(variable))




## WRITE data to excel
sets <- list(
  "Code book" = survey_questions,
  "Cleaned Data" = cleaned#,
  #"Raw Data (anonymised)" = Data_raw, 
  #"Numerical descriptives" = numerical_descriptives,
  #"Categorical desc. single" = single_categorical_descriptives,
  #"Categorical desc. multi" = multiple_categorical_descriptives
)

write.xlsx (sets, file = here::here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original/11122019 Coscharis Anom_transformed.xlsx"))

