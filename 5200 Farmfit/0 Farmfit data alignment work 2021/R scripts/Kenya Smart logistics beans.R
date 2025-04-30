# Script: Data alignment IDH small holder farmer primary data - CASE: Kenya potatoes
# Date: 18-08-2021
# Contact: carmen@akvo.org
# Case: EU Tanzania

# Libraries 
library(readxl)
library(openxlsx)
library(here)

library(tidyverse)
library(tidylog)
library(data.table)
library(zoo)
library(splitstackshape)
library(RecordLinkage)
library(stringdist)
library(cld2)
library(dplyr)

# "EASY" cases
cases <- c( "2021-03-19_smart logistics_anom.xlsx")
# Crops
crops <- c("beans")

# Companies
companies <- c( "smart logistics")

# Translation language (in case of no translation I used Swahili)
languages <- c("sw")

# Survey Library Template - V 2.1.1 (from Flow 15-07-2021)
format_raw_data <- read_excel(
  here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
       "question library format v2.1.1.xlsx")) %>%
  mutate(Title = na.locf(Title, na.rm = FALSE)) %>%
  rename(question_group = "Title",
         question = "Text", 
         variable = "Variable name",
         type = "Question type",
         mandatory = "Mandatory",
         dependent = "Dependent",
         dependent_on_question = "Question",
         dependent_on_answer = "Answer(s)",
         answer_options = "Options",
         multiple = "Allow multiple",
         other = "Allow other") %>%
  mutate_all(tolower) %>%
  # Spelling mistakes in the survey library
  mutate(variable = gsub("f_labour_andprep_rememberwage", "f_labour_landprep_rememberwage", variable)) %>%
  mutate(variable = gsub("f_labour_sirrigation_nrmonths", "f_labour_irrigation_nrmonths", variable))

# Add calculated variables - based on Tanzania Tea survey:
format_transformations <- read_excel(
  here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
       "variables with transformation.xlsx")) %>%
  rename("Core" = type) %>%
  mutate(variable = gsub('tea', "focus", variable)) 


# Loop through case:
codebooks <- list()
no_matches <- list()

for(i in 1:length(cases)){
  
  # CLEANED FILE
  case <- read_excel(
    here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]), 
    sheet="Cleaned Data")
  
  # CODEBOOK
  codebook <- read_excel(
    here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]), 
    sheet="Code book") %>%
    # split question to remove translation
    cSplit("question", sep="/") %>%
    mutate_at(vars(starts_with("question")), as.character) %>%
    # cSplit splits too much, so identify language to see what text is part of the original question
    mutate(lan_question_1 = detect_language(text = question_1)) %>%
    mutate(lan_question_2 = detect_language(text = question_2)) %>%
    mutate(lan_question_3 = detect_language(text = question_3)) %>%
    # Use ifelse to reconstruct question (english)
    mutate(question = ifelse(is.na(lan_question_2), 
                             question_1,
                             ifelse(lan_question_2 != languages[i], 
                                    paste(question_1, question_2, sep="/"), 
                                    question_1))) %>%
    mutate(question = ifelse(is.na(lan_question_3), 
                             question, 
                             ifelse(lan_question_3 != languages[i],
                                    paste(question, question_3, sep="/"), 
                                    question)))
  
  
  # DETAILS
  focus_crop = crops[i]
  company = companies[i]
  
  # Adjust the survey library format to fit the case survey (use crop name, company name, etc.)
  format_data <- format_raw_data %>% 
    full_join(format_transformations) %>%
    mutate(question = gsub("[[]optional[]] ", "", question)) %>%
    mutate(question = gsub("[[]case[]] ", "", question)) %>%
    mutate(question = gsub("[[]if applicable[]] ", "", question)) %>%
    mutate(question = gsub("[[]focus crop[]]", focus_crop, question))  %>%
    mutate(question = gsub("[[]sdm company[]]", company, question)) %>%
    mutate(question = gsub("sdm company", company, question))
  
  format_data <- format_data %>% mutate(question = gsub(
    "add more seasons if necessary, add timeframe to answer options. ex: season 1 (march-may)", 
    "", question))
} 
####----------------------------------------------------------------------------
#####Start cleaning

for(i in 1:length(cases)){
  # KENYA SMART LOGISTICS BEANS
  if(cases[i] ==  "2021-03-19_smart logistics_anom.xlsx"){
    case <- case %>% rename(
      ##New elements of library
      hh_loan_costs_additional = hh_loan_costs_interest,
      f_livestock_chickens = f_livestcok_income_chickens,
      ##CASE/CROP SPECIFIC
      f_focus_variety = f_beans_variety
      )
  }
}


# REQUESTS OSCAR

# Derived:
# cal_productivity
for(i in 1:length(cases)){   
  if("f_size (acre)" %in% colnames(case)){
    case <- case %>% 
      mutate("f_size_hectare" = `f_size (acre)`/2.471) %>%
      rename("f_size_acre" = "f_size (acre)")
  }
  
  if("f_focus_crop_size (acre)" %in% colnames(case)){
    case <- case %>%
      mutate("f_focus_crop_size_hectare" = `f_focus_crop_size (acre)`/2.471) %>%
      rename("f_focus_crop_size_acre" = "f_focus_crop_size (acre)")
  }
  
  if("f_size_othermaincrop_1 (acre)" %in% colnames(case)){
    case <- case %>%
      mutate("f_size_othermaincrop_1_hectare" = `f_size_othermaincrop_1 (acre)`/2.471) %>%
      mutate("f_size_othermaincrop_2_hectare" = `f_size_othermaincrop_2 (acre)`/2.471) %>%
      rename("f_size_othermaincrop_1_acre" = "f_size_othermaincrop_1 (acre)",
             "f_size_othermaincrop_2_acre" = "f_size_othermaincrop_2 (acre)")
  }
  
  if("cal_focus_productivity" %in% colnames(case)){
    case <- case %>% 
      mutate("cal_focus_productivity_hectare" = `cal_focus_productivity`/2.471) %>%
      rename("cal_focus_productivity_acre" = "cal_focus_productivity")
  }
  
  # Other main crop to KG?
  case <- case %>%
    rename_at(vars(one_of("cal_focus_quant_prod", 
                          "cal_focus_quant_sold", 
                          "cal_focus_quant_lost")),  
              ~paste0(., "_kg"))
  
  if("f_othermaincrop_1_meas_prod" %in% colnames(case)){
    case <- case %>% 
      mutate(f_othermaincrop_1_quant_prod_kg = ifelse(
        f_othermaincrop_1_meas_prod == "kgs",
        f_othermaincrop_1_quant_prod, ifelse(
          f_othermaincrop_1_meas_prod == "tonnes",
          f_othermaincrop_1_quant_prod*1000, NA))) %>% 
      mutate(f_othermaincrop_1_quant_sold_kg = ifelse(
        f_othermaincrop_1_meas_sold == "kgs",
        f_othermaincrop_1_quant_sold, ifelse(
          f_othermaincrop_1_meas_sold == "tonnes",
          f_othermaincrop_1_quant_sold*1000, NA)))
  }
  
  if("f_othermaincrop_2_meas_prod" %in% colnames(case)){
    case <- case %>% 
      mutate(f_othermaincrop_2_quant_prod_kg = ifelse(
        f_othermaincrop_2_meas_prod == "kgs",
        f_othermaincrop_2_quant_prod, ifelse(
          f_othermaincrop_2_meas_prod == "tonnes",
          f_othermaincrop_2_quant_prod*1000, NA))) %>% 
      mutate(f_othermaincrop_2_quant_sold_kg = ifelse(
        f_othermaincrop_2_meas_sold == "kgs",
        f_othermaincrop_2_quant_sold, ifelse(
          f_othermaincrop_2_meas_sold == "tonnes",
          f_othermaincrop_2_quant_sold*1000, NA)))
  }
  
  case <- case %>%
    mutate_at(vars(starts_with("hh_") & contains("_birthyear")), 
              function(x) ifelse(x < 120, 2021 - x, x)) %>%
    mutate_at(vars(starts_with("hh_") & contains("_age")), 
              function(x) ifelse(x < 120, 2021 - x, x)) %>% 
    rename_at(vars(starts_with("hh_") & contains("_age")), 
              funs(gsub("_age", "_birthyear", ., perl=T)))
}

####----------------------------------------------------------------------------

###START COMPARING

for(i in 1:length(cases)){
  # Compare different questions with the same variable name:
  match_codebook_questions <- codebook %>% 
    select(variable, question) %>% 
    rename(case_question = question) %>%
    inner_join(format_data %>% 
                 select(variable, question)) %>%
    mutate("match" = ifelse(case_question == question, TRUE, FALSE)) %>%
    filter(match == FALSE) %>%
    # Use string distance (%) to identify the size of the difference (levenshtein distance)
    mutate(levi_add = stringdist(question, case_question),
           n_char_add = nchar(question),
           levi_add_percent = 100-round(levi_add/n_char_add*100, digits = 1)) %>%
    select(variable, levi_add_percent, everything())
  
  # Create list of files to check by hand
  codebooks[[cases[i]]] <- match_codebook_questions
  
  # Compare different variable names with the same questions:
  match_codebook_variables <- codebook %>% 
    select(variable, question, section) %>% 
    rename("case_variable" = variable,
           "question" = question,
           "question_group" = section) %>%
    inner_join(format_data %>% select(variable, question, question_group)) %>%
    mutate("match" = ifelse(case_variable == variable, TRUE, FALSE)) %>%
    filter(match == FALSE) 
  
  # Identify variable names that can be updated
  variable_names_to_change <- match_codebook_variables %>%
    left_join(match_codebook_variables %>%
                group_by(question_group, question) %>%
                summarise( "n" = n())) %>%
    filter(n==1) %>% select(case_variable, "variable")
  
  # update variable names 
  setnames(case, 
           old = variable_names_to_change$case_variable, 
           new = variable_names_to_change$variable,
           skip_absent = TRUE)
  
  # find survey items without variable name or question match 
  no_match <- case  %>% 
    select(-one_of(format_data$variable))
  
  # Fill list
  no_matches[[cases[i]]] <- no_match
}

####Export non-matches
# write the lists for manual checks - differing questions:
write.xlsx(codebooks, file = here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Check match", 
                                  "Mismatch in questions_JB_smart_logistics_beans.xlsx"), 
           sep=",", 
           row.names = FALSE)

# write the lists for manual checks - no match at all:
write.xlsx(no_matches, file = here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Check match/", 
                                   "Mismatch in question and varname_JB_smart_logistics_beans.xlsx"), 
           sep=",", 
           row.names = FALSE)

# Compare the "no match" sets:
match_in_no_match <- colnames(no_matches[1][[1]])

aligned_data <- case

write.xlsx(aligned_data, file = here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Aligned/", 
                                     "2021-03-19_smart logistics_anom.xlsx"),
           sep="," ,
           sheetName = "Aligned data",
           row.names = FALSE)
