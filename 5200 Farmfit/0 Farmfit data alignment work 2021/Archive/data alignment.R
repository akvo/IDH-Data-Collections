# Script: Data alignment IDH small holder farmer primary data
# Date: 15-07-2021
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

# "EASY" cases
cases <- c("2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx",
           "2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx",
           # "2021-06-29_Alluvial_anom.xlsx",
           # "2021-06-29_Coscharis_anom.xlsx",
           "2021-03-19_smart logistics_anom.xlsx",
           "2021-05-14 AIF Anom nyagatare and kirehe.xlsx",
           "kenya-syngenta_potatoes.xlsx",
           "kenya-syngenta_tomatoes.xlsx")

# Crops
crops <- c("tea", "tea", 
           # "rice", "rice", 
           "beans", "maize", 
           "potatoes", "tomatoes")

# Companies
companies <- c("ikanga", "ikanga",
             "smart logistics", "aif", 
             "syngenta", "syngenta")

# Translation language (in case of no translation I used Swahili)
languages <- c("sw", "sw", "sw", "rw", "sw", "sw")

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
    here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage", cases[i]), 
    sheet="Cleaned Data")
  
  # CODEBOOK
  codebook <- read_excel(
    here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage", cases[i]), 
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
  
  # Compare different questions with the same variable name:
  match_codebook_questions <- codebook %>% 
    select(variable, question) %>% 
    rename(case_question = question) %>%
    inner_join(format_data %>% 
                 select(variable, question)) %>%
    mutate("match" = ifelse(case_question == question, TRUE, FALSE)) %>%
    filter(match == FALSE) %>%
    # Use string distance (%) to identify the size of the difference
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
  
  ############################
  
  # Changes identified by going though the lists BY HAND! 
  
  # EU TANZ TEA - IKANGA
  if(cases[i] %like% "2021-05-26_EU Tea Tanzania"){
    case <- case %>% rename(
      hh_loan_costs_additional = hh_loan_costs_interest,
      su_services_usage_farmer_organisation = su_services_usage,
      f_labour_infilling_nrpeople = f_labour_compostprep_nrpeople,
      fo_farming_investment_positive = fo_farming_investment,
      f_focus_quant_prod = f_tea_quant_prod,
      f_focus_quant_sold = f_tea_quant_sold,
      f_focus_measurement_sold = f_tea_measurement_sold,
      f_focus_price = f_tea_price,
      f_focus_quant_lost = f_tea_quant_lost,
      f_focus_measurement_lost = f_tea_measurement_lost
    )
  }
  
  # SMART LOGISTICS
  if(cases[i] == "2021-03-19_smart logistics_anom.xlsx"){
    case <- case %>% rename(
      hh_loan_costs_additional = hh_loan_costs_interest#,
      # f_focus_measurement_prod_yes_no = f_focus_measurement_prod
    )
  }
  
  ############################
  
  # REQUESTS OSCAR
  
  # Derived:
  # cal_productivity
  
  case <- case %>% 
    mutate("f_size_hectare" = `f_size (acre)`/2.471) %>%
    mutate("f_focus_crop_size_hectare" = `f_focus_crop_size (acre)`/2.471) %>%
    mutate("f_size_othermaincrop_1_hectare" = `f_size_othermaincrop_1 (acre)`/2.471) %>%
    mutate("f_size_othermaincrop_2_hectare" = `f_size_othermaincrop_2 (acre)`/2.471) %>%
    rename("f_size_acre" = "f_size (acre)",
           "f_focus_crop_size_acre" = "f_focus_crop_size (acre)",
           "f_size_othermaincrop_1_acre" = "f_size_othermaincrop_1 (acre)",
           "f_size_othermaincrop_2_acre" = "f_size_othermaincrop_2 (acre)")
  
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
}

# write the lists for manual checks - differing questions:
write.xlsx(codebooks, file = here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/", 
                             "Mismatch in questions.xlsx"), 
           sep=",", 
           row.names = FALSE)

# write the lists for manual checks - no match at all:
write.xlsx(no_matches, file = here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/", 
                                  "Mismatch in question and varname.xlsx"), 
           sep=",", 
           row.names = FALSE)


# Compare the "no match" sets:
match_in_no_match <- list()
for(i in 1:length(no_matches)){
  match <- no_matches[i] %>% inner_join(no_matches[i+1])
  match_in_no_match[i] <- match
}



