# Script: Data alignment IDH small holder farmer primary data
# Date: October 2021
# Contact: jildemarie@akvo.org


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

## ---- describe cases -----
cases <- c("2021-06-29_Alluvial_anom.xlsx",
           "2021-06-29_Coscharis_anom.xlsx",
           "2021-04-07_Kenya NKG Anom.xlsx",       
           "2021-04-12_Uganda NKG Anom.xlsx",
           "2021-04-12_Honduras NKG Anom.xlsx",
           "2021-04-15_Mexico NKG Anom.xlsx",
           "2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx",
           "2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx",
           "2021-03-19_smart logistics_anom.xlsx",
           "2021-05-14 AIF Anom nyagatare and kirehe.xlsx",
           "kenya-syngenta_potatoes.xlsx",
           "kenya-syngenta_tomatoes.xlsx",
           "2021_RGL_anom_beans.xlsx",
           "2021_RGL_anom_rice.xlsx")

cases <- c("2021_RGL_anom_beans.xlsx")

surveys <- c("survey alluvial.xlsx",
             "survey coscharis.xlsx",
             "survey nkg kenya.xlsx",                                     
             "survey nkg honduras.xlsx",
             "survey nkg mexico.xlsx",
             "survey nkg uganda.xlsx",
             "18052021_EU_tanzania_survey",
             "18052021_EU_tanzania_survey",
             "survey_smart_logistics.xlsx",
             "survey aif maize Rwanda.xlsx",
             "SURVEY_FORM_syngenta_potatoes.xlsx",
             "SURVEY_FORM_syngenta_tomatoes.xlsx",
             "2021_RGL_anom_beans.xlsx",
             "2021_RGL_anom_rice.xlsx")

surveys <- c("2021_RGL_anom_beans.xlsx")
             

# Crops
crops <- c("rice", "rice", 
           "coffee", "coffee","coffee","coffee",
           "tea", "tea", 
           "beans", "maize", 
           "potatoes", "tomatoes", "beans", "rice")

# Companies
companies <- c("alluvial", "coscharis", 
               "nkg","nkg","nkg","nkg",
               "ikanga", "ikanga",
               "smart logistics", "aif", 
               "syngenta", "syngenta", "rgl", "rgl")

# Translation language (in case of no translation I used Swahili)
languages <- c("sw", "sw", "sw", "sw",
               "es", "es", "sw", "sw",
               "sw", "rw",
               "sw", "sw",
               "sw", "sw", "sw")

## ----- import library ----

# Survey Library Template - V 2.1.1 (from Flow 15-07-2021)
format_raw_data <- read_excel("question library format v2.1.1.xlsx") %>%
  mutate(Title = na.locf(Title, na.rm = FALSE)) %>%
  dplyr::rename(question_group = "Title",
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
format_transformations <- read_excel("variables with transformation.xlsx")

## ---- Loop through case ----

codebooks <- list()
no_matches <- list()


for(i in 1:length(cases)){

    # CLEANED FILE
    case <- read_excel(cases[i], 
      sheet="Cleaned Data")


    # CODEBOOK
    codebook <- read_excel(cases[i], 
      sheet="Codebook") %>%
      # split question to remove translation
      cSplit("question", sep="/") %>%
      mutate_at(vars(starts_with("question")), as.character) %>%
      # cSplit splits too much, so identify language to see what text is part of the original question
      mutate(lan_question_1 = detect_language(text = question_1)) %>%
      mutate(lan_question_2 = detect_language(text = question_2)) %>%
      # Use ifelse to reconstruct question (english)
      mutate(question = ifelse(is.na(lan_question_2), 
                               question_1,
                               ifelse(lan_question_2 != languages[i], 
                                      paste(question_1, question_2, sep="/"), 
                                      question_1)))
    

  # DETAILS
  focus_crop = crops[i]
  company = companies[i]
  
  # Adjust the survey library format to fit the case survey (use crop name, company name, etc.)
  format_data <- format_raw_data %>% 
    full_join(format_transformations) %>%
    mutate(question = gsub("[[]optional[]]", "", question)) %>%
    mutate(question = gsub("[[]case[]]", "", question)) %>%
    mutate(question = gsub("[[]if applicable[]]", "", question)) %>%
    mutate(question = gsub("[[]focus crop[]]", focus_crop, question))  %>%
    mutate(question = gsub("[[]sdm company[]]", company, question)) %>%
    mutate(question = gsub("sdm company", company, question))
  
  format_data <- format_data %>% mutate(question = gsub(
    "add more seasons if necessary, add timeframe to answer options. ex: season 1 (march-may)", 
    "", question))
  
  # Compare different questions with the same variable name:
  #match_codebook_questions <- codebook %>% 
  #  select(variable, question) %>% 
  #  dplyr::rename(case_question = question) %>%
  #  inner_join(format_data %>% 
  #               select(variable, question)) %>%
  #  mutate("match" = ifelse(case_question == question, TRUE, FALSE)) %>%
  #  filter(match == FALSE) %>%
  #  # Use string distance (%) to identify the size of the difference (levenshtein distance)
  #  mutate(levi_add = stringdist(question, case_question),
  #         n_char_add = nchar(question),
  #         levi_add_percent = 100-round(levi_add/n_char_add*100, digits = 1)) %>%
  #  select(variable, levi_add_percent, everything())
  
  # Create list of files to check by hand
  #codebooks[[cases[i]]] <- match_codebook_questions



  # Compare different variable names with the same questions:
  match_codebook_variables <- codebook %>% 
    select(variable, question, section) %>% 
    dplyr::rename("case_variable" = variable,
           "question" = question,
           "question_group" = section) %>%
    inner_join(format_data %>% select(variable, question, question_group)) %>%
    mutate("match" = ifelse(case_variable == variable, TRUE, FALSE)) %>%
    filter(match == FALSE,
           case_variable != "hh_head_age",
           case_variable != "hh_farmer_age") 

  # Identify variable names that can be updated
  variable_names_to_change <- match_codebook_variables %>%
    left_join(match_codebook_variables %>%
                group_by(question_group, question) %>%
                dplyr::summarize( "n" = n())) %>%
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

  ###Adjust variable names - by hand
  ## ---- 13) RGL beans----
  if(cases[i] == "2021_RGL_anom_beans.xlsx"){
    case <- case %>%
      select(-contains("cost_timeperiod")) #, -cal_othermaincrop_income) %>%
      #rename(#cal_othermaincrop_1_cost = cal_othermaincrop_cost_1,
             #cal_othermaincrop_2_cost = cal_othermaincrop_cost_2,
            # f_unit_land_other = f_unit_land_no_other2,
            # f_focus_measurement_prod_other = f_focus_measurement_other_other2,
            # f_focus_lost_measurement_other = f_focus_own_lost_measurement_other2)

    codebook <- codebook %>%
      mutate(variable = ifelse(variable == "f_focus_measurement_other","f_focus_measurement_prod",variable),
    variable = ifelse(variable == "sdm_farmer","farmer_present",variable),
    variable = ifelse(variable == "sdm_crop","focus_crop",variable),
    variable = ifelse(variable == "f_unit_land_no","f_unit_land",variable),
    variable = ifelse(variable == "f_focus_own_lost_measurement","f_focus_lost_measurement",variable),
    variable = ifelse(variable == "m_crops_livestock_seller","m_crops_livestock_seller",variable),
    variable = ifelse(variable == "f_labour_andprep_rememberwage","f_labour_landprep_rememberwage",variable),
    variable = ifelse(variable == "f_inputs_usage","f_inputs_usage_types",variable),
    variable = ifelse(variable == "fo_urban_water","fo_water",variable),
    variable = ifelse(variable == "fo_business","fo_business_nonagri",variable),
    variable = ifelse(variable == "f_labour_sirrigation_nrmonths", "f_labour_irrigation_nrmonths",variable))
  }
  

## ---- REQUESTS OSCAR ----
  # Derived:
  # cal_productivity
  
  if("repeat no" %in% colnames(case)){
    case <- case %>%
      dplyr::rename("repeat_no" = `repeat no`)
  }
  
  
  if("hh_loan_interest_rate_SDM" %in% colnames(case)){
    case <- case %>%
      dplyr::rename(hh_loan_interest_rate_sdm = hh_loan_interest_rate_SDM)
  }
  
  if("f_size_acre" %in% colnames(case)){
    case <- case %>% 
      mutate(f_size_acre = as.numeric(f_size_acre)) %>%
      mutate(f_size_hectare = f_size_acre/2.471) 
  }
  
  if("f_focus_crop_size_acre" %in% colnames(case)){
    case <- case %>%
      mutate(f_focus_crop_size_acre = as.numeric(f_focus_crop_size_acre)) %>%
      mutate(f_focus_crop_size_hectare = f_focus_crop_size_acre/2.471) 
  }


  if("f_size_othermaincrop_1_acre" %in% colnames(case)){
    case <- case %>%
      mutate(f_size_othermaincrop_1_acre = as.numeric(f_size_othermaincrop_1_acre)) %>%
      mutate(f_size_othermaincrop_1_hectare = f_size_othermaincrop_1_acre/2.471) 
  }
  
  if("f_size_othermaincrop_2_acre" %in% colnames(case)){
    case <- case %>%
      mutate(f_size_othermaincrop_2_acre = as.numeric(f_size_othermaincrop_2_acre)) %>%
      mutate(f_size_othermaincrop_2_hectare = f_size_othermaincrop_2_acre/2.471) 
  }

  if("cal_focus_productivity" %in% colnames(case)){
    case <- case %>% 
      mutate("cal_focus_productivity" = as.numeric(cal_focus_productivity)) %>%
      mutate("cal_focus_productivity_hectare" = cal_focus_productivity/2.471) %>%
      dplyr::rename("cal_focus_productivity_acre" = "cal_focus_productivity")
  }
  
  # Other main crop to KG?
  case <- case %>%
    dplyr::rename_at(vars(one_of("cal_focus_quant_prod", 
                          "cal_focus_quant_sold", 
                          "cal_focus_quant_lost")),  
              ~paste0(., "_kg"))
  
  if("f_focus_own_consumption_kg" %in% colnames(case)){
    case <- case %>% 
      dplyr::rename("cal_focus_quant_own_consumption_kg" = "f_focus_own_consumption_kg")
  }
  
  if("f_focus_quant_prod_kg" %in% colnames(case)){
    case <- case %>% 
      dplyr::rename("cal_focus_quant_prod_kg" = "f_focus_quant_prod_kg")
  }
  
  if("f_focus_quant_sold_kg" %in% colnames(case)){
    case <- case %>% 
      dplyr::rename("cal_focus_quant_sold_kg" = "f_focus_quant_sold_kg")
  }
  
  if("f_focus_quant_lost_kg" %in% colnames(case)){
    case <- case %>% 
      dplyr::rename("cal_focus_quant_lost_kg" = "f_focus_quant_lost_kg")
  }
  

  if("f_othermaincrop_1_meas_prod" %in% colnames(case)){
    case <- case %>% 
      mutate(cal_othermaincrop_1_quant_prod_kg = ifelse(
        f_othermaincrop_1_meas_prod == "kgs",
        f_othermaincrop_1_quant_prod, ifelse(
          f_othermaincrop_1_meas_prod == "tonnes",
          f_othermaincrop_1_quant_prod*1000, NA))) %>% 
      mutate(cal_othermaincrop_1_quant_sold_kg = ifelse(
        f_othermaincrop_1_meas_sold == "kgs",
        f_othermaincrop_1_quant_sold, ifelse(
          f_othermaincrop_1_meas_sold == "tonnes",
          f_othermaincrop_1_quant_sold*1000, NA)))
  }
  
  if("f_othermaincrop_2_meas_prod" %in% colnames(case)){
    case <- case %>% 
      mutate(cal_othermaincrop_2_quant_prod_kg = ifelse(
        f_othermaincrop_2_meas_prod == "kgs",
        f_othermaincrop_2_quant_prod, ifelse(
          f_othermaincrop_2_meas_prod == "tonnes",
          f_othermaincrop_2_quant_prod*1000, NA))) %>% 
      mutate(cal_othermaincrop_2_quant_sold_kg = ifelse(
        f_othermaincrop_2_meas_sold == "kgs",
        f_othermaincrop_2_quant_sold, ifelse(
          f_othermaincrop_2_meas_sold == "tonnes",
          f_othermaincrop_2_quant_sold*1000, NA)))
  }

  #case <- case %>%
  #  mutate_at(vars(starts_with("hh_") & contains("_birthyear")), as.numeric) %>%
  #  mutate_at(vars(starts_with("hh_") & contains("_age")), as.numeric) %>%
  #  mutate_at(vars(starts_with("hh_") & contains("_birthyear")), 
  #            function(x) ifelse(x < 120, 2021 - x, x)) %>%
  #  mutate_at(vars(starts_with("hh_") & contains("_age")), 
  #            function(x) ifelse(x < 120, 2021 - x, x)) %>% 
    #rename_at(vars(starts_with("hh_") & contains("_age")), 
    #          funs(gsub("_age", "_birthyear", ., perl=T))) #%>%
  
  
  
  
  ### ---- Final tweaks----
  if("cal_farm_revenu" %in% colnames(case)){
    case <- case %>%
      dplyr::rename(cal_farm_revenue = cal_farm_revenu)
  }
  
  if("cal_focus_revenu" %in% colnames(case)){
    case <- case %>%
      dplyr::rename(cal_focus_revenue = cal_focus_revenu)
  }
  
  if("cal_livestock_revenu" %in% colnames(case)){
    case <- case %>%
      dplyr::rename(cal_livestock_revenue = cal_livestock_revenu)
  }
  if("cal_farm_netincome" %in% colnames(case)){
    case <- case %>%
      dplyr::rename(cal_farm_income = cal_farm_netincome)
  }
  
  if("cal_transport" %in% colnames(case)){
    case <- case %>%
      select(-cal_transport)
  }
  
  if("cal_transport_costs" %in% colnames(case)){
    case <- case %>%
      select(-cal_transport_costs)
  }
  if("cal_focus_lost_measurement" %in% colnames(case)){
    case <- case %>%
      dplyr::rename(cal_focus_measurement_lost = cal_focus_lost_measurement)
  }
  
  if("cal_offfarm_labour_netincome" %in% colnames(case)){
    case <- case %>% 
      dplyr::rename("cal_off_farm_labour_income" = "cal_offfarm_labour_netincome")
  }
  if("cal_offfarm_labour_income" %in% colnames(case)){
    case <- case %>% 
      dplyr::rename("cal_off_farm_labour_income" = "cal_offfarm_labour_income")
  }  
  #Repeat household demographics groups -> repeated rows
  if("identifier" %in% colnames(case)){
    if("hh_member_birthyear" %in% colnames(case)){        
      case <- case %>%
        group_by(identifier) %>%
        mutate(id = row_number())
      
      
      for (nr in c(1:15)){
        
        hh_member_birthyear_nr <- paste0("hh_member_birthyear_", nr)
        hh_member_gender_nr <- paste0("hh_member_gender_", nr)
        hh_member_education_nr <- paste0("hh_member_education_", nr)
        
        case <- case %>% 
          mutate(!!sym(hh_member_birthyear_nr) := ifelse(id == nr, hh_member_birthyear, NA),
                 !!sym(hh_member_gender_nr) := ifelse(id == nr, hh_member_gender, NA),
                 !!sym(hh_member_education_nr) := ifelse(id == nr, hh_member_education, NA)) %>%
          fill(!!sym(hh_member_birthyear_nr)) %>%
          fill(!!sym(hh_member_birthyear_nr), .direction = "up") %>%
          fill(!!sym(hh_member_gender_nr)) %>%
          fill(!!sym(hh_member_gender_nr), .direction = "up") %>%
          fill(!!sym(hh_member_education_nr)) %>%
          fill(!!sym(hh_member_education_nr), .direction = "up") %>%
          mutate(!!sym(hh_member_birthyear_nr) := ifelse(id >1 , NA, !!sym(hh_member_birthyear_nr) ),
                 !!sym(hh_member_gender_nr) := ifelse(id >1 , NA, !!sym(hh_member_gender_nr) ),
                 !!sym(hh_member_education_nr) := ifelse(id >1 , NA, !!sym(hh_member_education_nr) ))
        
      }
      case <- case %>%
        filter(id == 1) %>%
        select(-id) 
    }
  }
  

  
  codebook <- codebook %>%
  select(section, variable, question, type, options, multiple)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, cases[i] ,
             overwrite = TRUE)
  
}
