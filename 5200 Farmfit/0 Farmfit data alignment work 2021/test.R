# Script: Data alignment IDH small holder farmer primary data
# Date: October 2021
# Contact: jildemarie@akvo.org
# Part 1

# Libraries 
library(readxl)
library(openxlsx)
library(here)

here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/test.R")

library(tidyverse)
library(tidylog)
library(data.table)
library(zoo)
library(splitstackshape)
library(RecordLinkage)
library(stringdist)
library(cld2)

# "EASY" cases
cases <- c(#"2021-06-29_Alluvial_anom.xlsx",
           #"2021-06-29_Coscharis_anom.xlsx",
           #"2021-04-07_Kenya NKG Anom.xlsx",       
           #"2021-04-12_Uganda NKG Anom.xlsx",
           #"2021-04-12_Honduras NKG Anom.xlsx",
           #"2021-04-15_Mexico NKG Anom.xlsx",
           #"2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx",
           #"2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx",
           #"2021-03-19_smart logistics_anom.xlsx",
           "2021-05-14 AIF Anom nyagatare and kirehe.xlsx"#,
           #"kenya-syngenta_potatoes.xlsx",
           #"kenya-syngenta_tomatoes.xlsx",
           #"20082020 Batian Anom.xlsx",
           #"11082020 Mwea Anom.xlsx",
           #"26032020 Musoni Maize.xlsx",
           #"26032020 Musoni Sorghum.xlsx",
           #"2020-11-20 Rubutco Anom busokelo.xlsx",
           #"2020-11-20 Rubutco Anom rungwe.xlsx",
           #"04022020 Egranary Anom.xlsx",
           #"04102019_AGRI_WALLET_anonymized.xlsx",
           #"12122019_Alluvial_transformed.xlsx",
           #"11122019 Coscharis Anom.xlsx",
           #"30012020 McCormick Anom - new format.xlsx"
           )


surveys <- c(#"survey alluvial.xlsx",
             #"survey coscharis.xlsx",
             #"survey nkg kenya.xlsx",                                     
             #"survey nkg honduras.xlsx",
             #"survey nkg mexico.xlsx",
             #"survey nkg uganda.xlsx",
             "survey aif maize Rwanda.xlsx"#,
             #"survey_batian_12082020.xlsx",
             #"survey mwea rice kenya.xlsx",
             #"survey musoni.xlsx",
             #"survey musoni.xlsx",
             #"survey_rubutco.xlsx",
              #"survey_rubutco.xlsx",
              # "survey egranary.xlsx",
              # "survey agri wallet.xlsx",
              # "survey Alluvial 2019.xlsx",
              #"SURVEY_FORM_nigeria_rice_coscharis_14052021.xlsx",
              # "survey mccornick vietnam.xlsx"
             )

# Crops
crops <- c(#"rice", "rice", 
           #"coffee", "coffee","coffee","coffee",
           #"tea", "tea", 
           #"beans", 
           "maize"#, 
           #"potatoes", "tomatoes",
           #"nuts","rice",
           #"maize","sorghum",
           #"tea", "tea",
           #"maize","tomatoes potatoes",
           #"rize", "rice", "pepper"
           )

# Companies
companies <- c(#"alluvial", "coscharis", 
               #"nkg","nkg","nkg","nkg",
               #"ikanga", "ikanga",
               #"smart logistics", 
              "aif"#, 
               #"syngenta", "syngenta", "batian", "mwea",
               #"musoni","musoni",
               #"rubutco","rubutco", "egranary", "agriwallet",
               #"alluvial 19", "coscharis 19", "multiple"
              )

# Translation language (in case of no translation I used Swahili)
languages <- c(#"sw", "sw", "sw", "sw",
               #"es", "es", "sw", "sw",
               #"sw", 
               "rw"#,
               #"sw", "sw",
              # "sw", "sw", "sw", "sw", "sw", "sw", "sw", "sw", "sw", "sw", "sw", "sw", "sw", "sw"
              )

# Survey Library Template - V 2.1.1 (from Flow 15-07-2021)
format_raw_data <- read_excel(
  here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
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
  here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
       "variables with transformation.xlsx")) %>%
  rename("Core" = type) %>%
  mutate(variable = gsub('tea', "focus", variable)) 


# Loop through case:
codebooks <- list()
no_matches <- list()

for(i in 1:length(cases)){
  
  if(!cases[i] %in% c("2021-06-29_Alluvial_anom.xlsx",
                      "2021-06-29_Coscharis_anom.xlsx")){
    
    # CLEANED FILE
    case <- read_excel(
      here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]), 
      sheet="Cleaned Data")
    
    # CODEBOOK
    codebook <- read_excel(
      here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]), 
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
    
  }else{
    # CLEANED FILE
    case <- read_excel(
      here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]))
    
    # CODEBOOK
    codebook <- read_excel(
      here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", surveys[i]),
      sheet="Full Survey",
      skip=2) %>%
      mutate(Title = na.locf(Title, na.rm = FALSE)) %>%
      # Select right columns
      select("Title", "Variable name", "Text",
             "Question type", "Options",
             "Allow multiple") %>%
      # Trim trailing spaces
      mutate_if(is.character, str_trim) %>%
      # Rename columns
      rename("section" = "Title", 
             "variable" = "Variable name", 
             "question" = "Text",
             "type" = "Question type", 
             "options" = "Options",
             "multiple" = "Allow multiple") %>%
      # All variables to lowercase
      mutate_all(tolower) %>%
      # Filter the empty variables
      filter(!is.na(variable)) %>% 
      rename_all(tolower) %>%
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
    
  }
  
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
  
  ############################
  
  # Changes identified by going though the lists BY HAND! 
  
  # Comments Mirza:
  # - repeat no -> repeat_no
  # - hh_loan_interest_rate_SDM -> hh_loan_interest_rate_sdm
  # - cf_shortage_months is missing? - uitgehaald
  # - hh_loan_costs_repaymentvalue is missing? - nieuw
  # Birthyear
  # AIF Rwanda
  if(cases[i] == "2021-05-14 AIF Anom nyagatare and kirehe.xlsx"){
    case <- case %>% rename(
      f_labour_livestock_nrdays = avg_days_livestock,
      cl_coping_mechanisms_other = 'cl_coping_mechanisms--other--',
      f_labour_marketing_wage_per_kg = cost_market_crop,
      cs_sdm_company = cs_aif,
      f_equipment_cost = exp_equipment,
      f_focus_quant_lost = f_crop_lost,
      f_focus_measurement_lost = f_crop_lost_measure,
      f_quant_lost_poultry_feed  = f_crop_poultry,
      f_focus_crop_size_hectare = f_focus_crop_sizeHA,
      f_focus_measurement_prod_other_kg = f_focus_measurement_other,
      f_focus_own_consumption_measurement = f_focus_quant_measure,
      f_focus_measurement_lost_2 = f_focus_quant_measure_lost,
      f_focus_price = f_focus_sell_season,
      f_harvest_num = f_harvest_number_of_times,
      f_livestock_income_total = f_inc_add_livestock_poultry,
      f_livestock_days_hiredlabour = f_inc_day_ppl_livestock_poultry,
      f_livestock_nr_hired_labourers = f_inc_nrppl_hire_livestock_poultry,
      f_livestock_wages_hiredlabour = f_inc_pay_ppl_livestock_poultry,
      f_livestock_hiredlabour_yn = f_income_hireppl_livestock_poultry,
      f_livestock_income_type = f_income_livestock_poultry,
      f_income_other_total = f_income_other_sources,
      f_income_other_type_other = 'f_income_other_type--other--',
      f_inputs_challenges_types_other = 'f_inputs_challenges_types--other--',
      f_inputs_costs_chemicals_3 = f_inputs_costs_fungicides,
      f_inputs_costs_chemcials_4 = f_inputs_costs_pesticides,
      f_livestock_costs_medics = f_inputs_usage_medicine,
      f_labour_agrochemicalapp_nrhiredpeople = f_labour_farm_nrhire_agrochem,
      f_labour_cropmaint_nrhiredpeople = f_labour_farm_nrhire_cropmaintenance,
      f_labour_harvesting_nrhiredpeople = f_labour_farm_nrhire_harvesting,
      f_labour_irrigation_nrhiredpeople = f_labour_farm_nrhire_irrigaion,
      f_labour_marketing_nrhiredpeople = f_labour_farm_nrhire_marketing,
      f_labour_planting_nrhiredpeople = f_labour_farm_nrhire_planting,
      f_labour_postharvesting_nrhiredpeople = f_labour_farm_nrhire_postharvesting,
      f_labour_spraying_nrhiredpeople = f_labour_farm_nrhire_spraying,
      f_labour_agrochemicalapp_nrdays = f_labour_farm_nrpeople_agrochem,
      f_labour_cropmaint_nrdays = f_labour_farm_nrpeople_cropmaintenance,
      f_labour_harvesting_nrdays = f_labour_farm_nrpeople_harvesting,
      f_labour_irrigation_nrdays = f_labour_farm_nrpeople_irrigation,
      f_labour_marketing_nrdays = f_labour_farm_nrpeople_marketing,
      f_labour_planting_nrdays = f_labour_farm_nrpeople_planting,
      f_labour_postharvesting_nrdays = f_labour_farm_nrpeople_postharvesting,
      f_labour_spraying_nrdays = f_labour_farm_nrpeople_spraying,
      f_labour_agrochemicalapp_nrpeople = f_labour_farm_permanent_day_agrochem,
      f_labour_cropmaint_nrpeople = f_labour_farm_permanent_day_cropmaintenance,
      f_labour_harvesting_nrpeople = f_labour_farm_permanent_day_harvesting,
      f_labour_irrigation_nrpeople = f_labour_farm_permanent_day_irrigation,
      f_labour_planting_nrpeople = f_labour_farm_permanent_day_planting,
      f_labour_postharvesting_nrpeople = f_labour_farm_permanent_day_postharvesting,
      f_labour_spraying_nrpeople = f_labour_farm_permanent_day_spraying,
      f_ownership_type = f_land_own,
      f_focus_quant_lost_kg = 'f_lost (kilograms)',
      f_other_crop_income = f_othermaincrop_spend,
      f_focus_own_consumption_kg = 'f_own_consumption (kilograms)',
      f_focus_quant_prod_kg = 'f_produced (kilograms)',
      f_size_hectare = f_size_Ha,
      f_focus_quant_sold_kg = 'f_sold (kilograms)',
      f_unit_land = f_unit_land_no,
      f_unit_land_other = 'f_unit_land_no--other--',
      #sdm_farmer = farm_incharge_present,
      fs_introduction_other = 'fs_introduction--other--',
      #focus_crop = grow,
      hh_loan_purpose_other = 'hh_loan_purpose--other--',
      hh_loan_source_other = 'hh_loan_source--other--',
      f_labour_agrochemicalapp_paymentpertimeframe = hire_people_pay_perday_agrochem,
      f_labour_cropmaint_paymentpertimeframe = hire_people_pay_perday_cropmaintenance,
      f_labour_harvesting_paymentpertimeframe = hire_people_pay_perday_harevsting,
      f_labour_irrigation_paymentpertimeframe = hire_people_pay_perday_irrigation,
      f_labour_planting_paymentpertimeframe = hire_people_pay_perday_planting,
      f_labour_postharvesting_paymentpertimeframe = hire_people_pay_perday_postharevsting,
      f_labour_spraying_paymentpertimeframe = hire_people_pay_perday_spraying,
      cs_timely_payment = iaf_pay,
      cs_recommendation = iaf_recommend,
      f_labour_harvesting_kg_per_person = kg_marketedcrop_hiredlabour,
      f_livestock_nr_labourers = livestock_pplnr,
      hh_loan_source_inputs = loan_inputs,
      f_maincrop = maincrop,
      f_maincrop_other = maincrop_other,
      f_inputs_usage_other_2 = other_inputs,
      f_equip_type_other = other_tool,
      f_livestock_wages_hiredlabour_2 = pay_perday_livestock,
      f_livestock_nr_hired_labourers_2 = pplnr_livestock_hire,
      f_equip_harvesting_ownership_type = rent_harvester,
      f_equip_irrigation_ownership_type = rent_irrigation,
      f_equip_other_ownership_type = rent_other,
      f_equip_tillers_ownership_type = rent_rot,
      f_equip_weeding_ownership_type = rent_weeding,
      hh_farmer_birthyear = res_age,
      hh_farmer_gender = res_gender,
      f_othermaincrop_1 = second_maincrop,
      f_othermaincrop_1_other = second_maincrop_other,
      cs_sdm_company_services = services_aif_usage,
      cf_shortage_months = shortage_money_months,
      su_farmer_organisation = su_farmer_organisation_x,
      f_othermaincrop_2 = third_maincrop,
      f_othermaincrop_2_other = third_maincrop_other,
      cs_positive_recommendation = why_iaf,
      cs_negative_recommendation = whynot_iaf )
  }
  ############################
  
  # REQUESTS OSCAR
  
  # Derived:
  # cal_productivity
  
  if("repeat no" %in% colnames(case)){
    case <- case %>%
      rename("repeat_no" = `repeat no`)
  }
  
  if("hh_loan_interest_rate_SDM" %in% colnames(case)){
    case <- case %>%
      rename(hh_loan_interest_rate_sdm = hh_loan_interest_rate_SDM)
  }
  
  if("f_size (acre)" %in% colnames(case)){
    case <- case %>% 
      mutate(`f_size (acre)` = as.numeric(`f_size (acre)`)) %>%
      mutate("f_size_hectare" = `f_size (acre)`/2.471) %>%
      rename("f_size_acre" = "f_size (acre)")
  }
  
  if("f_focus_crop_size (acre)" %in% colnames(case)){
    case <- case %>%
      mutate(`f_focus_crop_size (acre)` = as.numeric(`f_focus_crop_size (acre)`)) %>%
      mutate("f_focus_crop_size_hectare" = `f_focus_crop_size (acre)`/2.471) %>%
      rename("f_focus_crop_size_acre" = "f_focus_crop_size (acre)")
  }
  
  if("f_size_othermaincrop_1 (acre)" %in% colnames(case)){
    case <- case %>%
      mutate(`f_size_othermaincrop_1 (acre)` = as.numeric(`f_size_othermaincrop_1 (acre)`)) %>%
      mutate(`f_size_othermaincrop_2 (acre)` = as.numeric(`f_size_othermaincrop_2 (acre)`)) %>%
      mutate("f_size_othermaincrop_1_hectare" = `f_size_othermaincrop_1 (acre)`/2.471) %>%
      mutate("f_size_othermaincrop_2_hectare" = `f_size_othermaincrop_2 (acre)`/2.471) %>%
      rename("f_size_othermaincrop_1_acre" = "f_size_othermaincrop_1 (acre)",
             "f_size_othermaincrop_2_acre" = "f_size_othermaincrop_2 (acre)")
  }
  
  if("cal_focus_productivity" %in% colnames(case)){
    case <- case %>% 
      mutate("cal_focus_productivity" = as.numeric(cal_focus_productivity)) %>%
      mutate("cal_focus_productivity_hectare" = cal_focus_productivity/2.471) %>%
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
    mutate_at(vars(starts_with("hh_") & contains("_birthyear")), as.numeric) %>%
    mutate_at(vars(starts_with("hh_") & contains("_age")), as.numeric) %>%
    mutate_at(vars(starts_with("hh_") & contains("_birthyear")), 
              function(x) ifelse(x < 120, 2021 - x, x)) %>%
    mutate_at(vars(starts_with("hh_") & contains("_age")), 
              function(x) ifelse(x < 120, 2021 - x, x)) %>% 
    rename_at(vars(starts_with("hh_") & contains("_age")), 
              funs(gsub("_age", "_birthyear", ., perl=T)))
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output", cases[i]))
  
}

# write the lists for manual checks - differing questions:
write.xlsx(codebooks, file = here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/", 
                                  "Mismatch in questions_test.xlsx"), 
           sep=",", 
           row.names = FALSE)

# write the lists for manual checks - no match at all:
write.xlsx(no_matches, file = here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/", 
                                   "Mismatch in question and varname_test.xlsx"), 
           sep=",", 
           row.names = FALSE)

# # Compare the "no match" sets:
# match_in_no_match <- colnames(no_matches[1][[1]])
# 
# for(i in 1:(length(no_matches)-1)){
#   print(i)
#   match_in_no_match <- append(match_in_no_match, colnames(no_matches[i+1][[1]]))
# }
# 
# match_in_no_match %>% table() %>% melt() %>% View()


