# Script: Data alignment IDH small holder farmer primary data
# Date: October 2021
# Contact: jildemarie@akvo.org


# Libraries 
library(readxl)
library(openxlsx)
library(here)

here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/data alignment_part_1.R")

library(tidyverse)
library(tidylog)
library(data.table)
library(zoo)
library(splitstackshape)
library(RecordLinkage)
library(stringdist)
library(cld2)

## ---- Merge 2 datasets for Robutco ----
rubutco_busokelo <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original/",
       "2020-11-20 Rubutco Anom busokelo.xlsx"),
  sheet = "Cleaned Data")

rubutco_busokelo <- rubutco_busokelo %>%
  mutate(region = c("busokelo"))
rubutco_rungwe <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original/",
       "2020-11-20 Rubutco Anom rungwe.xlsx"),
  sheet = "Cleaned Data")

codebook <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original/",
       "2020-11-20 Rubutco Anom rungwe.xlsx"),
  sheet = "Code book")

rubutco_rungwe <- rubutco_rungwe %>%
  mutate(region = c("rungwe"))

rubutco <- rbind(rubutco_busokelo, rubutco_rungwe)
sets <- list("Cleaned Data" = rubutco,
             "Code book" = codebook)

write.xlsx(sets, file = here::here("/Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original/", 
                             "2020-11-20 Rubutco Anom.xlsx"), 
           sep=",", 
           row.names = FALSE,
           overwrite=TRUE)


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
format_raw_data <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
       "question library format v2.1.1.xlsx")) %>%
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
format_transformations <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
       "variables with transformation.xlsx"))

## ---- Loop through case ----

codebooks <- list()
no_matches <- list()

for(i in 1:length(cases)){
  
    # CLEANED FILE
    case <- read_excel(
      here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]), 
      sheet="Cleaned Data")

    # CODEBOOK
    codebook <- read_excel(
      here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Samepage/Original", cases[i]), 
      sheet="Code book") %>%
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
  match_codebook_questions <- codebook %>% 
    select(variable, question) %>% 
    dplyr::rename(case_question = question) %>%
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
    dplyr::rename("case_variable" = variable,
           "question" = question,
           "question_group" = section) %>%
    inner_join(format_data %>% select(variable, question, question_group)) %>%
    mutate("match" = ifelse(case_variable == variable, TRUE, FALSE)) %>%
    filter(match == FALSE) 
  
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
  ## ---- 1) Alluvial ----
  if(cases[i] == "2021-06-29_Alluvial_anom.xlsx"){
    case <- case %>% dplyr::rename(
      focus_crop = sdm_crop,
      f_size_acre = `f_size (acre)`,
      f_focus_crop_size_acre = `f_focus_crop_size (acre)`,
      f_size_othermaincrop_1_acre = `f_size_othermaincrop_1 (acre)`,
      f_size_othermaincrop_2_acre = `f_size_othermaincrop_2 (acre)`,
      cal_focus_revenu = cal_focus_revenue,
      f_gap_types_applied = f_gap_use,
      f_gap_training_received = f_training_gap,
      f_gap_training_provider = f_who_trained_gap,
      cal_farm_costs_general = cal_farm_general_cost) %>%
      
      select(-State, -Community,
             -starts_with("cal_labour_cropmaint"),
             -starts_with("cal_labour_fertilizerapp"),
             -starts_with("cal_labour_harvesting"),
             -starts_with("cal_labour_irrigation"),
             -starts_with("cal_labour_landprep"),
             -starts_with("cal_labour_nurserymaint"),
             -starts_with("cal_labour_postharvest"),
             -cal_othermaincrop_income)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "sdm_crop","focus_crop",variable),
      variable = ifelse(variable == "cal_focus_revenue","cal_focus_revenu",variable),
      variable = ifelse(variable == "f_gap_use","f_gap_types_applied",variable),
      variable = ifelse(variable == "f_training_gap","f_gap_training_received",variable),
      variable = ifelse(variable == "f_who_trained_gap","f_gap_training_provider",variable))
  }

  
  ## ---- 2) Coscharis----
  if(cases[i] == "2021-06-29_Coscharis_anom.xlsx"){
    case <- case %>% dplyr::rename(
      focus_crop = sdm_crop,
      # f_unit_land_hectare_yes = f_unit_land,
      # f_focus_measurement_prod_kg_yes = f_focus_measurement_prod,
      f_size_acre = `f_size (acre)`,
      f_focus_crop_size_acre = `f_focus_crop_size (acre)`,
      f_size_othermaincrop_1_acre = `f_size_othermaincrop_1 (acre)`,
      f_size_othermaincrop_2_acre = `f_size_othermaincrop_2 (acre)`,
      cal_focus_revenu = cal_focus_revenue,
      f_livestock_days_hiredlabour_2 = f_livestcok_days_hiredlabour_2,
      f_gap_types_applied = f_gap_use,
      f_gap_training_received = f_training_gap,
      f_gap_training_provider = f_who_trained_gap,
      cal_farm_costs_general = cal_farm_general_cost) %>%
      
      select(-State, -Community,
             -starts_with("cal_labour_cropmaint"),
             -starts_with("cal_labour_fertilizerapp"),
             -starts_with("cal_labour_harvesting"),
             -starts_with("cal_labour_irrigation"),
             -starts_with("cal_labour_landprep"),
             -starts_with("cal_labour_nurserymaint"),
             -starts_with("cal_labour_postharvest"),
             -cal_othermaincrop_income)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "sdm_crop","focus_crop",variable),
      variable = ifelse(variable == "f_unit_land","# f_unit_land_hectare_yes",variable),
      variable = ifelse(variable == "cal_focus_revenue","cal_focus_revenu",variable),
      variable = ifelse(variable == "f_livestcok_days_hiredlabour_2","f_livestock_days_hiredlabour_2",variable),
      variable = ifelse(variable == "f_gap_use","f_gap_types_applied",variable),
      variable = ifelse(variable == "f_training_gap","f_gap_training_received",variable),
      variable = ifelse(variable == "f_who_trained_gap","f_gap_training_provider",variable))
      
  }

  ## ---- 3) NKG Kenya ----
  if(cases[i] == "2021-04-07_Kenya NKG Anom.xlsx"){
    case <- case %>% dplyr::rename(
      f_size_acre = `f_size (acre)`,
      f_focus_crop_size_acre =  `f_sdm_size (acre)`,
      f_harvest_num = f_seasons_num,
      f_cp_kenya_type = f_cp_type,
      f_other_crops_type = f_number_of_crops,
      f_coop_fee = f_coop_fee_amount,
      cal_focus_measurement_prod_s1 = cal_coffee_measurement_prod_s1,
      cal_focus_measurement_sold_s1 = cal_coffee_measurement_sold_s1,
      cal_focus_measurement_lost_s1 = cal_coffee_measurement_lost_s1,
      cal_focus_quant_prod_s1 = cal_coffee_quant_prod_s1,
      cal_focus_quant_sold_s1 = cal_coffee_quant_sold_s1,
      cal_focus_quant_lost_s1 = cal_coffee_quant_lost_s1,
      cal_focus_income_s1 = cal_coffee_income_s1,
      cal_focus_price_s1 = cal_coffee_price_s1,
      cal_focus_measurement_prod_s2 = cal_coffee_measurement_prod_s2,
      cal_focus_measurement_sold_s2 = cal_coffee_measurement_sold_s2,
      cal_focus_measurement_lost_s2 = cal_coffee_measurement_lost_s2,
      cal_focus_quant_prod_s2 = cal_coffee_quant_prod_s2,
      cal_focus_quant_sold_s2 = cal_coffee_quant_sold_s2,
      cal_focus_quant_lost_s2 = cal_coffee_quant_lost_s2,
      cal_focus_income_s2 = cal_coffee_income_s2,
      cal_focus_price_s2 = cal_coffee_price_s2,
      cal_focus_quant_prod = cal_coffee_quant_prod,
      cal_focus_quant_sold = cal_coffee_quant_sold,
      cal_focus_quant_lost = cal_coffee_quant_lost,
      cal_focus_revenue = cal_coffee_income,
      cal_focus_price = cal_coffee_price,
      cal_focus_cost = cal_coffee_cost,
      cal_focus_income = cal_coffee_netincome,
      ppi_ken_education_female = ppi_ken_educ_fem,
      ppi_ken_education_household = ppi_ken_educ_any,
      cal_hh_farmer_age = hh_age,
      hh_farmer_gender = hh_member_gender,
      hh_education_farmer = hh_member_educ_1,
      hh_size = hh_size_1,
      hh_male_nr = hh_male,
      hh_female_nr = hh_female,
      su_farmer_organisation = su_cooperative,
      pi_location_cascade_county = pi_location_cascade_county_,
      focus_crop = coffee_yn,
      f_coffee_trees_amount = f_coffee_trees_num,
      f_coffee_tree_age_0_to_4 = f_coffee_tree_age_0to4,
      f_coffee_tree_age_5_to_10 = f_coffee_tree_age_5to10,
      f_coffee_tree_age_11_to_20 = f_coffee_tree_age_11to20,
      f_coffee_tree_age_21_to_30 = f_coffee_tree_age_21to30,
      f_coffee_shade_trees = f_shade_trees_num,
      f_coffee_shade_trees_type1 = f_shade_trees_type1,
      f_coffee_shade_trees_type2 = f_shade_trees_type2,
      f_coffee_processing_steps = f_processing_steps,
      cal_actual_income = cal_actual_netincome,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_farm_income = cal_farm_netincome,
      cal_inputs_costs_s1 = cal_inputs_s1_costs,
      cal_inputs_costs_s2 = cal_inputs_s2_costs,
      cal_livestock_revenue = cal_livestock_income,
      cal_livestock_income = cal_livestock_netincome,
      cal_off_farm_labour_income = cal_offfarm_labour_netincome,
      cal_offfarm_income = cal_offfarm_netincome,
      cal_offfarm_non_labour_income = cal_offfarm_non_labour_netincome,
      cal_other_crop_income = cal_other_crop_netincome) %>%
      select(-contains("cal_labour_s1"), -contains( "cal_labour_s2"), - contains("all_cost")) %>%
      mutate(cal_othermaincrop_revenue = cal_othermaincrop_income,
             cal_other_crop_income = cal_othermaincrop_netincome + f_other_crop_income) %>%
      select(-cal_othermaincrop_income, -cal_othermaincrop_netincome)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "f_seasons_num","f_harvest_num",variable),
      variable = ifelse(variable == "f_cp_type","f_cp_kenya_type",variable),
      variable = ifelse(variable == "f_number_of_crops","f_other_crops_type",variable),
      variable = ifelse(variable == "f_coop_fee_amount","f_coop_fee",variable),
      variable = ifelse(variable == "cal_coffee_measurement_prod_s1","cal_focus_measurement_prod_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_sold_s1","cal_focus_measurement_sold_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_lost_s1","cal_focus_measurement_lost_s1",variable),
      variable = ifelse(variable == "cal_coffee_quant_prod_s1","cal_focus_quant_prod_s1",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold_s1","cal_focus_quant_sold_s1",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost_s1","cal_focus_quant_lost_s1",variable),
      variable = ifelse(variable == "cal_coffee_income_s1","cal_focus_income_s1",variable),
      variable = ifelse(variable == "cal_coffee_price_s1","cal_focus_price_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_prod_s2","cal_focus_measurement_prod_s2",variable),
      variable = ifelse(variable == "cal_coffee_measurement_sold_s2","cal_focus_measurement_sold_s2",variable),
      variable = ifelse(variable == "cal_coffee_measurement_lost_s2","cal_focus_measurement_lost_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_prod_s2","cal_focus_quant_prod_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold_s2","cal_focus_quant_sold_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost_s2","cal_focus_quant_lost_s2",variable),
      variable = ifelse(variable == "cal_coffee_income_s2","cal_focus_income_s2",variable),
      variable = ifelse(variable == "cal_coffee_price_s2","cal_focus_price_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_prod","cal_focus_quant_prod",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold","cal_focus_quant_sold",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost","cal_focus_quant_lost",variable),
      variable = ifelse(variable == "cal_coffee_income","cal_focus_revenue",variable),
      variable = ifelse(variable == "cal_coffee_price","cal_focus_price",variable),
      variable = ifelse(variable == "cal_coffee_cost","cal_focus_cost",variable),
      variable = ifelse(variable == "cal_coffee_netincome","cal_focus_income",variable),
      variable = ifelse(variable == "ppi_ken_educ_fem","ppi_ken_education_female",variable),
      variable = ifelse(variable == "ppi_ken_educ_any","ppi_ken_education_household",variable),
      variable = ifelse(variable == "hh_age","cal_hh_farmer_age",variable),
      variable = ifelse(variable == "hh_member_gender","hh_farmer_gender",variable),
      variable = ifelse(variable == "hh_member_educ_1","hh_education_farmer",variable),
      variable = ifelse(variable == "hh_size_1","hh_size",variable),
      variable = ifelse(variable == "hh_male","hh_male_nr",variable),
      variable = ifelse(variable == "hh_female","hh_female_nr",variable),
      variable = ifelse(variable == "su_cooperative","su_farmer_organisation",variable),
      variable = ifelse(variable == "pi_location_cascade_county_","pi_location_cascade_county",variable),
      variable = ifelse(variable == "coffee_yn","focus_crop",variable),
      variable = ifelse(variable == "f_coffee_trees_num","f_coffee_trees_amount",variable),
      variable = ifelse(variable == "f_coffee_tree_age_0to4","f_coffee_tree_age_0_to_4",variable),
      variable = ifelse(variable == "f_coffee_tree_age_5to10","f_coffee_tree_age_5_to_10",variable),
      variable = ifelse(variable == "f_coffee_tree_age_11to20","f_coffee_tree_age_11_to_20",variable),
      variable = ifelse(variable == "f_coffee_tree_age_21to30","f_coffee_tree_age_21_to_30",variable),
      variable = ifelse(variable == "f_shade_trees_num","f_coffee_shade_trees",variable),
      variable = ifelse(variable == "f_shade_trees_type1","f_coffee_shade_trees_type1",variable),
      variable = ifelse(variable == "f_shade_trees_type2","f_coffee_shade_trees_type2",variable),
      variable = ifelse(variable == "f_processing_steps","f_coffee_processing_steps",variable),
      variable = ifelse(variable == "cal_actual_netincome","cal_actual_income",variable),
      variable = ifelse(variable == "cal_farm_general_cost","cal_farm_costs_general",variable),
      variable = ifelse(variable == "cal_farm_netincome","cal_farm_income",variable),
      variable = ifelse(variable == "cal_inputs_s1_costs","cal_inputs_costs_s1",variable),
      variable = ifelse(variable == "cal_inputs_s2_costs","cal_inputs_costs_s2",variable),
      variable = ifelse(variable == "cal_livestock_income","cal_livestock_revenue",variable),
      variable = ifelse(variable == "cal_offfarm_labour_netincome","cal_off_farm_labour_income",variable),
      variable = ifelse(variable == "cal_offfarm_netincome","cal_offfarm_income",variable),
      variable = ifelse(variable == "cal_offfarm_non_labour_netincome","cal_offfarm_non_labour_income",variable),
      variable = ifelse(variable == "cal_other_crop_income","cal_other_crop_income",variable))
  }
  
  ## ---- 4) NKG uganda ----
  if(cases[i] == "2021-04-12_Uganda NKG Anom.xlsx"){
    case <- case %>% dplyr::rename(
      f_size_acre = `f_size (acre)`,
      f_focus_crop_size_acre =  `f_sdm_size (acre)`,
      
      f_harvest_num = f_seasons_num,
      
      f_other_crops_type = f_number_of_crops,
      f_coop_fee = f_coop_fee_amount,
      f_cp_uganda_type = f_cp_type_uganda,
      f_farm_size_wild = f_cp_size_wild_ugan,
      f_farm_size_old = f_farm_size_old_ugan,
      f_forestprotection_yn = f_forestprotection_yn_ugan,
      f_protect_watercatch = f_protect_watercatch_ugan,
      f_gap_types_applied = f_gap_type_ugan,
      cal_focus_measurement_prod_s1 = cal_coffee_measurement_prod_s1,
      cal_focus_measurement_sold_s1 = cal_coffee_measurement_sold_s1,
      cal_focus_measurement_lost_s1 = cal_coffee_measurement_lost_s1,
      cal_focus_quant_prod_s1 = cal_coffee_quant_prod_s1,
      cal_focus_quant_sold_s1 = cal_coffee_quant_sold_s1,
      cal_focus_quant_lost_s1 = cal_coffee_quant_lost_s1,
      cal_focus_income_s1 = cal_coffee_income_s1,
      cal_focus_price_s1 = cal_coffee_price_s1,
      cal_focus_measurement_prod_s2 = cal_coffee_measurement_prod_s2,
      cal_focus_measurement_sold_s2 = cal_coffee_measurement_sold_s2,
      cal_focus_measurement_lost_s2 = cal_coffee_measurement_lost_s2,
      cal_focus_quant_prod_s2 = cal_coffee_quant_prod_s2,
      cal_focus_quant_sold_s2 = cal_coffee_quant_sold_s2,
      cal_focus_quant_lost_s2 = cal_coffee_quant_lost_s2,
      cal_focus_income_s2 = cal_coffee_income_s2,
      cal_focus_price_s2 = cal_coffee_price_s2,
      cal_focus_quant_prod = cal_coffee_quant_prod,
      cal_focus_quant_sold = cal_coffee_quant_sold,
      cal_focus_quant_lost = cal_coffee_quant_lost,
      cal_focus_revenue = cal_coffee_income,
      cal_focus_price = cal_coffee_price,
      cal_focus_cost = cal_coffee_cost,
      cal_focus_income = cal_coffee_netincome,
      ppi_ugan_female_language = ppi_ugan_read,
      ppi_ugan_walls = ppi_ugan_wall,
      ppi_ugan_cooking = ppi_ugan_energy,
      ppi_ugan_mobile = ppi_ugan_phones,
      cal_hh_farmer_age = hh_age,
      hh_farmer_gender = hh_member_gender,
      hh_education_farmer = hh_member_educ_1,
      hh_size = hh_size_1,
      hh_male_nr = hh_male,
      hh_female_nr = hh_female,
      su_farmer_organisation = su_cooperative,
      focus_crop = coffee_yn,
      f_coffee_trees_amount = f_coffee_trees_num,
      f_coffee_tree_age_0_to_4 = f_coffee_tree_age_0to4,
      f_coffee_tree_age_5_to_10 = f_coffee_tree_age_5to10,
      f_coffee_tree_age_11_to_20 = f_coffee_tree_age_11to20,
      f_coffee_tree_age_21_to_30 = f_coffee_tree_age_21to30,
      f_coffee_shade_trees = f_shade_trees_num,
      f_coffee_shade_trees_type1 = f_shade_trees_type1,
      f_coffee_shade_trees_type2 = f_shade_trees_type2,
      f_coffee_processing_steps = f_processing_steps ,
      cal_actual_income = cal_actual_netincome,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_farm_income = cal_farm_netincome,
      cal_inputs_costs_s1 = cal_inputs_s1_costs,
      cal_inputs_costs_s2 = cal_inputs_s2_costs,
      cal_livestock_revenue = cal_livestock_income,
      cal_livestock_income = cal_livestock_netincome,
      cal_off_farm_labour_income = cal_offfarm_labour_netincome,
      cal_offfarm_income = cal_offfarm_netincome,
      cal_offfarm_non_labour_income = cal_offfarm_non_labour_netincome,
      cal_other_crop_income = cal_other_crop_netincome) %>%
      select(-contains("cal_labour_s1"), -contains( "cal_labour_s2"), - contains("all_cost")) %>%
      mutate(cal_othermaincrop_revenue = cal_othermaincrop_income,
             cal_other_crop_income = cal_othermaincrop_netincome + f_other_crop_income) %>%
      select(-cal_othermaincrop_income, -cal_othermaincrop_netincome)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "f_seasons_num","f_harvest_num",variable),
      
      variable = ifelse(variable == "f_number_of_crops","f_other_crops_type",variable),
      variable = ifelse(variable == "f_coop_fee_amount","f_coop_fee",variable),
      variable = ifelse(variable == "f_cp_type_uganda","f_cp_uganda_type",variable),
      variable = ifelse(variable == "f_cp_size_wild_ugan","f_farm_size_wild",variable),
      variable = ifelse(variable == "f_farm_size_old_ugan","f_farm_size_old",variable),
      variable = ifelse(variable == "f_forestprotection_yn_ugan","f_forestprotection_yn",variable),
      variable = ifelse(variable == "f_protect_watercatch_ugan","f_protect_watercatch",variable),
      variable = ifelse(variable == "f_gap_type_ugan","f_gap_types_applied",variable),
      variable = ifelse(variable == "cal_coffee_measurement_prod_s1","cal_focus_measurement_prod_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_sold_s1","cal_focus_measurement_sold_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_lost_s1","cal_focus_measurement_lost_s1",variable),
      variable = ifelse(variable == "cal_coffee_quant_prod_s1","cal_focus_quant_prod_s1",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold_s1","cal_focus_quant_sold_s1",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost_s1","cal_focus_quant_lost_s1",variable),
      variable = ifelse(variable == "cal_coffee_income_s1","cal_focus_income_s1",variable),
      variable = ifelse(variable == "cal_coffee_price_s1","cal_focus_price_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_prod_s2","cal_focus_measurement_prod_s2",variable),
      variable = ifelse(variable == "cal_coffee_measurement_sold_s2","cal_focus_measurement_sold_s2",variable),
      variable = ifelse(variable == "cal_coffee_measurement_lost_s2","cal_focus_measurement_lost_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_prod_s2","cal_focus_quant_prod_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold_s2","cal_focus_quant_sold_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost_s2","cal_focus_quant_lost_s2",variable),
      variable = ifelse(variable == "cal_coffee_income_s2","cal_focus_income_s2",variable),
      variable = ifelse(variable == "cal_coffee_price_s2","cal_focus_price_s2",variable),
      variable = ifelse(variable == "cal_coffee_quant_prod","cal_focus_quant_prod",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold","cal_focus_quant_sold",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost","cal_focus_quant_lost",variable),
      variable = ifelse(variable == "cal_coffee_income","cal_focus_revenue",variable),
      variable = ifelse(variable == "cal_coffee_price","cal_focus_price",variable),
      variable = ifelse(variable == "cal_coffee_cost","cal_focus_cost",variable),
      variable = ifelse(variable == "cal_coffee_netincome","cal_focus_income",variable),
      variable = ifelse(variable == "ppi_ugan_read","ppi_ugan_female_language",variable),
      variable = ifelse(variable == "ppi_ugan_wall","ppi_ugan_walls",variable),
      variable = ifelse(variable == "ppi_ugan_energy","ppi_ugan_cooking",variable),
      variable = ifelse(variable == "ppi_ugan_phones","ppi_ugan_mobile",variable),
      variable = ifelse(variable == "hh_age","cal_hh_farmer_age",variable),
      variable = ifelse(variable == "hh_member_gender","hh_farmer_gender",variable),
      variable = ifelse(variable == "hh_member_educ_1","hh_education_farmer",variable),
      variable = ifelse(variable == "hh_size_1","hh_size",variable),
      variable = ifelse(variable == "hh_male","hh_male_nr",variable),
      variable = ifelse(variable == "hh_female","hh_female_nr",variable),
      variable = ifelse(variable == "su_cooperative","su_farmer_organisation",variable),
      variable = ifelse(variable == "coffee_yn","focus_crop",variable),
      variable = ifelse(variable == "f_coffee_trees_num","f_coffee_trees_amount",variable),
      variable = ifelse(variable == "f_coffee_tree_age_0to4","f_coffee_tree_age_0_to_4",variable),
      variable = ifelse(variable == "f_coffee_tree_age_5to10","f_coffee_tree_age_5_to_10",variable),
      variable = ifelse(variable == "f_coffee_tree_age_11to20","f_coffee_tree_age_11_to_20",variable),
      variable = ifelse(variable == "f_coffee_tree_age_21to30","f_coffee_tree_age_21_to_30",variable),
      variable = ifelse(variable == "f_shade_trees_num","f_coffee_shade_trees",variable),
      variable = ifelse(variable == "f_shade_trees_type1","f_coffee_shade_trees_type1",variable),
      variable = ifelse(variable == "f_shade_trees_type2","f_coffee_shade_trees_type2",variable),
      variable = ifelse(variable == "f_processing_steps","f_coffee_processing_steps",variable),
      variable = ifelse(variable == "cal_actual_netincome","cal_actual_income",variable),
      variable = ifelse(variable == "cal_farm_general_cost","cal_farm_costs_general",variable),
      variable = ifelse(variable == "cal_farm_netincome","cal_farm_income",variable),
      variable = ifelse(variable == "cal_inputs_s1_costs","cal_inputs_costs_s1",variable),
      variable = ifelse(variable == "cal_inputs_s2_costs","cal_inputs_costs_s2",variable),
      variable = ifelse(variable == "cal_livestock_income","cal_livestock_revenue",variable),
      variable = ifelse(variable == "cal_offfarm_labour_netincome","cal_off_farm_labour_income",variable),
      variable = ifelse(variable == "cal_offfarm_netincome","cal_offfarm_income",variable),
      variable = ifelse(variable == "cal_offfarm_non_labour_netincome","cal_offfarm_non_labour_income",variable),
      variable = ifelse(variable == "cal_other_crop_income","cal_other_crop_income",variable))
    

  
}
  
  ## ---- 5) NKG Mexico ----
  if(cases[i] == "2021-04-15_Mexico NKG Anom.xlsx"){
    case <- case %>% dplyr::rename(
      f_size_acre = `f_size (acre)`,
      f_other_crops_type = f_number_of_crops,
      f_harvest_num = f_seasons_num,
      
      
      f_cp_mexico_type = f_cp_type_mex,
      f_forestprotection_yn = f_forestprotection_yn_mex,
      f_protect_watercatch = f_protect_watercatch_mex,
      f_gap_types_applied = f_gap_type_mex,
      
      cal_focus_measurement_prod = cal_coffee_measurement_prod_s1,
      cal_focus_measurement_sold = cal_coffee_measurement_sold_s1,
      cal_focus_measurement_lost = cal_coffee_measurement_lost_s1,

      cal_focus_quant_prod = cal_coffee_quant_prod,
      cal_focus_quant_sold = cal_coffee_quant_sold,
      cal_focus_quant_lost = cal_coffee_quant_lost,
      cal_focus_revenue = cal_coffee_income,
      cal_focus_price = cal_coffee_price,
      cal_focus_cost = cal_coffee_cost,
      cal_focus_income = cal_coffee_netincome,
      
      cal_hh_member_age = hh_age_mex,
      hh_member_gender = hh_member_gender_mex,
      hh_member_education = hh_member_educ_mex,
      
      focus_crop = coffee_yn,
      f_focus_crop_size_acre = `f_coffee_size (acre)`,
      
      f_coffee_trees_amount = f_coffee_trees_num,
      f_coffee_tree_age_0_to_4 = f_coffee_tree_age_0to4,
      f_coffee_tree_age_5_to_10 = f_coffee_tree_age_5to10,
      f_coffee_tree_age_11_to_20 = f_coffee_tree_age_11to20,
      f_coffee_tree_age_21_to_30 = f_coffee_tree_age_21to30,
      
      f_coffee_shade_trees_amount_wooduse = f_shade_trees_num_trees_used_for_wood,
      f_coffee_shade_trees_amount_fruittrees = f_shade_trees_num_wild_fruit_trees,
      f_coffee_shade_trees_amount_ornamental = f_shade_trees_num_ornamental_trees,
      f_coffee_shade_trees_wooduse_type = f_shade_trees_trees_used_for_wood,
      f_coffee_shade_trees_fruittrees_type = f_shade_trees_wild_fruit_trees,
      f_coffee_shade_trees_ornamental_type = f_shade_trees_ornamental_trees,
      
      f_month_first_season_start = f_month_last_season_start,
      f_month_first_season_end = f_month_last_season_end,
      f_month_first_season_end_plan = f_month_last_season_will_end,
      f_coffee_processing_steps = f_processing_steps,
      cal_actual_income = cal_actual_netincome,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_farm_income = cal_farm_netincome,
      cal_inputs_costs_s1 = cal_inputs_s1_costs,
      cal_livestock_revenue = cal_livestock_income,
      cal_livestock_income = cal_livestock_netincome,
      cal_off_farm_labour_income = cal_offfarm_labour_netincome,
      cal_offfarm_income = cal_offfarm_netincome,
      cal_offfarm_non_labour_income = cal_offfarm_non_labour_netincome,
      cal_other_crop_income = cal_other_crop_netincome) %>%
      select(-contains("cal_labour_s1"), -contains( "cal_labour_s2"), - contains("all_cost")) %>%
      mutate(cal_othermaincrop_revenue = cal_othermaincrop_income,
             cal_other_crop_income = cal_othermaincrop_netincome + f_other_crop_income) %>%
      select(-cal_othermaincrop_income, -cal_othermaincrop_netincome)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "f_number_of_crops","f_other_crops_type",variable),
      variable = ifelse(variable == "f_seasons_num","f_harvest_num",variable),
      
      
      variable = ifelse(variable == "f_cp_type_mex","f_cp_mexico_type",variable),
      variable = ifelse(variable == "f_forestprotection_yn_mex","f_forestprotection_yn",variable),
      variable = ifelse(variable == "f_protect_watercatch_mex","f_protect_watercatch",variable),
      variable = ifelse(variable == "f_gap_type_mex","f_gap_types_applied",variable),
      
      variable = ifelse(variable == "cal_coffee_measurement_prod_s1","cal_focus_measurement_prod",variable),
      variable = ifelse(variable == "cal_coffee_measurement_sold_s1","cal_focus_measurement_sold",variable),
      variable = ifelse(variable == "cal_coffee_measurement_lost_s1","cal_focus_measurement_lost",variable),
      
      variable = ifelse(variable == "cal_coffee_quant_prod","cal_focus_quant_prod",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold","cal_focus_quant_sold",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost","cal_focus_quant_lost",variable),
      variable = ifelse(variable == "cal_coffee_income","cal_focus_revenue",variable),
      variable = ifelse(variable == "cal_coffee_price","cal_focus_price",variable),
      variable = ifelse(variable == "cal_coffee_cost","cal_focus_cost",variable),
      variable = ifelse(variable == "cal_coffee_netincome","cal_focus_income",variable),
      
      variable = ifelse(variable == "hh_age_mex","cal_hh_member_age",variable),
      variable = ifelse(variable == "hh_member_gender_mex","hh_member_gender",variable),
      variable = ifelse(variable == "hh_member_educ_mex","hh_member_education",variable),
      
      variable = ifelse(variable == "coffee_yn","focus_crop",variable),
      variable = ifelse(variable == "f_coffee_size (acre)","f_focus_crop_size_acre",variable),
      
      variable = ifelse(variable == "f_coffee_trees_num","f_coffee_trees_amount",variable),
      variable = ifelse(variable == "f_coffee_tree_age_0to4","f_coffee_tree_age_0_to_4",variable),
      variable = ifelse(variable == "f_coffee_tree_age_5to10","f_coffee_tree_age_5_to_10",variable),
      variable = ifelse(variable == "f_coffee_tree_age_11to20","f_coffee_tree_age_11_to_20",variable),
      variable = ifelse(variable == "f_coffee_tree_age_21to30","f_coffee_tree_age_21_to_30",variable),
      
      variable = ifelse(variable == "f_shade_trees_num_trees_used_for_wood","f_coffee_shade_trees_amount_wooduse",variable),
      variable = ifelse(variable == "f_shade_trees_num_wild_fruit_trees","f_coffee_shade_trees_amount_fruittrees",variable),
      variable = ifelse(variable == "f_shade_trees_num_ornamental_trees","f_coffee_shade_trees_amount_ornamental",variable),
      variable = ifelse(variable == "f_shade_trees_trees_used_for_wood","f_coffee_shade_trees_wooduse_type",variable),
      variable = ifelse(variable == "f_shade_trees_wild_fruit_trees","f_coffee_shade_trees_fruittrees_type",variable),
      variable = ifelse(variable == "f_shade_trees_ornamental_trees","f_coffee_shade_trees_ornamental_type",variable),
      
      variable = ifelse(variable == "f_month_last_season_start","f_month_first_season_start",variable),
      variable = ifelse(variable == "f_month_last_season_end","f_month_first_season_end",variable),
      variable = ifelse(variable == "f_month_last_season_will_end","f_month_first_season_end_plan",variable),
      variable = ifelse(variable == "f_processing_steps","f_coffee_processing_steps",variable),
      variable = ifelse(variable == "cal_actual_netincome","cal_actual_income",variable),
      variable = ifelse(variable == "cal_farm_general_cost","cal_farm_costs_general",variable),
      variable = ifelse(variable == "cal_farm_netincome","cal_farm_income",variable),
      variable = ifelse(variable == "cal_inputs_s1_costs","cal_inputs_costs_s1",variable),
      variable = ifelse(variable == "cal_livestock_income","cal_livestock_revenue",variable),
      variable = ifelse(variable == "cal_offfarm_labour_netincome","cal_off_farm_labour_income",variable),
      variable = ifelse(variable == "cal_offfarm_netincome","cal_offfarm_income",variable),
      variable = ifelse(variable == "cal_offfarm_non_labour_netincome","cal_offfarm_non_labour_income",variable),
      variable = ifelse(variable == "cal_other_crop_netincome","cal_other_crop_income",variable))
  }
  
  ## ---- 6) NKG Honduras ----
  if(cases[i] == "2021-04-12_Honduras NKG Anom.xlsx"){
    case <- case %>% dplyr::rename(
      f_size_acre = `f_size (acre)`,
      f_other_crops_type = f_number_of_crops,
      f_harvest_num = f_seasons_num,
      
      
      f_cp_honduras_type = f_cp_type_mex, #wrong variable name in honduras survey, used _mex instead of _hond
      f_forestprotection_yn = f_forestprotection_yn_mex,
      f_protect_watercatch = f_protect_watercatch_mex,
      f_gap_types_applied = f_gap_type_mex,
      
      su_farmer_organisation = su_cooperative,
      
      cal_focus_measurement_prod_s1 = cal_coffee_measurement_prod_s1,
      cal_focus_measurement_sold_s1 = cal_coffee_measurement_sold_s1,
      cal_focus_measurement_lost_s1 = cal_coffee_measurement_lost_s1,
      cal_focus_quant_prod = cal_coffee_quant_prod,
      cal_focus_quant_sold = cal_coffee_quant_sold,
      cal_focus_quant_lost = cal_coffee_quant_lost,
      
      cal_focus_revenue = cal_coffee_income,
      cal_focus_price = cal_coffee_price,
      cal_focus_cost = cal_coffee_cost,
      cal_focus_income = cal_coffee_netincome,
      
      cal_hh_member_age = hh_age_mex, #wrong variable name in honduras survey, used _mex instead of _hond
      hh_member_gender = hh_member_gender_mex,
      hh_member_education = hh_member_educ_mex,
      
      focus_crop = coffee_yn,
      f_focus_crop_size_acre = `f_coffee_size (acre)`,
      
      f_coffee_trees_amount = f_coffee_trees_num,
      f_coffee_tree_age_0_to_4 = f_coffee_tree_age_0to4,
      f_coffee_tree_age_5_to_10 = f_coffee_tree_age_5to10,
      f_coffee_tree_age_11_to_20 = f_coffee_tree_age_11to20,
      f_coffee_tree_age_21_to_30 = f_coffee_tree_age_21to30,
      
      f_coffee_shade_trees_type2 = f_shade_trees_type2,
      f_coffee_shade_trees_amount_wooduse = f_shade_trees_num_trees_used_for_wood,
      f_coffee_shade_trees_amount_fruittrees = f_shade_trees_num_wild_fruit_trees,
      f_coffee_shade_trees_amount_ornamental = f_shade_trees_num_ornamental_trees,
      f_coffee_shade_trees_amount_lugmes = f_shade_trees_num_lugmes_trees,      
      f_coffee_shade_trees_wooduse_type = f_shade_trees_trees_used_for_wood,
      f_coffee_shade_trees_fruittrees_type = f_shade_trees_wild_fruit_trees,
      f_coffee_shade_trees_ornamental_type = f_shade_trees_ornamental_trees_1,
      
      f_month_first_season_start = f_month_last_season_start,
      f_month_first_season_end = f_month_last_season_end,
      f_month_first_season_end_plan = f_month_last_season_will_end,
      
      f_coffee_processing_steps = f_processing_steps,
      cal_actual_income = cal_actual_netincome,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_farm_income = cal_farm_netincome,
      cal_inputs_costs_s1 = cal_inputs_s1_costs,
      cal_livestock_revenue = cal_livestock_income,      
      cal_livestock_income = cal_livestock_netincome,
      cal_off_farm_labour_income = cal_offfarm_labour_netincome,
      cal_offfarm_income = cal_offfarm_netincome,
      cal_offfarm_non_labour_income = cal_offfarm_non_labour_netincome,
      cal_other_crop_income = cal_other_crop_netincome) %>%
      select(-contains("cal_labour_s1"), -contains( "cal_labour_s2"), - contains("all_cost")) %>%
      mutate(cal_othermaincrop_revenue = cal_othermaincrop_income,
             cal_other_crop_income = cal_othermaincrop_netincome + f_other_crop_income) %>%
      select(-cal_othermaincrop_income, -cal_othermaincrop_netincome)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "f_number_of_crops","f_other_crops_type",variable),
      variable = ifelse(variable == "f_seasons_num","f_harvest_num",variable),
      
      
      variable = ifelse(variable == "f_cp_type_mex","f_cp_honduras_type",variable),
      variable = ifelse(variable == "f_forestprotection_yn_mex","f_forestprotection_yn",variable),
      variable = ifelse(variable == "f_protect_watercatch_mex","f_protect_watercatch",variable),
      variable = ifelse(variable == "f_gap_type_mex","f_gap_types_applied",variable),
      
      variable = ifelse(variable == "su_cooperative","su_farmer_organisation",variable),
      
      variable = ifelse(variable == "cal_coffee_measurement_prod_s1","cal_focus_measurement_prod_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_sold_s1","cal_focus_measurement_sold_s1",variable),
      variable = ifelse(variable == "cal_coffee_measurement_lost_s1","cal_focus_measurement_lost_s1",variable),
      variable = ifelse(variable == "cal_coffee_quant_prod","cal_focus_quant_prod",variable),
      variable = ifelse(variable == "cal_coffee_quant_sold","cal_focus_quant_sold",variable),
      variable = ifelse(variable == "cal_coffee_quant_lost","cal_focus_quant_lost",variable),
      
      variable = ifelse(variable == "cal_coffee_income","cal_focus_revenue",variable),
      variable = ifelse(variable == "cal_coffee_price","cal_focus_price",variable),
      variable = ifelse(variable == "cal_coffee_cost","cal_focus_cost",variable),
      variable = ifelse(variable == "cal_coffee_netincome","cal_focus_income",variable),
      
      variable = ifelse(variable == "hh_age_mex","cal_hh_member_age",variable),
      variable = ifelse(variable == "hh_member_gender_mex","hh_member_gender",variable),
      variable = ifelse(variable == "hh_member_educ_mex","hh_member_education",variable),
      
      variable = ifelse(variable == "coffee_yn","focus_crop",variable),
      variable = ifelse(variable == "f_coffee_size (acre)","f_focus_crop_size_acre",variable),
      
      variable = ifelse(variable == "f_coffee_trees_num","f_coffee_trees_amount",variable),
      variable = ifelse(variable == "f_coffee_tree_age_0to4","f_coffee_tree_age_0_to_4",variable),
      variable = ifelse(variable == "f_coffee_tree_age_5to10","f_coffee_tree_age_5_to_10",variable),
      variable = ifelse(variable == "f_coffee_tree_age_11to20","f_coffee_tree_age_11_to_20",variable),
      variable = ifelse(variable == "f_coffee_tree_age_21to30","f_coffee_tree_age_21_to_30",variable),
      
      variable = ifelse(variable == "f_shade_trees_type2","f_coffee_shade_trees_type2",variable),
      variable = ifelse(variable == "f_shade_trees_num_trees_used_for_wood","f_coffee_shade_trees_amount_wooduse",variable),
      variable = ifelse(variable == "f_shade_trees_num_wild_fruit_trees","f_coffee_shade_trees_amount_fruittrees",variable),
      variable = ifelse(variable == "f_shade_trees_num_ornamental_trees","f_coffee_shade_trees_amount_ornamental",variable),
      variable = ifelse(variable == "f_shade_trees_num_lugmes_trees","f_coffee_shade_trees_amount_lugmes",variable),
      variable = ifelse(variable == "f_shade_trees_trees_used_for_wood","f_coffee_shade_trees_wooduse_type",variable),
      variable = ifelse(variable == "f_shade_trees_wild_fruit_trees","f_coffee_shade_trees_fruittrees_type",variable),
      variable = ifelse(variable == "f_shade_trees_ornamental_trees_1","f_coffee_shade_trees_ornamental_type",variable),
      
      variable = ifelse(variable == "f_month_last_season_start","f_month_first_season_start",variable),
      variable = ifelse(variable == "f_month_last_season_end","f_month_first_season_end",variable),
      variable = ifelse(variable == "f_month_last_season_will_end","f_month_first_season_end_plan",variable),
      
      variable = ifelse(variable == "f_processing_steps","f_coffee_processing_steps",variable),
      variable = ifelse(variable == "cal_actual_netincome","cal_actual_income",variable),
      variable = ifelse(variable == "cal_farm_general_cost","cal_farm_costs_general",variable),
      variable = ifelse(variable == "cal_farm_netincome","cal_farm_income",variable),
      variable = ifelse(variable == "cal_inputs_s1_costs","cal_inputs_costs_s1",variable),
      variable = ifelse(variable == "cal_livestock_income","cal_livestock_revenue",variable),
      variable = ifelse(variable == "cal_offfarm_labour_netincome","cal_off_farm_labour_income",variable),
      variable = ifelse(variable == "cal_offfarm_netincome","cal_offfarm_income",variable),
      variable = ifelse(variable == "cal_offfarm_non_labour_netincome","cal_offfarm_non_labour_income",variable),
      variable = ifelse(variable == "cal_other_crop_netincome","cal_other_crop_income",variable))
  }
  
  ## ---- 7) EU TANZ TEA Ikanga ----
  if(cases[i] == "2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx"){
    case <- case %>% dplyr::rename(
      f_focus_crop_size_acre =  `f_focus_crop_size (acre)`,
      f_size_othermaincrop_1_acre =  `f_size_othermaincrop_1 (acre)`,
      f_size_othermaincrop_2_acre =  `f_size_othermaincrop_2 (acre)`,
      f_size_acre = `f_size (acre)`,
      focus_crop = sdm_crop,
      #f_labour_infilling_nrpeople = f_labour_compostprep_nrpeople,
      fo_farming_investment_positive = fo_farming_investment,
      f_focus_quant_prod = f_tea_quant_prod,
      f_focus_quant_sold = f_tea_quant_sold,
      f_focus_measurement_sold = f_tea_measurement_sold,
      f_focus_price = f_tea_price,
      f_focus_quant_lost = f_tea_quant_lost,
      f_focus_measurement_lost = f_tea_measurement_lost,
      f_other_crops_type = f_othermaincrop_1_1,
      f_focus_measurement_prod = f_measurement_tea,
      f_focus_measurement_prod_other = f_measurement_other_tea,
      f_tea_bush_age_2_to_3 = f_tea_bush_age_2to3,
      f_tea_bush_age_3_to_40 = f_tea_bush_age_3to40,
      m_crops_livestock_seller_other = f_sellingpoint_other,
      f_tea_plucking_wage_female = l_payment_female,
      f_tea_plucking_wage_male = l_payment_male,
      f_tea_plucking_wage_youth = l_payment_youth,
      su_farmer_organisation = su_farmer_organisation_x,
      su_farmer_organisation_name = su_farmer_member_organisation,
      f_factory_selling = su_factory_sell,
      cs_sdm_company = su_sdmcompany,
      fs_breakfast_food = f_breakfast_food,
      fs_food_midmorning = n_food_midmorning,
      fs_foods_lunch = n_foods_lunch,
      fs_food_midafternoon = n_food_midafternoon,
      fs_foods_dinner = n_foods_dinner,
      fs_foods_other = n_foods_other,
      hh_phone_financial_service = m_finances_registered,
      ppi_tanz_building_main = ppi_building_main,
      ppi_tanz_building_roof = ppi_building_roof,
      ppi_tanz_fuel_main = ppi_fuel_main,
      ppi_tanz_own_tv = ppi_own_tv,
      ppi_tanz_own_radio = ppi_own_radio,
      ppi_tanz_own_lanterns = ppi_own_lanterns,
      ppi_tanz_own_tables = ppi_own_tables,
      ppi_tanz_own_livestock = ppi_own_livestock,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_tea_second_payment = cal_second_payment,
      hh_nrmembers_u18_inschool = hh_age_6to18_school,
      hh_nrmembers_u18 = hh_age_nrmember,
      cal_othermaincrop_1_inc_sold = f_othermaincrop_1_inc_sold,
      cal_othermaincrop_1_cost = cal_othermaincrop_cost) %>%
      select( -starts_with("cal_labour_agrochemicalapp"),
             -starts_with("cal_labour_fertilizerapp"),
             -starts_with("cal_labour_harvesting"),
             -starts_with("cal_labour_cropmaint"),
             -starts_with("cal_labour_landprep"),
             -starts_with("cal_labour_infilling"),
             -starts_with("cal_labour_postharvest"),
             -contains("_nrhoursdays"),
             -cal_othermaincrop_income)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "sdm_crop","focus_crop",variable),
      variable = ifelse(variable == "f_labour_compostprep_nrpeople","f_labour_infilling_nrpeople",variable),
      variable = ifelse(variable == "fo_farming_investment","fo_farming_investment_positive",variable),
      variable = ifelse(variable == "f_tea_quant_prod","f_focus_quant_prod",variable),
      variable = ifelse(variable == "f_tea_quant_sold","f_focus_quant_sold",variable),
      variable = ifelse(variable == "f_tea_measurement_sold","f_focus_measurement_sold",variable),
      variable = ifelse(variable == "f_tea_price","f_focus_price",variable),
      variable = ifelse(variable == "f_tea_quant_lost","f_focus_quant_lost",variable),
      variable = ifelse(variable == "f_tea_measurement_lost","f_focus_measurement_lost",variable),
      variable = ifelse(variable == "f_othermaincrop_1_1","f_other_crops_type",variable),
      variable = ifelse(variable == "f_measurement_tea","f_focus_measurement_prod",variable),
      variable = ifelse(variable == "f_measurement_other_tea","f_focus_measurement_prod_other",variable),
      variable = ifelse(variable == "f_tea_bush_age_2to3","f_tea_bush_age_2_to_3",variable),
      variable = ifelse(variable == "f_tea_bush_age_3to40","f_tea_bush_age_3_to_40",variable),
      variable = ifelse(variable == "f_sellingpoint_other","m_crops_livestock_seller_other",variable),
      variable = ifelse(variable == "l_payment_female","f_tea_plucking_wage_female",variable),
      variable = ifelse(variable == "l_payment_male","f_tea_plucking_wage_male",variable),
      variable = ifelse(variable == "l_payment_youth","f_tea_plucking_wage_youth",variable),
      variable = ifelse(variable == "su_farmer_organisation_x","su_farmer_organisation",variable),
      variable = ifelse(variable == "su_farmer_member_organisation","su_farmer_organisation_name",variable),
      variable = ifelse(variable == "su_factory_sell","f_factory_selling",variable),
      variable = ifelse(variable == "su_sdmcompany","cs_sdm_company",variable),
      variable = ifelse(variable == "f_breakfast_food","fs_breakfast_food",variable),
      variable = ifelse(variable == "n_food_midmorning","fs_food_midmorning",variable),
      variable = ifelse(variable == "n_foods_lunch","fs_foods_lunch",variable),
      variable = ifelse(variable == "n_food_midafternoon","fs_food_midafternoon",variable),
      variable = ifelse(variable == "n_foods_dinner","fs_foods_dinner",variable),
      variable = ifelse(variable == "n_foods_other","fs_foods_other",variable),
      variable = ifelse(variable == "m_finances_registered","hh_phone_financial_service",variable),
      variable = ifelse(variable == "ppi_building_main","ppi_tanz_building_main",variable),
      variable = ifelse(variable == "ppi_building_roof","ppi_tanz_building_roof",variable),
      variable = ifelse(variable == "ppi_fuel_main","ppi_tanz_fuel_main",variable),
      variable = ifelse(variable == "ppi_own_tv","ppi_tanz_own_tv",variable),
      variable = ifelse(variable == "ppi_own_radio","ppi_tanz_own_radio",variable),
      variable = ifelse(variable == "ppi_own_lanterns","ppi_tanz_own_lanterns",variable),
      variable = ifelse(variable == "ppi_own_tables","ppi_tanz_own_tables",variable),
      variable = ifelse(variable == "ppi_own_livestock","ppi_tanz_own_livestock",variable),
      variable = ifelse(variable == "hh_age_6to18_school","hh_nrmembers_u18_inschool",variable),
      variable = ifelse(variable == "hh_age_nrmember","hh_nrmembers_u18",variable),
      variable = ifelse(variable == "f_labour_compostprep_nrhiredpeople", "f_labour_infilling_nrhiredpeople", variable),
      variable = ifelse(variable == "f_labour_compostprep_rememberwage", "f_labour_infilling_rememberwage", variable),
      variable = ifelse(variable == "f_labour_sirrigation_nrmonths", "f_labour_irrigation_nrmonths", variable)
    )
  }
  
  
  ## ---- 8) EU TANZ TEA non Ikanga ----
  if(cases[i] == "2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx"){
    case <- case %>% dplyr::rename(
      focus_crop = sdm_crop,
      f_focus_crop_size_acre =  `f_focus_crop_size (acre)`,
      f_size_othermaincrop_1_acre =  `f_size_othermaincrop_1 (acre)`,
      f_size_othermaincrop_2_acre =  `f_size_othermaincrop_2 (acre)`,
      f_size_acre = `f_size (acre)`,
      # f_labour_infilling_nrpeople = f_labour_compostprep_nrpeople,
      fo_farming_investment_positive = fo_farming_investment,
      f_focus_quant_prod = f_tea_quant_prod,
      f_focus_quant_sold = f_tea_quant_sold,
      f_focus_measurement_sold = f_tea_measurement_sold,
      f_focus_price = f_tea_price,
      f_focus_quant_lost = f_tea_quant_lost,
      f_focus_measurement_lost = f_tea_measurement_lost,
      f_other_crops_type = f_othermaincrop_1_1,
      f_focus_measurement_prod = f_measurement_tea,
      f_focus_measurement_prod_other = f_measurement_other_tea,
      f_tea_bush_age_2_to_3 = f_tea_bush_age_2to3,
      f_tea_bush_age_3_to_40 = f_tea_bush_age_3to40,
      m_crops_livestock_seller_other = f_sellingpoint_other,
      f_tea_plucking_wage_female = l_payment_female,
      f_tea_plucking_wage_male = l_payment_male,
      f_tea_plucking_wage_youth = l_payment_youth,
      su_farmer_organisation = su_farmer_organisation_x,
      su_farmer_organisation_name = su_farmer_member_organisation,
      f_factory_selling = su_factory_sell,
      cs_sdm_company = su_sdmcompany,
      fs_breakfast_food = f_breakfast_food,
      fs_food_midmorning = n_food_midmorning,
      fs_foods_lunch = n_foods_lunch,
      fs_food_midafternoon = n_food_midafternoon,
      fs_foods_dinner = n_foods_dinner,
      fs_foods_other = n_foods_other,
      hh_phone_financial_service = m_finances_registered,
      ppi_tanz_building_main = ppi_building_main,
      ppi_tanz_building_roof = ppi_building_roof,
      ppi_tanz_fuel_main = ppi_fuel_main,
      ppi_tanz_own_tv = ppi_own_tv,
      ppi_tanz_own_radio = ppi_own_radio,
      ppi_tanz_own_lanterns = ppi_own_lanterns,
      ppi_tanz_own_tables = ppi_own_tables,
      ppi_tanz_own_livestock = ppi_own_livestock,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_tea_second_payment = cal_second_payment,
      hh_nrmembers_u18_inschool = hh_age_6to18_school,
      hh_nrmembers_u18 = hh_age_nrmember,
      cal_othermaincrop_1_inc_sold = f_othermaincrop_1_inc_sold,
      cal_othermaincrop_1_cost = cal_othermaincrop_cost) %>%
      select( -starts_with("cal_labour_agrochemicalapp"),
              -starts_with("cal_labour_fertilizerapp"),
              -starts_with("cal_labour_harvesting"),
              -starts_with("cal_labour_cropmaint"),
              -starts_with("cal_labour_landprep"),
              -starts_with("cal_labour_infilling"),
              -starts_with("cal_labour_postharvest"),
              -contains("_nrhoursdays"),
              -cal_quant_sold,
              -cal_othermaincrop_income)
    
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "sdm_crop","focus_crop",variable),
      variable = ifelse(variable == "f_labour_compostprep_nrpeople","f_labour_infilling_nrpeople",variable),
      variable = ifelse(variable == "fo_farming_investment","fo_farming_investment_positive",variable),
      variable = ifelse(variable == "f_tea_quant_prod","f_focus_quant_prod",variable),
      variable = ifelse(variable == "f_tea_quant_sold","f_focus_quant_sold",variable),
      variable = ifelse(variable == "f_tea_measurement_sold","f_focus_measurement_sold",variable),
      variable = ifelse(variable == "f_tea_price","f_focus_price",variable),
      variable = ifelse(variable == "f_tea_quant_lost","f_focus_quant_lost",variable),
      variable = ifelse(variable == "f_tea_measurement_lost","f_focus_measurement_lost",variable),
      variable = ifelse(variable == "f_othermaincrop_1_1","f_other_crops_type",variable),
      variable = ifelse(variable == "f_measurement_tea","f_focus_measurement_prod",variable),
      variable = ifelse(variable == "f_measurement_other_tea","f_focus_measurement_prod_other",variable),
      variable = ifelse(variable == "f_tea_bush_age_2to3","f_tea_bush_age_2_to_3",variable),
      variable = ifelse(variable == "f_tea_bush_age_3to40","f_tea_bush_age_3_to_40",variable),
      variable = ifelse(variable == "f_sellingpoint_other","m_crops_livestock_seller_other",variable),
      variable = ifelse(variable == "l_payment_female","f_tea_plucking_wage_female",variable),
      variable = ifelse(variable == "l_payment_male","f_tea_plucking_wage_male",variable),
      variable = ifelse(variable == "l_payment_youth","f_tea_plucking_wage_youth",variable),
      variable = ifelse(variable == "su_farmer_organisation_x","su_farmer_organisation",variable),
      variable = ifelse(variable == "su_farmer_member_organisation","su_farmer_organisation_name",variable),
      variable = ifelse(variable == "su_factory_sell","f_factory_selling",variable),
      variable = ifelse(variable == "su_sdmcompany","cs_sdm_company",variable),
      variable = ifelse(variable == "f_breakfast_food","fs_breakfast_food",variable),
      variable = ifelse(variable == "n_food_midmorning","fs_food_midmorning",variable),
      variable = ifelse(variable == "n_foods_lunch","fs_foods_lunch",variable),
      variable = ifelse(variable == "n_food_midafternoon","fs_food_midafternoon",variable),
      variable = ifelse(variable == "n_foods_dinner","fs_foods_dinner",variable),
      variable = ifelse(variable == "n_foods_other","fs_foods_other",variable),
      variable = ifelse(variable == "m_finances_registered","hh_phone_financial_service",variable),
      variable = ifelse(variable == "ppi_building_main","ppi_tanz_building_main",variable),
      variable = ifelse(variable == "ppi_building_roof","ppi_tanz_building_roof",variable),
      variable = ifelse(variable == "ppi_fuel_main","ppi_tanz_fuel_main",variable),
      variable = ifelse(variable == "ppi_own_tv","ppi_tanz_own_tv",variable),
      variable = ifelse(variable == "ppi_own_radio","ppi_tanz_own_radio",variable),
      variable = ifelse(variable == "ppi_own_lanterns","ppi_tanz_own_lanterns",variable),
      variable = ifelse(variable == "ppi_own_tables","ppi_tanz_own_tables",variable),
      variable = ifelse(variable == "ppi_own_livestock","ppi_tanz_own_livestock",variable),
      variable = ifelse(variable == "hh_age_6to18_school","hh_nrmembers_u18_inschool",variable),
      variable = ifelse(variable == "hh_age_nrmember","hh_nrmembers_u18",variable),
      variable = ifelse(variable == "f_labour_compostprep_nrhiredpeople", "f_labour_infilling_nrhiredpeople", variable),
      variable = ifelse(variable == "f_labour_compostprep_rememberwage", "f_labour_infilling_rememberwage", variable),
      variable = ifelse(variable == "f_labour_sirrigation_nrmonths", "f_labour_irrigation_nrmonths", variable)
    )
  }
  
  
  ## ---- 9) Smart logistics ----
  if(cases[i] == "2021-03-19_smart logistics_anom.xlsx"){
    case <- case %>% dplyr::rename(
      cal_hh_farmer_age = hh_farmer_age,
      focus_crop = sdm_crop,
      farmer_present = sdm_farmer,
      f_focus_crop_size_acre =  `f_focus_crop_size (acre)`,
      f_size_othermaincrop_1_acre =  `f_size_othermaincrop_1 (acre)`,
      f_size_othermaincrop_2_acre =  `f_size_othermaincrop_2 (acre)`,
      f_size_acre = `f_size (acre)`,
      f_livestock_income_chickens = f_livestcok_income_chickens,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_othermaincrop_1_cost = cal_othermaincrop_cost) %>%
      select( -starts_with("cal_labour_nurserymaint"),
              -starts_with("cal_labour_fertilizerapp"),
              -starts_with("cal_labour_harvesting"),
              -starts_with("cal_labour_cropmaint"),
              -starts_with("cal_labour_landprep"),
              -starts_with("cal_labour_irrigation"),
              -starts_with("cal_labour_postharvest"),
              -cal_othermaincrop_income)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "hh_farmer_age", "cal_hh_farmer_age", variable),
      variable = ifelse(variable == "sdm_crop","focus_crop",variable),
      variable = ifelse(variable == "sdm_farmer","farmer_present",variable),
      variable = ifelse(variable == "f_livestcok_income_chickens","f_livestock_income_chickens",variable))
  }

  ## ---- 10) Rwanda AIF Maize----
  
  if(cases[i] == "2021-05-14 AIF Anom nyagatare and kirehe.xlsx"){
    case <- case %>% dplyr::rename(
      repeat_no = `repeat no.x`,
      focus_crop = sdm_crop,
      farmer_present = sdm_farmer,
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
      fs_introduction_other = 'fs_introduction--other--',
      hh_loan_purpose_other = 'hh_loan_purpose--other--',
      hh_loan_source_other = 'hh_loan_source--other--',
      hh_loan = hh_loan_presence,
      f_labour_landprep_paymentpertimeframe = hire_people_pay_perday,
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
      cs_negative_recommendation = whynot_iaf) %>%
      select(-labor_cost, -`repeat no.y`)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "grow","focus_crop",variable),
      variable = ifelse(variable == "avg_days_livestock","f_labour_livestock_nrdays",variable),
      variable = ifelse(variable == "cl_coping_mechanisms--other--","cl_coping_mechanisms_other",variable),
      variable = ifelse(variable == "cost_market_crop","f_labour_marketing_wage_per_kg",variable),
      variable = ifelse(variable == "cs_aif","cs_sdm_company",variable),
      variable = ifelse(variable == "exp_equipment","f_equipment_cost",variable),
      variable = ifelse(variable == "f_crop_lost","f_focus_quant_lost",variable),
      variable = ifelse(variable == "f_crop_lost_measure","f_focus_measurement_lost",variable),
      variable = ifelse(variable == "f_crop_poultry","f_quant_lost_poultry_feed", variable),
      variable = ifelse(variable == "f_focus_crop_sizeHA","f_focus_crop_size_hectare",variable),
      variable = ifelse(variable == "f_focus_measurement_other","f_focus_measurement_prod_other_kg",variable),
      variable = ifelse(variable == "f_focus_quant_measure","f_focus_own_consumption_measurement",variable),
      variable = ifelse(variable == "f_focus_quant_measure_lost","f_focus_measurement_lost_2",variable),
      variable = ifelse(variable == "f_focus_sell_season","f_focus_price",variable),
      variable = ifelse(variable == "f_harvest_number_of_times","f_harvest_num",variable),
      variable = ifelse(variable == "f_inc_add_livestock_poultry","f_livestock_income_total",variable),
      variable = ifelse(variable == "f_inc_day_ppl_livestock_poultry","f_livestock_days_hiredlabour",variable),
      variable = ifelse(variable == "f_inc_nrppl_hire_livestock_poultry","f_livestock_nr_hired_labourers",variable),
      variable = ifelse(variable == "f_inc_pay_ppl_livestock_poultry","f_livestock_wages_hiredlabour",variable),
      variable = ifelse(variable == "f_income_hireppl_livestock_poultry","f_livestock_hiredlabour_yn",variable),
      variable = ifelse(variable == "f_income_livestock_poultry","f_livestock_income_type",variable),
      variable = ifelse(variable == "f_income_other_sources","f_income_other_total",variable),
      variable = ifelse(variable == "f_income_other_type--other--","f_income_other_type_other",variable),
      variable = ifelse(variable == "f_inputs_challenges_types--other--","f_inputs_challenges_types_other",variable),
      variable = ifelse(variable == "f_inputs_costs_fungicides","f_inputs_costs_chemicals_3",variable),
      variable = ifelse(variable == "f_inputs_costs_pesticides","f_inputs_costs_chemcials_4",variable),
      variable = ifelse(variable == "f_inputs_usage_medicine","f_livestock_costs_medics",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_agrochem","f_labour_agrochemicalapp_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_cropmaintenance","f_labour_cropmaint_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_harvesting","f_labour_harvesting_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_irrigaion","f_labour_irrigation_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_marketing","f_labour_marketing_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_planting","f_labour_planting_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_postharvesting","f_labour_postharvesting_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrhire_spraying","f_labour_spraying_nrhiredpeople",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_agrochem","f_labour_agrochemicalapp_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_cropmaintenance","f_labour_cropmaint_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_harvesting","f_labour_harvesting_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_irrigation","f_labour_irrigation_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_marketing","f_labour_marketing_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_planting","f_labour_planting_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_postharvesting","f_labour_postharvesting_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_nrpeople_spraying","f_labour_spraying_nrdays",variable),
      variable = ifelse(variable == "f_labour_farm_permanent_day_agrochem","f_labour_agrochemicalapp_nrpeople",variable),
      variable = ifelse(variable == "f_labour_farm_permanent_day_cropmaintenance","f_labour_cropmaint_nrpeople",variable),
      variable = ifelse(variable == "f_labour_farm_permanent_day_harvesting","f_labour_harvesting_nrpeople",variable),
      variable = ifelse(variable == "f_labour_farm_permanent_day_irrigation","f_labour_irrigation_nrpeople",variable),
      variable = ifelse(variable == "f_labour_farm_permanent_day_planting","f_labour_planting_nrpeople",variable),
      variable = ifelse(variable == "f_labour_farm_permanent_day_postharvesting","f_labour_postharvesting_nrpeople",variable),
      variable = ifelse(variable == "f_labour_farm_permanent_day_spraying","f_labour_spraying_nrpeople",variable),
      variable = ifelse(variable == "f_land_own","f_ownership_type",variable),
      variable = ifelse(variable == "f_lost (kilograms)","f_focus_quant_lost_kg",variable),
      variable = ifelse(variable == "f_othermaincrop_spend","f_other_crop_income",variable),
      variable = ifelse(variable == "f_own_consumption (kilograms)","f_focus_own_consumption_kg",variable),
      variable = ifelse(variable == "f_produced (kilograms)","f_focus_quant_prod_kg",variable),
      variable = ifelse(variable == "f_size_Ha","f_size_hectare",variable),
      variable = ifelse(variable == "f_sold (kilograms)","f_focus_quant_sold_kg",variable),
      variable = ifelse(variable == "f_unit_land_no","f_unit_land",variable),
      variable = ifelse(variable == "f_unit_land_no--other--","f_unit_land_other",variable),
      variable = ifelse(variable == "farm_incharge_present","farmer_present",variable),
      variable = ifelse(variable == "fs_introduction--other--","fs_introduction_other",variable),
      variable = ifelse(variable == "hh_loan_purpose--other--","hh_loan_purpose_other",variable),
      variable = ifelse(variable == "hh_loan_source--other--","hh_loan_source_other",variable),
      variable = ifelse(variable == "hire_people_pay_perday","f_labour_landprep_paymentpertimeframe",variable),
      variable = ifelse(variable == "hire_people_pay_perday_agrochem","f_labour_agrochemicalapp_paymentpertimeframe",variable),
      variable = ifelse(variable == "hire_people_pay_perday_cropmaintenance","f_labour_cropmaint_paymentpertimeframe",variable),
      variable = ifelse(variable == "hire_people_pay_perday_harevsting","f_labour_harvesting_paymentpertimeframe",variable),
      variable = ifelse(variable == "hire_people_pay_perday_irrigation","f_labour_irrigation_paymentpertimeframe",variable),
      variable = ifelse(variable == "hire_people_pay_perday_planting","f_labour_planting_paymentpertimeframe",variable),
      variable = ifelse(variable == "hire_people_pay_perday_postharevsting","f_labour_postharvesting_paymentpertimeframe",variable),
      variable = ifelse(variable == "hire_people_pay_perday_spraying","f_labour_spraying_paymentpertimeframe",variable),
      variable = ifelse(variable == "iaf_pay","cs_timely_payment",variable),
      variable = ifelse(variable == "iaf_recommend","cs_recommendation",variable),
      variable = ifelse(variable == "kg_marketedcrop_hiredlabour","f_labour_harvesting_kg_per_person",variable),
      variable = ifelse(variable == "livestock_pplnr","f_livestock_nr_labourers",variable),
      variable = ifelse(variable == "loan_inputs","hh_loan_source_inputs",variable),
      variable = ifelse(variable == "maincrop","f_maincrop",variable),
      variable = ifelse(variable == "maincrop_other","f_maincrop_other",variable),
      variable = ifelse(variable == "other_inputs","f_inputs_usage_other_2",variable),
      variable = ifelse(variable == "other_tool","f_equip_type_other",variable),
      variable = ifelse(variable == "pay_perday_livestock","f_livestock_wages_hiredlabour_2",variable),
      variable = ifelse(variable == "pplnr_livestock_hire","f_livestock_nr_hired_labourers_2",variable),
      variable = ifelse(variable == "rent_harvester","f_equip_harvesting_ownership_type",variable),
      variable = ifelse(variable == "rent_irrigation","f_equip_irrigation_ownership_type",variable),
      variable = ifelse(variable == "rent_other","f_equip_other_ownership_type",variable),
      variable = ifelse(variable == "rent_rot","f_equip_tillers_ownership_type",variable),
      variable = ifelse(variable == "rent_weeding","f_equip_weeding_ownership_type",variable),
      variable = ifelse(variable == "res_age","hh_farmer_birthyear",variable),
      variable = ifelse(variable == "res_gender","hh_farmer_gender",variable),
      variable = ifelse(variable == "second_maincrop","f_othermaincrop_1",variable),
      variable = ifelse(variable == "second_maincrop_other","f_othermaincrop_1_other",variable),
      variable = ifelse(variable == "services_aif_usage","cs_sdm_company_services",variable),
      variable = ifelse(variable == "shortage_money_months","cf_shortage_months",variable),
      variable = ifelse(variable == "su_farmer_organisation_x","su_farmer_organisation",variable),
      variable = ifelse(variable == "third_maincrop","f_othermaincrop_2",variable),
      variable = ifelse(variable == "third_maincrop_other","f_othermaincrop_2_other",variable),
      variable = ifelse(variable == "why_iaf","cs_positive_recommendation",variable),
      variable = ifelse(variable == "whynot_iaf","cs_negative_recommendation",variable))
  }
  
  ## ---- 11) Kenya syngenta tomatoes----
  if(cases[i] == "kenya-syngenta_tomatoes.xlsx"){
    case <- case %>% dplyr::rename(
      focus_crop = sdm_crop,
      f_size_acre = `f_size (acre)`,
      f_focus_crop_size_acre = `f_focus_crop_size (acre)`,
      f_size_othermaincrop_1_acre =  `f_size_othermaincrop_1 (acre)`,
      f_size_othermaincrop_2_acre =  `f_size_othermaincrop_2 (acre)`,
      ##Location
      pi_location_other_first_admin = pi_location_cascade_county,
      pi_location_other_second_admin = pi_location_cascade_sub_county,
      f_tomatoes_type = f_focus_variety,
      f_livestock_income_chickens = f_livestcok_income_chickens,
      ##There are 2 variables somehow, have the same meaning
      f_inputs_source_var2 = f_inputs_source_1,
      f_inputs_challenges_var2 = f_inputs_challenges_1,
      f_inputs_challenges_types_var2 = f_inputs_challenges_types_1,
      ##dplyr::rename focuscrop inside variables to let people know it applies to tomatoes
      f_tomatoes_use_seeds_yn = f_focuscrop_use_seeds,
      f_tomatoes_quant_keep_seeds = f_focuscrop_keep_seeds,
      f_tomatoes_quant_plant = f_focuscrop_kept_planted,
      f_tomatoes_quant_use_seeds_selling = f_focuscrop_sold_seeds,
      f_tomatoes_price_selling_seeds = f_cost_seeds_sold,
      f_tomatoes_measurement_use_seeds = f_focus_use_seeds_measurement,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_othermaincrop_1_inc_sold = f_othermaincrop_1_inc_sold,
      cal_othermaincrop_1_cost = cal_othermaincrop_cost) %>%
        select(-contains("cal_labour_cropmaint"),
               -contains("cal_labour_fertilizerapp"),
               -contains("cal_labour_harvesting"),
               -contains("cal_labour_landprep"),
               -contains("cal_labour_irrigation"),
               -contains("cal_labour_nurserymaint"),
               -contains("cal_labour_postharvest"),
               -cal_othermaincrop_income)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "sdm_crop","focus_crop",variable),
      
      variable = ifelse(variable == "pi_location_cascade_county","pi_location_other_first_admin",variable),
      variable = ifelse(variable == "pi_location_cascade_sub_county","pi_location_other_second_admin",variable),
      variable = ifelse(variable == "f_focus_variety","f_tomatoes_type",variable),
      variable = ifelse(variable == "f_livestcok_income_chickens","f_livestock_income_chickens",variable),
      
      variable = ifelse(variable == "f_inputs_source_1","f_inputs_source_var2",variable),
      variable = ifelse(variable == "f_inputs_challenges_1","f_inputs_challenges_var2",variable),
      variable = ifelse(variable == "f_inputs_challenges_types_1","f_inputs_challenges_types_var2",variable),

      variable = ifelse(variable == "f_focuscrop_use_seeds","f_tomatoes_use_seeds_yn",variable),
      variable = ifelse(variable == "f_focuscrop_keep_seeds","f_tomatoes_quant_keep_seeds",variable),
      variable = ifelse(variable == "f_focuscrop_kept_planted","f_tomatoes_quant_plant",variable),
      variable = ifelse(variable == "f_focuscrop_sold_seeds","f_tomatoes_quant_use_seeds_selling",variable),
      variable = ifelse(variable == "f_cost_seeds_sold","f_tomatoes_price_selling_seeds",variable),
      variable = ifelse(variable == "f_focus_use_seeds_measurement","f_tomatoes_measurement_use_seeds",variable))
    

  }
  
  ## ---- 12) Kenya syngenta potatoes----
  if(cases[i] == "kenya-syngenta_potatoes.xlsx"){
    case <- case %>% dplyr::rename(
      focus_crop = sdm_crop,
      f_size_acre = `f_size (acre)`,
      f_focus_crop_size_acre = `f_focus_crop_size (acre)`,
      f_size_othermaincrop_1_acre =  `f_size_othermaincrop_1 (acre)`,
      f_size_othermaincrop_2_acre =  `f_size_othermaincrop_2 (acre)`,
      ##Location
      pi_location_other_first_admin = pi_location_cascade_county,
      pi_location_other_second_admin = pi_location_cascade_sub_county,
      f_potatoes_type = f_focus_variety,
      f_livestock_income_chickens = f_livestcok_income_chickens,
      ##There are 2 variables somehow, have the same meaning
      f_inputs_source_var2 = f_inputs_source_1,
      f_inputs_challenges_var2 = f_inputs_challenges_1,
      f_inputs_challenges_types_var2 = f_inputs_challenges_types_1,
      ##Rename focuscrop inside variables to let people know it applies to potatoes
      f_potatoes_use_seeds_yn = f_focuscrop_use_seeds,
      f_potatoes_quant_keep_seeds = f_focuscrop_keep_seeds,
      f_potatoes_quant_plant = f_focuscrop_kept_planted,
      f_potatoes_quant_use_seeds_selling = f_focuscrop_sold_seeds,
      f_potatoes_price_selling_seeds = f_cost_seeds_sold,
      f_potatoes_measurement_use_seeds = f_focus_use_seeds_measurement,
      cal_farm_costs_general = cal_farm_general_cost,
      cal_othermaincrop_1_inc_sold = f_othermaincrop_1_inc_sold,
      cal_othermaincrop_1_cost = cal_othermaincrop_cost) %>%
      select(-cal_othermaincrop_income)
    
    codebook <- codebook %>% mutate(
      variable = ifelse(variable == "sdm_crop","focus_crop",variable),
      
      variable = ifelse(variable == "pi_location_cascade_county","pi_location_other_first_admin",variable),
      variable = ifelse(variable == "pi_location_cascade_sub_county","pi_location_other_second_admin",variable),
      variable = ifelse(variable == "f_focus_variety","f_potatoes_type",variable),
      variable = ifelse(variable == "f_livestcok_income_chickens","f_livestock_income_chickens",variable),
      
      variable = ifelse(variable == "f_inputs_source_1","f_inputs_source_var2",variable),
      variable = ifelse(variable == "f_inputs_challenges_1","f_inputs_challenges_var2",variable),
      variable = ifelse(variable == "f_inputs_challenges_types_1","f_inputs_challenges_types_var2",variable),
      
      variable = ifelse(variable == "f_focuscrop_use_seeds","f_potatoes_use_seeds_yn",variable),
      variable = ifelse(variable == "f_focuscrop_keep_seeds","f_potatoes_quant_keep_seeds",variable),
      variable = ifelse(variable == "f_focuscrop_kept_planted","f_potatoes_quant_plant",variable),
      variable = ifelse(variable == "f_focuscrop_sold_seeds","f_potatoes_quant_use_seeds_selling",variable),
      variable = ifelse(variable == "f_cost_seeds_sold","f_potatoes_price_selling_seeds",variable),
      variable = ifelse(variable == "f_focus_use_seeds_measurement","f_potatoes_measurement_use_seeds",variable))
  }
  
  ## ---- 13) RGL beans----
  if(cases[i] == "2021_RGL_anom_beans.xlsx"){
    case <- case %>%
      select(-contains("cost_timeperiod"), -cal_othermaincrop_income) %>%
      rename(cal_othermaincrop_1_cost = cal_othermaincrop_cost_1,
             cal_othermaincrop_2_cost = cal_othermaincrop_cost_2,
             f_unit_land_other = f_unit_land_no_other2,
             f_focus_measurement_prod_other = f_focus_measurement_other_other2,
             f_focus_lost_measurement_other = f_focus_own_lost_measurement_other2)
    
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
  
  ## ---- 14) RGL rice----
  if(cases[i] == "2021_RGL_anom_rice.xlsx"){
    case <- case %>%
      select(-contains("cost_timeperiod"), -cal_othermaincrop_income) %>%
      rename(cal_othermaincrop_1_cost = cal_othermaincrop_cost_1,
             cal_othermaincrop_2_cost = cal_othermaincrop_cost_2,
             f_unit_land_other = f_unit_land_no_other2,
             f_rice_type_other = f_beans_type_other2,
             f_focus_measurement_prod_other = f_focus_measurement_other_other2,
             f_focus_lost_measurement_other = f_focus_own_lost_measurement_other2)
    
    codebook <- codebook %>%
      mutate(variable = ifelse(variable == " f_focus_measurement_other","f_focus_measurement_prod",variable),
             variable = ifelse(variable == "sdm_farmer","farmer_present",variable),
             variable = ifelse(variable == "sdm_crop","focus_crop",variable),
             variable = ifelse(variable == "f_unit_land_no","f_unit_land",variable),
             variable = ifelse(variable == "f_focus_own_lost_measurement","f_focus_lost_measurement",variable),
             variable = ifelse(variable == "m_crops_livestock_seller","m_crops_livestock_seller",variable),
             variable = ifelse(variable == "f_labour_andprep_rememberwage","f_labour_landprep_rememberwage",variable),
             variable = ifelse(variable == "f_inputs_usage","f_inputs_usage_types",variable),
             variable = ifelse(variable == "fo_urban_water","fo_water",variable),
             variable = ifelse(variable == "fo_business","fo_business_nonagri",variable),
             variable = ifelse(variable == "f_labour_sirrigation_nrmonths", "f_labour_irrigation_nrmonths",variable),
             variable = ifelse(variable == "f_beans_type", "f_rice_type",variable))
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
  
  case <- case %>%
    mutate_at(vars(starts_with("hh_") & contains("_birthyear")), as.numeric) %>%
    mutate_at(vars(starts_with("hh_") & contains("_age")), as.numeric) %>%
    mutate_at(vars(starts_with("hh_") & contains("_birthyear")), 
              function(x) ifelse(x < 120, 2021 - x, x)) %>%
    mutate_at(vars(starts_with("hh_") & contains("_age")), 
              function(x) ifelse(x < 120, 2021 - x, x)) %>% 
    rename_at(vars(starts_with("hh_") & contains("_age")), 
              funs(gsub("_age", "_birthyear", ., perl=T))) #%>%
  
  
  
  
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
  
  write.xlsx(output, here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output", cases[i]) ,
             overwrite = TRUE)
  
}

names(codebooks)[5]<-"NKG Honduras"
names(codebooks)[7]<-"EU Tanzania Ikanga"
names(codebooks)[8]<-"EU Tanzania non Ikanga"
names(codebooks)[9]<-"Smart logistics"
names(codebooks)[10]<-"AIF Rwanda"

names(no_matches)[5]<-"NKG Honduras"
names(no_matches)[7]<-"EU Tanzania Ikanga"
names(no_matches)[8]<-"EU Tanzania non Ikanga"
names(no_matches)[9]<-"Smart logistics"
names(no_matches)[10]<-"AIF Rwanda"

# write the lists for manual checks - differing questions:
write.xlsx(codebooks, file = here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/", 
                                  "Mismatch in questions_part_1.xlsx"), 
           sep=",", 
           row.names = FALSE,
           overwrite = TRUE)

# write the lists for manual checks - no match at all:
write.xlsx(no_matches, file = here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/", 
                                   "Mismatch in question and varname_part_1.xlsx"), 
           sep=",", 
           row.names = FALSE,
           overwrite = TRUE)

