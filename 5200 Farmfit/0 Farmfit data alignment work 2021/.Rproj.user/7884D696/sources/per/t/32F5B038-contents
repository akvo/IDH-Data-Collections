# Script: Data alignment - prep for upload
# Date: December 2021
# Contact: jildemarie@akvo.org


# Libraries 

library(readxl)
library(openxlsx)
library(here)
library(tidyverse)
library(tidylog)
library(data.table)

library(tidyr)
library(tidyselect)
library(stringr)
library(reshape2)
library(zoo)
library(splitstackshape)
library(varhandle)
library(purrr)


  
  
## ---  1) alluvial ----

  case <- read_excel("Output_step 2/2021-06-29_Alluvial_anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-06-29_Alluvial_anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/alluvial_21.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable),
           variable = ifelse(variable == "sdm_farmer", "farmer_present", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer) %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_local_government_, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_local_government_) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) 
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-06-29_Alluvial_anom.xlsx"),
             overwrite = TRUE)
  
  
## --- 2) Coscharis ----
#"2021-06-29_Coscharis_anom.xlsx"
  case <- read_excel("Output_step 2/2021-06-29_Coscharis_anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-06-29_Coscharis_anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/coscharis_21.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable),
           variable = ifelse(variable == "sdm_farmer", "farmer_present", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))%>%
    rename(farmer_present = sdm_farmer) %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_local_government, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_local_government) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) 
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-06-29_Coscharis_anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 3) NKG Kenya ----
  #"2021-04-07_Kenya NKG Anom.xlsx"
  case <- read_excel("Output_step 2/2021-04-07_Kenya NKG Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-04-07_Kenya NKG Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/nkg_kenya.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_county_, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_county_) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) 
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-04-07_Kenya NKG Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 4) NKG Uganda ----
  #"2021-04-12_Uganda NKG Anom.xlsx"
  case <- read_excel("Output_step 2/2021-04-12_Uganda NKG Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-04-12_Uganda NKG Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/nkg_uganda.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_district, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_district) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) 
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-04-12_Uganda NKG Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 5) NKG Honduras ----
  #"2021-04-12_Honduras NKG Anom.xlsx"
  case <- read_excel("Output_step 2/2021-04-12_Honduras NKG Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-04-12_Honduras NKG Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/nkg_honduras.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
   mutate(pi_location_cascade_first_level = pi_location_cascade_departamento)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    filter(`repeat no`==1) %>%
    select(identifier, `submission date`) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) 
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-04-12_Honduras NKG Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 6) NKG Mexico ----
  #"2021-04-15_Mexico NKG Anom.xlsx"
  case <- read_excel("Output_step 2/2021-04-15_Mexico NKG Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-04-15_Mexico NKG Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/nkg_mexico.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(pi_location_cascade_first_level = pi_location_cascade_estado)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    filter(`repeat no`==1) %>%
    select(identifier, `submission date`) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) 
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-04-15_Mexico NKG Anom.xlsx"),
             overwrite = TRUE) 

  ## --- 7) EU Tanzania Ikanga ----
  #"2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx"
  case <- read_excel("Output_step 2/2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/eu tanzania.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable)) 
  
  case <- case %>%
    rename(farmer_present = sdm_farmer)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_region, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_region) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)  
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-05-26_EU Tea Tanzania_anom_Njombe_Ikanga_farmers.xlsx"),
             overwrite = TRUE) 
  
  ## --- 8) EU Tanzania non Ikanga ----
  #"2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx"
  case <- read_excel("Output_step 2/2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/eu tanzania.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable)) 
  
  case <- case %>%
    rename(farmer_present = sdm_farmer)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_region, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_region) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-06-02_EU Tea Tanzania_anom_Njombe_nonIkanga_farmers.xlsx"),
             overwrite = TRUE) 
  
  ## --- 9) Smart logistics ----
  #"2021-03-19_smart logistics_anom.xlsx"
  case <- read_excel("Output_step 2/2021-03-19_smart logistics_anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-03-19_smart logistics_anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/smart_logistics.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)  
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable),
           variable = ifelse(variable == "sdm_farmer", "farmer_present", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))%>%
    rename(farmer_present = sdm_farmer) %>%
    select(-`submission date`)
  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_1_county, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_1_county) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-03-19_smart logistics_anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 9) AIF Rwanda  ----
  #"2021-05-14 AIF Anom nyagatare and kirehe.xlsx"
  case <- read_excel("Output_step 2/2021-05-14 AIF Anom nyagatare and kirehe.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021-05-14 AIF Anom nyagatare and kirehe.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/aif_rwanda.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_district, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_district) %>%
    filter(!is.na(pi_location_cascade_first_level)) #%>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021-05-14 AIF Anom nyagatare and kirehe.xlsx"),
             overwrite = TRUE) 
  
  ## --- 10) Kenya syngenta potatoes ----
  #"kenya-syngenta_potatoes.xlsx"
  case <- read_excel("Output_step 2/kenya-syngenta_potatoes.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/kenya-syngenta_potatoes.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/syngenta_potatoes.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable),
           variable = ifelse(variable == "sdm_farmer", "farmer_present", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))%>%
    rename(farmer_present = sdm_farmer)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_1_sub_county, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_1_sub_county) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/kenya-syngenta_potatoes.xlsx"),
             overwrite = TRUE) 
  
  ## --- 11) Kenya syngenta tomatoes ----
  #"kenya-syngenta_tomatoes.xlsx"
  case <- read_excel("Output_step 2/kenya-syngenta_tomatoes.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/kenya-syngenta_tomatoes.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/syngenta_tomatoes.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable),
           variable = ifelse(variable == "sdm_farmer", "farmer_present", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer) %>%
    mutate(pi_location_cascade_first_level = pi_location_other_first_admin)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    filter(`repeat no` == 1) %>%
    select(identifier,  `submission date`) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/kenya-syngenta_tomatoes.xlsx"),
             overwrite = TRUE) 
  
  ## --- 12) RGL Anom beans  ----
  #"2021_RGL_anom_beans.xlsx"
  case <- read_excel("Output_step 2/2021_RGL_anom_beans.xlsx",
                     sheet = "Cleaned Data")

  codebook <- read_excel("Output_step 2/2021_RGL_anom_beans.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/rgl_beans.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable))  
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    mutate(f_size_acre = ifelse(f_unit_land == "hectares", f_size*2.471, f_size)) %>%
    mutate(f_focus_crop_size_acre = ifelse(f_unit_land == "hectares", f_focus_crop_size*2.471, f_focus_crop_size)) %>%
    mutate(f_size_othermaincrop_1_acre = ifelse(f_unit_land == "hectares", 
                                                f_size_othermaincrop_1*2.471, 
                                                f_size_othermaincrop_1)) %>%
    mutate(f_size_othermaincrop_2_acre = ifelse(f_unit_land == "hectares", 
                                                f_size_othermaincrop_2*2.471, 
                                                f_size_othermaincrop_2))  %>%
    rename(`submission date`= submission.date)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_1_region,`submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_1_region) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021_RGL_anom_beans.xlsx"),
             overwrite = TRUE) 
  
  ## --- 13) RGL Anom rice ----
  #"2021_RGL_anom_rice.xlsx"
  case <- read_excel("Output_step 2/2021_RGL_anom_rice.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021_RGL_anom_rice.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/rgl_rice.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  codebook <- codebook %>%
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable)) 
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    mutate(f_size_acre = ifelse(f_unit_land == "hectares", f_size*2.471, f_size)) %>%
    mutate(f_focus_crop_size_acre = ifelse(f_unit_land == "hectares", f_focus_crop_size*2.471, f_focus_crop_size)) %>%
    mutate(f_size_othermaincrop_1_acre = ifelse(f_unit_land == "hectares", 
                                                f_size_othermaincrop_1*2.471, 
                                                f_size_othermaincrop_1)) %>%
    mutate(f_size_othermaincrop_2_acre = ifelse(f_unit_land == "hectares", 
                                                f_size_othermaincrop_2*2.471, 
                                                f_size_othermaincrop_2))  %>%
    rename(`submission date`= submission.date)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_1_mkoa,`submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_1_mkoa) %>%
    filter(!is.na(pi_location_cascade_first_level)) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021_RGL_anom_rice.xlsx"),
             overwrite = TRUE) 
  
  ## --- 14) Sparx  ----
  #"12122019 Sparkx Anom.xlsx"
  case <- read_excel("Output_step 2/12122019 Sparkx Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/12122019 Sparkx Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/sparkx.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
   case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    mutate(f_size_acre = ifelse(f_unit_land == "hectares", f_size*2.471, f_size)) %>%
    mutate(pi_location_cascade_first_level = pi_location_other_first_admin)  %>%
     select(-`submission date`)
  
   names(raw) <- tolower (names(raw)) 
   raw <- raw %>%
     select(identifier, `submission date`) %>%
     filter(!is.na(`submission date`)) #%>%
     #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE) 
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/12122019 Sparkx Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 15) Usomi ----
  #"12122019_USOMI.xlsx"
  case <- read_excel("Output_step 2/12122019_USOMI.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/12122019_USOMI.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/usomi.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    mutate(pi_location_cascade_first_level = pi_location_second_admin)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, `submission date`) %>%
    filter(!is.na(`submission date`)) #%>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE) 
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/12122019_USOMI.xlsx"),
             overwrite = TRUE) 
  
  ## --- 16) Batian  ----
  #"20082020 Batian Anom.xlsx"
  
  case <- read_excel("Output_step 2/20082020 Batian Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/20082020 Batian Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/batian nuts.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer,
           focus_crop = sdm_crop)  %>%
    select(-`submission date`)
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "sdm_crop", "focus_crop", variable),
           variable = ifelse(variable == "sdm_farmer", "farmer_present", variable))  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_sub_county, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_sub_county) %>%
    filter(!is.na(pi_location_cascade_first_level))# %>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/20082020 Batian Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 17) Mwea  ----
  #"11082020 Mwea Anom.xlsx",
  
  case <- read_excel("Output_step 2/11082020 Mwea Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/11082020 Mwea Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/mwea_rice.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer,
           focus_crop_type = focus_crop,
           focus_crop = sdm_crop) 
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "focus crop", "focus_crop_type", variable),
           variable = ifelse(variable == "sdm_crop", "focus_crop", variable),
           variable = ifelse(variable == "sdm_farmer", "farmer_present", variable))  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_county, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_county) #%>%
    #filter(!is.na(pi_location_cascade_first_level)) %>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/11082020 Mwea Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 18) Musoni maize  ----
  #"26032020 Musoni Maize.xlsx",
  
  case <- read_excel("Output_step 2/26032020 Musoni Maize.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/26032020 Musoni Maize.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/musoni.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer,
           f_size_acre = `f_size (acre)`) %>%
    select(-`submission date`)
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable),
           variable = ifelse(variable == "f_size (acre)", "f_size_acre", variable),
           variable = ifelse(variable == "hh_age_farmer", "hh_farmer_birthyear", variable))  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_county, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_county) #%>%
    #filter(!is.na(pi_location_cascade_first_level)) %>%
   # mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/26032020 Musoni Maize.xlsx"),
             overwrite = TRUE) 
  
  ## --- 19) Musoni sorghum  ----
  #"26032020 Musoni Sorghum.xlsx",
  
  case <- read_excel("Output_step 2/26032020 Musoni Sorghum.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/26032020 Musoni Sorghum.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/musoni.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer,
           f_size_acre = `f_size (acre)`) %>%
    select(-`submission date`)
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable),
           variable = ifelse(variable == "f_size (acre)", "f_size_acre", variable),
           variable = ifelse(variable == "hh_age_farmer", "hh_farmer_birthyear", variable))  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_county, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_county) #%>%
    #filter(!is.na(pi_location_cascade_first_level)) %>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  
  write.xlsx(output, here::here("Output_step 3/26032020 Musoni Sorghum.xlsx"),
             overwrite = TRUE) 
  
  ## --- 20) Rubutco  ----
  #"2020-11-20 Rubutco Anom.xlsx",
  
  case <- read_excel("Output_step 2/2020-11-20 Rubutco Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2020-11-20 Rubutco Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/rubutco.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade_wards,`submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade_wards) %>%
    filter(!is.na(pi_location_cascade_first_level)) #%>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2020-11-20 Rubutco Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 21) Egranary  ----
  #"04022020 Egranary Anom.xlsx",
  case <- read_excel("Output_step 2/04022020 Egranary Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/04022020 Egranary Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/egranary.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer,
           f_size_acre = `f_size (acre)`)  %>%
    select(-`submission date`)
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable),
           variable = ifelse(variable == "f_size (acre)", "f_size_acre", variable))
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, pi_location_cascade, `submission date`) %>%
    rename(pi_location_cascade_first_level = pi_location_cascade) %>%
    filter(!is.na(pi_location_cascade_first_level)) #%>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/04022020 Egranary Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 22) Tanzania ----
  #"2021_tanzania-ussl.xlsx",
  case <- read_excel("Output_step 2/2021_tanzania-ussl.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/2021_tanzania-ussl.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/ussl_tanzania.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    mutate(pi_location_cascade_first_level = pi_location_cascade_region)  %>%
    select(-`submission date`)
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "hh_farmer_age", "hh_farmer_birthyear", variable)) 
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    filter( `repeat no` == 1) %>%
    select(identifier,  `submission date`) %>%
    mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/2021_tanzania-ussl.xlsx"),
             overwrite = TRUE)  
  
  ## --- 23) agri_wallet  ----
  #"04102019_AGRI_WALLET_anonymized.xlsx",
  case <- read_excel("Output_step 2/04102019_AGRI_WALLET_anonymized.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/04102019_AGRI_WALLET_anonymized.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/agriwallet.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))  %>%
    mutate(pi_location_cascade_first_level = pi_location_datacollection)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    filter( `repeat no` == 1) %>%
    select(identifier,  `submission date`) #%>%
   # mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/04102019_AGRI_WALLET_anonymized.xlsx"),
             overwrite = TRUE) 
  
  ## --- 24) alluvial  ----
  #"12122019_Alluvial.xlsx",
  case <- read_excel("Output_step 2/12122019_Alluvial.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/12122019_Alluvial.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/alluvial_19.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    mutate(pi_location_cascade_first_level = pi_location_other_first_admin)  %>%
    select(-`submission date`)
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier,  `submission date`) # %>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/12122019_Alluvial.xlsx"),
             overwrite = TRUE) 
  
  ## --- 25) coscharis  ----
  #"11122019 Coscharis Anom.xlsx",
  case <- read_excel("Output_step 2/11122019 Coscharis Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/11122019 Coscharis Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/coscharis_19.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier,  `submission date`) #%>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
 
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/11122019 Coscharis Anom.xlsx"),
             overwrite = TRUE) 
  
  ## --- 26) Mc cornick  ----
  #"30012020 McCormick Anom - new format.xlsx",
  case <- read_excel("Output_step 2/30012020 McCormick Anom - new format.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/30012020 McCormick Anom - new format.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/mccornick.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA))  %>%
    mutate(pi_location_cascade_first_level = pi_location_other_first_admin)  %>%
    select(-`submission date`)
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable))  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier,  `submission date`) # %>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/30012020 McCormick Anom - new format.xlsx"),
             overwrite = TRUE)   
  
  ## --- 27) NFC  ----
  #"17102019_NFC.xlsx",
  case <- read_excel("Output_step 2/17102019_NFC.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/17102019_NFC.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/new forest.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA),
           pi_location_cascade_first_level = pi_location_other_first_admin) %>%
    rename(farmer_present = sdm_farmer)  %>%
    select(-`submission date`)
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable))  
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier,  `submission date`) #%>%
    #mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/17102019_NFC.xlsx"),
             overwrite = TRUE)   
  
  ## --- 28) Bulamu  ----
  #"20190904 Bulamu Anom.xlsx"
  case <- read_excel("Output_step 2/20190904 Bulamu Anom.xlsx",
                     sheet = "Cleaned Data")
  codebook <- read_excel("Output_step 2/20190904 Bulamu Anom.xlsx",
                         sheet = "Codebook")
  raw <- read_excel("Raw data/bulamu.xlsx")
  
  raw <- raw %>%
    mutate(`Submission Date 2` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
    select(-`Submission Date`) %>%
    rename(`Submission Date` = `Submission Date 2`)
  
  case <- case %>%
    mutate(cal_hh_farmer_age = ifelse(hh_farmer_birthyear > 1900, 2021-hh_farmer_birthyear, NA)) %>%
    rename(farmer_present = sdm_farmer) 
  
  codebook <- codebook %>% 
    mutate(variable = ifelse(variable == "sdm_farmer", "farmer_present", variable))
  
  names(raw) <- tolower (names(raw)) 
  raw <- raw %>%
    select(identifier, f_location_level_1, `submission date`) %>%
    rename(pi_location_cascade_first_level = f_location_level_1) #%>%
    #filter(!is.na(pi_location_cascade_first_level)) %>%
   # mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T)))
  
  case <- merge(x = case, y = raw, by ="identifier", all.x = TRUE)
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3/20190904 Bulamu Anom.xlsx"),
             overwrite = TRUE)   

## --- Find mismatches ----
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
           "2021_RGL_anom_rice.xlsx",
           "12122019 Sparkx Anom.xlsx",
           "12122019_USOMI.xlsx",
           "20082020 Batian Anom.xlsx",
           "11082020 Mwea Anom.xlsx",
           "26032020 Musoni Maize.xlsx",
           "26032020 Musoni Sorghum.xlsx",
           "2020-11-20 Rubutco Anom.xlsx",
           "04022020 Egranary Anom.xlsx",
           "2021_tanzania-ussl.xlsx",
           "04102019_AGRI_WALLET_anonymized.xlsx",
           "12122019_Alluvial.xlsx",
           "11122019 Coscharis Anom.xlsx",
           "30012020 McCormick Anom - new format.xlsx",
           "17102019_NFC.xlsx",
           "20190904 Bulamu Anom.xlsx"
           )


vars_dashboard <- read_excel(
  here::here("variable names input dashboard.xlsx")) 

unmatched <- list()

## ---- loop through cases ---
for(i in 1:length(cases)){
  
  case <- read_excel(
    here::here("Output_step 3", cases[i]),
    sheet ="Cleaned Data") 
  
  codebook <- read_excel(
    here::here("Output_step 3", cases[i]),
    sheet ="Codebook") 
  
  #Add row to codebook
  variable <- c("pi_location_cascade_first_level")
  question <- c( "First level administrative divison. Variable created based on survey variable during data cleaning, for use in the Farmfit portal")
  section <- c("location - confidential ")
  type <- c("option")
  options <- c("context specific")
  add_var <- tibble(variable, question, section, type, options)
  
  codebook <- bind_rows(codebook, add_var)
  
  #All variables in delivered data into dataframe
  variable <-ls(case)
  data <- data.frame(variable)
  
  #Check if data variable is dashboard list     
  vars_dashboard$compare <- vars_dashboard$latest_var %in% data$variable 
  
  data <- vars_dashboard %>%
    filter(!is.na(latest_var)) %>%
    select(-old_var, -new_var ) %>%
    filter(compare == FALSE) %>%
    filter(is.na(note))
  
  unmatched[[cases[i]]] <- data
  
  output <- list("Cleaned Data" = case, "Codebook" = codebook)
  
  write.xlsx(output, here::here("Output_step 3", cases[i]),
             overwrite = TRUE)   
}
names(unmatched)[5]<-"NKG Honduras"
names(unmatched)[7]<-"EU Tanzania Ikanga"
names(unmatched)[8]<-"EU Tanzania non Ikanga"
names(unmatched)[9]<-"Smart logistics"
names(unmatched)[10]<-"AIF Rwanda"
names(unmatched)[24]<-"Agri wallet"
names(unmatched)[27]<-"McCornick"


## Export unmatched

write.xlsx(unmatched, file = here::here("Mismatch in dashboard variables.xlsx"), 
           sep=",", 
           row.names = FALSE,
           overwrite = TRUE)
