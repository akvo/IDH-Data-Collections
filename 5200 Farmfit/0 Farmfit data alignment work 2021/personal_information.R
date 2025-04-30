# Script: Extracting personal infomation
# Date: November 2021
# Contact: jildemarie@akvo.org


# Libraries 
library(readxl)
library(openxlsx)
library(here)

here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/personal_information.R")

library(tidyverse)
library(tidylog)
library(data.table)
library(cld2)
library(tidyr)

cases <- c("agriwallet.xlsx",
          "aif_rwanda.xlsx",
          "alluvial_19.xlsx",
          "alluvial_21.xlsx",
          "batian nuts.xlsx",
          "bulamu.xlsx",
          "coscharis_19.xlsx",
          "coscharis_21.xlsx",
          "egranary.xlsx",
          "eu tanzania.xlsx",
          "mccornick.xlsx",
          "musoni.xlsx",
          "mwea_rice.xlsx",
          "new forest.xlsx",
          "nkg_honduras.xlsx",
          "nkg_kenya.xlsx",
          "nkg_mexico.xlsx",
          "nkg_uganda.xlsx",
          "rubutco.xlsx",
          "smart_logistics.xlsx",
          "sparkx.xlsx",
          "syngenta_potatoes.xlsx",
          "syngenta_tomatoes.xlsx",
          "usomi.xlsx",
          "ussl_tanzania.xlsx")


for(i in 1:length(cases)){
raw_data <-  read_excel(
  here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Raw data", cases[i]), 
  sheet="Raw Data") %>%
  rename_all(funs(tolower)) %>%
  select(-contains("--option--")) 

list <- data.frame( ls(raw_data))

## ---- 1) Agri wallet Kenya Tomatoes Potatoes ---- ----

if(cases[i] == "agriwallet.xlsx"){
  raw_data <- raw_data %>% rename(
    monitoring_survey_yn = f_aw_survey_future,
    pi_location_other = f_location_other,
    pi_location_other_second_admin = f_location_other_county,
    pi_location_other_first_admin = f_location_other_district,
    pi_location_other_third_admin = f_location_other_subcounty,
    pi_location_datacollection = `where are you collecting farmer data?`,
    name_of_farmer = f_location_name_of_the_farmer,
    mobile_number_farmer = f_new_number,
    pi_location_cascade_region = f_location_region) 
}


## ---- 2) AIF Rwanda ----
## ---- 3) Alluvial 19 ----

if(cases[i] == "alluvial_19.xlsx"){
  raw_data <- raw_data %>% rename(
    pi_location = f_location,
    pi_location_2 = f_location_1,
    pi_location_other = f_location_other,
    pi_location_other_first_admin = f_location_other_lga,
    pi_location_other_second_admin = f_location_other_ward,
    pi_location_other_third_admin = ,
    pi_location_other_village = f_location_other_village,
    name_of_farmer = f_new_farmer,
    mobile_number_farmer = f_new_number,
    monitoring_survey_yn = f_aw_survey_future) 
}


## ---- 4) Alluvial 21 ----
if(cases[i] == "alluvial_21.xlsx"){
  raw_data <- raw_data %>% 
    select(-contains("option")) %>% rename(
    pi_location_other = pi_location_other_1,
    pi_geolocation_latitude = pi_geolocation_1_latitude,
    pi_geolocation_longitude = pi_geolocation_1_longitude,
    pi_geolocation_elevation = pi_geolocation_1_elevation) 
}


## ---- 5) Batian nuts ----
if(cases[i] == "batian nuts.xlsx"){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_village = village,
    pi_location_other_first_admin = pi_location_other_government_area,
    monitoring_survey_yn = pi_monitoring_survey,
    name_of_farmer = pi_name_of_farmer,
    mobile_number_farmer = pi_mobile_number_farmer)
}
  
## ---- 6) Bulamu----
if(cases[i] == "bulamu.xlsx"){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_level_2 = f_location_level_2,
    pi_location_cascade_level_3 = f_location_level_3,
    pi_location_cascade_level_4 = f_location_level_4,
    pi_location_cascade_level_5 = f_location_level_5,
    pi_location_other = f_location_other,
    pi_location_other_first_admin = f_location_other_district,
    pi_location_other_second_admin = f_location_other_county,
    pi_location_other_third_admin = f_location_other_subcounty,
    pi_location_fourth_admin = f_location_other_parish,
    pi_location_other_village = f_location_other_village,
    monitoring_survey_yn = future_cooperation,
    name_of_farmer = f_new_farmer,
    mobile_number_farmer = f_new_number) 
}


## ---- 7) Coscharis 19----
  if(cases[i] == "coscharis_19.xlsx"){
    raw_data <- raw_data %>% rename(
      pi_location = f_location,
      pi_location_other = f_location_other,
      pi_location_other_first_admin = f_location_other_lga,
      pi_location_other_village = f_location_other_village,
      name_of_farmer = f_new_farmer,
      mobile_number_farmer = f_new_number,
      monitoring_survey_yn = f_aw_survey_future) 
  }
  


## ---- 8) Coscharis 21----
if(cases[i] == "coscharis_21.xlsx"){
  raw_data <- raw_data %>% rename(
    pi_location_other = pi_location_other_1,
    pi_geolocation_latitude = pi_geolocation_1_latitude,
    pi_geolocation_longitude = pi_geolocation_1_longitude,
    pi_geolocation_elevation = pi_geolocation_1_elevation)
}

## ---- 9) Egrenary----
if(cases[i] == ("egranary.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_other_first_admin = pi_location_other_government_area,
    pi_location_other_second_admin = pi_location_other_ward,
    pi_location_other_third_admin = pi_location_other_village,
    pi_geolocation_latitude = geolocation_latitude,
    pi_geolocation_longitude = geolocation_longitude,
    pi_geolocation_elevation = geolocation_elevation)
}

## ---- 10) Eu tanzania----

## ---- 11) Mccornick----
if(cases[i] == ("mccornick.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_province = f_location_province,
    pi_location_cascade_district = f_location_district,
    pi_location_cascade_commune = f_location_commune,
    pi_location_other = f_location_other,
    pi_location_other_first_admin = f_location_other_district,
    pi_location_other_second_admin = f_location_other_commune,
    pi_location_other_third_admin = f_location_other_village,
    monitoring_survey_yn = future_cooperation,
    name_of_farmer = f_new_farmer,
    mobile_number_farmer = f_new_number)
}

## ---- 12) Musoni----
if(cases[i] == ("musoni.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_other_first_admin = pi_location_other_government_area,
    pi_location_other_second_admin = pi_location_other_ward,
    musoni_customer_number = pi_musoni_customer_number,
    first_agri_loan = pi_first_agri_loan,
    monitoring_survey_yn = pi_monitoring_survey,
    name_of_farmer = pi_name_of_farmer,
    mobile_number_farmer = pi_mobile_number_farmer)
}

## ---- 13) MWEA rice----
if(cases[i] == ("mwea_rice.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_other_first_admin = pi_location_other_ward,
    monitoring_survey_yn = pi_monitoring_survey,
    name_of_farmer = pi_name_of_farmer,
    mobile_number_farmer = pi_mobile_number_farmer)
}

## ---- 14) NFC----
if(cases[i] == ("new forest.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_district = f_location_other_district,
    pi_location_cascade_county = f_location_other_county,
    pi_location_cascade_subcounty = f_location_other_subcounty,
    pi_location_cascade_parish = f_location_other_parish,
    pi_location_cascade_village = f_location_other_village,
    monitoring_survey_yn = future_cooperation,
    name_of_farmer = f_new_farmer,
    mobile_number_farmer = f_new_number)
}

## ---- 15) NKG Honduras ---
if(cases[i] == ("nkg_honduras.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_geolocation_latitude = geolocation_latitude,
    pi_geolocation_longitude = geolocation_longitude,
    pi_geolocation_elevation = geolocation_elevation,
    name_of_farmer = name_farmer,
    mobile_number_farmer = contact_number) %>%
    mutate(monitoring_survey_yn = NA)
}


## ---- 16) NKG Kenya ----

if(cases[i] == ("nkg_kenya.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_geolocation_latitude = geolocation_latitude,
    pi_geolocation_longitude = geolocation_longitude,
    pi_geolocation_elevation = geolocation_elevation)
}

## ---- 17) NKG Mexico ----
if(cases[i] == ("nkg_mexico.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_geolocation_latitude = geolocation_latitude,
    pi_geolocation_longitude = geolocation_longitude,
    pi_geolocation_elevation = geolocation_elevation) %>%
    mutate(monitoring_survey_yn = NA,
           mobile_number_farmer = NA)
}

## ---- 18) NKG Uganda----

if(cases[i] == ("nkg_uganda.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_geolocation_latitude = geolocation_latitude,
    pi_geolocation_longitude = geolocation_longitude,
    pi_geolocation_elevation = geolocation_elevation)
}

## ---- 19) Rubutco----

if(cases[i] == ("rubutco.xlsx")){
  raw_data <- raw_data %>% rename(
    monitoring_survey_yn = pi_monitoring_survey,
    name_of_farmer = pi_name_of_farmer,
    mobile_number_farmer = pi_mobile_number_farmer)
}

## ---- 20) Smart logistics----

if(cases[i] == ("smart_logistics.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_county = pi_location_cascade_1_county,
    pi_location_cascade_sub_county = pi_location_cascade_1_sub_county,
    pi_location_cascade_ward = pi_location_cascade_1_ward,
    pi_location_cascade_village = pi_location_cascade_1_village,
    pi_location_other = pi_location_other,
    pi_location_other_first_admin = pi_location_other_first_admin_1,
    pi_location_other_second_admin = pi_location_other_second_admin_1,
    pi_location_other_third_admin = pi_location_other_third_admin_1,
    pi_location_other_village = pi_location_other_village_1)
}

## ---- 21) Sparkx ----

if(cases[i] == ("sparkx.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade = f_location,
    pi_location_cascade_district = f_location_1_district,
    pi_location_cascade_zone = f_location_1_zone,
    pi_location_other = f_location_other,
    pi_location_other_first_admin = f_location_other_lga,
    pi_location_other_second_admin = f_location_other_ward,
    pi_location_other_village = f_location_other_village,
    name_of_farmer = f_new_farmer,
    mobile_number_farmer = f_new_number,
    monitoring_survey_yn = f_aw_survey_future)
}

## ---- 22) Syngenta potatoes ----

if(cases[i] == ("syngenta_potatoes.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_sub_county = pi_location_cascade_1_sub_county,
    pi_location_cascade_area = pi_location_cascade_1_area,
    pi_location_other_first_admin = pi_location_other_first_admin_1,
    pi_location_other_second_admin = pi_location_other_second_admin_1,
    pi_location_other_third_admin = pi_location_other_third_admin_1,
    pi_location_other_village = pi_location_other_village_1,
    pi_geolocation_latitude = pi_geolocation_1_latitude,
    pi_geolocation_longitude = pi_geolocation_1_longitude,
    pi_geolocation_elevation = pi_geolocation_1_elevation
  )
}

## ---- 23) Syngenta tomatoes ----

if(cases[i] == ("syngenta_tomatoes.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_county = pi_location_cascade_1_county_,
    pi_location_cascade_sub_county = pi_location_cascade_1_sub_county,
    pi_location_other_first_admin = pi_location_other_first_admin_1,
    pi_location_other_second_admin = pi_location_other_second_admin_1,
    pi_location_other_third_admin = pi_location_other_third_admin_1,
    pi_location_other_village = pi_location_other_village_1,
    pi_geolocation_latitude = pi_geolocation_1_latitude,
    pi_geolocation_longitude = pi_geolocation_1_longitude,
    pi_geolocation_elevation = pi_geolocation_1_elevation
  )
}

## ---- 24) Usomi ----

if(cases[i] == ("usomi.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_sub_county = f_location_sub_county,
    pi_location_cascade_ward = f_location_ward,
    pi_location_cascade_village = f_location_village,
    pi_location_other = f_location_other,
    pi_location_other_first_admin = f_location_other_county,
    pi_location_other_second_admin = f_location_other_subcounty,
    pi_location_other_village = f_location_other_village,
    name_of_farmer = f_new_farmer,
    mobile_number_farmer = f_new_number,
    monitoring_survey_yn = f_aw_survey_future
  )
}

## ---- 25) USSL ----

if(cases[i] == ("ussl_tanzania.xlsx")){
  raw_data <- raw_data %>% rename(
    pi_location_cascade_mkoa = pi_location_cascade_1_mkoa,
    pi_location_cascade_wilaya = pi_location_cascade_1_wilaya,
    pi_location_cascade_level_3 = pi_location_cascade_1_level_3,
    pi_location_other = pi_location_other_1,
    pi_location_other_first_admin = pi_location_other_first_admin_1,
    pi_location_other_second_admin = pi_location_other_second_admin_1,
    pi_location_other_village = pi_location_other_village_1,
    pi_geolocation_latitude = pi_geolocation_1_latitude,
    pi_geolocation_longitude = pi_geolocation_1_longitude,
    pi_geolocation_elevation = pi_geolocation_1_elevation
  )
}



## ---- select variables ----
  raw_data <- raw_data %>%
    filter(`repeat no`==1) %>%
    select(identifier,
           name_of_farmer,
           mobile_number_farmer,
           contains("pi_"), -contains("ppi")) 


output <- list("Personal information" = raw_data)

write.xlsx(output, here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Personal information", paste0("pi_", cases[i])))

}


