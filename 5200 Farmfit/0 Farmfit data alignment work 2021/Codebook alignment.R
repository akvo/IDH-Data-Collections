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

library(tidyr)
library(tidyselect)
library(stringr)
library(reshape2)
library(zoo)
library(splitstackshape)
library(varhandle)
library(purrr)
here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Codebook alignment.R")


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
           "20190904 Bulamu Anom.xlsx")

question_library <- read_excel(
        here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
             "question library format v2.1.1.xlsx"))


question_library <- question_library %>%
        dplyr::rename(section = "Title",
               question = "Text", 
               variable = "Variable name",
               type = "Question type",
               options = "Options",
               multiple = "Allow multiple") %>%
        mutate_all(tolower) %>%
        select(section,
               question,
               variable,
               type,
               options,
               multiple)
question_library <- question_library %>%
        mutate(variable = ifelse(variable == "f_labour_andprep_rememberwage", "f_labour_landprep_rememberwage", variable),
               variable = ifelse(variable == "f_focus_own_lost_measurement", "f_focus_measurement_lost", variable)) %>%
  fill(section)

vars_transformed <- read_excel(
        here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
             "variables with transformation.xlsx")) 

household_demographics  <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
             "household_demographics_var.xlsx")) 

unmatched <- list()
# DATA TYPE Set factors to integers
factor_to_int <- function(x){
  as.numeric(as.character(x))
}
## ---- loop through cases ----
for(i in 1:length(cases)){

        case <- read_excel(
          here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output", cases[i]),
          sheet ="Cleaned Data") 
        
        
        codebook <- read_excel(
                here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output",cases[i]),
                sheet ="Codebook") 

        ## ---- delete the unnecessary
        hh_demo <- case %>%
          select(identifier,
                 starts_with("hh_member_birthyear_"),
                 starts_with("hh_member_gender_"),
                 starts_with("hh_member_education_"))
        hh_demo_2 <- hh_demo[ , colSums(is.na(hh_demo)) < nrow(hh_demo)] 
        
        case <- case %>%
          select(-starts_with("hh_member_birthyear_"),
                 -starts_with("hh_member_gender_"),
                 -starts_with("hh_member_education_"))
        case <- merge(case, hh_demo_2, by.x = "identifier")

        ## ---- align variable names ---- ##
        
        if("cal_othermaincrop_income" %in% colnames(cases)){
          if("f_other_crop_income" %in% colnames(cases)){
            case <- case %>%
              mutate(cal_other_crop_income = cal_othermaincrop_income + f_other_crop_income) 
            
          }
        }
        
        if("f_inputs_usage" %in% colnames(case)){
          case <- case %>% 
            mutate(f_inputs_usage_types = f_inputs_usage) 
        }

        ## ---- alluvial ---- ##
        if(cases[i] == "2021-06-29_Alluvial_anom.xlsx"){
                case <- case %>% dplyr::rename(
                        f_livestock_days_hiredlabour = `f_livestcok_days_hiredlabour‚Ä¶92`) 
        }
        ## ---- NKG Uganda ---- ##
        if(cases[i] == "2021-04-12_Uganda NKG Anom.xlsx"){
                
                variable <- c("hh_healthcare_costs_howcovered")
                question <- c( "in the past 12 months, was the household able to cover the costs related to receiving healthcare, medicines and travelling to the health care provider?")
                section <- c("healthcare expenditures")
                type <- c("option")
                options <- c("we were able to pay all the costs|the costs were covered by a health insurance|We were able to pay part of the costs|we were not able to pay the costs|i don't know|i prefer not to say")
                add_var <- tibble(variable, question, section, type, options)
                
                codebook <- bind_rows(codebook, add_var)
        }
        ## ---- SL ---- ##
        if(cases[i] == "2021-03-19_smart logistics_anom.xlsx"){
          case<- case %>%
        mutate(cal_other_crop_income = f_othermaincrop_1_inc_sold - cal_othermaincrop_1_cost + f_other_crop_income)
        }
        ## ---- syngenta ---- ##
        if(cases[i] == "kenya-syngenta_potatoes.xlsx"){
          case<- case %>%
            mutate(cal_other_crop_income = cal_othermaincrop_1_inc_sold - cal_othermaincrop_1_cost + f_other_crop_income)
        }
        if(cases[i] == "kenya-syngenta_tomatoes.xlsx"){
          case<- case %>%
            mutate(cal_other_crop_income = cal_othermaincrop_1_inc_sold - cal_othermaincrop_1_cost + f_other_crop_income)
        }
        
  
        
        ## ---- sparkx ---- ##
        if(cases[i] == "12122019 Sparkx Anom.xlsx"){
          
        variable <- c("cl_coping_mechanisms_needs")
        question <- c( "what would you need to become better protected against extreme weather events?")
        section <- c("climate resilience")
        type <- c("free text")
        add_var <- tibble(variable, question, section, type)
        codebook <- bind_rows(codebook, add_var)
        }
        
        ## ---- usomi ----##
        if(cases[i] == "12122019_USOMI.xlsx"){

          
          extension <-  read_excel(
            here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/codebook_extensions.xlsx"),
            sheet = "Usomi")
          
          codebook <- bind_rows(codebook, extension)
        }
        
        ## ---- batian ----##
        if(cases[i] == "20082020 Batian Anom.xlsx"){
          extension <-  read_excel(
            here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/codebook_extensions.xlsx"),
            sheet = "Batian")
        
          codebook <- bind_rows(codebook, extension)
        }
        
        ## ---- Musoni ----##
        if(cases[i] == "26032020 Musoni Maize.xlsx"){
          case <- case %>%
            rename(hh_farmer_birthyear = hh_birthyear_farmer) 
        }
        
        if(cases[i] == "26032020 Musoni Sorghum.xlsx"){
          case <- case %>%
            rename(hh_farmer_birthyear = hh_birthyear_farmer) 
        }
        
        
        ## ---- Egrenary ----##
        if(cases[i] == "04022020 Egranary Anom.xlsx"){
          case<- case %>%
            rename(f_other_crop_types_other = `do you have income from other crops than previously discussed? i'll read you a list.--other--`)
        }
        
        ## ---- agri wallet ----##
        if(cases[i] == "04102019_AGRI_WALLET_anonymized.xlsx"){
          extension <-  read_excel(
            here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/codebook_extensions.xlsx"),
            sheet = "Agriwallet")
          
          codebook <- bind_rows(codebook, extension)
        }
        

        #All variables in delivered data into dataframe
        variable <-ls(case)
        data <- data.frame(variable)
       
       
## ---- Variables ending with other  ----
        #Check if data variable is in codebook        
        data$compare <-data$variable  %in%  codebook$variable 
        

        #if it's a "other variable", do not remove
        data <- data %>%
                mutate(compare = ifelse(grepl('_other$',variable) & compare == FALSE , FALSE, TRUE)) %>%
                filter(compare == FALSE) 

        codebook <- bind_rows(codebook, data)
        codebook <- codebook %>%
                arrange(variable)  %>%
                        fill(question,
                            section) %>%
                mutate(type = ifelse(!is.na(compare), "free text",type)) %>%
                
                mutate(variable_2 = str_remove(variable,'_other$')) %>%
                mutate(same = ifelse(variable_2== lag(variable_2), 1, 0)) %>%
                        
                mutate(section = ifelse(!is.na(compare) & same == 0 , "unknown", section),
                       question = ifelse(!is.na(compare) & same == 0 , "no question available", question)) %>%
                select(-compare, -variable_2, -same)

## ---- Calculated variables  ----
#Check if its a calculated variable
        variable <-ls(case)
        data <- data.frame(variable)
        data$compare <-data$variable  %in%  codebook$variable 
        data <- data %>%
                filter(compare == FALSE) %>%
                select(-compare)

        data$compare <-data$variable  %in%  vars_transformed$variable 
        data <-  data %>%
                filter(compare == FALSE)    %>%
                select(-compare)
        
#Check if its in the quesiton library
        data$compare <-data$variable  %in%  question_library$variable 
        data <- data %>%
                filter(compare == TRUE) %>%
                select(-compare)
        #If so, add it to the codebook        
        data <- merge(data, question_library, by.x = "variable")
        codebook <- bind_rows(codebook, data)

        #Check if it's not in calculations or quesiton library
        variable <-ls(case)
        data <- data.frame(variable)
        data$compare <-data$variable  %in%  codebook$variable 
        data <- data %>%
                filter(compare == FALSE) %>%
                select(-compare)
        
        data$compare <-data$variable  %in%  vars_transformed$variable 
        data <-  data %>%
                filter(compare == FALSE)    %>%
                select(-compare)
        
        data$compare <-data$variable  %in%  question_library$variable 
        data <- data %>%
                filter(compare == FALSE) %>%
                select(-compare)
        
        data$compare <- data$variable %in% household_demographics$variable
        data <- data %>%
          filter(compare == TRUE) %>%
          select(-compare)

        data <- merge(data, household_demographics, by.x = "variable")
        codebook <- bind_rows(codebook, data)
         
        
        hh_demo <- codebook %>%
          filter(str_detect(variable, 'hh_member_')) %>%
          arrange(variable) %>%
          fill(section, options)
        
        codebook <- codebook %>%
          filter(!str_detect(variable, 'hh_member_')) 
        codebook <- bind_rows(codebook, hh_demo)
        
        numerical_columns <- codebook %>%
          filter(type == "number") %>%
          select("variable") %>% pull()
        
        variable <-ls(case)
        data <- data.frame(variable)
        numerical_columns <- data.frame(numerical_columns)
        numerical_columns$compare <- numerical_columns$numerical_columns %in% data$variable
        numerical_columns <- numerical_columns %>%
          filter(compare == TRUE) %>%
          select(-compare)
        numerical_columns <- numerical_columns$numerical_columns
        
        case <- case %>%
          mutate_at(vars(numerical_columns), funs(as.numeric))
        
        #Prepare final list of unmatched vars
        variable <-ls(case)
        data <- data.frame(variable)
        data$compare <-data$variable  %in%  codebook$variable 
        data <- data %>%
          filter(compare == FALSE) %>%
          select(-compare)
        
        data$compare <-data$variable  %in%  vars_transformed$variable 
        data <-  data %>%
          filter(compare == FALSE)    %>%
          select(-compare)
        
        data$compare <-data$variable  %in%  question_library$variable 
        data <- data %>%
          filter(compare == FALSE) %>%
          select(-compare)  
        unmatched[[cases[i]]] <- data
        
        output <- list("Cleaned Data" = case, "Codebook" = codebook)
        
        write.xlsx(output, here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output_step 2", cases[i]) ,
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
        
        write.xlsx(unmatched, file = here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/", 
                                          "Mismatch in vars data codebook.xlsx"), 
                   sep=",", 
                   row.names = FALSE,
                   overwrite = TRUE)




