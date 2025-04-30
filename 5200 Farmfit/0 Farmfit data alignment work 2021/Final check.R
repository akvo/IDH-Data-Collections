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
library(dplyr)


here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Final check.R")


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
           "20082020 Batian Anom.xlsx",
           "11082020 Mwea Anom.xlsx",
           "26032020 Musoni Maize.xlsx",
           "26032020 Musoni Sorghum.xlsx",
           "2020-11-20 Rubutco Anom.xlsx",
           "04022020 Egranary Anom.xlsx",
           "04102019_AGRI_WALLET_anonymized.xlsx",
           "12122019_Alluvial.xlsx",
           "11122019 Coscharis Anom.xlsx",
           "30012020 McCormick Anom - new format.xlsx",
           "17102019_NFC.xlsx",
           "20190904 Bulamu Anom.xlsx",
           "12122019 Sparkx Anom.xlsx",
           "12122019_USOMI.xlsx")

transformations <- read_excel(
        here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/",
             "variables with transformation.xlsx"),
        sheet = "Sheet1") 

for(i in 1:length(cases)){

        case <- read_excel(
          here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output", cases[i]),
          sheet ="Cleaned Data") 
        
        codebook <- read_excel(
                here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output",cases[i]),
                sheet ="Codebook") 
        
        
        
        check_x <-ls(case)
        data <- data.frame(check_x)
        
        codebook$compare <- codebook$variable %in% data$check_x
 
        #Check if data variable is in codebook        
        data$compare <-data$check_x  %in%  codebook$variable 
        
        #Check if data variable is in transformed variable list
        data$compare_2 <-data$check_x %in% transformations$variable
        
        #if it's a "other variable", do not remove
        data <- data %>%
                mutate(compare = ifelse(grepl("_other",check_x), TRUE, compare)) %>%
                mutate(compare = ifelse(compare_2 == TRUE, TRUE, compare)) %>%
                filter(compare == FALSE)
        
        
        output <- list("To check" = data, "Codebook" = codebook, "Data" = case)
        
        
        write.xlsx (output, file = here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/Farmfit data alignment/Output copy", cases[i]))
        
}
