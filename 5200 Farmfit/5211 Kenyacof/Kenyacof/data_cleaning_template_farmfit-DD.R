##  ---- Preps ----
# Data delivery bare bones

# Packages used
library(here); library(readxl); library(openxlsx);
library(plyr); library(dplyr); library(tidyr); library(tidylog); library(tidyselect)
library(stringr); library(data.table); library(reshape2)
library(zoo);library(splitstackshape) ;library(tidyverse)

setwd("/Users/generic/Documents/GitHub/Akvo_GitHub/Analytics-Guild/1. Descriptive Analytics/IDH/5200 Farmfit/5211 Kenyacof/Kenyacof")

##  ---- Describe datafiles ----
##DOWNLOAD THESE ALL TOGETHER IN A FOLDER
data_filename <- "Data/sucafina_raw_data.xlsx.xlsx"
survey_filename <- "survey/survey_sucafina_coffee.xls"
data_delivery <- "output/2022_sucafina_anom_coffee_all.xlsx"
data_delivery_embu <- "output/2022_sucafina_anom_coffee_embu.xlsx"
data_delivery_kirinyaga <- "output/2022_sucafina_anom_coffee_kirinyaga.xlsx"
data_delivery_nandi <- "output/2022_sucafina_anom_coffee_nandi.xlsx"


question_library <- read_excel("survey/question library format v3.1.1.xlsx")
vars_transformed <- read_excel("survey/variables with transformation.xlsx")
household_demographics <- read_excel("survey/household_demographics_var.xlsx")
vars_dashboard <- read_excel("survey/variable names input dashboard.xlsx")


##  ---- Provide case information ----
case = "Sucafina"
sdm_crop = "Coffee"
country = "Kenya"
year ="2022"


pi_filename <- paste("pi_",year,"_",case,"_",sdm_crop,".xlsx", sep="")


##  ---- Functions ----

# DATA TYPE Set factors to integers
factor_to_int <- function(x){
  as.numeric(as.character(x))
}

# Find and replace outliers
outlier_detection <- function(x){
  ifelse(
    x > (mean(x, na.rm=TRUE) + sd(x, na.rm=TRUE)*3) |
      x < (mean(x, na.rm=TRUE) - sd(x, na.rm=TRUE)*3), 
    9997,
    x
  )
}
options(scipen = 999)  

# count values without NA
count_n <- function(x){sum(!is.na(x))}

##  ---- Import data ----
# Read data from the data folder
Data <- read_excel(data_filename)
Data_raw <- read_excel((paste0(data_filename))) %>%
  select(-starts_with("pi_"), -name_of_farmer, -mobile_number_farmer)

# Dynamic filter as requested by IDH
#Data <- Data %>%
#  filter(pi_location_cascade_Level_1=="Embu")


# Read file with survey structure
survey_questions <- read_excel((survey_filename),sheet ="Full Survey", skip=2)


##  ---- Adjust survey to be used for analysis  ----
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
  rename("section" = "Title", 
         "variable" = "Variable name", 
         "question" = "Text",
         "type" = "Question type", 
         "options" = "Options",
         "multiple" = "Allow multiple") %>%
  # All variables to lowercase
  mutate_all(tolower) %>%
  # Filter the empty variables
  filter(!is.na(variable)) 

##  ---- Get basic information on your data ----

# Collect the different variables, numerical questions:
numerical_columns <- survey_questions %>%
  filter(type == "number") %>%
  select("variable") %>% pull()

## ---- Count initial number of participants
nr_participants_raw <- length(unique(Data$Identifier))

##  ---- Make changes to datasets and codebook ----

# Changes to data set:
Data <- Data %>%
  
  # Set the date to date format
  mutate(`Submission Date` = as.Date(`Submission Date`, format = "%d-%m-%Y")) %>%
  
  # Remove irrelevant columns
  select(-c(`Display Name`,`Device identifier`,`Instance`,
            `Submitter`, `Form version`)) %>%
  select(-contains("--option--")) %>%
  
  # All column names to lowercase
  rename_all(funs(tolower)) %>%
  
  # Rename option variables
  rename_all( funs(gsub("--other--", "_other2", ., perl=T))) %>%

  # All variables to lowercase
  mutate_all(tolower) %>%
  
  # Remove to farmers that didn't participate
  mutate(ic_informed_consent = na.locf(ic_informed_consent, na.rm = FALSE)) %>%
  filter(ic_informed_consent == "accepted to participate")  %>%
  
  
  # Numerical columns to numeric data type
  mutate_at(vars(numerical_columns), funs(as.numeric)) %>%
    
    #General case
    mutate(f_coffee_harvest_num = as.numeric(f_coffee_harvest_num)) %>%
    ## In many cases, the crop has been added to this variable name. Variable will be f_[CROP]_harvest_num. See example below for a cocoa case.
    #mutate(f_cocoa_harvest_num = as.numeric(f_cocoa_harvest_num)) %>% 
  
  # Remove all "other" option text ("other, please specify")
  # mutate_each(funs(str_remove(., "other, please specify"))) %>%
  mutate_if(is.character, funs(gsub("other, please specify", NA,.))) %>% 
  mutate_if(is.character, funs(gsub("\\|$","",.))) %>% 
  
  # Remove trailing spaces
  mutate_if(is.character, str_trim) %>% 
  
  # Remove punctuation and special characters - EXCEPT for "|"
  mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) %>% 
  
  # OUTLIERS:
  mutate_at(vars(numerical_columns), funs(outlier_detection)) %>%
  
  # All variables that are 9999/"i don't know" or 9998/"i prefer not to say" are set to NA
  mutate_if(is.numeric, list(~na_if(., 9999))) %>%
  mutate_if(is.numeric, list(~na_if(., 9998))) %>%
  mutate_if(is.numeric, list(~na_if(., 9997))) %>%
  mutate_if(is.character, list(~na_if(., "i don't know"))) %>%
  mutate_if(is.character, list(~na_if(., "i prefer not to say"))) 

 
  # FARM SIZE
Data <- Data %>%
  
  # Calculate size in acres for all cases
  mutate(f_size_acre = f_size * ifelse(f_unit_land == "hectares", 2.471, 1)) %>%
  # Calculate size in hectares for all cases
  mutate(f_size_hectare = f_size_acre / 2.471) %>%
  # Calculate focus crop size in acres for all cases
  mutate(f_focus_crop_size_acre = f_focus_crop_size * ifelse(f_unit_land == "hectares", 2.471, 1)) %>%
  # Calculate focus crop size in hectares for all cases
  mutate(f_focus_crop_size_hectare = f_focus_crop_size_acre / 2.471) %>%
  # Calculate other main crop sizes in acres for all cases
  mutate(f_size_othermaincrop_1_acre = f_size_othermaincrop_1 * ifelse(f_unit_land == "hectares", 2.471, 1)) %>%
  mutate(f_size_othermaincrop_2_acre = f_size_othermaincrop_2 * ifelse(f_unit_land == "hectares", 2.471, 1)) %>%
  # Calculate other main crop sizes in hectares for all cases
  mutate(f_size_othermaincrop_1_hectare = f_size_othermaincrop_1_acre / 2.471) %>%
  mutate(f_size_othermaincrop_2_hectare = f_size_othermaincrop_2_acre / 2.471) %>%
  
  
  # Extend farm size to determine productivity
  mutate(f_focus_crop_size_acre = na.locf(f_focus_crop_size_acre, na.rm = FALSE)) %>%
  # Age
  mutate(cal_hh_farmer_age = 2022 - hh_head_birthyear,
         cal_hh_head_age = 2022-hh_head_birthyear)
         #cal_hh_member_birthyear = 2022-hh_head_birthyear) #update to current year
         
  # #Rename variable names in data and codebook - delete rows that do not apply
  # ##Cocoa case:
  # Data <- Data %>%
  #   rename(f_harvest_num = f_cocoa_harvest_num,
  #          f_focus_month_first_season_start = f_cocoa_month_first_season_start,
  #          f_focus_month_first_season_end = f_cocoa_month_first_season_end,
  #          f_focus_month_second_season_start = f_cocoa_month_second_season_start,
  #          f_focus_month_second_season_end = f_cocoa_month_second_season_end,
  #          f_focus_shade_trees = f_cocoa_shade_trees)
  # 
  # ##Make sure that each variable you rename in the data, is also renamed in the codebook
  # survey_questions <- survey_questions %>%
  #   mutate(variable = ifelse(variable == "f_cocoa_harvest_num", "f_focus_harvest_num", variable),
  #          variable = ifelse(variable == "f_cocoa_month_first_season_start", "f_focus_month_first_season_start", variable),
  #          variable = ifelse(variable == "f_cocoa_month_first_season_end", "f_focus_month_first_season_end", variable),
  #          variable = ifelse(variable == "f_cocoa_month_second_season_start", "f_focus_month_second_season_start", variable),
  #          variable = ifelse(variable == "f_cocoa_month_second_season_end", "f_focus_month_second_season_end", variable),
  #          variable = ifelse(variable == "f_cocoa_shade_trees", "_focus_shade_trees", variable))
  # 
  
  ##Coffee case:
  Data <- Data %>%
    rename(f_harvest_num = f_coffee_harvest_num,
           f_focus_month_first_season_start = f_coffee_month_first_season_start,
           f_focus_month_first_season_end = f_coffee_month_first_season_end,
           f_focus_month_second_season_start = f_coffee_month_second_season_start,
           f_focus_month_second_season_end = f_coffee_month_second_season_end,
           f_focus_shade_trees = f_coffee_shade_trees,
           f_focus_rev_timeperiod = f_coffee_rev_timeperiod,
           f_focus_measurement_prod = f_coffee_measurement_prod,
           f_focus_measurement_sold = f_coffee_measurement_sold,
           #f_focus_lost_yn = f_coffee_lost_yn,
           f_focus_measurement_lost = f_coffee_measurement_lost)
  
  ##Make sure that each variable you rename in the data, is also renamed in the codebook
  survey_questions <- survey_questions %>%
    mutate(variable = ifelse(variable == "f_coffee_harvest_num", "f_focus_harvest_num", variable),
           variable = ifelse(variable == "f_coffee_month_first_season_start", "f_focus_month_first_season_start", variable),
           variable = ifelse(variable == "f_coffee_month_first_season_end", "f_focus_month_first_season_end", variable),
           variable = ifelse(variable == "f_coffee_month_second_season_start", "f_focus_month_second_season_start", variable),
           variable = ifelse(variable == "f_coffee_month_second_season_end", "f_focus_month_second_season_end", variable),
           variable = ifelse(variable == "f_coffee_shade_trees", "f_focus_shade_trees", variable),
           variable = ifelse(variable == "f_coffee_rev_timeperiod,", "f_focus_rev_timeperiod", variable),
           variable = ifelse(variable == "f_coffee_measurement_prod", "f_focus_measurement_prod", variable),
           variable = ifelse(variable == "f_coffee_measurement_sold", "f_focus_measurement_sold", variable),
           # variable = ifelse(variable == "f_coffee_lost_yn", "f_focus_lost_yn", variable),
           variable = ifelse(variable == "f_coffee_measurement_lost", "f_focus_measurement_lost", variable))
  
  # # ##Tea case:
  # Data <- Data %>%
  #   rename(f_focus_measurement_prod = f_tea_measurement_prod,
  #          f_focus_quant_prod = f_tea_quant_prod,
  #          f_focus_price = f_tea_price,
  #          # f_focus_lost_yn = f_tea_lost_yn,
  #          f_focus_quant_lost = f_tea_quant_lost,
  #          f_focus_measurement_lost = f_coffee_measurement_lost
  #          )

  # Get original number of participants
  nr_participants_ic <- length(unique(Data$identifier))

  
  # OTHER
  variables_other <- Data %>% 
    select(ends_with("_other"), ends_with("_other2")) %>% 
    names() 

  for(other in variables_other){
    
    if(any(names(Data) %in% gsub("_other$|_other2$", "", other))){
      
      # Fuzzy match? - many spelling mistakes in "other" questions
      
      # Combine "other" variables with their parents
      Data <- Data %>%
        unite(!!gsub("_other$|_other2$", "", other), 
              c(gsub("_other$|_other2$", "", other), all_of(other)), 
              sep="|", remove=FALSE, na.rm=TRUE) %>%
        select(-other)
    }
  }

# NUMBER OF SEASONS
number_of_seasons <- max(Data$f_harvest_num, na.rm=TRUE)

##  ---- ______ ----
##  ---- ACTUAL INCOME CALCULATIONS ----
#For more detailed description, see: https://docs.google.com/spreadsheets/d/1LvBxEfzq7jfLlw6Yv3VXi6hRps90SVG1qMEixSBxfZ4/edit#gid=208057956

##  ---- 1) Contextualize-> Prep: transform measurement units  ----
  ## 1a --- Standard case ----


  Data <- Data %>%
  # Extract the numeric values from the measurement units for quantity produced, sold, consumed and lost
  ## Always check by hand whether this is going well. We extract the numbers from the measurement units to calculate total quantities.
  ## Example, if a farmer produces 5 bags of 25 kg -> We extract 25 from the measurement unit, to calculate total production: 5*25 = 125kg
  ## If there are other measurement units, not related to kg, put a value for those in the variable: cal_focus_measurement_prod
  ## Example, if there is a measurement unit named "bucket" that has a value of about 20 kg, put a value of 20 in the variable cal_focus_measurement_prod
     
   ##Measurement unit for production
      mutate(cal_focus_measurement_prod := ifelse(
          grepl("[[:digit:]]", f_focus_measurement_prod), 
         extract_numeric(f_focus_measurement_prod), 
          ifelse(f_focus_measurement_prod %in% c("KG","kg","kilo","kgs","kilogram"), 1, 0))) %>%
 
      ##Measurement unit for sales
      mutate(cal_focus_measurement_sold := ifelse(
          grepl("[[:digit:]]", f_focus_measurement_sold), 
          extract_numeric(f_focus_measurement_sold), 
          ifelse(f_focus_measurement_sold %in% c("kg","kilo","kgs","kilogram"), 1, 0))) %>%       
  
     # measurement unit for lost
      mutate(cal_focus_measurement_lost := ifelse(
      grepl("[[:digit:]]", f_focus_measurement_lost), 
      extract_numeric(f_focus_measurement_lost), 
      ifelse(f_focus_measurement_lost %in% c("kg","kilo","kgs","kilogram"), 1, 0)))
  
    # #measurement unit for own consumption
    # ##!! REMOVE IF NOT APPLICABLE
    # mutate(cal_focus_measurement_own_consumption := ifelse(
    #   grepl("[[:digit:]]", f_focus_own_consumption_measurement), 
    #   extract_numeric(f_focus_own_consumption_measurement), 
    #   ifelse(f_focus_own_consumption_measurement %in% c("kg","kilo","kgs","kilogram"), 1, 0))) 
  
    #code to use if you need to have "odd" measurement units for your case, such as a "bucket" , example code:
    #mutate(cal_focus_measurement_sold := ifelse(
    #  f_focus_measurement_sold == "bucket", 20,
    #  cal_focus_measurement_sold )) 

    ## ---- 1b Coffee case ----
    ##Adapt based on the types of coffee applicable
    ##Measurement unit for production
    #  mutate(cal_focus_measurement_prod := ifelse(
    #    grepl("[[:digit:]]", f_coffee_measurement_prod), 
    #    extract_numeric(f_coffee_measurement_prod), 
    #    ifelse(f_coffee_measurement_prod %in% c("KG","kg","kilo","kgs","kilogram"), 1, 0))) %>%
        
    #  ##Measurement unit for sales
    #  mutate(cal_focus_measurement_sold := ifelse(
    #    grepl("[[:digit:]]", f_coffee_measurement_sold), 
    #    extract_numeric(f_coffee_measurement_sold), 
    #    ifelse(f_coffee_measurement_sold %in% c("kg","kilo","kgs","kilogram"), 1, 0))) %>%       
      
      # measurement unit for lost
    #  mutate(cal_focus_measurement_lost_arabica := ifelse(
    #    grepl("[[:digit:]]", f_coffee_lost_measurement), 
    #    extract_numeric(f_coffee_lost_measurement), 
    #    ifelse(f_coffee_lost_measurement %in% c("kg","kilo","kgs","kilogram"), 1, 0)))

    #   
    #code to use if you need to have "odd" measurement units for your case, such as a "bucket" , example code:
    #mutate(cal_focus_measurement_sold := ifelse(
    #  f_coffee_measurement_sold == "bucket", 20,
    #  cal_focus_measurement_sold )) 

##  ---- 2) Prep: Calculate quantities  ----
    ##Calculate the quantities
    ##2a) Standard case - USE APPROACH 2B IF YOU HAVE A COFFEE CASE ----

Data <- Data %>%
    mutate(cal_coffee_price_arabica= as.numeric(f_coffee_price_arabica),
           cal_coffee_price_arabica = ifelse(is.na(f_coffee_price_arabica),100,cal_coffee_price_arabica)) %>% 
    mutate(cal_coffee_quant_prod_kg_arabica = f_coffee_quant_prod_arabica * cal_focus_measurement_prod) %>%
    mutate(cal_coffee_quant_sold_kg_arabica = f_coffee_quant_sold_arabica * cal_focus_measurement_sold) %>%
    mutate(cal_coffee_quant_lost_kg_arabica = f_coffee_quant_lost_arabica * cal_focus_measurement_lost) 
    
  ##  ---- 3) Calc: Focus crop income  ----  
   
   ##  ---- 3a.1) Calc: Focus crop revenue - STANDARD ----  
   # Data <- Data %>%
   #   mutate(cal_coffee_revenue_arabica = cal_focus_quant_sold_kg * cal_focus_price)  #Delete if type of coffee is not used in survey
   # 
  ##  ---- 3a.2) Calc: Focus crop revenue - COFFEE ----  


  Data <- Data %>%
    mutate(cal_coffee_revenue_arabica = cal_coffee_quant_sold_kg_arabica * cal_coffee_price_arabica)  #Delete if type of coffee is not used in survey
   

  ##  ---- X) STANDARD APPROACH Inbetween, adjust for repeated question groups (IF APPLICABLE)  ----  
    ### REPLACE OR ADD ROWS FOR COFFEE CASES
    ## For some variables you need to add _robusta, _arabica, _hybridimproved, _liberica, _other -> check above which variables you have used.
    ## cal_focus_quant_prod_kg, cal_focus_quant_sold_kg,cal_focus_quant_lost_kg,cal_coffee_revenue_arabica
    ##Adress values for those that have multiple seasons
    Data <- Data %>%
      #Identify duplicates
      arrange(`repeat no`,identifier) %>%
      group_by(identifier, f_focus_rev_timeperiod) %>% mutate(count_dup = n()) %>%
      mutate(count_dup = ifelse(is.na(f_focus_rev_timeperiod) , NA, count_dup)) %>% #check whether we don't make mistakes here. For some cases farmer enter twice wrongly, in other cases entering same values for 2 seasons is ok.
      #Identify those with multiple seasons
      group_by(identifier, count_dup) %>% mutate(count_rep_seasons = ifelse(is.na(f_focus_rev_timeperiod),0, n())) 

    #Replace values with 0 for those that entered the data twice 
    # For coffee case, replicate this piece of code for each coffee type 
    Data <- Data %>%
      mutate(`repeat no` = as.numeric(`repeat no`)) %>%
      mutate(cal_coffee_quant_prod_kg_arabica = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_coffee_quant_prod_kg_arabica)) %>%
      mutate(cal_coffee_quant_sold_kg_arabica = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_coffee_quant_sold_kg_arabica)) %>%
      # mutate(cal_focus_quant_own_consumption_kg = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_focus_quant_own_consumption_kg )) %>%
      mutate(cal_coffee_quant_lost_kg_arabica = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_coffee_quant_lost_kg_arabica)) %>%
      mutate(cal_coffee_revenue_arabica = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_coffee_revenue_arabica)) %>%
      
      
      #Replace values with 0 for those that entered data for 2 seasons and for a year. Keep the data for a year.
      mutate(cal_coffee_quant_prod_kg_arabica = ifelse(identifier == "eugm2du6g7us"  & f_focus_rev_timeperiod == "season 2 timefrmae",
                                              0, cal_coffee_quant_prod_kg_arabica)) %>%
      mutate(cal_coffee_quant_sold_kg_arabica = ifelse(identifier == "eugm2du6g7us"  & f_focus_rev_timeperiod == "season 2 timefrmae",
                                              0, cal_coffee_quant_sold_kg_arabica)) %>%
      mutate(cal_coffee_quant_lost_kg_arabica = ifelse(identifier == "eugm2du6g7us"  & f_focus_rev_timeperiod == "season 2 timefrmae",
                                              0, cal_coffee_quant_lost_kg_arabica)) %>%
      mutate(cal_coffee_revenue_arabica = ifelse(identifier == "eugm2du6g7us"  & f_focus_rev_timeperiod == "season 2 timefrmae",
                                        0, cal_coffee_revenue_arabica)) 
      
    #Sum the quantities for those that entered data for multiple seasons
    # Approach 1) STANDARD CASE
      # group_by(identifier, count_seasons) %>%
      # mutate(cal_focus_quant_prod_kg =  sum(cal_focus_quant_prod_kg)) %>%
      # mutate(cal_focus_quant_sold_kg =  sum(cal_focus_quant_sold_kg)) %>%
      # mutate(cal_focus_quant_own_consumption_kg =  sum(cal_focus_quant_own_consumption_kg)) %>%
      # mutate(cal_focus_quant_lost_kg =  sum(cal_focus_quant_lost_kg)) %>%
      # mutate(cal_coffee_revenue_arabica = sum(cal_coffee_revenue_arabica)) %>%
    
    # Approach 2) COFFEE CASE, Example for Arabica ---- USe this 
  Data <- Data %>%
    mutate(cal_coffee_quant_prod_kg_arabica =  ifelse(is.na(cal_coffee_quant_prod_kg_arabica), 0, cal_coffee_quant_prod_kg_arabica)) %>%
    mutate(cal_coffee_quant_sold_kg_arabica  =  ifelse(is.na(cal_coffee_quant_sold_kg_arabica), 0, cal_coffee_quant_prod_kg_arabica)) %>%
    mutate(cal_coffee_quant_lost_kg_arabica  =  ifelse(is.na(cal_coffee_quant_lost_kg_arabica), 0, cal_coffee_quant_prod_kg_arabica)) %>%
    mutate(cal_coffee_revenue_arabica = ifelse(is.na(cal_coffee_revenue_arabica), 0, cal_coffee_quant_prod_kg_arabica)) %>%
    group_by(identifier) %>%
    mutate(cal_coffee_quant_prod_kg_arabica =  sum(cal_coffee_quant_prod_kg_arabica )) %>%
    mutate(cal_coffee_quant_sold_kg_arabica  =  sum(cal_coffee_quant_sold_kg_arabica )) %>%
    mutate(cal_coffee_quant_lost_kg_arabica  =  sum(cal_coffee_quant_lost_kg_arabica )) %>%
    mutate(cal_coffee_revenue_arabica = sum(cal_coffee_revenue_arabica)) %>%
  
      #Ensure we have 1 row of data for each farmer
       # Approach 1) STANDARD CASE
      # mutate(cal_focus_quant_prod_kg = ifelse(`repeat no` >1 , NA, cal_focus_quant_prod_kg)) %>%
      # mutate(cal_focus_quant_sold_kg = ifelse(`repeat no` >1  , NA, cal_focus_quant_sold_kg)) %>%
      # mutate(cal_focus_quant_own_consumption_kg = ifelse(`repeat no` >1  , NA, cal_focus_quant_own_consumption_kg )) %>%
      # mutate(cal_focus_quant_lost_kg = ifelse(`repeat no` >1 , NA, cal_focus_quant_lost_kg)) %>%
      # mutate(cal_coffee_revenue_arabica = ifelse (`repeat no`>1, NA, cal_coffee_revenue_arabica)) %>%
      # ungroup() %>%
      # select(-`repeat no`,-count_dup, -count_seasons)
      # 
      # Approach 2) COFFEE CASE, Example for Arabica
        mutate(cal_coffee_quant_prod_kg_arabica = ifelse(`repeat no` >1 , NA, cal_coffee_quant_prod_kg_arabica)) %>%
        mutate(cal_coffee_quant_sold_kg_arabica = ifelse(`repeat no` >1  , NA, cal_coffee_quant_sold_kg_arabica)) %>%
        mutate(cal_coffee_quant_lost_kg_arabica = ifelse(`repeat no` >1 , NA, cal_coffee_quant_lost_kg_arabica)) %>%
        mutate(cal_coffee_revenue_arabica = ifelse (`repeat no`>1, NA, cal_coffee_revenue_arabica)) %>%
        ungroup() %>%
        select(-count_dup, -count_rep_seasons)

  ##  ---- 3b) Calc: Revenues  ----   
  ##  ---- 3c.1) STANDARD CASE  ---- 
  ##Calculate revenue from focus crop
  # Data <- Data %>%
  # mutate(cal_coffee_revenue_arabica= ifelse(is.na(cal_coffee_revenue_arabica), 0, cal_coffee_revenue_arabica))  
  # 
  ##  ---- 3c.2) Coffee case  ---- 
  ##Calculate revenue from focus crop
  Data <- Data %>%
    mutate(
      cal_coffee_revenue_arabica = ifelse(is.na(cal_coffee_revenue_arabica), 0, cal_coffee_revenue_arabica),# DELETE IF TYPE IS NOT USED
           ) %>% # DELETE IF TYPE IS NOT USED
    mutate(cal_coffee_revenue_arabica = Data %>%
             select(starts_with("cal_coffee_revenue_arabica")) %>% 
             rowSums(na.rm=TRUE)) #%>%
    #select(-starts_with("cal_coffee_revenue_"))
         
  ##  ---- 3c) Calc: Productivity  ----  
  ##Calculate productivity
  ## Approach 1 STANDARD CASE
  # Data <- Data %>%
  # mutate(cal_focus_productivity_acre = cal_focus_quant_prod_kg/f_focus_crop_size_acre) 

        
  # Approach 2) COFFEE CASE, Example for Arabica
Data <- Data %>%
  mutate(cal_coffee_productivity_acre_arabica = cal_coffee_quant_prod_kg_arabica/f_focus_crop_size_acre) 
        
   #If crop is perennial crop, calculate yield/tree
   ## COFFEE CASE: Add or change rows if you have multiple coffee varieties -> you will have an adjusted cal_focus_quant_prod_kg variable
   #mutate (cal_focus_productivity_tree = cal_focus_quant_prod_kg/f_trees_amount)

  ##  ---- 3d) Calc: Labour costs  ----  
  ###ONLY FOR KENYACOF CASE:
  Data <- Data %>%
  rename(f_labour_compostprep_paymentperday = f_labour_compostprep_paymentpertimeframe_1_1,
         f_labour_cropmaint_paymentperactivity = f_labour_cropmaint_paymentpertimeframe)

  survey_questions <- survey_questions %>%
    mutate(variable = ifelse(variable == "f_labour_cropmaint_paymentpertimeframe","f_labour_cropmaint_paymentperactivity",variable),
           variable = ifelse(variable == "f_labour_compostprep_paymentpertimeframe_1_1","f_labour_compostprep_paymentperday",variable))
  

  
  #Prepare 
  labour_types <- Data %>% 
    cSplit("f_crop_labour_types", "|") %>% 
    select(starts_with("f_crop_labour_types")) %>% 
    gather(key, value) %>% filter(!is.na(value)) %>% 
    select(value) %>% unique() %>% pull()
  
  survey_labour_types <- substr(gsub("\\s|-", "", labour_types), 1, 5)
  Data$f_labour_cropmaint_paymentperactivity <- as.numeric(Data$f_labour_cropmaint_paymentperactivity)
  

  ##!!!!!!Adjust labour_options per case. Check in the survey which activities are surveyed.
  labour_options <- c("landprep", "nurserymaint", "cropmaint",
                      "irrigation", "fertilizerapp", "compostprep", "harvesting", "agrochemicalapp",
                      "postharvest", "otheractivity")
  
  
  
  #Calculation of labour costs
  for(j in survey_labour_types) {
    
    if(any(startsWith(labour_options, j))){
      
      var_name <- paste0("_labour_", labour_options[startsWith(labour_options, j)])
      
        Data <- Data %>% 
        mutate_at(vars(contains("_othertype")), as.numeric) %>%
        mutate_at(vars(contains("_othercosts")), as.numeric)
      
        Data <- Data %>% 
        #Calculate total number of hours for those that reported per hour
        mutate("cal{var_name}_nrhours" := Data %>% 
                 select(!!sym(paste0("f", var_name, "_nrhours")),
                        !!sym(paste0("f", var_name, "_nrdays"))) %>%
                 apply(.,1,prod,na.rm=FALSE)) %>%
        mutate("f{var_name}_nrdays" := ifelse(!is.na(!!sym(paste0("f", var_name, "_nrhours"))), NA,
                                              !!sym(paste0("f", var_name, "_nrdays"))))
        
        ##Calculate wage costs for those that paid per hour
        Data <- Data %>% 
        mutate("cal{var_name}_hour_costs" := Data %>% 
                 select(!!sym(paste0("f", var_name, "_nrhours")),
                        !!sym(paste0("f", var_name, "_paymentperhour")),
                        !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
                 apply(.,1,prod))
        
        ##Calculate wage costs for those that paid per day
        Data <- Data %>% 
        mutate("cal{var_name}_day_costs" := Data %>% 
                 select(!!sym(paste0("f", var_name, "_nrdays")),
                        !!sym(paste0("f", var_name, "_paymentperday")),
                        !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
                 apply(.,1,prod))
        
        ##Calculate wage costs for those that paid per months
        Data <- Data %>% 
        mutate("cal{var_name}_month_costs" := Data %>% 
                 select(!!sym(paste0("f", var_name, "_nrmonths")),
                        !!sym(paste0("f", var_name, "_paymentpermonth")),
                        !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
                 apply(.,1,prod))
        
 
        Data <- Data %>%
        mutate("cal{var_name}_activity_costs" := Data %>%
                 select(!!sym(paste0("f", var_name, "_paymentperactivity")),
                        !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
                 apply(.,1,prod))
          
      Data <- Data %>% 
        unite(!!paste0("cal", var_name, "_part_1_total_cost"),
              !!sym(paste0("cal", var_name, "_day_costs")):!!sym(paste0("cal", var_name, "_activity_costs")), 
              sep="|", remove=FALSE, na.rm=FALSE) %>%  
        mutate("cal{var_name}_part_1_total_cost" := extract_numeric( !!sym(paste0("cal", var_name, "_part_1_total_cost"))))
        
      Data <- Data %>% 
      #Calculate total cost for those that  reported wage using "other method"
      mutate("f{var_name}_rememberwage_othercosts" := as.numeric(
        !!sym(paste0("f", var_name, "_rememberwage_othercosts"))))
        
      Data <- Data %>% 
      mutate("cal{var_name}_part_2_total_cost" := Data %>% 
               select(!!sym(paste0("f", var_name, "_rememberwage_othercosts")),
                      !!sym(paste0("f", var_name, "_nrhiredpeople"))) %>%
               apply(., 1, prod))
      
    }
  }
  
    #Remove several variables
    Data <- Data %>%
      select(-contains("alltimeframes"), 
             -ends_with("hour_costs"),
             -ends_with("day_costs"),
             -ends_with("month_costs"),
             -ends_with("activity_costs"),
             -cal_labour_agrochemicalapp_nrhours,
             -cal_labour_compostprep_nrhours,
             -cal_labour_cropmaint_nrhours,
             -cal_labour_fertilizerapp_nrhours,
             -cal_labour_harvesting_nrhours,
             -cal_labour_irrigation_nrhours,
             -cal_labour_landprep_nrhours,
             -cal_labour_postharvest_nrhours) %>%
      mutate_at(vars(starts_with("cal_labour") & ends_with("_total_cost")), as.numeric) 

  # Combine costs of all types of labour
  Data <- Data %>% 
    mutate(cal_labour_cost = Data %>%
             select(starts_with("cal_labour") & ends_with("_total_cost")) %>% 
             rowSums(na.rm=TRUE)) %>%
    mutate(cal_labour_cost = ifelse(is.na(f_crop_labour_timeperiod), NA, cal_labour_cost))
  
  Data <- Data %>%
    select(-contains("part_1_total_cost"),-contains("part_2_total_cost")) 
  
  ##  ---- X) Inbetween, adjust for repeated questions groups (IF APPLICABLE) ----  
  
            ##Adress values for those that have multiple seasons
          Data <- Data %>%
            #Identify duplicates
            group_by(identifier, f_crop_labour_timeperiod) %>% mutate(count_dup = n()) %>% #check whether we don't make mistakes here. For some cases farmer enter twice wrongly, in other cases entering same values for 2 seasons is ok.
            mutate(count_dup = ifelse(is.na(f_crop_labour_timeperiod), NA, count_dup)) %>%
            #Identify those with multiple seasons
            group_by(identifier, count_dup) %>% mutate(count_rep_seasons = ifelse(is.na(f_crop_labour_timeperiod),0, n())) %>%
            
            #Replace values with 0 for those that entered the data twice
            mutate(`repeat no` = as.numeric(`repeat no`)) %>%
            mutate(cal_labour_cost = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_labour_cost)) %>%
            
            #Sum the values for those that entered data for multiple seasons
            mutate(cal_labour_cost =  ifelse(is.na(cal_labour_cost), 0, cal_labour_cost)) %>%
            group_by(identifier) %>%
            mutate(cal_labour_cost =  sum(cal_labour_cost)) %>%
            
            #Ensure we have 1 row of data for each farmer
            mutate(cal_labour_cost = ifelse(`repeat no` >1 , NA, cal_labour_cost)) %>%
            ungroup() %>%
            select(-count_dup, -count_rep_seasons) %>%
            
            #put 0 for those that don't hire
            mutate(cal_labour_cost = ifelse(f_crop_labour_types == "none of the above", 0, cal_labour_cost))
  
    
  ##  ---- 3e) Calc: Inputs costs  ----  
  Data <- Data %>%
    rename(f_inputs_types = f_inputs_costs_types) %>%
    mutate_at(vars(starts_with("f_inputs_costs")), as.numeric) 
  
  Data <- Data %>%
    mutate(cal_inputs_costs = Data %>% 
             select(starts_with("f_inputs_costs")) %>%
             rowSums(na.rm=TRUE)) 
  
  Data <- Data %>%
    rename(f_inputs_costs_types = f_inputs_types) 
  
  
  
  ##  ---- X) Inbetween, adjust for RQG  ----  
              ##Address values for those that have multiple seasons
              Data <- Data %>%
                #Identify duplicates
                group_by(identifier, f_inputs_timeperiod) %>% mutate(count_dup = n()) %>%
                mutate(count_dup = ifelse(is.na(f_inputs_timeperiod), NA, count_dup)) %>%
                #Identify those with multiple seasons
                group_by(identifier, count_dup) %>% mutate(count_rep_seasons = ifelse(is.na(f_inputs_timeperiod),0, n())) %>%
                
                #Replace values with 0 for those that entered the data twice
                mutate(repeat_no = as.numeric(`repeat no`)) %>%
                mutate(cal_inputs_costs = ifelse(repeat_no >1  & count_dup > 1, 0, cal_inputs_costs)) %>%
                
                #Sum the values for those that entered data for multiple seasons
                
                mutate(cal_inputs_costs =  ifelse(is.na(cal_inputs_costs),0,cal_inputs_costs)) %>% 
                group_by(identifier) %>%
                mutate(cal_inputs_costs =  sum(cal_inputs_costs)) %>%
                
                #Ensure we have 1 row of data for each farmer
                mutate(cal_inputs_costs = ifelse(`repeat no` >1 , NA, cal_inputs_costs)) %>%
                ungroup() %>%
                select(-repeat_no,-count_dup, -count_rep_seasons)
  
  ##  ---- 3e) Calc: Total focus crop production costs ----                
  Data <- Data %>%
    mutate(cal_labour_cost = ifelse(is.na(cal_labour_cost), 0, cal_labour_cost),
           cal_inputs_costs = ifelse(is.na(cal_inputs_costs), 0, cal_inputs_costs)) %>%
    mutate(cal_focus_cost = cal_labour_cost + cal_inputs_costs + f_transport) %>%

  ##  ---- 3f) Calc: Net-income focus crop ----                
  
  mutate(cal_focus_income = cal_coffee_revenue_arabica - cal_focus_cost)

  ##  ---- 4) Calc: Net-income other crops ----                

Data <- Data %>%
  mutate(cal_othermaincrop_1_cost = Data %>% 
           select(starts_with("f_othermaincrop_1_cost")) %>%
           apply(.,1,sum, na.rm=TRUE)) %>%
  mutate(cal_othermaincrop_2_cost = Data %>% 
           select(starts_with("f_othermaincrop_2_cost")) %>%
           apply(.,1,sum, na.rm=TRUE)) %>%
  mutate(cal_othermaincrop_1_inc_sold = ifelse(is.na(f_othermaincrop_1_inc_sold), 0, f_othermaincrop_1_inc_sold)) %>%
  mutate(cal_othermaincrop_2_inc_sold = ifelse(is.na(f_othermaincrop_2_inc_sold), 0, f_othermaincrop_2_inc_sold))  %>%
  mutate(cal_othermaincrop_income = cal_othermaincrop_1_inc_sold - cal_othermaincrop_1_cost + cal_othermaincrop_2_inc_sold - cal_othermaincrop_2_cost  ) %>%
  mutate(cal_other_crop_income = ifelse(is.na(f_other_crop_income), 0, f_other_crop_income)) %>%
  mutate(cal_other_crop_income = cal_othermaincrop_income + cal_other_crop_income) %>%
  select(-cal_othermaincrop_income)
  
  
  
  ##  ---- 5) Calc: Net-income livestock ----                
  
  Data <- Data %>%

  #Revenue -!!! Check which approach is used. 
    ## 1) AsK for income at once (f_livestock_income_total) OR 
    ## 2) per type of livestock (f_livestock_income_[ANIMAL])
    ##Delete the code for the approach that does not apply
    
    #Approach 1
    # mutate(cal_livestock_revenue = f_livestock_income_type)
  
    #Approach 2
    mutate(cal_livestock_revenue = Data %>% 
             select(contains("f_livestock_income"), -contains("type")) %>%
             rowSums(na.rm=TRUE)) %>%
    mutate(cal_livestock_revenue= ifelse(is.na(cal_livestock_revenue), 0, cal_livestock_revenue)) %>%
    
    #Input costs
    mutate(cal_livestock_inputs_cost = Data %>% 
             select(f_livestock_costs_fodderwater, 
                    f_livestock_costs_medics) %>%
             rowSums(na.rm=TRUE)) %>%
    mutate(cal_livestock_inputs_cost= ifelse(is.na(cal_livestock_inputs_cost), 0, cal_livestock_inputs_cost)) 

    #Labour cost
    Data <- Data %>%
      mutate(cal_livestock_labour_cost = Data %>% 
               select(f_livestock_nr_hired_labourers, 
                      f_livestock_days_hiredlabour, 
                      f_livestock_wages_hiredlabour) %>%
               apply(., 1, prod, na.rm=FALSE)) %>%
      mutate(cal_livestock_labour_cost= ifelse(is.na(cal_livestock_labour_cost), 0, cal_livestock_labour_cost)) 

    # total cost
    Data <- Data %>%
      mutate(cal_livestock_cost = Data %>% 
               select(cal_livestock_inputs_cost,
                      cal_livestock_labour_cost) %>%
               rowSums(na.rm=FALSE)) %>%
      #mutate(cal_livestock_cost= ifelse(is.na(cal_livestock_cost), 0, cal_livestock_cost)) %>%
      
    #net income
    mutate(cal_livestock_income = cal_livestock_revenue - cal_livestock_cost)

  ##  ---- 6) Calc: Other farm costs ----                
    
  # EQUIPMENT
  Data <- Data %>%
    mutate(cal_equipment_costs = Data %>% 
             select(f_nonmech_equip_costs, f_mech_equip_costs, f_materials_other_costs) %>% 
             apply(.,1,sum, na.rm=TRUE))
  
  # GENERAL ON FARM COSTS
   Data <- Data %>%
    mutate(cal_farm_other_costs = Data %>% 
             select(f_costs_land_focuscrop,
                    hh_loan_one_other_costs_interest,
                    hh_loan_one_agri_costs_interest,
                    hh_loan_multiple_largest_costs_interest,
                    hh_loan_multiple_agri_costs_interest) %>%
             apply(., 1, sum, na.rm=TRUE)) 
    
    ##  ---- 7) Calc: Off farm labour income----                
    
  Data <- Data %>%
    mutate(cal_off_farm_labour_income = Data %>% 
             select(f_nonfarm_enterpr_1_profit,
                    # f_nonfarm_enterpr_2_profit,
                    # f_nonfarm_enterpr_3_profit,
                    f_income_offfarmlabour_wage) %>%
             apply(., 1, sum, na.rm=TRUE)) 

    ##  ---- 8) Calc: Off farm non labour income----    
    Data <- Data %>% 
      mutate(cal_offfarm_non_labour_income = Data %>%
               select(starts_with("f_income_other_"), -contains("_type")) %>% 
               apply(.,1,sum, na.rm=TRUE)) 


    ##  ---- 9) Calc: Farm income and actual income----    
Data <- Data %>%
  mutate(cal_farm_income = cal_focus_income + cal_other_crop_income + cal_livestock_income - 
           cal_equipment_costs - cal_farm_other_costs) %>%
  mutate(cal_actual_income = cal_focus_income + cal_other_crop_income + cal_livestock_income - 
           cal_equipment_costs - cal_farm_other_costs + cal_off_farm_labour_income + cal_offfarm_non_labour_income) %>%
  mutate(cal_farm_income = ifelse(is.na(sdm_farmer)  , NA, cal_farm_income )) %>%
  mutate(cal_actual_income  = ifelse(is.na(sdm_farmer) , NA, cal_actual_income))

    ##  ---- ______----    
    ##  ---- Calculate net promotor score----    
    #NPS = (Promoters/total # farmers) - (Detractors/total # farmers)
    #This is calculated from the variable cs_recommendation:
    #“How likely is it that you would recommend [SDM company] to a friend or peer?”)
    
    #Not likely, somewhat likely, likely = detractor
    #Very likely = promoter
    
    hh_farmer_gender <- c("all farmers")
    county <- c("all farmers")
    
    NSP_total <- Data %>%
      select(`repeat no`, cs_recommendation) %>% 
      filter(`repeat no` == 1)%>%
      mutate(cs_recommendation = ifelse(is.na(cs_recommendation), "no input",cs_recommendation)) %>%
      mutate(detractor = ifelse(cs_recommendation == "not likely"| cs_recommendation == "somewhat likely"| cs_recommendation == "likely",1,0),
             promoter = ifelse(cs_recommendation == "very likely", 1,0),
             passive = ifelse(cs_recommendation == "most likely"| cs_recommendation == "i dont know",1,0),
             nr_recommenders = ifelse(cs_recommendation == "no input", 0,1)) %>%
      summarize(nr_promoters = sum(promoter),
                nr_detractors = sum(detractor),
                nr_passive = sum(passive),
                nr_farmers = sum(nr_recommenders)) %>%
      mutate(nsp_total = round((nr_promoters/nr_farmers) - (nr_detractors/nr_farmers),2),
             hh_farmer_gender = hh_farmer_gender,
             pi_location_cascade_first_level = county)
    
    NSP_by_gender <- Data %>%
      select(`repeat no`, cs_recommendation, hh_farmer_gender) %>% 
      filter(`repeat no` == 1)%>%
      mutate(cs_recommendation = ifelse(is.na(cs_recommendation), "no input",cs_recommendation)) %>%
      mutate(detractor = ifelse(cs_recommendation == "not likely"| cs_recommendation == "somewhat likely"| cs_recommendation == "likely",1,0),
             promoter = ifelse(cs_recommendation == "very likely", 1,0),
             passive = ifelse(cs_recommendation == "most likely"| cs_recommendation == "i dont know",1,0),
             nr_recommenders = ifelse(cs_recommendation == "no input", 0,1)) %>%
      group_by(hh_farmer_gender) %>%
      summarize(nr_promoters = sum(promoter),
                nr_detractors = sum(detractor),
                nr_passive = sum(passive),
                nr_farmers = sum(nr_recommenders)) %>%
      mutate(nsp_gender = round((nr_promoters/nr_farmers) - (nr_detractors/nr_farmers),2),
             pi_location_cascade_first_level = county)
    
    NSP_by_county <- Data %>%
      select(`repeat no`, cs_recommendation, pi_location_cascade_first_level) %>% 
      filter(`repeat no` == 1)%>%
      mutate(cs_recommendation = ifelse(is.na(cs_recommendation), "no input",cs_recommendation)) %>%
      mutate(detractor = ifelse(cs_recommendation == "not likely"| cs_recommendation == "somewhat likely"| cs_recommendation == "likely",1,0),
             promoter = ifelse(cs_recommendation == "very likely", 1,0),
             passive = ifelse(cs_recommendation == "most likely"| cs_recommendation == "i dont know",1,0),
             nr_recommenders = ifelse(cs_recommendation == "no input", 0,1)) %>%
      group_by(pi_location_cascade_first_level) %>%
      summarize(nr_promoters = sum(promoter),
                nr_detractors = sum(detractor),
                nr_passive = sum(passive),
                nr_farmers = sum(nr_recommenders)) %>%
      mutate(nsp_county = round((nr_promoters/nr_farmers) - (nr_detractors/nr_farmers),2),
             hh_farmer_gender = hh_farmer_gender)
    
    NSP <- full_join(NSP_total, NSP_by_gender)
    NSP <- full_join(NSP, NSP_by_county)
    NSP <- NSP %>% select(hh_farmer_gender, pi_location_cascade_first_level, nr_farmers, nr_promoters, nr_detractors,nr_passive, nsp_total, nsp_gender, nsp_county)
    
##  ---- ______----    
##  ---- Prepare anom clean data for further use----    
#Private information is deleted
#We need to keep the variable capturing the highest level of location (pi_location_cascade_first..) for the Farmfit portal. Check what variable that is for each case, this differs.
    #Rename it temporarily such that it is not deleted. Always rename it to pi_location_cascade_first_level
    Data <- Data %>%
          rename(location_cascade_region = pi_location_cascade_level_1) %>% 
          select(
      -c(starts_with("pi_"), -contains("county")),
      -c(name_of_farmer, mobile_number_farmer)) %>%
          rename(pi_location_cascade_first_level = location_cascade_region)
  
##  ---- _______ ----
    ##  ---- COMPLETE DATA ----  
##  ---- Summary statistics ----    
##  ---- 1) ALL FARMERS ----

farmer_type <- c("all farmers")
all_var <- Data %>% 
  filter(`repeat no` == 1) %>%
  select(`repeat no`) %>% 
  table() %>% 
  melt(c("farmer type"), value.name="n") %>%
  mutate(`farmer type` = farmer_type)

## ---- a) numerical descriptives ----
numerical_descriptives_all_farmers <- Data %>%
  select_if(is.numeric) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
  melt(value.name="mean") %>%
  left_join(
    Data %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
      melt(value.name="sd")) %>%
  left_join(
    Data %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(min(., na.rm = TRUE))) %>%
      melt(value.name="min")
  ) %>% 
  left_join(
    Data %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
      melt(value.name="max")
  ) %>% 
  left_join(
    Data %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(count_n))  %>%
      melt(value.name="freq")
  ) %>%
  left_join(survey_questions) %>%
  select("variable", "freq", "mean","sd", "min","max") 

## ---- b) single categorical descriptives ----

single_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(is.na(multiple)) %>% 
  pull(variable)

single_categorical_descriptives_all_farmers <- all_var

for(i in single_mc){
  
  if(any(names(Data) %in% i)){
    
    frequencies <- Data %>% 
      filter(`repeat no`==1) %>%
      select(all_of(i)) %>%       
      table() %>% 
      melt(c("category"),value.name = "freq") %>%
      mutate(n = nr_participants_raw) %>%
      mutate(`farmer type` = farmer_type) %>%
      mutate("variable" = i) %>%
      left_join(all_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    single_categorical_descriptives_all_farmers <- single_categorical_descriptives_all_farmers %>% 
      full_join(frequencies) %>%
      select(variable, `farmer type`, n, category, freq, "%")
    
  }
}

## ---- c) multiple categorical descriptives ----
multiple_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(multiple == "yes") %>%
  pull(variable)

multiple_categorical_descriptives_all_farmers <- all_var

for(j in multiple_mc){
  
  if(any(names(Data) %in% j)){
    
    frequencies <- Data %>% 
      select(all_of(j)) %>% 
      cSplit(j, "|") %>%
      gather(key, value) %>%
      select(-key) %>% table() %>%
      melt(c("category"), value.name="freq") %>%
      mutate(n = nr_participants_raw) %>%
      mutate("variable" = j) %>%
      left_join(all_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    multiple_categorical_descriptives_all_farmers <- multiple_categorical_descriptives_all_farmers %>% 
      full_join(frequencies) %>%
      select(variable, `farmer type`, n, category, freq, "%")
    
  }
}
##  ---- _______ ----
##  ---- 2) FEMALE FARMERS ----

Data_female <- Data %>%
  filter(hh_farmer_gender == "female")

nr_female <- length(Data_female$identifier)

farmer_type <- c("female farmers")
all_var <- Data_female %>% 
  filter(`repeat no` == 1) %>%
  select(`repeat no`) %>% 
  table() %>% 
  melt(c("farmer type"), value.name="n") %>%
  mutate(`farmer type` = farmer_type)

## ---- a) numerical descriptives ----
numerical_descriptives_female_farmers <- Data_female %>%
  select_if(is.numeric) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
  melt(value.name="mean") %>%
  left_join(
    Data_female %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
      melt(value.name="sd")) %>%
  left_join(
    Data_female %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(min(., na.rm = TRUE))) %>%
      melt(value.name="min")
  ) %>% 
  left_join(
    Data_female %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
      melt(value.name="max")
  ) %>% 
  left_join(
    Data_female %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(count_n))  %>%
      melt(value.name="freq")
  ) %>%
  left_join(survey_questions) %>%
  mutate(gender = "Female") %>%
  mutate(n=nr_female) %>%
  select("variable","gender","n", "freq", "mean","sd", "min","max") 

## ---- b) single categorical descriptives ----

single_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(is.na(multiple)) %>% 
  pull(variable)

single_categorical_descriptives_female_farmers <- all_var

for(i in single_mc){
  
  if(any(names(Data_female) %in% i)){
    
    frequencies <- Data_female %>% 
      filter(`repeat no`==1) %>%
      select(all_of(i)) %>%       
      table() %>% 
      melt(c("category"),value.name = "freq") %>%
      mutate(n = nr_female) %>%
      mutate(`farmer type` = farmer_type) %>%
      mutate("variable" = i) %>%
      left_join(all_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
      full_join(frequencies) %>%
      select(variable, `farmer type`, n, category, freq, "%")
    
  }
}

single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
  mutate(gender = "Female") %>%
  mutate(n=nr_female) %>%
  select("variable","gender","n", "category", "freq","%")


## ---- c) multiple categorical descriptives ----
multiple_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(multiple == "yes") %>%
  pull(variable)

multiple_categorical_descriptives_female_farmers <- all_var

for(j in multiple_mc){
  
  if(any(names(Data_female) %in% j)){
    
    frequencies <- Data_female %>% 
      select(all_of(j)) %>% 
      cSplit(j, "|") %>%
      gather(key, value) %>%
      select(-key) %>% table() %>%
      melt(c("category"), value.name="freq") %>%
      mutate(n = nr_female) %>%
      mutate("variable" = j) %>%
      left_join(all_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
      full_join(frequencies) %>%
      select(variable, `farmer type`, n, category, freq, "%")
    
  }
}

multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
  mutate(gender = "Female") %>%
  mutate(n=nr_female) %>%
  select("variable","gender","n", "category", "freq","%")

##  ---- _______ ----
##  ---- 3) MALE FARMERS ----

Data_male <- Data %>%
  filter(hh_farmer_gender == "male")

nr_male <- length(Data_male$identifier)

farmer_type <- c("male farmers")
all_var <- Data_male %>% 
  filter(`repeat no` == 1) %>%
  select(`repeat no`) %>% 
  table() %>% 
  melt(c("farmer type"), value.name="n") %>%
  mutate(`farmer type` = farmer_type)

## ---- a) numerical descriptives ----
numerical_descriptives_male_farmers <- Data_male %>%
  select_if(is.numeric) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
  melt(value.name="mean") %>%
  left_join(
    Data_male %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
      melt(value.name="sd")) %>%
  left_join(
    Data_male %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(min(., na.rm = TRUE))) %>%
      melt(value.name="min")
  ) %>% 
  left_join(
    Data_male %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
      melt(value.name="max")
  ) %>% 
  left_join(
    Data_male %>% 
      select_if(is.numeric) %>%
      summarise_each(funs(count_n))  %>%
      melt(value.name="freq")
  ) %>%
  left_join(survey_questions) %>%
  mutate(gender = "Male") %>%
  mutate(n=nr_male) %>%
  select("variable","gender","n", "freq", "mean","sd", "min","max") 

## ---- b) single categorical descriptives ----

single_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(is.na(multiple)) %>% 
  pull(variable)

single_categorical_descriptives_male_farmers <- all_var

for(i in single_mc){
  
  if(any(names(Data_male) %in% i)){
    
    frequencies <- Data_male %>% 
      filter(`repeat no`==1) %>%
      select(all_of(i)) %>%       
      table() %>% 
      melt(c("category"),value.name = "freq") %>%
      mutate(n = nr_male) %>%
      mutate(`farmer type` = farmer_type) %>%
      mutate("variable" = i) %>%
      left_join(all_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
      full_join(frequencies) %>%
      select(variable, `farmer type`, n, category, freq, "%")
    
  }
}

single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
  mutate(gender = "Male") %>%
  mutate(n=nr_male) %>%
  select("variable","gender","n", "category", "freq","%")


## ---- c) multiple categorical descriptives ----
multiple_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(multiple == "yes") %>%
  pull(variable)

multiple_categorical_descriptives_male_farmers <- all_var

for(j in multiple_mc){
  
  if(any(names(Data_male) %in% j)){
    
    frequencies <- Data_male %>% 
      select(all_of(j)) %>% 
      cSplit(j, "|") %>%
      gather(key, value) %>%
      select(-key) %>% table() %>%
      melt(c("category"), value.name="freq") %>%
      mutate(n = nr_male) %>%
      mutate("variable" = j) %>%
      left_join(all_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
      full_join(frequencies) %>%
      select(variable, `farmer type`, n, category, freq, "%")
    
  }
}

multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
  mutate(gender = "Male") %>%
  mutate(n=nr_male) %>%
  select("variable","gender","n", "category", "freq","%")
##  ---- _______ ----
##  ---- 4) COMBINE GENDER  ----
## ---- a) numerical descriptives ----
numerical_descriptives_by_gender <- rbind(numerical_descriptives_female_farmers, numerical_descriptives_male_farmers)
numerical_descriptives_by_gender <- numerical_descriptives_by_gender %>%
  arrange(variable, gender)

## ---- b) single categorical descriptives ----
single_categorical_descriptives_by_gender <- rbind(single_categorical_descriptives_female_farmers, single_categorical_descriptives_male_farmers)
single_categorical_descriptives_by_gender <- single_categorical_descriptives_by_gender %>%
  arrange(variable, gender)

## ---- c) multiple categorical descriptives ----
multiple_categorical_descriptives_by_gender <- rbind(multiple_categorical_descriptives_female_farmers, multiple_categorical_descriptives_male_farmers)
multiple_categorical_descriptives_by_gender <- multiple_categorical_descriptives_by_gender %>%
  arrange(variable, gender)

## ---- ___ ----

# Check completeness of the codebook

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
         multiple) %>% fill(section)

unmatched <- list()

#Make a dataframe listing all variables 
variable <-ls(Data)
data <- data.frame(variable)

## ---- Variables ending with other  ---

#Check if data variable is in codebook        
data$compare <-data$variable  %in%  survey_questions$variable 

#if it's a "other variable", do not remove
data <- data %>%
  mutate(compare = ifelse(grepl('_other$',variable) & compare == FALSE , FALSE, TRUE)) %>%
  filter(compare == FALSE) 

survey_questions <- bind_rows(survey_questions, data)
survey_questions <- survey_questions %>%
  arrange(variable)  %>%
  fill(question,
       section) %>%
  mutate(type = ifelse(!is.na(compare), "free text",type)) %>%
  
  mutate(variable_2 = str_remove(variable,'_other$')) %>%
  mutate(same = ifelse(variable_2== lag(variable_2), 1, 0)) %>%
  
  mutate(section = ifelse(!is.na(compare) & same == 0 , "unknown", section),
         question = ifelse(!is.na(compare) & same == 0 , "no question available", question)) %>%
  select(-compare, -variable_2, -same)


variable <-ls(Data)
data <- data.frame(variable)
data$compare <-data$variable  %in%  survey_questions$variable 
data <- data %>%
  filter(compare == FALSE) %>%
  select(-compare)

data$compare <-data$variable  %in%  vars_transformed$variable 

#check whether there is a match by checking out the "data" dataset MANUALLY
#if a variable is not in the vars_transformed, update the excel with variable transformations


#One changes are applied, append the codebook with information from the calculated variables (vars_transformed)
library(dplyr)

# Assuming "data" and "vars_transformed" are your dataframes

vars_calculated <- data$variable

calculated_variables <- vars_transformed %>%
  filter(variable %in% vars_calculated)

survey_questions <- survey_questions %>%
  bind_rows(calculated_variables)


#Check whether already in the codebook
variable <-ls(Data)
data <- data.frame(variable)
data$compare <-data$variable  %in%  survey_questions$variable 
data <- data %>%
  filter(compare == FALSE) 


data <- data %>%
  filter(variable %in% household_demographics$variable) %>%
  select(-compare)

data <- merge(data, household_demographics, by.x = "variable")
survey_questions <- bind_rows(survey_questions, data)

# --- Check household demographic questions ---
hh_demo <- survey_questions %>%
  filter(str_detect(variable, 'hh_member_')) %>%
  arrange(variable) %>%
  fill(section, options)

survey_questions <- survey_questions %>%
  filter(!str_detect(variable, 'hh_member_')) 
survey_questions <- bind_rows(survey_questions, hh_demo)


# ---- dashboard variables ----
#All variables in delivered data into dataframe

variable <-ls(Data)
data <- data.frame(variable)

#Check if data variable is dashboard list     
vars_dashboard$compare <- vars_dashboard$latest_var %in% data$variable 

data <- vars_dashboard %>%
  filter(!is.na(latest_var)) %>%
  filter(compare == FALSE) %>%
  filter(is.na(note))


variable <- ls(Data)
data <- data.frame(variable) %>%
  filter(
    !variable %in% c(survey_questions$variable, vars_transformed$variable, question_library$variable)
  )

  ## ---- ___ ----
  ## ---- all farmers - Combine all inputs in one excel ----
  
  #Check if applicable, this change is necessary to upload data to the portal.

  sets <- list(
    "Codebook" = survey_questions,
    "Cleaned Data" = Data,
    "Raw Data (anonymised)" = Data_raw, 
    "Num. desc. all farmers" = numerical_descriptives_all_farmers,
    "Cat. desc. single all farmers" = single_categorical_descriptives_all_farmers,
    "Cat. desc. multi all farmers" = multiple_categorical_descriptives_all_farmers,
    "Num. desc. by gender" = numerical_descriptives_by_gender,
    "Cat. desc. single by gender" = single_categorical_descriptives_by_gender,
    "Cat. desc. multi by gender" = multiple_categorical_descriptives_by_gender,
    "Net promoter score" = NSP)
  
  write.xlsx (sets, file = (paste0(data_delivery)))
  
  ##  ---- _______ ----
  ##  ---- EMBU DATA ----  
  ##  ---- Summary statistics ----    
  ##  ---- 1) ALL FARMERS ----
  Data_embu <- Data %>%
    filter(pi_location_cascade_first_level=="embu")
  
  Data_embu_raw<- read_excel((paste0(data_filename))) %>%
    filter(pi_location_cascade_Level_1 == "Embu") %>%
    select(-starts_with("pi_"), -name_of_farmer, -mobile_number_farmer) 
   
  
  farmer_type <- c("all farmers")
  all_var <- Data_embu %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_all_farmers <- Data_embu %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_embu %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_embu %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_embu %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_embu %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    select("variable", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_all_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_embu) %in% i)){
      
      frequencies <- Data_embu %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_participants_raw) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_all_farmers <- single_categorical_descriptives_all_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_all_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_embu) %in% j)){
      
      frequencies <- Data_embu %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_participants_raw) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_all_farmers <- multiple_categorical_descriptives_all_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  ##  ---- _______ ----
  ##  ---- 2) FEMALE FARMERS ----
  
  Data_embu_female <- Data_embu %>%
    filter(hh_farmer_gender == "female")
  
  nr_female <- length(Data_embu_female$identifier)
  
  farmer_type <- c("female farmers")
  all_var <- Data_embu_female %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_female_farmers <- Data_embu_female %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_embu_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_embu_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_embu_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_embu_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_female_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_embu_female) %in% i)){
      
      frequencies <- Data_embu_female %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_female) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "category", "freq","%")
  
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_female_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_embu_female) %in% j)){
      
      frequencies <- Data_embu_female %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_female) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "category", "freq","%")
  
  ##  ---- _______ ----
  ##  ---- 3) MALE FARMERS ----
  
  Data_embu_male <- Data_embu %>%
    filter(hh_farmer_gender == "male")
  
  nr_male <- length(Data_embu_male$identifier)
  
  farmer_type <- c("male farmers")
  all_var <- Data_embu_male %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_male_farmers <- Data_embu_male %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_embu_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_embu_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_embu_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_embu_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_male_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_embu_male) %in% i)){
      
      frequencies <- Data_embu_male %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_male) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "category", "freq","%")
  
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_male_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_embu_male) %in% j)){
      
      frequencies <- Data_embu_male %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_male) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "category", "freq","%")
  ##  ---- _______ ----
  ##  ---- 4) COMBINE GENDER  ----
  ## ---- a) numerical descriptives ----
  numerical_descriptives_by_gender <- rbind(numerical_descriptives_female_farmers, numerical_descriptives_male_farmers)
  numerical_descriptives_by_gender <- numerical_descriptives_by_gender %>%
    arrange(variable, gender)
  
  ## ---- b) single categorical descriptives ----
  single_categorical_descriptives_by_gender <- rbind(single_categorical_descriptives_female_farmers, single_categorical_descriptives_male_farmers)
  single_categorical_descriptives_by_gender <- single_categorical_descriptives_by_gender %>%
    arrange(variable, gender)
  
  ## ---- c) multiple categorical descriptives ----
  multiple_categorical_descriptives_by_gender <- rbind(multiple_categorical_descriptives_female_farmers, multiple_categorical_descriptives_male_farmers)
  multiple_categorical_descriptives_by_gender <- multiple_categorical_descriptives_by_gender %>%
    arrange(variable, gender)
  
 
  ## ---- ___ ----
  ## ---- all farmers - Combine all inputs in one excel ----
  
  #Check if applicable, this change is necessary to upload Data_embu to the portal.
  
  sets <- list(
    "Codebook" = survey_questions,
    "Cleaned Data_embu" = Data_embu,
    "Raw Data_embu (anonymised)" = Data_embu_raw, 
    "Num. desc. all farmers" = numerical_descriptives_all_farmers,
    "Cat. desc. single all farmers" = single_categorical_descriptives_all_farmers,
    "Cat. desc. multi all farmers" = multiple_categorical_descriptives_all_farmers,
    "Num. desc. by gender" = numerical_descriptives_by_gender,
    "Cat. desc. single by gender" = single_categorical_descriptives_by_gender,
    "Cat. desc. multi by gender" = multiple_categorical_descriptives_by_gender,
    "Net promoter score" = NSP)
  
  write.xlsx (sets, file = (paste0(data_delivery_embu)))
  
  ##  ---- _______ ----
  ##  ---- KIRINYAGA DATA ----  
  ##  ---- Summary statistics ----    
  ##  ---- 1) ALL FARMERS ----
  Data_kirinyaga <- Data %>%
    filter(pi_location_cascade_first_level=="Kirinyaga")
  
  Data_kirinyaga_raw<- read_excel((paste0(data_filename))) %>%
    filter(pi_location_cascade_Level_1 == "Kirinyaga") %>%
    select(-starts_with("pi_"), -name_of_farmer, -mobile_number_farmer) 
  
  farmer_type <- c("all farmers")
  all_var <- Data_kirinyaga %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_all_farmers <- Data_kirinyaga %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_kirinyaga %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_kirinyaga %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_kirinyaga %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_kirinyaga %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    select("variable", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_all_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_kirinyaga) %in% i)){
      
      frequencies <- Data_kirinyaga %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_participants_raw) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_all_farmers <- single_categorical_descriptives_all_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_all_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_kirinyaga) %in% j)){
      
      frequencies <- Data_kirinyaga %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_participants_raw) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_all_farmers <- multiple_categorical_descriptives_all_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  ##  ---- _______ ----
  ##  ---- 2) FEMALE FARMERS ----
  
  Data_kirinyaga_female <- Data_kirinyaga %>%
    filter(hh_farmer_gender == "female")
  
  nr_female <- length(Data_kirinyaga_female$identifier)
  
  farmer_type <- c("female farmers")
  all_var <- Data_kirinyaga_female %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_female_farmers <- Data_kirinyaga_female %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_kirinyaga_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_kirinyaga_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_kirinyaga_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_kirinyaga_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_female_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_kirinyaga_female) %in% i)){
      
      frequencies <- Data_kirinyaga_female %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_female) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "category", "freq","%")
  
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_female_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_kirinyaga_female) %in% j)){
      
      frequencies <- Data_kirinyaga_female %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_female) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "category", "freq","%")
  
  ##  ---- _______ ----
  ##  ---- 3) MALE FARMERS ----
  
  Data_kirinyaga_male <- Data_kirinyaga %>%
    filter(hh_farmer_gender == "male")
  
  nr_male <- length(Data_kirinyaga_male$identifier)
  
  farmer_type <- c("male farmers")
  all_var <- Data_kirinyaga_male %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_male_farmers <- Data_kirinyaga_male %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_kirinyaga_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_kirinyaga_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_kirinyaga_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_kirinyaga_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_male_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_kirinyaga_male) %in% i)){
      
      frequencies <- Data_kirinyaga_male %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_male) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "category", "freq","%")
  
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_male_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_kirinyaga_male) %in% j)){
      
      frequencies <- Data_kirinyaga_male %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_male) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "category", "freq","%")
  ##  ---- _______ ----
  ##  ---- 4) COMBINE GENDER  ----
  ## ---- a) numerical descriptives ----
  numerical_descriptives_by_gender <- rbind(numerical_descriptives_female_farmers, numerical_descriptives_male_farmers)
  numerical_descriptives_by_gender <- numerical_descriptives_by_gender %>%
    arrange(variable, gender)
  
  ## ---- b) single categorical descriptives ----
  single_categorical_descriptives_by_gender <- rbind(single_categorical_descriptives_female_farmers, single_categorical_descriptives_male_farmers)
  single_categorical_descriptives_by_gender <- single_categorical_descriptives_by_gender %>%
    arrange(variable, gender)
  
  ## ---- c) multiple categorical descriptives ----
  multiple_categorical_descriptives_by_gender <- rbind(multiple_categorical_descriptives_female_farmers, multiple_categorical_descriptives_male_farmers)
  multiple_categorical_descriptives_by_gender <- multiple_categorical_descriptives_by_gender %>%
    arrange(variable, gender)
  
 
  
  ## ---- ___ ----
  ## ---- all farmers - Combine all inputs in one excel ----
  
  #Check if applicable, this change is necessary to upload data to the portal.
  
  sets <- list(
    "Codebook" = survey_questions,
    "Cleaned Data" = Data_kirinyaga,
    "Raw Data (anonymised)" = Data_kirinyaga_raw, 
    "Num. desc. all farmers" = numerical_descriptives_all_farmers,
    "Cat. desc. single all farmers" = single_categorical_descriptives_all_farmers,
    "Cat. desc. multi all farmers" = multiple_categorical_descriptives_all_farmers,
    "Num. desc. by gender" = numerical_descriptives_by_gender,
    "Cat. desc. single by gender" = single_categorical_descriptives_by_gender,
    "Cat. desc. multi by gender" = multiple_categorical_descriptives_by_gender,
    "Net promoter score" = NSP)
  
  write.xlsx (sets, file = (paste0(data_delivery_kirinyaga)))
  
  
  #
  ##  ---- _______ ----
  ##  ---- NANDI DATA ----  
  ##  ---- Summary statistics ----    
  ##  ---- 1) ALL FARMERS ----
  Data_nandi <- Data %>%
    filter(pi_location_cascade_first_level=="nandi")
  
  Data_nandi_raw<- read_excel((paste0(data_filename))) %>%
    filter(pi_location_cascade_Level_1 == "Nandi") %>%
    select(-starts_with("pi_"), -name_of_farmer, -mobile_number_farmer) 
  
  farmer_type <- c("all farmers")
  all_var <- Data_nandi %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_all_farmers <- Data_nandi %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_nandi %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_nandi %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_nandi %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_nandi %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    select("variable", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_all_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_nandi) %in% i)){
      
      frequencies <- Data_nandi %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_participants_raw) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_all_farmers <- single_categorical_descriptives_all_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_all_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_nandi) %in% j)){
      
      frequencies <- Data_nandi %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_participants_raw) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_all_farmers <- multiple_categorical_descriptives_all_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  ##  ---- _______ ----
  ##  ---- 2) FEMALE FARMERS ----
  
  Data_nandi_female <- Data_nandi %>%
    filter(hh_farmer_gender == "female")
  
  nr_female <- length(Data_nandi_female$identifier)
  
  farmer_type <- c("female farmers")
  all_var <- Data_nandi_female %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_female_farmers <- Data_nandi_female %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_nandi_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_nandi_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_nandi_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_nandi_female %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_female_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_nandi_female) %in% i)){
      
      frequencies <- Data_nandi_female %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_female) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  single_categorical_descriptives_female_farmers <- single_categorical_descriptives_female_farmers %>% 
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "category", "freq","%")
  
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_female_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_nandi_female) %in% j)){
      
      frequencies <- Data_nandi_female %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_female) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  multiple_categorical_descriptives_female_farmers <- multiple_categorical_descriptives_female_farmers %>% 
    mutate(gender = "Female") %>%
    mutate(n=nr_female) %>%
    select("variable","gender","n", "category", "freq","%")
  
  ##  ---- _______ ----
  ##  ---- 3) MALE FARMERS ----
  
  Data_nandi_male <- Data_nandi %>%
    filter(hh_farmer_gender == "male")
  
  nr_male <- length(Data_nandi_male$identifier)
  
  farmer_type <- c("male farmers")
  all_var <- Data_nandi_male %>% 
    filter(`repeat no` == 1) %>%
    select(`repeat no`) %>% 
    table() %>% 
    melt(c("farmer type"), value.name="n") %>%
    mutate(`farmer type` = farmer_type)
  
  ## ---- a) numerical descriptives ----
  numerical_descriptives_male_farmers <- Data_nandi_male %>%
    select_if(is.numeric) %>%
    summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
    melt(value.name="mean") %>%
    left_join(
      Data_nandi_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
        melt(value.name="sd")) %>%
    left_join(
      Data_nandi_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(min(., na.rm = TRUE))) %>%
        melt(value.name="min")
    ) %>% 
    left_join(
      Data_nandi_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
        melt(value.name="max")
    ) %>% 
    left_join(
      Data_nandi_male %>% 
        select_if(is.numeric) %>%
        summarise_each(funs(count_n))  %>%
        melt(value.name="freq")
    ) %>%
    left_join(survey_questions) %>%
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "freq", "mean","sd", "min","max") 
  
  ## ---- b) single categorical descriptives ----
  
  single_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(is.na(multiple)) %>% 
    pull(variable)
  
  single_categorical_descriptives_male_farmers <- all_var
  
  for(i in single_mc){
    
    if(any(names(Data_nandi_male) %in% i)){
      
      frequencies <- Data_nandi_male %>% 
        filter(`repeat no`==1) %>%
        select(all_of(i)) %>%       
        table() %>% 
        melt(c("category"),value.name = "freq") %>%
        mutate(n = nr_male) %>%
        mutate(`farmer type` = farmer_type) %>%
        mutate("variable" = i) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  single_categorical_descriptives_male_farmers <- single_categorical_descriptives_male_farmers %>% 
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "category", "freq","%")
  
  
  ## ---- c) multiple categorical descriptives ----
  multiple_mc <- survey_questions %>% 
    filter(type=="option") %>% 
    filter(multiple == "yes") %>%
    pull(variable)
  
  multiple_categorical_descriptives_male_farmers <- all_var
  
  for(j in multiple_mc){
    
    if(any(names(Data_nandi_male) %in% j)){
      
      frequencies <- Data_nandi_male %>% 
        select(all_of(j)) %>% 
        cSplit(j, "|") %>%
        gather(key, value) %>%
        select(-key) %>% table() %>%
        melt(c("category"), value.name="freq") %>%
        mutate(n = nr_male) %>%
        mutate("variable" = j) %>%
        left_join(all_var) %>%
        mutate("%" = round(freq/n,2)*100) %>%
        mutate(category = as.character(category))
      
      multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
        full_join(frequencies) %>%
        select(variable, `farmer type`, n, category, freq, "%")
      
    }
  }
  
  multiple_categorical_descriptives_male_farmers <- multiple_categorical_descriptives_male_farmers %>% 
    mutate(gender = "Male") %>%
    mutate(n=nr_male) %>%
    select("variable","gender","n", "category", "freq","%")
  ##  ---- _______ ----
  ##  ---- 4) COMBINE GENDER  ----
  ## ---- a) numerical descriptives ----
  numerical_descriptives_by_gender <- rbind(numerical_descriptives_female_farmers, numerical_descriptives_male_farmers)
  numerical_descriptives_by_gender <- numerical_descriptives_by_gender %>%
    arrange(variable, gender)
  
  ## ---- b) single categorical descriptives ----
  single_categorical_descriptives_by_gender <- rbind(single_categorical_descriptives_female_farmers, single_categorical_descriptives_male_farmers)
  single_categorical_descriptives_by_gender <- single_categorical_descriptives_by_gender %>%
    arrange(variable, gender)
  
  ## ---- c) multiple categorical descriptives ----
  multiple_categorical_descriptives_by_gender <- rbind(multiple_categorical_descriptives_female_farmers, multiple_categorical_descriptives_male_farmers)
  multiple_categorical_descriptives_by_gender <- multiple_categorical_descriptives_by_gender %>%
    arrange(variable, gender)
  
  
  # Get personal information sheet (this piece of code was created in 2024 as IDH requested the PI file that was not generated for this case (or were missign))
  
  Data_raw <- read_excel(data_filename)
  
  Data_raw <- Data_raw %>%
    select(-contains("--option--")) %>%
    rename_all(funs(tolower)) %>%
    filter(`repeat no`==1) %>%
    select(identifier,
           name_of_farmer,
           mobile_number_farmer,
           contains("pi_")) #-contains("ppi")) 
  
  
  pi_info<- list("Personal information" = Data_raw)
  
  write.xlsx(pi_info, file=pi_filename) #pi_filename was created in the beginning of the script
  
  
  ## ---- ___ ----
  ## ---- all farmers - Combine all inputs in one excel ----
  
  #Check if applicable, this change is necessary to upload Data to the portal.
  
  sets <- list(
    "Codebook" = survey_questions,
    "Cleaned Data" = Data_nandi,
    "Raw Data (anonymised)" = Data_nandi_raw, 
    "Num. desc. all farmers" = numerical_descriptives_all_farmers,
    "Cat. desc. single all farmers" = single_categorical_descriptives_all_farmers,
    "Cat. desc. multi all farmers" = multiple_categorical_descriptives_all_farmers,
    "Num. desc. by gender" = numerical_descriptives_by_gender,
    "Cat. desc. single by gender" = single_categorical_descriptives_by_gender,
    "Cat. desc. multi by gender" = multiple_categorical_descriptives_by_gender,
    "Net promoter score" = NSP)
  
  write.xlsx (sets, file = (paste0(data_delivery_nandi)))
  
  
  #