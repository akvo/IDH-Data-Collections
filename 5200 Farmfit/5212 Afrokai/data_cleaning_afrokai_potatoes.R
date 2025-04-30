##  ---- Preps ----
# Data delivery bare bones

# Packages used
library(here); library(readxl); library(openxlsx);
library(plyr); library(dplyr); library(tidyr); library(tidylog); library(tidyselect)
library(stringr); library(data.table); library(reshape2)
library(zoo);library(splitstackshape) ;library(tidyverse)

setwd("/Users/ineslesire/Documents/GitHub/Analytics-Guild/1. Descriptive Analytics/IDH/5200 Farmfit/5212 Afrokai")
##  ---- Describe datafiles ----
##DOWNLOAD THESE ALL TOGETHER IN A FOLDER
pi_filename <- "pi_2022_afrokai_potatoes.xlsx"
data_filename <- "06062022_afrokai_potatoes_raw_data.xlsx"
survey_filename <- "survey_afrokai_potatoes.xlsx"
data_delivery <- "2022_afrokai_anom_potatoes.xlsx"
question_library <- read_excel("question library format v3.1.1.xlsx",
                               sheet = "Full Survey", skip = 2) 
vars_transformed <- read_excel("variables with transformation.xlsx") 
household_demographics  <- read_excel("household_demographics_var.xlsx") 
vars_dashboard <- read_excel("variable names input dashboard.xlsx") 

##  ---- Provide case information ----
case = "5212 Afrokai - Potatoes farmers"
sdm_crop = "Potatoes"
country = "Uganda"

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
Data_raw <- read_excel(data_filename)

Data_raw <- Data_raw %>%
  select(-starts_with("pi_"), -name_of_farmer, -mobile_number_farmer)


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
            `Submitter`, 
            `Form version`)) %>%
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
    mutate(f_harvest_num = as.numeric(f_harvest_num)) %>%
    ## In many cases, the crop has been added to this variable name. Variable will be f_[CROP]_harvest_num. See example below for a cocoa case.
    #mutate(f_cocoa_harvest_num = as.numeric(f_cocoa_harvest_num)) %>% 
  
  # Remove all "other" option text ("other, please specify")
  # mutate_each(funs(str_remove(., "other, please specify"))) %>%
  mutate_if(is.character, funs(gsub("other, please specify", NA,.))) %>% 
  mutate_if(is.character, funs(gsub("\\|$","",.))) %>% 
  
  # Remove trailing spaces
  mutate_if(is.character, str_trim) %>% 
  
  # Remove punctuation and special characters - EXCEPT for "|"
  mutate_if(is.character, funs(gsub("[^\\|[:^punct:]]", "", ., perl=T))) 
  

  # FARM SIZE
  Data <- Data %>%
  unite("f_unit_land", c(f_unit_land, f_unit_land_other2), 
        na.rm = TRUE, remove = FALSE, sep=" ") %>%
    
 
  # Remove 1 observation that was use for testing:
  filter(identifier != "xp5efxrfxvsv") %>%
    
  #Remove variable about revenues from potatoes that start with f_focus (kept in by accident)
  select(-f_focus_rev_timeperiod,
         -f_focus_quant_prod,
         -f_focus_measurement_prod,
         -f_focus_quant_sold,
         -f_focus_measurement_sold,
         -f_focus_price,
         -f_focus_own_consumption_yn,
         -f_focus_own_consumption,
         -f_focus_own_consumption_measurement,
         -f_focus_lost_yn,
         -f_focus_quant_lost,
         -f_focus_measurement_lost,
         -f_potatoes_measurement_lost_other2,
         -f_potatoes_measurement_prod_other2, 
         -f_potatoes_measurement_sold_other2,
         -f_potatoes_own_consumption_measurement_other2) 
  
  survey_questions <- survey_questions %>%
    filter(variable != "f_focus_rev_timeperiod",
           variable != "f_focus_quant_prod",
           variable != "f_focus_measurement_prod",
           variable != "f_focus_quant_sold",
           variable != "f_focus_measurement_sold",
           variable != "f_focus_price",
           variable != "f_focus_own_consumption_yn",
           variable != "f_focus_own_consumption",
           variable != "f_focus_own_consumption_measurement",
           variable != "f_focus_lost_yn",
           variable != "f_focus_quant_lost",
           variable != "f_focus_measurement_lost") 
    
    Data <- Data %>%
    
  #Rename variable names in data and codebook - delete rows that do not apply
    rename(f_focus_rev_timeperiod = f_potatoes_rev_timeperiod,
           f_focus_quant_prod = f_potatoes_quant_prod,
           f_focus_measurement_prod = f_potatoes_measurement_prod,
           f_focus_quant_sold = f_potatoes_quant_sold,
           f_focus_measurement_sold = f_potatoes_measurement_sold,
           f_focus_price = f_potatoes_price,
           f_focus_own_consumption_yn = f_potatoes_own_consumption_yn,
           f_focus_own_consumption = f_potatoes_own_consumption,
           f_focus_own_consumption_measurement = f_potatoes_own_consumption_measurement,
           f_focus_lost_yn = f_potatoes_lost_yn,
           f_focus_quant_lost = f_potatoes_quant_lost,
           f_focus_measurement_lost = f_potatoes_measurement_lost) 
  
  ##Make sure that each variable you rename in the data, is also renamed in the codebook
  survey_questions <- survey_questions %>%
    mutate(variable = ifelse(variable == "f_potatoes_rev_timeperiod", "f_focus_rev_timeperiod", variable),
           variable = ifelse(variable == "f_potatoes_quant_prod", "f_focus_quant_prod", variable),
           variable = ifelse(variable == "f_potatoes_measurement_prod", "f_focus_measurement_prod", variable),
           variable = ifelse(variable == "f_potatoes_quant_sold", "f_focus_quant_sold", variable),
           variable = ifelse(variable == "f_potatoes_measurement_sold", "f_focus_measurement_sold", variable),
           variable = ifelse(variable == "f_potatoes_price", "f_focus_price", variable),
           variable = ifelse(variable == "f_potatoes_own_consumption_yn", "f_focus_own_consumption_yn", variable),
           variable = ifelse(variable == "f_potatoes_own_consumption", "f_focus_own_consumption", variable),
           variable = ifelse(variable == "f_potatoes_own_consumption_measurement", "f_focus_own_consumption_measurement", variable),
           variable = ifelse(variable == "f_potatoes_lost_yn", "f_focus_lost_yn", variable),
           variable = ifelse(variable == "f_potatoes_quant_lost", "f_focus_quant_lost", variable),
           variable = ifelse(variable == "f_potatoes_measurement_lost", "f_focus_measurement_lost", variable))
  
 

    # Context specific adjustments
    # Check for obvious outliers first by running the full code except for running the outlier function below. 
    # Check with the data collection lead why certain numbers are odd. 
    #You can use the output file "productivity_check" at the end of the page to check the key variables.
    Data <- Data %>%
      fill(pi_location_cascade_sub_county) %>%
      mutate(f_focus_price = ifelse(f_focus_price > 1800 & pi_location_cascade_sub_county == "harugongo" |
                                      pi_location_cascade_sub_county == "kichwamba", f_focus_price/100, f_focus_price),
             f_focus_price = ifelse(f_focus_price == 10000 & pi_location_cascade_sub_county == "kabende", 100000, f_focus_price),
             f_focus_price = ifelse(pi_location_cascade_sub_county == "kabende", f_focus_price/120, f_focus_price),
             f_focus_price = ifelse(pi_location_cascade_sub_county == "karambi", f_focus_price/100, f_focus_price))
             
  
    #Identify numerical columns again because of change in names above
    numerical_columns <- survey_questions %>%
    filter(type == "number") %>%
    select("variable") %>% pull()
    
    # OUTLIERS:
    # Collect the different variables, numerical questions:
    Data <- Data %>%
    mutate_at(vars(numerical_columns), funs(outlier_detection)) %>%
    
    # All variables that are 9999/"i don't know" or 9998/"i prefer not to say" are set to NA
    mutate_if(is.numeric, list(~na_if(., 9999))) %>%
    mutate_if(is.numeric, list(~na_if(., 9998))) %>%
    mutate_if(is.numeric, list(~na_if(., 9997))) %>%
    mutate_if(is.character, list(~na_if(., "i don't know"))) %>%
    mutate_if(is.character, list(~na_if(., "i prefer not to say"))) %>%
      
      ##Check what the common meausurement unit for land is and adjust the following section accordingly. 
      ##We need a farm size in acres for the total farm size, focus crop, other main crop 1 and othermaincrop 2.
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
      mutate(cal_hh_farmer_age = 2022 - hh_farmer_birthyear,
             cal_hh_head_age = 2022-hh_head_birthyear,
             cal_hh_member_age = 2022-hh_member_birthyear) #update to current year, check whether applicable, not always included.
      
  
  
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
          ifelse(f_focus_measurement_prod %in% c("KG","kg","kilo","kgs","kilogram", "kilograms kg"), 1, 0))) %>%
 
      ##Measurement unit for sales
      mutate(cal_focus_measurement_sold := ifelse(
          grepl("[[:digit:]]", f_focus_measurement_sold), 
          extract_numeric(f_focus_measurement_sold), 
          ifelse(f_focus_measurement_sold %in% c("KG","kg","kilo","kgs","kilogram", "kilograms kg"), 1, 0))) %>%       
  
     # measurement unit for lost
    mutate(cal_focus_measurement_lost := ifelse(
      grepl("[[:digit:]]", f_focus_measurement_lost), 
      extract_numeric(f_focus_measurement_lost), 
      ifelse(f_focus_measurement_lost %in% c("KG","kg","kilo","kgs","kilogram", "kilograms kg"), 1, 0))) %>% 
  
    #measurement unit for own consumption
    ##!! REMOVE IF NOT APPLICABLE
    mutate(cal_focus_measurement_own_consumption := ifelse(
      grepl("[[:digit:]]", f_focus_own_consumption_measurement), 
      extract_numeric(f_focus_own_consumption_measurement), 
      ifelse(f_focus_own_consumption_measurement %in% c("KG","kg","kilo","kgs","kilogram", "kilograms kg"), 1, 0))) 
  
   
##  ---- 2) Prep: Calculate quantities  ----
    ##Calculate the quantities
    ##2a) Standard case - USE APPROACH 2B IF YOU HAVE A COFFEE CASE ----
    Data <- Data %>%
    mutate(cal_focus_quant_prod_kg = f_focus_quant_prod * cal_focus_measurement_prod) %>%
    mutate(cal_focus_quant_sold_kg = f_focus_quant_sold * cal_focus_measurement_sold) %>%
    mutate(cal_focus_quant_lost_kg = f_focus_quant_lost * cal_focus_measurement_lost) %>%
    mutate(cal_focus_quant_own_consumption_kg = f_focus_own_consumption * cal_focus_measurement_own_consumption) %>%

    mutate(cal_focus_price= f_focus_price)  
  
 
    
  ##  ---- 3) Calc: Focus crop income  ----  
   
   ##  ---- 3a.1) Calc: Focus crop revenue - STANDARD ----  
   Data <- Data %>%
     mutate(cal_focus_revenue = cal_focus_quant_sold_kg * cal_focus_price)  #Delete if type of coffee is not used in survey
   
   ##  ---- X) STANDARD APPROACH Inbetween, adjust for repeated question groups (IF APPLICABLE)  ----  
    ### REPLACE OR ADD ROWS FOR COFFEE CASES
    ## For some variables you need to add _robusta, _arabica, _hybridimproved, _liberica, _other -> check above which variables you have used.
    ## cal_focus_quant_prod_kg, cal_focus_quant_sold_kg,cal_focus_quant_lost_kg,cal_focus_revenue
    ##Adress values for those that have multiple seasons
    Data <- Data %>%
      #Identify duplicates
      group_by(identifier, f_focus_rev_timeperiod) %>% mutate(count_dup = n()) %>%
      mutate(count_dup = ifelse(is.na(f_focus_rev_timeperiod), NA, count_dup)) %>% #check whether we don't make mistakes here. For some cases farmer enter twice wrongly, in other cases entering same values for 2 seasons is ok.
      #Identify those with multiple seasons
      group_by(identifier, f_focus_rev_timeperiod) %>% mutate(count_seasons = n()) %>%
      mutate(count_seasons = ifelse(is.na(f_focus_rev_timeperiod) | count_seasons > 1, NA, count_seasons)) %>%
    
    
  
    #Sum the quantities for those that entered data for multiple seasons
    # For coffee case, replicate this piece of code for each coffee type 
      group_by(identifier, count_seasons) %>%
      mutate(cal_focus_quant_prod_kg =  sum(cal_focus_quant_prod_kg)) %>%
      mutate(cal_focus_quant_sold_kg =  sum(cal_focus_quant_sold_kg)) %>%
      mutate(cal_focus_quant_own_consumption_kg =  sum(cal_focus_quant_own_consumption_kg)) %>%
      mutate(cal_focus_quant_lost_kg =  sum(cal_focus_quant_lost_kg)) %>%
      mutate(cal_focus_revenue = sum(cal_focus_revenue)) %>%
    
    #Ensure we have 1 row of data for each farmer
    # For coffee case, replicate this piece of code for each coffee type 
      mutate(cal_focus_quant_prod_kg = ifelse(`repeat no` >1 , NA, cal_focus_quant_prod_kg)) %>%
      mutate(cal_focus_quant_sold_kg = ifelse(`repeat no` >1  , NA, cal_focus_quant_sold_kg)) %>%
      mutate(cal_focus_quant_own_consumption_kg = ifelse(`repeat no` >1  , NA, cal_focus_quant_own_consumption_kg )) %>%
      mutate(cal_focus_quant_lost_kg = ifelse(`repeat no` >1 , NA, cal_focus_quant_lost_kg)) %>%
      mutate(cal_focus_revenue = ifelse (`repeat no`>1, NA, cal_focus_revenue)) %>%
      ungroup() %>%
      select(-count_dup, -count_seasons)
  
  ##  ---- 3b) Calc: Revenues  ----   
  ##  ---- 3c.1) STANDARD CASE  ---- 
  ##Calculate revenue from focus crop
  Data <- Data %>%
  mutate(cal_focus_revenue= ifelse(is.na(cal_focus_revenue), 0, cal_focus_revenue))  
 
  ##  ---- 3c) Calc: Productivity  ----  
  ##Calculate productivity
  Data <- Data %>%
  mutate(cal_focus_productivity_acre = cal_focus_quant_prod_kg/f_focus_crop_size_acre) 

  ##  ---- 3d) Calc: Labour costs  ----  
  #Prepare 
  labour_types <- Data %>% 
    cSplit("f_crop_labour_types", "|") %>% 
    select(starts_with("f_crop_labour_types")) %>% 
    gather(key, value) %>% filter(!is.na(value)) %>% 
    select(value) %>% unique() %>% pull()
  
  survey_labour_types <- substr(gsub("\\s|-", "", labour_types), 1, 5)
  
  ##!!!!!!Adjust labour_options per case. Check in the survey which activities are surveyed.
  labour_options <- c("landprep", 
                      #"nurserymaint", 
                      "cropmaint", 
                      "irrigation", 
                      "fertilizerapp", 
                      #"compostprep", 
                      "harvesting", 
                      "postharvest")
                      #"otheractivity")
  
  Data <- Data %>%
    mutate(cal_labour_cropmaint_nrhours = f_labour_cropmaint_nrhours * f_labour_cropmaint_nrdays,
           cal_labour_cropmaint_hours_costs = cal_labour_cropmaint_nrhours * f_labour_cropmaint_paymentperhour * f_labour_cropmaint_nrhiredpeople)
  
  #Calculation of labour costs
  for(j in survey_labour_types){
    
    if(any(startsWith(labour_options, j))){
      
      var_name <- paste0("_labour_", labour_options[startsWith(labour_options, j)])
      
      Data <- Data %>% 
        mutate_at(vars(contains("_othertype")), as.numeric) %>%
        mutate_at(vars(contains("_othercosts")), as.numeric) %>%     
        
        #mutate("cal{var_name}_nrhours" := Data %>% 
        #         select(!!sym(paste0("f", var_name, "_nrhours")),
        #                !!sym(paste0("f", var_name, "_nrdays"))) %>%
        #         apply(.,1,prod,na.rm=FALSE)) %>%
        #mutate("f{var_name}_nrdays" := ifelse(!is.na(!!sym(paste0("f", var_name, "_nrhours"))), NA,
        #                                      !!sym(paste0("f", var_name, "_nrdays"))))  %>%
        
        ##Calculate wage costs for those that paid per hour
        #mutate("cal{var_name}_hour_costs" := Data %>% 
        #         select(!!sym(paste0("cal", var_name, "_nrhours")),
        #                !!sym(paste0("f", var_name, "_paymentperhour")),
        #                !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
        #         apply(.,1,prod)) %>%
        
        ##Calculate wage costs for those that paid per day
        mutate("cal{var_name}_day_costs" := Data %>% 
                 select(!!sym(paste0("f", var_name, "_nrdays")),
                        !!sym(paste0("f", var_name, "_paymentperday")),
                        !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
                 apply(.,1,prod)) %>%
        
        ##Calculate wage costs for those that paid per months
        mutate("cal{var_name}_month_costs" := Data %>% 
                 select(!!sym(paste0("f", var_name, "_nrmonths")),
                        !!sym(paste0("f", var_name, "_paymentpermonth")),
                        !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
                 apply(.,1,prod)) %>%
        
        unite(!!paste0("cal", var_name, "_part_1_total_cost"),
              !!sym(paste0("cal", var_name, "_day_costs")):!!sym(paste0("cal", var_name, "_month_costs")), 
              sep="|", remove=FALSE, na.rm=FALSE) %>%  
        mutate("cal{var_name}_part_1_total_cost" := extract_numeric( !!sym(paste0("cal", var_name, "_part_1_total_cost")))) %>%
        
        
      #Calculate total cost 
      #for those that  reported wage using "other method"
      mutate("f{var_name}_rememberwage_othercosts" := as.numeric(
        !!sym(paste0("f", var_name, "_rememberwage_othercosts")))) %>%
   
      mutate("cal{var_name}_part_2_total_cost" := Data %>% 
               select(!!sym(paste0("f", var_name, "_rememberwage_othercosts")),
                      !!sym(paste0("f", var_name, "_nrhiredpeople"))) %>%
               apply(., 1, prod))
      
    }
  }
  

    #Remove several variables
    Data <- Data %>%
      select(-contains("alltimeframes"), 
             #-ends_with("hour_costs"),
             -ends_with("day_costs"),
             -ends_with("month_costs"),
             -ends_with("activity_costs")) %>%
      mutate_at(vars(starts_with("cal_labour") & ends_with("_total_cost")), as.numeric) 

  # Combine costs of all types of labour
  Data <- Data %>% 
    mutate(cal_labour_cost = Data %>%
             select(starts_with("cal_labour") & ends_with("_total_cost")) %>% 
             rowSums(na.rm=TRUE)) %>%
    #Add the hours costs that we calculated before
    mutate(cal_labour_cropmaint_hours_costs = ifelse(is.na(cal_labour_cropmaint_hours_costs), 0, cal_labour_cropmaint_hours_costs))%>%
    mutate(cal_labour_cost = cal_labour_cost + cal_labour_cropmaint_hours_costs) %>%
    select(-ends_with("hours_costs"),
           -cal_labour_cropmaint_nrhours)
  
  Data <- Data %>%
    select(-contains("part_1_total_cost"),-contains("part_2_total_cost")) 
  
  ##  ---- X) Inbetween, adjust for repeated questions groups (IF APPLICABLE) ----  
  
            ##Adress values for those that have multiple seasons
            Data <- Data %>%
            #Identify duplicates
            group_by(identifier, f_crop_labour_timeperiod) %>% mutate(count_dup = n()) %>% #check whether we don't make mistakes here. For some cases farmer enter twice wrongly, in other cases entering same values for 2 seasons is ok.
            mutate(count_dup = ifelse(is.na(f_crop_labour_timeperiod), NA, count_dup)) %>%
            #Identify those with multiple seasons
            group_by(identifier, f_crop_labour_timeperiod) %>% mutate(count_seasons = n()) %>%
            mutate(count_seasons = ifelse(is.na(f_crop_labour_timeperiod) | count_seasons > 1, NA, count_seasons)) %>%
            
            #Replace values with 0 for those that entered the data twice
            mutate(`repeat no` = as.numeric(`repeat no`)) %>%
            mutate(cal_labour_cost = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_labour_cost)) %>%
          
            #Sum the values for those that entered data for multiple seasons
            group_by(identifier, count_seasons) %>%
            mutate(cal_labour_cost =  sum(cal_labour_cost)) %>%
            
            #Ensure we have 1 row of data for each farmer
            mutate(cal_labour_cost = ifelse(`repeat no` >1 , NA, cal_labour_cost)) %>%
            ungroup() %>%
            select(-count_dup, -count_seasons)
    
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
                group_by(identifier, f_inputs_timeperiod) %>% mutate(count_seasons = n()) %>%
                mutate(count_seasons = ifelse(is.na(f_inputs_timeperiod) | count_seasons > 1, NA, count_seasons)) %>%
                
                #Replace values with 0 for those that entered the data twice
                mutate(`repeat no` = as.numeric(`repeat no`)) %>%
                mutate(cal_inputs_costs = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_inputs_costs)) %>%
                
                #Sum the values for those that entered data for multiple seasons
                group_by(identifier, count_seasons) %>%
                mutate(cal_inputs_costs =  sum(cal_inputs_costs)) %>%
                
                #Ensure we have 1 row of data for each farmer
                mutate(cal_inputs_costs = ifelse(`repeat no` >1 , NA, cal_inputs_costs)) %>%
                ungroup() %>%
                select(-count_dup, -count_seasons)
  
  ##  ---- 3e) Calc: Total focus crop production costs ----                
  Data <- Data %>%
    mutate(cal_focus_cost = cal_labour_cost + cal_inputs_costs + f_transport) %>%

  ##  ---- 3f) Calc: Net-income focus crop ----                
  
  mutate(cal_focus_income = cal_focus_revenue - cal_focus_cost)

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
    mutate(cal_livestock_revenue = f_livestock_income_total) 
  
    #Approach 2
    #mutate(cal_livestock_revenue = Data %>% 
    #         select(contains("f_livestock_income"), -contains("type")) %>%
    #         rowSums(na.rm=TRUE)) %>%
    #mutate(cal_livestock_revenue= ifelse(is.na(cal_livestock_revenue), 0, cal_livestock_revenue)) %>%
    
    #Input costs
   # mutate(cal_livestock_inputs_cost = Data %>% 
  #           select(f_livestock_costs_fodderwater, 
   #                 f_livestock_costs_medics) %>%
  #           rowSums(na.rm=TRUE)) %>%
  #  mutate(cal_livestock_inputs_cost= ifelse(is.na(cal_livestock_inputs_cost), 0, cal_livestock_inputs_cost)) 

    #Labour cost
   # Data <- Data %>%
   #    mutate(cal_livestock_labour_cost = Data %>% 
  #             select(f_livestock_nr_hired_labourers, 
   #                   f_livestock_days_hiredlabour, 
    #                  f_livestock_wages_hiredlabour) %>%
    #           apply(., 1, prod, na.rm=FALSE)) %>%
    #  mutate(cal_livestock_labour_cost= ifelse(is.na(cal_livestock_labour_cost), 0, cal_livestock_labour_cost)) 

    # total cost
    #Data <- Data %>%
    #  mutate(cal_livestock_cost = Data %>% 
    #           select(cal_livestock_inputs_cost,
    #                  cal_livestock_labour_cost) %>%
    #           rowSums(na.rm=FALSE)) %>%
    #  #mutate(cal_livestock_cost= ifelse(is.na(cal_livestock_cost), 0, cal_livestock_cost)) %>%
      
    #net income
    #mutate(cal_livestock_income = cal_livestock_revenue - cal_livestock_cost)

  ##  ---- 6) Calc: Other farm costs ----                
    
  # EQUIPMENT
  Data <- Data %>%
    mutate(cal_equipment_costs = Data %>% 
             select(f_nonmech_equip_costs, f_mech_equip_costs, f_materials_other_costs) %>% 
             apply(.,1,sum, na.rm=TRUE)) %>%
  
  # GENERAL ON FARM COSTS
    mutate(cal_farm_other_costs = Data %>% 
             select(f_costs_land,
                    hh_loan_one_other_costs_interest,
                    hh_loan_one_agri_costs_interest,
                    hh_loan_multiple_largest_costs_interest,
                    hh_loan_multiple_agri_costs_interest) %>%
             apply(., 1, sum, na.rm=TRUE)) 
    
    ##  ---- 7) Calc: Off farm labour income----                
    
  Data <- Data %>%
    mutate(cal_off_farm_labour_income = Data %>% 
             select(f_nonfarm_enterpr_1_profit,
                    f_nonfarm_enterpr_2_profit,
                    f_nonfarm_enterpr_3_profit,
                    f_income_offfarmlabour_wage) %>%
             apply(., 1, sum, na.rm=TRUE)) 

    ##  ---- 8) Calc: Off farm non labour income----    
    Data <- Data %>% 
      mutate(cal_offfarm_non_labour_income = Data %>%
               select(starts_with("f_income_other_"), -contains("_type")) %>% 
               apply(.,1,sum, na.rm=TRUE)) 


    ##  ---- 9) Calc: Farm income and actual income----    
Data <- Data %>%
  mutate(cal_farm_income = cal_focus_income + cal_livestock_revenue + cal_other_crop_income - cal_equipment_costs - cal_farm_other_costs) %>%
  mutate(cal_actual_income = cal_focus_income + cal_other_crop_income + cal_livestock_revenue - 
           - cal_equipment_costs - cal_farm_other_costs + cal_off_farm_labour_income + cal_offfarm_non_labour_income) %>%
  mutate(cal_farm_income = ifelse(is.na(farmer_present)  , NA, cal_farm_income )) %>%
  mutate(cal_actual_income  = ifelse(is.na(farmer_present) , NA, cal_actual_income))

    ##  ---- ______----    
    ##  ---- Calculate net promotor score----    
    #NPS = (Promoters/total # farmers) - (Detractors/total # farmers)
    #This is calculated from the variable cs_recommendation:
    #“How likely is it that you would recommend [SDM company] to a friend or peer?”)
    
    #Not likely, somewhat likely, likely = detractor
    #Very likely = promoter
    
    hh_farmer_gender <- c("all farmers")
    
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
             hh_farmer_gender = hh_farmer_gender)
    
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
      mutate(nsp_gender = round((nr_promoters/nr_farmers) - (nr_detractors/nr_farmers),2))
    
    NSP <- full_join(NSP_total, NSP_by_gender)
    NSP <- NSP %>% select(hh_farmer_gender, nr_farmers, nr_promoters, nr_detractors,nr_passive, nsp_total, nsp_gender)
    
##  ---- ______----    
##  ---- Prepare anom clean data for further use----    
#Private information is deleted
#We need to keep the variable capturing the highest level of location (pi_location_cascade_first..) for the Farmfit portal. Check what variable that is for each case, this differs.
    #Rename it temporarily such that it is not deleted. Always rename it to pi_location_cascade_first_level
    Data <- Data %>%
          rename(location_cascade_region = pi_location_cascade_district) %>% 
          select(-pi_location_cascade_county_, 
                 -pi_location_cascade_sub_county,
      -c(starts_with("pi_"), -contains("county")),
      -c(name_of_farmer, mobile_number_farmer)) %>%
          rename(pi_location_cascade_first_level = location_cascade_region)
  
##  ---- _______ ----
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
  filter(variable != "f_labour_otheractivity_labour_type",
         variable != "f_labour_nurserymaint_labour_type",
         variable != "f_labour_compostprep_labour_type",
         variable != "f_coop_sustainability_standard_other",
         variable != "cs_negative_recommendation",
         section != "agent model farmers",
         variable != "hh_loan_one_other_purpose",
         variable != "hh_loan_mulitple_largest_purpose") %>%
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
##  ---- 2) BY GENDER ----
# Gender descriptive for aggregation
gender_var <- Data %>% 
  select(hh_farmer_gender) %>% 
  table() %>% 
  melt("hh_farmer_gender", value.name="n")

## ---- a) numerical descriptives ----

numerical_descriptives_by_gender <- Data %>%
  group_by(hh_farmer_gender) %>%
  select_if(is.numeric) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE),2))) %>%
  melt(value.name="mean") %>%
  left_join(
    Data %>% 
      group_by(hh_farmer_gender) %>%
      select_if(is.numeric) %>%
      summarise_each(funs(round(sd(., na.rm = TRUE),2))) %>%
      melt(value.name="sd")) %>%
  left_join(
    Data %>% 
      group_by(hh_farmer_gender) %>%
      select_if(is.numeric) %>%
      summarise_each(funs(min(., na.rm = TRUE))) %>%
      melt(value.name="min")
  ) %>% 
  left_join(
    Data %>% 
      group_by(hh_farmer_gender) %>%
      select_if(is.numeric) %>%
      summarise_each(funs(round(max(., na.rm = TRUE),2))) %>%
      melt(value.name="max")
  ) %>% 
  left_join(
    Data %>% 
      group_by(hh_farmer_gender) %>%
      select_if(is.numeric) %>%
      summarise_each(funs(count_n))  %>%
      melt(value.name="freq")
  ) %>%
  left_join(survey_questions) %>%
  left_join(gender_var) %>%
  select("variable","hh_farmer_gender","n", "freq", "mean","sd", "min","max") 


## ---- b) single categorical descriptives ----
single_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(is.na(multiple)) %>% 
  filter(variable != "hh_farmer_gender") %>%
  pull(variable)

single_categorical_descriptives_by_gender <- gender_var

for(i in single_mc){
  
  if(any(names(Data) %in% i)){
    
    frequencies <- Data %>% 
      select(hh_farmer_gender, all_of(i)) %>% 
      table() %>% 
      melt(c("hh_farmer_gender","category"), value.name="freq") %>%
      mutate("variable" = i) %>%
      left_join(gender_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    single_categorical_descriptives_by_gender <- single_categorical_descriptives_by_gender %>% 
      full_join(frequencies) %>%
      select(variable, hh_farmer_gender, n, category, freq, "%")
  }
}

## ---- c) multiple categorical descriptives ----
multiple_mc <- survey_questions %>% 
  filter(type=="option") %>% 
  filter(variable != "f_labour_otheractivity_labour_type",
         variable != "f_labour_nurserymaint_labour_type",
         variable != "f_labour_compostprep_labour_type",
         variable != "f_coop_sustainability_standard_other",
         variable != "cs_negative_recommendation",
         section != "agent model farmers",
         variable != "hh_loan_one_other_purpose",
         variable != "hh_loan_mulitple_largest_purpose") %>%
  
  filter(multiple == "yes") %>%
  pull(variable)

multiple_categorical_descriptives_by_gender <- gender_var
check <- as.data.frame(multiple_mc)
for(j in multiple_mc){
  
  if(any(names(Data) %in% j)){
    
    frequencies <- Data %>% 
      select(hh_farmer_gender, all_of(j)) %>% 
      cSplit(j, "|") %>%
      gather(key, value, -hh_farmer_gender) %>%
      select(-key) %>% table() %>%
      melt(c("hh_farmer_gender","category"), value.name="freq")   %>%
      mutate("variable" = j) %>%
      left_join(gender_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    multiple_categorical_descriptives_by_gender <- multiple_categorical_descriptives_by_gender %>% 
      full_join(frequencies) %>%
      select(variable, hh_farmer_gender, n, category, freq, "%")
  }
}


check <- as.data.frame(multiple_mc)
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

## Remove repeated rows for household members and create columns for each household member (max household number is 15)
## ---- delete the unnecessary --- only if household roster questions are asked, check also which ones are used
if("identifier" %in% colnames(Data)){     
  Data <- Data %>%
    group_by(identifier) %>%
    mutate(id = row_number())
  
  for (nr in c(1:15)){
    
    ### ---- delete the unnecessary --- only if household roster questions are asked, check also which ones are used
    hh_unpaidlabour_hhactivities_nr <- paste0("hh_unpaidlabour_hhactivities_", nr)
    hh_focuscrop_nr <- paste0("hh_focuscrop_", nr)
    hh_otherfarm_activities_nr <- paste0("hh_otherfarm_activities_", nr)
    hh_offarmlabour_activities_nr <- paste0("hh_offarmlabour_activities_", nr)
    
    Data <- Data %>%
      #Delete the rows that do not apply
      mutate(
        !!sym(hh_unpaidlabour_hhactivities_nr) := ifelse(id == nr, hh_unpaidlabour_hhactivities_hrs, NA),
        !!sym(hh_focuscrop_nr) := ifelse(id == nr, hh_focuscrop_hrs, NA),
        !!sym(hh_otherfarm_activities_nr) := ifelse(id == nr, hh_otherfarm_activities_hrs, NA),
        !!sym(hh_offarmlabour_activities_nr) := ifelse(id == nr, hh_offarmlabour_activities_hrs, NA),) %>%
      
      #Delete the rows that do not apply
      fill(!!sym(hh_unpaidlabour_hhactivities_nr)) %>%
      fill(!!sym(hh_unpaidlabour_hhactivities_nr), .direction = "up") %>%
      fill(!!sym(hh_focuscrop_nr)) %>%
      fill(!!sym(hh_focuscrop_nr), .direction = "up") %>%
      fill(!!sym(hh_otherfarm_activities_nr)) %>%
      fill(!!sym(hh_otherfarm_activities_nr), .direction = "up") %>%
      fill(!!sym(hh_offarmlabour_activities_nr)) %>%
      fill(!!sym(hh_offarmlabour_activities_nr), .direction = "up") %>%
      
      #Delete the rows that do not apply
      mutate(
        !!sym(hh_unpaidlabour_hhactivities_nr) := ifelse(id >1 , NA, !!sym(hh_unpaidlabour_hhactivities_nr) ),
        !!sym(hh_focuscrop_nr) := ifelse(id >1 , NA, !!sym(hh_focuscrop_nr) ),
        !!sym(hh_otherfarm_activities_nr) := ifelse(id >1 , NA, !!sym(hh_otherfarm_activities_nr) ),
        !!sym(hh_offarmlabour_activities_nr) := ifelse(id >1 , NA, !!sym(hh_offarmlabour_activities_nr) ))
    
  }
}

Data <- Data %>%
  filter(id == 1) %>%
  select(-id,
         -hh_unpaidlabour_hhactivities_hrs,
         -hh_focuscrop_hrs,
         -hh_otherfarm_activities_hrs,
         -hh_offarmlabour_activities_hrs)

  ## ---- ___ ----
  ## ---- Combine all inputs in one excel for sharing with IDH ----

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
  
  write.xlsx (sets, file = (paste0(data_delivery)), overwrite = TRUE)
  
  ## ---- Extract personal information ----
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
  
  ## ---- Check for productivity----
  ## TO MAKE THIS CODE WORK, KEEP THE SUBMITTER VARIABLE FOR THE TIME BEING.
  #For final delivery, this variable should be deleted
  productivity <- Data %>%
    mutate(focus_product_min_sold = cal_focus_quant_prod_kg - cal_focus_quant_sold_kg) %>%
    select(identifier,
           submitter,
           `submission date`,
           starts_with("pi_location_cascade"),
           f_unit_land,
           f_focus_crop_size,
           f_focus_crop_size_acre,
           f_focus_quant_prod,
           cal_focus_quant_prod_kg,
           f_focus_measurement_prod,
           cal_focus_productivity_acre,
           f_focus_quant_sold,
           cal_focus_quant_sold_kg,
           focus_product_min_sold,
           f_focus_price,
           cal_focus_price,
           cal_focus_revenue)
  
  write.xlsx (productivity, file = "productivity_check.xlsx", overwrite = TRUE)