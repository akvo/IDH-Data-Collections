##  ---- readdata ----
# Data delivery bare bones

# Packages used
library(here); library(readxl); library(openxlsx);
library(plyr); library(dplyr); library(tidyr); library(tidylog); library(tidyselect)
library(stringr); library(data.table); library(reshape2)
# library(knitr); library(Hmisc)
library(zoo);library(splitstackshape) 

here::i_am("Volumes/GoogleDrive/My Drive/04_data_analytics/RGL/DD_version2_JB.R")


data_filename <- "24092021_rgl_beans.xlsx"
survey_filename <- "survey_rgl_beans.xlsx"

# CROP
case = "RGL"
sdm_crop = "beans"
country = "Tanzania"
PPI = FALSE

# Output file names
output_file = paste0(Sys.Date(), "_", case, "_all")
anonymized_output_file = paste0(Sys.Date(), "_", case,"_anom")

# FUNCTIONS

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

# count values without NA
count_n <- function(x){sum(!is.na(x))}

# Read data from the data folder
Data <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/RGL",data_filename))



# Get original number of participants
nr_participants_raw <- length(unique(Data$Identifier))

# Read file with survey structure
survey_questions <- read_excel(
  here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/RGL", survey_filename),
  sheet ="Full Survey", skip=2)

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

## FLOW MISTAKE NUMERIC

# DATA ADJUSTMENTS

# Collect the different variables, numerical questions:
numerical_columns <- survey_questions %>%
  filter(type == "number") %>%
  select("variable") %>% pull()

# Changes to data set:
Data <- Data %>%
  
  # Set the date to date format
  mutate(`Submission Date` = as.Date(`Submission Date`, format="%d-%m-%y")) %>% 
  
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
  mutate(f_harvest_num = as.numeric(f_harvest_num)) %>% 
  
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
  mutate_if(is.character, list(~na_if(., "i prefer not to say"))) %>%
  
  # FARM SIZE
  unite("f_unit_land_no", c(f_unit_land_no, f_unit_land_no_other2), 
        na.rm = TRUE, remove = FALSE, sep=" ") %>%
  # mutate(f_unit_land = ifelse(f_unit_land == "yes", "acres", f_unit_land_no)) %>%
  mutate(f_unit_land_no == "acres",f_unit_land_no) %>%
  mutate(f_size_acre = ifelse(f_unit_land_no == "hectares", f_size*2.471, f_size)) %>%
  mutate(f_focus_crop_size_acre = ifelse(f_unit_land_no == "hectares", f_focus_crop_size*2.471, f_focus_crop_size)) %>%
  mutate(f_size_othermaincrop_1_acre = ifelse(f_unit_land_no == "hectares", 
                                         f_size_othermaincrop_1*2.471, 
                                         f_size_othermaincrop_1)) %>%
  mutate(f_size_othermaincrop_2_acre = ifelse(f_unit_land_no == "hectares", 
                                         f_size_othermaincrop_2*2.471, 
                                         f_size_othermaincrop_2)) %>%
  mutate(f_unit_land = f_unit_land_no) %>%
  
  # Extend farm size to determine productivity
  mutate(f_focus_crop_size_acre = na.locf(f_focus_crop_size_acre, na.rm = FALSE)) %>%
  
  # Age
  mutate(hh_farmer_age = ifelse(hh_farmer_age > 1000, 2021 - hh_farmer_age, hh_farmer_age)) %>%
  mutate(hh_head_age = ifelse(hh_head_age > 1000, 2021 - hh_head_age, hh_head_age)) %>%
  mutate(hh_member_age = ifelse(hh_member_age > 1000, 2021 - hh_member_age, hh_member_age))

# Get original number of participants
nr_participants_ic <- length(unique(Data$identifier))

### Check if outlier function has worked ####
# write.csv(Data, "data/checkoutliers.csv")

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

# PRODUCTION / SOLD / LOST / REVENUE (main crop)

# FOCUS CROP - GENERAL ####

# Data %>%

# if(sdm_crop %in% c("focus", "beans", "maize", "rice")){
  
  # prod_measurement <- extract_numeric(
  #   survey_questions[
  #     survey_questions$variable == "f_focus_measurement_prod", 
  #     survey_questions$variable == "f_focus_measurement_prod", 
  #     "question"])
  
  Data <- Data %>%
  # Extract the numeric values from the measurement units for quantity produced, sold, consumed and lost
  
      ##Measurement unit for production
      mutate(cal_focus_measurement_prod := ifelse(
          grepl("[[:digit:]]", f_focus_measurement_other), 
          extract_numeric(f_focus_measurement_other), 
          ifelse(f_focus_measurement_other %in% c("KG","kg","kilo","kgs","kilogram"), 1, 0))) %>%
 
  
      ##Measurement unit for sales
 
      mutate(cal_focus_measurement_sold := ifelse(
          grepl("[[:digit:]]", f_focus_measurement_sold), 
          extract_numeric(f_focus_measurement_sold), 
          ifelse(f_focus_measurement_sold %in% c("kg","kilo","kgs","kilogram"), 1, 0))) %>%       
      #Specific for RGL case:
      mutate(cal_focus_measurement_sold := ifelse(
                f_focus_measurement_sold == "bucket", 20,
                cal_focus_measurement_sold )) %>%
      mutate(cal_focus_measurement_sold := ifelse(
                f_focus_measurement_sold == "hajauza", 0,
                cal_focus_measurement_sold)) %>%
      
  
      # Price -> if unit of measurement doesn't seem to work
      # mutate("cal_{sdm_crop}_price" := ifelse(
      #   f_focus_price < 100,
      #   f_focus_price, f_focus_price/cal_focus_measurement_sold)) %>%
  
     # measurement unit for lost
    mutate(cal_focus_lost_measurement := ifelse(
      grepl("[[:digit:]]", f_focus_own_lost_measurement), 
      extract_numeric(f_focus_own_lost_measurement), 
      ifelse(f_focus_own_lost_measurement %in% c("kg","kilo","kgs","kilogram"), 1, 0))) %>% 
    #Specific for RGL case:
    mutate(cal_focus_measurement_sold := ifelse(
      f_focus_measurement_sold == "bucket", 20,
      cal_focus_measurement_sold )) %>%
    
    #measurement unit for own consumption
    mutate(cal_focus_lost_measurement := ifelse(
      grepl("[[:digit:]]", f_focus_own_consumption_measurement), 
      extract_numeric(f_focus_own_consumption_measurement), 
      ifelse(f_focus_own_consumption_measurement %in% c("kg","kilo","kgs","kilogram"), 1, 0))) %>%
  
    #Specific for RGL case:
    mutate(cal_focus_measurement_sold := ifelse(
      f_focus_measurement_sold == "bucket", 20,
      cal_focus_measurement_sold )) %>%
  
    ##Calculate the quantities
    mutate(cal_focus_quant_prod = f_focus_quant_prod * cal_focus_measurement_prod) %>%
    mutate(cal_focus_quant_sold = f_focus_quant_sold * cal_focus_measurement_sold) %>%
    mutate(cal_focus_quant_lost = f_focus_quant_lost * cal_focus_lost_measurement) %>%
    mutate(cal_focus_quant_own_consumption = f_focus_own_consumption * cal_focus_lost_measurement) %>%


    mutate(cal_focus_price= ifelse(is.na(f_focus_price), 0, f_focus_price/cal_focus_measurement_prod))  %>%
    mutate(cal_focus_revenue = cal_focus_quant_sold * cal_focus_price)

    ##Adress values for those that have multiple seasons
    Data <- Data %>%
      #select(identifier,`repeat no`, f_focus_rev_timeperiod,cal_focus_quant_prod, cal_focus_quant_sold, cal_focus_quant_own_consumption, cal_focus_quant_lost) %>%
      #Identify duplicates
      group_by(identifier, f_focus_rev_timeperiod) %>% mutate(count_dup = n()) %>%
      mutate(count_dup = ifelse(is.na(f_focus_rev_timeperiod), NA, count_dup)) %>%
      #Identify those with multiple seasons
      group_by(identifier, f_focus_rev_timeperiod) %>% mutate(count_seasons = n()) %>%
      mutate(count_seasons = ifelse(is.na(f_focus_rev_timeperiod) | count_seasons > 1, NA, count_seasons)) %>%
    
    #Replace values with 0 for those that entered the data twice
      mutate(repeat_no = as.numeric(`repeat no`)) %>%
      mutate(cal_focus_quant_prod = ifelse(repeat_no >1  & count_dup > 1, 0, cal_focus_quant_prod)) %>%
      mutate(cal_focus_quant_sold = ifelse(repeat_no >1  & count_dup > 1, 0, cal_focus_quant_sold)) %>%
      mutate(cal_focus_quant_own_consumption = ifelse(repeat_no >1  & count_dup > 1, 0, cal_focus_quant_own_consumption )) %>%
      mutate(cal_focus_quant_lost = ifelse(repeat_no >1  & count_dup > 1, 0, cal_focus_quant_lost)) %>%
      mutate(cal_focus_revenue = ifelse(repeat_no >1  & count_dup > 1, 0, cal_focus_revenue)) %>%
  
    
    #Sum the quantities for those that entered data for multiple seasons
      group_by(identifier, count_seasons) %>%
      mutate(cal_focus_quant_prod =  sum(cal_focus_quant_prod)) %>%
      mutate(cal_focus_quant_sold =  sum(cal_focus_quant_sold)) %>%
      mutate(cal_focus_quant_own_consumption =  sum(cal_focus_quant_own_consumption)) %>%
      mutate(cal_focus_quant_lost =  sum(cal_focus_quant_lost)) %>%
      mutate(cal_focus_revenue = sum(cal_focus_revenue)) %>%
      
      #Ensure we have 1 row of data for each farmer
      mutate(cal_focus_quant_prod = ifelse(repeat_no >1 , NA, cal_focus_quant_prod)) %>%
      mutate(cal_focus_quant_sold = ifelse(repeat_no >1  , NA, cal_focus_quant_sold)) %>%
      mutate(cal_focus_quant_own_consumption = ifelse(repeat_no >1  , NA, cal_focus_quant_own_consumption )) %>%
      mutate(cal_focus_quant_lost = ifelse(repeat_no >1 , NA, cal_focus_quant_lost)) %>%
      mutate(cal_focus_revenue = ifelse (repeat_no>1, NA, cal_focus_revenue)) %>%
      ungroup() %>%
      select(-repeat_no,-count_dup, -count_seasons) %>%
  

  ##Calculate revenue from focus crop
  mutate(cal_focus_revenue= ifelse(is.na(cal_focus_revenue), 0, cal_focus_revenue))  %>%
    
  
  ##Calculate productivity
  mutate(cal_focus_productivity = cal_focus_quant_prod/f_focus_crop_size_acre) 

# LABOUR

labour_types <- Data %>% 
  cSplit("f_crop_labour_types", "|") %>% 
  select(starts_with("f_crop_labour_types")) %>% 
  gather(key, value) %>% filter(!is.na(value)) %>% 
  select(value) %>% unique() %>% pull()

survey_labour_types <- substr(gsub("\\s|-", "", labour_types), 1, 5)
labour_options <- c("landprep", "nurserymaint", "cropmaint", 
                    "irrigation", "fertilizerapp", "compostprep", "harvesting", 
                    "postharvest", "otheractivity")

Data <- Data %>%
mutate(f_labour_landprep_rememberwage = f_labour_andprep_rememberwage) %>%
mutate(f_labour_landprep_paymentpertimeframe = as.numeric(f_labour_landprep_paymentpertimeframe)) %>%
mutate(f_labour_landprep_rememberwage_othercosts = as.numeric(f_labour_landprep_rememberwage_othercosts)) %>%
#mutate(f_labour_irrigation_nrmonths = NA) %>%
mutate(f_labour_cropmaint_rememberwage_othercosts = as.numeric(f_labour_cropmaint_rememberwage_othercosts)) %>%
rename(f_labour_irrigation_nrmonths = f_labour_sirrigation_nrmonths)
Data$f_labour_cropmaint_paymentpertimeframe <- as.numeric(Data$f_labour_cropmaint_paymentpertimeframe)

# LABOUR NOT WORKING ####
for(j in survey_labour_types){
  
  if(any(startsWith(labour_options, j))){
    
    var_name <- paste0("_labour_", labour_options[startsWith(labour_options, j)])
    
    Data <- Data %>% 
      mutate_at(vars(contains("_othertype")), as.numeric) %>%
      mutate_at(vars(contains("_othercosts")), as.numeric) %>%     
      
      #Calculate total number of hours for those that reported per hour
      mutate("f{var_name}_nrhours" := Data %>% 
               select(!!sym(paste0("f", var_name, "_nrhours")),
                      !!sym(paste0("f", var_name, "_nrdays"))) %>%
               apply(.,1,prod,na.rm=FALSE)) %>%
      mutate("f{var_name}_nrdays" := ifelse(!is.na(!!sym(paste0("f", var_name, "_nrhours"))), NA,
                                            !!sym(paste0("f", var_name, "_nrdays"))))  %>%
      
      #Merge all indicated timeperiods, each farmers has 1 value for the total timeperiod 1 hired labourer is hired. Either a value for the total
      # total # of hours, days, weeks, monhts, or for the total activity
      unite(!!paste0("cal", var_name, "_alltimeframes"),
            !!sym(paste0("f", var_name, "_nrhours")):!!sym(paste0("f", var_name, "_nrmonths")), 
            sep="|", remove=FALSE, na.rm=TRUE) %>%
      
      mutate("cal{var_name}_alltimeframes" := as.numeric(!!sym(paste0("cal", var_name, "_alltimeframes")))) %>%
      
    
    # Calculate the cost per worker for a certain timeperiod
    
    mutate("f{var_name}_paymentpertimeframe" := as.numeric(
      !!sym(paste0("f", var_name, "_paymentpertimeframe")))) 
      
    Data <- Data %>%
      mutate("cal{var_name}_cost_timeperiod" := Data %>% 
               select(!!sym(paste0("cal", var_name, "_alltimeframes")),
                      !!sym(paste0("f", var_name, "_paymentpertimeframe"))) %>%
               apply(.,1,prod)) %>%
      
    mutate("cal{var_name}_cost_timeperiod" := ifelse(is.na(!!sym(paste0("cal", var_name, "_cost_timeperiod"))), !!sym(paste0("f", var_name, "_paymentpertimeframe")),
                                            !!sym(paste0("cal", var_name, "_cost_timeperiod"))))  
    
Data <- Data %>%
    mutate("cal{var_name}_part_1_total_cost" := Data %>% 
               select(!!sym(paste0("cal", var_name, "_cost_timeperiod")),
                      !!sym(paste0("f", var_name, "_nrhiredpeople"))) %>%
               apply(., 1, prod)) %>%

    #Calculate total cost for those that  reported wage using "other method"
    
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
    select(-contains("alltimeframes"), -contains("part_1_total_cost"),-contains("part_2_total_cost"))


#### COMBINE ALL LABOUR ####

# Combine costs of all types of labour
Data <- Data %>% 
  mutate(cal_labour_cost = Data %>%
           select(starts_with("cal_labour") & ends_with("_cost")) %>% 
           rowSums(na.rm=TRUE)) 

          ##Adress values for those that have multiple seasons
          Data <- Data %>%
          #Identify duplicates
          group_by(identifier, f_crop_labour_timeperiod) %>% mutate(count_dup = n()) %>%
          mutate(count_dup = ifelse(is.na(f_crop_labour_timeperiod), NA, count_dup)) %>%
          #Identify those with multiple seasons
          group_by(identifier, f_crop_labour_timeperiod) %>% mutate(count_seasons = n()) %>%
          mutate(count_seasons = ifelse(is.na(f_crop_labour_timeperiod) | count_seasons > 1, NA, count_seasons)) %>%
          
          #Replace values with 0 for those that entered the data twice
          mutate(repeat_no = as.numeric(`repeat no`)) %>%
          mutate(cal_labour_cost = ifelse(repeat_no >1  & count_dup > 1, 0, cal_labour_cost)) %>%
        
          #Sum the values for those that entered data for multiple seasons
          group_by(identifier, count_seasons) %>%
          mutate(cal_labour_cost =  sum(cal_labour_cost)) %>%
          
          #Ensure we have 1 row of data for each farmer
          mutate(cal_labour_cost = ifelse(repeat_no >1 , NA, cal_labour_cost)) %>%
          ungroup() %>%
          select(-repeat_no,-count_dup, -count_seasons)
  
# INPUTS
Data <- Data %>%
  mutate(cal_inputs_costs = Data %>% 
           select(starts_with("f_inputs_costs")) %>%
           rowSums(na.rm=TRUE)) 

            ##Address values for those that have multiple seasons
            Data <- Data %>%
              #Identify duplicates
              group_by(identifier, f_inputs_timeperiod) %>% mutate(count_dup = n()) %>%
              mutate(count_dup = ifelse(is.na(f_inputs_timeperiod), NA, count_dup)) %>%
              #Identify those with multiple seasons
              group_by(identifier, f_inputs_timeperiod) %>% mutate(count_seasons = n()) %>%
              mutate(count_seasons = ifelse(is.na(f_inputs_timeperiod) | count_seasons > 1, NA, count_seasons)) %>%
              
              #Replace values with 0 for those that entered the data twice
              mutate(repeat_no = as.numeric(`repeat no`)) %>%
              mutate(cal_inputs_costs = ifelse(repeat_no >1  & count_dup > 1, 0, cal_inputs_costs)) %>%
              
              #Sum the values for those that entered data for multiple seasons
              group_by(identifier, count_seasons) %>%
              mutate(cal_inputs_costs =  sum(cal_inputs_costs)) %>%
              
              #Ensure we have 1 row of data for each farmer
              mutate(cal_inputs_costs = ifelse(repeat_no >1 , NA, cal_inputs_costs)) %>%
              ungroup() %>%
              select(-repeat_no,-count_dup, -count_seasons)
            
#Total focus crop cost
Data <- Data %>%
  mutate(cal_focus_cost = cal_labour_cost + cal_inputs_costs + f_transport) %>%

#Net-income focus crop
  mutate(cal_focus_income = cal_focus_revenue - cal_focus_cost)





# OTHER CROP income
Data <- Data %>%
  mutate(cal_othermaincrop_cost_1 = Data %>% 
           select(starts_with("f_othermaincrop_1_cost")) %>%
           apply(.,1,sum, na.rm=TRUE)) %>%
  mutate(cal_othermaincrop_cost_2 = Data %>% 
           select(starts_with("f_othermaincrop_2_cost")) %>%
           apply(.,1,sum, na.rm=TRUE)) %>%
  mutate(cal_othermaincrop_1_inc_sold = ifelse(is.na(f_othermaincrop_1_inc_sold), 0, f_othermaincrop_1_inc_sold)) %>%
  mutate(cal_othermaincrop_2_inc_sold = ifelse(is.na(f_othermaincrop_2_inc_sold), 0, f_othermaincrop_2_inc_sold))  %>%
  mutate(cal_othermaincrop_income = cal_othermaincrop_1_inc_sold - cal_othermaincrop_cost_1 + cal_othermaincrop_2_inc_sold - cal_othermaincrop_cost_2  ) %>%
  mutate(cal_other_crop_income = ifelse(is.na(f_other_crop_income), 0, f_other_crop_income)) %>%
  mutate(cal_other_crop_income = cal_othermaincrop_income + cal_other_crop_income)
  
  
  # LIVESTOCK
  Data <- Data %>%
  rename(f_livestock_days_hiredlabour = f_livestcok_days_hiredlabour) %>%
  
  #Revenue
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
  mutate(cal_livestock_income = cal_livestock_revenue - cal_livestock_cost)



  
##OTHER FARM COSTS

# EQUIPMENT
Data <- Data %>%
  mutate(cal_equipment_costs = Data %>% 
           select(f_nonmech_equip_costs, f_mech_equip_costs, f_materials_other_costs) %>% 
           apply(.,1,sum, na.rm=TRUE)) %>%



# GENERAL ON FARM COSTS
  mutate(cal_farm_other_costs = Data %>% 
           select(f_costs_land,
                  hh_loan_costs_interest) %>%
           apply(., 1, sum, na.rm=TRUE)) #%>%
  #mutate(cal_farm_cost = cal_inputs_costs + 
  #         cal_labour_cost + 
  #         cal_equipment_costs +
  #         cal_inputs_costs +
  #         cal_livestock_cost +
  #         cal_othermaincrop_cost_1 +
  #         cal_othermaincrop_cost_2 +
  #         cal_farm_other_costs,
  #         f_transport_cost) %>%
  # mutate(cal_farm_cost= ifelse(is.na(cal_farm_cost), 0, cal_farm_cost)) 
    
    



# OFF FARM LABOUR INCOME
Data <- Data %>%
  mutate(cal_offfarm_labour_income = Data %>% 
           select(f_nonfarm_enterpr_1_profit,
                  f_nonfarm_enterpr_2_profit,
                  f_nonfarm_enterpr_3_profit,
                  f_income_offfarmlabour_wage) %>%
           apply(., 1, sum, na.rm=TRUE)) 

# OFF FARM NON LABOUR INCOME
Data <- Data %>% 
  mutate(cal_offfarm_non_labour_income = Data %>%
           select(starts_with("f_income_other_"), -contains("_type")) %>% 
           apply(.,1,sum, na.rm=TRUE)) 


# TOTAL INCOME CALCULATIONS
Data <- Data %>%
  mutate(cal_farm_income = cal_focus_income + cal_other_crop_income + cal_livestock_income - 
           cal_equipment_costs - cal_farm_other_costs) %>%
  mutate(cal_actual_income = cal_focus_income + cal_other_crop_income + cal_livestock_income - 
           cal_equipment_costs - cal_farm_other_costs + cal_offfarm_labour_income + cal_offfarm_non_labour_income) %>%
  mutate(cal_farm_income = ifelse(is.na(sdm_farmer)  , NA, cal_farm_income )) %>%
  mutate(cal_actual_income  = ifelse(is.na(sdm_farmer) , NA, cal_actual_income))

# 
data_grouped <- Data %>% 
  group_by(identifier) #%>%
  #summarise(f_focus_price = mean(f_focus_price, na.rm=TRUE)) 

hh_repeated <- Data %>% 
  select(identifier, contains("hh_member"), 
         "hh_unpaidlabour_hhactivities_hrs")

#Data <- Data %>%
  #select(-starts_with("cal_")) %>%
  #filter(`repeat no` == 1) %>%
  #select(-c(#"f_focus_rev_timeperiod",
            #"f_focus_price",
            #"f_focus_own_consumption",
            #"f_focus_own_consumption_measurement",
            #"f_focus_own_lost_measurement",
            #"f_crop_labour_timeperiod",
            #"f_crop_labour_types",
            #"f_transport",
#            "hh_member_age",
#            "hh_member_gender",
#            "hh_member_education",
#            "hh_unpaidlabour_hhactivities_hrs")) %>%
  #left_join(data_grouped) %>%
  #full_join(hh_repeated)

## FILTER PRIVATE INFO
Data <- Data %>% select(
  -c(starts_with("pi_"), -contains("county")),
  -c(name_of_farmer, mobile_number_farmer)) #%>%
  # mutate(pi_location_cascade_county = "nakuru") %>%
  # rename(
    # pi_location_cascade_county = pi_location_cascade_1_county_,
    # pi_location_cascade_sub_county = pi_location_cascade_1_sub_county)

write.table(Data, file = here::here("Volumes/GoogleDrive/My Drive/04_data_analytics/RGL", paste0(anonymized_output_file, "_beans.csv")))



#