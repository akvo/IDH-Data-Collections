#FARMFIT ACTUAL INCOME CALCULATION

##Some preps

##Check what the common meausurement unit for land is and adjust the following section accordingly. 
##We need a farm size in acres for the total farm size, focus crop, other main crop 1 and othermaincrop 2.
mutate(f_size_acre = ifelse(f_unit_land == "hectares", f_size*2.471, f_size)) %>%
  mutate(f_focus_crop_size_acre = ifelse(f_unit_land == "hectares", f_focus_crop_size*2.471, f_focus_crop_size)) %>%
  mutate(f_size_othermaincrop_1_acre = ifelse(f_unit_land == "hectares", 
                                              f_size_othermaincrop_1*2.471, 
                                              f_size_othermaincrop_1)) %>%
  mutate(f_size_othermaincrop_2_acre = ifelse(f_unit_land == "hectares", 
                                              f_size_othermaincrop_2*2.471, 
                                              f_size_othermaincrop_2)) %>%
  
  
  # Age
  # It could be that some variables are not used! For example hh_member_birthyear
  mutate(cal_hh_farmer_age = 2022 - hh_farmer_birthyear,
         cal_hh_head_age = 2022-hh_head_birthyear,
         cal_hh_member_birthyear = 2022-hh_member_birthyear) #update to current year, check whether applicable, not always included.



##  ---- ______ ----
##  ---- ACTUAL INCOME CALCULATIONS ----
#For more detailed description, see the excel sheet: https://docs.google.com/spreadsheets/d/1LvBxEfzq7jfLlw6Yv3VXi6hRps90SVG1qMEixSBxfZ4/edit#gid=208057956

##  ---- 1) Contextualize-> Prep: transform measurement units  ----
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
  mutate(cal_focus_lost_measurement := ifelse(
    grepl("[[:digit:]]", f_focus_measurement_lost), 
    extract_numeric(f_focus_measurement_lost), 
    ifelse(f_focus_measurement_lost %in% c("KG","kg","kilo","kgs","kilogram", "kilograms kg"), 1, 0))) %>% 
  
  #measurement unit for own consumption
  ##!! REMOVE IF NOT APPLICABLE
  mutate(cal_focus_measurement_own_consumption := ifelse(
    grepl("[[:digit:]]", f_focus_own_consumption_measurement), 
    extract_numeric(f_focus_own_consumption_measurement), 
    ifelse(f_focus_own_consumption_measurement %in% c("KG","kg","kilo","kgs","kilogram", "kilograms kg"), 1, 0))) 

#If there are other measurement units used like bucket, bags etc, use the below code
# This can apply for produce, sold, lost and own consumption
# Below is an example code for "a bucket of 20 kg":
#mutate(cal_focus_measurement_sold := ifelse(
#  f_focus_measurement_sold == "bucket", 20,
#  cal_focus_measurement_sold )) 

##  ---- 2) Prep: Calculate quantities  ----
##Calculate the quantities
Data <- Data %>%
  mutate(cal_focus_quant_prod_kg = f_focus_quant_prod * cal_focus_measurement_prod) %>%
  mutate(cal_focus_quant_sold_kg = f_focus_quant_sold * cal_focus_measurement_sold) %>%
  mutate(cal_focus_quant_lost_kg = f_focus_quant_lost * cal_focus_lost_measurement) %>%
  mutate(cal_focus_quant_own_consumption_kg = f_focus_own_consumption * cal_focus_measurement_own_consumption) %>%
  
  mutate(cal_focus_price= f_focus_price)  

##  ---- 3) Calc: Focus crop income  ----  

##  ---- 3a) Calc: Focus crop revenue ----  
Data <- Data %>%
  mutate(cal_focus_revenue = cal_focus_quant_sold_kg * cal_focus_price)  #Delete if type of coffee is not used in survey

##  ---- X) STANDARD APPROACH Inbetween, adjust for repeated question groups (IF APPLICABLE)  ----  
### REPLACE OR ADD ROWS FOR COFFEE CASES
## cal_focus_quant_prod_kg, cal_focus_quant_sold_kg,cal_focus_quant_lost_kg,cal_focus_revenue
##Adress values for those that have multiple seasons
Data <- Data %>%
  #Identify duplicates
  group_by(identifier, f_focus_rev_timeperiod) %>% mutate(count_dup = n()) %>%
  mutate(count_dup = ifelse(is.na(f_focus_rev_timeperiod), NA, count_dup)) %>% #check whether we don't make mistakes here. For some cases farmer enter twice wrongly, in other cases entering same values for 2 seasons is ok.
  #Identify those with multiple seasons
  group_by(identifier, f_focus_rev_timeperiod) %>% mutate(count_seasons = n()) %>%
  mutate(count_seasons = ifelse(is.na(f_focus_rev_timeperiod) | count_seasons > 1, NA, count_seasons)) %>%
  
  #Replace values with 0 for those that entered the data twice 
  mutate(`repeat no` = as.numeric(`repeat no`)) %>%
  mutate(cal_focus_quant_prod_kg = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_focus_quant_prod_kg)) %>%
  mutate(cal_focus_quant_sold_kg = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_focus_quant_sold_kg)) %>%
  mutate(cal_focus_quant_own_consumption_kg = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_focus_quant_own_consumption_kg )) %>%
  mutate(cal_focus_quant_lost_kg = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_focus_quant_lost_kg)) %>%
  mutate(cal_focus_revenue = ifelse(`repeat no` >1  & count_dup > 1, 0, cal_focus_revenue)) %>%
  
  #Sum the quantities for those that entered data for multiple seasons
  group_by(identifier, count_seasons) %>%
  mutate(cal_focus_quant_prod_kg =  sum(cal_focus_quant_prod_kg)) %>%
  mutate(cal_focus_quant_sold_kg =  sum(cal_focus_quant_sold_kg)) %>%
  mutate(cal_focus_quant_own_consumption_kg =  sum(cal_focus_quant_own_consumption_kg)) %>%
  mutate(cal_focus_quant_lost_kg =  sum(cal_focus_quant_lost_kg)) %>%
  mutate(cal_focus_revenue = sum(cal_focus_revenue)) %>%
  
  #Ensure we have 1 row of data for each farmer
  mutate(cal_focus_quant_prod_kg = ifelse(`repeat no` >1 , NA, cal_focus_quant_prod_kg)) %>%
  mutate(cal_focus_quant_sold_kg = ifelse(`repeat no` >1  , NA, cal_focus_quant_sold_kg)) %>%
  mutate(cal_focus_quant_own_consumption_kg = ifelse(`repeat no` >1  , NA, cal_focus_quant_own_consumption_kg )) %>%
  mutate(cal_focus_quant_lost_kg = ifelse(`repeat no` >1 , NA, cal_focus_quant_lost_kg)) %>%
  mutate(cal_focus_revenue = ifelse (`repeat no`>1, NA, cal_focus_revenue)) %>%
  ungroup() %>%
  select(-count_dup, -count_seasons)

##  ---- 3b) Calc: Revenues  ----   
##Calculate revenue from focus crop
Data <- Data %>%
  mutate(cal_focus_revenue= ifelse(is.na(cal_focus_revenue), 0, cal_focus_revenue))  

##  ---- 3c) Calc: Productivity  ----  
##Calculate productivity
Data <- Data %>%
  mutate(cal_focus_productivity_acre = cal_focus_quant_prod_kg/f_focus_crop_size_acre) 

#If crop is perennial crop, calculate yield/tree
## COFFEE CASE: Add or change rows if you have multiple coffee varieties -> you will have an adjusted cal_focus_quant_prod_kg variable
#mutate (cal_focus_productivity_tree = cal_focus_quant_prod_kg/f_trees_amount)

##  ---- 3d) Calc: Labour costs  ----  
survey_questions <- survey_questions %>%
  mutate(variable = ifelse(variable == "f_labour_compostprep_paymentpertimeframe_1_1","f_labour_compostprep_paymentperday",variable))

#Prepare 
labour_types <- Data %>% 
  cSplit("f_crop_labour_types", "|") %>% 
  select(starts_with("f_crop_labour_types")) %>% 
  gather(key, value) %>% filter(!is.na(value)) %>% 
  select(value) %>% unique() %>% pull()

survey_labour_types <- substr(gsub("\\s|-", "", labour_types), 1, 5)

##!!!!!!Adjust labour_options per case. Check in the survey which activities are surveyed.
## Sometimes, a combination of a labour activity and timeperiod is not selected and therefore not appearing.
# For example, f_labour_compostprep_nrhours -> there can be a case that no one that is hiring labour for compost preparation is registering this wage per hour.
# This piece of code can give some error, handle with care and check whether there are errors -> solve them. There is not one solution fits all.
labour_options <- c("landprep", "nurserymaint", "cropmaint", 
                    "irrigation", "fertilizerapp", "compostprep", "harvesting", 
                    "postharvest", "otheractivity")

#Calculation of labour costs
for(j in survey_labour_types){
  
  if(any(startsWith(labour_options, j))){
    
    var_name <- paste0("_labour_", labour_options[startsWith(labour_options, j)])
    
    Data <- Data %>% 
      mutate_at(vars(contains("_othertype")), as.numeric) %>%
      mutate_at(vars(contains("_othercosts")), as.numeric) %>%     
      
      #Calculate total number of hours for those that reported per hour
      mutate("cal{var_name}_nrhours" := Data %>% 
               select(!!sym(paste0("f", var_name, "_nrhours")),
                      !!sym(paste0("f", var_name, "_nrdays"))) %>%
               apply(.,1,prod,na.rm=FALSE)) %>%
      mutate("f{var_name}_nrdays" := ifelse(!is.na(!!sym(paste0("f", var_name, "_nrhours"))), NA,
                                            !!sym(paste0("f", var_name, "_nrdays"))))  %>%
      
      ##Calculate wage costs for those that paid per hour
      mutate("cal{var_name}_hour_costs" := Data %>% 
               select(!!sym(paste0("cal", var_name, "_nrhours")),
                      !!sym(paste0("f", var_name, "_paymentperhour")),
                      !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
               apply(.,1,prod)) %>%
      
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
      
      ##Calculate wage costs for those that paid per activity
      mutate("cal{var_name}_activity_costs" := Data %>% 
               select(!!sym(paste0("f", var_name, "_paymentperactivity")),
                      !!sym(paste0("f", var_name, "_nrhiredpeople")),) %>%
               apply(.,1,prod)) %>%
      
      unite(!!paste0("cal", var_name, "_part_1_total_cost"),
            !!sym(paste0("cal", var_name, "_day_costs")):!!sym(paste0("cal", var_name, "_activity_costs")), 
            sep="|", remove=FALSE, na.rm=FALSE) %>%  
      mutate("cal{var_name}_part_1_total_cost" := extract_numeric( !!sym(paste0("cal", var_name, "_part_1_total_cost")))) %>%
      
      
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
  select(-contains("alltimeframes"), 
         -ends_with("hour_costs"),
         -ends_with("day_costs"),
         -ends_with("month_costs"),
         -ends_with("activity_costs")) %>%
  mutate_at(vars(starts_with("cal_labour") & ends_with("_total_cost")), as.numeric) 

# Combine costs of all types of labour
Data <- Data %>% 
  mutate(cal_labour_cost = Data %>%
           select(starts_with("cal_labour") & ends_with("_total_cost")) %>% 
           rowSums(na.rm=TRUE)) 

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
  select(-`repeat no`,-count_dup, -count_seasons)

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
  select(-`repeat no`,-count_dup, -count_seasons)

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
  mutate(cal_livestock_revenue = f_livestock_income_type)

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
  mutate(cal_farm_income = cal_focus_income + cal_other_crop_income + cal_livestock_income - 
           cal_equipment_costs - cal_farm_other_costs) %>%
  mutate(cal_actual_income = cal_focus_income + cal_other_crop_income + cal_livestock_income - 
           cal_equipment_costs - cal_farm_other_costs + cal_off_farm_labour_income + cal_offfarm_non_labour_income) %>%
  mutate(cal_farm_income = ifelse(is.na(farmer_present)  , NA, cal_farm_income )) %>%
  mutate(cal_actual_income  = ifelse(is.na(farmer_present) , NA, cal_actual_income))
