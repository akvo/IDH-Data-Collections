##  ---- readdata ----
# Data delivery bare bones

# Packages used
library(here); library(readxl);
library(plyr); library(dplyr); library(tidyr); library(tidylog); library(tidyselect)
library(stringr); library(zoo);library(splitstackshape); library(reshape2); library(openxlsx)



##  ----prepare functions ----
# count values without NA
count_n <- function(x){sum(!is.na(x))}

##  ---- define filenames ----
cleaned_data = "2021_RGL_anom_beans_intermediate.csv"
survey_filename <- "survey_rgl_beans.xlsx"
raw_data <- "24092021_rgl_beans_raw_data.xlsx"
data_delivery <- "2021_RGL_anom_beans.xlsx"

##  ---- import cleaned data and survey ----
# Read cleaned data from output folder
Data <- read.table(cleaned_data)

# Read file with survey structure
survey_questions <- read_excel(survey_filename,
  sheet ="Full Survey", skip=2)
question_library <- read_excel("question library format v3.1.1.xlsx",
                               sheet = "Full Survey", skip = 2) 
vars_transformed <- read_excel("variables with transformation.xlsx") 
household_demographics  <- read_excel("household_demographics_var.xlsx") 
vars_dashboard <- read_excel("variable names input dashboard.xlsx") 

#Raw data
Data_raw <- read_excel(raw_data) %>%
  select(-starts_with("pi_"), -name_of_farmer, -mobile_number_farmer)


##  ---- adjust survey for analysis ----
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


##  ---- _______ ----
##  ---- 1) ALL FARMERS ----

nr_participants_raw <- length(unique(Data$identifier))

farmer_type <- c("all farmers")
all_var <- Data %>% 
  filter(repeat.no == 1) %>%
  select(repeat.no) %>% 
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
      filter(repeat.no==1) %>%
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
  filter(multiple == "yes") %>%
  pull(variable)

multiple_categorical_descriptives_by_gender <- gender_var

for(j in multiple_mc){
  
  if(any(names(Data) %in% j)){
    
    frequencies <- Data %>% 
      select(hh_farmer_gender, all_of(j)) %>% 
      cSplit(j, "|") %>%
      gather(key, value, -hh_farmer_gender) %>%
      select(-key) %>% table() %>%
      melt(c("hh_farmer_gender","category"), value.name="freq") %>%
      mutate("variable" = j) %>%
      left_join(gender_var) %>%
      mutate("%" = round(freq/n,2)*100) %>%
      mutate(category = as.character(category))
    
    multiple_categorical_descriptives_by_gender <- multiple_categorical_descriptives_by_gender %>% 
      full_join(frequencies) %>%
      select(variable, hh_farmer_gender, n, category, freq, "%")
  }
}


##  ---- ______----    
##  ---- Calculate net promotor score----    
#NPS = (Promoters/total # farmers) - (Detractors/total # farmers)
#This is calculated from the variable cs_recommendation:
#“How likely is it that you would recommend [SDM company] to a friend or peer?”)

#Not likely, somewhat likely, likely = detractor
#Very likely = promoter

hh_farmer_gender <- c("all farmers")

NSP_total <- Data %>%
  select(repeat.no, cs_recommendation) %>% 
  filter(repeat.no == 1)%>%
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
  select(repeat.no, cs_recommendation, hh_farmer_gender) %>% 
  filter(repeat.no == 1)%>%
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

# Repeated rows 

if("identifier" %in% colnames(Data)){     
    Data <- Data %>%
      group_by(identifier) %>%
      mutate(id = row_number())
    
    for (nr in c(1:15)){
      

    }
  }
  
  Data <- Data %>%
    filter(id == 1) %>%
    select(-id)
           #-hh_member_birthyear,
           #-hh_member_gender,
           #-hh_member_education,
           
           #-hh_focuscrop_hrs)

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
## ---- Combine all inputs in one excel ----


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

write.xlsx (sets, file = data_delivery, overwrite = TRUE)



