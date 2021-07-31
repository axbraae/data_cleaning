# script to clean halloween candy data
# abraae jul 2021

# load the libraries needed
library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(lubridate) 
library(rio)

# load cleaning functions

source(here::here("data_cleaning_scripts/2_cleaning_functions.R"))


# load data ---------------------------------------------------------------

candy_2015 <- rio::import(
  "https://www.scq.ubc.ca/wp-content/uploads/2015/10/CANDY-HIERARCHY-2015-SURVEY-Responses.xlsx")

candy_2016 <- rio::import(
  "https://www.scq.ubc.ca/wp-content/uploads/2016/10/BOING-BOING-CANDY-HIERARCHY-2016-SURVEY-Responses.xlsx")

candy_2017 <- rio::import(
  "https://www.scq.ubc.ca/wp-content/uploads/2017/10/candyhierarchy2017.xlsx")

# select only columns required for downstream analysis
# use candy_column_selector function
candy_2015 <- candy_column_selector(candy_2015)
candy_2016 <- candy_column_selector(candy_2016)
candy_2017 <- candy_column_selector(candy_2017)


# 2015 data ---------------------------------------------------------------

# make id and year columns
# rename age and trick or treating columns

candy_2015 <- candy_2015 %>% 
  mutate(id = as.numeric((Timestamp)), 
         year = year(Timestamp)) %>% 
  rename("age" = "How old are you?",
         "going_trick_or_treat" = 
           "Are you going actually going trick or treating yourself?") %>% 
  pivot_longer(
  cols = starts_with("["), 
  names_to = "candy", 
  values_to = "joy_induction") %>% 
  clean_names() %>% 
  mutate(
    candy = str_remove_all(candy, "\\[|\\]"),
    age = as.numeric(age)
    )



# 2016 data ---------------------------------------------------------------

#follow same cleaning as for 2015

# add id and year columns and rename age and trick or treating,
# rename gender and country

candy_2016 <- candy_2016 %>% 
  mutate(id = as.numeric((Timestamp)),
         year = year(Timestamp)) %>%
  rename("age" = "How old are you?",
         "going_trick_or_treat" = 
           "Are you going actually going trick or treating yourself?",
         "gender" = "Your gender:",          
         "country" = "Which country do you live in?") %>% 
  mutate(
    age = if_else(
      is.na(as.numeric(country)) == FALSE & is.na(age) == TRUE,
      as.numeric(country),
      as.numeric(age)),
    country = if_else(
      is.na(as.numeric(country)) == FALSE,
      NA_character_,
      as.character(country))
    )

candy_2016 <- candy_2016 %>% pivot_longer(
  cols = starts_with(c("[", "Q6")), 
  names_to = "candy", 
  values_to = "joy_induction") %>% 
  clean_names() %>%
  mutate(
    candy = str_remove_all(candy, "\\[|\\]|Q6 \\| "),
    age = as.numeric(age),
    country = str_to_lower(str_replace_all(country, "[:punct:]", ""))
  )

# for downstream analysis, recode country column to usa, canda, uk or other
# can ignore case and punctuation because these have been removed from country

candy_2016 <- candy_2016 %>% 
  mutate(country = case_when(
    str_detect(country, "not[\\s]{1,}") ~ NA_character_,
    str_detect(country, str_c(usa_pattern, collapse = "|")) ~ "us",
    str_detect(country, str_c(uk_pattern, collapse = "|")) ~ "uk",
    str_detect(country, "^can") ~ "canada",
    is.na(country) == TRUE ~ NA_character_,
    TRUE ~ "other")
  )

# 2017 data ---------------------------------------------------------------

# follow same cleaning as for 2015, 2016

candy_2017 <- candy_2017 %>% 
  mutate(year = 2017) %>%  
  rename("id" = "Internal ID",
         "age" = "Q3: AGE", 
         "going_trick_or_treat" = 
           "Q1: GOING OUT?",
         "gender" = "Q2: GENDER",
         "country" = "Q4: COUNTRY") %>% 
  mutate(
    age = if_else(
      is.na(as.numeric(country)) == FALSE & is.na(age) == TRUE,
      as.numeric(country),
      as.numeric(age)),
    country = if_else(
      is.na(as.numeric(country)) == FALSE,
      NA_character_,
      as.character(country))
    )

candy_2017 <- candy_2017 %>% pivot_longer( 
  cols = starts_with(c("[", "Q6")), 
  names_to = "candy", 
  values_to = "joy_induction") %>% 
  clean_names() %>%
  mutate(
    candy = str_remove_all(candy, "\\[|\\]|Q6 \\| "),
    age = as.numeric(age),
    country = str_to_lower(str_replace_all(country, "[:punct:]", ""))
    )

candy_2017 <- candy_2017 %>% 
  mutate(country = case_when(
  str_detect(country, "not[\\s]{1,}") ~ NA_character_,
  str_detect(country, str_c(usa_pattern, collapse = "|")) ~ "us",
  str_detect(country, str_c(uk_pattern, collapse = "|")) ~ "uk",
  str_detect(country, "^can") ~ "canada",
  is.na(country) == TRUE ~ NA_character_,
  TRUE ~ "other")
  )

# pre-joining wrangling ---------------------------------------------------

# limit age using age_corrector function for all three data sets

candy_2015 <- age_corrector(candy_2015)
candy_2016 <- age_corrector(candy_2016)
candy_2017 <- age_corrector(candy_2017)

# add confectionery column useful for analysis
# perform final data cleaning and select columns for data binding

candy_2017 <- candy_2017 %>% 
  mutate(gender = replace_na(gender, "I'd rather not say"),
         candy = if_else(str_detect(candy, "Box’o’ Raisins"), "Box'o'Raisins", candy),
         confectionery = ! candy %in% not_candy) %>% 
  select(id, year, age, gender, country, going_trick_or_treat, 
         candy, joy_induction, confectionery)

candy_2016 <- candy_2016 %>% 
  mutate(gender = replace_na(gender, "I'd rather not say"),
         candy = if_else(str_detect(candy, "Box’o’ Raisins"), "Box'o'Raisins", candy),
         confectionery = ! candy %in% not_candy) %>% 
  select(id, year, age, gender, country, going_trick_or_treat, 
         candy, joy_induction, confectionery)

candy_2015 <- candy_2015 %>% 
  mutate(gender = "I'd rather not say",
         candy = if_else(str_detect(candy, "Box’o’ Raisins"), "Box'o'Raisins", candy),
         confectionery = ! candy %in% not_candy,
         country = NA_character_) %>% 
  select(id, year, age, gender, country, going_trick_or_treat, 
         candy, joy_induction, confectionery)



# generate final merged dataset -------------------------------------------

candy_2017_2016_2015 <- bind_rows(candy_2017, candy_2016, candy_2015)

#write clean csv
write_csv(candy_2017_2016_2015, here::here("clean_data/clean_candy.csv"))