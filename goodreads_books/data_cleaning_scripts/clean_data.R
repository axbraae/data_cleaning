# script to clean kaggle dataset on goodreads books
# https://www.kaggle.com/jealousleopard/goodreadsbooks

# load libraries
library(tidyverse)
library(janitor)
library(here)

book <- read_csv("raw_data/books_edit.csv", quote = "") %>%
  clean_names()

# recode missing page numbers
book_clean <- book %>% 
  mutate(num_pages = na_if(num_pages, 0),
         average_rating = if_else(average_rating == 0 & ratings_count > 0, 
                                  NA_real_,
                                  as.numeric(average_rating)),
         ratings_count = if_else(ratings_count == 0 & average_rating > 0, 
                                 NA_real_,
                                 as.numeric(ratings_count))
         )

# write cleaned csv
write_csv(book_clean, here("clean_data/books_clean.csv"))