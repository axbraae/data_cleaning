### this file contains:
### - cleaning functions common to all three datasets
### - lists used to filter/generate some of the additional columns

# functions ---------------------------------------------------------------

### candy_column_selector
# function to select the columns needed for downstream analysis

candy_column_selector <- function(candy_data){
  select_candy_data <- candy_data %>% 
    select(
      contains("timestamp", ignore.case = TRUE), #not in 2017
      contains("internal", ignore.case = TRUE), #not in 2015, 2016
      contains("old", ignore.case = TRUE),  #not in 2017
      contains(" age", ignore.case = TRUE), #not in 2015, 2016
      contains("gender", ignore.case = TRUE), #not in 2015
      contains("country", ignore.case = TRUE), #not in 2015
      contains("going", ignore.case = TRUE), #selects trick or treating all years
      contains("Q6"), #selects candy 2017
      starts_with("["), #selects candy 2015, 2016
    ) 
  return(select_candy_data)
}

### age_corrector function 
# oldest man alive is 112, set to NA if >112.
# ref: https://www.guinnessworldrecords.com/news/2021/6/emilio-flores-marquez-confirmed-as-the-worlds-oldest-man-living-at-112-665641
# set youngest age to >1, assume anyone younger than 1 cannot eat sweets

age_corrector <- function(age_data){
  corrected_age_data <- age_data %>% 
    mutate(
      age = case_when(
        age > 112 ~ NA_real_,
        age < 1 ~ NA_real_,
        TRUE ~ as.numeric(age)
      )
    )
  return(corrected_age_data)
}

# lists -------------------------------------------------------------------


### not_candy list 
# list of column headings which are not edible confectionery
# If revisit think of a reproducible way to generate this list.

not_candy <- c(
  "Box’o’ Raisins", #2015
  "Box'o'Raisins", #2016, 2017
  "Chardonnay",
  "Cash, or other forms of legal tender",
  "Dental paraphenalia",
  "Generic Brand Acetaminophen",
  "Glow sticks",
  "Broken glow stick",
  "Creepy Religious comics/Chick Tracts",
  "Healthy Fruit",
  "Hugs (actual physical hugs)",
  "Kale smoothie",
  "Lapel Pins",
  "Minibags of chips",
  "JoyJoy (Mit Iodine)",
  "Mint Juleps",
  "Mint Leaves",
  "Spotted Dick",
  "Peterson Brand Sidewalk Chalk",
  "Peanut Butter Jars",
  "Trail Mix",
  "Vicodin",
  "White Bread",
  "Whole Wheat anything",
  "Person of Interest Season 3 DVD Box Set (not including Disc 4 with hilarious outtakes)",
  "Real Housewives of Orange County Season 9 Blue Ray",
  "Abstained from M&M'ing."
)


### regex patterns for recoding country in 2016, 2017

usa_pattern <- c("usa",
                 "^u[ s]",
                 "ed[\\s]{1,}st",
                 "[ast]es$",
                 "m[eu]r{1,}i[ck]a$",
                 "amer[ica]",
                 "trump",
                 "a{2,}y{4,}",
                 "alask",
                 "olina$",
                 "pi[t]{1,}s",
                 "^new[ ]{1,}[jy][oe]",
                 "^cali",
                 "^atl")

uk_pattern <- c("u[ k]$", "[ng]dom$", "[ron][gdte]land$")