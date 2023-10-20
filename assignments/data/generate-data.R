## ---- Load packages ---- 

library(tidyverse)
library(extraDistr)

## ---- Generate example dataset ---- 

# grades dataset -> creating it so that it needs cleaning
# -> ID: 4 numbers
# -> Student: 2 initials + 1 number for class (1 or 2)
# -> Test 1: grade out of 30
# -> Test 2: grade out of 70

# strategy
# -> generate a one row dataframe for a single student
# --> paste() -> not totally sure how the collapse vs sep works...
# --> ID: assuming no duplicates and can check later
#- > use replicate() to repeat the random generation n times -> result is an array with dim = 4 (# columns in df) x n (# students)
# -> then transpose to be n x # of columns and convert to dataframe
# -> final mutate statement is to convert datatypes to what they should be, some weird error happened when trying to write csv (all data types were logical for some reason befoere this step)
set.seed(042023)
data_grades <- replicate(n = 65,
                         data.frame(
                           ID = sample(0:9, size = 4, replace = TRUE) %>% paste(collapse = ""), # just assuming there won't be any duplicates, can check for it
                           Student = sample(x = LETTERS, size = 2, replace = TRUE) %>% 
                             paste(collapse = "") %>% 
                             paste(sample(x = 1:2, size = 1, replace = TRUE), sep = "-"),
                           Test_1 = rdunif(n = 1, min = 10, max = 30),
                           Test_2 = rdunif(n = 1, min = 40, max = 70)
                         ),
                         simplify = TRUE) %>% 
  t %>% 
  data.frame %>% 
  mutate(ID = as.character(ID),
         Student = as.character(Student),
         Test_1 = as.numeric(Test_1),
         Test_2 = as.numeric(Test_2))

# check all unique
length(unique(data_grades$ID))

# major dataset -> creating it so that it needs joined
# -> ID: 4 numbers
# -> Major: one of two majors

# strategy
# -> start from grades data and get IDs
# -> sample from a string of the two majors
# -> then sort some way so that it is in different order than data_grades
data_majors <- data_grades %>% 
  select(ID) %>% 
  mutate(Major = sample(c("Chemisty", "Physics"), size = 65, replace = TRUE)) %>% 
  arrange(desc(ID))

# export two datasets
write_csv(data_grades, file = "data-grades.csv")
write_csv(data_majors, file = "data-majors.csv")
