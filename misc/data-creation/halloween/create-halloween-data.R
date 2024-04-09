### ---- Data descriptions ---- 

# Halloween data
# source: http://www.dataplusscience.com/HalloweenData.html
# half hour interval number of candies given out at a home in Cincinnati

# Gdp data
# source: https://fred.stlouisfed.org/series/NGMP17140
# Cincinnati gdp over years at start of year 

# Weather data
# -> GOAL: Find Cincinnati temperature and precipitation data for halloween since 2008

# manually found data here: https://www.ncei.noaa.gov/pub/data/ghcn/daily/   -> then by_station/
# searched for Cincinnati station here: https://www.ncdc.noaa.gov/cdo-web/datatools/findstation 
# -> ID = USC00331515 -> Cheviot, OH

# downloaded .cvs.gz file
# unzipped using terminal following this tutorial: https://opencsvfile.com/csv-gz-to-csv.html

# data dictionary: https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme-by_station.txt

### ---- Setup ---- 

library(tidyverse)

### ---- Weather data ---- 

# load raw data
data_raw_candy <- read_csv(file = "Data - Originals/candy-data.csv",
                       col_types = c("ccncc"))
data_raw_gdp <- read_csv(file = "Data - Originals/cincinnati-gdp-data.csv")
data_raw_weather <- read_csv(file = "Data - Originals/USC00331515.csv",
                     col_names = c("StationID", "Date", "Element", "Value", "MFlag", "QFlag", "SFlag", "ObsTime"),
                     col_types = c("cccncccc"))

# convert to date at Halloween
data_candy <- data_raw_candy %>% 
  mutate(Date = mdy(Date))
data_gdp <- data_raw_gdp %>% 
  mutate(Year = year(mdy(Year)),
         Date = mdy(paste("10", "31", Year))) %>% 
  select(-Year)

# manipulate weather data
# -> convert to date
# -> filter to time frame to match that of Halloween data
# -> convert units to US systems
# -> convert to wide
# units for elements of interest
# -> TMIN and TMAX are in tenths of degrees Celsius
# -> PRCP is in tenths of mm
data_weather <- data_raw_weather %>% 
  mutate(Date = ymd(Date)) %>% 
  filter(year(Date) >= 2008, month(Date) == 10, day(Date) == 31,
         Element %in% c("TMIN", "TMAX", "PRCP")) %>% 
  select(Date, Element, Value) %>% 
  mutate(Value = 
           case_when(
             Element %in% c("TMIN", "TMAX") ~ 9 / 5 *  (Value / 10) + 32, # to Celsius then to F
             Element == "PRCP" ~ round(Value / 100 / 2.54, 2), # to cm to in
             TRUE ~ Value
           )
  ) %>% 
  pivot_wider(names_from = Element, values_from = Value)
  

### ---- Combine data ---- 

# merge weather and gdp to the candy data
data_halloween <- data_candy %>% 
  left_join(data_weather, by = c("Date")) %>% 
  left_join(data_gdp, by = c("Date")) 

# this results in duplicated aggregate values
# -> which is okay, going to work with this, then in later content explain relationships to avoid this problem
# save combined data
write_csv(data_halloween, file = "data-halloween.csv")