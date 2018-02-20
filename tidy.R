
#---------------Tidy Data----------------

# Installations
# The easiest way to get tidyr is to install the whole tidyverse:
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyr)
}

# Alternatively, install just tidyr:
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
  }

library("stringr")
library("reshape2")
library("plyr")
library(dplyr)

# Arranged in one way
preg <- read.csv("E:/Data Analytics with RET/Assignment 3 Data Cleaning/Tidy Data/preg.csv",
                 stringsAsFactors = FALSE)
preg

# Arranged in second way
preg2 <- read.csv("E:/Data Analytics with RET/Assignment 3 Data Cleaning/Tidy Data/preg2.csv",
                  stringsAsFactors = FALSE)
preg2

# Tidy data
preg3 <- preg %>%gather(treatment, n, treatmenta:treatmentb) %>%
  mutate(treatment = gsub("treatment", "", treatment)) %>%
  arrange(names, treatment)
preg3

#--------------------------------------------------------------------------------
# most messy datasets, including types of messiness, can be tidied 
# with a small set of tools: gathering, separating and spreading

#--------------------------------------------------------------------------------
# Column headers are values, not variable names
library(haven)
pew <- tbl_df(read.csv("E:/Data Analytics with RET/Assignment 3 Data Cleaning/Tidy Data/pew.csv",
                       stringsAsFactors = FALSE, check.names = FALSE))
pew
pew %>%gather(income, frequency, -religion)
# -------------------------------------------------------------------------------

billboard <- tbl_df(read.csv("E:/Data Analytics with RET/Assignment 3 Data Cleaning/Tidy Data/billboard.csv",
                             stringsAsFactors = FALSE))
billboard
# To tidy this dataset, we first gather together all the wk columns. The column names give the week 
# and the values are the ranks:

billboard2 <- billboard %>% gather(week, rank,x1st.week:x76th.week, na.rm = TRUE)
billboard2
# Here we use na.rm to drop any missing values from the gather columns. In this data, missing values
# represent weeks that the song wasn't in the charts, so can be safely dropped
billboard3 <- billboard2 %>%
  mutate(
    week = extract_numeric(week),
    date = as.Date(date.entered) + 7 * (week - 1)) %>%
  select(-date.entered)
billboard3

#------------------------------------------------------------------------------------------

# Multiple variables stored in one column

tb <- tbl_df(read.csv("E:/Data Analytics with RET/Assignment 3 Data Cleaning/Tidy Data/tb.csv", 
                      stringsAsFactors = FALSE))
tb
# First we gather up the non-variable columns:
tb2 <- tb %>% gather(demo, n, -iso2, -year, na.rm = TRUE)
tb2

#------------------------------------------------------------------------------------------

#Variables are stored in both rows and columns
weather <- tbl_df(read.csv("E:/Data Analytics with RET/Assignment 3 Data Cleaning/Tidy Data/weather.csv", 
                           stringsAsFactors = FALSE))
weather
weather2 <- weather %>%gather(day, value, d1:d31, na.rm = TRUE)
weather2

weather3 <- weather2 %>%
  mutate(day = extract_numeric(day)) %>%
  select(id, year, month, day, element, value) %>%
  arrange(id, year, month, day)
weather3

weather3 %>% spread(element, value)
# This form is tidy: there's one variable in each column, and each row represents one day.
#-----------------------------------------------------------------------------------------

# Multiple types in one table
song <- billboard3 %>%
  select(artist.inverted, track, year, time) %>%
  unique() %>%
  mutate(song_id = row_number())
song

# Then use that to make a rank dataset by replacing repeated song facts with a pointer to 
# song details (aunique song id):
rank <- billboard3 %>%
  left_join(song, c("artist.inverted", "track", "year", "time")) %>%
  select(song_id, date, week, rank) %>%
  arrange(song_id, date)
rank

#-----------------------------------------------------------------------------------------

pew <- read.delim(
  file = "http://stat405.had.co.nz/data/pew.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = F
)
View(pew)

tb <- read.csv(
  file = "http://stat405.had.co.nz/data/tb.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)

weather <- read.delim(
  file = "http://stat405.had.co.nz/data/weather.txt",
  stringsAsFactors = FALSE
)

billboards <- read.csv(
  file = "http://stat405.had.co.nz/data/billboard.csv",
  stringsAsFactors = FALSE
)
names(billboards) <- gsub("\\.", "_", names(billboards))
#-----------------------------------------------------------------

library(reshape2)
pew_tidy <- melt(
  data = pew,
  id = "religion",
  variable.name = "income",
  value.name = "frequency"
)
# set column `new_sp` to NULL and clean up column names
tb$new_sp = NULL
names(tb) <- gsub("new_sp_", "", names(tb))

# Use na.rm = TRUE to remove missing observations
tb_tidy <- melt(
  data = tb,
  id = c("iso2", "year"),
  variable.name = "gender_age",
  value.name = "cases",
  na.rm = TRUE
)

# split gender_age into gender and age group
library(plyr)
tb_tidy <- mutate(tb_tidy,
                  gender = sub("^([m|f])(.*)$", "\\1", gender_age),
                  age = sub("^([m|f])(.*)$", "\\2", gender_age),
                  gender_age = NULL
)
tb_tidy <- tb_tidy[c('iso2', 'year', 'gender', 'age', 'cases')]

weather_tidy <- melt(
  data = weather,  
  id = 1:4,
  variable.name = "day",
  value.name = "temparature",
  na.rm = TRUE
)
weather_tidy <- mutate(weather_tidy,
                       day = sub("^d", "", day)  
)
weather_tidy2 <- dcast(
  data = weather_tidy,
  formula = id + year + month + day ~ element, 
  value.var = "temparature"
)
billboards <- mutate(billboards,
                     artist_inverted = iconv(artist_inverted, "MAC", "UTF-8"),
)  
billboards_tidy <- melt(billboards, 
                        id = 1:7,
                        variable.name = "week",
                        value.name = "rank",
                        na.rm = TRUE
)
billboards_tidy <- mutate(billboards_tidy,
                          week = as.numeric(gsub("^x([[:digit:]]+).*", "\\1", week))  
)
