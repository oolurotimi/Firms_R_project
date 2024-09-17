#' This script gets CHIRPS/CHIRTS data for the locations
#' identified by unique combinations of id_districtName,
#' subcounty_new, and parishnew.
#' 



# Setup ####

# Load required packages
library(haven)
library(dplyr)
library(stringr)
library(chirps)
library(furrr)
library(lubridate)

# Source a .R script containing a function to download data.
# If you are not working within an R project,
# you may need to modify the path to find this script. 
source(here::here("get_met.R"))

# Read in parishes data
# Again, if you're not in an R project, 
# you may need to modify the path.
df <- read_dta(here::here("climateandfirms.dta"))

##Adding one week to survey date to generate next week
df$nextweek<-add_with_rollback(df$surveydate, weeks(1))
class (df$nextweek)

# Assign unique IDs to firm x date 
df <- df %>%
  # firms_group is a unique id for firms
  # to add additional firm identifiers, add column names to group_by
  group_by(obs_id, surveydate) |>
  mutate(firms_group = cur_group_id()) |>
  ungroup()



##Short term temperature

df_collapse <- df %>% 
  drop_na(longitude, latitude, surveydate) %>% 
  group_by(firms_group, surveydate) %>% 
  slice(1) %>% 
  select(longitude, latitude) %>% 
  ungroup()

# Temperature on survey dates =================================

# Temperature on survey dates =================================


dates <- c("2010-06-15","2010-09-08")
temp1<- get_chirts(lonlat, dates, var = "Tmin" )



# This section gets the latitude and longitude of each firm
firm_locs <- df |>
  select(firms_group,
         latitude,
         longitude) |>
  distinct()

# This section prepares the lat/long combinations to get CHIRTS/CHIRPS data for.
# The CHIRPS functions will use the first column in this data frame as the 
# **longitude** and the second column as the **latitude**.
# It does not matter what they are named, but it is very important that they are
# in the correct order! 

locs_to_pull <- firm_locs |>
  select(longitude, 
         latitude) |>
  distinct()

# Download CHIRTS/CHIRPS data ####
# Set up multisession workers to run in parallel.
# More workers will speed things up, but too many may crash your computer.
plan(multisession, workers = 15)

# Download tmax, tmin, and precip data
# The system.time here just prints out how long this code takes to run.
# You can increase the number of years, e.g. (1985:1990), but it may get slow.

system.time(tmaxes <-
              lapply(c(2015:2017), # here you can select any range of years
                     get_annual_met, 
                     locs = locs_to_pull, # here you can substitute any data frame
                                          # with columns for LONGITUDE, LATITUDE
                                          # in that order
                     var = "Tmax"))

system.time(tmins <-
              lapply(c(2015:2018), 
                     get_annual_met, 
                     locs = locs_to_pull, 
                     var = "Tmin"))

system.time(annual_precip <-
              lapply(c(2015:2018), 
                     get_annual_met, 
                     locs = locs_to_pull, 
                     var = "Precip"))

# To get all the years of data (e.g. 1985:2016), if the above takes too long, 
# you could break it into pieces like so.
# This will save each piece as a .csv file, so if your R session crashes
# you can still re-load the data and start from where you left off.
# 
 ##tmaxes_1 <- lapply(c(2015:2018), get_annual_met, locs = locs_to_pull, var = "Tmax")
 ##write.csv(tmaxes_1, "tmaxes_1.csv", row.names = F)
#
# tmaxes_2 <- lapply(c(1996:2006), get_annual_met, locs = locs_to_pull, var = "Tmax")
# write.csv(tmaxes_2, "tmaxes_2.csv", row.names = F)
#
# tmaxes_3 <- lapply(c(2007:2016), get_annual_met, locs = locs_to_pull, var = "Tmax")
# write.csv(tmaxes_3, "tmaxes_3.csv", row.names = F)
#
# all_tmaxes <- bind_rows(tmaxes_1, tmaxes_2) |>
#   bind_rows(tmaxes_3)

# Close down multisession workers
# This is important to save your computer memory.
plan(sequential)

# Join CHIRPS/CHIRTS data to the parish locs data #### 
tmaxes <- tmaxes |>
  # bind_rows stacks all of the yearly tmax data frames into one big data frame
  bind_rows() |> 
  # update the lat/lon names from CHIRTS to match their names in parish_locs
  rename(max_latitude = lat, 
         max_longitude = lon) |>
  # join to parish_locs, which has the parish group names and lat/lon for all the parishes
  right_join(parish_locs, relationship = "many-to-many") |>
  # sort by parish and year, just to make it easier to look at
  arrange(parish_group, year)

tmins <- tmins |>
  bind_rows() |>
  rename(max_latitude = lat, 
         max_longitude = lon) |>
  right_join(parish_locs, relationship = "many-to-many") |>
  arrange(parish_group, year)

annual_precip <- annual_precip |>
  bind_rows() |>
  rename(max_latitude = lat, 
         max_longitude = lon) |>
  right_join(parish_locs, relationship = "many-to-many") |>
  arrange(parish_group, year)

all_met <- parish_locs |>
  left_join(tmaxes) |>
  left_join(tmins) |>
  left_join(annual_precip)

# Save all_met to .csv
write.csv(all_met, "parish_chirts_data.csv", row.names = F)

# save.image will save everything in your environment to "temps.RData".
# I do not recommend it for your actual data workflow.
save.image("temps.RData")
