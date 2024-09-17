# Extracting CRU Climate data: CRU TS v4.01
# Complete guide available at: http://www.benjaminbell.co.uk
##CRU data downloaded from https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.04/
# Load packages
library(raster)
library(ncdf4)
library(tidyverse)
library(dplyr)
library(haven)

setwd("/Users/OsaretinOlurotimi2/Dropbox/Mac/Desktop/Climate and Micro firms/Firms_R_project/Data/CRU Climate data")
getwd()

# Open CRU TMX file near urface Temperature Maxiumum
nc.tmx <- nc_open("cru_ts4.04.2011.2019.tmx.dat.nc")
print(nc.tmx)

##Load the tmx variable into R

tmx <- brick("cru_ts4.04.2011.2019.tmx.dat.nc", varname="tmx") #  monthly average daily maximum temperatur
# Load the CRU TS datasets into R 
pre <- brick("cru_ts4.04.2011.2019.pre.dat.nc", varname="pre") # Precipitation
tmp <- brick("cru_ts4.04.2011.2019.tmp.dat.nc", varname="tmp") # Mean monthly temperature
cld <- brick("cru_ts4.04.2011.2019.cld.dat.nc", varname="cld") # % cloud cover

#ssh <- brick("cru_ts4.04.2011.2019.ssh.dat.nc", varname="tmp") # Sunshine duration (hours)
#rhm <- brick("cru_ts4.04.2011.2019.rhm.dat.nc", varname="tmp") # Relative Humidity (%)

# Read in firms data
# Again, if you're not in an R project, 
# you may need to modify the path.
df <- read_dta(here::here("climateandfirms.dta"))

# Assign unique IDs to firm x date 
df <- df %>%
  # firms_group is a unique id for firms
  # to add additional firm identifiers, add column names to group_by
  group_by(obs_id, surveydate) |>
  mutate(firms_group = cur_group_id()) |>
  ungroup()



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


# Extract climate data from the RasterBrick as a data.frame using the raster package(may need to uncheck tidyr)
tmx.sites <- data.frame(extract(tmx, firm_locs, ncol=2))
pre.sites <- data.frame(extract(pre, locs_to_pull, ncol=2))
cld.sites <- data.frame(extract(cld, locs_to_pull, ncol=2))



#Merge with locs_to_pull

# Change column names
years <- 1980:2019
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(tmx.sites) <- paste(rep(years, each=12), rep(month, times=9), sep="_Tmx")
names(pre.sites) <- paste(rep(years, each=12), rep(month, times=9), sep="_Pre")
names(cld.sites) <- paste(rep(years, each=12), rep(month, times=9), sep="_Cld")

##Combine temp_max with GPS information of firms
monthly_temp<-cbind(locs_to_pull,tmx.sites)
monthly_rain<-cbind(locs_to_pull,pre.sites)
monthly_cloudcover <- cbind(locs_to_pull,cld.sites)

##Combine tenp_max with firm data

df_temp<-left_join(df,monthly_temp) %>%
          left_join(monthly_rain)

write.csv(df_temp,"firms_withtemprain.csv", row.names = F)
write_dta(df_temp,"firms_withtemprain.dta" )



###Saving Extracted data

names(tmx.sites) <- paste(rep(years, each=12), rep(month, times=9), sep="_")
names(pre.sites) <- paste(rep(years, each=12), rep(month, times=9), sep="_")
names(cld.sites) <- paste(rep(years, each=12), rep(month, times=9), sep="_")
write.csv(pre.sites, file="Precipitation Data.csv")
write.csv(tmx.sites, file="Temperature Data.csv")