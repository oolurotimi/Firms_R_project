#### Author: osaretin Olurotimi
##Precursor code: CRC Download

# Purpose: This scripts generates summary variables for temp variables by each firm


# Import extracted climate data
precip <- read.csv("Precipitation Data.csv", header=TRUE, row.names=1, sep=",", check.names=FALSE) # Precipitation
temp <- read.csv("Temperature Data.csv", header=TRUE, row.names=1, sep=",", check.names=FALSE) # Mean monthly temperature

###Creeate annual total precipitation data for each firm
##Note: need to change this so as to have a longer run temperature
years <- 2011:2019
precip.year.total <- as.data.frame(sapply(years, function(x) rowSums(precip[, grep(x, names(precip))])))
names(precip.year.total) <- years # Rename columns in the new data frame object

##Calculating mean annual temperature
temp.year.mean <- as.data.frame(sapply(years, function(x) rowMeans(temp[, grep(x, names(temp))])))
names(temp.year.mean) <- years # Rename columns in the new data frame object

## Add overall long run mean
temp.year.mean$lonrun_temp<-rowMeans(temp.year.mean)

###Add a dummy variable if temp in year 2016 and 2017 greater than longrunmean temp


table(temp.year.mean$abovein2016)

temp.year.mean$abovein2017<-ifelse(temp.year.mean$"2017" >temp.year.mean$lonrun_temp,1,0)
temp.year.mean$abovein2016<-ifelse(temp.year.mean$"2016" >temp.year.mean$lonrun_temp,1,0)

##Combine annual temperratures with complete dataset

annual_temp<-cbind(locs_to_pull,temp.year.mean)
df_annualtemp<-left_join(df,annual_temp) 

write.csv(df_annualtemp, "firms_withannualtemp.csv", row.names = F)

years<-c(2011:2019)

df_annualtemp<- rename(df_annualtemp,"Year_2011"= "2011") %>%


write_dta(df_annualtemp,"data/firms_withannualtemp.dta",  label = attr(data, "label"))

###Test Regression in R

install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
#Next, load the packages into your R environment by running this code (you need to do this every time you restart R):
  
  library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

str(df_annualtemp)


##Outcome variables: otherexpenses, rawmaterials
###attempting to write a regresson model
f2ba.mod<-lm(otherexpenses ~ a20a+a20b+a20c +abovein2016+ abovein2017+ state-1+lonrun_temp, data=df_annualtemp)

summary(f2ba.mod)
