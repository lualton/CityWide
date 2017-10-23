# Stagnant Measure
# Used to identify bottom 5% schools over range of years

# CRPE Measuring Up, City Wide Project
# Walton Foundation
# Alton Lu

# Code example


rm(list=ls()) 

# Load packages
library(foreign)
library(plyr)
library(utils)
#install.packages("data.table")
library(data.table)
## NOTE: need to restore old version of data.table (R was updated, and with it data.table, but old R version still on CSDE TS)
options(datatable.old.bywithoutby=TRUE)
library(dtplyr)
library(nlme)
library(dplyr)
library(tidyr)

# Set state-specific objects
## data file

setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide/IL") # if on PC
setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/IL") # if on Mac 

data <- readRDS("IL_complete_analysis.Rdata")
str(data)

#Charter 1 and yes = charter
# 2 and no = not charter
data$ischarter[data$ischarter == '1' | data$ischarter == 'Yes'] <- 1
data$ischarter[data$ischarter == '2' | data$ischarter == 'No'] <- 0

unique(data$year)

table(data$year)
data <- filter(data, notschool == 0)

# Change for each City - - - - - 
city <- "Chicago city"

# Years
# unique(data$year)
data <- filter(data, year != 2011)
year <- c(2012,2013,2014,2015)
city_df <- data[which(data$city == city),]
data <- data[which(data$notschool== '0'),]

####################################################################
####################################################################

##### RUN TWICE:
##### 1st RUN = MATH - - - - -
data$subject <- data$math

data$subject <- data$read


### (2) STAGNANCY MEASURE --------------------------------------------------------------

myvars <- c("year", "seasch", "city.new", "enroll", "subject", "ischarter")

stdata <- data[myvars]

stdata <- na.omit(stdata)

# # # # # - - - - - 
# stdata <- filter(stdata, ischarter == '1')
# stdata <- filter(stdata, ischarter == '0')

getListStuck <- function(dataframe){
  
  stdata <- dataframe
  
  # Find cutoffs for bottom 5% of state
  pct5a <- quantile(stdata$subject[stdata$year==year[1]], c(0.05))
  pct5b <- quantile(stdata$subject[stdata$year==year[2]], c(0.05))
  pct5c <- quantile(stdata$subject[stdata$year==year[3]], c(0.05))
  pct5d <- quantile(stdata$subject[stdata$year==year[4]], c(0.05))
  #pct5e <- quantile(stdata$subject[stdata$year==year[5]], c(0.05))
  
  # Mark which schools are in bottom 5% of state
  stdata$stuck <- NA # creates new variables - assigns NA
  stdata$stuck[stdata$year==year[1]] <- ifelse(stdata$subject[stdata$year==year[1]]<=pct5a, 1, 0)
  stdata$stuck[stdata$year==year[2]] <- ifelse(stdata$subject[stdata$year==year[2]]<=pct5b, 1, 0)
  stdata$stuck[stdata$year==year[3]] <- ifelse(stdata$subject[stdata$year==year[3]]<=pct5c, 1, 0)
  stdata$stuck[stdata$year==year[4]] <- ifelse(stdata$subject[stdata$year==year[4]]<=pct5d, 1, 0)
  #stdata$stuck[stdata$year==year[5]] <- ifelse(stdata$subject[stdata$year==year[5]]<=pct5e, 1, 0)
  
  # Set cities and time variable
  stdata <- stdata[which(stdata$city.new==city[1]), ]
  
  stdata$year <- stdata$year - min(stdata$year) + 1
  stdata$time[stdata$year==1] <- "Y1"
  stdata$time[stdata$year==2] <- "Y2"
  stdata$time[stdata$year==3] <- "Y3"
  stdata$time[stdata$year==4] <- "Y4"
  
  # Create variable to count schools
  stdata$temp <- 1
  
  # Find number of students & schools, overall & in bottom 5% of state
  stag <- ddply(stdata, .(city.new, time), function(x)
    c(enrolltotal=sum(x$enroll),
      enrollstuck=sum(x[which(x$stuck==1),]$enroll),
      schooltotal=sum(x$temp),
      schoolstuck=sum(x[which(x$stuck==1),]$temp) ) )
  
  # Calculate percent of students & schools in bottom 5% of state
  stag$stuck.stu <- stag$enrollstuck / stag$enrolltotal
  stag$stuck.sch <- stag$schoolstuck / stag$schooltotal
  
  # Create dataframe of above results
  vars <- c("city.new", "time", "stuck.stu", "stuck.sch")
  stag <- stag[vars]
  
  # Find share of schools in Y1 that were stuck for all years
  ## mark if stuck in y1
  stdata$iny1 <- NA
  stdata$iny1 <- ifelse(stdata$year==1 & stdata$stuck==1, 1, 0)
  
  # Adding in a value for if the school existed in the dataset - Alton
  stdata$present <- 1
  
  stdata <- filter(stdata, time != "NA")
  
  ## collapse by school
  glimpse(stdata) # check if there are missing data in stdata
  
  change <- ddply(stdata, .(seasch, city.new), function(x)
    c(stuckinyr1=sum(x$iny1, na.rm = TRUE),
      stucktotal=sum(x$stuck, na.rm = TRUE),
      yearsin = sum(x$present, na.rm = TRUE))) # This number will show if school left data early - Alton
  
  change$outStuck <- change$yearsin - change$stucktotal
  #yearsin represents the number of years a school is in the city
  
  ## subset to include only schools stuck in y1
  change <- change[which(change$stuckinyr1==1),]
  ##mark if stuck in all years 
  if(nrow(change) == 0) { stop("No city schools in bottom 5% in Year One")}
  change$all <- NA
  change$all <- ifelse(change$stucktotal==length(year), 1, 0) ###
  ###Georgia request, find the porportion of schools that were stuck in year 1 and that never returned to the bottom....
  change$only1 <- ifelse(change$stucktotal==1, 1, 0)
  return(as.data.frame(change))
}

stdata_charter <- filter(stdata, ischarter == '1')
change_charter <- getListStuck(stdata_charter)

stdata_nocharter <- filter(stdata, ischarter == '0')
change_nocharter <- getListStuck(stdata_nocharter)

change_all <- getListStuck(stdata)

collapseStuckList <- function(dataframe){
  
  dataframe <- change_nocharter
  
  ## collapse by city
  change2 <- ddply(dataframe, .(city.new), function(x)
    c(stuck.sch.y1=sum(x$stuckinyr1),
      stuck.sch.all=sum(x$all)))
  
  ## calculate percentage of schools stuck in y1 that were stuck in all years
  change2$stuck.sch.y1all <- change2$stuck.sch.all / change2$stuck.sch.y1
  
  change2$Stuckonly_yr1 <- mean(dataframe$only1)
  return(as.data.frame(change2))
}

# Stuck Total
change_all <- collapseStuckList(change_all)
change_all$ischarter <- "All"

change_charter_total <- collapseStuckList(change_charter)
change_charter_total$ischarter <- '1'

change_nocharter_total <- collapseStuckList(change_nocharter)
change_nocharter_total$ischarter <- '0'

change2 <- rbind(change_all, change_nocharter_total)

# write.csv(name)
