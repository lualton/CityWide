# CITYWIDE, STEPPING UP PROJECT
# LOAD Data

# Louisiana
# New Orleans


### PREP THE DATA

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

setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide/LA") # if on PC
setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/LA") # if on Mac 

data <- readRDS("LA_complete_analysis.Rdata")
#Charter 1 and yes = charter
# 2 and no = not charter
data$ischarter[data$ischarter == '1' | data$ischarter == 'Yes'] <- 1
data$ischarter[data$ischarter == '2' | data$ischarter == 'No'] <- 0
str(data)

# Change for each City - - - - - 
city <- "New Orleans city"
# city <- "Baton Rouge city"

# Years
# unique(data$year)
# data <- filter(data, year != 2011)
year <- c(2011,2012,2013,2014)
city_df <- data[which(data$city == city),]
data <- data[which(data$notschool== '0'),]


data$subject <- data$math
data$subject <- data$read
