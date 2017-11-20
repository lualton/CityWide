#### PREP THE DATA
# AR Performance

rm(list=ls()) 

# Load packages
library(plyr)
# library(utils)
library(data.table)
## NOTE: need to restore old version of data.table (R was updated, and with it data.table, 
# but old R version still on CSDE TS)
options(datatable.old.bywithoutby=TRUE)
library(dtplyr)
# library(nlme)
library(dplyr)
library(tidyr)
library(gdata)
library(stringr)


# Set state-specific objects
## data file
setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide/LA") # if on PC
# setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/LA") # if on Mac 

# 2016 ----------------------------------------------------

data <- read.xls("spring-2016-state-district-and-school-achievement-level-summary.xls",
                 sheet = 1,
                 perl = 'perl',
                 verbose = verbose)


# Merging ---------------------------------------------------
data <- read.csv("Louisianaperformance.csv")
enroll <- read.csv("LAenroll.csv")

glimpse(data)

glimpse(enroll)

for(i in 1:17){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}


for(i in 1:19){
  if(class(enroll[,i]) == "factor")
    enroll[,i] <- as.character(enroll[,i])
}

data <- select(data, -X)
enroll <- select(enroll, -X)

df <- inner_join(data, enroll, by = c("school_id", "year"))
test <- anti_join(data, enroll, by = c("school_id", "year"))

saveRDS(df, "LA_Complete.Rdata")
