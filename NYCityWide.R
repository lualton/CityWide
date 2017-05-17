# ### PREP THE DATA
# New York Performance

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
setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide/NY") # if on PC
# setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/NY") # if on Mac 

# 2016 data --------------------------------------------------------------
data <- read.csv("2016 Perf.csv", na.strings = "-") # 436464

glimpse(data)

for(i in 1:23){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data$L1_PCT <- as.numeric(sub("%", "", data$L1_PCT))
data$L2_PCT <- as.numeric(sub("%", "", data$L2_PCT))
data$L3_PCT <- as.numeric(sub("%", "", data$L3_PCT))
data$L4_PCT <- as.numeric(sub("%", "", data$L4_PCT))
data$L2.L4_PCT <- as.numeric(sub("%", "", data$L2.L4_PCT))
data$L3.L4_PCT <- as.numeric(sub("%", "", data$L3.L4_PCT))

data <- filter(data, SUBGROUP_NAME == "All Students")
data <- filter(data, COUNTY_CODE != "NA")
data <- filter(data, NRC_CODE != "NA")

data$TYPE <- word(data$NAME, -1)

unique(data$TYPE)

data <- filter(data, TYPE != "DISTRICT")

backup <- data
# data <- backup

data <- ddply(data, c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                      "BEDSCODE", "NAME", "ITEM_SUBJECT_AREA"),
              summarise,
              TOTAL_TESTED = sum(TOTAL_TESTED, na.rm = TRUE),
              L1_PCT = mean(L1_PCT, na.rm = TRUE),
              L2_PCT = mean(L2_PCT, na.rm = TRUE),
              L3_PCT = mean(L3_PCT, na.rm = TRUE),
              L4_PCT = mean(L4_PCT, na.rm = TRUE),
              L2.L4_PCT = mean(L2.L4_PCT, na.rm = TRUE),
              L3.L4_PCT = mean(L3.L4_PCT, na.rm = TRUE))

data[,9:14] <- round(data[,9:14], digits = 1)

data <- reshape(data,
                timevar = "ITEM_SUBJECT_AREA",
                idvar = c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                          "BEDSCODE", "NAME"),
                direction = "wide")

data$YEAR <- 2016

df <- data


# 2015 -------------------------------------------------------
data <- read.csv("2015 Perf.csv", na.strings = '-') # 65534

glimpse(data)

for(i in 1:24){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data <- filter(data, SUBGROUP_NAME == "All Students")
data <- filter(data, COUNTY_CODE != "NA")
data <- filter(data, NRC_CODE != "NA")

data$SCHOOL_YEAR <- 2015
unique(data$NRC_DESC)

data <- ddply(data, c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                      "BEDSCODE", "NAME", "ITEM_SUBJECT_AREA"),
              summarise,
              TOTAL_TESTED = sum(TOTAL_TESTED, na.rm = TRUE),
              L1_PCT = mean(L1_PCT, na.rm = TRUE),
              L2_PCT = mean(L2_PCT, na.rm = TRUE),
              L3_PCT = mean(L3_PCT, na.rm = TRUE),
              L4_PCT = mean(L4_PCT, na.rm = TRUE),
              L2.L4_PCT = mean(L2.L4_PCT, na.rm = TRUE),
              L3.L4_PCT = mean(L3.L4_PCT, na.rm = TRUE))

data[,9:14] <- round(data[,9:14], digits = 1)

data <- reshape(data,
                timevar = "ITEM_SUBJECT_AREA",
                idvar = c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                          "BEDSCODE", "NAME"),
                direction = "wide")

data$YEAR <- 2015

df <- rbind(df, data)

unique(df$YEAR)

# 2014 -----------------------------------------------------
data <- read.csv("2014 Perf.csv", na.strings = '-') # 435946

glimpse(data)

for(i in 1:24){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data <- filter(data, SUBGROUP_NAME == "All Students")
data <- filter(data, COUNTY_CODE != "NA")
data <- filter(data, NRC_CODE != "NA")

data <- ddply(data, c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                      "BEDSCODE", "NAME", "ITEM_SUBJECT_AREA"),
              summarise,
              TOTAL_TESTED = sum(TOTAL_TESTED, na.rm = TRUE),
              L1_PCT = mean(L1_PCT, na.rm = TRUE),
              L2_PCT = mean(L2_PCT, na.rm = TRUE),
              L3_PCT = mean(L3_PCT, na.rm = TRUE),
              L4_PCT = mean(L4_PCT, na.rm = TRUE),
              L2.L4_PCT = mean(L2.L4_PCT, na.rm = TRUE),
              L3.L4_PCT = mean(L3.L4_PCT, na.rm = TRUE))

data[,9:14] <- round(data[,9:14], digits = 1)

data <- reshape(data,
                timevar = "ITEM_SUBJECT_AREA",
                idvar = c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                          "BEDSCODE", "NAME"),
                direction = "wide")

data$YEAR <- 2014

df <- rbind(df, data)
unique(df$YEAR)

# 2013 data ----------------------------------------------------
data <- read.csv("2013 Perf.csv", na.strings = '-') 

glimpse(data)

for(i in 1:24){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data <- filter(data, SUBGROUP_NAME == "All Students")
data <- filter(data, COUNTY_CODE != "NA")
data <- filter(data, NRC_CODE != "NA")


data <- ddply(data, c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                      "BEDSCODE", "NAME", "ITEM_SUBJECT_AREA"),
              summarise,
              TOTAL_TESTED = sum(TOTAL_TESTED, na.rm = TRUE),
              L1_PCT = mean(L1_PCT, na.rm = TRUE),
              L2_PCT = mean(L2_PCT, na.rm = TRUE),
              L3_PCT = mean(L3_PCT, na.rm = TRUE),
              L4_PCT = mean(L4_PCT, na.rm = TRUE),
              L2.L4_PCT = mean(L2.L4_PCT, na.rm = TRUE),
              L3.L4_PCT = mean(L3.L4_PCT, na.rm = TRUE))

data[,9:14] <- round(data[,9:14], digits = 1)

data <- reshape(data,
                timevar = "ITEM_SUBJECT_AREA",
                idvar = c("NRC_CODE","NRC_DESC", "COUNTY_CODE","COUNTY_DESC",
                          "BEDSCODE", "NAME"),
                direction = "wide")

data$YEAR <- 2013

df <- rbind(df, data)

# Checking after merge
data <- df
glimpse(data)
summary(data)
unique(data$YEAR)


saveRDS(data, "NY.PerformanceData.Rdata")

# Merge Data -------------------------------------------------
data <- readRDS("NY.PerformanceData.Rdata")

glimpse(data)

enroll <- read.csv("NYenrollment.csv")

glimpse(enroll)

for(i in 1:19){
  if(class(enroll[,i]) == "factor")
    enroll[,i] <- as.character(enroll[,i])
}

enroll <- select(enroll, -X)
unique(enroll$year)
unique(data$year)

# County_desc = county name
# Bedscode = school_id

names(data)[5] <- "school_id"
names(data)[4] <- "county_name"
names(data)[21] <- "year"

df <- inner_join(data, enroll, by = c("school_id", "county_name", "year"))
test <- anti_join(data, enroll, by = c("school_id", "county_name", "year"))
unique(test$year)

# All lost data - mostly all district aggregated
test <- filter(test, year == 2013 | year == 2014 | year == 2015)

saveRDS(df, "NY_Complete.Rdata")
