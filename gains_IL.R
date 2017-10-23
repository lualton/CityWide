# Gains, or Pace measurement
# Gains" -- which are defined as the coefficient on a linear time trend (year)
#     in the following equation: %proficient ~ year + e
#

# Used to identify whether schools are keeping pace with the state average


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

# Do twice
data$subject <- data$math
data$subject <- data$read

### (3) GAINS -------------------------------------------------------------

library(plm)
library(lmtest)
library(lme4)
library(arm)

get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}

cluster.se <- function(model, cluster) {
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef <-coeftest(model, vcovCL)
  w <- waldtest(model, vcov = vcovCL, test = "F")
  ci <- get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}

# data$seasch <- as.numeric(as.character(data$seasch))

model <- data[,c("seasch","subject","propfrl","propwhite","propblack","prophisp",
                 "propasian","urban","level","enroll","year","city.new","ischarter")]

# mdata <- na.omit(model)
mdata <- model
mdata$time <- mdata$year - min(mdata$year)

mdata <- data.table(mdata, key="year")
mdata[,subject.std:=scale(subject),by=year]

data1 <- mdata[which(mdata$city.new==city[1]),]

missingValues <- colnames(data1)[colSums(is.na(data1)) > nrow(data1)/2]
urban <- length(unique(data1$urban))
level <- length(unique(data1$level))

if(urban == 1){
  missingValues <- append(missingValues, "factor(urban)")
}

if(level == 1){
  missingValues <- append(missingValues, "factor(level)")
}

factors <- c("time", "propwhite", "propfrl", "propblack", "prophisp",
             "propasian", "factor(urban)", "factor(level)", "enroll")

factors <- factors [! factors %in% missingValues]

equation <- as.formula(paste("subject.std~", paste(factors, collapse="+")))

runGainsModel <- function(se1, dataframe){
  
  b1.adj <- se1[[1]][2,1]
  p1.adj <- se1[[1]][2,4]
  
  gains.b.adj <- c(b1.adj) #beta for year 
  gains.p.adj <- c(p1.adj) #p-value
  
  gains <- data.frame(cbind(city,gains.b.adj,gains.p.adj))
  names(gains)<- c("City","gains","p_value")
  return(gains)
}

# alias(lm(equation, data=dataframe))
data1_charter <- filter(data1, ischarter == "1")
data1_charter <- dplyr::select(data1_charter, -propfrl, -urban)
lm1_charter <- lm(equation, data = data1_charter)
se1_charter <- cluster.se(lm1_charter, data1_charter$seasch) 

gains_charter <- runGainsModel(se1_charter, data1_charter)

data1_nocharter <- filter(data1, ischarter == "0")
data1_nocharter <- dplyr::select(data1_nocharter, -propfrl, -urban)
lm1_nocharter <- lm(equation, data = data1_nocharter)
se1_nocharter <- cluster.se(lm1_nocharter, data1_nocharter$seasch) 

gains_nocharter <- runGainsModel(se1_nocharter, data1_nocharter) 

data1_all <- dplyr::select(data1, -propfrl, -urban)
lm1_all <- lm(equation, data = data1_all)
se1_all <- cluster.se(lm1_all, data1_all$seasch)

gains_all <- runGainsModel(se1_all, data1_all)


gains_charter$ischarter <- "1"
gains_nocharter$ischarter <- "0"
gains_all$ischarter <- "All"

gains <- rbind(gains_all, gains_charter, gains_nocharter)

# write.csv(name)
    