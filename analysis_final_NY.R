# ARNOLD INDICATORS USING city BOUNDARY
# ANALYSIS: Beating the odds, Stagnant, Gains, Top/bottom quintiles
# Original: Patrick Denise
# Updated: Jose Hernandez
# 02/15/17
# 03/7/17

# (1) "Beating the odds" -- where look at the residuals of a relatively 
#     simple regression: 
#     %proficient ~ %white + %black + %hispanic + %frl + urban + schoollevel + schoolsize + e
#     Models are run separately by year. Schools are "beating the odds" if the 
#     residual, e, is negative (expected is lower than observed) and its 95% 
#     confidence interval of residual does not contain zero.
#     Reference: http://files.eric.ed.gov/fulltext/ED544802.pdf
# (2) "Stagnant measures" -- schools stuck in bottom 5% of state
# (3) "Gains" -- which are defined as the coefficient on a linear time trend (year)
#     in the following equation: %proficient ~ year + e
# (4) "Top/bottom quintiles" -- I flag schools in the bottom and top quintiles within
#     within each city, then find the proportion of students in each city (overall, 
#     and by FRL/non-FRL & white/black/latino) in those quintiles

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

setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide/NY") # if on PC
setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/NY") # if on Mac 

data <- readRDS("NY_complete_analysis.Rdata")
str(data)

unique(data$year)


# Change for each City - - - - - 
city <- "New York city"

# Years
# unique(data$year)
# data <- filter(data, year != 2011)
year <- c(2012,2013,2014,2015)
city_df <- data[which(data$city == city),]
data <- data[which(data$notschool== '0'),]

####################################################################
####################################################################

##### RUN TWICE:
##### 1st RUN = MATH - - - - -
data$subject <- data$math

namelist <- c("MathschoolsStuckNewYork.csv", "MathschoolsStuckDetailsNewYork.csv", 
              "MathproficiencyNewYork.csv", "MathgainsNewYork.csv",
              "MathTopBottomNewYork.csv", "MathBTONewYork.csv")

##### 2nd RUN = READ - - - - - 
# data$subject <- data$read

# namelist <- c("ELAschoolsStuckNewYork.csv", "ELAschoolsStuckDetailsNewYork.csv", 
#              "ELAproficiencyNewYork.csv", "ELAgainsNewYork.csv",
#              "ELATopBottomNewYork.csv", "ELABTONewYork.csv")


### (1) BEAT THE ODDS -----------------------------------------------------------

# Subset data
model <- data[,c("name","subject","propwhite","propblack","prophisp",
                  "propasian","urban","level","enroll","year","city.new","ischarter")]

mdata <- na.omit(model)

###Create data loop here...

data1 <- mdata[which(mdata$year==year[1]),]
data2 <- mdata[which(mdata$year==year[2]),]
data3 <- mdata[which(mdata$year==year[3]),]
data4 <- mdata[which(mdata$year==year[4]),]


# Run regressions by year, and save residuals
lm1 <- lm(subject ~  propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data1)
lm2 <- lm(subject ~  propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data2)
lm3 <- lm(subject ~  propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data3)
lm4 <- lm(subject ~  propwhite + propblack + prophisp + propasian + factor(urban) + factor(level) + enroll, data=data4)
summary(lm4)
# Calculate residuals' 90% confidence intervals
data1$res <- residuals(lm1)
data1$res.lo <- data1$res - (1.645*sd(data1$res))
data1$res.hi <- data1$res + (1.645*sd(data1$res))
data2$res <- residuals(lm2)
data2$res.lo <- data2$res - (1.645*sd(data2$res))
data2$res.hi <- data2$res + (1.645*sd(data2$res))
data3$res <- residuals(lm3)
data3$res.lo <- data3$res - (1.645*sd(data3$res))
data3$res.hi <- data3$res + (1.645*sd(data3$res))
#
data4$res <- residuals(lm4)
data4$res.lo <- data4$res - (1.645*sd(data4$res))
data4$res.hi <- data4$res + (1.645*sd(data4$res))

# Identify schools that beat the odds
data1$beat <- ifelse(data1$res>0 & data1$res.lo>0, 1, 
                     ifelse(data1$res<=0 | data1$res.lo<=0, 0, NA))
data2$beat <- ifelse(data2$res>0 & data2$res.lo>0, 1, 
                     ifelse(data2$res<=0 | data2$res.lo<=0, 0, NA))
data3$beat <- ifelse(data3$res>0 & data3$res.lo>0, 1, 
                     ifelse(data3$res<=0 | data3$res.lo<=0, 0, NA))
#
data4$beat <- ifelse(data4$res>0 & data4$res.lo>0, 1, 
                     ifelse(data4$res<=0 | data4$res.lo<=0, 0, NA))


# Keep only necessary variables
myvars <- c("city.new", "year", "subject", "beat", "ischarter", "enroll")
data1 <- data1[myvars]
data2 <- data2[myvars]
data3 <- data3[myvars]
data4 <- data4[myvars]

# Append years together
master <- rbind(data1, data2, data3, data4)

# Keep only cities of interest
master <- master[which(master$city.new %in% city),]

# Create categorical time variable
master$year <- master$year - min(master$year) + 1
master$time[master$year==1] <- "Y1"
master$time[master$year==2] <- "Y2"
master$time[master$year==3] <- "Y3"
master$time[master$year==4] <- "Y4"


# Create categorical sector variable NOt needed for CityMetrics
#master$sector[master$ischarter==0 | master$ischarter==2] <- "tps"
#master$sector[master$ischarter==1] <- "cs"

# Aggregate master data: find % of all schools and students beating the odds
master$temp <- 1
master.agg <- ddply(master, .(city.new, time), function(x)
  c(enrolltotal = sum(x$enroll),
    enrollbeat  = sum(x[which(x$beat==1),]$enroll),
    schooltotal = sum(x$temp),
    schoolbeat  = sum(x[which(x$beat==1),]$temp) ) )


master.agg$enrollbeat.pct = master.agg$enrollbeat / master.agg$enrolltotal
master.agg$schoolbeat.pct = master.agg$schoolbeat / master.agg$schooltotal
#Get the average BTO for both enrollments and schools...

master.2 <- master.agg %>%
            summarise(avgBTOEnrollment = mean(enrollbeat.pct),
                      avgBTOSchool = mean(schoolbeat.pct))

master.2$city <- city
#save file
write.csv(master.2, file=file6)

### (2) STAGNANCY MEASURE --------------------------------------------------------------

myvars <- c("year", "seasch", "city.new", "enroll", "subject")

stdata <- data[myvars]


stdata <- na.omit(stdata)
summary(stdata)

# Find cutoffs for bottom 5% of state
pct5a <- quantile(stdata$subject[stdata$year==year[1]], c(0.05))
pct5b <- quantile(stdata$subject[stdata$year==year[2]], c(0.05))
pct5c <- quantile(stdata$subject[stdata$year==year[3]], c(0.05))
pct5d <- quantile(stdata$subject[stdata$year==year[4]], c(0.05))

# Mark which schools are in bottom 5% of state
stdata$stuck <- NA
stdata$stuck[stdata$year==year[1]] <- ifelse(stdata$subject[stdata$year==year[1]]<=pct5a, 1, 0)
stdata$stuck[stdata$year==year[2]] <- ifelse(stdata$subject[stdata$year==year[2]]<=pct5b, 1, 0)
stdata$stuck[stdata$year==year[3]] <- ifelse(stdata$subject[stdata$year==year[3]]<=pct5c, 1, 0)
stdata$stuck[stdata$year==year[4]] <- ifelse(stdata$subject[stdata$year==year[4]]<=pct5d, 1, 0)


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
change$all <- NA
change$all <- ifelse(change$stucktotal==length(year), 1, 0) ###
## collapse by city
change2 <- ddply(change, .(city.new), function(x)
  c(stuck.sch.y1=sum(x$stuckinyr1),
    stuck.sch.all=sum(x$all)))

## calculate percentage of schools stuck in y1 that were stuck in all years
change2$stuck.sch.y1all <- change2$stuck.sch.all / change2$stuck.sch.y1

###Georgia request, find the porportion of schools that were stuck in year 1 and that never returned to the bottom....
change$only1 <- ifelse(change$stucktotal==1, 1, 0)
change2$Stuckonly_yr1 <- mean(change$only1)

write.csv(change2, file=namelist[1])

# For details about the stuck schools 
detailList <- change$seasch
test <- dplyr::select(city_df, seasch, name) # if doesn't work, just close r and reopen
test$seasch <- as.numeric(test$seasch)
detail <- join(change, test, by = c("seasch"), match = "first")
write.csv(detail, file=namelist[2])

### (3) GAINS -------------------------------------------------------------
## Adjusted gains measure with clustered standard errors (Chingos review, 5/4/15, + convo w/ BG, 5/6/15)
## NOTE: this measure replaces both the original (unadjusted) HLM model and the adjusted (non-HLM) model
#install.packages('plm')
#install.packages('lmtest')

library(plm)
library(lmtest)
#install.packages("multiwayvcov")
library(lme4)
#install.packages("arm")
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
  
  coef<-coeftest(model, vcovCL)
  w<-waldtest(model, vcov = vcovCL, test = "F")
  ci<-get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}

data$seasch <- as.numeric(as.character(data$seasch))

model <- subject ~ year + city.new + seasch + propfrl + propwhite + 
  propblack + prophisp + propasian + factor(urban) + factor(level) + enroll

model <- data[,c("seasch","subject","propfrl","propwhite","propblack","prophisp",
                  "propasian","urban","level","enroll","year","city.new","ischarter")]

model <- dplyr::select(model, -propfrl)

mdata <- na.omit(model)
mdata$time <- mdata$year - min(mdata$year)

mdata <- data.table(mdata, key="year")
mdata[,subject.std:=scale(subject),by=year]



### This is adding in year-by-year proficiency rates ### - Alton

# scaling, do we need to do it here?
profdata <- mdata

proficiency.all <- ddply(profdata, c("year"), summarise,
               mean = mean(subject, na.rm = TRUE),
               median = median(subject, na.rm = TRUE))

proficiency.all$type <- "all"

proficiency.city <- ddply(filter(profdata, city.new == city[1]), c("year"), summarise,
               mean = mean(subject, na.rm = TRUE),
               median = median(subject, na.rm = TRUE))

proficiency.city$type <- city

proficiency <- rbind(proficiency.all, proficiency.city)
proficiency

write.csv(proficiency, namelist[3])


data1 <- mdata[which(mdata$city.new==city[1]),]

# FRL removed for New York
equation <- subject.std ~ time + propwhite + 
  propblack + prophisp +propasian + factor(level) + enroll

lm1 <- lm(equation, data=data1)

se1 <- cluster.se(lm1, data1$seasch)


b1.adj <- se1[[1]][2,1]
p1.adj <- se1[[1]][2,4]


gains.b.adj <- c(b1.adj) #beta for year 
gains.p.adj <- c(p1.adj) #p-value

gains <- data.frame(cbind(city,gains.b.adj,gains.p.adj))
names(gains)<- c("City","gains","p_value")

write.csv(gains, namelist[4])


### (4) TOP/BOTTOM QUINTILES --------------------------------------------------

myvars <- c("year", "seasch", "city.new", "level", 
            "subject", "enroll", "numfrl", "numwhite", "numblack", "numhisp")

qdata <- data[myvars]


qdata$year <- qdata$year - min(qdata$year) + 1
qdata$time[qdata$year==1] <- "Y1"
qdata$time[qdata$year==2] <- "Y2"
qdata$time[qdata$year==3] <- "Y3"
qdata$time[qdata$year==4] <- "Y4"


qdata <- na.omit(qdata)

qdata <- qdata[which(qdata$city.new==city[1]), ]

qdata$numnonfrl <- qdata$enroll - qdata$numfrl

quint <- ddply(qdata, .(city.new, time), function(x) 
  c(p20=quantile(x$subject, c(0.20)),
    p80=quantile(x$subject, c(0.80)),
    enroll=sum(x$enroll),
    numfrl=sum(x$numfrl),
    numnonfrl=sum(x$numnonfrl),
    numwhite=sum(x$numwhite),
    numblack=sum(x$numblack),
    numhisp=sum(x$numhisp),
    p20.enroll=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$enroll),
    p80.enroll=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$enroll), 
    p20.frl=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numfrl),
    p80.frl=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numfrl), 
    p20.nonfrl=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numnonfrl),
    p80.nonfrl=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numnonfrl), 
    p20.white=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numwhite),
    p80.white=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numwhite), 
    p20.black=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numblack),
    p80.black=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numblack), 
    p20.hisp=sum(x[which(x$subject<=quantile(x$subject, c(0.20))),]$numhisp),
    p80.hisp=sum(x[which(x$subject>=quantile(x$subject, c(0.80))),]$numhisp) ) )


colnames(quint)[1] <- "city"
colnames(quint)[3] <- "p20"
colnames(quint)[4] <- "p80"

quint$diff8020 <- round((quint$p80 - quint$p20), digits=2)
quint$ratio8020 <- round((quint$p80 / quint$p20), digits=2)

quint$p20.enroll <- round((quint$p20.enroll / quint$enroll), digits=4)*100
quint$p80.enroll <- round((quint$p80.enroll / quint$enroll), digits=4)*100
quint$p20.frl <- round((quint$p20.frl / quint$numfrl), digits=4)*100
quint$p80.frl <- round((quint$p80.frl / quint$numfrl), digits=4)*100
quint$p20.nonfrl <- round((quint$p20.nonfrl / quint$numnonfrl), digits=4)*100
quint$p80.nonfrl <- round((quint$p80.nonfrl / quint$numnonfrl), digits=4)*100
quint$p20.white <- round((quint$p20.white / quint$numwhite), digits=4)*100
quint$p80.white <- round((quint$p80.white / quint$numwhite), digits=4)*100
quint$p20.black <- round((quint$p20.black / quint$numblack), digits=4)*100
quint$p80.black <- round((quint$p80.black / quint$numblack), digits=4)*100
quint$p20.hisp <- round((quint$p20.hisp / quint$numhisp), digits=4)*100
quint$p80.hisp <- round((quint$p80.hisp / quint$numhisp), digits=4)*100

vars <- c("city", "time", "p20", "p80", "p20.enroll",
          "p80.enroll", "p20.frl", "p80.frl", "p20.nonfrl",
          "p80.nonfrl", "p20.white", "p80.white", "p20.black", 
          "p80.black", "p20.hisp", "p80.hisp", "diff8020", "ratio8020")
quint <- quint[vars]


write.csv(quint, file=namelist[5])

