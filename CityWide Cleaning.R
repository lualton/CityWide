# ### PREP THE DATA

rm(list=ls()) 

# Load packages
library(foreign)
library(plyr)
library(utils)
library(data.table)
## NOTE: need to restore old version of data.table (R was updated, and with it data.table, 
# but old R version still on CSDE TS)
options(datatable.old.bywithoutby=TRUE)
library(dtplyr)
library(nlme)
library(dplyr)
library(tidyr)

# Set state-specific objects
## data file
setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide")
# setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide") - if on Mac 

# New Jersey ----------------------------------------------------------------------------------
# 2015 Performance Data
nj <- read.csv("NJ 2015 PARC.csv")

# change from factor to character
for(i in 1:14){
  if(class(nj[,i]) == "factor")
    nj[,i] <- as.character(nj[,i])
}

nj$GRADE <- nj$TESTPRG

nj$GRADE <- as.numeric(sub("NJASK", "", nj$GRADE))
nj$SUBJECT[nj$SUBJECT == "PARCC MATH"] <- "Math"
nj$SUBJECT[nj$SUBJECT == "PARCC ELA"] <- "ELA"

nj <- filter(nj, TYPE == "Schoolwide")

# Go look at MO data CRPE Github
data <- ddply(nj, c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "SUBJECT"),
              summarise,
              TESTED = sum(VALID.SCORES, na.rm = TRUE),
              Mean = mean(MEAN.SCORE, na.rm = TRUE),
              Level1 = mean(LEVEL_1, na.rm = TRUE),
              Level2 = mean(LEVEL_2, na.rm = TRUE),
              Level3 = mean(LEVEL_3, na.rm = TRUE),
              Level4 = mean(LEVEL_4, na.rm = TRUE),
              Level5 = mean(LEVEL_5, na.rm = TRUE),
              MET_EXCEED = mean(MET_EXCEED, na.rm = TRUE))

data[,6:12] <- round(data[,6:12], digits = 1)

data <- reshape(data,
                timevar = "SUBJECT",
                idvar = c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE"),
                direction = "wide")

data$YEAR <- 2015
df <- data

# write.csv(data, "CleanedNJParc.csv")


nameslist <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "TESTPRG", "SUBJECT",
               "ALL.PARTIAL", "ALL.PROFICIENT", "ALL.ADVANCED",
               "WHT.PARTIAL", "WHT.PROFICIENT", "WHT.ADVANCED",
               "BLK.PARTIAL", "BLK.PROFICIENT", "BLK.ADVANCED",
               "ASN.PARTIAL", "ASN.PROFICIENT", "ASN.ADVANCED",
               "AIN.PARTIAL", "AIN.PROFICIENT", "AIN.ADVANCED",
               "HIS.PARTIAL", "HIS.PROFICIENT", "HIS.ADVANCED",
               "OTR.PARTIAL", "OTR.PROFICIENT", "OTR.ADVANCED",
               "DIS.PARTIAL", "DIS.PROFICIENT", "DIS.ADVANCED",
               "LEP.PARTIAL", "LEP.PROFICIENT", "LEP.ADVANCED",
               "EDS.PARTIAL", "EDS.PROFICIENT", "EDS.ADVANCED")


#2012
data <- read.csv("NJ 2011-2012 performance.csv")
names(data) <- nameslist

for(i in 1:35){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data$GRADE <- data$TESTPRG
data$GRADE <- as.numeric(sub("NJASK", "", data$GRADE))
data$YEAR <- 2012

#2013
data1 <- read.csv("NJ 2012-2013 performance.csv")
names(data1) <- nameslist

for(i in 1:35){
  if(class(data1[,i]) == "factor")
    data1[,i] <- as.character(data1[,i])
}

data1$GRADE <- data1$TESTPRG
data1$GRADE <- as.numeric(sub("NJASK", "", data1$GRADE))
data1$YEAR <- 2013

#2014
data2 <- read.csv("NJ 2013-2014 performance.csv")
names(data2) <- nameslist

for(i in 1:35){
  if(class(data2[,i]) == "factor")
    data2[,i] <- as.character(data2[,i])
}

data2$GRADE <- data2$TESTPRG
data2$GRADE <- as.numeric(sub("NJASK", "", data2$GRADE))
data2$YEAR <- 2014

data <- rbind(data, data1, data2)

# Clean out some stuff from data

data <- filter(data, SUBJECT != "Science")
data <- data[c(1:5, 36:37, 6:35)]

data <- ddply(data, c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "YEAR", "SUBJECT"),
              summarise,
              ALL.PARTIAL = mean(ALL.PARTIAL, na.rm = TRUE), 
              ALL.PROFICIENT = mean(ALL.PROFICIENT, na.rm = TRUE),
              ALL.ADVANCED = mean(ALL.ADVANCED, na.rm = TRUE),
              WHT.PARTIAL = mean(WHT.PARTIAL, na.rm = TRUE), 
              WHT.PROFICIENT = mean(WHT.PROFICIENT, na.rm = TRUE), 
              WHT.ADVANCED = mean(WHT.ADVANCED, na.rm = TRUE),
              BLK.PARTIAL = mean(BLK.PARTIAL, na.rm = TRUE),
              BLK.PROFICIENT = mean(BLK.PROFICIENT, na.rm = TRUE),
              BLK.ADVANCED = mean(BLK.ADVANCED, na.rm = TRUE),
              ASN.PARTIAL = mean(ASN.PARTIAL, na.rm = TRUE),
              ASN.PROFICIENT = mean(ASN.PROFICIENT, na.rm = TRUE), 
              ASN.ADVANCED = mean(ASN.ADVANCED, na.rm = TRUE),
              AIN.PARTIAL = mean(AIN.PARTIAL, na.rm = TRUE),
              AIN.PROFICIENT = mean(AIN.PROFICIENT, na.rm = TRUE),
              AIN.ADVANCED = mean(AIN.ADVANCED, na.rm = TRUE),
              HIS.PARTIAL = mean(HIS.PARTIAL, na.rm = TRUE),
              HIS.PROFICIENT = mean(HIS.PROFICIENT, na.rm = TRUE), 
              HIS.ADVANCED = mean(HIS.ADVANCED, na.rm = TRUE),
              OTR.PARTIAL = mean(OTR.PARTIAL, na.rm = TRUE),
              OTR.PROFICIENT = mean(OTR.PROFICIENT, na.rm = TRUE), 
              OTR.ADVANCED = mean(OTR.ADVANCED, na.rm = TRUE),
              DIS.PARTIAL = mean(DIS.PARTIAL, na.rm = TRUE),
              DIS.PROFICIENT = mean(DIS.PROFICIENT, na.rm = TRUE), 
              DIS.ADVANCED = mean(DIS.ADVANCED, na.rm = TRUE),
              LEP.PARTIAL = mean(LEP.PARTIAL, na.rm = TRUE),
              LEP.PROFICIENT = mean(LEP.PROFICIENT, na.rm = TRUE), 
              LEP.ADVANCED = mean(LEP.ADVANCED, na.rm = TRUE),
              EDS.PARTIAL = mean(EDS.PARTIAL, na.rm = TRUE),
              EDS.PROFICIENT = mean(EDS.PROFICIENT, na.rm = TRUE), 
              EDS.ADVANCED = mean(EDS.ADVANCED, na.rm = TRUE))

data[,6:35] <- round(data[,6:35], digits = 1)

data <- reshape(data,
                timevar = "SUBJECT",
                idvar = c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "YEAR"),
                direction = "wide")

data <- select(data, 1:7, ALL.PARTIAL.Math, ALL.PROFICIENT.Math, ALL.ADVANCED.Math)


data$MET_EXCEED.ELA <- data$ALL.ADVANCED.ELA + data$ALL.PROFICIENT.ELA
data$MET_EXCEED.Math <- data$ALL.ADVANCED.Math + data$ALL.PROFICIENT.Math

data <- rbind.fill(data, df)

saveRDS(data, "CleanedNJPerformance.RData")



# Ten data ----------------------------------------

data <- read.csv("TN IN depth 2015.csv", na.strings = "**")
data <- filter(data, subgroup == "All Students")
glimpse(data)

for(i in 1:15){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data$pct_below_bsc <- as.numeric(data$pct_below_bsc)
data$pct_bsc <- as.numeric(data$pct_bsc)
data$pct_bsc_and_below <- as.numeric(data$pct_bsc_and_below)
data$pct_prof <- as.numeric(data$pct_prof)
data$pct_prof_adv <- as.numeric(data$pct_prof_adv)
data$pct_adv <- as.numeric(data$pct_adv)


df <- data

data <- read.csv("TN IN depth 2014.csv", na.strings = "**")
data <- filter(data, subgroup == "All Students")
glimpse(data)

for(i in 1:15){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data$pct_below_bsc <- as.numeric(data$pct_below_bsc)
data$pct_bsc <- as.numeric(data$pct_bsc)
data$pct_bsc_and_below <- as.numeric(data$pct_bsc_and_below)
data$pct_prof <- as.numeric(data$pct_prof)
data$pct_prof_adv <- as.numeric(data$pct_prof_adv)
data[data == "*"] <- NA
data$pct_adv <- as.numeric(data$pct_adv)

df <- rbind(df, data)


data <- read.csv("TN IN depth 2013.csv", na.strings = "**")
data <- filter(data, subgroup == "All Students")
glimpse(data)

for(i in 1:15){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data$pct_below_bsc <- as.numeric(data$pct_below_bsc)
data$pct_bsc <- as.numeric(data$pct_bsc)
data$pct_bsc_and_below <- as.numeric(data$pct_bsc_and_below)
data$pct_prof <- as.numeric(data$pct_prof)
data$pct_prof_adv <- as.numeric(data$pct_prof_adv)
data[data == "*"] <- NA
data$pct_adv <- as.numeric(data$pct_adv)

df <- rbind(df, data)

data <- df
data$valid_tests <- as.numeric(data$valid_tests)

data$subject[data$subject == "Algebra I" | data$subject == "Algebra II"] <- "Math"
data$subject[data$subject == "English II" | data$subject == "English III" |
             data$subject == "RLA"] <- "ELA"

unique(data$subject)

data <- ddply(data, c("year", "district", "school", "district_name", "school_name", "subject"),
              summarise,
              validtests = sum(valid_tests, na.rm = TRUE),
              below = mean(pct_below_bsc, na.rm = TRUE),
              bsc = mean(pct_bsc, na.rm = TRUE),
              bscbelow = mean(pct_bsc_and_below, na.rm = TRUE),
              proficient = mean(pct_prof, na.rm = TRUE),
              advanced = mean(pct_adv, na.rm = TRUE),
              advprof = mean(pct_prof_adv, na.rm = TRUE))

data[,7:13] <- round(data[,7:13], digits = 1)
df <- data

df <- select(df, -validtests)

df <- reshape(df,
              timevar = "subject",
              idvar = c("year", "district", "school", "district_name", "school_name"),
              direction = "wide")



data <- read.csv("TN 2010.csv")
data$year <- 2010
data1 <- read.csv("TN 2011.csv")
data1$year <- 2011
data2 <- read.csv("TN 2012.csv")
data2$year <- 2012

data <- rbind(data, data1, data2)
for(i in 1:20){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data <- filter(data, Grade == "All Grades" & Student.Group == "All")
data <- select(data, 1:5, 14:20)

data <- reshape(data,
                timevar = "Subject",
                idvar = c("District.Name", "School.Name", "District.ID", "School.ID", "year"),
                direction = "wide")
data <- data[c(5, 3:4, 1:2, 6:17)]

names(data) <- names(df)
data <- rbind(data, df)

saveRDS(data, "CleanedTNPerformance.RData")




# GA data -----------------------------------------
data <- read.csv("GA performance 2014-15.csv")
data <- filter(data, TEST_CMPNT_TYP_NM == c("English Language Arts", "Mathematics" ))
data <- filter(data, SUBGROUP_NAME == "All Students")
data <- filter(data, INSTN_NAME != "ALL")

data <- ddply(data, c("SCHOOL_DISTRCT_CD", "SCHOOL_DSTRCT_NM", 
                      "INSTN_NUMBER","INSTN_NAME", "TEST_CMPNT_TYP_NM"),
              summarise,
              Tested = sum(NUM_TESTED_CNT, na.rm = TRUE),
              CBegin = round(mean(BEGIN_CNT, na.rm = TRUE), digits = 2),
              CDevelop = round(mean(DEVELOPING_CNT, na.rm = TRUE), digits = 2),
              CProficient = round(mean(PROFICIENT_CNT, na.rm = TRUE), digits = 2),
              CDistinguished = round(mean(DISTINGUISHED_CNT, na.rm = TRUE), digits = 2),
              Begin = round(mean(BEGIN_PCT, na.rm = TRUE), digits = 2),
              Develop = round(mean(DEVELOPING_PCT, na.rm = TRUE), digits = 2),
              Proficient = round(mean(PROFICIENT_PCT, na.rm = TRUE), digits = 2),
              Distinguished = round(mean(DISTINGUISHED_PCT, na.rm = TRUE), digits = 2))


# AK data --------------------------------------------
data <- read.csv("Arkansas 2014-15 performance.csv")
names(data)

data <- select(data, 1:22)

nameslist <- c("district.lea", "district.name", "school.lea", "school.name", 
               "subject", "test", "subgroup", "grade", "year", "num.cat", 
               "tested", "perc.test", "lv1count", "lv1perc", "lv2count", 
               "lv2perc", "lv3count", "lv3perc", "lv4count", "lv4perc",
               "lv5count", "lv5perc")

names(data) <- nameslist

for(i in 1:22){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data$year <- as.numeric(sub("14-", "", data$year))
data$perc.test <- as.numeric(sub("%", "", data$perc.test))
data$lv1perc <- as.numeric(sub("%", "", data$lv1perc))
data$lv2perc <- as.numeric(sub("%", "", data$lv2perc))
data$lv3perc <- as.numeric(sub("%", "", data$lv3perc))
data$lv4perc <- as.numeric(sub("%", "", data$lv4perc))
data$lv5perc <- as.numeric(sub("%", "", data$lv5perc))

data[data == "#VALUE!"] <- NA
data[data == "n<10"] <- NA
data$lv1count <- as.numeric(data$lv1count)
data$lv2count <- as.numeric(data$lv2count)
data$lv3count <- as.numeric(data$lv3count)
data$lv4count <- as.numeric(data$lv4count)
data$lv5count <- as.numeric(data$lv5count)

data <- filter(data, subgroup == "Overall")

data <- ddply(data, 
              c("district.lea", "district.name", "school.lea", 
                "school.name", "subject"),
              summarise,
                tested = sum(tested, na.rm = TRUE),
                perc.tet = mean(perc.test, na.rm = TRUE),
                lv1count = sum(lv1count, na.rm = TRUE),
                lv2count = sum(lv2count, na.rm = TRUE),
                lv3count = sum(lv3count, na.rm = TRUE),
                lv4count = sum(lv4count, na.rm = TRUE),
                lv5count = sum(lv5count, na.rm = TRUE),
                lv1perc = round(mean(lv1perc, na.rm = TRUE), digits = 2),
                lv2perc = round(mean(lv2perc, na.rm = TRUE), digits = 2),
                lv3perc = round(mean(lv3perc, na.rm = TRUE), digits = 2),
                lv4perc = round(mean(lv4perc, na.rm = TRUE), digits = 2),
                lv5perc = round(mean(lv5perc, na.rm = TRUE), digits = 2))



data <- read.csv("Arkansas 2013-14 performance.csv")

nameslist<- c("SchoolID", "RegionID", "Distrct", "School", "MathNum",
              "MathScore", "Below.Math", "Basic.Math", "Prof.Math",
              "Adv.Math", "ELANum", "ELAScore", "Below.ELA", "Basic.ELA",
              "Prof.ELA", "Adv.ELA", "Grade", "Year")

names(data) <- nameslist


for(i in 1:18){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

# data$SchoolID <- as.numeric(sub("-", "", data$SchoolID))

data <- ddply(data, c("SchoolID", "RegionID", "Distrct", "School"),
              summarise,
              MathNum = sum(MathNum, na.rm = TRUE),
              MathScore = round(mean(MathScore, na.rm = TRUE), digits = 2),
              Below.M = round(mean(Below.Math, na.rm = TRUE), digits = 2),
              Basic.M = round(mean(Basic.Math, na.rm = TRUE), digits = 2),
              Prof.M = round(mean(Prof.Math, na.rm = TRUE), digits = 2),
              Adv.M = round(mean(Adv.Math, na.rm =TRUE), digits = 2),
              ELANum = sum(ELANum, na.rm = TRUE),
              ELAScore = round(mean(ELAScore, na.rm = TRUE), digits = 2),
              Below.E = round(mean(Below.ELA, na.rm = TRUE), digits = 2),
              Basic.E = round(mean(Basic.ELA, na.rm = TRUE), digits = 2),
              Prof.E = round(mean(Prof.ELA, na.rm = TRUE), digits = 2),
              Adv.E = round(mean(Adv.ELA, na.rm =TRUE), digits = 2))

data <- read.csv("Arkansas 2012-13 performance.csv", na.strings = "xx")
data[data == ""] <- NA

data <- filter(data, School.Number != "NA")

names(data) <- nameslist


# No school number, filter those out

for(i in 1:18){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

data <- filter(data, School != "NA")

data <- ddply(data, c("RegionID", "District"),
              summarise,
              MathNum = sum(MathNum, na.rm = TRUE),
              MathScore = round(mean(MathScore, na.rm = TRUE), digits = 2),
              Below.M = round(mean(Below.Math, na.rm = TRUE), digits = 2),
              Basic.M = round(mean(Basic.Math, na.rm = TRUE), digits = 2),
              Prof.M = round(mean(Prof.Math, na.rm = TRUE), digits = 2),
              Adv.M = round(mean(Adv.Math, na.rm =TRUE), digits = 2),
              ELANum = sum(ELANum, na.rm = TRUE),
              ELAScore = round(mean(ELAScore, na.rm = TRUE), digits = 2),
              Below.E = round(mean(Below.ELA, na.rm = TRUE), digits = 2),
              Basic.E = round(mean(Basic.ELA, na.rm = TRUE), digits = 2),
              Prof.E = round(mean(Prof.ELA, na.rm = TRUE), digits = 2),
              Adv.E = round(mean(Adv.ELA, na.rm =TRUE), digits = 2))

# Next step, R data merging enrollment and performance data

data <- readRDS("CleanedNJParc12_15.rdata")
enroll <- readRDS("NJ_enrollment2010_16.rdata")

# changing performance data names to common format, lowercase underscores
datanames <- c("county_id", "district_id", "school_id", "year",
               "county_name", "district_name", "school_name",
               "prfadv.math", "prfadv.read")


names(data) <- datanames

# merging enrollment and performance by county, district, school ids and year
df <- inner_join(data, enroll, by = c("county_id", "district_id", "school_id",
                                      "year"))
# Reordering columns and removing extra county, district, school names
df <- df[c(1:7,10:12, 8:9, 13:24)]
df <- select(df, -5, -6, -7)

# renaming a few columns
colnames(df)[5] <- "county_name"
colnames(df)[6] <- "district_name"
colnames(df)[7] <- "school_name"

# save to rdata file
saveRDS(df, file="NJPerformanceEnrollment.RData")


data <- read.csv("NJEnroll.csv")
for(i in 1:27){
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

df <- filter(data, GRADE_LEVEL == "TOTAL")
df <- filter(df, SCHOOL_NAME != "District Total")



































