# Jose Hernandez 
# 3/1/2017
# Alton Lu
# 4/26/2017
###################################################
# CCD contains city information and school level charecteristics 
# State school data contains performance data 
# Enrollment data 
# Merging them will create the analysis master file 
#####################################################
rm(list=ls()) 

# Step 1 - Merging Shape files with the CCD files --------------------------------------------------------------------

library(sp)
library(rgdal)
library(maps)
library(maptools)
library(foreign)
library(dplyr)

ccd <- readRDS("/Users/Alton/OneDrive/Work/CRPE/City Wide/ccd_2010_15.Rdata")

setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/shapefiles/shp_NY") # if on Mac 

# path = "/Users/Alton/OneDrive/Work/CRPE/City Wide/shapefiles"
filenames <- list.files(pattern = ".dbf", recursive = TRUE)
filenames <- unlist(strsplit(filenames, split='/', fixed=TRUE))
filenames

# https://nces.ed.gov/ccd/pdf/sc122a_documentation_052716.pdf
# Table 2 - State Code in above URL
# # # - - - -  CHANGE HERE
ccd <- filter(ccd, fipst == "36")

## STEP 2: Load geographic data   
#read in Census Places polygons
shape <- ""
NJ <- readOGR(filenames)

#combine all states' shapefiles 
summary(NJ$NAME)
plot(NJ)
##first, need to change the polygon IDs so that they are not duplicated across shapefile sets
NJ1 <- spChFIDs(NJ, as.character(NJ$GEOID))

## STEP 3: Attach city onto state data
#set missing lon/lat to 0, & set lon/lat to coordinates
ccd$latcod[is.na(ccd$latcod)] <- 0
ccd$loncod[is.na(ccd$loncod)] <- 0
coordinates(ccd) <- c("loncod", "latcod")

#tell R that school coordinates are in the same lat/long reference system as the places data
proj4string(ccd) <- proj4string(NJ1)

#combine is.na() with over() to do the containment test (note that we need to "demote" places to a SpatialPolygons object first)
inside.place <- !is.na(over(ccd, as(NJ1, "SpatialPolygons")))

#use "over" again, this time with places as a SpatialPolygonsDataFrame object, to determine which places (if any) contains each school, and store the place name as attribute of the schools data
ccd$city <- rep(NA, nrow(ccd))
ccd$city <- over(ccd, NJ1)$NAMELSAD

#write the augmented state dataset to new .dta file
schools <- as.data.frame(ccd)
####
# setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/GA") 
# saveRDS(schools, "ccd_2010_2015_GA.Rdata")
####

for(i in 1:dim(schools)[2]) {
  if(class(schools[,i]) == "factor")
    schools[,i] <- as.character(schools[,i])
}


# STEP 2 - Merge CCD with performance data -----------------------------------------------------------------------------------
# rm(list=ls())
library(tidyr)
library(dplyr)

ccd <- schools

setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide/NY") # if on PC
setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/NY") # if on Mac 

# ccd <- readRDS("ccd_2010_2015_GA.Rdata")

data <- readRDS("NY_Complete.Rdata") # Provided from Alton

ccd$seasch <- as.numeric(ccd$seasch)

glimpse(ccd)
glimpse(data)

#### - - - - - CHANGE
names(data)[6] <- "name" #School Name
names(data)[5] <- "seasch" #school ID, might need to change to num
names(data)[21] <- "year"
# names(data)[3] <- "stid"
#### - - - - - 

unique(data$year)
unique(ccd$year)

ccd$ncessch <- as.numeric(ccd$ncessch)
ccd$fipst <- as.numeric(ccd$fipst)
ccd$leaid <- as.numeric(ccd$leaid)
ccd$schno <- as.numeric(ccd$schno)
ccd$stid <- as.numeric(ccd$stid)
ccd$seasch <- as.numeric(ccd$seasch)

# data$seasch <- as.numeric(data$seasch)

# check random school to make sure they both exist
# Can use ID or school name to check
# Will see some differences in how their ID is displayed. Will have to modify. 
# ccd[which(ccd$schnam=="Maya Angelou - Evans"), c(5,6)]

# data[which(data$school_name.x=="Maya Angelou - Evans"), c(3,5)]

### CHECK DATA FILES

Alldata <- inner_join(data, ccd, by = c("seasch", "year"), match = "first")

# We want test to have a few observations as possible.
# If large amount, flag. 
test <- anti_join(data, ccd, by = c("seasch", "year"))


glimpse(Alldata)

####
# saveRDS(Alldata, "GA_ccd_merged.Rdata")
####


### STEP 3 - Final prep ------------------------------------------------------------------

# rm(list=ls())
library(tidyr)
library(dplyr)

# setwd("/Users/lualt/OneDrive/Work/CRPE/City Wide/GA") # if on PC
# setwd("/Users/Alton/OneDrive/Work/CRPE/City Wide/GA") # if on Mac 

data <- Alldata

# data  <- readRDS("GA_ccd_merged.Rdata")


str(data)

#Fix the enrollment file
#factors
#change all factors to characters
for(i in 1:dim(data)[2]) {
  if(class(data[,i]) == "factor")
    data[,i] <- as.character(data[,i])
}

names(data)

#data_diag <- anti_join(data_state,dataCCD, by=c("stid_str","seasch_str","year")) 
# Prep math & reading variables
summary(data)

# tapply is a check to see if the code will run correctly
# There will be issues if the variable has a space in it, 
# such as Prov Adv vs ProvAdv, so change the name of the variable if it does

# names(data)[20] <- "profAdv_Math"
# names(data)[13] <- "profAdv_ELA"

tapply(data$profAdv_ELA, data$year, mean, na.rm=T) # THIS IS A TEST
tapply(data$profAdv_Math, data$year, mean, na.rm=T) # THIS IS A TEST

# Should be pcific and asian
data$propasian <- rowSums(cbind(data$asian_enrollment_total, data$pacisl_enrollment_total), 
                          na.rm=T) / data$total_enrollment
data$numasian <- rowSums(cbind(data$asian_enrollment_total, data$pacisl_enrollment_total), na.rm=T) 
data$propfrl <- data$free_and_reduced_lunch_total_enrollment / data$total_enrollment
data$propwhite <- data$white_enrollment_total/ data$total_enrollment
data$propblack <- data$black_enrollment_total/ data$total_enrollment
data$prophisp <- data$hisp_enrollment_total/ data$total_enrollment

# Only if free and reduced lunch are separate variables
data$propfrl <- rowSums(cbind(data$free_lunch, data$reduced_lunch), na.rm=T) / data$row_total
data$numfrl <- rowSums(cbind(data$free_lunch, data$reduced_lunch), na.rm=T)
# data$numfrl <- data$free_and_reduced_lunch_total_enrollment


table(data$level)
data$level[is.na(data$level)] <- 4
data$level[data$level == "N"] <- 4 

data$city.new <- ifelse(is.na(data$city), "Missing", data$city)

glimpse(data)


names(data)[21] <- "year"
names(data)[5] <- "seasch"
names(data)[6] <- "name"
names(data)[13] <- "profAdv_ELA"
names(data)[20] <- "profAdv_Math"
names(data)[27] <- "frl_total"

##Vars that we need for analyses
# This will need to be changed to fit the dataframe
selectvars <- data[,c("year","seasch","name","profAdv_ELA", "profAdv_Math","frl_total","propfrl",
                      "white_enrollment_total","propwhite","black_enrollment_total",
                      "propblack","hisp_enrollment_total","prophisp",
                      "numasian","propasian","urban","level","total_enrollment",
                      "city.new","chartr","notschool")]

names(selectvars) <- c("year","seasch","name","read","math","numfrl",
                       "propfrl","numwhite","propwhite","numblack",
                       "propblack","numhisp","prophisp",
                       "numasian","propasian","urban","level","enroll",
                       "city.new","ischarter","notschool")


names(selectvars)

##### 
saveRDS(selectvars, "NY_complete_analysis.Rdata")
##### 
