
### City Wide Data Cleaning ###
# rm(list=ls())

### Dplyr and Tidyr guides. Google R Style Guide
# http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html 
# http://garrettgman.github.io/tidying/ 
# https://google.github.io/styleguide/Rguide.xmls

# Loading Packages 
# If these won't load, you'll need to install them.
library(dplyr) # install.packages("dplyr")
library(tidyr) # install.packages("tidyr")
library(scales) # install.packages("scales")
library(stringr) # install.packages("stringr")

# Set Working Directory, wherever you save the files from Box
setwd("/Users/lualt/OneDrive/Work/CRPE/Clean")

### Loading data in: Enrollment -------------------------------------------
data2016 <- read.csv("NJEnrollment2016.csv")
data2015 <- read.csv("NJEnrollment2015.csv")
data2014 <- read.csv("NJEnrollment2014.csv")
data2013 <- read.csv("NJEnrollment2013.csv")
data2012 <- read.csv("NJEnrollment2012.csv")
data2011 <- read.csv("NJEnrollment2011.csv")


# Looks at structure of the data
str(data2016)

# Manipulation to individuals years ------------------------
# Some small modifications to data frames
# Some data has columns in different areas. 2011 was missing an entire column.
# The code below can run and be mostly sufficient. Explore some of the data
# using "str", "summary", to look at some of the issues that came up. 
# 2016, 2012, and 2011 had some changes that needed to be done.

# 2016 had a column in the wrong place. 
data2016 <- select(data2016, 2:6, 1, 7:27)

# 2012 had an extra column and no grade
data2012 <- select(data2012, -c(SPECED))
data2012$GRADE_LEVEL <- "NA" # Adds an "NA" to new column Grade Level
data2012 <- select(data2012, 1:7, 27, 8:26)

# 2011 had no grade levels
data2011$GRADE_LEVEL <- "NA"
data2011 <- select(data2011, 1:7, 27, 8:26)


# Adding in years to data frames. The "$" pulls a particular column. But if no column
# exists, it adds it. Easier form of mutate.
data2016$YEAR <- 2016
data2015$YEAR <- 2015 # Adds a year into a new year column
data2014$YEAR <- 2014
data2013$YEAR <- 2013
data2012$YEAR <- 2012
data2011$YEAR <- 2011


# Creating a list of column names 
namelist <- c("COUNTY.ID", "COUNTY.NAME", "DISTRICT.ID", "LEA.NAME",
              "SCHOOL.ID","SCHOOL.NAME", "PGR.CODE", "GRADE", "WH_M", "WH_F",
              "BL_M","BL_F", "HI_M", "HI_F", "AS_M", "AS_F", "AM_M", "AM_F",
              "PI_M","PI_F","MU_M", "MU_F", "ROW.TOTAL", "FREE.LUNCH",
              "FRL", "LEP", "MIGRANT", "YEAR")


# Modifying all column names. This allows us to combine them.
names(data2016) <- namelist
names(data2015) <- namelist
names(data2014) <- namelist
names(data2013) <- namelist
names(data2012) <- namelist
names(data2011) <- namelist

# Binding the data by row using rbind. Now we'll have a large data frame of all the years
data <- rbind(data2016, data2015, data2014, data2013, data2012, data2011)

# Removing Uneeded data. Run if needed. 
# rm(data2016, data2015, data2014, data2013, data2012, data2011, namelist)

# Manipulating parts of the big data frame --------------------------------------

# Filter out only totals or where there were no grades given 
data <- filter(data, GRADE == "TOTAL" | GRADE == "NA")

# Gather the columns 9-22 and combine them into two columns
data <- gather(data, "RACE", "ENROLLMENT", 9:22)


# Separate the race and gender classifications 
data <- separate(data, Race, 
                 into = c("RACE", "GENDER"), sep = "_")

# "Un" selecting Grade and ROW.TOTAL. Using the "-" sign before something
# makes it a negative or opposite. In this case, we're unselecting those two
data <- select(data, -c(GRADE, ROW.TOTAL))


# Assign a backup in case something goes wrong
backup <- data


# Writing a CSV if needed
write.csv(data, "Full NJ Enrollment.csv")

# Next things. ------------------------------------------
# There will probably be some errors in 2011 that my method didn't completely fix.
# Let me know if you need any other help


data <- read.csv("2014-2015-demographics-for-schools Version 3.csv")
data$School.Enrollment <- as.integer(data$School.Enrollment)
data[,19:28] <- data$School.Enrollment * data[,5:14]


### Non Parc Data --------------------------------------
# STOP FORGETTING NA.RM = TRUE 
# NA.RM = TRUE
# NA.RM = TRUE


# 'na.strings' lets R know that the "*" is a string. Before, it would code as factor
data <- read.csv("NJState2014.csv", header = TRUE, encoding = "latin1", 
                 allowEscapes = FALSE, na.strings = "*")


colMeans(data$Total.Enroll.ELA, data$Total.Not.Present.ELA, na.rm = TRUE)



glimpse(data)

# Insert NA's wherever there is empty space. These data had both zero and single space
data[data == ""] <- NA
data[data == " "] <- NA

# Creates object of factor variables from our data
i <- sapply(data, is.factor)

# Using the i object, lapply tells R to go down our dataframe and change i 
# into a character. i is a factor from previous code, so this says
# change all factors into characters. 
data[i] <- lapply(data[i], as.character)

# take a look. We get both numerics and integers. Not 100% on that.
glimpse(data)

### Parc Data ------------------------------------------------------
data <- read.csv("Parc2015Math.csv", header = TRUE, encoding = "latin1", 
                 allowEscapes = FALSE, na.strings = "*")

glimpse(data)

# List of names to rename the columns
namelist <- c("County.Code", "County.Name", "District.Code", "
              District.Name", "School.Code","School.Name", 
              "DFG", "Subgroup", "Subgroup.Type", "NumberTest", 
              "Not.Tested", "Valid.Scores", "Mean.Scale.Scores", 
              "Not.Meeting", "Partial.Meeting", "Approaching",
              "Meeting", "Exceeding")

names(data) <- namelist
data <- filter(data, County.Code != "DFG")

data[data == ""] <- NA
data[data == " "] <- NA

# Creates object of factor variables from our data
i <- sapply(data, is.factor)

# Using the i object, lapply tells R to go down our dataframe and change i 
# into a character. i is a factor from previous code, so this says
# change all factors into characters. 
data[i] <- lapply(data[i], as.character)

glimpse(data) # see if it's good


