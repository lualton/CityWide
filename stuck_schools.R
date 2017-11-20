# Center on Reinventing Public Education

# CITYWIDE, STEPPING UP
# Louisiana
# New Orleans

# Stuck Schools Measure (Stagnancy)
# Bottom 5% analysis


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
## mark if stuck in y4
# stdata$iny4 <- ifelse(stdata$year==4 & stdata$stuck==1, 1, 0)

# Adding in a value for if the school existed in the dataset - Alton
stdata$present <- 1

stdata <- filter(stdata, time != "NA")

## collapse by school
glimpse(stdata) # check if there are missing data in stdata

change <- ddply(stdata, .(seasch, city.new), function(x)
  c(stuckinyr1=sum(x$iny1, na.rm = TRUE),
    stuckinyr4=sum(x$iny4, na.rm = TRUE),
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

stuckMatt <- ddply(detail, .(city.new), function(x)
  c(stuckinY1 = sum(x$stuckinyr1, na.rm = TRUE),
    stuck_all = sum(x$all, na.rm = TRUE),
    moved_out = sum(detail$outStuck >= 1, na.rm = TRUE),
    left_data = sum(detail$yearsin < length(year), na.rm = TRUE)))

write.csv(stuckMatt, file=namelist[7])

