# Center on Reinveinting Public Education
# CITYWIDE, STEPPING UP

# Louisiana
# New Orleans

# Proficiency Rates


### (3.5) Proficiency Rates -------------------------------------
profdata <- data

findProficiencyRates <- function(dataframe){
  proficiency.all <- ddply(dataframe, c("year"), summarise,
                           mean = mean(subject, na.rm = TRUE),
                           median = median(subject, na.rm = TRUE))
  
  proficiency.all$type <- "all"
  
  proficiency.city <- ddply(filter(dataframe, city.new == city[1]), c("year"), summarise,
                            mean = mean(subject, na.rm = TRUE),
                            median = median(subject, na.rm = TRUE))
  
  proficiency.city$type <- city
  
  proficiency <- rbind(proficiency.all, proficiency.city)
  
  return(as.data.frame(proficiency))
}

profdata_charter <- filter(profdata, ischarter == '1')
profdata_nocharter <- filter(profdata, ischarter == '0')

proficiency_charter <- findProficiencyRates(profdata_charter)
proficiency_charter$ischarter <- "1"
proficiency_nocharter <- findProficiencyRates(profdata_nocharter)
proficiency_nocharter$ischarter <- "0"
proficiency_allschools <- findProficiencyRates(profdata)
proficiency_allschools$ischarter <- "All"
proficiency1 <- rbind(proficiency_charter, proficiency_nocharter, proficiency_allschools)

proficiency1$subject <- "math"
proficiency_math <- proficiency1

proficiency1$subject <- "read"
proficiency_read <- proficiency1

proficiency <- rbind(proficiency_math, proficiency_read)

write.csv(proficiency, "BatonRouge_proficiency.csv")