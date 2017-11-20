
# Center on Reinveinting Public Education
# CITYWIDE, STEPPING UP

# Louisiana
# New Orleans

# Math Enrollment


# Math Enrollment ----------------------------------------------------------

ocr <- read.csv("/Users/Alton/OneDrive/Work/CRPE/City Wide/OCR/ocr_clean.csv")
# ocr <- read.csv("/Users/lualt/OneDrive/Work/CRPE/City Wide/OCR/ocr_master.csv")

glimpse(ocr)
names(ocr)

# LEVEL is 3 for high school

for(i in 1:dim(ocr)[2]) {
  if(class(ocr[,i]) == "factor")
    ocr[,i] <- as.character(ocr[,i])
}

ocr$TOT_MATHENR_CALC <- rowSums(ocr[,c("TOT_MATHENR_CALC_F", "TOT_MATHENR_CALC_M")], na.rm = TRUE)
ocr$TOT_MATH <- rowSums(ocr[,c("TOT_MATHENR_ADVM", "TOT_MATHENR_CALC")], na.rm = TRUE)

ocr$ADVM_HI <- rowSums(ocr[,c("SCH_MATHENR_ADVM_HI_M", "SCH_MATHENR_ADVM_HI_F")], na.rm = TRUE)
ocr$ADVM_WH <- rowSums(ocr[,c("SCH_MATHENR_ADVM_WH_M", "SCH_MATHENR_ADVM_WH_F")], na.rm = TRUE)
ocr$ADVM_BL <- rowSums(ocr[,c("SCH_MATHENR_ADVM_BL_M", "SCH_MATHENR_ADVM_BL_F")], na.rm = TRUE)
ocr$CALC_HI <- rowSums(ocr[,c("SCH_MATHENR_CALC_HI_M", "SCH_MATHENR_CALC_HI_F")], na.rm = TRUE)
ocr$CALC_WH <- rowSums(ocr[,c("SCH_MATHENR_CALC_WH_M", "SCH_MATHENR_CALC_WH_F")], na.rm = TRUE)
ocr$CALC_BL <- rowSums(ocr[,c("SCH_MATHENR_CALC_BL_M", "SCH_MATHENR_CALC_BL_F")], na.rm = TRUE)
ocr$MATH_HI <- rowSums(ocr[,c("ADVM_HI", "CALC_HI")], na.rm = TRUE)
ocr$MATH_WH <- rowSums(ocr[,c("ADVM_WH", "CALC_WH")], na.rm = TRUE)
ocr$MATH_BL <- rowSums(ocr[,c("ADVM_BL", "CALC_BL")], na.rm = TRUE)

ocr$PCT_MATHENR_ADVM <- ocr$TOT_MATHENR_ADVM/ocr$TOT_ENR
ocr$PCT_MATHENR_CALC <- ocr$TOT_MATHENR_CALC/ocr$TOT_ENR
ocr$PCT_TOT_MATH <- ocr$TOT_MATH/ocr$TOT_ENR

ocr$PCT_HI <- ocr$MATH_HI/ocr$TOT_MATH
ocr$PCT_WH <- ocr$MATH_WH/ocr$TOT_MATH
ocr$PCT_BL <- ocr$MATH_BL/ocr$TOT_MATH

ocr <- dplyr::select(ocr, 2:9, 68:70, 109:124)

ocr[,22:27] <- round(ocr[,22:27], digits = 4)

### - - - - - 
ocr <- filter(ocr, LEA_STATE == "LA")

ocr$NCES_SCHOOL_ID <- as.numeric(ocr$NCES_SCHOOL_ID)
names(ocr)[8] <- "ncessch"

### - - - - - 
ccd <- readRDS("/Users/Alton/OneDrive/Work/CRPE/City Wide/LA/ccd_2010_2015_LA.Rdata")
ccd <- readRDS("/Users/lualt/OneDrive/Work/CRPE/City Wide/LA/ccd_2010_2015_LA.Rdata")
for(i in 1:dim(ccd)[2]) {
  if(class(ccd[,i]) == "factor")
    ccd[,i] <- as.character(ccd[,i])
}

glimpse(ccd)
ccd <- filter(ccd, year == 2012)
glimpse(ocr[1:10])

# ccd <- filter(ccd, level == 3)

data <- inner_join(ocr, ccd, by = "ncessch", match = "first")
names(data)
data <- dplyr::select(data, 1,8:27, 46)




### Run both once
data <- filter(data, city == "New Orleans city")
glimpse(data)

colSums(data[,3:15], na.rm = TRUE)
math <- colMeans(data[,16:21], na.rm = TRUE)
math <- as.data.frame(math)
write.csv(math, "MathEnrollmentNewOrleans.csv")