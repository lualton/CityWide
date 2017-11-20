
# Center on Reinveinting Public Education
# CITYWIDE, STEPPING UP

# Louisiana
# New Orleans

# Gains measure
# clustered se


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

mdata <- na.omit(model) # IMPORTANT HERE
mdata$time <- mdata$year - min(mdata$year)

mdata <- data.table(mdata, key="year")
mdata[,subject.std:=scale(subject),by=year]

data1 <- mdata[which(mdata$city.new==city[1]),]

factors <- c("time", "propwhite", "propfrl", "propblack", "prophisp",
             "propasian", "factor(urban)", "factor(level)", "enroll")

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

# alias(lm(equation_charter, data=data1_charter))
data1_charter <- filter(data1, ischarter == "1")
equation_charter <- update(equation, ~. -factor(urban))
lm1_charter <- lm(equation_charter, data = data1_charter)
se1_charter <- cluster.se(lm1_charter, data1_charter$seasch) 

gains_charter <- runGainsModel(se1_charter, data1_charter)

data1_nocharter <- filter(data1, ischarter == "0")
# data1_nocharter <- dplyr::select(data1_nocharter, -propfrl, -urban)
equation_nocharter <- update(equation, ~. -factor(urban))
lm1_nocharter <- lm(equation_nocharter, data = data1_nocharter)
se1_nocharter <- cluster.se(lm1_nocharter, data1_nocharter$seasch) 

gains_nocharter <- runGainsModel(se1_nocharter, data1_nocharter) 

data1_all <- data1
data1_all <- filter(data1, level != 3)
equation_all <- update(equation, ~. - factor(urban))
lm1_all <- lm(equation_all, data = data1_all)
se1_all <- cluster.se(lm1_all, data1_all$seasch)

gains_all <- runGainsModel(se1_all, data1_all)


gains_charter$ischarter <- "1"
gains_nocharter$ischarter <- "0"
gains_all$ischarter <- "All"

gains <- rbind(gains_all, gains_charter, gains_nocharter)

gains$subject <- "math"
gains_math <- gains

gains$subject <- "read"
gains_read <- gains

gains <- rbind(gains_math, gains_read)
# write.csv(gains, "NewOrleans_gains.csv")