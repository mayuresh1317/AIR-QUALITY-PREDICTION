rm(list=ls()) #removed Global Environment Variables

install.packages(tidyr)
install.packages(tidyselect)
install.packages("jmv")
install.packages("broom")
install.packages("MLmetrics")

library("tidyr")
library("tidyselect")
library(jmv)
library("caret")
library("dplyr")
library("tidyr")
library("broom")
library("MLmetrics")

# selecting working directory where data set is located.
setwd("F://OneDrive - National College of Ireland/NCI/SEMESTER 1/SUBJECTS/DMML/Project/Dataset/AIRQUALITYDATASET/3")

# importing dataset csv as a data frame
openaq <- read.csv("openaq.csv", header=T, na.strings=c(""), stringsAsFactors = T)


# Initial look at the data frame
summary(openaq)
class(openaq)
str(openaq)
View(openaq)

#############################Start of Data Preprocessing ########################

# Removing COlumns which are not required for Analysis
columns_removed <- c("country","utc","unit","latitude","longitude","attribution")
openaq = openaq[,!(names(openaq) %in% columns_removed)]

View(openaq)


#Uisng spread function converting row wise data of pollutants and there values 
#into column wise 
Nopenaq =openaq %>% spread(parameter,value)
View(Nopenaq)

# removing location column
n_columns_removed <- c("location")
Nopenaq = Nopenaq[,!(names(Nopenaq) %in% n_columns_removed)]
View(Nopenaq)


#initial Look at descriptive statistics of dataset
str(Nopenaq)
summary(Nopenaq)

descriptives(Nopenaq, vars = vars(co, no2, o3, pm10,pm25,so2), freq = TRUE)

#imputing null values by downup method 
#Using fill function of tidy r which takes column name and direction of imputation as input.

Nopenaq<- Nopenaq %>% 
  fill(pm25, .direction = "downup")
Nopenaq<- Nopenaq %>% 
  fill(co, .direction = "downup")
Nopenaq<- Nopenaq %>% 
  fill(no2, .direction = "downup")
Nopenaq<- Nopenaq %>% 
  fill(pm10, .direction = "downup")
Nopenaq<- Nopenaq %>% 
  fill(so2, .direction = "downup")
Nopenaq<- Nopenaq %>% 
  fill(o3, .direction = "downup")

summary(Nopenaq)



#Exporting the Dataframe into csv to convert the Concentration values into AQI
write.csv(Nopenaq,"F://OneDrive - National College of Ireland/NCI/SEMESTER 1/SUBJECTS/DMML/Project/Dataset/AIRQUALITYDATASET/3/Nopenaq.csv", row.names = FALSE)

# We used excel to compute AQI values now we will be working aqi data.

# Reading the AQI_vaues csv from same directory mentioned above
AQI <- read.csv("AQI_Values.csv", header=T, na.strings=c(""), stringsAsFactors = T)

descriptives(AQI, vars = vars(coaqi,no2aqi, o3aqi, pm10aqi,pm25aqi,so2aqi,AQI), freq = TRUE)

# Dataset contains zero value at numerous occasions which we will be converting NA
AQI[AQI == 0] <- NA

# runnung descriptive again to check how many zero's were there in each column which 
#willbe now NA'S
descriptives(AQI, vars = vars(coaqi,no2aqi, o3aqi, pm10aqi,pm25aqi,so2aqi,AQI), freq = TRUE)

#now inmupting them with same method as above
AQI<- AQI %>% 
  fill(pm25aqi, .direction = "downup")
AQI<- AQI %>% 
  fill(coaqi, .direction = "downup")
AQI<- AQI %>% 
  fill(no2aqi, .direction = "downup")
AQI<- AQI %>% 
  fill(pm10aqi, .direction = "downup")
AQI<- AQI %>% 
  fill(so2aqi, .direction = "downup")
AQI<- AQI %>% 
  fill(o3aqi, .direction = "downup")
AQI<- AQI %>% 
  fill(AQI, .direction = "downup")

# from the descriptives we can see that AQI values are Negative as well , which is should be 
#converted into absolute values i.e. Positives.

AQI <- AQI %>%
  dplyr::select(where(is.numeric)) %>%
  abs()

# Data Partition for Training and Testing
# this is function of caret package which allow us to split the data for training and
# testing purpose
splitindex <- createDataPartition(AQI$coaqi, p = 0.75, list = FALSE)

aqi_train  <- AQI[splitindex,]
aqi_test   <- AQI[-splitindex,]

#######################################End of Data Pre processing ####################

#################################Start of Model Building ###############################

########Model with all predictor variables################
mlr_aqi <- lm(AQI ~ pm25aqi+coaqi+no2aqi+pm10aqi+so2aqi+o3aqi, data =aqi_train)


#using library function to plot Residual vs fitted and other plots
par(mfrow = c(2, 2))
plot(mlr_aqi)

#summary to check significance of model and each variable and R square 
#and adjusted Rsuare

summary(mlr_aqi)

#Model after removing insignificant variables so2aqi o3aqi          
mlr_aqi_1 <- lm(AQI ~ pm25aqi+coaqi+pm10aqi+no2aqi, data =aqi_train)

#using library function to plot Residual vs fitted and other plots
par(mfrow = c(2, 2))
plot(mlr_aqi_1)

summary(mlr_aqi_1)

###########Prediction of test and train values ############################

mlr_aqi_train <- as.numeric(predict(mlr_aqi_1,aqi_train))
mlr_aqi_test <- predict(mlr_aqi_1,aqi_test)


###################### Start of Evaluation #############################

training_mlr_results<-data.frame(
  RMSE   =RMSE(mlr_aqi_1$fitted.values, aqi_train$AQI),
  MAE    =MAE(mlr_aqi_1$fitted.values, aqi_train$AQI),
  MAPE   =MAPE(mlr_aqi_1$fitted.values, aqi_train$AQI))

Rsquare_mlr_train = caret::R2(mlr_aqi_train, aqi_train$AQI)
view(training_mlr_results)


testing_mlr_results<-data.frame(
  RMSE   =RMSE(mlr_aqi_test, aqi_test$AQI),
  MAE    =MAE(mlr_aqi_test, aqi_test$AQI),
  MAPE   =MAPE(mlr_aqi_test, aqi_test$AQI))

Rsquare_mlr_test = caret::R2(mlr_aqi_test, aqi_test$AQI)

view(training_mlr_results)


######################End of Evaluation #########################


