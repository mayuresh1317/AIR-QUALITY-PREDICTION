rm(list=ls())#removed Global Environment and Variables
#################################Start of Packages and Libraries #########################


install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("na.tools")
install.packages("VIM")
install.packages("simputation")
install.packages("MLmetrics")
install.packages("pls")

library("caret")
library("dplyr")
library("tidyr")
library("na.tools")
library("VIM")
library("simputation")
library("MLmetrics")
library("pls")

##################################End of Packages and Libraries ##################################


#####################################Start of Data Import and Pre Processing ################################

setwd("F://OneDrive - National College of Ireland/NCI/SEMESTER 1/SUBJECTS/DMML/Project/Dataset/AIRQUALITYDATASET/1")

# importing dataset csv as a dataframe
AQIDATA <- read.csv("city_day.csv", header=T, na.strings=c(""), stringsAsFactors = T)

# Intial look at the dataframe
summary(AQIDATA)
class(AQIDATA)
str(AQIDATA)

# checked the format of the datecolumn
AQIDATA$Date[1]
# Format is incorrect , converted it to day month year formt using as.date function
AQIDATA$Date <- as.Date(AQIDATA$Date, format= "%d-%m-%Y")
# verifying changes made to Date column
AQIDATA$Date[1]
# verify class of the Date Column which is now a Date which was previously imported as Factor
class(AQIDATA$Date)

#NA Values in all variables
colSums(is.na(AQIDATA))

library(VIM)
aggr_plot <- aggr(AQIDATA, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Removing rows which didn't contain in value in AQI and AQI Bucket
#Using Drop_na function of tidy r package along with pipe operator where column
#names within which na value rows need to be removed are specified. 

AQIDATA <- AQIDATA %>%
  drop_na(AQI,AQI_Bucket)

# checking whether aqi and aqi bucket na values are zero.
colSums(is.na(AQIDATA))

#Imputed values which were less than 505 using fill function and down up method.
#Using fill function of tidy r which takes column name and direction of imputation as input.

AQIDATA<- AQIDATA %>% 
  fill(PM2.5, .direction = "downup")
AQIDATA<- AQIDATA %>% 
  fill(NO, .direction = "downup")
AQIDATA<- AQIDATA %>% 
  fill(NO2, .direction = "downup")
AQIDATA<- AQIDATA %>% 
  fill(NOx, .direction = "downup")
AQIDATA<- AQIDATA %>% 
  fill(CO, .direction = "downup")
AQIDATA<- AQIDATA %>% 
  fill(SO2, .direction = "downup")
AQIDATA<- AQIDATA %>% 
  fill(O3, .direction = "downup")
AQIDATA<- AQIDATA %>% 
  fill(Benzene, .direction = "downup")

#Verifying whether all null values are imputed.

summary(AQIDATA)

install.packages("na.tools")
install.packages("VIM")
install.packages("simputation")

#Imputing pollutant which are greater than 505 using multiple linear regression.
# this is function of simputation package which takes dataframe and multiple linear regression model as input 
#to remove na values with predicted values from model.

AQIDATA <- impute_lm(AQIDATA,PM10 ~ Benzene + O3+ SO2 + CO + NO + PM2.5+ NO2)
AQIDATA <- impute_lm(AQIDATA,NH3 ~ NO+NO2+NOx)
AQIDATA <- impute_lm(AQIDATA,Toluene ~ Benzene + O3+ SO2 + CO + NO + PM2.5)
AQIDATA <- impute_lm(AQIDATA,Xylene ~ Benzene + O3+ SO2 + CO + NO + PM2.5+ NO2)




# Data Partition for Training and Testing
# this is fucntion of caret package which allow us tosplit the data for training and
# testing purpose

splitindex <- createDataPartition(AQIDATA$PM2.5, p = 0.75, list = FALSE)

aqi_train  <- AQIDATA[splitindex,]
aqi_test   <- AQIDATA[-splitindex,]

###################################END OF PREPROCESSING#################################################

##################################START OF MODEL BUILDING (PLS)######################################################

#Partial Least Square Regression
#assigning seed value 
set.seed(123)

#Model Training
modelplsfit <- train(
  AQI~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene  + Toluene + Xylene, data = aqi_train, method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 12),
  tuneLength = 12
)

# Plot of Component vs RMSE
plot(modelplsfit)

# To get the model with best Variance explained and Accuracy obtained
modelplsfit$bestTune

#to get the summary ofmodel which describes the component wise variance and accuracy explained

summary(modelplsfit$finalModel)
summary(modelplsfit)
#################################END OF PLS Model Building ###########################

################################Start of PLS Training evaluation ##################
#predicting the training values forevaluation
predictionplsmodel_training <- modelplsfit %>% predict(aqi_train)

# RMSE R Square MAE for Training Data using Caret
PLSCARETMETRICS_training<-data.frame(
  RMSE = caret::RMSE(predictionplsmodel_training, aqi_train$AQI),
  Rsquare = caret::R2(predictionplsmodel_training, aqi_train$AQI),
  MAE= caret::MAE(predictionplsmodel_training,aqi_train$AQI)
  )

install.packages("MLmetrics")
#Verify RMSE AND MAE FROM CARET AND MLMETRICS PACKAGE and Added MAPE.
PLSMLpackageMETRICS_training<-data.frame(
  PLSRMSE   =RMSE(predictionplsmodel_training, aqi_train$AQI),
  PLSMAE    =MAE(predictionplsmodel_training, aqi_train$AQI),
  PLSMAPE   =MAPE(predictionplsmodel_training, aqi_train$AQI))


#Verifying MAPE from manual function.
plsmapecomputed_training <- function(aqi_train$AQI,predictionplsmodel_training){
  plsmapecomputed_training <- mean(abs((aqi_train$AQI - predictionplsmodel_training)/aqi_train$AQI))*100
  return (plsmapecomputed_training)
}
plsmapecomputed_training
#This is 15.59 which is 0.155*100 which is same as mlmetrics computed mape
##################################END OF PLS TRAINING EVALUATION####################

#######################start of pls Testing EVALUATION######################################

predictionplsmodel_test <- modelplsfit %>% predict(aqi_test)
# Model performance RMSE , MAE AND RSQUARE 
PLSCARETMETRICS_testing<-data.frame(
  RMSE = caret::RMSE(predictionplsmodel_test, aqi_test$AQI),
  Rsquare = caret::R2(predictionplsmodel_test, aqi_test$AQI),
  MAE= caret::MAE(predictionplsmodel_test,aqi_test$AQI)
  
)


#Verify RMSE AND MAE FROM CARET AND MLMETRICS PACKAGE and Added MAPE.
PLSMLpackageMETRICS_testing<-data.frame(
  PLSRMSE   =RMSE(predictionplsmodel_test, aqi_test$AQI),
  PLSMAE    =MAE(predictionplsmodel_test, aqi_test$AQI),
  PLSMAPE   =MAPE(predictionplsmodel_test, aqi_test$AQI))


#Verifying MAPE from manual function.
plsmapecomputed_testing <- function(aqi_test$AQI,predictionplsmodel_test){
  plsmapecomputed_testing <- mean(abs((aqi_test$AQI - predictionplsmodel_test)/aqi_test$AQI))*100
  return (plsmapecomputed_testing)
}
plsmapecomputed_testing
#This is 15.59 which is 0.155*100 which is same as mlmetrics computed mape

###################### END OF PLS TESTING EVALUATION################################




##################################Start of PCR Model Building #################


#PrincipalComponent Regression


set.seed(123)
modelpcrfit <- train(
  AQI~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene  + Toluene + Xylene, data = aqi_train, method = "pcr",
  scale = TRUE,
  trControl = trainControl("cv", number = 12),
  tuneLength = 12
)
plot(modelpcrfit)

modelpcrfit$bestTune

summary(modelpcrfit$finalModel)

summary(modelpcrfit)

###########################END OF PCR MODEL BUILDING###############################

#########################Start of PCR Training Evaluation #######################
predictionpcrmodel_tarining <- modelpcrfit %>% predict(aqi_train)

# Model performance RMSE , MAE AND RSQUARE 
PCRCARETMERICS_training<-data.frame(
  RMSE = caret::RMSE(predictionpcrmodel_tarining, aqi_train$AQI),
  Rsquare = caret::R2(predictionpcrmodel_tarining, aqi_train$AQI),
  MAE= caret::MAE(predictionpcrmodel_tarining,aqi_train$AQI)
  
)


#Verify RMSE AND MAE FROM CARET AND MLMETRICS PACKAGE and added MAPE.
PCRMLpackageMetrics_training <-data.frame(
  RMSEPCR      =RMSE(predictionpcrmodel_tarining, aqi_train$AQI),
  MAEPCR       =MAE(predictionpcrmodel_tarining, aqi_train$AQI),
  MAPEPCR      =MAPE(predictionpcrmodel_tarining, aqi_train$AQI))


#Verifying MAPE from manual function.
PCRmapecomputed_training<- function(aqi_train$AQI,predictionpcrmodel_tarining){
  PCRmapecomputed_training <- mean(abs((aqi_train$AQI - predictionpcrmodel_tarining)/aqi_train$AQI))*100
  return (PCRmapecomputed_training)
}
PCRmapecomputed_training



##############################END OF PCR Training Evaluation#####################



#############################START of PCR Testing Evaluation####################

predictionpcrmodel_testing <- modelpcrfit %>% predict(aqi_test)
# Model performance RMSE , MAE AND RSQUARE 
PCRCARETMERICS_testing<-data.frame(
  RMSE = caret::RMSE(predictionpcrmodel_testing, aqi_test$AQI),
  Rsquare = caret::R2(predictionpcrmodel_testing, aqi_test$AQI),
  MAE= caret::MAE(predictionpcrmodel_testing,aqi_test$AQI)
  
)


#Verify RMSE AND MAE FROM CARET AND MLMETRICS PACKAGE and added MAPE.
PCRMLpackageMetrics_testing <-data.frame(
  RMSEPCR      =RMSE(predictionpcrmodel_testing, aqi_test$AQI),
  MAEPCR       =MAE(predictionpcrmodel_testing, aqi_test$AQI),
  MAPEPCR      =MAPE(predictionpcrmodel_testing, aqi_test$AQI))


#Verifying MAPE from manual function.
PCRmapecomputed_testing<- function(aqi_test$AQI,predictionpcrmodel_testing){
  PCRmapecomputed_testing <- mean(abs((aqi_test$AQI - predictionpcrmodel_testing)/aqi_test$AQI))*100
  return (PCRmapecomputed_testing)
}
PCRmapecomputed_testing


##################################Leave One Out Cv PLSR#####################

PLS_LOO_train <- plsr(AQI~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene  + Toluene + Xylene, ncomp = 12, data = aqi_train, validation = "LOO")

summary(PLS_LOO_train)

plot(PLS_LOO_train)


plot(RMSEP(PLS_LOO_train), legendpos = "topright")
plot(PLS_LOO_train, plottype = "scores", comps = 1:6)

explvar(PLS_LOO_train)


loo_pls_training_result  <-data.frame(
  RMSEPls_loo     =RMSE(PLS_LOO_train$fitted.values, aqi_train$AQI),
  MAEPls_loo      =MAE(PLS_LOO_train$fitted.values, aqi_train$AQI),
  MAPEpls_loo      =MAPE(PLS_LOO_train$fitted.values, aqi_train$AQI))


predictionpls_test_loo <- PLS_LOO_train %>% predict(aqi_test)

loo_pls_testing_result  <-data.frame(
  RMSEPls_loo     =RMSE(predictionpls_test_loo, aqi_test$AQI),
  MAEPls_loo      =MAE(predictionpls_test_loo, aqi_test$AQI),
  MAPEpls_loo      =MAPE(predictionpls_test_loo, aqi_test$AQI))



############################Lave one out CV PCR############################################################

PCR_LOO_train <- pcr(AQI~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene  + Toluene + Xylene, ncomp = 12, data = aqi_train, validation = "LOO")

#modelsummary
summary(PCR_LOO_train)


plot(PCR_LOO_train)


plot(RMSEP(PCR_LOO_train), legendpos = "topright")
plot(PCR_LOO_train, plottype = "scores", comps = 1:6)

explvar(PCR_LOO_train)

# training evaluation results
loo_pcr_training_result  <-data.frame(
  RMSEPls_loo     =RMSE(PCR_LOO_train$fitted.values, aqi_train$AQI),
  MAEPls_loo      =MAE(PCR_LOO_train$fitted.values, aqi_train$AQI),
  MAPEpls_loo      =MAPE(PCR_LOO_train$fitted.values, aqi_train$AQI))


#prediction for test data  
predictionpcr_test_loo <- PCR_LOO_train %>% predict(aqi_test)

# test evaluation results
loo_pcr_testing_result  <-data.frame(
  RMSEPls_loo     =RMSE(predictionpcr_test_loo, aqi_test$AQI),
  MAEPls_loo      =MAE(predictionpcr_test_loo, aqi_test$AQI),
  MAPEpls_loo      =MAPE(predictionpcr_test_loo, aqi_test$AQI))
