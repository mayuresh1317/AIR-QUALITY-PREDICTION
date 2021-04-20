rm(list=ls()) #removed Global Environment and Variables
#################################Start of Packages and Libraries #########################
install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("na.tools")
install.packages("VIM")
install.packages("simputation")
install.packages("MLmetrics")
install.packages("mice")
install.packages('class')
install.packages('caTools')
install.packages('nnet')
install.packages("gmodels")
install.packages("pROC")
install.packages("ROCR")
install.packages('DescTools')
install.packages('lmtest')

library("caret")
library("dplyr")
library("tidyr")
library("na.tools")
library("VIM")
library("simputation")
library("MLmetrics")
library("Amelia")
library("mice")
library("class")
library("caTools")
library("nnet") 
library("gmodels")
library('pROC')
library('ROCR')
library("DescTools")
library("lmtest")



##################################End of Packages and Libraries ##################################

#####################################Start of Data Import and Pre Processing ################################
# selecting working directory where data set is located.
setwd("F://OneDrive - National College of Ireland/NCI/SEMESTER 1/SUBJECTS/DMML/Project/Dataset/AIRQUALITYDATASET/2")

# importing dataset csv as a dataframe
AQISTATIONDATA <- read.csv("station_day.csv", header=T, na.strings=c(""), stringsAsFactors = T)

# Intial look at the dataframe
summary(AQISTATIONDATA)
class(AQISTATIONDATA)
str(AQISTATIONDATA)

# checked the format of the datecolumn
AQISTATIONDATA$Date[1]
# Format is incorrect , converted it to day month year formt using as.date function
AQISTATIONDATA$Date <- as.Date(AQISTATIONDATA$Date, format= "%d-%m-%Y")
# verifying changes made to Date column
AQISTATIONDATA$Date[1]
# verify class of the Date Column which is now a Date which was previously imported as Factor
class(AQISTATIONDATA$Date)


#Creating Subset of data for selected Station Id's
stationAP001<-subset(AQISTATIONDATA,StationId== 'AP001')
stationAP005<-subset(AQISTATIONDATA,StationId== 'AP005') 

stationBR005<-subset(AQISTATIONDATA,StationId== 'BR005')  
stationBR006<-subset(AQISTATIONDATA,StationId== 'BR006')  
stationBR007<-subset(AQISTATIONDATA,StationId== 'BR007')  
stationBR008<-subset(AQISTATIONDATA,StationId== 'BR008')  
stationBR009<-subset(AQISTATIONDATA,StationId== 'BR009')  
stationBR010<-subset(AQISTATIONDATA,StationId== 'BR010')  

stationDL001<-subset(AQISTATIONDATA,StationId== 'DL001')  
stationDL002<-subset(AQISTATIONDATA,StationId== 'DL002')  
stationDL003<-subset(AQISTATIONDATA,StationId== 'DL003')  
stationDL004<-subset(AQISTATIONDATA,StationId== 'DL004')  
stationDL005<-subset(AQISTATIONDATA,StationId== 'DL005') 


stationMH005<-subset(AQISTATIONDATA,StationId== 'MH005')  
stationMH006<-subset(AQISTATIONDATA,StationId== 'MH006')  
stationMH007<-subset(AQISTATIONDATA,StationId== 'MH007')  
stationMH008<-subset(AQISTATIONDATA,StationId== 'MH008')
stationMH009<-subset(AQISTATIONDATA,StationId== 'MH009')
stationMH010<-subset(AQISTATIONDATA,StationId== 'MH010')


# Combined all these Sations in one Dataframe which will be used for further analysis.
AQISUBSETDATA <- rbind(
stationAP001,
stationAP005,
stationBR005,
stationBR006,
stationBR007,
stationBR008,
stationBR009,
stationBR010,
stationDL001,
stationDL002,
stationDL003,
stationDL004,
stationDL005,
stationDL002,
stationDL003,
stationDL004,
stationDL005,
stationMH005,
stationMH006,
stationMH007,
stationMH008,
stationMH009,
stationMH010
)

# removing subsets

rm(stationAP001,stationAP005,stationBR005,stationBR006,stationBR007,stationBR008,stationBR009,stationBR010,
   stationDL001,stationDL002,stationDL003,stationDL004,stationDL005,stationDL002,stationDL003,
   stationDL004,stationDL005,stationMH005,stationMH006,stationMH007,stationMH008,stationMH009,
   stationMH010)

#Checking Summary ofthe Subset Data
summary(AQISUBSETDATA)

#Removing rows which didn't contain in value in AQI and AQI Bucket
#Using Drop_na function of tidy r package along with pipe operator where column
#names within which na value rows need to be removed are specified. 

AQISUBSETDATA<- AQISUBSETDATA %>%
  drop_na(AQI,AQI_Bucket)


#Verifying whether AQI and AQI Bucket Columns has zero Null values
colSums(is.na(AQISUBSETDATA))


#Using Library Amelia missmap function to plot the missing values
missmap(AQISUBSETDATA,main="Missing Values")

#Removed the columns which are having majority of Null  Values
columnsremoved <- c("Toluene","Xylene","NH3")
AQISUBSETDATA = AQISUBSETDATA[,!(names(AQISUBSETDATA) %in% columnsremoved)]


colSums(is.na(AQISUBSETDATA))

#imputing mean value of the PM2.5 COlumn values in place of null values
AQISUBSETDATA$PM2.5[which(is.na(AQISUBSETDATA$PM2.5))]<- mean(AQISUBSETDATA$PM2.5,na.rm = TRUE)

#Verifying the COunt of PM2.5 Null Values
colSums(is.na(AQISUBSETDATA))


library(mice)

# saving the values computed by pmm (predictive mean matching) for each variable specified below in impute variable
impute= mice(AQISUBSETDATA,m=5,method = c("","","","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","",""),maxit=3)

#this create 5 different set of values for every missing value in each column

# Looking at values computed by pmm forcolumn PM10
impute$imp$PM10


summary(AQISUBSETDATA)

#here we can use any of the 5 created datasets for null value to impute in original dataset. 
# we have selected second row of each column predicted by pmm
AQICLEANDATA = complete(impute,2)

#verifying count of null values.
colSums(is.na(AQICLEANDATA))


#removing Station and Date column before feature scaling.
datestationidremoved <- c("StationId","Date")
AQICLEANDATA = AQICLEANDATA[,!(names(AQICLEANDATA) %in% datestationidremoved)]




# Data Partition for Training and Testing
# this is function of caret package which allow us to split the data for training and
# testing purpose

set.seed(123)
split <- sample.split(AQICLEANDATA$AQI_Bucket,SplitRatio =0.75)
training_set <- subset(AQICLEANDATA,split == TRUE)
test_set <- subset(AQICLEANDATA,split == FALSE)


#feature scaling

training_set[-11] <-scale(training_set[-11])
test_set[-11] <-scale(test_set[-11])
#######################################End of Data Import and Pre processing ####################

############################Start of Model Building with KNN K=5############################### 
y_pred <-knn(train=training_set[,-11],
             test= test_set[,-11],
             cl= training_set[,11],
             k=5,
             prob=TRUE)

y_pred_10 <-knn(train=training_set[,-11],
             test= test_set[,-11],
             cl= training_set[,11],
             k=10,
             prob=TRUE)

y_pred_15 <-knn(train=training_set[,-11],
                test= test_set[,-11],
                cl= training_set[,11],
                k=15,
                prob=TRUE)

y_pred_20 <-knn(train=training_set[,-11],
                test= test_set[,-11],
                cl= training_set[,11],
                k=25,
                prob=TRUE)

y_pred_25 <-knn(train=training_set[,-11],
                test= test_set[,-11],
                cl= training_set[,11],
                k=25,
                prob=TRUE)

#################################End of Model Building ################################3

summary(y_pred)


#Confusion matrix of all K levels 
cm <- table(test_set[,11],y_pred)
cm

cm_10<- table(test_set[,11],y_pred_10)
cm_10

cm_15<- table(test_set[,11],y_pred_15)
cm_15

cm_20<- table(test_set[,11],y_pred_20)
cm_20

cm_25<- table(test_set[,11],y_pred_25)
cm_25

# accuracy of all the k values

accuracy_knn<-data.frame(
accuracy_cm_5=round((sum(diag(cm))/sum(cm))*100,2),
accuracy_cm_10=round((sum(diag(cm_10))/sum(cm_10))*100,2),
accuracy_cm_15=round((sum(diag(cm_15))/sum(cm_15))*100,2),
accuracy_cm_20=round((sum(diag(cm_20))/sum(cm_20))*100,2),
accuracy_cm_25=round((sum(diag(cm_25))/sum(cm_25))*100,2))

#converting predicted factor variable to ordered factor to get ROC Curve
ord_y_pred<- factor(y_pred,ordered = TRUE, levels = c('Good','Moderate','Poor','Satisfactory','Severe','Very Poor'))


#ROC and AUC
knn_roc = multiclass.roc(test_set$AQI_Bucket ~ ord_y_pred, plot = TRUE, print.auc = TRUE)

#to get the statistics along with confusion matrix 
confusionMatrix(y_pred, test_set$AQI_Bucket)

CrossTable(x = test_set$AQI_Bucket, y = y_pred, prop.chisq=FALSE)


##################################KNN with repeated CV tunelength 10####################################################################

#Training set without AQI Bucket column

aqiknntrain <- training_set[,names(training_set) != "AQI_Bucket"]

#scaling of train data 
preProcaqitrain <- preProcess(x = aqiknntrain,method = c("center", "scale"))
preProcaqitrain
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 2)



knncaretFit <- train(AQI_Bucket ~ ., data = training_set, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 10)
knncaretFit



plot(knncaretFit)

#Predicting Values for test set
knnPredictAQI <- predict(knncaretFit,newdata = test_set)

summary(knnPredictAQI)
mean(knnPredictAQI == test_set$AQI_Bucket)

confusionMatrix(knnPredictAQI, test_set$AQI_Bucket)

CrossTable(x = test_set$AQI_Bucket, y = knnPredictAQI, prop.chisq=FALSE)

#converting predicted factor variable to ordered factor to get ROC Curve
ord_knnPredictAQI<- factor(knnPredictAQI,ordered = TRUE, levels = c('Good','Moderate','Poor','Satisfactory','Severe','Very Poor'))



#ROC and AUC
knn_caret_roc = multiclass.roc(test_set$AQI_Bucket ~ ord_knnPredictAQI, plot = TRUE, print.auc = TRUE)


#####################################End of KNN with repeated CV tunelength 10####################################

########################################Logistic Regression #############################

############################Sart of Model Building############################ 

AQILOGISTICS=multinom(AQI_Bucket~PM2.5+PM10+NO+NO2+NOx+CO+SO2+O3+Benzene, data=training_set)

#Calculating Rsquare 
Calculate the R Square
Rsqare_mlog<- PseudoR2(AQILOGISTICS, which = c("CoxSnell","Nagelkerke","McFadden"))
Rsqare_mlog

# chi square test 
chisq.test(training_set$AQI_Bucket,predict(AQILOGISTICS))


###################################End Of Model Building#########################

##############################################Start of Training Data Evaluation #####################
#MODEL ACCURACY FOR TRAINING  datasets

AQI_TRAIN_PREDICTED_LOGISTICS<- predict(AQILOGISTICS,newdata = training_set,"class")

class(AQI_TRAIN_PREDICTED_LOGISTICS)
ctable <- table(training_set$AQI_Bucket, AQI_TRAIN_PREDICTED_LOGISTICS)
ctable


# This is gmodlefunction to look at confusion matrix.
CrossTable(x = training_set$AQI_Bucket, y = AQI_TRAIN_PREDICTED_LOGISTICS, prop.chisq=FALSE)

#Summary of Multinomial Logistic regression models which gives coefficients and std errors AIC and Residual Devicance. 
summary(AQILOGISTICS)

head(probability.table <- fitted(AQILOGISTICS))

exp(coef(AQILOGISTICS))

# Accuracy of Training Prediction 
round((sum(diag(ctable))/sum(ctable))*100,2)

############Calculating Prediction error for training ########################## 

mlog_train_error_rate = function(actual, predicted) {
  mean(actual != predicted)*100
}
mlog_train_error_rate(actual = training_set$AQI_Bucket, predicted = AQI_TRAIN_PREDICTED_LOGISTICS)



#####ROC curve for Training prediction ##########
#converting predicted factor variable to ordered factor to get ROC Curve
ord_AQI_TRAIN_PREDICTED_LOGISTICS<- factor(AQI_TRAIN_PREDICTED_LOGISTICS, 
                  ordered = TRUE, levels = c('Good','Moderate','Poor','Satisfactory','Severe','Very Poor'))


#ROC and AUC				  
mlog_train_roc = multiclass.roc(training_set$AQI_Bucket ~ ord_AQI_TRAIN_PREDICTED_LOGISTICS, plot = TRUE, print.auc = TRUE)




#######################################End of Training Data Evaluation######################################

#####################################Start of Test Data Evaluation ###########################################
AQI_TEST_PREDICTED_LOGISTICS<- predict(AQILOGISTICS,newdata = test_set,"class")


cTESTtable <- table(test_set$AQI_Bucket, AQI_TEST_PREDICTED_LOGISTICS)
cTESTtable

#accuracy of Test Data prediction 
round((sum(diag(cTESTtable))/sum(cTESTtable))*100,2)

CrossTable(x = test_set$AQI_Bucket, y = AQI_TEST_PREDICTED_LOGISTICS, prop.chisq=FALSE)


###############################Error rate for Test Prediction #########################################################
mlog_test_error_rate = function(actual, predicted) {
  mean(actual != predicted)*100
}
mlog_test_error_rate(actual = test_set$AQI_Bucket, predicted = AQI_TEST_PREDICTED_LOGISTICS)

#converting predicted factor variable to ordered factor to get ROC Curve
ord_AQI_TEST_PREDICTED_LOGISTICS<- factor(AQI_TEST_PREDICTED_LOGISTICS, 
                                           ordered = TRUE, levels = c('Good','Moderate','Poor','Satisfactory','Severe','Very Poor'))

#ROC and AUC										   
mlog_test_roc = multiclass.roc(test_set$AQI_Bucket ~ ord_AQI_TEST_PREDICTED_LOGISTICS, plot = TRUE, print.auc = TRUE)


####################################End of Testing Data Evaluation #######################################

