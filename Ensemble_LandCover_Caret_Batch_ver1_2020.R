#Estimate time
startTime0 <- proc.time()
startTime <- proc.time()

#libraries
#install packages if missing
library(mlbench)
library(raster)
# library(sf)
# library(rgdal)
# library(tmap)
# library(tmaptools)
library(rpart)
library(randomForest)
library(rotationForest)
library(xgboost)
library(fastAdaboost)
#library(tensorflow)
library(keras)
library(e1071)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)

#To debug errors run followings
#traceback()
#rlang::last_error()

#Set Seed so that same sample can be reproduced in future also
set.seed(100)

#Dont forget to check the working directory
setwd("C:/Users/dev_3/Google Drive/IMETI_GEE_Landcover_Data/Ensemble_mapping_ver1")
# setwd("C:/Users/DELL-PC/Google Drive/IMETI_GEE_Landcover_Data/Ensemble_mapping_ver1")
getwd()

#read LS image
LS8_2020 <- brick("All_bands_2020_clip.tif")
#LS8_2020_1 <- stack("All_bands_2020_clip.tif")
names(LS8_2020)
head(LS8_2020)
# plotRGB(LS8_2020,r=1,g=3,b=2,stretch="lin")

#read training points
training_2020 <- shapefile("training_data/training_2020.shp")
names(training_2020)
head(training_2020)
# plot(training_2020)

# #plot together
# plotRGB(LS8_2020,r=4,g=3,b=2,stretch="lin")
# plot(training_2020,pch=20,cex=1,col=colorRampPalette(c("red", "blue","green","white","yellow"))(255),add=T)

#extract spectral value
training_image_2020 <- extract(LS8_2020,training_2020,df=TRUE)
head(training_image_2020)

#add training class and remove 
training_image_2020$Class <- as.factor(training_2020$Class)

#Remember that this dataset may have clouds or cloud shadows. Let's mask them out:
training_image_2020 <- na.omit(training_image_2020)
head(training_image_2020)
# training_image_2020 <- training_image_2020[,2:13]
# head(training_image_2020)

# Now Selecting 75% of data as sample from total 'n' rows of the data
train_test_split <- sample.int(n=nrow(training_image_2020),size=floor(.75*nrow(training_image_2020)),replace=F)
typeof(train_test_split)

train_split <- training_image_2020[train_test_split,]
test_split  <- training_image_2020[-train_test_split,]

#check the training numbers
Class_n <- train_split %>% dplyr::group_by(Class) %>% dplyr::count()
print(Class_n)

# define the control using a random forest selection function
trainctrl_cv <- trainControl(method="cv",number=10,verboseIter=FALSE,allowParallel=TRUE)
trainctrl_repcv <- trainControl(method="repeatedcv",number=10,repeats=3,allowParallel=TRUE)
trainctrl_repcv_grid <- trainControl(method="repeatedcv",number=10,repeats=3,search="grid",allowParallel=TRUE)
trainctrl_rfe <- rfeControl(functions=rfFuncs,method="repeatedcv",number=10,allowParallel=TRUE)

#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()

# #Feature selection by correlation
# correlationMatrix <- cor(training_image_2020[,2:12])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix,cutoff=0.90)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
# 
# #Rank Features By Importance
# model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,data=training_image_2020,method="lvq",preProcess="scale",trControl=trainctrl_repcv)
# # estimate variable importance
# importance <- varImp(model,scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)
# 
# #Feature Selection
# # run the RFE algorithm
# model_rfe <- rfe(training_image_2020[,2:12],training_image_2020[,13],sizes=c(2:12),rfeControl=trainctrl_rfe)
# # summarize the model_rfe
# print(model_rfe)
# # list the chosen features
# predictors(model_rfe)
# # plot the model_rfe
# plot(model_rfe,type=c("g","o"))

#Estimate time
print(proc.time()-startTime)
################################################################################

#for multiple model runs
list_model <- c("svmRadial","nnet","rf","xgbTree")
#compare_model <- c()

for (i in list_model) {
  startTime <- proc.time()
  #Choose factors here
  model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=train_split,method=i,
                 preProcess=c("center","scale"),tuneLength=10,trControl=trainctrl_cv,
                 metric="Accuracy",na.action=na.roughfix)
  model
  predict <- predict(model,test_split)
  mytxt <- file.path(paste0(getwd(),"/BatchResults/","LS8_2020_CM_",i,".txt"))
  sink(mytxt)
  print(confusionMatrix(predict,test_split$Class))
  sink()
  confusionMatrix(predict,test_split$Class)
  LS8_2020_pred <- predict(LS8_2020,model,na.rm=T)
  plot(LS8_2020_pred,main=i,col=colorRampPalette(c("red", "blue","green","grey","yellow"))(255))
  mytif <- file.path(paste0(getwd(),"/BatchResults/","LS8_2020_pred_",i,".tif"))
  writeRaster(LS8_2020_pred,filename=mytif,format="GTiff",overwrite=TRUE)
  print(proc.time()-startTime)
}

################################################################################

#Estimate time
mytxt <- file.path(paste0(getwd(),"/BatchResults/","LS8_2020_Consumedtime.txt"))
sink(mytxt)
print(proc.time()-startTime0)
print((proc.time()-startTime0)[[3]])
print(((proc.time()-startTime0)[[3]])/60)
sink()