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
library(caret)
library(rpart)
library(randomForest)
# library(xgboost)
library(dplyr)
library(e1071)
library(ggplot2)
library(rattle)

#To debug errors run followings
#traceback()
#rlang::last_error()

#Set Seed so that same sample can be reproduced in future also
set.seed(100)

#Dont forget to check the working directory
#setwd("C:/Users/dev_3/Google Drive/IMETI_GEE_Landcover_Data/Ensemble_mapping_ver1")
setwd("C:/Users/DELL-PC/Google Drive/IMETI_GEE_Landcover_Data/Ensemble_mapping_ver1")
getwd()

#read LS image
LS8_2010 <- brick("All_bands_2010_clip.tif")
#LS8_2010_1 <- stack("All_bands_2010_clip.tif")
names(LS8_2010)
head(LS8_2010)
# plotRGB(LS8_2010, r=1, g=3, b=2, stretch="lin")

#read training points
training_2010 <- shapefile("training_data/training_2010.shp")
names(training_2010)
head(training_2010)
# plot(training_2010)

# #plot together
plotRGB(LS8_2010, r=4, g=3, b=2, stretch="lin")
plot(training_2010, pch=20, cex=1, col=colorRampPalette(c("red",  "blue", "green", "white", "yellow"))(255), add=T)

#extract spectral value
training_image_2010 <- extract(LS8_2010, training_2010, df=TRUE)
head(training_image_2010)

#add training class and remove 
training_image_2010$Class <- as.factor(training_2010$Class)

#Remember that this dataset may have clouds or cloud shadows. Let's mask them out:
training_image_2010 <- na.omit(training_image_2010)
head(training_image_2010)
# training_image_2010 <- training_image_2010[,2:13]
# head(training_image_2010)


# Now Selecting 75% of data as sample from total 'n' rows of the data
train_test_split <- sample.int(n = nrow(training_image_2010), size = floor(.75*nrow(training_image_2010)), replace = F)
typeof(train_test_split)

train_split <- training_image_2010[train_test_split, ]
test_split  <- training_image_2010[-train_test_split, ]

#check the training numbers
Class_n <- train_split %>% dplyr::group_by(Class) %>% dplyr::count()
print(Class_n)

# define the control using a random forest selection function
trainctrl_cv <- trainControl(method = "cv", number = 10, verboseIter = FALSE, allowParallel = TRUE)
trainctrl_repcv <- trainControl(method="repeatedcv", number=10, repeats=3, allowParallel = TRUE)
trainctrl_rfe <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10, allowParallel = TRUE)

#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()

# #Feature selection by correlation
# correlationMatrix <- cor(training_image_2010[,2:12])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.90)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
# 
# #Rank Features By Importance
# model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=training_image_2010, method="lvq", preProcess="scale", trControl=trainctrl_repcv)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)
# 
# #Feature Selection
# # run the RFE algorithm
# results <- rfe(training_image_2010[,2:12], training_image_2010[,13], sizes=c(2:12), rfeControl=trainctrl_rfe)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))

#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()

# #rf_parameter_tune
# #Manual Search
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_split))))
# modellist <- list()
# for (ntree in c(500,1000, 1500, 2000, 2500)) {
#   set.seed(100)
#   fit <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=training_image_2010, 
#                method="rf", metric='Accuracy', tuneGrid=tunegrid, trControl=control, ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# 
# # compare results
# mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2010_Parametre_tuning_rf.txt"))
# sink(mytxt)
# results <- resamples(modellist)
# summary(results)
# sink()
# dotplot(results)
# #tuning done
# 
# #Estimate time
# print(proc.time()-startTime)
startTime <- proc.time()

#tuned rf model
set.seed(100)
# tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_split))))
rf_model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=train_split, method = "rf",
                  tuneLength = 10,
                  ntree = 2000,
                  tuneGrid=expand.grid(.mtry=c(sqrt(ncol(train_split)))),
                  preProcess = c("center", "scale"),
                  trControl = trainctrl_cv,
                  metric="Accuracy")
summary(rf_model)

#validation
rf_predict <- predict(rf_model, test_split)
print (rf_predict)
mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2010_CM_rf.txt"))
sink(mytxt)
confusionMatrix(rf_predict, test_split$Class)
sink()
confusionMatrix(rf_predict, test_split$Class)

mytxt <- file.path(paste0(getwd(),"/Results/", "t_data.csv"))
sink(mytxt)
training_image_2020
sink()

# Predict!
LS8_2010_pred_rf <- predict(LS8_2010, model=rf_model, na.rm=T)
# plot(LS8_2010_pred_rf, col=topo.colors(5))
plot(LS8_2010_pred_rf, col=colorRampPalette(c("red",  "blue", "green", "grey", "yellow"))(255))
LS8_2010_pred_rf
# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2010_pred_rf.tif"))
writeRaster(LS8_2010_pred_rf, filename=mytif, format="GTiff", overwrite=TRUE)


#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()


# #XGBOOST hyperparameter tuning
# # note to start nrounds from 200, as smaller learning rates result in errors so
# # big with lower starting points that they'll mess the scales
# nrounds <- 1000
#  tune_grid <- expand.grid(
#   nrounds = seq(from = 200, to = nrounds, by = 50),
#   eta = c(0.025, 0.05, 0.1, 0.3),
#   max_depth = c(2, 3, 4, 5, 6),
#   gamma = 0,
#   colsample_bytree = 1,
#   min_child_weight = 1,
#   subsample = 1
# )
# 
# tune_control <- trainControl(
#   method = "cv", # cross-validation
#   number = 10, # with n folds
#   #index = createFolds(tr_treated$Id_clean), # fix the folds
#   verboseIter = FALSE, # no training log
#   allowParallel = TRUE # FALSE for reproducible results
# )
# 
# xgb_tune <- train(
#   Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,
#   training_image_2010,
#   trControl = tune_control,
#   tuneGrid = tune_grid,
#   method = "xgbTree",
#   verbose = TRUE
# )
# 
# #save plot
# #mypng <- file.path(paste0(getwd(),"/Results/", "LS8_2010_XGB_tuning_plot.png"))
# #png(mypng, width = 600, height = 600)
# tuneplot <- function(x, probs = .90) {
#   ggplot(x) +
#     coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
#     theme_bw()
# }
# tuneplot(xgb_tune)
# #dev.off()

# xgb_tune$bestTune
# #tuning_finished

# #Estimate time
# print(proc.time()-startTime)
################################################################################
startTime <- proc.time()

#tuned xbgtree model
tune_grid <- expand.grid(
  nrounds = 200,
  eta = 0.0025,
  max_depth = 2,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- trainControl(
  method = "cv", # cross-validation
  number = 10, # with n folds
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results
)

xgb_model <- train(
  Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,
  train_split,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE)
summary(xgb_model)

#validation
xgb_predict <- predict(xgb_model, test_split)
mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2010_CM_xgb.txt"))
sink(mytxt)
confusionMatrix(xgb_predict, test_split$Class)
sink()
confusionMatrix(xgb_predict, test_split$Class)

# Predict!
LS8_2010_pred_xgb <- predict(LS8_2010, model=xgb_model, na.rm=T)
plot(LS8_2010_pred_xgb, col=colorRampPalette(c("red",  "blue", "green", "grey", "yellow"))(255))


# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2010_pred_xgb.tif"))
writeRaster(LS8_2010_pred_xgb, filename=mytif, format="GTiff", overwrite=TRUE)

#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()


#Rpart Tuning
#search with the caret r packageR
# Ptune <- tune.rpart(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, training_image_2010, cp = c(0.001,0.002,0.003,0.004,0.005,0.01,0.015,0.02,0.025))
Ptune <- tune.rpart(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, training_image_2010, cp = seq(0.001, 0.05, by=0.001))
#summary(Ptune)
plot(Ptune)
Ptune

#tuned rpart
rpart_model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, train_split, trControl = trainctrl_cv, cp=0.005, method = "rpart",)
summary(rpart_model)
plot(rpart_model$finalModel, uniform=TRUE, main="Classification Tree")
text(rpart_model$finalModel, use.n.=TRUE, all=TRUE, cex=1)
plot(rpart_model)
fancyRpartPlot(rpart_model$finalModel)

#Validation
rpart_predict <- predict(rpart_model, test_split)
mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2010_CM_rpart.txt"))
sink(mytxt)
confusionMatrix(rpart_predict, test_split$Class)
sink()
confusionMatrix(rpart_predict, test_split$Class)

# Predict!
LS8_2010_pred_rpart <- predict(LS8_2010, model=rpart_model, na.rm=T)
plot(LS8_2010_pred_xgb, col=colorRampPalette(c("red",  "blue", "green", "grey", "yellow"))(255))

# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2010_pred_rpart.tif"))
writeRaster(LS8_2010_pred_rpart, filename=mytif, format="GTiff", overwrite=TRUE)

#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()

# 
# #for multiple model runs
# list_model <- c("rf", "knn", "xgbTree", "svmRadial")
# #compare_model <- c()
# 
# for (i in list_model) {
#   startTime <- proc.time()
#   model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=train_split, method = i,
#                  preProcess = c("center", "scale"),
#                  tuneLength = 10,
#                  trControl = trainctrl_cv,
#                  metric="Accuracy",
#                  na.action=na.roughfix)
#   
#   predict <- predict(model, test_split)
# 
#   mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2010_CM_", i, ".txt"))
#   sink(mytxt)
#   print(confusionMatrix(predict, test_split$Class))
#   sink()
# 
#   LS8_2010_pred <- predict(LS8_2010, model, na.rm=T)
#   mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2010_pred_", i, ".tif"))
#   writeRaster(LS8_2010_pred, filename=mytif, format="GTiff", overwrite=TRUE)
#   print(proc.time()-startTime)
# }
# 

#statistical t-test
LS8_2010_pred_rf_extract<- extract(LS8_2010_pred_rf, training_2010, df=TRUE)
LS8_2010_pred_xgb_extract<- extract(LS8_2010_pred_xgb, training_2010, df=TRUE)
LS8_2010_pred_rf_extract<-na.omit(LS8_2010_pred_rf_extract)
LS8_2010_pred_xgb_extract<-na.omit(LS8_2010_pred_xgb_extract)
LS8_2010_pred_rf_layer<-LS8_2010_pred_rf_extract$layer
LS8_2010_pred_xgb_layer<-LS8_2010_pred_xgb_extract$layer
t.test(LS8_2010_pred_rf_layer,LS8_2010_pred_xgb_layer)


#Estimate time
mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2010_Consumedtime.txt"))
sink(mytxt)
print(proc.time()-startTime0)
print((proc.time()-startTime0)[[3]])
print(((proc.time()-startTime0)[[3]])/60)
sink()