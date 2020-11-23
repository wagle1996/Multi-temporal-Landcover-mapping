#Estimate time
startTime <- proc.time()

#libraries
library(raster)
# library(sf)
# library(rgdal)
# library(tmap)
# library(tmaptools)
library(caret)
# library(rpart.plot)
library(randomForest)
# library(xgboost)
library(dplyr)
library(e1071)

#To debug errors run followings
#traceback()
#rlang::last_error()


#Dont forget to check the working directory
#setwd("C:/Users/dev_3/Google Drive/IMETI_GEE_Landcover_Data/Ensemble_mapping_ver1")
setwd("C:/Users/DELL-PC/Google Drive/IMETI_GEE_Landcover_Data/Ensemble_mapping_ver1")
getwd()


LS8_2010 <- brick("All_bands_2010_clip.tif")
#LS8_2010_1 <- stack("All_bands_2010_clip.tif")
names(LS8_2010)
plotRGB(LS8_2010, r=1, g=3, b=2, stretch="lin")
# plotRGB(LS8_2010, r=4, g=3, b=2, stretch="lin")
# plotRGB(LS8_2010, r=5, g=3, b=2, stretch="lin")


training_2010 <- shapefile("training_data/training_2010.shp")
head(training_2010)

training_image_2010 <- extract(LS8_2010, training_2010, df=TRUE)
head(training_image_2010)

training_image_2010$Class <- as.factor(training_2010$Class)

#Remember that this dataset may have clouds or cloud shadows. Let's mask them out:
training_image_2010 <- na.omit(training_image_2010)

#Set Seed so that same sample can be reproduced in future also
set.seed(100)

# Now Selecting 75% of data as sample from total 'n' rows of the data  
train_test_split <- sample.int(n = nrow(training_image_2010), size = floor(.80*nrow(training_image_2010)), replace = F)
typeof(train_test_split)

train_split <- training_image_2010[train_test_split, ]
test_split  <- training_image_2010[-train_test_split, ]

Class_n <- train_split %>% dplyr::group_by(Class) %>% dplyr::count()
print(Class_n)

set.seed(100)
trainctrl <- trainControl(method = "cv", number = 10, verboseIter = FALSE)


#rf_parameter_tune




# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_split))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(100)
  fit <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=train_split, method="rf", metric='Accuracy', tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
# 
# 
# 
# #tuning done
# 
# #rf model
set.seed(100)
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_split))))
rf_model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=train_split, method = "rf",
                  tuneLength = 10,
                  ntree = 2000,
                  tuneGrid=tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_split)))),
                  preProcess = c("center", "scale"),
                  trControl = trainctrl,
                  metric="Accuracy")
summary(rf_model)
rf_predict <- predict(rf_model, test_split)

mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2010_CM_rf.txt"))
sink(mytxt)
confusionMatrix(rf_predict, test_split$Class)
sink()

# Predict!
library(caret)
LS8_2010_pred_rf <- predict(LS8_2010, model=rf_model, na.rm=T)
plot(LS8_2010_pred_rf)

# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2010_pred_rf.tif"))
writeRaster(LS8_2010_pred_rf, filename=mytif, format="GTiff", overwrite=TRUE)

#Total time
print(proc.time()-startTime)



#XGBOOST


#XGBOOST hyperparameter tuning


# note to start nrounds from 200, as smaller learning rates result in errors so
# big with lower starting points that they'll mess the scales
nrounds <- 1000
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
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

xgb_tune <- train(
  Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,
  train_split,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
    theme_bw()
}

tuneplot(xgb_tune)
xgb_tune$bestTune


#tuning_finished

#now fitting using best parametre

tune_grid <- expand.grid(
  nrounds = 750,
  eta = 0.1,
  max_depth = 3,
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
xgb_predict <- predict(xgb_model, test_split)

mytxt_x <- file.path(paste0(getwd(),"/Results/", "LS8_2010_CM_xgb.txt"))
sink(mytxt_x)
confusionMatrix(xgb_predict, test_split$Class)
sink()
print(a)
confusionMatrix(xgb_predict, test_split$Class)

# Predict!
library(caret)
LS8_2010_pred_xgb <- predict(LS8_2010, model=xgb_model, na.rm=T)
plot(LS8_2010_pred_xgb)

# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2010_pred_xgb.tif"))
writeRaster(LS8_2010_pred_xgb, filename=mytif, format="GTiff", overwrite=TRUE)




#Rpart Tuning
#search with the caret r packageR
Ptune <- tune.rpart(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, train_split, cp = c(0.002,0.005,0.01,0.015,0.02,0.03))
summary(Ptune)
plot(Ptune)

readline()
#prediction rpart

rpart_model <- train(
  Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,
  train_split,
  trControl = tune_control,
  cp=0.05,
  method = "rpart",)

summary(rpart_model)
rpart_predict <- predict(rpart_model, test_split)

mytxt_x <- file.path(paste0(getwd(),"/Results/", "LS8_2010_CM_rpart.txt"))
sink(mytxt_x)
confusionMatrix(rpart_predict, test_split$Class)
sink()
rpart_model.tree
confusionMatrix(rpart_predict, test_split$Class)
plot(rpart_model$finalModel, uniform=TRUE,
     main="Classification Tree")
text(rpart_model$finalModel, use.n.=TRUE, all=TRUE, cex=.8)

# Predict!
library(caret)
LS8_2010_pred_rpart <- predict(LS8_2010, model=rpart_model, na.rm=T)
plot(LS8_2010_pred_rpart)

# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2010_pred_rpart.tif"))
writeRaster(LS8_2010_pred_rpart, filename=mytif, format="GTiff", overwrite=TRUE)










##for muntiple model runs
# list_model <- c("rf", "knn", "xgbTree", "svmRadial")
# #compare_model <- c()
# 
# for (i in list_model) {
#   startTime <- proc.time()
#   model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=train_split, method = i,
#                  preProcess = c("center", "scale"),
#                  tuneLength = 10,
#                  trControl = trainctrl,
#                  metric="Accuracy",
#                  na.action=na.roughfix)
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
