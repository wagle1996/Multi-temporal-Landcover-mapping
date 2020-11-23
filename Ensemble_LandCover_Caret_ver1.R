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
# library(randomForest)
# library(xgboost)
library(dplyr)

#To debug errors run followings
#traceback()
#rlang::last_error()


#Dont forget to check the working directory
setwd("C:/Users/dev_3/Google Drive/IMETI_GEE_Landcover_Data/Ensemble_mapping_ver1")
# setwd("C:/Users/DELL-PC/Google Drive/R_ensemble_mapping")
getwd()


LS8_2020 <- brick("All_bands_2020_clip.tif")
#LS8_2020_1 <- stack("All_bands_2020_clip.tif")
names(LS8_2020)
plotRGB(LS8_2020, r=1, g=3, b=2, stretch="lin")
# plotRGB(LS8_2020, r=4, g=3, b=2, stretch="lin")
# plotRGB(LS8_2020, r=5, g=3, b=2, stretch="lin")


training_2020 <- shapefile("training_data/training_2010.shp")
head(training_2020)

training_image_2020 <- extract(LS8_2020, training_2020, df=TRUE)
head(training_image_2020)

training_image_2020$Class <- as.factor(training_2020$Class)

#Remember that this dataset may have clouds or cloud shadows. Let's mask them out:
training_image_2020 <- na.omit(training_image_2020)

#Set Seed so that same sample can be reproduced in future also
set.seed(100)

# Now Selecting 75% of data as sample from total 'n' rows of the data  
train_test_split <- sample.int(n = nrow(training_image_2020), size = floor(.75*nrow(training_image_2020)), replace = F)
typeof(train_test_split)

train_split <- training_image_2020[train_test_split, ]
test_split  <- training_image_2020[-train_test_split, ]

Class_n <- train_split %>% dplyr::group_by(Class) %>% dplyr::count()
print(Class_n)

set.seed(100)
trainctrl <- trainControl(method = "cv", number = 10, verboseIter = FALSE)

set.seed(100)
rf_model <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope, data=train_split, method = "rf",
                  tuneLength = 10,
                  ntree = 500,
                  preProcess = c("center", "scale"),
                  trControl = trainctrl,
                  metric="Accuracy")
summary(rf_model)
rf_predict <- predict(rf_model, test_split)

mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2020_CM_rf.txt"))
sink(mytxt)
confusionMatrix(rf_predict, test_split$Class)
sink()

# Predict!
library(caret)
LS8_2020_pred_rf <- predict(LS8_2020, model=rf_model, na.rm=T)
plot(LS8_2020_pred_rf)

# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2020_pred_rf.tif"))
writeRaster(LS8_2020_pred_rf, filename=mytif, format="GTiff", overwrite=TRUE)

#Total time
print(proc.time()-startTime)

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
#   mytxt <- file.path(paste0(getwd(),"/Results/", "LS8_2020_CM_", i, ".txt"))
#   sink(mytxt)
#   print(confusionMatrix(predict, test_split$Class))
#   sink()
# 
#   LS8_2020_pred <- predict(LS8_2020, model, na.rm=T)
#   mytif <- file.path(paste0(getwd(),"/Results/", "LS8_2020_pred_", i, ".tif"))
#   writeRaster(LS8_2020_pred, filename=mytif, format="GTiff", overwrite=TRUE)
#   print(proc.time()-startTime)
# }