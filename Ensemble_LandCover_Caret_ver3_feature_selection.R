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
# plot(training_2020,pch=20,cex=1,col=colorRampPalette(c("red","blue","green","white","yellow"))(255),add=T)

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
train_test_split <- sample.int(n = nrow(training_image_2020),size = floor(.75*nrow(training_image_2020)),replace = F)
typeof(train_test_split)

train_split <- training_image_2020[train_test_split,]
test_split  <- training_image_2020[-train_test_split,]

#check the training numbers
Class_n <- train_split %>% dplyr::group_by(Class) %>% dplyr::count()
print(Class_n)

# define the control using a random forest selection function
trainctrl_cv <- trainControl(method = "cv",number = 10,verboseIter = FALSE,allowParallel = TRUE)
trainctrl_repcv <- trainControl(method="repeatedcv",number=10,repeats=3,allowParallel = TRUE)
trainctrl_repcv_grid <- trainControl(method="repeatedcv",number=10,repeats=3,search="grid",allowParallel = TRUE)
trainctrl_rfe <- rfeControl(functions=rfFuncs,method="repeatedcv",number=10,allowParallel = TRUE)

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
# model_lvq <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,data=training_image_2020,method="lvq",
#                preProcess="scale",trControl=trainctrl_repcv)
# # estimate variable importance
# importance_lvq <- varImp(model_lvq,scale=FALSE)
# # summarize importance
# print(importance_lvq)
# # plot importance
# plot(importance_lvq)
# 
# #Feature Selection
# # run the RFE algorithm
# model_rfe <- rfe(training_image_2020[,2:12],training_image_2020[,13],sizes=c(2:12),rfeControl=trainctrl_rfe)
# # summarize the results
# print(model_rfe)
# # list the chosen features
# predictors(model_rfe)
# # plot the results
# plot(model_rfe,type=c("g","o"))

#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()

# #rf_parameter_tune
# #search with the e1071 package
# set.seed(100)
# tune_rf <- tuneRF(training_image_2020[,2:12],training_image_2020[,13],stepFactor=1.5)
# # summary(tune_rf)
# # rf_tune
# 
# #tuning mtry by obb.error (takes long time)
# y = training_image_2020$Class
# X = training_image_2020[,2:12]
# X = X[,!names(X)%in%"Class"]
# nvar = ncol(X)
# nrep = 25 
# rf.list = lapply(1:nvar,function(i.mtry) {
#   oob.errs = replicate(nrep,{
#     oob.err = tail(randomForest(X,y,mtry=i.mtry,ntree=2000)$err.rate[,1],1)})
# })
# plot(replicate(nrep,1:nvar),do.call(rbind,rf.list),col="#12345678",
#      xlab="mtry",ylab="oob.err",main="tuning mtry by oob.err")
# rep.mean = sapply(rf.list,mean)
# rep.sd = sapply(rf.list,sd)
# points(1:nvar,rep.mean,type="l",col=3)
# points(1:nvar,rep.mean+rep.sd,type="l",col=2)
# points(1:nvar,rep.mean-rep.sd,type="l",col=2)
# 
# #compare the mtry with
# floor(ncol(X)/3)
# sqrt(ncol(training_image_2020))
# 
#Manual Search
# tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_split))))
# modellist <- list()
# for (ntree in c(500,1000,1500,2000,2500)) {
#   set.seed(100)
#   fit <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,data=training_image_2020,
#                method="rf",metric='Accuracy',tuneGrid=tunegrid,trControl=trainctrl_repcv_grid,ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# 
# # compare results
# results <- resamples(modellist)
# summary(results)
# dotplot(results)
# #tuning done
# 
# #Estimate time
# print(proc.time()-startTime)
################################################################################
startTime <- proc.time()

#tuned rf model
set.seed(100)
# tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_split))))
model_rf <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,data=train_split,method = "rf",
                  tuneLength = 10,
                  ntree = 2000,
                  tuneGrid=expand.grid(.mtry=c(sqrt(ncol(train_split)))),
                  preProcess = c("center","scale"),
                  trControl = trainctrl_cv,
                  metric="Accuracy")
summary(model_rf)
model_rf
plot(model_rf$finalModel)


#validation
predict_rf <- predict(model_rf,test_split)
mytxt <- file.path(paste0(getwd(),"/Results/","LS8_2020_CM_rf.txt"))
sink(mytxt)
confusionMatrix(predict_rf,test_split$Class)
sink()
confusionMatrix(predict_rf,test_split$Class)

# Predict!
LS8_2020_pred_rf <- predict(LS8_2020,model=model_rf,na.rm=T)
# plot(LS8_2020_pred_rf,col=topo.colors(5))
plot(LS8_2020_pred_rf,col=colorRampPalette(c("red", "blue","green","grey","yellow"))(255))

# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/","LS8_2020_pred_rf.tif"))
writeRaster(LS8_2020_pred_rf,filename=mytif,format="GTiff",overwrite=TRUE)
# 
# 
# #Estimate time
# print(proc.time()-startTime)
# ################################################################################
# startTime <- proc.time()
# 
# 
# #XGBOOST hyperparameter tuning
# # note to start nrounds from 200,as smaller learning rates result in errors so
# # big with lower starting points that they'll mess the scales
# nrounds <- 1000
#  tune_grid <- expand.grid(
#   nrounds = seq(from = 200,to = nrounds,by = 50),
#   eta = c(0.025,0.05,0.1,0.3),
#   max_depth = c(2,3,4,5,6),
#   gamma = 0,
#   colsample_bytree = 1,
#   min_child_weight = 1,
#   subsample = 1
# )
# 
# tune_xbg <- train(
#   Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,
#   training_image_2020,
#   trControl = trainctrl_cv,
#   tuneGrid = tune_grid,
#   method = "xgbTree",
#   verbose = TRUE
# )
# 
# #save plot
# #mypng <- file.path(paste0(getwd(),"/Results/","LS8_2020_XGB_tuning_plot.png"))
# #png(mypng,width = 600,height = 600)
# tuneplot <- function(x,probs = .90) {
#   ggplot(x) +
#     coord_cartesian(ylim = c(quantile(x$results$Accuracy,probs = probs),min(x$results$Accuracy))) +
#     theme_bw()
# }
# tuneplot(tune_xbg)
# #dev.off()
# 
# tune_xbg$bestTune
# #tuning_finished
# 
# #Estimate time
# print(proc.time()-startTime)
# ################################################################################
# startTime <- proc.time()

#tuned xbgtree model
tune_grid <- expand.grid(nrounds=200,max_depth=2,eta=0.1,gamma=0,colsample_bytree=1,min_child_weight=1,subsample=1)

model_xbg <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,train_split,trControl = trainctrl_cv,
                   tuneGrid = tune_grid,method = "xgbTree",verbose = TRUE)

summary(model_xbg)
model_xbg

#validation
predict_xbg <- predict(model_xbg,test_split)
mytxt <- file.path(paste0(getwd(),"/Results/","LS8_2020_CM_xgb.txt"))
sink(mytxt)
confusionMatrix(predict_xbg,test_split$Class)
sink()
confusionMatrix(predict_xbg,test_split$Class)

# Predict!
LS8_2020_pred_xgb <- predict(LS8_2020,model=model_xbg,na.rm=T)
plot(LS8_2020_pred_xgb,col=colorRampPalette(c("red", "blue","green","grey","yellow"))(255))

# write to a new geotiff file
mytif <- file.path(paste0(getwd(),"/Results/","LS8_2020_pred_xgb.tif"))
writeRaster(LS8_2020_pred_xgb,filename=mytif,format="GTiff",overwrite=TRUE)

#Estimate time
print(proc.time()-startTime)
################################################################################
startTime <- proc.time()


# #Rpart Tuning
# #search with the e1071 apckage
# set.seed(100)
# # Ptune <- tune.rpart(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,training_image_2020,cp = c(0.001,0.002,0.003,0.004,0.005,0.01,0.015,0.02,0.025))
# tune_rpart <- tune.rpart(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,training_image_2020,cp = seq(0.000,0.01,by=0.001))
# #summary(rpart_tune)
# plot(tune_rpart)
# tune_rpart
# 
# #tuned rpart
# model_rpart <- train(Class~B4+B2+B3+B6+B5+B7+NDVI+NDBI+MNDWI+elevation+Slope,train_split,trControl = trainctrl_cv,cp=0.002,method = "rpart",)
# summary(model_rpart)
# # plot(rpart_model$finalModel,uniform=TRUE,main="Classification Tree")
# # text(rpart_model$finalModel,use.n.=TRUE,all=TRUE,cex=1)
# plot(model_rpart)
# fancyRpartPlot(model_rpart$finalModel)
# model_rpart
# 
# #Validation
# predict_rpart <- predict(model_rpart,test_split)
# mytxt <- file.path(paste0(getwd(),"/Results/","LS8_2020_CM_rpart.txt"))
# sink(mytxt)
# confusionMatrix(predict_rpart,test_split$Class)
# sink()
# confusionMatrix(predict_rpart,test_split$Class)
# 
# # Predict!
# LS8_2020_pred_rpart <- predict(LS8_2020,model=model_rpart,na.rm=T)
# plot(LS8_2020_pred_rpart,col=colorRampPalette(c("red", "blue","green","grey","yellow"))(255))
# 
# # write to a new geotiff file
# mytif <- file.path(paste0(getwd(),"/Results/","LS8_2020_pred_rpart.tif"))
# writeRaster(LS8_2020_pred_rpart,filename=mytif,format="GTiff",overwrite=TRUE)
# 
# #Estimate time
# mytxt <- file.path(paste0(getwd(),"/Results/","LS8_2020_Consumedtime.txt"))
# sink(mytxt)
# print(proc.time()-startTime0)
# print((proc.time()-startTime0)[[3]])
# print(((proc.time()-startTime0)[[3]])/60)
# sink()

val1<- extract(LS8_2020_pred_rf, training_2020, df=TRUE)
val2<- extract(LS8_2020_pred_xgb, training_2020, df=TRUE)
a<-na.omit(val1)
b<-na.omit(val2)
sample1<-a$layer
sample2<-b$layer
ttest<-t.test(sample1,sample2)
ttest
