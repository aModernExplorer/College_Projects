#############################################################################################
######################################  Imports  ############################################
#############################################################################################

library(tidyverse)
library(car) 
library(plyr)
library(e1071)
library(caret)
library(MASS)
library(ggplot2)
library(glmnet)
library(tree)
library(ISLR)
library(randomForest)
library(Metrics)
library(DMwR2)

#############################################################################################
#####################################  R CLeaning  ##########################################
#############################################################################################

# Read in Merge Real Estate
re_data <- read.csv("./Real_Estate_Data_GitHub.csv", header = TRUE, sep = ",")

# Summarize and view data to look for errors
summary(re_data)
View(re_data)

# Removing LotFrontage from dataset
re_data <- re_data[,-4]
View(re_data)

# Removing Electrical from data set because I believe that the type of electrical is not a concern, 
# just whether its good or not is the concern
re_data <- re_data[,-42]

# Remove Heating due to the fact that I beleive that a party would be more concerned with quaulity of 
# heating system rathern than what exactly the heating system is
re_data <- re_data[, -39]

# Replaced all of the Twnhs with TwnhsE for the variable "BldgType"
re_data$BldgType[re_data$BldgType == "Twnhs"] <-  "TwnhsE"
summary(re_data$BldgType)
re_data$BldgType <- factor(re_data$BldgType) # Refactor to eleminate the unused level
summary(re_data$BldgType)

# Make MSSubclass factor
re_data$MSSubClass <- as.factor(re_data$MSSubClass)
mode(re_data$MSSubClass)
class(re_data$MSSubClass) # Making sure class is factor
summary(re_data$MSSubClass) # Looks like level 20 is the most common occuring MSSubclass

# Merge two different factor levels of BrkFace in MsnVnrType to one
summary(re_data$MasVnrType)
levels(re_data$MasVnrType) <- c("BrkCmn", "BrkFace", "BrkFace", "None", "Stone")
summary(re_data$MasVnrType)

# Make Street either 0 (Gravel) or 1 (Paved) and ordered
summary(re_data$Street)
re_data$Street <- revalue(re_data$Street, c("Grvl"=0, "Pave"=1))
class(re_data$Street)
is.ordered(re_data$Street)
re_data$Street <- ordered(re_data$Street, levels = c(0, 1))
is.ordered(re_data$Street)
summary(re_data$Street) # Paved streets occur the most often(2907/2919)

# Refactor and revalue MSZoning
re_data$MSZoning <- revalue(re_data$MSZoning, c("C"=0, "RM"=1, "RH"=2, "RL"=3, "FV"=4)) # Rename and revalue levels based on mean sale price

# Revalue and order Alley
re_data$Alley <- revalue(re_data$Alley, c("Grvl"=1, "Pave"=2, "NoAcc"=0), warn_missing = TRUE)
re_data$Alley <- ordered(re_data$Alley, levels = c(0, 1, 2)) # Make the values ordered for Alley

# Revaluing LandContour to numeric and ordered
re_data$LandContour <- revalue(re_data$LandContour, c("Bnk"=1, "Lvl" = 3, "Low" = 0, "HLS" = 2)) # based on price of each factor and then revaluing it
re_data$LandContour <- ordered(re_data$LandContour, levels = c(0, 1, 2, 3))

# Making Utilities an ordered numeric factor
summary(re_data$Utilities)
re_data$Utilities <- revalue(re_data$Utilities, c("NoSeWa" = 0, "AllPub" = 1)) # revaluing the factors of Utilities
re_data$Utilities <- ordered(re_data$Utilities, levels = c(0, 1)) # ordering Utilities
re_data$Utilities <- as.numeric(as.character(re_data$Utilities)) # to keep all of the variables

# Making LotShape ordered and numeric
re_data$LotShape <- revalue(re_data$LotShape, c("Reg"=3, "IR1"=2, "IR2"=1, "IR3"=0))
re_data$LotShape <- ordered(re_data$LotShape, levels = c(0, 1, 2, 3))

# Making LandSlope numeric and ordered
re_data$LandSlope <- revalue(re_data$LandSlope, c("Sev"=0, "Mod"=1, "Gtl"=2))
re_data$LandSlope <- ordered(re_data$LandSlope, levels = c(0,1,2))

# Convert Condition1 and Condition2 to ordered numeric variables
re_data$Condition1 <- revalue(re_data$Condition1, c("RRAe"=0, "RRNe"=0, "RRAn"=0, "RRNn"=0, 
                                                    "Artery"=1, "Feedr"=2, "Norm"=3, "PosN"=4,
                                                    "PosA"=5))
re_data$Condition1 <- ordered(re_data$Condition1, levels = c(0,1,2,3,4,5))
re_data$Condition2 <- revalue(re_data$Condition2, c("RRAe"=0, "RRAn"=0, "RRNn"=0, 
                                                    "Artery"=1, "Feedr"=2, "Norm"=3, "PosN"=4,
                                                    "PosA"=5))
re_data$Condition2 <- ordered(re_data$Condition2, levels = c(0,1,2,3,4,5))
summary(re_data$Condition1)

# Converting RoofMatl to ordered numeric columns
re_data$RoofMatl <- revalue(re_data$RoofMatl, c("Membran"=0, "Roll"=0, "Tar&Grv"=0, "CompShg"=1,
                                                "WdShake"=2, "WdShngl"=2, "Metal"=3, "ClyTile"=3))
re_data$RoofMatl <- ordered(re_data$RoofMatl, levels = c(0,1,2,3))

# Make ExterQual numeric
re_data$ExterQual <- revalue(re_data$ExterQual, c("Fa"=0, "TA"=1, "Gd"=2, "Ex"=3))
levels(re_data$ExterQual)

# Make ExtCond numeric
re_data$ExterCond <- revalue(re_data$ExterCond, c("Po"=0, "Fa"=1, "TA"=2, "Gd"=3, "Ex"=4))
re_data$ExterCond <- ordered(re_data$ExterCond, levels = c(0,1,2,3,4))

# Make BsmtQual numeric and ordered
re_data$BsmtQual <- revalue(re_data$BsmtQual, c("NoBsmnt"=0, "Fa"=1, "TA"=2, "Gd"=3, "Ex"=4))
re_data$BsmtQual <- ordered(re_data$BsmtQual, levels = c(0,1,2,3,4))

# Make BsmtCond numeric and ordered
re_data$BsmtCond <- revalue(re_data$BsmtCond, c("NoBsmnt"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4))
re_data$BsmtCond <- ordered(re_data$BsmtCond, levels = c(0,1,2,3,4))

# Convert BsmtExposure to numeric and ordered (exposure essentially means ability to walkout outside
# directly from basement) ?
re_data$BsmtExposure <- revalue(re_data$BsmtExposure, c("NoBsmnt"=0, "No"=1, "Mn"=2, "Av"=3, "Gd"=4))
re_data$BsmtExposure <- ordered(re_data$BsmtExposure, levels = c(0,1,2,3,4))

# Convert BsmntFinType1 into a numeric ordered variable
re_data$BsmtFinType1 <- revalue(re_data$BsmtFinType1, c("NoBsmnt"=0, "Unf"=1, "LwQ"=2, "Rec"=3, 
                                                      "BLQ"=4, "ALQ"=5, "GLQ"=6))
re_data$BsmtFinType1 <- ordered(re_data$BsmtFinType1, levels = c(0,1,2,3,4,5,6))

# Make BsmntFinType2 into a numeric ordered variable
re_data$BsmtFinType2 <- revalue(re_data$BsmtFinType2, c("NoBsmnt"=0, "Unf"=1, "LwQ"=2, "Rec"=3, 
                                                      "BLQ"=4, "ALQ"=5, "GLQ"=6))
re_data$BsmtFinType2 <- ordered(re_data$BsmtFinType2, levels = c(0,1,2,3,4,5,6))

# Convert HeatingQC into numeric and ordered variable
re_data$HeatingQC <- revalue(re_data$HeatingQC, c("Po"=0, "Fa"=1, "TA"=2, "Gd"=3, "Ex"=4))
re_data$HeatingQC <- ordered(re_data$HeatingQC, levels = c(0,1,2,3,4))

# Converting CentralAir to binary numeric variable
re_data$CentralAir <- revalue(re_data$CentralAir, c("N"=0, "Y"=1))

# Convert KitchenQual into numeric and ordered factored variable
re_data$KitchenQual <- revalue(re_data$KitchenQual, c("Fa"=0, "TA"=1, "Gd"=2, "Ex"=3))
re_data$KitchenQual <- ordered(re_data$KitchenQual, levels = c(0,1,2,3))

summary(re_data$KitchenQual)

# Convert Functional into numeric and ordered factor variable
re_data$Functional <- revalue(re_data$Functional, c("Sev"=0, "Maj2"=1, "Maj1"=2, "Mod"=3, "Min2"=4,
                                                    "Min1"=5, "Typ"=6))
re_data$Functional <- ordered(re_data$Functional, levels = c(0,1,2,3,4,5,6))

# Make FireplaceQu numeric and ordered
re_data$FireplaceQu <- revalue(re_data$FireplaceQu, c("NoFrPl"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, 
                                                      "Ex"=5))
re_data$FireplaceQu <- ordered(re_data$FireplaceQu, levels = c(0,1,2,3,4,5))

# Make GarageType ordered and numeric
re_data$GarageType <- revalue(re_data$GarageType, c("NoGar"=0, "CarPort"=1, "Detchd"=2, "Attchd"=3, 
                                                    "Basment"=3, "BuiltIn"=4, "2Types"=5))
re_data$GarageType <- ordered(re_data$GarageType, levels = c(0,1,2,3,4,5))

# Make GarageFinish into numeric and ordered 
re_data$GarageFinish <- revalue(re_data$GarageFinish, c("NoGar"=0, "Unf"=1, "RFn"=2, "Fin"=3))
re_data$GarageFinish <- ordered(re_data$GarageFinish, levels = c(0, 1, 2, 3))

# Make GarageQual numeric and ordered
re_data$GarageQual <- revalue(re_data$GarageQual, c("NoGar"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5))
re_data$GarageQual <- ordered(re_data$GarageQual, levels = c(0, 1, 2, 3, 4, 5))
summary(re_data$GarageQual)

# Make GarageCond into numeric and ordered
re_data$GarageCond <- revalue(re_data$GarageCond, c("None"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5))
re_data$GarageCond <- ordered(re_data$GarageCond, levels = c(0, 1, 2, 3, 4, 5))

# Make PavedDrive ordered and numeric
re_data$PavedDrive <- revalue(re_data$PavedDrive, c("N"=0, "P"=1,"Y"=2))
re_data$PavedDrive <- ordered(re_data$PavedDrive, levels = c(0, 1, 2))

# Make PoolQC numeric and ordered
re_data$PoolQC <- revalue(re_data$PoolQC, c("NP"=0, "Fa"=1, "Gd"=2, "Ex"=3))
re_data$PoolQC <- ordered(re_data$PoolQC, levels = c(0, 1, 2, 3))

# Fence
re_data$Fence <- revalue(re_data$Fence, c("NF"=0, "GdPrv"=2, "MnPrv"=1, "GdWo"=2, "MnWw"=1))
re_data$Fence <- ordered(re_data$Fence, levels = c(0, 1, 2))

# Revaluing MScFeature to numeric and ordering it (make the one TenC into same level as Othr)
re_data$MiscFeature <- revalue(re_data$MiscFeature, c("None"=0, "Shed"=1, "Gar2"=2, "Othr"=2, "TenC"=2))
re_data$MiscFeature <- ordered(re_data$MiscFeature, levels = c(0, 1, 2))

# Revaluing SaleCondition to numeric and making it ordered
re_data$SaleCondition <- revalue(re_data$SaleCondition, c("Partial"=0, "Abnorml"=0, "Alloca"=1, "Normal"=1, "Family"=1,"AdjLand"=2))
re_data$SaleCondition <- ordered(re_data$SaleCondition, levels = c(0, 1, 2))

# Get rid of two records that contain level 3 for RoofMatl
rowstodelete <-  which(re_data$RoofMatl == 3)
rowstodelete
re_data <- re_data[which(re_data$RoofMatl<3), ]
summary(re_data$RoofMatl)
re_data$RoofMatl <- factor(re_data$RoofMatl) # drop a level
summary(re_data$RoofMatl) # check to see level is dropped
summary(re_data)
which(is.na(re_data$SalePrice))

# Get rid of the one record that contains Other for Exterior2nd
which(re_data$Exterior2nd == "Other")
re_data <- re_data[-595, ]
which(re_data$Exterior2nd == "Other")
re_data$Exterior2nd <- factor(re_data$Exterior2nd)
summary(re_data$Exterior2nd)
View(re_data)

# Get rid of one record that contains ImStucc for Exterior1st
which(re_data$Exterior1st == "ImStucc")
re_data <- re_data[-1186, ]
which(re_data$Exterior1st == "ImStucc")
re_data$Exterior1st <- factor(re_data$Exterior1st)
summary(re_data$Exterior1st)

which(is.na(re_data$SalePrice))
#############################################################################################
#####################################  Transformations  #####################################
#############################################################################################

# See if LotArea needs transforming
summary(re_data$LotArea)
ggplot(re_data) + geom_density(aes(x=re_data$LotArea)) + coord_cartesian(xlim = c(0, 20000)) # looks to be ok

# See if MsnVnrArea needs transforming
ggplot(re_data) + geom_density(aes(x=re_data$MasVnrArea)) + coord_cartesian(xlim = c(0, 300)) # looks to be ok
re_data$MasVnrArea_Sqrt <- sqrt(re_data$MasVnrArea)

#############################################################################################
#####################################  Linear Regression  ###################################
#############################################################################################

# Splitting training and test sets
train <- re_data[1:1456, ] # Split the data in the way that it was given (training and test)
View(train) # 
test <- re_data[1457:2915, ] # Split to the original test set
View(test)

# Make a test dataset with predictions
lmmodl <- lm(SalePrice~.-ID, data = train)
summary(lmmodl)
predictions <- predict(lmmodl, test) # making predictions on test set
test$SalePrice <- round(predictions, digits = 0)
View(test)

# Combine test and train into one dataset => final dataset
fnldf <- rbind(train, test)
View(fnldf)
summary(fnldf)

# Randomly partition data into training and test sets, split data by half
?createDataPartition
set.seed(2345) # set seed to make work reproducible
indx <- createDataPartition(fnldf$SalePrice, p=.5, list = FALSE)
train <- fnldf[indx,]
test <- fnldf[-indx,]
summary(fnldf)

# Create a baseline lm with all variables in it
lm0 <- lm(SalePrice~.-ID, data = train)
summary(lm0) # R = .9713, F = 179.5
View(train)

# Feature selection
modl <- lm(formula = SalePrice ~ .-ID, data=train)
summary(modl)
step <- stepAIC(modl, direction="backward", na.action=na.remove)
step$anova

# Running lm with variables selected from backwards stepwise regression
lm1 <- lm(SalePrice ~ MSSubClass + MSZoning + LotArea + Street + Alley + 
            LandContour + Utilities + LotConfig + LandSlope + Neighborhood + 
            Condition1 + Condition2 + HouseStyle + OverallQual + OverallCond + 
            YearBuilt + YearRemodAdd + RoofMatl + Exterior1st + MasVnrType + 
            MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + 
            BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + 
            BsmtFinSF2 + BsmtUnfSF + HeatingQC + X1stFlrSF + X2ndFlrSF + 
            BsmtFullBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + 
            KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
            GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + 
            GarageQual + GarageCond + WoodDeckSF + X3SsnPorch + ScreenPorch + 
            PoolArea + PoolQC + Fence + MoSold + YrSold + SaleType + 
            SaleCondition + MasVnrArea_Sqrt, data = train)
summary(lm1)

# Testing feature selection model on test set
lm2 <- lm(SalePrice ~ MSSubClass + MSZoning + LotArea + Street + Alley + 
            LandContour + Utilities + LotConfig + LandSlope + Neighborhood + 
            Condition1 + Condition2 + HouseStyle + OverallQual + OverallCond + 
            YearBuilt + YearRemodAdd + RoofMatl + Exterior1st + MasVnrType + 
            MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + 
            BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + 
            BsmtFinSF2 + BsmtUnfSF + HeatingQC + X1stFlrSF + X2ndFlrSF + 
            BsmtFullBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + 
            KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
            GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + 
            GarageQual + GarageCond + WoodDeckSF + X3SsnPorch + ScreenPorch + 
            PoolArea + PoolQC + Fence + MoSold + YrSold + SaleType + 
            SaleCondition + MasVnrArea_Sqrt, data = test)
summary(lm2)

plot(lm2)
summary(fnldf$Exterior2nd)
summary(fnldf$Exterior1st)
summary(fnldf$ExterCond)
predictionts <- predict(lm2, test)
rmsle(test$SalePrice, pred = predictionts)
#############################################################################################
#####################################  Ridge Regression  ####################################
#############################################################################################

# Setting seed to make work reproducible
set.seed(2345)

# Calculate best hyperparameter (lambda)
x <- model.matrix(train$SalePrice~., train)[,-c(1, 78)]
y <- train$SalePrice
cv.out <- cv.glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda # best lambda = 6,932.817

# Ridge model
rdgmodel <- glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001, standardize = TRUE)

# performing test
xnew <- model.matrix(test$SalePrice~., test)[,-c(1, 78)]
fitresults <- predict(rdgmodel, newx = xnew) # making predictions for SalePrice on test set
coef(rdgmodel, s=best.lambda)
cv.glmnet(x,y)$lambda.min #prints lambda for min error

# Calculating RMSLE
error<-rmsle(test$SalePrice,fitresults)
sprintf("The RMSLE for Ridge is: %f or about %f %%",error,round(error*100,digits=0))

#############################################################################################
#####################################  LASSO Regression  ####################################
#############################################################################################

set.seed(2345)

# LASSO Regression
LASSOmdl <- glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001, standardize = TRUE)

# Calculate best hyperparameter (lambda)
cv.lasso <- cv.glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001)
plot(cv.lasso)
lasso.best.lambda <- cv.lasso$lambda.min
lasso.best.lambda # best lambda = 30.717

# Performing test
LASSOFitResults <- predict(LASSOmdl, newx = xnew)
coef(LASSOmdl, s=lasso.best.lambda)
cv.glmnet(x,y)$lambda.min # lambda = 21.172

# Calculating RMSLE
errorLASSO <- rmsle(test$SalePrice,LASSOFitResults)
which(is.nan(errorLASSO))
sprintf("The RMSLE for LASSO is: %f or about %f %%",errorLASSO,round(errorLASSO*100,digits=0))

#############################################################################################
#####################################  Elastic Regression  ##################################
#############################################################################################

set.seed(2345)

# Elastic regression
ELASTICmodl <- glmnet(x,y, alpha = .5, nlambda = 100, lambda.min.ratio = 0.0001, standardize = TRUE)

# Calculate hyperparameters
cv.elastic <- cv.glmnet(x,y, alpha = .5, nlambda = 100, lambda.min.ratio = 0.0001)
plot(cv.elastic)
elastic.best.lambda <- cv.elastic$lambda.min
elastic.best.lambda # 61.434

# Performing test
ELSTCFitResult <- predict(ELASTICmodl, newx = xnew)
coef(ELASTICmodl, s = elastic.best.lambda)
cv.glmnet(x,y)$lambda.min # 21.172

# Calculating RMSLE
errorELSTC <- rmsle(test$SalePrice, ELSTCFitResult)
sprintf("The RMSLE for Elastic Net is: %f or about %f %%",errorELSTC,round(errorELSTC*100,digits=0))

#############################################################################################
#####################################  K Nearest Neighbor  ##################################
#############################################################################################

# Setting the seed
set.seed(2345)

# Making train/test KNN datasets
trainKNN <- train[, -c(1,10,12,15,16,21,22,23,24,25,29,76)] # removing categorical variables
View(trainKNN)
testKNN <- test[, -c(1,10,12,15,16,21,22,23,24,25,29,76)]
View(testKNN)

# Testing KNN model with k = 5
KNN5 <- kNN(Result ~ .,trainKNN,testKNN,k=5)


#############################################################################################
#####################################  Random Forest  #######################################
#############################################################################################

# Set seed
set.seed(2345)

# Bagging data while using all variables
bag.train=randomForest(SalePrice~.-ID,mtry=76,train,ntrees=1000)
bag.train # look at the summary
yhat.bag = predict(bag.train,mtry=76,newdata=test)
plot(test$SalePrice,yhat.bag)
abline(0,1)

# Now with variables chosen by "importance"
rf.train=randomForest(SalePrice~.-ID,train,mtry=4,importance=TRUE)
rf.train
yhat.rf$SalePrice = predict(rf.train,newdata=test)
mean((yhat.rf$SalePrice-test$SalePrice)^2)
importance(rf.train)
varImpPlot(rf.train)

# RF Evaluation
rf = randomForest(SalePrice ~.-ID, ntree = 100, data = train)
test$predicted.responseRF = predict(rf, test)
view(test)
plot(rf)
print(rf)
rmsle(test$SalePrice, test$predicted.responseRF) # rmsle of RF = .1629

# ctrl <- trainControl(method = 'cv',
#                      number = 10,
#                      verboseIter = FALSE)
# 
# 
# set.seed(123)
# model_rf <- train(SalePrice ~ .-ID,
#                   data = train,
#                   method = "rf",
#                   preProcess = c("scale","center"),
#                   trControl = ctrl,
#                   model = F)
# 
# 
# final_under <- data.frame(actual = test$SalePrice,
#                           predict(model_rf, newdata = test, type ="prob"))
# ?complete.cases
# NeededRows <- function(Df, Clmns) {           # Function help courtesy of BenBarnes on StackOverflow
#  complete <- complete.cases(Df[, Clmns])     # create a df with compelete cases based off certain column
#  return(Df[complete, ])                      # Return completed rows of Df 
# }

# scatterplotMatrix(~re_data$LotFrontage+re_data$MSSubClass+re_data$MSZoning+re_data$LotConfig, data = re_data) # scatterplot matrix to see what qualitative variable would best explain LotFrontage (by being most sensitive to change in LotArea) 
# 
# CCLotFrontage <- NeededRows(re_data, "LotFrontage") # create dataset with only complete observations for LotFtontage
# 
# by(CCLotFrontage, CCLotFrontage$MSSubClass, function(x) mean(CCLotFrontage$LotFrontage), simplify = TRUE) # Find means of LotFrontage based off of MSSubclass
# 
# PractDat <- re_data # Make pract dataset to try different cleaning on so that I don't mess up on actual dataset
# # MeanSubsFunc <- function(Data, RefClmn, SubClmn) {
# #  SubClmn <- with(Data, ave(SubClmn, RefClmn, FUN = function(x) 
#  #  replace(x, is.na(x), mean(x, na.rm = TRUE))))
# #}
# # MeanSubsFunc(PractDat, MSSubClass, LotFrontage)
# 
# PractDat$LotFrontage <- ave(PractDat$LotFrontage, PractDat$MSSubClass, FUN = function(x)
#   replace(x, is.na(x), mean(x, na.rm = TRUE))) # Test to see if it works (which it does)
# 
# re_data$LotFrontage <- ave(re_data$LotFrontage, re_data$MSSubClass, FUN = function(x)
#   replace(x, is.na(x), mean(x, na.rm = TRUE))) # Substitute all of the na's in LotFrontage with averages based on MSSubClass
# 
# # Make MSSubClass Factors so that I can use the variable properly in analysis (and not as an int)
# PractDat$MSSubClass <- factor(PractDat$MSSubClass, c("20", "30", "40", "45", "50", 
#                                                      "60", "70", "75", "80", "85", "90", "120",
#                                                      "150", "160", "180", "190"))
# 
# class(PractDat$MSSubClass)
# 
# re_data$MSSubClass <- factor(re_data$MSSubClass, c("20", "30", "40", "45", "50", 
#                                                    "60", "70", "75", "80", "85", "90", "120",
#                                                    "150", "160", "180", "190"))  # Made all of the ints into factors
# class(re_data$MSSubClass)
# 
# levels(re_data$MSSubClass) # Check to see if factors are in appropriate order
# 
# summary(re_data$MSSubClass) # Get numeric idea of distribution of types of housing involved in sale
# 
# # Replace C(all) in MSZoning with C and Refactor and Relabel MSZoning
# class(re_data$MSZoning)
# 
# levels(re_data$MSZoning)
# 
# re_data$MSZoning <- factor(re_data$MSZoning, c("A", "C (all)", "FV", "I", "RH", "RL", "RP", "RM"), 
#                            labels = c("A", "C", "FV", "I", "RH", "RL", "RP", "RM"))
# levels(re_data$MSZoning) # Check to see if levels are correct
# 
# # Change all "Unk" in Alley to "NoAcc" (short for no access) which means I need to undo my changing of "NA" to "Unk"
# class(re_data$Alley)
# 
# levels(re_data$Alley)
# 
# re_data$Alley <- factor(re_data$Alley, c("Grvl", "Pave", "Unk"), # Refactor and relabel so that I can understand 
#                            labels = c("Grvl", "Pave", "NoAcc"))  # each level and use it for analysis
# 
# # Replace "Unk" in Utilities with "AllPub" because >99% of houses have AllPub
# class(re_data$Utilities)
# 
# summary(re_data)
# 
# ReFactFunc <- function(Data, Clmn, Lev, Lab) {
# #  Data$Clmn <- factor(Clmn, levels = c(Lev), labels = c(Labels))
# #}
# #ReNamFact <- function(Data, Clmn, "Frm", "To") {
# #  Data$Clmn <- mapvalues(Data$Clmn, from = c(Frm), to = c(To))
# #}
# #ReNamFact(PractDat, Utilities..Orig., "Unk", "AllPub")
# re_data$Utilities <- revalue(re_data$Utilities, c("Unk"="AllPub")) # Substituted "Unk" with "AllPub"
# summary(re_data$Utilities)
# 
# # Change all of the "Twnhs" to "TwnhsE" or "TwnhsI" ??????????????????
# typeof(re_data$BldgType)
# 
# re_data$BldgType <- revalue(re_data$BldgType, c("Twnhs"="TwnhsI"))
# 
# # Convert OverallQual to Factor (to accurately reflect the scale)
# class(re_data$OverallQual)
# 
# mode(re_data$OverallQual)
# 
# re_data$OverallQual <- as.factor(re_data$OverallQual) # Make OverallQual a factor
# 
# levels(re_data$OverallQual) # To see the order of the levels
# 
# is.ordered(re_data$OverallQual) # To check to see if OverallQual is ordered
# 
# # Convert OverallCond to Factor (so that I can implement this data accurately in models)
# class(re_data$OverallCond)
# 
# re_data$OverallCond <- factor(re_data$OverallCond, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
#                               ordered = TRUE) # Convert OverallCond into a factor and order it
# 
# levels(re_data$OverallCond)
# 
# is.ordered(re_data$OverallCond)
# 
# class(re_data$OverallCond) # Check to make sure OverallCond changed to a factor
# 
# 
# # Fill in NA of variable "Exterior1st" with most occuring exterior
# class(re_data$Exterior1st)
# re_data$Exterior1st <- revalue(re_data$Exterior1st, c("NA"="VinylSd"))
# 
# # Fill in NA of variable "Exterior1st" with most occuring exterior
# ratio <- function(x, which) {
#   b <- !is.na(x)
#   sum(x[b] == which) / sum(b)
# }
# 
# ratio(PractDat$MasVnrType, VinylSd)
# RatioFill <- function(x) {
#   b <- is.na(x)
#   x[b] <- sample(x[!b], sum(b), replace=TRUE)
#   x
# }
# RatioFill(PractDat$Exterior1st)
# 
# summary(PractDat$Exterior1st)
# 
# # Fill in NA of MasVnrType by ratio of two most occurring types
# 
# write.csv(re_data, file = "re_dataCln.csv")
# View(re_data)



# sc_matrix <- scatterplotMatrix(~re_data$GarageArea+re_data$LotFrontage)
# sc_matrix