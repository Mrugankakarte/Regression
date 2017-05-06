#Loading data
library(caret)
raw_data <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv",stringsAsFactors = F)
test$SalePrice <- 0
alldata <- rbind(raw_data, test)

summary(raw_data)

#####################################################################################################
###Cleaning##########################################################################################
#####################################################################################################

summary(alldata)

alldata$MSZoning[is.na(alldata$MSZoning)] <- "RL"
alldata$LotFrontage[is.na(alldata$LotFrontage)] <- median(alldata$LotFrontage, na.rm = T)
alldata$Alley[is.na(alldata$Alley)] <- "None"
alldata$Utilities[is.na(alldata$Utilities)] <- "AllPub"
alldata$Exterior1st[is.na(alldata$Exterior1st)] <- "VinylSd"
alldata$Exterior2nd[is.na(alldata$Exterior2nd)] <- "VinylSd"
alldata$MasVnrType[is.na(alldata$MasVnrType)] <- "None"
alldata$MasVnrArea[is.na(alldata$MasVnrArea)] <- 0
alldata$BsmtQual[is.na(alldata$BsmtQual)] <- "None"
alldata$BsmtCond[is.na(alldata$BsmtCond)] <- "None"
alldata$BsmtExposure[is.na(alldata$BsmtExposure)] <- "None"
alldata$BsmtFinType1[is.na(alldata$BsmtFinType1)] <- "None"
alldata$BsmtFinType2[is.na(alldata$BsmtFinType2)] <- "None"
alldata$BsmtFinSF1[is.na(alldata$BsmtFinSF1)] <- median(alldata$BsmtFinSF1, na.rm = T)
alldata$BsmtFinSF2[is.na(alldata$BsmtFinSF2)] <- median(alldata$BsmtFinSF2, na.rm = T)
alldata$BsmtUnfSF[is.na(alldata$BsmtUnfSF)] <- median(alldata$BsmtUnfSF, na.rm = T)
alldata$TotalBsmtSF[is.na(alldata$TotalBsmtSF)] <- median(alldata$TotalBsmtSF, na.rm = T)
alldata$Electrical[is.na(alldata$Electrical)] <- "SBrkr"
alldata$BsmtFullBath[is.na(alldata$BsmtFullBath)] <- median(alldata$BsmtFullBath, na.rm = T)
alldata$BsmtHalfBath[is.na(alldata$BsmtHalfBath)] <- median(alldata$BsmtHalfBath, na.rm = T)
alldata$KitchenQual[is.na(alldata$KitchenQual)] <- "TA"
alldata$Functional[is.na(alldata$Functional)] <- "Typ"
alldata$FireplaceQu[is.na(alldata$FireplaceQu)] <-"None"
alldata$GarageType[is.na(alldata$GarageType)] <- "None"
alldata$GarageYrBlt[is.na(alldata$GarageYrBlt)] <- "None"
alldata$GarageFinish[is.na(alldata$GarageFinish)] <- "None"
alldata$GarageCars[is.na(alldata$GarageCars)] <- median(alldata$GarageCars, na.rm = T)
alldata$GarageArea[is.na(alldata$GarageArea)] <- median(alldata$GarageArea, na.rm = T)
alldata$GarageQual[is.na(alldata$GarageQual)] <- "None"
alldata$GarageCond[is.na(alldata$GarageCond)] <- "None"
alldata$PoolQC[is.na(alldata$PoolQC)] <- "None"
alldata$Fence[is.na(alldata$Fence)] <- "None"
alldata$MiscFeature[is.na(alldata$MiscFeature)] <- "None"
alldata$SaleType[is.na(alldata$SaleType)] <- "WD"
alldata$GarageYrBlt <- NULL

#One-hot-encoding features:
library(ade4)
library(data.table)
#one hot encoding categorical variables
cat_variables = c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig'
                     ,'LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle'
                     ,'RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual'
                     ,'ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1'
                     ,'BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual'
                     ,'Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond'
                     ,'PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

num_variables <- c('MSSubClass','LotFrontage','LotArea','OverallQual','OverallCond','YearBuilt','YearRemodAdd'
                   ,'MasVnrArea','BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','X1stFlrSF','X2ndFlrSF'
                   ,'LowQualFinSF','GrLivArea','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath','BedroomAbvGr'
                   ,'KitchenAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars','GarageArea','WoodDeckSF'
                   ,'OpenPorchSF','EnclosedPorch','X3SsnPorch','ScreenPorch','PoolArea','MiscVal','MoSold','YrSold')

for (f in cat_variables){
      df_all_dummy = acm.disjonctif(alldata[f])
      alldata[f] = NULL
      alldata = cbind(alldata, df_all_dummy)
}

#Log transforming numerical variables
transf_num <-NULL
for(f in num_variables){
      transf_num <- log(alldata[f]+1)
      alldata[f] <- NULL
      alldata <- cbind(alldata, transf_num)
      }
transf_num <-as.data.frame(transf_num)
Summary(alldata)


ntrain <- alldata[1:1460,]
ntest <- alldata[1461:2919,]

#write.csv(ntrain, "New train.csv", row.names = F)
#write.csv(ntest, "New test.csv", row.names = F)


write.csv(ntrain, "transformed train.csv", row.names = F)
write.csv(ntest, "transformed test.csv", row.names = F)

#####################################################################################################