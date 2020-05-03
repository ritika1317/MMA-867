requiredPackages = c('DataExplorer', 'corrplot', 'ggplot2', 'olsrr',
                     'formattable', 'data.table', 'mice', 'caret'                    , 
                      'glmnet', 'tidyverse', 'dplyr')

for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only=TRUE)
}

setwd("C:\\Users\\imrit\\Desktop\\MMA\\Predictive Modelling_MMA-867")

#### Reading CSV file.
housefile_train_src <- read.csv('train.csv',header = TRUE)
housefile_test_src <- read.csv('test.csv',header = TRUE)

# Getting Features and Target.
housefile_y_src <- housefile_train_src$SalePrice
housefile_feats_src <- housefile_train_src[,-ncol(housefile_train_src)]

####Dimensions of both the dataframe
dim(housefile_y_src)
dim(housefile_feats_src)
dim(housefile_test_src)


######Structure of the dataset
str(housefile_test_src)
str(housefile_feats_src)


######Check for missing & NA values
sapply(housefile_feats_src, function(housefile_feats_src) sum(is.na(housefile_feats_src)))
plot_missing(housefile_feats_src)

sapply(housefile_test_src, function(housefile_test_src) sum(is.na(housefile_test_src)))
plot_missing(housefile_test_src)

#####Binding both the Dataset 
housefile_all_feats_src <- rbind(housefile_feats_src, housefile_test_src)
dim(housefile_all_feats_src)

#####Replacing blank & NA will 0 
housefile_all_feats_src$LotFrontage[is.na(housefile_all_feats_src$LotFrontage)] <- 0
housefile_all_feats_src$LotFrontage[is.na(housefile_all_feats_src$LotFrontage)] <- 0
housefile_all_feats_src$BsmtFinSF1[is.na(housefile_all_feats_src$BsmtFinSF1)] <- 0
housefile_all_feats_src$BsmtFinSF2[is.na(housefile_all_feats_src$BsmtFinSF2)] <- 0
housefile_all_feats_src$BsmtUnfSF[is.na(housefile_all_feats_src$BsmtUnfSF)] <- 0
housefile_all_feats_src$TotalBsmtSF[is.na(housefile_all_feats_src$TotalBsmtSF)] <- 0
housefile_all_feats_src$BsmtFullBath[is.na(housefile_all_feats_src$BsmtFullBath)] <- 0
housefile_all_feats_src$BsmtHalfBath[is.na(housefile_all_feats_src$BsmtHalfBath)] <- 0
housefile_all_feats_src$GarageYrBlt[is.na(housefile_all_feats_src$GarageYrBlt)] <- 0
housefile_all_feats_src$GarageCars[is.na(housefile_all_feats_src$GarageCars)] <- 0
housefile_all_feats_src$GarageArea[is.na(housefile_all_feats_src$GarageArea)] <- 0


#####Replacing NA with None
housefile_all_feats_src$PoolQC1 <- as.character(housefile_all_feats_src$PoolQC)
housefile_all_feats_src$PoolQC1[which(is.na(housefile_all_feats_src$PoolQC))] <- "None"
housefile_all_feats_src$PoolQC <- as.factor(housefile_all_feats_src$PoolQC1)
housefile_all_feats_src <- subset(housefile_all_feats_src,select = -PoolQC1)

housefile_all_feats_src$FireplaceQu1 <- as.character(housefile_all_feats_src$FireplaceQu)
housefile_all_feats_src$FireplaceQu1[which(is.na(housefile_all_feats_src$FireplaceQu))] <- "None"
housefile_all_feats_src$FireplaceQu <- as.factor(housefile_all_feats_src$FireplaceQu1)
housefile_all_feats_src <- subset(housefile_all_feats_src,select = -FireplaceQu1)



####Dropping attributes which has high missing values -more than 2000 missing values
housefile_all_feats_src  <- housefile_all_feats_src[, -c(7,74,75)]


#Correct the typing error in year in the dataset.
housefile_all_feats_src$GarageYrBlt[housefile_all_feats_src$GarageYrBlt==2207] <- 2007


######Plot the Dataset to check the value after replacing NA with 0 & None & dropping some
sapply(housefile_all_feats_src, function(housefile_all_feats_src) sum(is.na(housefile_all_feats_src)))
plot_missing(housefile_all_feats_src)

######Impute the missing values
imputed_data <- mice(housefile_all_feats_src, m=5, maxit=30,  meth="cart", seed=1)

housefile_all_feats_filled = complete(imputed_data, 1)

######Check after imputation, all missing values should be imputed 
plot_missing(housefile_all_feats_filled)


#####Spilt dataset in in train & predict set
housefile_feats_train <- housefile_all_feats_filled[1:nrow(housefile_feats_src), ]
predict_data <- housefile_all_feats_filled[(1 + nrow(housefile_feats_src)):nrow(housefile_all_feats_filled),]


######## Add SalePrice is original Train dataset
housefile_train_imputed <- merge(x = housefile_feats_train, y = housefile_train_src[,c("Id","SalePrice")],
                                         by.x = "Id", by.y = "Id")


####Split Dataset 
train_data <- subset.data.frame(housefile_train_imputed,Id <= 1168)
test_data <- subset.data.frame(housefile_train_imputed,Id >= 1169 & Id <=1460)


#create a Tableau-like plot of SalePrice vs LotSize with YearBuilt 
ggplot(train_data, aes(x=YearBuilt, y=SalePrice, coArealor=MSZoning)) +  geom_point(size=3) 


ggplot(train_data, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=50), 
             colour='red', lwd=2) 


# Plotting 'GrLivArea' vs SalePrice
ggplot(train_data,aes(y=SalePrice,x=GrLivArea))+geom_point()



#######Build a model on training data
fit <- lm(SalePrice ~  . ,train_data) 
summary(fit)
####With all attributes
fit <- lm(SalePrice ~  . ,train_data) #build a model on training data
summary(fit)
#####Adjusted R-squared:  0.9144 



fit1 <- lm(SalePrice~MSZoning+LotArea+Street+
             LotConfig+LandSlope+Neighborhood+Condition1+OverallQual+OverallCond+ YearBuilt+RoofStyle+
             RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +BsmtUnfSF+X1stFlrSF+
             X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+ScreenPorch+PoolArea,train_data) #build a model on training data
summary(fit1)

sapply(test_data, function(test_data) sum(is.na(test_data)))
plot_missing(test_data)


try_predicted <-predict(fit1, test_data) #predict the prices for testing the model on test data
percent.errors <- abs((train_data$SalePrice - try_predicted)/train_data$SalePrice )*100
mean(percent.errors) 



####Log() model#########################

str(train_data)
fit.log <-lm(log(SalePrice) ~ log(LotArea) + Street + LotConfig + 
               LandSlope + Neighborhood + Condition1 + log(OverallQual) + 
               log(OverallCond) + log(YearBuilt) + RoofStyle + RoofMatl + 
               MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinSF2 + 
               BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenQual + 
               GarageQual + ScreenPorch + PoolArea ,train_data)
summary(fit.log)

predicted.prices.log<-exp(predict(fit.log, test_data)) # predicting prices. Note, the fit.log model predicts the log of price, hence add exp() 

####Log() model END #########################



model_1 <- lm(log(SalePrice) ~ PoolArea + GarageQual + ExterQual + Condition1 + BsmtQual  + KitchenQual   +
                BsmtUnfSF + BsmtExposure + ScreenPorch + log(LotArea) + YearBuilt + OverallQual + Neighborhood +
                OverallCond + RoofMatl + X2ndFlrSF + X1stFlrSF,train_data )

summary(model_1)

predicted.prices.log1<-exp(predict(model_1, test_data)) # predicting prices. Note, the fit.log model predicts the log of price, hence add exp() 

predicted.prices.log1<-exp(predict(model_1, predict_data)) # predicting prices. Note, the fit.log model predicts the log of price, hence add exp() 

write.csv(predicted.prices.log1, file = "Predicted Sales Prices_v6.csv") # export the predicted prices into a CSV file


###
### Interactions 
###

fit.log.i <- lm(log(SalePrice) ~ log(LotArea)* Street + LotConfig + 
                  LandSlope + Neighborhood + Condition1 + OverallQual + 
                  OverallCond + YearBuilt + RoofStyle + RoofMatl + 
                  MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinSF2 + 
                  BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenQual + 
                  GarageQual + ScreenPorch + PoolArea + PoolQC ,train_data)

summary(fit.log.i)

predicted.prices.log.i<-exp(predict(fit.log.i, test_data)) # predicting prices. Note, the fit.log model predicts the log of price, hence add exp() 




###
### Variable Selection (Forward/Backward/Stepwise regression)
###

fit.log.ii<-lm(log(SalePrice) ~ log(LotArea)* Street + LotConfig + 
                 LandSlope + Neighborhood + Condition1 + OverallQual + 
                 OverallCond + YearBuilt + RoofStyle + RoofMatl + 
                 MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinSF2 + 
                 BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenQual + 
                 GarageQual + ScreenPorch + PoolArea + PoolQC ,train_data)
summary(fit.log.ii)


fit.log.step<-step(lm(log(SalePrice) ~ log(LotArea)* Street + LotConfig + 
                        LandSlope + Neighborhood + Condition1 + OverallQual + 
                        OverallCond + YearBuilt + RoofStyle + RoofMatl + 
                        MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinSF2 + 
                        BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenQual + 
                        GarageQual + ScreenPorch + PoolArea + PoolQC ,train_data),direction="backward")
summary(fit.log.step)

fit.log.step<-step(lm(log(SalePrice) ~ log(LotArea)* Street + LotConfig + 
                        LandSlope + Neighborhood + Condition1 + OverallQual + 
                        OverallCond + YearBuilt + RoofStyle + RoofMatl + 
                        MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinSF2 + 
                        BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenQual + 
                        GarageQual + ScreenPorch + PoolArea + PoolQC ,train_data),direction="both")

summary(fit.log.step)

