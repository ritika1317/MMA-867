requiredPackages = c('DataExplorer', 'ggthemes','corrplot',
                     'formattable', 'data.table', 'mice', 'caret'                    , 
                     'estimatr', 'glmnet', 'tidyverse', 'dplyr')

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
dim(housefile_test_src)
dim(housefile_y_src)
dim(housefile_feats_src)


str(housefile_test_src)
str(housefile_feats_src)



# Missing information.
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

sapply(housefile_all_feats_src, function(housefile_all_feats_src) sum(is.na(housefile_all_feats_src)))
plot_missing(housefile_all_feats_src)

####Dropping attributes which has high missing values -more than 2000 missing values
housefile_all_feats_src  <- housefile_all_feats_src[, -c(7,74,75)]


str(housefile_all_feats_src)
dim(housefile_all_feats_src)


#Also notice that there are some obvious typos in the dataset.
housefile_all_feats_src$GarageYrBlt[housefile_all_feats_src$GarageYrBlt==2207] <- 2007

#####Replacing NA with None
housefile_all_feats_src$PoolQC1 <- as.character(housefile_all_feats_src$PoolQC)
housefile_all_feats_src$PoolQC1[which(is.na(housefile_all_feats_src$PoolQC))] <- "None"
housefile_all_feats_src$PoolQC <- as.factor(housefile_all_feats_src$PoolQC1)
housefile_all_feats_src <- subset(housefile_all_feats_src,select = -PoolQC1)

housefile_all_feats_src$FireplaceQu1 <- as.character(housefile_all_feats_src$FireplaceQu)
housefile_all_feats_src$FireplaceQu1[which(is.na(housefile_all_feats_src$FireplaceQu))] <- "None"
housefile_all_feats_src$FireplaceQu <- as.factor(housefile_all_feats_src$FireplaceQu1)
housefile_all_feats_src <- subset(housefile_all_feats_src,select = -FireplaceQu1)



sapply(housefile_all_feats_src, function(housefile_all_feats_src) sum(is.na(housefile_all_feats_src)))
plot_missing(housefile_all_feats_src)


imputed_data <- mice(housefile_all_feats_src, m=2, maxit=2,  meth="cart", seed=1)
#imputed_data1 <- mice(housefile_all_feats_src, maxit=2,m=1, meth="pmm", seed=1)

housefile_all_feats_filled = complete(imputed_data, 1)

plot_missing(housefile_all_feats_filled)


######## Add SalePrice is original Train dataset
housefile_feats_input_filled_sp <- merge(x = housefile_feats_input_filled, y = housefile_train_src[,c("Id","SalePrice")],
                                         by.x = "Id", by.y = "Id")

#####Check count for seed value
dim(housefile_feats_input_filled_sp)


####Split Dataset in 80:20 ratio


train_data <- subset.data.frame(housefile_all_feats_filled,Id <= 1168)
test_data <- subset.data.frame(housefile_all_feats_filled,Id >= 1169 & Id <=1460)
predict_data <- subset.data.frame(housefile_all_feats_filled,Id > 1460)

train_data <- merge(x = train_data, y = housefile_train_src[,c("Id","SalePrice")],
                    by.x = "Id", by.y = "Id")

test_data <- merge(x = test_data, y = housefile_train_src[,c("Id","SalePrice")],
                   by.x = "Id", by.y = "Id")



fit <- lm(SalePrice ~  . ,train_data) #build a model on training data
summary(fit)


str(train_data)
fit.log.m <-lm(log(SalePrice) ~ log(LotArea) + Street + LotConfig + 
                 +                    LandSlope + Neighborhood + Condition1 + log(OverallQual) + 
                 +                    log(OverallCond) + log(YearBuilt) +   
                 +                    MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinSF2 + 
                 +                    BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenQual + 
                 +                    GarageQual + ScreenPorch + PoolArea ,train_data)
summary(fit.log.m)

predicted.prices.test <-exp(predict(fit.log.m, test_data)) # predicting prices. Note, the fit.log model predicts the log of price, hence add exp() 

percent.errors <- abs((test_data$SalePrice - predicted.prices.test)/test_data$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE)


predicted.prices.log<-exp(predict(fit.log.m, predict_data)) # predicting prices. Note, the fit.log model predicts the log of price, hence add exp() 

my_submission <- data_frame('Id' = as.integer(predict_data$Id), 'SalePrice' = predicted.prices.log)
write_csv(my_submission, 'Predicted Sales Price33.csv')

