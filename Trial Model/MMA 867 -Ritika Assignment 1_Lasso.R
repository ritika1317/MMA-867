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

#########Using VIF to check for multicollinearity and variable selection 
ols_vif_tol(fit)


###
### Interactions 
###

model <- lm(log(SalePrice) ~  MSZoning + LotArea + PoolArea + GarageQual + ExterQual + Condition1 + 
              BsmtUnfSF + BsmtExposure + ScreenPorch + log(LotArea) * Neighborhood  +  KitchenQual +
              sqrt(TotalBsmtSF) * Neighborhood + log(GrLivArea) * Neighborhood+  sqrt(GrLivArea) * Neighborhood+
              LotConfig + Street + log(OverallQual) * Neighborhood  + Neighborhood + GrLivArea + BsmtQual+
              sqrt(GarageArea)*log(LotArea)+ log(YearBuilt)* Neighborhood + YearRemodAdd + BldgType + 
              sqrt(MasVnrArea)+ OverallCond + X2ndFlrSF + log(X1stFlrSF),train_data) 


# predicting prices. Note, the fit.log model predicts the log of price, hence add exp() 
predicted.prices <-exp(predict(model, test_data)) 

#calculate absolute percentage errors
percent.errors <- abs((test_data$SalePrice - predicted.prices)/test_data$SalePrice)*100 
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE)



###
### Variable Selection - Stepwise regression
###

model.log.step <- step(lm(log(SalePrice) ~  MSZoning + LotArea + PoolArea + GarageQual + ExterQual + Condition1 + 
                            BsmtUnfSF + BsmtExposure + ScreenPorch + log(LotArea) * Neighborhood  +  KitchenQual +
                            sqrt(TotalBsmtSF) * Neighborhood + log(GrLivArea) * Neighborhood+  sqrt(GrLivArea) * Neighborhood+
                            LotConfig + Street + log(OverallQual) * Neighborhood  + Neighborhood + GrLivArea + BsmtQual+
                            sqrt(GarageArea)*log(LotArea)+ log(YearBuilt)* Neighborhood + YearRemodAdd + BldgType + 
                            sqrt(MasVnrArea)+ OverallCond + X2ndFlrSF + log(X1stFlrSF),train_data),direction="both")

summary(model.log.step)

###
### Regularizations (LASSO and ridge)
###

#create the y variable and matrix (capital X) of x variables 
y<-log(train_data$SalePrice)
dim(train_data)
X<-model.matrix(Id ~ MSZoning + PoolArea + GarageQual + 
                  ExterQual + Condition1 + BsmtQual + BsmtUnfSF + BsmtExposure + 
                  ScreenPorch + log(LotArea) + Neighborhood + KitchenQual + 
                  sqrt(TotalBsmtSF) + log(GrLivArea) + sqrt(GrLivArea) + log(OverallQual) + 
                  GrLivArea + sqrt(GarageArea) + log(YearBuilt) + YearRemodAdd + 
                  BldgType + OverallCond + log(X1stFlrSF) + Neighborhood:sqrt(TotalBsmtSF) + 
                  Neighborhood:log(GrLivArea) + Neighborhood:sqrt(GrLivArea) + 
                  Neighborhood:log(YearBuilt), housefile_all_feats_filled)[,-1]

X<-cbind(housefile_all_feats_filled$Id,X)


# split X into testing, trainig/holdout and prediction
X.training<-subset(X,X[,1]<=1168)
X.testing<-subset(X, (X[,1]>=1169 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

###
### Regularizations (LASSO and ridge)
###

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-8.5,-6),ylim=c(0.006,0.008)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))

mean(abs((lasso.testing-test_data$SalePrice)/test_data$SalePrice)*100) #calculate and display MAPE

#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs((ridge.testing-test_data$SalePrice)/test_data$SalePrice)*100) #calculate and display MAPE

# comparing the performance on the testing set, LASSO is better, so use it for prediction
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
write.csv(predicted.prices.log.i.lasso, file = "Predicted SalePrice_LASSO_2.csv") # export the predicted prices into a CSV file
#####Predict SalePrice of Predict Dataset
predicted.prices.log.i.ridge <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.prediction))


