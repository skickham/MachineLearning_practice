## The Curse of Dimensionality

#Q1: Ridge Regression 
library(glmnet)

prostate <- read.table("[06] Prostate.txt", header = TRUE)
prostate = na.omit(prostate)
head(prostate)
names(prostate)
str(prostate)
cor(prostate)
# high cor (>50%) between svi and lcp, lcp and lcavol, svi and lcavol, 
# as well as some moderate correltions



# 1 : create training (80%) and test sets

# Need matrices for glmnet() function. 
# Automatically conducts conversions as well for factor variables into dummy variables.
x = model.matrix(lpsa ~ ., prostate)[, -1] 
# Dropping the intercept (lpsa) column b/c thats what we want to predict.
y = prostate$lpsa
# the y is the response column we just dropped

set.seed(0)  # reproducible results
train = sample(1:nrow(x), 8*nrow(x)/10)   # randomly select 80% of the data to be training
test = (-train)   # neg train gives remaning rows in sample after taking out train 
y.test = y[test]  

length(train)/nrow(x)   # confirm 80%
length(y.test)/nrow(x)   # confirm 20% 

# 100 values of lambda from 10^-2 to 10^5 in a vector
grid = 10^seq(5, -2, length = 100)





# 2 : fit ridge regression models on training data across many lambdas, save coefficients

# x train is input, y train is response, grid is lambda vectors to test, alpha is 0 is ridge
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid) 
dim(coef(ridge.models.train)) # 100 lambdas for 9 predictors
train.coeffs = coef(ridge.models.train) # save cooefs to object




# 3 : plot coefficients and interpret shrinkage
plot(ridge.models.train, xvar = "lambda", label = TRUE, main = "Ridge Regression")
# each curve is one prediction attached to one predictor
# I have no idea how to interpret this!!!!!



# 4 : 10-fold cross validation on training data and save
set.seed(0)   # reproducible results
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)


# 5 : plot & interpret
plot(cv.ridge.out, main = "Ridge Regression\n")
# when log of lambda is between -2 and 0 (lambdas between 0.01 and 1), the RSS is minimized 



# 6 : best lambda?
bestlambda.ridge = cv.ridge.out$lambda.min  # get minimum MSE for lambda
bestlambda.ridge    # gives 0.135 as best lambda
log(bestlambda.ridge)   # matches -2 from the Ridge regression graph



# 7 : test MSE of this lambda?
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
# using the training model with lambda (s) set as the best lambda we found, 
# get the predict values of the test data set of x predictors

# find the average square difference between predicted results and actual results
mean((ridge.bestlambdatrain - y.test)^2)
# the MSE is 0.49 which matches minimum MSE (y-value) 
# from Ridge Regression Graph




# 8 : Refit ridge regression with best lambda to full dataset, interpret coefficient estimates
ridge.bestlambda = glmnet(x, y, alpha = 0, lambda = bestlambda.ridge) 
ridge.bestlambda$beta
#coef(ridge.bestlambda) # same as above, just no beta0
dim(coef(ridge.bestlambda)) # 100 lambdas for 9 predictors




# 9 : overall MSE? how does this compare to previous?
ridge.bestlambda.predict = predict(ridge.bestlambda, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda.predict - y)^2)
# MSE is 0.45 which is lower than before! 
# This is good but I do not know why
# it also may not be good 


#Q2: Lasso Regression

# 1 : Repeat everything with lasso

#x and y and training and test sets already assigned
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid) 
dim(coef(lasso.models.train)) 
train.coeffs = coef(lasso.models.train)
# 3 : plot coefficients and interpret shrinkage
plot(lasso.models.train, xvar = "lambda", label = TRUE, main = "Lasso Regression") 
# feature selection occurs but I really don't know how to read this graph well.  
# 4 : 10-fold cross validation on training data and save
set.seed(0)   
cv.lasso.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 1, nfolds = 10)
# 5 : plot & interpret
plot(cv.lasso.out, main = "Lasso Regression\n")
# minimum betw -3 and -2
# 6 : best lambda?
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso    # gives 0.037 as best lambda
log(bestlambda.lasso)   # matches -3.3 from the Lasso regression graph
# 7 : test MSE of this lambda?
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
# find the average square difference between predicted results and actual results
mean((lasso.bestlambdatrain - y.test)^2)   # MSE is 0.5
# 8 : Refit lasso regression with best lambda to full dataset, interpret coefficient estimates
lasso.bestlambda = glmnet(x, y, alpha = 1, lambda = bestlambda.lasso) 
lasso.bestlambda$beta  # training new data shows feature selection (lcp eliminated)
# 9 : overall MSE? how does this compare to previous?
lasso.bestlambda.predict = predict(lasso.bestlambda, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda.predict - y)^2)  # MSE is 0.46 (lower than before, close to ridge's 0.45)


# 2 : compare and contrasts ridge and lasso, explain which one to choose
# because svi and lcp seem correlated, I would use the lasso regression
# also, the MSE at the end is 45% for both so I would choose the model that's easier 
# to interpret, i.e. the one with less features

