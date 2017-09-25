#######################################################
#######################################################
###### [06] Regularization and Cross validation #######
#######################################################
#######################################################

####NOTE: 
# we didnt do the normalization for any of these examples for simplicity, but we always should!

##########################
#####Ridge Regression#####
##########################
library(ISLR)  #intro to statistical learning
Hitters = na.omit(Hitters)
help(Hitters)
head(Hitters)

#Need matrices for glmnet() function. Automatically conducts conversions as well
#for factor variables into dummy variables.
x = model.matrix(Salary ~ ., Hitters)[, -1] #Dropping the intercept (salary) column b/c thats what we want to predict.  #type of x is matrix
y = Hitters$Salary

#Values of lambda over which to check.
grid = 10^seq(5, -2, length = 100)   # choose 100 possible lambda from 10^-2 to 10^5 in a vector

#Fitting the ridge regression. Alpha = 0 for ridge regression.
library(glmnet)
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)
# x is input, y is response, lambda is lambda vectors to test, alpha is 0 is ridge, alpha as 1 is lasso :)

dim(coef(ridge.models)) #20 different coefficients, estimated 100 times --
# what does this mean????? IDK : 100 lambda for 100 model, and each lambda has 20 coefficients, meaning 19 predictors plus beta0!!
# dim(x) is 263 observations by 19 columns (predictors)


#once each per lambda value.
coef(ridge.models) #Inspecting the various coefficient estimates.

#What do the estimates look like for a smaller value of lambda?
ridge.models$lambda[80] #Lambda = 0.2595.
coef(ridge.models)[, 80] #Estimates not close to 0.
sqrt(sum(coef(ridge.models)[-1, 80]^2)) #L2 norm is 136.8179.
#smaller lambda shows higher penalty


#What do the estimates look like for a larger value of lambda?
ridge.models$lambda[15] #Lambda = 10,235.31.
coef(ridge.models)[, 15] #Most estimates close to 0.
sqrt(sum(coef(ridge.models)[-1, 15]^2)) #L2 norm is 7.07.
#larger lambda shows smaller penalty


#Visualizing the ridge regression shrinkage.
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")
#each curve is one prediction attached to one predictor

#Can use the predict() function to obtain ridge regression coefficients for a
#new value of lambda, not necessarily one that was within our grid:
predict(ridge.models, s = 50, type = "coefficients")
#tool for best coefficients but very often not that precise????



###############cross valdation
#Creating training and testing sets. Here we decide to use a 70-30 split with
#approximately 70% of our data in the training set and 30% of our data in the
#test set.
set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)   # select 70% of them
test = (-train)   #neg train gives remaning rows in sample after taking out train 
y.test = y[test]

length(train)/nrow(x)   # confirm 70%
length(y.test)/nrow(x)   # confirm 30% 

#Let's attempt to fit a ridge regression using some arbitrary value of lambda;
#we still have not yet figured out what the best value of lambda should be!
#We will arbitrarily choose 5. We will now use the training set exclusively.
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid) 
ridge.lambda5 = predict(ridge.models.train, s = 5, newx = x[test, ])    # only use x b/c ...??? 
mean((ridge.lambda5 - y.test)^2)    # MSE ^ squared

#Here, the MSE is approximately 115,541.

#What would happen if we fit a ridge regression with an extremely large value
#of lambda? Essentially, fitting a model with only an intercept:
ridge.largelambda = predict(ridge.models.train, s = 1e10, newx = x[test, ])
mean((ridge.largelambda - y.test)^2)

#Here, the MSE is much worse at aproximately 208,920.

#Instead of arbitrarily choosing random lambda values and calculating the MSE
#manually, it's a better idea to perform cross-validation in order to choose
#the best lambda over a slew of values.





#########################################################
# Running 10-fold cross validation. #####################
#########################################################

set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
# 100 red dots, each one gives MSE for each model 
# also notice that bigger lambda gives less variance
# we want one with smallest error
bestlambda.ridge = cv.ridge.out$lambda.min  # get minimum MSE for lambda
bestlambda.ridge
log(bestlambda.ridge)

#What is the test MSE associated with this best value of lambda?
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)

#Here the MSE is lower at approximately 113,173; a further improvement
#on that which we have seen above. With "cv.ridge.out", we can actually access
#the best model from the cross validation without calling "ridge.models.train"
#or "bestlambda.ridge":
ridge.bestlambdatrain = predict.cv.glmnet(cv.ridge.out, s ="lambda.min", newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)

### Alternative method with caret
### doing cross validation with caret is one of most important uses of caret
library(caret)
set.seed(0)
train_control = trainControl(method = 'cv', number=10)  #specifies type of cross validation with 10 folds
tune.grid = expand.grid(lambda = grid, alpha=c(0))    # stick to rideg with alpha = c(0)
ridge.caret = train(x[train, ], y[train],              # train the data 
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)

### Plot the tuning object:
plot(ridge.caret, xTrans=log)
# caret is easier to see the minimum at 6.7
# better than previous model so use this one 

### We see caret::train returns a different result from the
### one by cv.glmnet. By comparing the ridge.caret$results
### and cv.ridge.out$cvm, it's most likely to be rounding and 
### averaging.

### Predicting with the final model
pred = predict.train(ridge.caret, newdata = x[test,])
mean((pred - y[test])^2)

### Note: there is a "finalModel in ridge.caret. But unfortunately, using it
###       for predicting often results in error.There is some issue with the 
###       compactibility of the "predict" function and the model from caret.train
predict(ridge.caret$finalModel, newdata = x[test,])   # newx?????????


##########################
#####Lasso Regression#####
##########################
#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)

dim(coef(lasso.models)) #20 different coefficients, estimated 100 times --
#once each per lambda value.
coef(lasso.models) #Inspecting the various coefficient estimates.

#What do the estimates look like for a smaller value of lambda?
lasso.models$lambda[80] #Lambda = 0.2595.
coef(lasso.models)[, 80] #Most estimates not close to 0.
sum(abs(coef(lasso.models)[-1, 80])) #L1 norm is 228.1008.

#What do the estimates look like for a larger value of lambda?
lasso.models$lambda[15] #Lambda = 10,235.31.
coef(lasso.models)[, 15] #Estimates all 0.
sum(abs(coef(lasso.models)[-1, 15])) #L1 norm is essentially 0.

#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")
# some get to zero earlier
# lasso reg does feture selection for us by us picking lambda and anything that goes to zero beforehand can be eliminated?
# are the lines the beta values for each feature?
# notice some lines are not as high in magnitude but reach zero later
# also notice beta1 gets larger (more negative) before reaching zero
#Can use the predict() function to obtain lasso regression coefficients for a
#new value of lambda, not necessarily one that was within our grid:
predict(lasso.models, s = 50, type = "coefficients")

#Let's attempt to fit a lasso regression using some arbitrary value of lambda;
#we still have not yet figured out what the best value of lambda should be!
#We will arbitrarily choose 5. We will now use the training set exclusively.
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.lambda5 = predict(lasso.models.train, s = 5, newx = x[test, ])
mean((lasso.lambda5 - y.test)^2)

#Here, the MSE is approximately 107,660.

#Instead of arbitrarily choosing random lambda values and calculating the MSE
#manually, it's a better idea to perform cross-validation in order to choose
#the best lambda over a slew of values.

#Running 10-fold cross validation.
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")   # does the top axis show the number of remaining features
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso   # this is the actual lambda value (not seen on the graph)
log(bestlambda.lasso)  # this correlates with the minimum on the graph

#What is the test MSE associated with this best value of lambda?
lasso.bestlambdatrain = predict(cv.lasso.out, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

#This time the MSE is actually higher at approximately 113,636. What happened?

### Exercise: Tune the same lasso model with caret!

