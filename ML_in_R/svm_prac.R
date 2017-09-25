###################################################
###################################################
#####[10] Support Vector Machines Lecture Code#####
###################################################
###################################################



###################################
#####Maximal Margin Classifier#####
###################################
#Generating linearly separable data. randomly as we always do 
set.seed(0)
x1 = c(rnorm(100, 0, 4), rnorm(100, 1, 3)) # a vector of 200 observations
x2 = c(rnorm(100, 0, 1), rnorm(100, 6, 1)) #
y = as.factor(c(rep(-1, 100), rep(1, 100))) # y values of -1 and 1 
linearly.separable = data.frame(x1, x2, y) # put the vectors into a dataframe

#Plotting the linearly separable data. # clearly separable
plot(linearly.separable$x1, linearly.separable$x2, col = linearly.separable$y)

#Creating training and test sets.
set.seed(0)
train.index = sample(1:200, 200*.8) # randomly select 80% of observations for training set
test.index = -train.index # test index is everybody else

#Importing the e1071 library in order to use the svm() function to fit support
#vector machines.
library(e1071) 

#Fitting a maximal margin classifier to the training data.
svm.mmc.linear = svm(y ~ .,                      # Familiar model fitting notation.
                     data = linearly.separable,  # Using the linearly separable data.
                     subset = train.index,       # Using the training data.
                     kernel = "linear",          # Using a linear kernel.
                     cost = 1e6)                 # A very large cost; default is 1. 
# why do we want so large a cost??????? b/c dont allow any misclassifications!

#Visualizing the results of the maximal margin classifier.
plot(svm.mmc.linear, linearly.separable[train.index, ])
# why is the boundary not smooth? IDK but i think it doesnt matter 

#Additional information for the fit.
summary(svm.mmc.linear)
# 3 support vectors because 3 missclassified options???? NO!
# 3 closest to boundary?? kind of , the ones that fall on or in the margin  

#Finding the indices of the support vectors.
svm.mmc.linear$index
# i dont see how grabbing these actually matter 

#Predicting on the test data.
ypred = predict(svm.mmc.linear, linearly.separable[test.index, ])
table("Predicted Values" = ypred, "True Values" = linearly.separable[test.index, "y"])

#Adding a single point to display the sensitivity of the maximal margin classifier.
linearly.separable2 = rbind(linearly.separable, c(-5, 3, 1))
plot(linearly.separable2$x1, linearly.separable2$x2, col = linearly.separable2$y)

#Fitting a maximal margin classifier to the new data.
svm.mmc.linear2 = svm(y ~ .,
                      data = linearly.separable2,
                      kernel = "linear",
                      cost = 1e6)
# if we used a smaller cost, then the SVM would allow for some misclassification

#Visualizing the results of the maximal margin classifier; comparing the output.
plot(svm.mmc.linear, linearly.separable[train.index, ]) #Old model.
plot(svm.mmc.linear2, linearly.separable2) #New model.
# the oundary is shown to be unstable 

#Additional information for the fit.
summary(svm.mmc.linear2)

#Finding the indices of the support vectors.
svm.mmc.linear2$index



###################################
#####Support Vector Classifier#####
###################################
#Fitting a support vector classifier by reducing the cost of a misclassified
#observation.
svm.svc.linear2 = svm(y ~ .,
                      data = linearly.separable2,
                      kernel = "linear",
                      cost = 1) # much smaller cost

#Visualizing the results of the support vector classifier.
plot(svm.svc.linear2, linearly.separable2)
summary(svm.svc.linear2)
svm.svc.linear2$index

#What happens if we reduce the cost even more?
svm.svc.linear3 = svm(y ~ .,
                      data = linearly.separable2,
                      kernel = "linear",
                      cost = .1)   # even smaller cost....get a lot more Xs (support vectors)
plot(svm.svc.linear3, linearly.separable2)
summary(svm.svc.linear3)
svm.svc.linear3$index

#We generally find the best cost parameter by implementing the cross-validation
#procedure; this isn't as interesting with linearly separable data. Let's generate
#some data that is not linearly separable.
set.seed(0)
x1 = c(rnorm(100, -1, 1), rnorm(100, 1, 1))
x2 = c(rnorm(100, -1, 1), rnorm(100, 1, 1))
y = as.factor(c(rep(-1, 100), rep(1, 100)))
overlapping = data.frame(x1, x2, y)
plot(overlapping$x1, overlapping$x2, col = overlapping$y)
#overlapping plot created 


#Implement cross-validation to select the best parameter value of the cost.
set.seed(0)
cv.svm.overlapping = tune(svm,
                          y ~ .,
                          data = overlapping[train.index, ],
                          kernel = "linear",
                          ranges = list(cost = 10^(seq(-5, .5, length = 100))))
# tune function to figure out amount of cost

#Inspecting the cross-validation output.
summary(cv.svm.overlapping)
# the best cost we got was 0.005 (as selected by the minimal error...dispersion is the std dev of the error)


#Plotting the cross-validation results.   # SHOWS THE LEARNING CURVE
plot(cv.svm.overlapping$performances$cost,
     cv.svm.overlapping$performances$error,
     xlab = "Cost",
     ylab = "Error Rate",
     type = "l")

#Inspecting the best model.
best.overlapping.model = cv.svm.overlapping$best.model   # takes the best cost
summary(best.overlapping.model)

#Using the best model to predict the test data.
ypred = predict(best.overlapping.model, overlapping[test.index, ])
table("Predicted Values" = ypred, "True Values" = overlapping[test.index, "y"])







# left off here










#Constructing and visualizing the final model.
svm.best.overlapping = svm(y ~ .,
                           data = overlapping,
                           kernel = "linear",
                           cost = best.overlapping.model$cost)
plot(svm.best.overlapping, overlapping)
summary(svm.best.overlapping)
svm.best.overlapping$index
ypred = predict(svm.best.overlapping, overlapping)
table("Predicted Values" = ypred, "True Values" = overlapping[, "y"])



#################################
#####Support Vector Machines#####
#################################
#What happens if we have data that is not linearly separable?
set.seed(0)
x1 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100))
x2 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100))
y = as.factor(c(rep(-1, 200), rep(1, 100)))
nonlinear = data.frame(x1, x2, y)
plot(nonlinear$x1, nonlinear$x2, col = nonlinear$y)

#A linear kernel will fail in this scenario. Let's try using a radial kernel.
svm.radial = svm(y ~ .,
                 data = nonlinear,
                 kernel = "radial",   # radial!!!!
                 cost = 1,
                 gamma = .5) #Default is 1/p.  it is th eboundary of the radial kernel or something or another
# larger gamma means effect of further observations will be lower (looking locally when creating boundary)
# larger gamma is for local observations # ie. the boundary is more exact
# smaller gamma is for global observations # ie. the boundary is more smooth (circular even)

#Visualizing the results of the support vector machine
plot(svm.radial, nonlinear)
summary(svm.radial)
svm.radial$index

#What happens if we make gamma small?
svm.radial.smallgamma = svm(y ~ .,
                            data = nonlinear,
                            kernel = "radial",
                            cost = 1,
                            gamma = .05)
plot(svm.radial.smallgamma, nonlinear)
summary(svm.radial.smallgamma)
svm.radial.smallgamma$index

#What happens if we make gamma large?
svm.radial.largegamma = svm(y ~ .,
                            data = nonlinear,
                            kernel = "radial",
                            cost = 1,
                            gamma = 10)
plot(svm.radial.largegamma, nonlinear)
summary(svm.radial.largegamma)
svm.radial.largegamma$index

#Let's use cross-validation to figure out the best combination of the tuning
#parameters.

#Creating training and test sets.
set.seed(0)
train.index = sample(1:300, 300*.8)
test.index = -train.index

#Performing the cross-validation.
#CAUTION: Will take about 30 seconds.
set.seed(0)
cv.svm.radial = tune(svm,
                     y ~ .,
                     data = nonlinear[train.index, ],
                     kernel = "radial",
                     ranges = list(cost = 10^(seq(-1, 1.5, length = 20)),
                                   gamma = 10^(seq(-2, 1, length = 20))))

# cost length of 20 and gamma length of 20 makes 20*20 = 400 
# combinations of models to test altogether
# called a group search

#Inspecting the cross-validation output.
summary(cv.svm.radial)
names(cv.svm.radial)
cv.svm.radial$best.parameters


#Plotting the cross-validation results.
library(rgl)
plot3d(cv.svm.radial$performances$cost,
       cv.svm.radial$performances$gamma,
       cv.svm.radial$performances$error,
       xlab = "Cost",
       ylab = "Gamma",
       zlab = "Error",
       type = "s",
       size = 1)

#Inspecting the best model.
best.nonlinear.model = cv.svm.radial$best.model
summary(best.nonlinear.model)

#Using the best model to predict the test data.
ypred = predict(best.nonlinear.model, nonlinear[test.index, ])
table("Predicted Values" = ypred, "True Values" = nonlinear[test.index, "y"])
(53)/(44 + 1 +  6 + 9) # 88% accuracy


#Constructing and visualizing the final model.
svm.best.nonlinear = svm(y ~ .,
                         data = nonlinear,
                         kernel = "radial",
                         cost = best.nonlinear.model$cost,
                         gamma = best.nonlinear.model$gamma)
plot(svm.best.nonlinear, nonlinear)
summary(svm.best.nonlinear)
svm.best.nonlinear$index
ypred = predict(svm.best.nonlinear, nonlinear)
table("Predicted Values" = ypred, "True Values" = nonlinear[, "y"])
(190 + 84)/(190+84+10+16) # 91% accuracy









########################################
#####Multi-Class SVM Classification#####
########################################
#Creating multi-class data.
set.seed(0)
x1 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100), rnorm(100, 2))
x2 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100), rnorm(100, -2))
y = as.factor(c(rep(-1, 200), rep(1, 100), rep(2, 100)))   #the last repeat group is for the two groups of black
multi = data.frame(x1, x2, y)
plot(multi$x1, multi$x2, col = multi$y) #color by DF groupings

#Creating training and test sets.
set.seed(0)
train.index = sample(1:400, 400*.8) # select 80% of the 400 observations we have
test.index = -train.index

#Performing the cross-validation.
#CAUTION: Will take about 45 seconds.
set.seed(0)
cv.multi = tune(svm,
                y ~ .,
                data = multi[train.index, ],
                kernel = "radial",
                ranges = list(cost = 10^(seq(-1, 1.5, length = 20)),
                              gamma = 10^(seq(-2, 1, length = 20))))

#Inspecting the cross-validation output.
summary(cv.multi)

#Plotting the cross-validation results.
plot3d(cv.multi$performances$cost,
       cv.multi$performances$gamma,
       cv.multi$performances$error,
       xlab = "Cost",
       ylab = "Gamma",
       zlab = "Error",
       type = "s",
       size = 1)

#Inspecting the best model.
best.multi.model = cv.multi$best.model
summary(best.multi.model)

#Using the best model to predict the test data.
ypred = predict(best.multi.model, multi[test.index, ])
table("Predicted Values" = ypred, "True Values" = multi[test.index, "y"])
(37+16+17)/(37+16+17+5+5) # 88% accuracy

#Constructing and visualizing the final model.
svm.best.multi = svm(y ~ .,
                     data = multi,
                     kernel = "radial",
                     cost = best.multi.model$cost,
                     gamma = best.multi.model$gamma)
plot(svm.best.multi, multi)
summary(svm.best.multi)   # same as summary above??? why is that????
svm.best.multi$index
ypred = predict(svm.best.multi, multi)
table("Predicted Values" = ypred, "True Values" = multi[, "y"])
(176+81+91)/(348 + 24 + 19 + 7)  # 87% accuracy

