###################################################
###################################################
#####[10] Support Vector Machines Lecture Code WITH CARET!!!!!  #####
###################################################
###################################################



###################################
#####Maximal Margin Classifier#####
###################################
#Generating linearly separable data.
set.seed(0)
x1 = c(rnorm(100, 0, 4), rnorm(100, 1, 3))
x2 = c(rnorm(100, 0, 1), rnorm(100, 6, 1))
y = as.factor(c(rep(-1, 100), rep(1, 100)))
linearly.separable = data.frame(x1, x2, y)

#Plotting the linearly separable data.
library(ggplot2)
ggplot(linearly.separable) + 
  geom_point(aes(x2, x1, color = y))

#Creating training and test sets.
set.seed(0)
train.index = sample(1:200, 200*.8)
test.index = -train.index

#Importing the e1071 library in order to use the svm() function to fit support
#vector machines.
library(kernlab)

#Fitting a maximal margin classifier to the training data.
svm.mmc.linear <- ksvm(y~., data= linearly.separable[train.index, ],
           kernel="vanilladot", C = 1e6)


#Visualizing the results of the maximal margin classifier.
plot(svm.mmc.linear, data =  linearly.separable[train.index, ])

#Additional information for the fit.
svm.mmc.linear

#Finding the indices of the support vectors.
alphaindex(svm.mmc.linear)

#Predicting on the test data.
ypred = predict(svm.mmc.linear, linearly.separable[test.index, ])
table("Predicted Values" = ypred, "True Values" = linearly.separable[test.index, "y"])


#Adding a single point to display the sensitivity of the maximal margin classifier.
linearly.separable2 = rbind(linearly.separable, c(-5, 3, 1))
ggplot(linearly.separable2) + geom_point(aes(x2, x1, color = y))

#Fitting a maximal margin classifier to the new data.
svm.mmc.linear2 = ksvm(y~., data= linearly.separable2,
                       kernel="vanilladot", C = 1e6)

#Visualizing the results of the maximal margin classifier; comparing the output.
plot(svm.mmc.linear, data = linearly.separable[train.index, ]) #Old model.
plot(svm.mmc.linear2, data = linearly.separable2) #New model.

#Additional information for the fit.
svm.mmc.linear2

#Finding the indices of the support vectors.
alphaindex(svm.mmc.linear2)



###################################
#####Support Vector Classifier#####
###################################
#Fitting a support vector classifier by reducing the cost of a misclassified
#observation.
svm.svc.linear2 = ksvm(y~., data= linearly.separable2,
                       kernel="vanilladot", C = 1)

#Visualizing the results of the support vector classifier.
plot(svm.svc.linear2, data= linearly.separable2)
svm.svc.linear2
alphaindex(svm.svc.linear2)

#What happens if we reduce the cost even more?
svm.svc.linear3 = ksvm(y~., data= linearly.separable2,
                       kernel="vanilladot", C = .1)


plot(svm.svc.linear3, data = linearly.separable2)
svm.svc.linear3
alphaindex(svm.svc.linear3)

#We generally find the best cost parameter by implementing the cross-validation
#procedure; this isn't as interesting with linearly separable data. Let's generate
#some data that is not linearly separable.
set.seed(0)
x1 = c(rnorm(100, -1, 1), rnorm(100, 1, 1))
x2 = c(rnorm(100, -1, 1), rnorm(100, 1, 1))
y = as.factor(c(rep(-1, 100), rep(1, 100)))
overlapping = data.frame(x1, x2, y)
ggplot(overlapping) + geom_point(aes(x2, x1, color=y))


#Implement cross-validation to select the best parameter value of the cost.
set.seed(0)
library(caret)
train.control <- trainControl(method='cv',
                              number=10)
tune.grid = expand.grid(C = 10^(seq(-5, .5, length = 100)))
cv.svm.overlapping = train(y~., data = overlapping[train.index, ],
           method = 'svmLinear',
           trControl = train.control,
           tuneGrid = tune.grid)


#Inspecting the cross-validation output.
cv.svm.overlapping

#Plotting the cross-validation results.
plot(cv.svm.overlapping)

#Inspecting the best model.
cv.svm.overlapping$finalModel

#Using the best model to predict the test data.
ypred = predict.train(cv.svm.overlapping, overlapping[test.index, ])
table("Predicted Values" = ypred, "True Values" = overlapping[test.index, "y"])

#Constructing and visualizing the final model.
svm.best.overlapping = ksvm(y ~ .,
                           data = overlapping,
                           kernel = "vanilladot",
                           C = cv.svm.overlapping$bestTune)
plot(svm.best.overlapping, data = overlapping)
svm.best.overlapping
alphaindex(svm.best.overlapping)
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
ggplot(nonlinear) + geom_point(aes(x2, x1, color=y))


#A linear kernel will fail in this scenario. Let's try using a radial kernel.
svm.radial = ksvm(y ~ .,
                 data = nonlinear,
                 kernel = "rbfdot",
                 C = 1,
                 sigma = .5) #Default is 1/p.

#Visualizing the results of the support vector machine
plot(svm.radial, data = nonlinear)
svm.radial
alphaindex(svm.radial)

#What happens if we make gamma small?
svm.radial.smallgamma = ksvm(y ~ .,
                            data = nonlinear,
                            kernel = "rbfdot",
                            C = 1,
                            sigma = .05)
plot(svm.radial.smallgamma, data=nonlinear)
summary(svm.radial.smallgamma)
svm.radial.smallgamma$index

#What happens if we make gamma large?
svm.radial.largegamma = ksvm(y ~ .,
                            data = nonlinear,
                            kernel = "rbfdot",
                            C = 1,
                            sigma = 10)
plot(svm.radial.largegamma, data = nonlinear)
svm.radial.largegamma
alphaindex(svm.radial.largegamma)

#Let's use cross-validation to figure out the best combination of the tuning
#parameters.

#Creating training and test sets.
set.seed(0)
train.index = sample(1:300, 300*.8)
test.index = -train.index

#Performing the cross-validation.
#CAUTION: Will take about 3 minutes.
set.seed(0)
train.control <- trainControl(method='cv',
                              number=10)
tune.grid = expand.grid(C = 10^(seq(-5, .5, length = 100)),
                        sigma = 10^(seq(-2, 1, length = 20))
                        )
system.time({
cv.svm.radial = train(y~., data = nonlinear[train.index, ],
                           method = 'svmRadial',
                           trControl = train.control,
                           tuneGrid = tune.grid)
})


#Inspecting the cross-validation output.
cv.svm.radial

#Plotting the cross-validation results.
library(rgl)
plot3d(cv.svm.radial$results$C,
       cv.svm.radial$results$sigma,
       1-cv.svm.radial$results$Accuracy,
       xlab = "C",
       ylab = "Sigma",
       zlab = "Error",
       type = "s",
       size = 1)

#Inspecting the best model.
best.nonlinear.model = cv.svm.radial$finalModel
best.nonlinear.model

#Using the best model to predict the test data.
ypred = predict.train(cv.svm.radial, nonlinear[test.index, ])
table("Predicted Values" = ypred, "True Values" = nonlinear[test.index, "y"])

#Constructing and visualizing the final model.
svm.best.nonlinear = ksvm(y ~ .,
                         data = nonlinear,
                         kernel = "rbfdot",
                         C = cv.svm.radial$bestTune$C,
                         sigma = cv.svm.radial$bestTune$sigma)
plot(svm.best.nonlinear, data = nonlinear)
svm.best.nonlinear
alphaindex(svm.best.nonlinear)



########################################
#####Multi-Class SVM Classification#####
########################################
#Creating multi-class data.
set.seed(0)
x1 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100), rnorm(100, 2))
x2 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100), rnorm(100, -2))
y = as.factor(c(rep(-1, 200), rep(1, 100), rep(2, 100)))
multi = data.frame(x1, x2, y)
ggplot(multi) + geom_point(aes(x2, x1, color=y))

#Creating training and test sets.
set.seed(0)
train.index = sample(1:400, 400*.8)
test.index = -train.index

#Performing the cross-validation.
set.seed(0)
train.control <- trainControl(method='cv',
                              number=3)
tune.grid = expand.grid(C = 10^(seq(-1, 1.5, length = 20)),
                        sigma = 10^(seq(-2, 1, length = 20)))

system.time({
  cv.multi = train(y~., data = multi[train.index, ],
                        method = 'svmRadial',
                        trControl = train.control,
                        tuneGrid = tune.grid)
})




#Inspecting the cross-validation output.
cv.multi

#Plotting the cross-validation results.
plot3d(cv.multi$results$C,
       cv.multi$results$sigma,
       1- cv.multi$results$Accuracy,
       xlab = "Cost",
       ylab = "Gamma",
       zlab = "Error",
       type = "s",
       size = 1)

#Inspecting the best model.
best.multi.model = cv.multi$finalModel
best.multi.model

#Using the best model to predict the test data.
ypred = predict(cv.multi, multi[test.index, ])
table("Predicted Values" = ypred, "True Values" = multi[test.index, "y"])
