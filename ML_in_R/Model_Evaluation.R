set.seed(0)
x = seq(-2, 5, length=100)
noise = rnorm(100)
y = 3 + 2*x^2 + 4*noise


model1 = lm(y~x); pred1 = predict(model1)
plot(x, y); model1 = lm(y~x); pred1 = predict(model1)
abline(model1)

model2 = lm(y~x+I(x^2)); pred2 = predict(model2)

y - pred1

# Aggregating the Errors:
# MAE
# MSE
# RMSE
# R^2

mae1 = mean(abs(y-pred1))
mae1
mae2 = mean(abs(y-pred2))
mae2

mse1 = mean((y-pred1)^2)
mse1
mse2 = mean((y-pred2)^2)
mse2

rmse1 = sqrt(mean((y-pred1)^2))
rmse1
rmse2 = sqrt(mean((y-pred2)^2))
rmse2

r2_1 = 1 - mean((y-pred1)^2)/mean((y-mean(y))^2)
r2_1
r2_2 = 1 - mean((y-pred2)^2)/mean((y-mean(y))^2)
r2_2

print(r2_1); print(cor(y,pred1)^2) # they are equal, confirmed
print(r2_2); print(cor(y,pred2)^2)


# Performance of classification
# Confusion Matrix:
    # accuracy: proportion of correct
    # sensitivity, specificity, kappa
# ROC and AUC

dat = read.csv('data/school.csv',
               colClasses = c('factor','factor','numeric','factor'))
model = glm(y ~ . , dat, family='binomial') # binomial means logistic regression
pred.prob = predict(model, type='response')
pred.class = ifelse(pred.prob > 0.5, 1, 0) # set the threshold (doesnt have to be 0.5)

# confusion matrix
cmat = table(dat$y, pred.class)
row.names(cmat) = c('actual_0','actual_1')
cmat
# type 1 error: false positive
# type 2 error: false negative

#accuracy (true cases out of all cases)
accuracy = (cmat[1,1] + cmat[2,2]) / sum(cmat)
print(accuracy)

error.rate = 1 - accuracy
print(error.rate)

#sensitivity/recall (how often the test classifies positives as positive)
sensitivity = cmat[1,1] / (cmat[1,1] + cmat[2,1])
sensitivity

#specificity (how often the test classifies negative as negative)
specificity = cmat[2,2] / (cmat[2,2] + cmat[1,2])
specificity

#precision (how many of the positive results are actually positive)
precision = cmat[2,2] / (cmat[2,2] + cmat[2,1])
precision

# when youre more careful with what you categorize as "positive",
# your precision increase but sensitivity decreases

#fscore (tradeoff between precision and recall/sensitivity)
fscore = (2 * precision*sensitivity)/(sensitivity + precision)
fscore

#Kappa stats
pmat = cmat / sum(cmat) #confusion matrix is cmat
p = addmargins(pmat)
p

# assuming independence so that probabilities work
ep   # doesnt workkkkkk!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ep[1,1] = ep[1,3] * ep[3,1]
ep[1,2] = ep[1,3] * ep[3,2]
ep[2,1] = ep[2,3] * ep[3,1]
ep[2,2] = ep[2,3] * ep[3,2]
ep


pr.actual = p[1,1] + p[2,2]
pr.actual
pr.expected = ep[1,1] + ep[2,2]
pr.expected

# Poor agreement ?????? Less than 0.20
# Fair agreement ?????? 0.20 to 0.40
# Moderate agreement ?????? 0.40 to 0.60
# Good agreement ?????? 0.60 to 0.80
# Very good agreement ?????? 0.80 to 1.00



# or just use caret
library(caret)
confusionMatrix(dat$y, pred.class, positive='1')
# low p-values mean....what???

#prediction probability prob and the actual results
preObs = data.frame(prob=pred.prob, obs=dat$y)
#sort descending according to the predicted probability 
preObs = preObs[order(-preObs$prob), ]
head(preObs)

n = nrow(preObs)
tpr = fpr = rep(0,n)
#calculate TPR and FPR according to different thresholds; draw ROC curve
for (i in 1:n) {
  threshold = preObs$prob[i]
  tp = sum(preObs$prob > threshold & preObs$obs == 1)
  fp = sum(preObs$prob > threshold & preObs$obs == 0)
  tn = sum(preObs$prob < threshold & preObs$obs == 0)
  fn = sum(preObs$prob < threshold & preObs$obs == 1)
  tpr[i] = tp / (tp + fn) # true positive rate (sensitivity)
  fpr[i] = fp / (tn + fp) # false positive rate (1-specificity)
}

library(pROC)
modelroc = roc(preObs$obs, preObs$prob)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2), grid.col=c("green", "red"), 
     max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)
auc(modelroc)
ci(modelroc)


# Evaluating Models
#underfitting
plot(x, y)
model1 = lm(y ~ x)
abline(model1)
# overfitting
model2 = lm(y ~ poly(x,20))
plot(x, y)
lines(x, model2$fitted.values)

# learning curve... performance vs. complexity
#... split the data into two portions
dat = data.frame(x,y)
set.seed(1)
index = sample(1:100, 50)
train = dat[index,]
test = dat[-index,]
#intordocue complexity by increasing degree of polynomial
# compute the rmse of reg. models with different order in polynomials
rmse_train = function(n) {
  model = lm(y ~ poly(x,n), data=train)
  pred = predict(model)
  rmse = sqrt(mean((train$y-pred)^2))
  return(rmse)
}
rmse1 = sapply(1:10, rmse_train)
# plot performance versus complexity (i.e. the learning curve)
plot(1:10, rmse1, type='b')

# do to test
rmse_test = function(n) {
  model = lm(y ~ poly(x, n), data=train)
  pred = predict(model, newdata=test)
  rmse = sqrt(mean((test$y-pred)^2))
  return(rmse)
}
rmse2 = sapply(1:10, rmse_test)

plot(1:10, rmse2, type='b')

plotdata = data.frame(rmse=c(rmse1,rmse2), 
                      type=rep(c('train','test'), each=10),
                      x=rep(1:10,times=2))
p = ggplot(plotdata, aes(x=x,y=rmse,group=type,color=type))
p = p + geom_point() + geom_line()
#overfitting detected as the curves deviate
print(p)

# holdout method
library(randomForest)
credit = read.csv("./data/credit.csv")
set.seed(0)
index = sample(1:nrow(credit), size= nrow(credit)*0.7)
train = credit[index, ]    # randomize it so distribution in training is similar to distribution in test
test = credit[-index, ]
rf = randomForest(data = train, default~.)
mean(predict(rf, test) == test$default)

#there is a potential issue with randomizing
# class imbalance
y = c(rep('a', 9990), rep('b', 10))
set.seed(2)
index = sample(1:10000, size= 7000)
train = y[index]
test = y[-index]
print(mean(train=='b')); print(mean(test=='b'))
# a lot more 'b's in training set than in test set
# sidenote: performance is often underestimated

# CV: the createFolds function
folds = createFolds(credit$default, 5)
str(folds)

folds2 = createFolds(y, 3)
str(folds2)
print(mean(folds2$Fold1=='b'))
print(mean(folds2$Fold2=='b'))
print(mean(folds2$Fold3=='b'))

# it didnt work....WTF is wrong

n=5
accuracy = numeric(n)
for(i in 1:n){
  train = credit[folds[[i]], ]
  test = credit[-folds[[i]], ]
  rf = randomForest(data = train, default~.)
  accuracy[i] = mean(predict(rf, test)== test$default)
}
accuracy
print(mean(accuracy)); print(sd(accuracy))

# caret's cross validation
ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5)
m_rf = train(default ~ ., data=credit, method = "rf",
             metric = "Kappa", trControl = ctrl)
m_rf$results


# hyperparameter cross validation or tuning parameters
set.seed(0)
index = sample(1:nrow(credit), size= nrow(credit)*0.8)
train = credit[index, ]
test = credit[-index, ]
grid_rf = expand.grid(mtry = c(2, 6, 12, 18, 25, 35))
ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5)
m_rf = train(default ~ ., data=train, method = "rf",
             metric = "Kappa", trControl = ctrl,
             tuneGrid = grid_rf)
m_rf$results

# Final evaluation
m_rf$bestTune
predict.train(m_rf, test)

# evaluate the performance of this model with the test dataset
mean(predict.train(m_rf, test)==test$default)

