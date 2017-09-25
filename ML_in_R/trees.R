#############
# Q1: Trees #
#############
library(ISLR)
library(tree)
library(randomForest)
library(gbm) # wont load

head(OJ)
summary(OJ)
attach(OJ)

# 1 : Split data 80-20
set.seed(0)
train = sample(1:nrow(OJ), 8*nrow(OJ)/10)
OJ.train = OJ[train, ]
OJ.test = OJ[-train,]
purchase.test = Purchase[-train]


# 2 : Decision tree Purchase ~ . using Gini for splits
set.seed(0)
tree.OJ = tree(Purchase ~., split = 'gini', data = OJ.train)    #make sure its training data!!!
plot(tree.OJ)
text(tree.OJ, pretty = 0)
summary(tree.OJ)

 
# 3 : # of terminal nodes? accuracy?
# 86 terminal nodes with a misclassifcation rate of 0.1393


# 4 : Predict purchase with test set. Accuracy?
set.seed(0)
tree.predict = predict(tree.OJ, OJ.test, type = 'class')
table(tree.predict, OJ.test$Purchase)
(103 + 60)/(103 + 60 + 28 + 23)    
# accuracy of 76%
####################################################
##############
#######


# 5 : CV and cross-complexity pruning
set.seed(0)
cv.OJ = cv.tree(tree.OJ, FUN = prune.misclass)
names(cv.OJ)
cv.OJ


# 6 : Visualize pruning for different # nodes and alphas
par(mfrow = c(1,2))
plot(cv.OJ$size, cv.OJ$dev, type = 'b',
     xlab = 'Terminal Nodes', ylab = 'Misclassified Observations')
plot(cv.OJ$k, cv.OJ$dev, type = 'b',
     xlab= 'Alpha', ylab = 'Misclassified Observations')


# 7 : Prune tree
best.nodes = cv.OJ$size[which(cv.OJ$dev == min(cv.OJ$dev))]
prune.OJ = prune.misclass(tree.OJ, best = best.nodes)
par(mfrow = c(1,1))



# 8 : # terminal nodes? accuracy?
summary(prune.OJ)
# six terminal nodes with a misclassification error of 0.257


# 9 : Visualize pruned tree
plot(prune.OJ)
text(prune.OJ, pretty = 0)


# 10 : Predict purchase with test set. Accuracy?
tree.pred2 = predict(prune.OJ, OJ.test, type = 'class')
table(tree.pred2, OJ.test$Purchase)
(113 + 57)/(113+57+26+18)
# 79% accuracy


# 11 : Why are test predictions more accurate for pruned tree than intial?
#The initial tree is likely overfitting to our data, even though it stops splitting
#before placing each observation into its own terminal node. The cost-complexity
#pruning process provides a balance that will ultimately produce a model that
#penalizes complexity to a certain degree in order to stray from the potential
#of overfitting.




##################################
# Q2: Bagging and Random Forests #
##################################


# 1 : Create random forest Purchase ~ . (should make 500 trees)
set.seed(0)
rf.OJ = randomForest(Purchase ~ ., data = OJ, subset = train, importance = TRUE)
rf.OJ


# 2 : Accuracy on training and test sets
# 80% for training (1 - oob.err)
rf.pred = predict(rf.OJ, OJ.test, type='class')
table(rf.pred, OJ.test$Purchase)
(113+60)/nrow(OJ.test)
# 80% for test


# 3 : Which variable best for classifying OJ purchase?
importance(rf.OJ) # LoyalCH   (loyalty to Citrus Hill)
varImpPlot(rf.OJ) # LoyalCH


# 4 : Vary number of variables for each node and record oob.err
set.seed(0)
oob.err = numeric(17)

for (mtry in 1:17) {
  fit = randomForest(Purchase ~ ., data = OJ[train, ], mtry = mtry)
  oob.err[mtry] = fit$err.rate[500,1]  #training 500 trees, default is 500
  cat("We're performing iteration", mtry, "\n")
}
# mtry: Number of variables randomly sampled as candidates at each split. 
# why do we always record err.rate[500,1]?
# Since by default, ntree=500 



# 5 : Visualize oob.err
plot(1:17, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")


# 6 : Max accuracy in ranom forest? # variables used at each split?
1 - min(oob.err)  # 81% 
which(oob.err == min(oob.err)) #with 2 variables (from graph too)


# 7 : Accuracy of bagged model on training set? # variables at each split?
fit = randomForest(Purchase ~ ., data = OJ[train, ], mtry = 2)
pred = predict(fit, OJ.test)
table(pred, OJ.test$Purchase)
(114 + 58)/(114 + 17 + 25 + 58) # 80%, 2 variables
1 - oob.err[17]


# 8 : Accuracy of best random forest on test set?
set.seed(0)
fit2 = randomForest(Purchase ~ ., data = OJ.train, mtry = 2)
pred2 = predict(fit2, OJ.test, type = 'class')
table(pred2, OJ.test$Purchase)
(117 + 58)/(117+58 +14+25) # 82% with 2 variables


# 9 : Accuracy of bagged model on test set?
set.seed(0)
rf.bagged = randomForest(Purchase ~ ., data = OJ.train, mtry = 17)
table(predict(rf.bagged, OJ.test, type = 'class'), OJ.test$Purchase)
(110+62)/(172+42)   #80.37%



################
# Q3: Boosting #
################


# 1 : Provided data munging

OJ.train.indicator = OJ.train
OJ.test.indicator = OJ.test
OJ.train.indicator$Purchase = as.vector(OJ.train$Purchase, 
                                        mode = "numeric") - 1
OJ.test.indicator$Purchase = as.vector(OJ.test$Purchase, 
                                       mode = "numeric") - 1


# 2 : Initial boosted model (bernoulli, 10,000 trees, interaction 4, shrinkage 0.001)
set.seed(0)

boost.OJ = gbm(Purchase ~ ., data = OJ.train.indicator,
               distribution = "bernoulli",
               n.trees = 10000,
               interaction.depth = 4,
               shrinkage = 0.001)


# 3 : Predict test set
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.OJ, 
                  newdata = OJ.test.indicator, 
                  n.trees = n.trees,
                  type="response")
predmat = round(predmat)
##################### why use round????




# 4 : Accuracies? Minimum # trees nec. to reach max accuracy?
accuracy.boost = numeric(100)
for (i in 1:100) {
  accuracy.boost[i] = 
    sum(diag(table(OJ.test.indicator$Purchase, predmat[, i]))) / 214  
}
min(which(accuracy.boost == max(accuracy.boost)) * 100)

acc=rep(0,100)
for (i in 1:100){
  acc[i]<-length(which(predmat[,i]==OJ.test.indicator$Purchase))/length(OJ.test.indicator$Purchase)
}
max(acc)
#[1] 0.8364485981
which.max(acc)
# 21


# 5 : Plot accuracies against # trees & add three horizontal lines

plot(n.trees, 
     accuracy.boost,
     pch = 16,
     type = 'b',
     ylab = "Accuracy",
     xlab = "# Trees",
     main = "Boosted Accuracy")



abline(h = max(accuracy.boost), lty = 2) #Boosting
abline(h = (1 - min(oob.err)), col = "red3", lty = 2) #Random forests.
abline(h = (113 + 57)/nrow(OJ.test), col = "blue", lty = 2) #Pruned tree.
legend("bottomright",
       c("Boosting", "Random Forests", "Pruned Tree"),
       lwd = 2,
       lty = 2,
       col = c("black", "red3", "blue"))

