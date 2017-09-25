# load libraries

library(PASWR)
library(VIM)
library(mice)
library(caret)
library(Hmisc)
library(RANN)
library(deldir)
library(kknn)
library(dplyr)
library(ggplot2)

######################################################
# Q1: Missingness and Imputation for Titanic Dataset #
######################################################

data("titanic3")
head(titanic3)
str(titanic3)
summary(titanic3)
?titanic3

# 1: Variables with at least 1 missing value
sapply(titanic3, function(x) sum(is.na(x)))
# age     fare      body 
# 263     1        1188 
round(sapply(titanic3, function(x) sum(is.na(x)))/dim(titanic3)[1]*100, 1)
# age       fare     body 
# 20.1      0.1      90.8


# 2: observations with at least 1 missing value
sum(!complete.cases(titanic3))   #1190 incomplete cases
sum(!complete.cases(titanic3))/nrow(titanic3)   #91% incomplete
sum(complete.cases(titanic3))   #119 complete cases


# 3: cells with missing values
sum(is.na(titanic3))  # 1452
sum(is.na(titanic3))/prod(dim(titanic3)) * 100     # 7.9% missing


# 4: different combinations of missingness in the dataset
md.pattern(titanic3)
aggr(titanic3)
# body, age-body, fare, age


# 5: what kind of missingness for each variable? Reason why
titanic3[is.na(titanic3$fare), ]   # just 1
titanic3[is.na(titanic3$age), ] 
titanic3 %>% 
  group_by(pclass) %>% 
  summarise(sum(is.na(age)))
titanic3 %>% 
  group_by(survived) %>% 
  summarise(sum(is.na(age)))
  
# for "body", it is Missing Not at Random (MNAR) b/c if they survived, 
# they would not have a body count. It could also be MAR 
# depending on if the body was not recovered if it sunk
# for "fare", I am unsure, but it seems MCAR because 
# it is only one value and I don't see any pattern
# for "age", it seems MAR because there seems to be not pattern to me,
# but it could be MNAR because maybe people of a certain age 
# do not wish to tell their age or they died or they were of a lower class


# 6: mean value imputation age & graph
# cor(titanic3$age, titanic3$survived, use = "complete.obs") 
titanic = titanic3
titanic$age[is.na(titanic$age)] = mean(titanic$age, na.rm =TRUE)
ggplot(titanic3, aes(x = age)) + geom_density()
ggplot(titanic, aes(x = age)) + geom_density()
# there is a huge spike in people aged 29 (just past the median) 
# this can skew insights from our data, especially any correlations based on age
pre = preProcess(titanic3, 
                 method = c("scale", "medianImpute"))   # wait, what does scale do
titanic2 = predict(pre, titanic3)
ggplot(titanic2, aes(x = age)) + geom_density()

# 7: simple random value imputation age & graph
set.seed(0)
imputed_age = Hmisc::impute(titanic3$age, "random")
imputed_age
titanic3$imputed_age = imputed_age
ggplot(titanic3, aes(x = imputed_age)) + geom_density()
# it looks closer to actual distributionbut also it could not be 
# an acurate representation based on missingness type



####################################################
# Q2: K-Nearest Neighbors with the Titanic Dataset #
####################################################
data("titanic3")

# 1: simple random value imputation fare
set.seed(0)
imputed_fare = Hmisc::impute(titanic3$fare, "random")
imputed_fare
titanic3$imputed_fare = imputed_fare
titanic3$imputed_fare[is.na(titanic3$fare)]
# seed(0) gives 69.55

# 2: plot simp rand fare vs. simp rand age
ggplot(titanic3, aes(x = imputed_fare, y = imputed_age)) + geom_point(aes(color = pclass))
# other than higher classes pay more, I dont see a trend


# 3: add two points to plot
new_points  <- data.frame(imputed_age = c(50, 10), 
                          imputed_fare = c(400,100), 
                          pclass = c(NA, NA))
ggplot(titanic3, aes(x = imputed_fare, y = imputed_age)) + 
  geom_point(aes(color = pclass)) + 
  geom_point(data = new_points, color = "purple", size  = 2)


# 4: what classes do points belong to?
# both belong to first class since they pass a certain threshold of fare

# 5: KNN k = 1 impute class
titanic_selected = select(titanic3, imputed_age, imputed_fare, pclass)
k = rbind(titanic_selected, new_points)
tail(k)
test1nn = kNN(k, k = 1)     # performs test
table(k$pclass, test1nn$pclass)    # shows table 
tail(test1nn)    # shows imputed classes (both 1st)

# 6: KNN k = sqrt(n) impute class ... why change/no change?
new_points  <- data.frame(imputed_age = c(50, 10), 
                          imputed_fare = c(400,100), 
                          pclass = c(NA, NA))
titanic_selected = select(titanic3, imputed_age, imputed_fare, pclass)
k = rbind(titanic_selected, new_points)
root_n = as.integer(sqrt(nrow(k)))
test_sqrt_nn = kNN(k, k = root_n)
tail(test_sqrt_nn)
# the $100 fare changed to 3rd class, because there arent as
# many first class people or no as many young ones
table(k$pclass, test_sqrt_nn$pclass)






####################################################
# Q3: Minkowski Distances with the Titanic Dataset #
####################################################

# 1: create new DF from titanic
data("titanic3")
titanic_new = titanic3 %>% 
  select(pclass, survived, sex, age,
         sibsp, parch)
titanic_new$fare = Hmisc::impute(titanic3$fare, "random")
head(titanic_new)



# 2: separate DF into two DFs (all unique observations)
titanic_complete = titanic_new[complete.cases(titanic_new),]
titanic_incomplete = titanic_new[!complete.cases(titanic_new),] %>% 
  select(-age)



# 3: KNN k = 1 w/Manhattan (p = 1), Euclidean (p = 2), and Minkowski (p > 2)
titanic_manhattan = kknn(age ~ ., titanic_complete, titanic_incomplete, k = 1, distance = 1)
manhattan_age = titanic_manhattan$fitted.values
manhattan_age

titanic_euclidean = kknn(age ~ ., titanic_complete, titanic_incomplete, k = 1, distance = 2)
euclidean_age = titanic_euclidean$fitted.values
euclidean_age

titanic_minkowski = kknn(age ~ ., titanic_complete, titanic_incomplete, k = 1, distance = 10)
minkowski_age = titanic_minkowski$fitted.values
minkowski_age


# 4: graph 4 density curves
ggplot() + 
  geom_density(aes(x = titanic_complete$age, 
                   color = "complete set")) +
  geom_density(aes(x = manhattan_age,
                   color = "manhattan")) +
  geom_density(aes(x = euclidean_age,
                   color = "euclidean")) +
  geom_density(aes(x = minkowski_age,
                   color = "minkowski"))

# the imputed values show many more passengers in their 60s and 20s
# than the complete set. This is probably because the complete set had 
# peaks in these spots as well, so it was exaggerated with the imputation.


# 5: KNN k = sqrt(n) w/Manhattan, Euclidean, and Minkowski
titanic_manhattan = kknn(age ~ ., titanic_complete, titanic_incomplete, k = root_n, distance = 1)
manhattan_age = titanic_manhattan$fitted.values
manhattan_age

titanic_euclidean = kknn(age ~ ., titanic_complete, titanic_incomplete, k = root_n, distance = 2)
euclidean_age = titanic_euclidean$fitted.values
euclidean_age

titanic_minkowski = kknn(age ~ ., titanic_complete, titanic_incomplete, k = root_n, distance = 10)
minkowski_age = titanic_minkowski$fitted.values
minkowski_age


# 6: graph 4 density curves
ggplot() + 
  geom_density(aes(x = titanic_complete$age, 
                   color = "complete set")) +
  geom_density(aes(x = manhattan_age,
                   color = "manhattan")) +
  geom_density(aes(x = euclidean_age,
                   color = "euclidean")) +
  geom_density(aes(x = minkowski_age,
                   color = "minkowski"))

# the spike in the late 20s becomes even more profound b/c
# when you take even more neighbors, and the majority is 
# already in that age range, the imputations will just mimic that
