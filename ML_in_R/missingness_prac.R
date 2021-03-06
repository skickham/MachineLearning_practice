##########################################################
##########################################################
#####[02] Missingness, Imputation, & KNN Lecture Code#####
##########################################################
##########################################################



##################################
#####Visualizing Missing Data#####
##################################
library(VIM) #For the visualization and imputation of missing values.

help(sleep) #Inspecting the mammal sleep dataset.
sleep

summary(sleep) #Summary information for the sleep dataset.
sapply(sleep, function(x) sd(x, na.rm = T)) #Standard deviations for the sleep dataset; any issues?

aggr(sleep) #A graphical interpretation of the missing values and their
            #combinations within the dataset.

library(mice) #Load the multivariate imputation by chained equations library.
md.pattern(sleep) #Can also view this information from a data perspective.
# the number in front tells you how many observations match this missingness
# under "Dream" there are three 0s which indicate missing
# first 0 had 9 observations (name of row), second had 2, and last had 1 --> that gives 12 altogether
# 38 missing values total 



###############################
#####Mean Value Imputation#####
###############################
#Creating a dataset that has missing values.
missing.data = data.frame(x1 = 1:20, x2 = c(1:10, rep(NA, 10)))
missing.data

mean(missing.data$x2, na.rm = TRUE) #Mean of x2 prior to imputation.
sd(missing.data$x2, na.rm = TRUE) #Standard deviation of x2 prior to imputation.
cor(missing.data, use = "complete.obs") #Correlation prior to imputation.

#Mean value imputation method 1.
missing.data$x2[is.na(missing.data$x2)] = mean(missing.data$x2, na.rm=TRUE)
missing.data
sd(missing.data$x2, na.rm = TRUE) #Standard deviation of x2 prior to imputation.
cor(missing.data, use = "complete.obs") #Correlation prior to imputation.


#Mean value imputation method 2.
missing.data = data.frame(x1 = 1:20, x2 = c(1:10, rep(NA, 10))) #Recreating dataset.
missing.data = transform(missing.data,               # same method, just with transform func from dplyr
                         x2 = ifelse(is.na(x2),
                                     mean(x2, na.rm=TRUE),
                                     x2))
missing.data

#Mean value imputation method 3.
library(caret)

missing.data = data.frame(x1 = 1:20, x2 = c(1:10, rep(NA, 10))) #Recreating dataset.
pre = preProcess(missing.data, method = "medianImpute") 
missing.data = predict(pre, missing.data)
missing.data

### Why Caret?
missing.data = data.frame(x1 = 1:20, x2 = c(1:10, rep(NA, 10))) #Recreating dataset.
pre = preProcess(missing.data, method = c("scale", "medianImpute"))   # wait, what does scale do
missing.data = predict(pre, missing.data)
missing.data


#Mean value imputation method 4.
library(Hmisc) #Load the Harrell miscellaneous library.
missing.data = data.frame(x1 = 1:20, x2 = c(1:10, rep(NA, 10))) #Recreating dataset.
imputed.x2 = impute(missing.data$x2, mean) #Specifically calling the x2 variable.
imputed.x2

summary(imputed.x2) #Summary information for the imputed variable.
is.imputed(imputed.x2) #Boolean vector indicating imputed values.

missing.data$x2 = imputed.x2 #Replacing the old vector.

mean(missing.data$x2) #Mean of x2 after imputation.
sd(missing.data$x2) #Standard deviation of x2 after imputation.
cor(missing.data, use = "complete.obs") #Correlation afterto imputation.

plot(missing.data) #What are some potential problems with mean value imputation?



##################################
#####Simple Random Imputation#####
##################################
#Recreating a dataset that has missing values.
missing.data = data.frame(x1 = 1:20, x2 = c(1:10, rep(NA, 10)))
missing.data

mean(missing.data$x2, na.rm = TRUE) #Mean of x2 prior to imputation.
sd(missing.data$x2, na.rm = TRUE) #Standard deviation of x2 prior to imputation.
cor(missing.data, use = "complete.obs") #Correlation prior to imputation.

set.seed(0)
imputed.x2 = impute(missing.data$x2, "random") #Simple random imputation using the
                                           #impute() function from the Hmisc package.
imputed.x2

summary(imputed.x2) #Summary information for the imputed variable.
is.imputed(imputed.x2) #Boolean vector indicating imputed values.

missing.data$x2 = imputed.x2 #Replacing the old vector.

mean(missing.data$x2) #Mean of x2 after imputation.
sd(missing.data$x2) #Standard deviation of x2 after imputation.
cor(missing.data, use = "complete.obs") #Correlation afterto imputation.

plot(missing.data) #What are some potential problems with mean value imputation?



#############################
#####K-Nearest Neighbors#####
#############################
#Recreating a dataset that has missing values.
missing.data = data.frame(x1 = 1:20, x2 = c(1:10, rep(NA, 10)))
missing.data

### Note: knnImpute with caret::preProcess force normalization
#Imputing using 1NN.
pre.1nn = preProcess(missing.data, method = 'knnImpute', k=1)
library(RANN)
imputed.1nn = predict(pre.1nn, missing.data)

#Imputing using 5NN.
pre.5nn = preProcess(missing.data, method = 'knnImpute', k=5)
imputed.5nn = predict(pre.5nn, missing.data)

#Imputing using 9NN.
pre.9nn = preProcess(missing.data, method = 'knnImpute', k=9)
imputed.9nn = predict(pre.9nn, missing.data)


#the resuts show the normalization of elements (why?)

imputed.1nn #Inspecting the imputed values of each of the methods;  
imputed.5nn #what is going on here? Given our dataset, should we
imputed.9nn #expect these results?



#K-Nearest Neighbors regression on the sleep dataset.
sqrt(nrow(sleep)) #Determining K for the sleep dataset.

 #Using 8 nearest neighbors.
pre.8nn = preProcess(sleep, method = 'knnImpute', k=8)
sleep.imputed8NN = predict(pre.8nn, sleep)

summary(sleep) #Summary information for the original sleep dataset.
summary(sleep.imputed8NN[, 1:10]) #Summary information for the imputed sleep dataset.
#remember: its normalized


#K-Nearest Neighbors classification on the iris dataset.
help(iris) #Inspecting the iris measurement dataset.
iris

iris.example = iris[, c(1, 2, 5)] #For illustration purposes, pulling only the
                                  #sepal measurements and the flower species.

#Throwing some small amount of noise on top of the data for illustration
#purposes; some observations are on top of each other.
set.seed(0)
iris.example$Sepal.Length = jitter(iris.example$Sepal.Length, factor = .5)
iris.example$Sepal.Width = jitter(iris.example$Sepal.Width, factor= .5)

col.vec = c(rep("red", 50), #Creating a color vector for plotting purposes.
            rep("green", 50),
            rep("blue", 50))

plot(iris.example$Sepal.Length, iris.example$Sepal.Width,
     col = col.vec, pch = 16,
     main = "Sepal Measurements of Iris Data")
legend("topleft", c("Setosa", "Versicolor", "Virginica"),
       pch = 16, col = c("red", "green", "blue"), cex = .75)

missing.vector = c(41:50, 91:100, 141:150) #Inducing missing values on the Species
# i dont understand how that c() works
iris.example$Species[missing.vector] = NA  #vector for each category.
iris.example

col.vec[missing.vector] = "purple" #Creating a new color vector to
                                   #mark the missing values.

plot(iris.example$Sepal.Length, iris.example$Sepal.Width,
     col = col.vec, pch = 16,
     main = "Sepal Measurements of Iris Data")
legend("topleft", c("Setosa", "Versicolor", "Virginica", "NA"),
       pch = 16, col = c("red", "green", "blue", "purple"), cex = .75)

#Inspecting the Voronoi tesselation for the complete observations in the iris
#dataset.
library(deldir) #Load the Delaunay triangulation and Dirichelet tesselation library.
info = deldir(iris.example$Sepal.Length[-missing.vector],
              iris.example$Sepal.Width[-missing.vector])
plot.tile.list(tile.list(info),
               fillcol = col.vec[-missing.vector],
               main = "Iris Voronoi Tessellation\nDecision Boundaries")

#Adding the observations that are missing species information.
points(iris.example$Sepal.Length[missing.vector],
       iris.example$Sepal.Width[missing.vector],
       pch = 16, col = "white")
points(iris.example$Sepal.Length[missing.vector],
       iris.example$Sepal.Width[missing.vector],
       pch = "?", cex = .66)
legend("topleft", c("Setosa", "Versicolor", "Virginica", "NA"),
       pch = 16, col = c("red", "green", "blue", "purple"), cex = .75)
#Conducting a 1NN classification imputation.
iris.imputed1NN = kNN(iris.example, k = 1)


#Assessing the results by comparing to the truth known by the original dataset.
table(iris$Species, iris.imputed1NN$Species)

#Conducting a 12NN classification imputation based on the square root of n.
sqrt(nrow(iris.example))
iris.imputed12NN = kNN(iris.example, k = 12)


#Assessing the results by comparing to the truth known by the original dataset.
table(iris$Species, iris.imputed12NN$Species)



##################################################
#####Using Minkowski Distance Measures in KNN#####
##################################################
library(kknn) #Load the weighted knn library.

#Separating the complete and missing observations for use in the kknn() function.
complete = iris.example[-missing.vector, ]
sum(is.na(complete))   # no missing data
head(complete)
missing = iris.example[missing.vector, -3]
sum(is.na(missing))   # no missing data, just unclassified data
head(missing)

#Distance corresponds to the Minkowski power.
iris.euclidean = kknn(Species ~ ., complete, missing, k = 12, distance = 2)
#  whatImClassifying ~ col(s) you want to use to do it
summary(iris.euclidean)

iris.manhattan = kknn(Species ~ ., complete, missing, k = 12, distance = 1)
summary(iris.manhattan)

