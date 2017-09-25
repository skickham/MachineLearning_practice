
#Q1: Wine Quality
library(data.table)
wine = fread('[10] Wine Quality.csv')

# 1 : data munging: recode quality to Low/High
wine$quality = ifelse(wine$quality<6, 'Low', 'High')
wine$quality = as.factor(wine$quality)

# 1b: scale and center numeric vectors
num_scale = wine[,names(wine) != 'quality']
wine2 = as.data.frame(scale(num_scale, center = TRUE, scale = TRUE))
wine = cbind(wine2, wine$quality)


# 2 : split into training, test sets


# 3 : graphical EDA
# 3a: Explain why maximal margin classifier works with this data


# 3b: Explain why Support Vector Classifier is more desireable


# 4 : Tune support vector classifier
# 4a: Best cost parameter?
# 4b: Best error rate for best cost?
# 4c: Graph CV results. Is it possible you checked enough values of cost?


# 5 : # support vecotrs in best sv classifier?

# 6 : Test error of support vector classifier?

# 7 : Fit support vector classifier to all of data using best cost parameter
# 7a: # support vectors?
# 7b: Is the 555th observation a support vector?


# 8 : Overall error rate?


# 9 : Vis sv classifier by examining free sulfur dioxide and total sulfar dioxide cross-section



# 10 : tune a SVM with a radial kernel 
# 10a: best cost parameter?
# 10b: best gamma param?
# 10c: best error rate of that combo?
# 10d: graph CV results. Checked enough values of cost and gamma?


# 11 : How many support vectors in best SVM?



# 12 : What is the test error?


# 13 : Fit SVM to all of data using best cost/gamma
# 13a: # support vectors?
# 13b: Is 798th observation a support vector?



# 14 : Overall error rate for SVM?


# 15 : Vis SVM by examining free sulfur dioxide and total sulfur dioxide cross-section.



# 16 : List a pro and con for both
# 16a: the best support vector classifier.
# 16b: the best support vector machine. 



