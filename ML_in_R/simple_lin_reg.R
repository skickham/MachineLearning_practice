library(caret)
library(MASS)
head(cats)

#Q1 : scatterplot of heart weight vs. body weight

plot(cats$Bwt, cats$Hwt, xlab = 'Body weight', ylab = 'Heart weight', main = "Scatterplot of Cats Dataset") 
# The plot does seem to increase linearly to me, so I do think it would be good

#Q2: Regress heart onto body weight
model = lm(Hwt ~ Bwt, cats)
summary(model)

# a : H = 4B - 0.36
# b : 4 is the slope, the increase in heart weight for each kg in body weight, -.36 is the height weight for a cat that weighs 0 kg
# c : beta1 is because the p-val is so small, but beta0 is not
# d : it is significant because the slope is significant. The F-statistic is the t-value squared
# e : The RSE is 1.452, it is the average deviation of the observation around the regression
# f : The R squared value/coefficient of determination is 65%, which is the percent of variability in heart weight that can be determined by body weight

#Q3: add regression line
abline(model, lty = 3)  # the abline takes the intercept and slope from the models summary, the lty = 3 gives the type of line

#Q4: add residual visulaization
segments(cats$Bwt, cats$Hwt,    #starting points for line
         cats$Bwt, (model$coefficients[1] + model$coefficients[2]*cats$Bwt),  #equation of line to draw line segments to 
         col = "red")
text(cats$Bwt - .03, cats$Hwt, round(model$residuals, 2), cex = 0.5)   # don't totally understand this part??

# The last point has the largest residual, but it doesn't seem too unusual since it's a single point and it's at the end

#Q5: construct 95% CI for coefficients
confint(model)

# 95% confident that the heart weight of a no-pound cat is between those two values
# and 95% confident that the heart weight increases between those two amount for each 1 kg increase in its body weight


#Q6: assess assumptions

#linearity, normality, constant variance, independent errors
plot(model)
# everything looks good. point 144 is only thing concerning but still within cook's distance

#Q7: add confidence and prediction bands to scatterplot
plot(cats$Bwt, cats$Hwt, xlab = 'Body weight', ylab = 'Heart weight', main = "Scatterplot of Cats Dataset") 
abline(model, lty = 3)

newdata = data.frame(Bwt = seq(1.9, 4, length.out = 100))      #i dont understand what the newdata does   1.9 and 4 is the x-axis range, 100 gives the number of pts in between??
conf.band = predict(model, newdata, interval = "confidence")
pred.band = predict(model, newdata, interval = "prediction")


lines(newdata$Bwt, conf.band[, 2], col = "blue") #Plotting the lower confidence band. # second col gives lower bound
lines(newdata$Bwt, conf.band[, 3], col = "blue") #Plotting the upper confidence band.
lines(newdata$Bwt, pred.band[, 2], col = "red") #Plotting the lower prediction band.
lines(newdata$Bwt, pred.band[, 3], col = "red") #Plotting the upper prediction band.
legend("topleft", c("Regression Line", "Conf. Band", "Pred. Band"),
       lty = c(2, 1, 1), col = c("black", "blue", "red"))

# the confidence band represents the area of all possible regression lines given any random sample of the data
# the prediction bands give area of all possible observations
# The conf band widens because it shows a rotation of the regression line about a single point
# additionally, on the ends of the graph, there a fewer points to assess our data, allowing for more error, making the bands wider


#Q8: conf and pred intervals for 2.8, 5, 10 kg. any problems?
newdata = data.frame(Bwt = c(2.8, 5, 10))   # init new body weights
predict(model, newdata, interval = "confidence")  # use the model as anchor for new data and construct conf interval
predict(model, newdata, interval = "prediction")    # same with prediction

# 2.8 is fine because it is within the range of our current data, however, 5 and 10 are outside our current range, making it hard to predict.
# for instance, maybe at a certain point, the cats heart stops growing as the cat becomes heavier but our data wouldn't account for that



#Question 2: Considering Transformations

#Q1: box-cox plot
box = boxcox(model)

#Q2: what's the best lambda? (interpretability and accuracy)
# between -0.3 and 0.6
lambda = box$x[which(box$y == max(box$y))]
# max at 0.10101
# will choose 0.10101  (could also choose 0 for ease)

#Q3: transform data with new lambda
Hwt.box = (cats$Hwt^lambda - 1)/lambda #Applying the Box-Cox transformation.

#Q4: construct new regression
model.box = lm(Hwt.box ~ cats$Bwt)

#Q5: scatterplot with new reg line
plot(cats$Bwt, Hwt.box, xlab = "Body Weight", ylab = "BC Heart Weight",
     main = "Box-Cox Transformed Data")
abline(model.box, lty = 2)

#Q6: summary and assess. any concerns?
summary(model.box)
plot(model.box)
# nothing concerning, the intercept is better, R^2 is about the same, the plots show nothing bad

#Q7: compare models
# the original model is easier to understanding with regard to the slope in particular. At first I was confused by the boxcox slope wondering how
# it changed so drastically and then I realized that the data was transformed
# the new model is better because it corrects the intercept. However, I would still choose the original because its just easier.

#Q8: try applying box cox again. what happens?
boxcox(model.box)
# lambda equals 1, which would change nothing because it has already been optimized by box cox