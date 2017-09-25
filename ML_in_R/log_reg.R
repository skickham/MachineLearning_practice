library(car)
library(dplyr)


#Q1: Birdkeeping and lung cancer
library(Sleuth2)
head(case2002)
?case2002
sapply(case2002, class)


# i : numerical & graphical EDA (exploratory data analysis), scatterplots of continuous variables
summary(case2002)
sapply(case2002, sd)
nrow(case2002)  # 147
plot(case2002[c('AG', 'YR', 'CD')], col = case2002$LC)
# years smoking is limited by the subjects age. and so they are somewhat correlated
# but it does seem like there is some LC groups which means it may be predictable base don these parameters
scatterplotMatrix(~AG+YR+CD|LC,data=case2002[, c(1,5:7)])
# shows same thing but with lines


# ii : log regress LC ~ . 
logit.overall = glm(LC ~ .,
                    family = "binomial",   #this fam is for logistic
                    data = case2002)
summary(logit.overall)   #BK and YR seem significant
confint(logit.overall)
confint.default(logit.overall)


# iii : assess residual plot and influence plot
# plot(logit.overall)   # not useful for nonlinear
#Residual plot for logistic regression with an added loess smoother; we would
#hope that, on average, the residual values are 0.
scatter.smooth(logit.overall$fit,
               residuals(logit.overall, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Lung Cancer")   
# residual plot is not that useful in general tho, so dont spend too much time on it
abline(h = 0, lty = 2)
# we would want the average residuals to be zero, but the red line curves around it
influencePlot(logit.overall)
# seems to be some strong influencers/outliers, 
# yet only two distinguished (28 & 47)
# within cooks distance tho



# iv : goodness of fit test
summary(logit.overall)
pchisq(logit.overall$deviance, logit.overall$df.residual, lower.tail = FALSE)
# significant factors: BK bird & YR smoking
# if a subject has a bird, independent of the other factors, 
# odds of having lungcancer increase by 1.36
# for every extra year a subject has been smoking, odds of LC
# increase 0.07 holding other variables constant

# the hypothesis is swtched for the Pearson's chi square test
#The p-value for the overall goodness of fit test is about 0.20, which is greater
#than the cutoff value of 0.05. We do not have evidence to reject the null
#hypothesis that the model is appropriate.
# thus the model is appropriate



# v : interpret coefficient of gender
# for Female, the coefficient is 0.56, meaning they are slightly 
# more likely than males to have lung cancer, but this is not significant
# according to the p-value


# vi : log regress LC ~ . - BK
logit.noBK = glm(LC ~ . - BK,
                 family = "binomial",
                 data = case2002)
plot(logit.noBK)

# vii : goodness of fit test
summary(logit.noBK)
#T he AIC score is higher than before, 
# YR and AG both show significance




# viii : compare two models with drop in deviance test
#simple way
anova(logit.noBK, logit.overall, test = "Chisq")   #p-val is 0.0006

#long way
reduced.deviance = logit.noBK$deviance #Comparing the deviance of the reduced
reduced.df = logit.noBK$df.residual    #model (the one without rank) to...

full.deviance = logit.overall$deviance #...the deviance of the full model (the
full.df = logit.overall$df.residual    #one with the rank terms).

pchisq(reduced.deviance - full.deviance,   # diff of dev
       reduced.df - full.df,              # diff of resids
       lower.tail = FALSE)   
# p-value small, so original full model (with BK) is better 
#(dont get why?????)

#The p-value for the drop in deviance test is < 0.001, which is quite significant.
#We reject the null hypothesis that the coefficient for the birdkeeping variable
#is 0, and conclude that having it in the model should provide a better fit.




# ix : log regress LC ~ YR + BK
logit.reduced = glm(LC ~ YR + BK,
                    family = "binomial",
                    data = case2002)
summary(logit.reduced)




# x : compare orig model to current model with drop in deviance test
anova(logit.reduced, logit.overall, test = "Chisq")
# the p-value is large (0.42) so I would keep the reduced model




# xi : compare three models across:

#1: AIC
AIC(logit.overall, logit.noBK, logit.reduced)
#2: BIC
BIC(logit.overall, logit.noBK, logit.reduced)
#3: R(dev)^2
#What about checking the McFadden's pseudo R^2 based on the deviance?
1 - logit.overall$deviance/logit.overall$null.deviance
1 - logit.noBK$deviance/logit.noBK$null.deviance
1 - logit.reduced$deviance/logit.reduced$null.deviance
#4: argument for choosing last model?
# lowest AIC and BIC for the last model
# but McFaddens R^2 is not as high as full model


#The AIC and BIC are minimized for the most simplified model, indicating that it
#is most preferable. While the McFadden's R^2 term isn't maximized for the most
#simplified model (only about 15.5% as compared to about 17.6% for the overall
#model with all coefficients), we choose to move forward with the simplified
#model because it has relatively high predictive power alongside simplicity.



# xii : using most recent model, predict:
#1: prob LC with avg yr smoking and with or without BK
levels(case2002$BK)
newdata = with(case2002, data.frame(YR = mean(YR),
                                    BK = c("NoBird", "Bird")))
predict(logit.reduced, newdata, type = "response") # response to not give logs
# 17% LC for no bird, 48% LC for bird

#2: prob LC with 0 yr smoking and with or without BK
newdata = with(case2002, data.frame(YR = 0,
                                    BK = c("NoBird", "Bird")))
predict(logit.reduced, newdata, type = "response") 
# 4% with no bird, # 15% with bird with no smoking



# xiii : use latest model to classify training set and comment on how well model performs compared to baseline
LC.predicted = round(logit.overall$fitted.values)    # with c = 50%
table(truth = case2002$LC, prediction = LC.predicted)
# 80 + 27 = 107 correct
# 18 + 22 = 40 incorrect
# 107/147 = 73% correct classification with BK/YR model!

