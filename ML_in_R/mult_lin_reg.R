library(car)
library(MASS)
library(caret)


#Q1: Multiple Linear Regression on NYC Restaurants
# load dataset
NYC = read.table("./Homework/[04] NYC Restaurants.txt")
head(NYC)
summary(NYC)
sapply(NYC, sd)
cor(NYC)   # wont work # how to fix this?

#Q1: scatterplot matrix of continuous variables
plot(NYC)
# price, food, decor, and service all seem to have somewhat linear relationships to each other
# service ~ price and food ~ price seem to have most variance

#Q2: mult reg model
model = lm(Price ~ . - Restaurant, NYC)
summary(model)

# a : Price = (1.5 * Food) + (1.9 * Service) + (-.002 * Service) - (2.07 * West location) - 21.96
# b : a west location and better serice have a negative relationship with price, food and decor have positive, the intercept is a neg price
# c : the p-vlue shows that the price, food, and especially decor are significant. Service is not. A west location has a p-val of 0.3 so it is significant, but not as much as others
# d : The Fstatistic has a low p-value, so it is significant
# e : The RSE is 5.738 which shows the standard deviation of the residuals from the regression 
# f : the R(adj)^2 (coeff of determination) is 63%, which shows 63% of variability in price is due to the other variables

#Q3: model assumptions:
plot(model)

# looks pretty good, 
# football pattern for first graph possibly (resid v. fitted)
# the normal Q-Q varies past 1 Theoretical quantile
# and the Scale-Location shows a gradual increase, but everyhting within cook's distance


#Q4: influence plot: any conerning restaurants
influencePlot(model)
#restaurants 56, 130, and 168 show most influence but all below cook distance threshold
# the graph shows two with extreme Hat-Values (what are those??????)

#Q5: coefficient variance inflation factors...discuss multicollinearity
vif(model)
# nothing is greater than 5 so we don't need to be concerned

#Q6: added variable plots ... conclusions?
avPlots(model)
# decor most significant, then food, west location negative, and service seems to have no influence
# ???????

#Q7: simple lin reg price ~ service
model.simple = lm(Price ~ Service, data = NYC)
plot(NYC$Price, NYC$Service)
summary(model.simple)
abline(model.simple, lty = 3)   #why wont this work?????
plot(model.simple)
# beta1 is significant but RSE increased from 5.7 to 7.2 
# and R^2 decreased from 63% to 41% showing the relationship is 
# not as strong
# this agrees with what the avplot showed








#Q2: Model selection on NYC Restaurants
#Q1: regress price ~ food, decor, location
model.new = lm(Price ~ Food + Decor + Location, data = NYC)
# a : everything is very significant, location has highest p-value but still lower than 0.05
summary(model.new)
# b : linearity, normality, constant variance, independent errors, multicollinearity 
plot(model.new)
# first graph football like, normality Q-Q good
# scale-location still increasing (good/bad??????) # its fine because theres just lets dots on the side
# everything still within cook's distance
# c : restaurants 130, 56, 117 show most influence, but still within dook's distance so fine
influencePlot(model.new)
# d : everything below 5, better than before as well :)
vif(model.new)
# e : food has improved i think, location still not great
avPlots(model.new)


#Q2: partial F Test & interpret
anova(model.new, model)
# the p-value is 0.99 so knocking off service was good 
# (had to think backwards on this one)
# b/c the null hypothesis for the f-test is that the slopes (of all removed features) are all zero!
# the p-value is high so we keep the null hypothesis, i.e. the beta coef of service is zero


#Q3: new reduced model price ~ food + decor
model.reduced = lm(Price ~ Food + Decor, data = NYC)
summary(model.reduced)
# everything significant, R^2 is 62%
plot(model.reduced)
# scale-location model still shows increase but everything else is good 
# is scale-location indicating independent errors????    # independence isn't happening if you see a pattern in the errors


#Q4: Compare with AIC 
AIC(model, model.new, model.reduced)
# model.new has smallest AIC (1068)


#Q5: Compare with BIC
BIC(model, model.new, model.reduced)
# model.reduced had smallest BIC but just barely



#Q6: did you expet results from AIC/BIC? which model would you choose
#,BIC puts larger penalty on complexity of model
# BIC is descriptive, AIC is predictive

# I would choose model.new from AIC b/c we are trying to predict the price, not classify it
# also, model.new was much closer in BIC than model.reduced was in AIC
# also, AIC score was overall lower (but idk if that matters...probs not)