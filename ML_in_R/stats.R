library(datasets)
library(dplyr)
library(ggplot2)
library(data.table)

#Question 1
#1
setwd('C:/Users/skick/Desktop/NYC Data Science Academy/Class_Stats/1_stats/')
temp = read.table('[01] Temp.txt')
temp
summary(temp)

#Body.Temp         Gender     Heart.Rate   
#Min.   : 96.30   Female:65   Min.   :57.00  
#1st Qu.: 97.80   Male  :65   1st Qu.:69.00  
#Median : 98.30               Median :74.00  
#Mean   : 98.25               Mean   :73.76  
#3rd Qu.: 98.70               3rd Qu.:79.00  
#Max.   :100.80               Max.   :89.00  

sd(temp$Body.Temp)    #0.733
var(temp$Body.Temp)   #0.538
sd(temp$Heart.Rate)   #7.062
var(temp$Heart.Rate)  #49.873


group_by(temp, Gender) %>% 
  mean()

cor(temp$Body.Temp, temp$Heart.Rate)    #0.254  --> not a strong correlation

#2
boxplot(temp$Body.Temp)
boxplot(temp$Heart.Rate)

g = ggplot(temp, aes(x = Gender)) 

g + geom_boxplot(aes(y = Body.Temp))
ggplot(temp, aes(x = Body.Temp)) + geom_density(aes(color = Gender))

g + geom_boxplot(aes(y = Heart.Rate))
ggplot(temp, aes(x = Heart.Rate)) + geom_density(aes(color = Gender))

ggplot(temp, aes(x = Body.Temp, y = Heart.Rate)) + 
  geom_point() + 
  geom_smooth(method ='lm', se = F)


#3
#h0 : mu = 98.6
#hA : mu != 98.6
t.test(temp$Body.Temp, mu = 98.6, conf.level = 0.95) #automatically does two-sided
#95% confidence interval is between 98.122 and 98.37646        
#??????????? --> shouldn't it be on either side of 98.6 
#---> no it's around the calculated mean. 98.6 is outside of the 95% CI and so we reject h0

#p-value = 2.411e-07   --> reject h0 in favor of hA: the mean is NOT equal to 98.6

#4
#h0 : mean diff == 0
#hA : true diff in mean != 0
t.test(temp$Body.Temp[temp$Gender == 'Male'], 
       temp$Body.Temp[temp$Gender == 'Female'])
#95% CI: [-1.674501  3.243732]
#male mean: 98.1, female mean: 98.39
#p-value = 0.024 = 2% <  5% --> significant difference
View(temp)


#5
#h0 : no diff in mean heart rate
#hA : mean(femal HR) < mean(male HR)
var.test(temp$Heart.Rate[temp$Gender == 'Female'], 
       temp$Heart.Rate[temp$Gender == 'Male'],
       alternative = 'less')
#95% CI: [-Inf 2.843314]     # negative infiniti to 2.8 bpm diff
#female mean: 74.15, male mean: 73.37
#p-value = 0.7357 = 74% > 5% --> retain null hypothesis




#Question 2
#1
head(PlantGrowth)
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot(aes(color = group))

#the control group has the widest range with a median of 5.2
#the first treatment decreases the weight (median = 4.5)
#the second treatment increases weight (median = 5.4)


#2
#are they asking for std dev or variance....does the difference matter?
ggplot(PlantGrowth, aes(x = weight)) + geom_density(aes(color = group))
plants = PlantGrowth
sd_weights = group_by(plants, group) %>% 
  summarise(sd(weight))
#would use the F-test, but we have more than two groups

#bartlett.test(sd_weights)
#bartlett.test(plants$weight[plants$group == 'ctrl'],
#              plants$weight[plants$group == 'trt1'], 
#              plants$weight[plants$group == 'trt2'])
#bartlett.test(select(filter(PlantGrowth, group == 'ctrl'), weight),
#              select(filter(PlantGrowth, group == 'trt1'), weight),
#              select(filter(PlantGrowth, group == 'trt2'), weight))

bartlett.test(weight ~ group, PlantGrowth)    #compare the variances of the weights grouped_by group
#Bartlett's K-squared = 2.8786, df = 2, p-value = 0.2371 = 23% > 5%
#retain null, they do NOT differ significantly



#3
#One-way ANOVA Test (comparing more than two means)
#h0 : there is no significant difference in weight by group
#hA : mu(weight) differ significantly by applied treatment
summary(aov(weight ~ group, PlantGrowth))  #anova test of weight by treatment group 
#            Df Sum Sq Mean Sq F value Pr(>F)  
#group        2  3.766  1.8832   4.846 0.0159 *
#Residuals   27 10.492  0.3886                 
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#p-value = .0159 = 1.6% < 5%
#reject h0, because the means differ significantly 

#??? I do not know how to relate this to the Bartlett test
#the anova test assumes no sig diff in std devs, since the Bartlett test showed this to be true,
#the ANOVA test result is valid





#Question 3
#1
mosaicplot(HairEyeColor, shade = TRUE)
#not sure if reading this correctly, buuuutttt
#blonde females with blue eyes were more than expected
#as were males and females with black hair and brown eyes
#blonde male and females with brown eyes were lower than expected
#as were brunette and black-haired females with blue eyes


#2
class(HairEyeColor)

#tried converting to data frame to use filter and narrow down the table 
#then convert back to table but this didnt work
#hair = as.data.frame(HairEyeColor)
#females = filter(hair, Sex == 'Female' & 
#                   (Eye == 'Brown' | Eye == 'Blue'))
#class(females)
#females2 = as.data.table(females)
#mosaicplot(females2, shade = TRUE)   #this mosaic plot does not line up as nicely as the previous

females = HairEyeColor[, c('Blue', 'Brown'), 'Female']
mosaicplot(females, shade = TRUE)

#blonde hair & blue eyes most overly represented, also black hair & brown eyes
#blonde hair & brown eyes least represented, as well as brown & black hair with blue eyes


#3
View(females)     #observed table
Exp_females = as.array(margin.table(females,1)) %*%   #total col by hair
  t(as.array(margin.table(females,2))) /              #* total col by eye     #Idk t() or why we need as.array
  margin.table(females)                               #/ total people
View(Exp_females)     #expected table
sum((Exp_females - as.array(females))^2/Exp_females)   #manual chi-sq test  #82.7

chisq.test(females)
#X-squared = 82.727, df = 3, p-value < 2.2e-16
# p-val << 5% : there is a sig diff, so some categories are out of whack

chisq.test(females)$stdres    #any result less than -2 is less than expected, more than 2 is greater than exp
#Eye
#Hair         Blue     Brown
#Black -4.223667  4.223667        #blonde contrib most to deviations 
#Brown -3.771001  3.771001        #red heads contrib the least
#Red   -1.805268  1.805268        #???? --> why do all the std dev complement each other??
#Blond  8.960249 -8.960249

(chisq.test(females)$residuals) ^2      #chi-sq formula rendering
#Eye
#Hair         Blue     Brown
#Black  7.463604  6.974187
#Brown  4.236313  3.958522
#Red    1.520543  1.420835
#Blond 29.545084 27.607701

(reduced.test$observed - reduced.test$expected)^2/reduced.test$expected
reduced.test$observed - reduced.test$expected
# reduced.test$expected:the expected counts under the null hypothesis.

#chisqaredtested $observed, expected, stdres, residuals
