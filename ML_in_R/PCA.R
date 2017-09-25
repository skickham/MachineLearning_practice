####### PCA Homework
par(mfrow = c(1,1))
library(HSAUR)
head(heptathlon)

#1
plot(heptathlon)

# much linearity in performance between events...few outliers
# certain events seem more correlated than others

#2 
colnames(heptathlon)
heptathlon2 = heptathlon
heptathlon2$hurdles = max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon2$run200m = max(heptathlon$run200m) - heptathlon$run200m
heptathlon2$run800m = max(heptathlon$run800m) - heptathlon$run800m
head(heptathlon2)

#3
plot(heptathlon2)
# the linearity and correlation is more clear when they all go in the same direction
# all positively correlated


#4 
library(psych)
fa.parallel(heptathlon2[,-8], #The data in question. (remember its covariance matrix)
            n.obs = 25, #Since we supplied a covaraince matrix, need to know n.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform. # why 100?
abline(h = 1) #Adding a horizontal line at 1.
# the scree plot is the SS loadings


# 3 PCs, because that's where it stops dropping significantly
# 2 PCS because there its above the line h = 1
# 1 PC because thats the only one above the simulated data


#5 
pc_heptathlon = principal(heptathlon2[,-8], #The data in question. (a covariance matrix...always?)
                      nfactors = 2, #The number of PCs to extract.
                      rotate = "none")
pc_heptathlon
# SS loadings is the scree plot

#6 
# PC1 is 4.46, PC2s variance is 1.19


#7 
# 81% of the variability in the original dataset is captured by the PCs


#8
plot(pc_heptathlon)   # they do the same thing but diff labels
factor.plot(pc_heptathlon,
            labels = colnames(heptathlon[,-8])) #Add variable names to the plot.


#9 
# The first principal captures the variability of most the events
# PC2 mainly just captures javelin (up in its own corner)
# long jump, hurdles high jump, running all more closely ocrrelated


#10   # i dont understand this one so much
plot(pc_heptathlon$scores, type = "n")
text(pc_heptathlon$scores, rownames(heptathlon2), cex = .5)
# shes an outliear 


#11
View(heptathlon2)
# the major outlier is Launa and that is because she did not participate in a couple
# of events for some reason (score of zero)
