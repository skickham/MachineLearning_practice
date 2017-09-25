#######################################################
#######################################################
###########Principal Component Analysis################
#######################################################
#######################################################



#######################
#####Tools for PCA#####
#######################
library(psych) #Library that contains helpful PCA functions, such as:
# good at showing relation between original whatever and PCs
# but still other good PCA packages in R 

principal() #Performs principal components analysis with optional rotation.
fa.parallel() #Creates scree plots with parallell analyses for choosing K.
factor.plot() #Visualizes the principal component loadings.


############################
#####Data for Example 1#####
############################
bodies = Harman23.cor$cov #Covariance matrix of 8 physical measurements on 305 girls.
class(bodies)   # matrix!!!! needs to b a mutha fuckin matrix
bodies
# the matrix is already normalized for us
?Harman23.cor

####################
#####Choosing K#####
####################


# use this simple functiont o choose k!!!


fa.parallel(bodies, #The data in question. (remember its covariance matrix)
            n.obs = 305, #Since we supplied a covaraince matrix, need to know n.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform. # why 100?
abline(h = 1) #Adding a horizontal line at 1.
# makes the damn scree plot
# only goes up to 8 because thats the number of columns
# shows the height of the variance at each point (blue x)
# first PC has biggest variance, second PC has second biggest, and so on
# couple of different ways to select number of components you want
  # the total variance will equal 8 becase thats the number of columns we have
  # the first PC shows half the total variance (above 4)
  # simple way: remove everybody whos variance is less than one
  # so this way, we choose k = TWO = 2
  # other option: once the drop is no longer dramatic, 
  # it indicates our PC no longer gives good information
  # so, in this case, we choose THREE = 3 = k = number of PCs

# very confused by the red dashed line, something about create a random data set
# and by chance it gives us a PC/eigenvalue if data is completely random 
# so we want everything above the randomness but once it goes below the red dashed line,
# cut it out uncle joey --> in which case we choose k = 2



#1. Kaiser-Harris criterion suggests retaining PCs with eigenvalues > 1; PCs with
#   eigenvalues < 1 explain less varaince than contained in a single variable.
#2. Cattell Scree test visually inspects the elbow graph for diminishing return;
#   retain PCs before a drastic drop-off.
#3. Run simulations and extract eigenvalues from random data matrices of the same
#   dimension as your data; find where the parallel analysis overshadows real data.



########################
#####Performing PCA#####
########################
pc_bodies = principal(bodies, #The data in question. (a covariance matrix...always?)
                      nfactors = 2, #The number of PCs to extract.
                      rotate = "none")
pc_bodies

#-PC columns contain loadings; correlations of the observed variables with the PCs.
#-h2 column displays the component comunalities; amount of variance explained by
# the components.
#-u2 column is the uniqueness (1 - h2); amount of varaince NOT explained by the
# components.
#-SS loadings row shows the eigenvalues of the PCs; the standardized varaince.
#-Proportion/Cumulative Var row shows the variance explained by each PC.
#-Proportion Explained/Cumulative Proportion row considers only the selected PCs.

########################
### OUTPUT #############
########################

# Principal Components Analysis
# Call: principal(r = bodies, nfactors = 2, rotate = "none")
# Standardized loadings (pattern matrix) based upon correlation matrix
#                PC1   PC2   h2    u2 com
# height         0.86 -0.37 0.88 0.123 1.4      #######  0.86 is the correlation between height and PC1!!!
# arm.span       0.84 -0.44 0.90 0.097 1.5
# forearm        0.81 -0.46 0.87 0.128 1.6      #######  also notice that half of numbers in PC2 are negative....hmmm
# lower.leg      0.84 -0.40 0.86 0.139 1.4
# weight         0.76  0.52 0.85 0.150 1.8
# bitro.diameter 0.67  0.53 0.74 0.261 1.9
# chest.girth    0.62  0.58 0.72 0.283 2.0
# chest.width    0.67  0.42 0.62 0.375 1.7

0.86**2 + 0.37**2 # = 0.875 = 0.88 which is h2
# i.e. h2 is sum of squares of first 2 PCs
# u2 = 1.0 - h2

#                        PC1  PC2
# SS loadings           4.67 1.77   # shows the variance values from first 2 PCs on graph
# Proportion Var        0.58 0.22   # 58% of total variance (8) attrib to PC1, 22% PC2
# Cumulative Var        0.58 0.81   # 58% of total variance left is PC1, 81% in combination with PC1 :)
# Proportion Explained  0.73 0.27   # 4.67/(4.67+1.77) = 73%


########################################
#####Visualizing & Interpreting PCA#####
########################################
factor.plot(pc_bodies,
            labels = colnames(bodies)) #Add variable names to the plot.

# theres two groups in the plot
# lengths in bottom right, widths/girths/volumes in upper left

#-PC1 correlates highly positively with length-related variables (height, arm
# span, forearm, and lower leg). This is a "length" dimension.
#-PC2 correlates highly positively with volume-related variables (weight, bitro
# diameter, chest girth, and chest width). This is a "volume" dimension.



############################
#####Data for Example 2#####
############################
iris_meas = iris[, -5] #Measurements of iris dataset.
iris_meas
plot(iris_meas)
# look at al the scatter plots, what would PC grab??
# capture 2 columns at once and use one PC to describe them
# i.e. petal.length and petal.width show some ocllinearity so PCA will get those



####################
#####Choosing K#####
####################
fa.parallel(iris_meas, #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.
# Should extract 1 PC, but let's look at 2.
# arbitrary, so whatevs dawg



########################
#####Performing PCA#####
########################
pc_iris = principal(iris_meas, #The data in question.
                    nfactors = 2,
                    rotate = "none") #The number of PCs to extract.
pc_iris

factor.plot(pc_iris,
            labels = colnames(iris_meas)) #Add variable names to the plot.

#-PC1 separates out the importance of the sepal width as cotrasted with the
# remaining variables.
#-PC2 contrasts the differences between the sepal and petal measurements.



################################
#####Viewing Projected Data#####
################################
plot(iris_meas) #Original data: 4 dimensions.
plot(pc_iris$scores) #Projected data: 2 dimensions.
# looks like the original plot of petal length vs sepal width
# bc those to properties had the highest correlations with the PCs
pc_iris$scores # gives projection of points on eigenvectors, the transformation of the original data
dim(pc_iris$scores) # 2 components on 150 observations
cor(pc_iris$scores[,1], pc_iris$scores[,2])

############################
#####Data for Example 3#####
############################
library(Sleuth2)
case1701
printer_data = case1701[, 1:11]

fa.parallel(printer_data, #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.
#Should extract 1 PC, but let's look at 3.
# why does the first one drop so much more quickly than everything else????
# could be high multicollinearity
# something is very biased!!!! (didnt normalize!!!) so BE-FUCKIN-WARE!


pc_printer = principal(printer_data, #The data in question.
                       nfactors = 3,
                       rotate = "none") #The number of PCs to extract.
pc_printer
# PC1 kind of encapsulates strengths of magentic field as a whole, captures all features
# does PC2 say something? half the features pso and half neg, so some symmetry is there
# PC3 reveals...? middle is different from the two sides (neg on sides, pos in middle)


factor.plot(pc_printer) #Add variable names to the plot.

#-PC1 ends up being a weighted average.
#-PC2 contrasts one side of the rod with the other.
#-PC3 contrasts the middle of the rod with the sides of the rod.

plot(printer_data)   # all of them seem correlated which is why only one PC is good 
pairs(pc_printer$scores)

######################################
######################################
#########   can use caret too!!!!!!!
# Oh Baby Baby!


### Preprocess with PCA with caret
library(caret)

### The number of principal components kept can be decided by
### 1. thres: set the threshold for cumulative variance
### 2. pcaComp: set explicitlythe amount to be kept
### If pcaComp is set, thres would be ignored

ctrl <- trainControl(preProcOptions = list(thres = 0.90,
                                           pcaComp = 3)) 
# four numerical columns but keep 3 most important ones
# or can have a threshold so that once you get greater than it, you stop
# (unsure of the math on this, but get general idea)

# for the test dataset, keep the eigenvectors from the training dataset,
# ie. no need to reconfigure new eigenvalues/vectors each time

md = train(Species ~ ., data = iris,
           method = 'glmnet',
           preProc = 'pca',
           family = 'multinomial',
           trControl = ctrl)

### The predictors included in the final model
md$finalModel$xNames
