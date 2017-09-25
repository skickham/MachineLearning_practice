library(data.table)

protein = fread('[08] Protein.txt')
head(protein)

#1
protein = read.table("[08] Protein.txt", sep = "\t", header = TRUE)
protein.scaled = as.data.frame(scale(protein[, -1]))
rownames(protein.scaled) = protein$Country

#inspect
summary(protein.scaled)
sapply(protein.scaled, sd)


#2 
wssplot = function(data, nc = 15, seed = 0) {
  wss = (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
  }
  plot(1:nc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
}
wssplot(protein.scaled)

# kMeans might not truly be appropriate because 
# there is more of a curve to the scree plot and less of an elbow 
# which makes it seem like there is no clear good number of clusters to use
# also, possibly too few observations

#3
set.seed(0)
km.protein2 = kmeans(protein.scaled, centers = 2) #Running the K-means procedure
km.protein3 = kmeans(protein.scaled, centers = 3) #5 different times, but with
km.protein4 = kmeans(protein.scaled, centers = 4) #only one convergence of the
km.protein5 = kmeans(protein.scaled, centers = 5) #algorithm each time.
km.protein6 = kmeans(protein.scaled, centers = 6)

#4
set.seed(0)
km.protein.multi = kmeans(protein.scaled, centers = 6, nstart = 100) 


#5
par(mfrow = c(2, 3))
plot(x=protein.scaled$Cereals, y=protein.scaled$RedMeat, col = km.protein2$cluster,
     main = paste("Single K-Means Attempt #1\n WCV: ",
                  round(km.protein2$tot.withinss, 4)))
plot(x=protein.scaled$Cereals, y=protein.scaled$RedMeat, col = km.protein3$cluster,
     main = paste("Single K-Means Attempt #2\n WCV: ",
                  round(km.protein3$tot.withinss, 4)))
plot(x=protein.scaled$Cereals, y=protein.scaled$RedMeat, col = km.protein4$cluster,
     main = paste("Single K-Means Attempt #3\n WCV: ",
                  round(km.protein4$tot.withinss, 4)))
plot(x=protein.scaled$Cereals, y=protein.scaled$RedMeat, col = km.protein5$cluster,
     main = paste("Single K-Means Attempt #4\n WCV: ",
                  round(km.protein5$tot.withinss, 4)))
plot(x=protein.scaled$Cereals, y=protein.scaled$RedMeat, col = km.protein6$cluster,
     main = paste("Single K-Means Attempt #5\n WCV: ",
                  round(km.protein6$tot.withinss, 4)))
plot(x=protein.scaled$Cereals, y=protein.scaled$RedMeat, col = km.protein.multi$cluster,
     main = paste("Best K-Means Attempt out of 100\n WCV: ",
                  round(km.protein.multi$tot.withinss, 4)))

#6 
par(mfrow = c(1,1))
plot(x=protein.scaled$Cereals, 
     y=protein.scaled$RedMeat, 
     col = km.protein.multi$cluster,
     main = paste("Best K-Means Attempt out of 100\n WCV: ",
                  round(km.protein.multi$tot.withinss, 4)))
points(km.protein.multi$centers[, 6], km.protein.multi$centers[,1], pch = 16, col = "blue")
abline(h = 0)
abline(v =0)
### remember this next one
text(protein.scaled$Cereals, protein.scaled$RedMeat,
     labels = rownames(protein.scaled),
     col = km.proteinsim$cluster)






#7
# I think 6 is too many clusters and particularly in the 
# middle group there don't seem to be clear lines





#Q2: Hierarchical Clustering
#1
library(flexclust)
d = dist(protein.scaled)

#2
fit.single = hclust(d, method = "single")
fit.complete = hclust(d, method = "complete")
fit.average = hclust(d, method = "average")


#3
par(mfrow = c(1, 3))
plot(fit.single, hang = -1, main = "Dendrogram of Single Linkage")
plot(fit.complete, hang = -1, main = "Dendrogram of Complete Linkage")
plot(fit.average, hang = -1, main = "Dendrogram of Average Linkage")
# single not good to use b/c higher groups adding one at a time
# complete is good to use b/c groups of groups an dhigher height(?)

#4 
clusters.complete = cutree(fit.complete, k = 2)
clusters.complete
table(clusters.complete)   # 8 in first, 17 in second

#Visualizing the groups in the dendrogram.
par(mfrow = c(1, 1))
plot(fit.complete, hang = -1, main = "Dendrogram of Average Linkage\n2 Clusters")
rect.hclust(fit.average, k = 2)
#Aggregating the original data by the cluster assignments.
aggregate(protein, by = list(cluster = clusters.complete), median)

#Aggregating the scaled data by the cluster assignments.
aggregate(protein.scaled, by = list(cluster = clusters.complete), median)

# cluster 2 consumes more whitemeat and milk, cluster 1 
# consumes more cereals and fruits and veggies

#5
clusters.complete = cutree(fit.complete, k = 5)
clusters.complete
table(clusters.complete)   # 8 in first, 17 in second
par(mfrow = c(1, 1))
plot(fit.complete, hang = -1, main = "Dendrogram of Average Linkage\n2 Clusters")
rect.hclust(fit.average, k = 5)

aggregate(protein, by = list(cluster = clusters.complete), median)
aggregate(protein.scaled, by = list(cluster = clusters.complete), median)

# group 2 more red meat
# cluster 5 much less white meat
# cluster 1 least eggs and fish
# cluster 3 most white meat and starch
# cluster 4 most milk and least nuts and fruits and veggies


# From solutions:
#The 5-cluster solution of the complete linkage dendrogram creates clusters that
#have between 2 and 8 members each. The smallest cluster consists of just Portugal
#and Spain and has a particualrly low median consumption of red meat, white meat,
#and milk, but particularly high median consumption of fish, starch, nuts, fruits,
#and vegetables. The second smallest cluster consists of Finland, Norway, Denmark,
#and Sweden. This cluster has the highest median consumption of milk, the second
#largest median consumption of fish, but particularly low consumption of nuts
#fruits, and vegetables. The next smallest cluster consists of Hungary, USSR,
#Poland, Czech., and East Germany. This cluster is particularly denoted by its
#high median consumption of white meat and starch. The largest cluster is home
#to the countries France, UK, Ireland, Belgium, West Germany, Switzerland,
#Austria, and the Netherlands. These countries have the particular characteristic
#of the highest median consumption of red meat and eggs. The second largest cluster
#contains Greece, Italy, Albania, Bulgaria, Romania, and Yugoslavia. This cluster
#is denoted by their lowest median consumption of eggs, fish, and starch, but
#the highest median consumption of cereals and nuts.
