############################################################################################
Imports
############################################################################################

library(plyr)
library(psych)
library(fpc)
library(ggplot2)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(purrr)
library(gridExtra)

############################################################################################
# Cleaning
############################################################################################
getwd()
data <- read.csv("./Unsupervised_Learning_Data", header = TRUE, sep = ",")
summary(data) # Look for anything unusual, see that there was an extra column added that I
              # need to delete
View(data)
data <- data[,-c(1,2)] # Get rid of the added column and ID column
View(data) # see that the columns were removed

data$Inquired <- revalue(data$Inquired, c(Yes = 1, No = 0)) # revalued inquired to No = 0, Yes = 1
class(data$Inquired)
mode(data$Inquired)
data$PermntCountry <- revalue(data$PermntCountry, c("United States"=1, "Out"=0)) # revalue PermntCountry to either 0, meaning outside of the U.S., or 1, meanining inside U.S.
class(data$PermntCountry)
mode(data$PermntCountry)
summary(data$DecisionPlan)
data$DecisionPlan <- revalue(data$DecisionPlan, c("Early Decision I"=2, "Early Decision II"=2, "Early Action I"=1, "Early Action II"=1, "Regular Decision"=0))
View(data)
data$SportRating <- revalue(data$SportRating, c("Blue Chip"= 3, "Franchise"=2, "Varsity"=1, "None"=0))
data <- data[, -c(2, 4, 5, 11, 18, 20)] # Get rid of non-numeric columns that I could
                                               #  not make into numeric or ordinal without obstructing the analysis
View(data)

############################################################################################
# Hierarchial Clustering
############################################################################################

hcdat <- data # make a separate dataset for hierarchial clusetring
View(hcdat)

sapply(hcdat, class) # check to see that all variables are numeric (which they are not)
hcdat <- sapply(hcdat, function(x) as.numeric(as.character(x))) # convert all columns to numeric while retaining their values
sapply(hcdat, class) # check to see all variables are numeric
View(hcdat)
class(hcdat) # hcdat has become a matrix now, which is fine for the time being

set.seed(345) # setting the seed for reproucible work

hc.comp=hclust(dist(hcdat), method = "complete", members = NULL) # hc by complete linkage
hc.average=hclust(dist(hcdat), method = "average", members = NULL) # hc by average linkage
hc.single=hclust(dist(hcdat), method = "single", members = NULL) # hc by single linkage
hc.centr=hclust(dist(hcdat), method = "centroid", members = NULL) # hc by centroid

par(mfrow = c(2, 2))
plot(hc.comp, main="Complete Linkage", cex = .6, hang = -1)
plot(hc.average, main = "Average Linkage", cex = .9, hang = -1)
plot(hc.single, main = "Single Linkage", cex = .9, hang = -1)
plot(hc.centr, main = "Centroid", cex=.9, hang = -1)

# Clustering based off of correlation distance
dd=as.dist(1-cor(t(hcdat))) # matrix with the "correlation distance" between observations
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="") # plot

# Make a scaled matrix of hcdat
scaledhcdat <- scale(hcdat)
View(scaledhcdat)

# Dissimilarity matrix
hcdismat <- dist(scaledhcdat, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(hcdismat, method = "complete" )
plot(hc1, cex=.6, hang = -1)

# Assessing hierarchial models based on different methods for AGNES (Agglomerative Clustering)
method <- c("average", "single", "complete", "ward") # create a variable that stores method names (code courtesy of UC R Github)
names(method) <- c("average", "single", "complete", "ward") # setting the names of variable method (code courtesy of UC R Github)
assessfunc <- function(x) {
  agnes(scaledhcdat, method = x)$ac
} # created function to compute various coefficients to see which model is best (code courtesy of UC R Github)
map_dbl(method, assessfunc) # average    single  complete      ward 
                            # 0.9066583 0.9039645 0.9359926 0.9868039 
                            # ward method turns out to be the best
assessfunc1 <- function(x) {
  agnes(hcdismat, method = x)$ac
} # created function with dissimilarity matrix to compare results of function that did not use dissimilarity matrix (code courtesy of UC R Github)
map_dbl(method, assessfunc1) # average    single  complete      ward 
                             # 0.9066583 0.9039645 0.9359926 0.9868039
                             # see results are the same, ward is still best method

hcw <- agnes(hcdismat, method = "ward") # Make an AGNES cluster based on the ward method
hcwtree <- pltree(hcw, cex = .6, hang = -1, main = "AGNES Ward Dendrogram") # making dendrogram of AGNES algorithm measured by Ward method
hcwtree # viewing the tree created *********************** Not able to read it *********************

# Assessing hierarchial model based off of DIANA algorithm (Divisive hierarchial clustering)
hcd <- diana(scaledhcdat) # making a DIANA model (code courtesy of UC R Github)
hcd$dc # looking at score of model, which equals 0.9270291 (which is not bad but not the best model) (code courtesy of UC R Github)
pltree(hcd, cex = .6, hang = -1, main = "Dendrogram of DIANA") # plotting a tree based off of DIANA algorithm (code courtesy of UC R Github)

# Calculating optimal number of clusters to apply to dendrograms
fviz_nbclust(scaledhcdat, FUN = hcut, method = "wss") # does not appear to be a clear optimal # of clusters for the elbow method (code courtesy of UC R Github)
fviz_nbclust(scaledhcdat, FUN = hcut, method = "silhouette") # looks like 4 clusters is the optimal for average silhouette method (code courtesy of UC R Github)

# Use best clustering model (which is an AGNES algorthm using the method ward) and split it by k=4 and K=7
hcagnesward <- hclust(hcdismat, method = "ward.D2") # make variable that stores hierarchial cluster using ward method (code help courtesy of UC R Github)
plot(hcagnesward, cex = .6) # (code help courtesy of UC R Github)
rect.hclust(hcagnesward, k = 4, border = 2:5) # put clusters around dedrogram (code help courtesy of UC R Github)
plot(hcagnesward, cex = .6)
rect.hclust(hcagnesward, k = 7, border = 2:5)

# Create cluster plot of hcagneward
plotcut4 <- cutree(hcagnesward, k = 4) # cut tree by 4 clusters
fviz_cluster(list(data = hcdat, cluster = plotcut4)) # view cluster plot (for the points out on the tope right of the plot)
describeBy(data, plotcut4)
plotcut7 <- cutree(hcagnesward, k = 7) # cut tree by 7 clusters
fviz_cluster(list(data = hcdat, cluster = plotcut7)) # see that clustering by 7 shows ver little dissimilarity
describeBy(data, plotcut7)

############################################################################################
# K-Means Clustering
############################################################################################

set.seed(345)
mdl1 <- kmeans(data, 4, nstart = 45)
data <- cbind(data, mdl1$cluster)
describeBy(data, mdl1$cluster) # Looking at descriptives

mdl2 <- kmeans(data, 4, nstart = 60) # Testng to see how changing nstart might affect model
data <- cbind(data, mdl2$cluster)
describeBy(data, mdl2$cluster) # There is a difference between mdl1 and mdl2 clustering

mdl3 <- kmeans(data, 4, nstart = 100) # Testng to see how changing nstart might affect model
data <- cbind(data, mdl3$cluster)
describeBy(data, mdl3$cluster) # There is a difference between mdl1m, mdl2, and mdl3 clustering

mdl4 <- kmeans(data, 4, nstart = 100, iter.max = 100) # Testng to see how adding iter.max might affect model
data <- cbind(data, mdl4$cluster)
describeBy(data, mdl4$cluster) 

sum_sq <- sapply(1:10, 
                 function(k){
                   kmeans(data, k, nstart=100, iter.max = 100)$tot.withinss
                   }) # create a variable that stores square distances within groups for various k
sum_sq # view stored square distances
plot(1:10, sum_sq, type = "b", pch = 1, xlab = "K", ylab ="Within clusters sum of squares")
# Looks like optimal k is 2

# Find optimal k through other methods
View(data)
kmnsdata <- data[, -c(18:21)] # create datat that does not have added columns of cluster columns
kmnsdata <- sapply(kmnsdata, function(x) as.numeric(as.character(x))) # make all columns numeric
kmnsdata <- scale(kmnsdata) # scaled the data for use
mxdat <- as.matrix(kmnsdata) # make a matrix of data to make it easier to find optimal k through other forms
calinhara.clustering <- kmeansruns(kmnsdata, krange = 1:10, crtierion="ch", scaledata = TRUE)
calinhara.clustering$bestk # finding best k through Calinski-Harabasz Index
asw.clustering <- kmeansruns(mxdata, krange = 1:10, criterion = "asw")
asw.clustering$bestk # finding best k through average silhouette width
fviz_nbclust(kmnsdata, kmeans, method = "wss") # another way to do elbow method
fviz_nbclust(kmnsdata, kmeans, method = "silhouette") # find optimal k through average sum of squares

# Plotting optimal k methods to visually see optimal K
critframe <- data.frame(k=1:10, ch=scale(calinhara.clustering$crit), 
                        asw=scale(asw.clustering$crit))
critframe <- melt(critframe, id.vars=c("k"), variable.name="measure", value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks = 1:10, labels = 1:10)
View(kmnsdata)

# Plotting K-means clusters
KM2 <- kmeans(kmnsdata, centers = 2, nstart = 25) # creating k-means with 2 clusters
KM3 <- kmeans(kmnsdata, centers = 3, nstart = 25) # creating k-means with 3 clusters
KM4 <- kmeans(kmnsdata, centers = 4, nstart = 25) # creating k-means with 4 clusters
KM5 <- kmeans(kmnsdata, centers = 5, nstart = 25) # creating k-means with 5 clusters
kmp2 <- fviz_cluster(KM2, geom = "point", data = kmnsdata) + ggtitle("k = 2") # preparing plot to compare to other k-means
kmp3 <- fviz_cluster(KM3, geom = "point",  data = kmnsdata) + ggtitle("k = 3") # preparing plot to compare to other k-means
kmp4 <- fviz_cluster(KM4, geom = "point",  data = kmnsdata) + ggtitle("k = 4") # preparing plot to compare to other k-means
kmp5 <- fviz_cluster(KM5, geom = "point",  data = kmnsdata) + ggtitle("k = 5") # preparing plot to compare to other k-means
grid.arrange(kmp2, kmp3, kmp4, kmp5, nrow = 2) # plot k-means cluster plots side by side to compare

kmmdl2 <- kmeans(kmnsdata, 2, nstart = 100, iter.max = 100) # Testng with k = 2 based off of elbow model
data <- cbind(data, kmmdl2$cluster)
describeBy(data, kmmdl2$cluster)

kmmdl5 <- kmeans(kmnsdata, 5, nstart = 100, iter.max = 100) # Testng with k = 2 based off of elbow model
data <- cbind(data, kmmdl5$cluster)
describeBy(data, kmmdl5$cluster)

View(data)
