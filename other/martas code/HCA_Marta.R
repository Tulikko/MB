
library(ggplot2)

data <- read.csv("~/Desktop/R/Gourniaxrf.csv")

require(graphics)
head(data)
dataselect <- (data1[, c(12, 14, 16, 24, 30, 32, 38)])
head(dataselect)

scaled.dat <- scale(dataselect)
head(scaled.dat)
apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)

###

clusters <- hclust(dist(scaled.dat[, c(1, 2, 3, 4, 5, 6, 7)]), method = "average")
par(mar=c(7,3,3,3)+1 )
plot(clusters, labels=data1[,1], cex=0.6, hang=-1, main="Hierarchical Cluster Analysis", xlab="Distance: Square Euclidean, Cluster Method=Average")

groups <- cutree(clusters, k=5)
rect.hclust(clusters, k=5, border="red")

##WARD

clusters <- hclust(dist(scaled.dat[, c(1, 2, 3, 4, 5, 6, 7)]), method = "ward.D2")
par(mar=c(7,3,3,3)+1 )
plot(clusters, labels=data1[,1], cex=0.5, hang=-1, main="Hierarchical Cluster Analysis", xlab="Distance: Square Euclidean, Cluster Method=Ward")

##SAVE
## dev.copy2pdf(file="Main.pdf", encoding="WinAnsi")
setwd("~/Desktop/R/cluster/HCA")

dev.copy2pdf(file="ClusterHCAward.pdf", encoding="WinAnsi")

###############################################################################

#per logtransfomed
log10(data[, c(12, 14, 16, 24, 30, 32, 38)])

#or
##

# Ward Hierarchical Clustering
data <- read.csv("~/Desktop/R/Rtablexrf.csv")
datag <- read.csv("~/Desktop/R/Gourniaxrf.csv")
datam <- read.csv("~/Desktop/R/Maliaxrf.csv")
data1 <- read.csv("~/Desktop/R/MaliaMu.csv")
data2 <- read.csv("~/Desktop/R/MaliaPalace.csv")
data3 <- read.csv("~/Desktop/R/Maliapi.csv")
datamo <- read.csv("~/Desktop/R/Monastirakixrf.csv")
head(data)
d <- dist(data[ c(12, 14, 16, 24, 30, 32, 38)], method = "euclidean")
head(d)
fit <- hclust(d, method="average") 
head(fit)
plot(fit, labels=data[,1], cex=0.5, hang=-1, main="Hierarchical Cluster Analysis", xlab="Distance: Square Euclidean Cluster Method=Average")
# display dendogram
groups <- cutree(fit, k=5) 
# cut tree into 5 clusters and draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")