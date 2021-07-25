library("plyr")
install.packages("plyr")
library(cluster)
install.packages("cluster")
install.packages("lattice")
install.packages("graphics")
install.packages("grid")
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(grid)
library(graphics)
install.packages("gridExtra")
library(gridExtra)
wine_input = as.data.frame(read.csv("E:/Downloads/Datasets/winequality-red_1.csv"))
kmdata_orig = as.matrix(wine_input[, c("fixed.acidity", "volatile.acidity","citric.acid","residual.sugar","chlorides")])
kmdata <- kmdata_orig[,2:4]
kmdata[1:10,]
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers = k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab= "number of clusters", ylab = "within sum of squares")
km = kmeans(kmdata,3,nstart = 25)
km
c(wss[3], sum(km$withinss))
df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers = as.data.frame(km$centers)

g1 = ggplot(data = df, aes(x=citric.acid, y=volatile.acidity, color=cluster))+
  geom_point()+ theme(legend.position = "right")+
  geom_point(data = centers,
             aes(x=citric.acid,y=volatile.acidity,color=as.factor(c(1,2,3))),
             size = 10, alpha=0.3, show.legend =FALSE)
g2 = ggplot(data = df, aes(x=citric.acid,y=residual.sugar, color=cluster))+
  geom_point()+
  geom_point(data = centers,
             aes(x=citric.acid, y=residual.sugar,color=as.factor(c(1,2,3))),
             size = 10, alpha=0.3, show.legend =FALSE)
g3 = ggplot(data = df, aes(x=volatile.acidity,y=residual.sugar, color=cluster))+
  geom_point()+
  geom_point(data = centers,
             aes(x=volatile.acidity,y=residual.sugar,color=as.factor(c(1,2,3))),
             size = 10, alpha=0.3, show.legend =FALSE)

tmp = ggplot_gtable(ggplot_build(g1))

grid.arrange(arrangeGrob(g1 + theme(legend.position = "none"),
                         g2 + theme(legend.position = "none"),
                         g3 + theme(legend.position = "none")))


