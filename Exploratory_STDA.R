setwd("C:/Users/james/Desktop/Masters Work/CEGE0042 - Spatial-Temporal Data Analysis and Data Mining")
temp <- read.csv("Data/Temp_China.csv")
colnames(temp)[4:ncol(temp)] <- as.character(c(1951:2002))
station <- paste("sta",1:nrow(temp),sep = "")
temp <- cbind(station,temp)
temp_matrix<-data.matrix(temp[,5:ncol(temp)])

mu = mean(temp_matrix)
mu
sdev = sd(temp_matrix)
sdev

hist(temp_matrix)
abline(v=mu, col="red")
qqnorm(temp_matrix)
qqline(temp_matrix, col="red")

pairs(~LOG+LAT+ALT+rowMeans(temp_matrix),data=temp,
      main="Simple Scatterplot Matrix")

library(scatterplot3d)
library(plot3D)
library(rgl)
scatterplot3d(x=temp$LAT, y=temp$ALT, z=rowMeans(temp_matrix))
scatter3D(temp$LAT, temp$ALT, rowMeans(temp_matrix))
plot3d(temp$LAT, temp$ALT, rowMeans(temp_matrix))

plot(colMeans(temp_matrix), xlab = "Year", ylab = "Temperature", type="l", xaxt="n")
axis(1, at = seq(9, 49, 10), labels=seq(1960, 2000, 10))

library(reshape)
newtemp <- melt(temp, id.vars=1:4, measure.vars=5:ncol(temp))

colnames(newtemp)[5:6] <- c("year", "temperature") 

library(lattice)
station.chosen=c("sta1","sta2","sta3","sta4","sta5","sta6","sta7","sta8","sta9","sta10")
#Create a variable containing just the selected stations
a <- newtemp[station %in% station.chosen,]

xyplot(temperature ~ year | station, xlab = "year", type = "l",
       layout = c(5, 2),
       data=a,
       main = "Temperature in China")

#Create the heatmap:
heatmap(temp_matrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column", margins=c(5,3),xlab="Year",ylab="Station ID", cexCol=1.1,y.scale.components.subticks(n=10))

#Order stations by altitude
temp_order<-temp[order(temp$ALT, decreasing=TRUE),]
temp_ordermatrix<-data.matrix(temp_order[,5:ncol(temp)])
heatmap(temp_ordermatrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column",margins=c(3,3))

levelplot(t(temp_ordermatrix), aspect="fill")

library(ggplot2)
library(OpenStreetMap)
library(raster)

#Get the data from the final year (2002):
data_last <- cbind(temp[1:4],temp$"2002")
# Change the column name:
colnames(data_last)[5]<-"tempvalue"
# Make a proportional symbol of the latest data. Convert latitude and longitude to Mercator projection:
data_last[,2:3] <-projectMercator(data_last$LAT, data_last$LOG)
# Download a map tile:
map <- openmap(c(53.5,73.6),c(15.7,134.7),type= 'esri-topo')

autoplot.OpenStreetMap(map)+ geom_point (data= data_last, aes(x=LOG,y=LAT, color=tempvalue, size=tempvalue))+ ggtitle("Annual Average Temperature in China, 2002")

hist(data_last$ALT, 20)

hist(log(data_last$ALT), 20)