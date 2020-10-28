install.packages("cluster",dependencies = TRUE)
install.packages("factoextra",dependencies = TRUE) #This package is for drawing charts for cluster

#load the packages
library(cluster) 
library(factoextra)

setwd("C:/R")
custData<-read.csv("cust_data.csv") #read the data file

head(custData)

custData<-custData[,2:4] #We must not cluster based on the customer number so remove it.

write.csv(custData,"non_scaled_custdata.csv")

scaled_custData<-scale(custData) #scale the data.

write.csv(scaled_custData,"scaled_custdata.csv")

custClusters<-kmeans(scaled_custData,3) #run kmeans with the scaled data and no. of clusters= 3 (the second parameter)

custClusters

custClusters$totss

custClusters$tot.withinss
custClusters$betweenss

custClusters$tot.withinss + custClusters$betweenss

custClusters$withinss

sum(custClusters$withinss)

class(custClusters)

custClusters$cluster #gives the cluster number of the customers

custClusters$tot.withinss #this parameter gives the total  within sum of squared

#########How do I know how many clusters? Draw the Tot_within_SS curve vs No. of Clusters ( Elbow curve)
k_max<-50

tot_wss<-sapply(1:k_max,function(k){kmeans(scaled_custData,k)$tot.withinss})

between_ss<-sapply(1:k_max,function(k){kmeans(scaled_custData,k)$betweenss})

tot_ss<-sapply(1:k_max,function(k){kmeans(scaled_custData,k)$totss})


plot(1:k_max,tot_wss,
     type = "b",xlab="Number of Clusters",ylab="Total Within SS")

plot(1:k_max,between_ss,
     type = "b",xlab="Number of Clusters",ylab="Between SS")

plot(1:k_max,tot_ss,
     type = "b",xlab="Number of Clusters",ylab="Total SS")

fviz_cluster(custClusters,scaled_custData) #visually represent the clusters
fviz_nbclust(scaled_custData,kmeans,method="wss") #gives the optimal 


##########################################################################################################

#################Breakfast Cereals#################################################################
cerealDataOrig<-read.csv("cereal.csv") #read the data file
cerealData<-cerealDataOrig[,-1] #first column is the name of the cereal, not considered for cluster analysis.
                                #the original data is kept in the cerealDataOrig as we will need it later to show the
                                #cluster numbers alongside the names of the cereals

cerealData

cerealData<-scale(cerealData) #scale the data

cerealCluster<-kmeans(cerealData,3) #run kmeans with no. of clusters = 3

cerealCluster

cerealCluster$cluster #gives the cluster number of each breakfast cereal 


######create the elbow curve for this problem.
#We start from 1 cluster to 20 and check which is the optimal value of k

k_max<-20

tot_wss<-sapply(1:k_max,function(k){kmeans(cerealData,k)$tot.withinss})

plot(1:k_max,tot_wss,
     type = "b",xlab="Number of Clusters",ylab="Total Within SS")

##############end of code for elbow curve

##################The following two lines of code draws two charts
fviz_cluster(cerealCluster,cerealData) #visually represent the clusters
fviz_nbclust(cerealData,kmeans,method="silhouette") #gives the optimal 
                                                    #number of clusters from 
                                                    #the silhouette value


########################Wine Data problem###########################

wineData<-read.csv("Wine.csv") #read the data

wineData[is.na(wineData)] <-0 #replace the NAs with 0

winedata.transposed<-t(wineData[,8:107]) #we will be clustering the customers based
                                        #on their responses to the 32 offers
                                        #So we first take the responses of the
                                        #customers (column 8 to 107), and then
                                        #transpose it as the field that we have to 
                                        #group (cluster) must be along the rows.

winedata.transposed

wineCluster<-kmeans(winedata.transposed,5) #run the kmeans and get the clusters

wineCluster #check the details of the cluster


wineCluster$tot.withinss #this gives you the total within SS of the clusters

wineCluster$cluster["Bell"] #this returns the cluster number of the customer named
                            #Bell


write.csv(wineCluster$cluster,"checkCluster.csv") #write the cluster number 
                                                  #of the 100 customers in a file
                                                  #checkCluster.csv. The file is 
                                                  #saved in the working directory

####Now that we have clustered the data, we will have to make sense from it
#we find out how many customers from a certail cluster said Yes (1) to an offer
#So, we will find out 
#how many customers who have been assigned cluster 1 said yes to Offer 1, 2, 3,...32
#how many customers who have been assigned cluster 2 said yes to Offer 1, 2, 3,...32
#how many customers who have been assigned cluster 3 said yes to Offer 1, 2, 3,...32
#how many customers who have been assigned cluster 4 said yes to Offer 1, 2, 3,...32
#how many customers who have been assigned cluster 5 said yes to Offer 1, 2, 3,...32

#aggregate function helps us do it
aggregate(winedata.transposed,by=list(wineCluster$cluster),sum)

#Transpose it
t(aggregate(winedata.transposed,by=list(wineCluster$cluster),sum))

#take only rows 2 to 33
winedata.clusterCounts<-t(aggregate(winedata.transposed,
                                    by=list(wineCluster$cluster),sum))[2:33,]
winedata.clusterCounts

#Add the offer details (present in wineData column 1 to 7) in front
winecluster.Offer<-cbind(wineData[,1:7],winedata.clusterCounts)

winecluster.Offer
