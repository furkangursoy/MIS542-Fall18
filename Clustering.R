#install.packages("cluster")
library(cluster)


# data preparation #
setwd("C:\\Users\\admin2\\Desktop\\lab")
auto.df <- read.table("autompg1.csv", header=TRUE, sep=",") #read data from the file
names(auto.df) #see column headers (i.e., variables)
autox.df <- auto.df[,-c(1,7,8)] #create a new data frame storing only the desired variables 

# k-means #
auto.k3m <- kmeans(autox.df, 3, i=50) #run k-means with number of clusters: 3, maximum iterations:50
auto.k3m
names(auto.k3m) 
auto.k3m$iter
auto.k3m$size #cluster sizes
auto.k3m$centers #cluster centers
auto.k3m$cluster #cluster assignments
auto.df$cls_k3m <- auto.k3m$cluster #store cluster assignments in the data frame
table(auto.df$origin, auto.df$cls_k3m) #compare found clusters with manufacturing locations of cars



autox.scaled.df <- scale(autox.df) #z-transform
summary(autox.scaled.df)

auto.scaled.k3m <- kmeans(autox.scaled.df, 3, i=50) #the same procedure, this time with the scaled data
auto.scaled.k3m
auto.scaled.k3m$centers
auto.df$cls_k3m_scaled <- auto.scaled.k3m$cluster
table(auto.df$origin, auto.df$cls_k3m_scaled)

table(auto.df$cls_k3m, auto.df$cls_k3m_scaled) #compare scaled vs non-scaled k-means results

# calculate distance matrix #

# k-medoids #
auto.scaled.pam3 <- pam(autox.scaled.df, 3) #partioning around medoids. accepts either a distance matrix or the data itself
names(auto.scaled.pam3)
auto.scaled.pam3$medoids
auto.df$cls_pam3_scaled <-auto.scaled.pam3$clustering

table(auto.df$cls_pam3_scaled, auto.df$cls_k3m_scaled)

auto.scaled.clara3 <- clara(autox.scaled.df, 3) #clara. similar to pam but suitable for larger datasets. accepts the data itself
auto.scaled.clara3$medoids
auto.df$cls_clara3_scaled <-auto.scaled.clara3$clustering

table(auto.df$cls_pam3_scaled, auto.df$cls_clara3_scaled)


pamk(autox.scaled.df, 2:10) #find number of clusters and perform pam (or clara if usepam=FALSE)
pam(autox.scaled.df, 2) #see that result is the same as above line





#set.seed(100) #be careful since all random numbers will be the same



