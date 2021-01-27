library(recommenderlab)
library(recommenderlabBX)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(kableExtra)
library(readr)
library(anytime)
library(knitr)
library(klaR)
library(clustMixType)
library(caTools)
library(tidyverse)


movies <- read_csv("movies_group5.csv")
ratings <- read_csv("ratings_group5.csv")

setDT(ratings)
setDT(movies)

ratings = ratings[,-1]

head(ratings)

ratings$timestamp <- anytime(ratings$timestamp)

max(ratings$timestamp)
min(ratings$timestamp)


#Informations about ratings
Number_Ratings = nrow(ratings)
Number_Movies = length(unique(ratings$movieId))
Number_Users = length(unique(ratings$userId))



data = ratings
data$timestamp = NULL

#Split data set ratings into 1 set to train and 1 set to test model
spl = sample.split(data$rating, 0.7)
train = subset(data, spl == TRUE)
test = subset(data, spl == FALSE)


#### Clusters ####





s <- str_split(movies$genres, "\\|")

movies_enrich <- data_frame(movieID = rep(movies$movieId, sapply(s, length)),
                            title = rep(movies$title, sapply(s, length)),
                            genre = unlist(s))


movies_enrich$genre <- as.factor(movies_enrich$genre)

levels(movies_enrich$genre)

setDT(movies_enrich)
setnames(movies_enrich, "movieID", "movieId")

users_clusters <- inner_join(train, movies_enrich)

users_clusters$movieId=NULL
users_clusters$timestamp <- NULL
users_clusters$title <- NULL
users_clusters$cluster <- NULL

users_clusters$rating <- as.integer(users_clusters$rating)


users_clusters = users_clusters %>%
  pivot_wider(names_from = genre,
              values_from = rating,
              values_fn = mean,
              values_fill = 0)



users_clusters2 = users_clusters
users_clusters2$userId = NULL

wss <- numeric(30)

#determine an appropriate value for k
#The option nstart=25 specifies that the k-means algorithm will be repeated 25 times, 
#each starting with k random initial centroids.

for(k in 1:30){
  wss[k]<-sum(kmeans(users_clusters2, centers=k, nstart = 25, iter.max = 50)$withinss)
}

#The Within Sum of Squares metric (wss) is plotted against the respective number of centroids
plot(1:30, wss, type="b", xlab="Number of Clusters" , ylab="Within Sum of Squares" )

#Therefore, the k-means analysis will be conducted for k = 14.
km<-kmeans(users_clusters2,14,nstart=25)
km

users_clusters$cluster <- factor(km$cluster)


users_clusters$cluster <- as.integer(users_clusters$cluster)

hist(users_clusters$cluster, breaks = seq(0,14,1))




