ratingsIBCF <- read.csv("ratings_group5.csv")
moviesIBCF <- read.csv("movies_group5.csv")
setDT(ratingsIBCF)
setDT(moviesIBCF)

# Filtering users who rated less than 10 movies

frequent_users <- ratingsIBCF[ratingsIBCF$userId %in% names(which(table(ratingsIBCF$userId) > 10)), ]

# Removing movies less than 10 ratings

ratingsIBCF <- frequent_users[frequent_users$movieId %in% names(which(table(frequent_users$movieId) > 10)), ]
ratingsIBCF = ratingsIBCF[,-1]
moviesIBCF_rate <- ratingsIBCF[, .(avg_rating = mean(rating)), by = movieId]
moviesIBCF_rate <- inner_join(moviesIBCF,
                          moviesIBCF_rate)


#Splitting data into test & train

data = ratingsIBCF
data$timestamp = NULL
spl = sample.split(data$rating, 0.7)
train = subset(data, spl == TRUE)
test = subset(data, spl == FALSE)


#Table of users by average rate by genre
s <- str_split(moviesIBCF$genres, "\\|")
moviesIBCF_enrich <- data_frame(movieID = rep(moviesIBCF$movieId, sapply(s, length)),
                            title = rep(moviesIBCF$title, sapply(s, length)),
                            genre = unlist(s))
moviesIBCF_enrich$genre <- as.factor(moviesIBCF_enrich$genre)
setDT(moviesIBCF_enrich)
setnames(moviesIBCF_enrich, "movieID", "movieId")

users_clusters <- inner_join(train, moviesIBCF_enrich)
users_clusters$movieId=NULL
users_clusters$title <- NULL
users_clusters$rating <- as.integer(users_clusters$rating)

users_clusters = users_clusters %>%
  pivot_wider(names_from = genre,
              values_from = rating,
              values_fn = mean,
              values_fill = 0)

#Table of for clusters users by average rate by genre
users_clusters2 = users_clusters
users_clusters2$userId = NULL

#Clusters
print("predict cluster")
km<-kmeans(users_clusters2,14,nstart=25)
km

#add clusters value to the table of users and average rates by genre
users_clusters$cluster <- factor(km$cluster)
users_clusters$cluster <- as.integer(users_clusters$cluster)


#Extract from training set users from a specific cluster 
setDT(users_clusters)
uc <- users_clusters[,.(userId,cluster)]

train <- left_join(train, uc)

setDT(train)

for(i in 1:14) { 
  nam <- paste("train_c", i, sep = "")
  assign(nam, train[cluster == i])
  
}


train_c1$cluster <- NULL
train_c2$cluster <- NULL
train_c3$cluster <- NULL
train_c4$cluster <- NULL
train_c5$cluster <- NULL
train_c6$cluster <- NULL
train_c7$cluster <- NULL
train_c8$cluster <- NULL
train_c9$cluster <- NULL
train_c10$cluster <- NULL
train_c11$cluster <- NULL
train_c12$cluster <- NULL
train_c13$cluster <- NULL
train_c14$cluster <- NULL


#### Prediction #####
recommendIBCF<-function(userid){
  if(userid%in%test$userId==FALSE){
    return(paste("This userID doesn't match any registered account."))
  }else{
  
  users_predict <- inner_join(test, moviesIBCF_enrich)
  users_predict$movieId=NULL
  users_predict$title <- NULL
  users_predict$rating <- as.integer(users_predict$rating)
  
  users_predict = users_predict %>%
    pivot_wider(names_from = genre,
                values_from = rating,
                values_fn = mean,
                values_fill = 0)
  setDT(users_predict)
  
  
  
  u_predict = users_predict[userId == userid]
  u_predict$userId = NULL
  
  closest.cluster <- function(x) {
    cluster.dist <- apply(km$centers, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }
  
  
  clusters2 <- apply(u_predict, 1, closest.cluster)
  
  u_predict = test[userId == userid]
  
  if (clusters2==1){clustertocheck<-train_c1
  }else if (clusters2==2){clustertocheck<-train_c2
  }else if (clusters2==3){clustertocheck<-train_c3
  }else if (clusters2==4){clustertocheck<-train_c4
  }else if (clusters2==5){clustertocheck<-train_c5
  }else if (clusters2==6){clustertocheck<-train_c6
  }else if (clusters2==7){clustertocheck<-train_c7
  }else if (clusters2==8){clustertocheck<-train_c8
  }else if (clusters2==9){clustertocheck<-train_c9
  }else if (clusters2==10){clustertocheck<-train_c10
  }else if (clusters2==11){clustertocheck<-train_c11
  }else if (clusters2==12){clustertocheck<-train_c12
  }else if (clusters2==13){clustertocheck<-train_c13
  }else if (clusters2==14){clustertocheck<-train_c14}
  
  u_predict = rbind(clustertocheck,
                    u_predict)
  print("training going on")
  r2 <- as(u_predict, "realRatingMatrix")
  
  r_ibcf_2<- Recommender(r2[1:700], method = "IBCF")
  
  p1 <- as(u_predict, "realRatingMatrix")
  
  print("prediction on his way")
  recom <- predict(r_ibcf_2, p1, n=5, type = "topNList")
  
  recom@items[[as.character(userid)]]
  recom_moviesIBCF <- as.data.frame(recom@items[[as.character(userid)]])
  
  setDT(recom_moviesIBCF)
  setnames(recom_moviesIBCF, , "movieId")
  recom_moviesIBCF
  
  recom_moviesIBCF <- inner_join(moviesIBCF, recom_moviesIBCF)
  
  recom_moviesIBCF <- inner_join(recom_moviesIBCF, moviesIBCF_rate)
  return(recom_moviesIBCF[,2])
  }
}

##recommendIBCF(85403)
