moviesAR<-read.csv("movies_group5.csv",stringsAsFactors = FALSE)
ratingsAR<-read.csv("ratings_group5.csv",stringsAsFactors = FALSE)

#change the date to the correct time

ratingsAR$timestamp<-date(as_datetime(ratingsAR$timestamp))

#Clustering
s <- str_split(moviesAR$genres, "\\|")
moviesAR_enrich <- data.frame(movieID = rep(moviesAR$movieId, sapply(s, length)), #!
                            title = rep(moviesAR$title, sapply(s, length)),
                            genre = unlist(s))
moviesAR_enrich$genre <- as.factor(moviesAR_enrich$genre)
levels(moviesAR_enrich$genre)
setDT(moviesAR_enrich)
setnames(moviesAR_enrich, "movieID", "movieId")

users_clusters <- inner_join(ratingsAR, moviesAR_enrich)
users_clusters$movieId=NULL
users_clusters$timestamp <- NULL
users_clusters$title <- NULL
users_clusters$rating <- as.integer(users_clusters$rating)
users_clusters = users_clusters %>%
  pivot_wider(names_from = genre,
              values_from = rating,
              values_fn = mean,
              values_fill = 0)
users_clusters2 = users_clusters
users_clusters2$userId = NULL

#Therefore, the k-means analysis will be conducted for k = 6.
km<-kmeans(users_clusters2,6,nstart=25)
users_clusters$cluster <- factor(km$cluster)
users_clusters$cluster <- as.integer(users_clusters$cluster)
setDT(users_clusters)

#Select one cluster
data_with_clusters<-users_clusters[,c(2,23)] 

myfunction <-function(my_userId){ 
  if(my_userId%in% data_with_clusters$userId==FALSE){
    return(paste("This userID doesn't match any registered account."))
  }else{
    my_cluster<-unique(data_with_clusters[data_with_clusters$userId==my_userId,2])
    my_cluster<-as.vector(unlist(my_cluster)) #my_cluster is a list, let's transform it into integer
    data_with_1_cluster<-data_with_clusters[data_with_clusters$cluster==my_cluster,]
    #Association rules
    data_for_ass.rules<-left_join(data_with_1_cluster,ratingsAR[,c(2:3,5)]) #to get movieId
    data_for_ass.rules<-left_join(data_for_ass.rules,moviesAR[,1:2]) #to get the titles
    data_for_ass.rules<-as.data.frame(data_for_ass.rules)#Change the class 
    all_user_moviesAR<-data_for_ass.rules[data_for_ass.rules$userId==my_userId,] #What are the moviesAR the user watched
    last_movie<-(tail(all_user_moviesAR[order(all_user_moviesAR$timestamp), ])[1,5]) #What are the last movie
    #Association rules
    user_item_matrix <- as(split(data_for_ass.rules[,"title"] ,data_for_ass.rules[,"userId"]), "transactions")
    user_item_matrix
    rules<-rules<-apriori(user_item_matrix,parameter = list(supp=0.00001,conf=0.7,minlen=2,maxlen=2))
    rules<-sort(rules,by="confidence",decreasing=TRUE)
    #Predict 3 moviesAR if the last film is "my_movie"
    top_moviesAR<-subset(rules,rhs %in% last_movie)
    top_moviesAR<-(as(lhs(top_moviesAR), "list")) [1:5]#Extract moviesAR
    top_moviesAR<-data.frame(matrix(unlist(top_moviesAR), nrow=length(top_moviesAR), byrow=T))
    colnames(top_moviesAR)<-"data"
    return(top_moviesAR)
  }
}