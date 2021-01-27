#extracting data
moviesCR <- read.csv("movies_group5.csv",stringsAsFactors=FALSE)
ratingsCR <-read.csv("ratings_group5.csv")
str(moviesCR)



#isolate movie genre
movie_genre <- as.data.frame(moviesCR$genres, stringsAsFactors=FALSE)


# split the differents genre
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) #DataFlair
colnames(movie_genre2) <- c(1:10)
list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,nrow(movie_genre2)+1,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre


#create a sparse matrix with all the column corresponding to a genre
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)


#merge sparse matrix+movie id and title
SearchMatrix <- cbind(moviesCR[,1:2], genre_mat2[])
head(SearchMatrix)  

# clusterise the moviesCR by genre
#wss <- numeric(30)
#for(k in 1:30){
#   wss[k]<-sum(kmeans(SearchMatrix[,c(-1,-2)], centers=k, nstart = 25, iter.max = 50)$withinss)
#}
#plot(1:30, wss, type="b", xlab="Number of Clusters" , ylab="Within Sum of Squares" )

#k=14 seems appropriate
km<-kmeans(SearchMatrix[,c(-1,-2)],14,nstart=25)
km
SearchMatrix$cluster<-km$cluster
######################################
##### function recommend with cosine #####


#to try 
#
recommend2<-function(userid){
  if(userid%in%ratingsCR$userId==FALSE){
    return(paste("This userID doesn't match any registered account."))
  }else{
    setDT(ratingsCR)
    ratingsCR[,Mean:=mean(rating),by=movieId]
    #gather the movie id already watched
    watched<-ratingsCR[userId==userid,list(movieId,rating,timestamp,Mean)]
    #find the lasted watched
    movie_id<-as.integer(watched[watched$timestamp==max(watched$timestamp),][,1])
    #find the cluster of the movie watching
    movie_cluster<-SearchMatrix[SearchMatrix[,1]== movie_id,"cluster"]
    #find the similarity between moviesCR from the same cluster
    essai<-tcosine(SearchMatrix[SearchMatrix$cluster==movie_cluster,c(3:20),drop = FALSE])
    #add row and colum names according to the movide id 
    row.names(essai)<-SearchMatrix[SearchMatrix$cluster==movie_cluster,c(1)]
    colnames(essai)<-SearchMatrix[SearchMatrix$cluster==movie_cluster,c(1)]
    setDT(data.frame(essai))
    #find the closest movie to the one watching inside the cluster
    s<-sort(essai[,colnames(essai)==movie_id],decreasing=TRUE)
    recom<-SearchMatrix[SearchMatrix[,1]%in%as.numeric(names(s[s>0.9])),]
    row.names(recom)<-recom[,1]
    # recommend the moviesCR the closest removing the one already watched
    for(i in 1:dim(recom)[1]){
      if(recom[i,1]%in%watched[,1]){
        recom<-recom[-i,]
      }
    }
    ##sort by year
    annee<-substr(recom$title,unlist(gregexpr(pattern ="\\([0-9][0-9][0-9][0-9]\\)",recom$title)),unlist(gregexpr(pattern ="\\([0-9][0-9][0-9][0-9]\\)",recom$title))+5)
    annee<-gsub("\\(|\\)","",annee)
    annee<-as.integer(annee)
    recommendate<-cbind(recom[,c(1,2)],annee)
    ##sort by average rating
    recommendate<-unique(merge(recommendate,ratingsCR[,c('movieId','Mean')],by="movieId", all.x = TRUE))
    return(recommendate[order(-recommendate[,4],-annee)[1:5],2])
  }
}