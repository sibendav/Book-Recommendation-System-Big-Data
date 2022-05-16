#importing the library we need to use
library(RMySQL)
library(sqldf)
library(dplyr)
library(ggplot2)
library(recommenderlab)

#connecting to mysql database
connect <- dbConnect(MySQL(), user='simha', password='Sbdf2021!', dbname='nextbook4u', host='localhost')

#importing the tables with sqldf library
#books <- sqldf("SELECT * FROM `bx-books`", con=connect)
#users <- sqldf("SELECT * FROM `bx-users`", con=connect)
#ratings <- sqldf("SELECT * FROM `bx-book-ratings`", con=connect)

#importing the tables with dbSendQuery library
books <- dbSendQuery(connect, "SELECT * FROM `bx-books`")
books <- fetch(books, n=-1)
users <- dbSendQuery(connect, "SELECT * FROM `bx-users`")
users <- fetch(users, n=-1)
ratings <- dbSendQuery(connect, "SELECT * FROM `bx-book-ratings`")
ratings <- fetch(ratings, n=-1)

#removing duplicates
ratings <- unique(ratings)
books <- unique(books)
users <- unique(users)

# removing all the ratings that not exist in users or books 
book <- books[which(books$'ISBN' %in% ratings$'ISBN'),]
user <-users[which(users$'User-ID' %in% ratings$'User-ID'),]

rating <- ratings[which(ratings$'User-ID' %in% users$'User-ID'),]
rating <- ratings[which(ratings$'ISBN' %in% books$'ISBN'),]


#book <- books[which(ratings$'ISBN' %in% books$'ISBN'),]
#ratings$"User-ID" <- as.factor(ratings$"User-ID")
#ratings$'ISBN' <- as.factor(ratings$'ISBN')

#removing users or books outliers 
rating.hist <- table(rating$'User-ID') 
rating.hist <- as.data.frame(rating.hist)
head(rating.hist)

correct.users.index <- which(rating.hist$Freq > 7)

rating.afer.ouliers <- rating.hist[correct.users.index,]
rating.afer.ouliers.3.vars <- rating[which(rating$'User-ID' %in% rating.afer.ouliers$Var1),]#s
rating.afer.ouliers.3.vars <- rating.afer.ouliers.3.vars[which(rating.afer.ouliers.3.vars$'Book-Rating'>0),]
head(rating.afer.ouliers.3.vars)



### books clearings
rating.hist2 <- table(rating$'ISBN') #s
rating.hist2 <- as.data.frame(rating.hist2)
head(rating.hist2)

correct.users.index2 <- which(rating.hist2$Freq > 7 & rating.hist2$Freq< 100)

rating.afer.ouliers2 <- rating.hist2[correct.users.index2,]
rating.afer.ouliers.3.vars2 <- rating.afer.ouliers.3.vars[which(rating.afer.ouliers.3.vars$ISBN %in% rating.afer.ouliers2$Var1),]
#rating.afer.ouliers.3.vars2 <- rating.afer.ouliers.3.vars2[which(rating.afer.ouliers.3.vars2$'ISBN'>0),]
head(rating.afer.ouliers.3.vars2)

# Few checking before running the CF
dim(rating.afer.ouliers.3.vars2)
ratings.df <- rating.afer.ouliers.3.vars2
paste("Number of users", length(unique(ratings.df$'User-ID')))
paste("Number of users", length(unique(ratings.df$ISBN)))

# converting to real rating matrix type
ratings.matrix <- as(ratings.df, "realRatingMatrix")
str(ratings.matrix)




##################################################
#                 _
#                | |
#                | | ___ __  _ __
#                | |/ / '_ \| '_ \
#                |   <| | | | | | |
#                |_|\_\_| |_|_| |_|
#
##################################################
# knn function
.knn <- function(sim, k)
  lapply(1:nrow(sim), FUN = function(i)
    head(order(sim[i,], decreasing = TRUE, na.last = NA), k))


# splitting the data into train 80% and test 20%
eval_sets <- evaluationScheme(
  data = ratings.matrix, method = "split",
  train = 0.8, given = -1,
  goodRating = 6, k = 5
)

str(eval_sets)

# Running the recommender system
eval_recommender.UB <- Recommender(
  data = getData(eval_sets, "train"),
  method = "UBCF", parameter = list(method = "Euclidean", normalize="Z-score")
)

str(eval_recommender.UB)

eval_recommender.UB@model$weighted <- FALSE


######################################################
#                             _ _      _    
#                            | (_)    | |   
#          _ __  _ __ ___  __| |_  ___| |_  
#         | '_ \| '__/ _ \/ _` | |/ __| __| 
#         | |_) | | |  __/ (_| | | (__| |_  
#         | .__/|_|  \___|\__,_|_|\___|\__| 
#         | |
#         |_|
#
######################################################

########## top 10 ####################################
R.UB <- predict(eval_recommender.UB,
                newdata = getData(eval_sets, "known"),
                n = 10,
                type = "ratings"
)

eval_accuracy <- calcPredictionAccuracy(x = R.UB,
                                        data = getData(eval_sets, "unknown"),
                                        byUser = TRUE)

eval_accuracy

R.UB.RECOMMEND <- my.predict(eval_recommender.UB@model,
                          newdata = getData(eval_sets, "known"),
                          n = 10,
                          type = "topNList"
)
library(data.table)
h<-R.UB.RECOMMEND@items[1:50]
R.UB.RECOMMEND.top10 <- as.data.frame(h)
h

##################
### my predict ###
##################

my.predict <- function (model, newdata, n = 10, data = NULL, type = c("topNList", "ratings", "ratingMatrix"), ...)
{
  print(newdata)
  type <- match.arg(type)
  newdata_id <- NULL
  if (is.numeric(newdata)) {
    if (model$sample)
      stop("(EE) User id in newdata does not work when sampling is used!")
    newdata_id <- newdata
    newdata <- model$data[newdata, ]
  }
  else {
    if (ncol(newdata) != ncol(model$data))
      stop("(EE) number of items in newdata does not match model.")
    if (!is.null(model$normalize))
      newdata <- normalize(newdata, method = model$normalize)
  }
  cat('(II) running similarity() calculation\n')
  
  sim <- similarity(newdata, model$data, method = model$method,
                    min_matching = model$min_matching_items,
                    min_predictive = model$min_predictive_items)
  cat('(II) similarity() done\n')
  
  if (!is.null(newdata_id))
    sim[cbind(seq(length(newdata_id)), newdata_id)] <- NA
  
  cat(paste('(II) creating knn with', model$nn ,'neighbors\n'))
  neighbors <- .knn(sim, model$nn)
  cat('(II) knn done\n')
  
  if (model$weighted) {
    cat ('(II) weigh the ratings by similarities\n')
    s_uk <- sapply(1:nrow(sim), FUN = function(i) sim[i, neighbors[[i]]])
    if (!is.matrix(s_uk)) s_uk <- as.matrix(t(s_uk))
    ratings <- t(sapply(1:nrow(newdata), FUN = function(i) {
      r_neighbors <- as(model$data[neighbors[[i]]], "dgCMatrix")
      Cols.1 <- drop(as(crossprod(r_neighbors, s_uk[, i]), "matrix"))
      Cols.2 <- drop(as(crossprod(!dropNAis.na(r_neighbors), s_uk[, i]), "matrix"))
      Cols.1 / Cols.2
    }))
    
    ratings[!is.finite(ratings)] <- NA
    cat ('(II) done weigh the ratings\n')
  }
  else {
    cat ("(II) copy the ratings across the user's knn\n")
    ratings <- t(sapply(1:nrow(newdata), FUN = function(i) {
      r_neighbors <- as(model$data[neighbors[[i]]], "dgCMatrix")
      Cols.1 <- colSums(r_neighbors)
      Cols.2 <- colSums(!dropNAis.na(r_neighbors))
      Cols.1 / Cols.2
    }))
    ratings[!is.finite(ratings)] <- NA
    cat ("(II) copy the ratings ... done\n")
  }
  rownames(ratings) <- rownames(newdata)
  ratings <- new("realRatingMatrix", data = dropNA(ratings),
                 normalize = getNormalize(newdata))
  
  cat ('(II) de-normalize the ratings (back to rating scale)\n')
  ratings <- denormalize(ratings)
  cat ('(II) de-normalize done\n')
  
  returnRatings(ratings, newdata, type, n)
}
 
# running the prediction model
my.predict.model  <- my.predict(eval_recommender.UB@model,
                                                      newdata = getData(eval_sets, "known"),
                                                      n = 10,
                                                      type = "ratings")
# RMSE table output from the prediction model
eval_accuracy_ubcf <- calcPredictionAccuracy(x = my.predict.model,
                                             data = getData(eval_sets, "unknown"),
                                             byUser = TRUE)
View(eval_accuracy_ubcf[,'RMSE'])
v <- as.data.frame(eval_accuracy_ubcf[,'RMSE'])
v1 <- as.data.frame(eval_accuracy[,'RMSE'])
v1 <- na.omit(v1)

# IBCF
eval_sets <- subsetData(eval_sets)
eval_sets@data@data <- eval_sets@data@data[1:100,1:100]
eval_sets@knownData@data <- eval_sets@knownData@data[1:100,1:100]
eval_sets@unknownData@data <- eval_sets@unknownData@data[1:100,1:100]
eval_recommender.IB <- Recommender(
  data = getData(eval_sets, "train"),
  method = "IBCF", parameter = list(method = "Cosine", normalize="Z-score")
)

str(eval_recommender.IB)

eval_recommender.IB@model$weighted <- FALSE
eval_prediction_ibcf <- my.predict(eval_recommender.IB@model,
                                   newdata = getData(eval_sets, "known"),
                                   n = 10,
                                   type = "ratings")

eval_accuracy_ibcf <- calcPredictionAccuracy(x = eval_prediction_ibcf,
                                             data = getData(eval_sets, "unknown"),
                                             byUser = FALSE)

##### saving ####
save(eval_recommender.UB, file = "model.Rdata")