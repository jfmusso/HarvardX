##################################################
# Install and load required packages.
##################################################

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(groupdata2)) install.packages("groupdata2", 
                                          repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", 
                                        repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", 
                                         repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", 
                                       repos = "http://cran.us.r-project.org")

#################################################
# Import the movielens data into an R data frame.
#################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

###############################################
# Tidy the movielens data frame.
###############################################

# Remove seven ratings with no genres.

movielens <- movielens %>% filter(genres != "(no genres listed)")

# Add two separate variables, "title" and "movie", from movielens$title.

movielens <- extract(data = movielens, col = title, into = c("title", "movie_year"), perl = TRUE, 
               regex = "(.*[^\\(\\d{4}\\)])\\((\\d{4})")

# Convert movielens$timestamp to POSIX date-time values.

class(movielens$timestamp) = c('POSIXt', 'POSIXct')
as_datetime(movielens$timestamp)

# Add the column "week_day" from movielens$timestamp.

movielens <- mutate(movielens, week_day = wday(timestamp))

# Round the date-time values in movielens$timestamp to the nearest hour.

movielens <- mutate(movielens, timestamp_round = round_date(movielens$timestamp, unit = "hour"))

# Add two separate variables, "date" and "time", from movielens$timestamp.

movielens <- separate(movielens, timestamp_round, into = c("date", "time"), perl = TRUE, 
                sep = "\\s", remove = TRUE, convert = FALSE)

# Add three separate variables, "year", "month", and "month_day" from 
# movielens$date.

movielens <- separate(movielens, date, into = c("year", "month", "month_day"), 
                perl = TRUE, remove = FALSE, convert = FALSE)

# Remove extra zeros from movielens$time.

movielens <- extract(movielens, time, into = "hour", perl = TRUE, regex = "(^\\d{2})", 
               remove = TRUE, convert = FALSE)

# Reorder columns.

movielens <- select(movielens, userId, rating, movieId, title, movie_year, genres, timestamp, 
              date, year, month, month_day, week_day, hour)

#################################################################
# Create edx (training) set, validation set. (Note: this process could take a couple of minutes.)
#################################################################

# Partition the movielens data frame. Use 10% of movielens data to make the validation set.

# If using R 3.5 or earlier, use `set.seed(1)`, otherwise ...
# set.seed(1, sample.kind="Rounding")

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userIds, movieIds, and genres in validation set are also in edx set.

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") %>% 
  semi_join(edx, by = "genres")

# Add rows removed from validation set back into edx set.

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

###############################################
# Build a regularized movie, user, and genre effects model that accurately predicts the rating
# given to a particular movie (in a particular genre) by a particular user. The model's RMSE
# must be at or below 0.8649.
###############################################

# Develop a measure of accuracy.

RMSE <- function(true_ratings, pred_ratings){
  sqrt(mean((true_ratings - pred_ratings)^2))
}

# Determine the optimal value of lambda in the penalty term of the model by using k-fold cross-validation 
# with only the edx training set.

# Using the edx training data, create and name 7 equal (or near-equal) subsets (folds) of the data.

set.seed(1)
folds <- fold(edx, k = 7)

v1 <- folds %>% filter(.folds == 1) %>% as.data.frame()
v2 <- folds %>% filter(.folds == 2) %>% as.data.frame()
v3 <- folds %>% filter(.folds == 3) %>% as.data.frame()
v4 <- folds %>% filter(.folds == 4) %>% as.data.frame()
v5 <- folds %>% filter(.folds == 5) %>% as.data.frame()
v6 <- folds %>% filter(.folds == 6) %>% as.data.frame()
v7 <- folds %>% filter(.folds == 7) %>% as.data.frame()

e1 <- rbind(v2, v3, v4, v5, v6, v7)
e2 <- rbind(v1, v3, v4, v5, v6, v7)
e3 <- rbind(v1, v2, v4, v5, v6, v7)
e4 <- rbind(v1, v2, v3, v5, v6, v7)
e5 <- rbind(v1, v2, v3, v4, v6, v7)
e6 <- rbind(v1, v2, v3, v4, v5, v7)
e7 <- rbind(v1, v2, v3, v4, v5, v6)

# Make sure userIds, movieIds, and genres in the validation (testing) subsets 
# (v1, v2, etc.) are also in edx (training) subsets (e1, e2, etc.).

val1 <- v1 %>% 
  semi_join(e1, by = "movieId") %>%
  semi_join(e1, by = "userId") %>% 
  semi_join(e1, by = "genres")

val2 <- v2 %>% 
  semi_join(e2, by = "movieId") %>%
  semi_join(e2, by = "userId") %>% 
  semi_join(e2, by = "genres")

val3 <- v3 %>% 
  semi_join(e3, by = "movieId") %>%
  semi_join(e3, by = "userId") %>% 
  semi_join(e3, by = "genres")

val4 <- v4 %>% 
  semi_join(e4, by = "movieId") %>%
  semi_join(e4, by = "userId") %>% 
  semi_join(e4, by = "genres")

val5 <- v5 %>% 
  semi_join(e5, by = "movieId") %>%
  semi_join(e5, by = "userId") %>% 
  semi_join(e5, by = "genres")

val6 <- v6 %>% 
  semi_join(e6, by = "movieId") %>%
  semi_join(e6, by = "userId") %>% 
  semi_join(e6, by = "genres")

val7 <- v7 %>% 
  semi_join(e7, by = "movieId") %>%
  semi_join(e7, by = "userId") %>% 
  semi_join(e7, by = "genres")

# Add rows removed from validation subsets back into training subsets.

removed <- anti_join(v1, val1)
e1 <- rbind(e1, removed)

removed <- anti_join(v2, val2)
e2 <- rbind(e2, removed)

removed <- anti_join(v3, val3)
e3 <- rbind(e3, removed)

removed <- anti_join(v4, val4)
e4 <- rbind(e4, removed)

removed <- anti_join(v5, val5)
e5 <- rbind(e5, removed)

removed <- anti_join(v6, val6)
e6 <- rbind(e6, removed)

removed <- anti_join(v7, val7)
e7 <- rbind(e7, removed)

# Compute the RMSE for each value of lambda in each of the seven iterations.

mu <- mean(edx$rating)
lambdas <- seq(0, 10, 0.25)

############### 1

rmses1 <- sapply(lambdas, function(lambda){
  b_i <- e1 %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
  b_u <- e1 %>% left_join(b_i, by = "movieId") %>% group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  b_g <- e1 %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
    group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))
  
  predicted_ratings <- val1 %>% left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_g, by = 'genres') %>% 
    mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
  return(RMSE(val1$rating, predicted_ratings))
})

opt_lambda1 <- lambdas[which.min(rmses1)]
opt_lambda1

############### 2

rmses2 <- sapply(lambdas, function(lambda){
  b_i <- e2 %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
  b_u <- e2 %>% left_join(b_i, by = "movieId") %>% group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  b_g <- e2 %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
    group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))
  
  predicted_ratings <- val2 %>% left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_g, by = 'genres') %>% 
    mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
  return(RMSE(val2$rating, predicted_ratings))
})

opt_lambda2 <- lambdas[which.min(rmses2)]
opt_lambda2

############### 3

rmses3 <- sapply(lambdas, function(lambda){
  b_i <- e3 %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
  b_u <- e3 %>% left_join(b_i, by = "movieId") %>% group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  b_g <- e3 %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
    group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))
  
  predicted_ratings <- val3 %>% left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_g, by = 'genres') %>% 
    mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
  return(RMSE(val3$rating, predicted_ratings))
})

opt_lambda3 <- lambdas[which.min(rmses3)]
opt_lambda3

############### 4

rmses4 <- sapply(lambdas, function(lambda){
  b_i <- e4 %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
  b_u <- e4 %>% left_join(b_i, by = "movieId") %>% group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  b_g <- e4 %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
    group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))
  
  predicted_ratings <- val4 %>% left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_g, by = 'genres') %>% 
    mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
  return(RMSE(val4$rating, predicted_ratings))
})

opt_lambda4 <- lambdas[which.min(rmses4)]
opt_lambda4

############### 5

rmses5 <- sapply(lambdas, function(lambda){
  b_i <- e5 %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
  b_u <- e5 %>% left_join(b_i, by = "movieId") %>% group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  b_g <- e5 %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
    group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))
  
  predicted_ratings <- val5 %>% left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_g, by = 'genres') %>% 
    mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
  return(RMSE(val5$rating, predicted_ratings))
})

opt_lambda5 <- lambdas[which.min(rmses5)]
opt_lambda5

############### 6

rmses6 <- sapply(lambdas, function(lambda){
  b_i <- e6 %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
  b_u <- e6 %>% left_join(b_i, by = "movieId") %>% group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  b_g <- e6 %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
    group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))
  
  predicted_ratings <- val6 %>% left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_g, by = 'genres') %>% 
    mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
  return(RMSE(val6$rating, predicted_ratings))
})

opt_lambda6 <- lambdas[which.min(rmses6)]
opt_lambda6

############### 7

rmses7 <- sapply(lambdas, function(lambda){
  b_i <- e7 %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
  b_u <- e7 %>% left_join(b_i, by = "movieId") %>% group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  b_g <- e7 %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
    group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))
  
  predicted_ratings <- val7 %>% left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_g, by = 'genres') %>% 
    mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
  return(RMSE(val7$rating, predicted_ratings))
})

opt_lambda7 <- lambdas[which.min(rmses7)]
opt_lambda7

# Compute the value of optimal lambda using the results of our 7-fold cross-validation.

opt_lambda <- mean(c(opt_lambda1, opt_lambda2, opt_lambda3, opt_lambda4, opt_lambda5, 
                     opt_lambda6, opt_lambda7))
opt_lambda

# Use optimal lambda to compute regularized movie, user, and genre effects.

lambda <- opt_lambda
mu <- mean(edx$rating)

b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu) / (n() + lambda))
b_u <- edx %>% left_join(b_i, by = 'movieId') %>% group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
b_g <- edx %>% left_join(b_i, by = 'movieId') %>% left_join(b_u, by = 'userId') %>% 
  group_by(genres) %>% summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))

# Use the Regularized Movie, User, and Genre Effects Model to predict ratings from the validation set.

predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = 'userId') %>% 
  left_join(b_g, by = 'genres') %>% 
  mutate(pred = mu + b_i + b_u + b_g) %>% .$pred

# Assess the accuracy of the predictions by computing the RMSE of the 
# Regularized Movie, User, and Genre Effects Model.

reg_movie_user_genre_rmse <- RMSE(validation$rating, predicted_ratings)
reg_movie_user_genre_rmse
