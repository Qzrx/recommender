# Recommender

library(plyr) 
library(reshape2)

path_to_training_data    <- "data/users.csv"
path_to_advertisers      <- "data/advertisers.csv"
path_to_test_data        <- "data/test.csv"

# Set up our data for later
setup_data <- function(input){
  input <- dcast(input, user_id ~ advertiser_id)
  row.names(input) <- input[,"user_id"]
  input
}

users <- setup_data(read.csv(path_to_training_data))
test  <- setup_data(read.csv(path_to_test_data))

advertisers <- read.csv(path_to_advertisers)

# What's our overall plan?
# For a given user, let's define a measure of similarity between them and all 
# other users.
# How will we define this?
# Let's be simple & naive for now!
#
# Similarity between 2 users is simply the magnitude of the intersection of the
# advertisers they follow. That is,
# weight = length(intersection(u1_advertisers, u2_advertisers))
#
# There are additional fancy things we could do for normalizing this against the
# lengths, etc. but for now, keep-it-simple.
# Once we have a measure of similarity for a user, we count each advertiser they
# follow as a single vote for that advertiser, and multiply that vote by the
# similarity between them and our new user. Thus, a user that has a similarity
# score of 20 counts his votes at 20x weight. A user that has similarity of 50
# counts as 50x, 3 -> 3x, etc. etc.
# We can then sum up each advertiser's vote, and give the highest score as our 
# recommendation. If, through some alignment of planets we have a tie, we'll just
# say the first guy gets nominated.

similarity <- function(user1, user2){
  # Trim NA elements or they count in the intersection!
  user1 <- user1[!is.na(user1)]
  user2 <- user2[!is.na(user2)]
  length(intersect(user1, user2))
}

get_advertiser <- function(user_data, new_user){
  # First, add a similarity measure to our user_data
  similarity <- daply(user_data, .(user_id), .fun=function(input){similarity(input, new_user)})

  # Now that we have our similarities, calculate our column-wise sums.
  sum2 <- function(input, weights){sum(input*weights, na.rm=TRUE)}
  colsum <- colwise(sum2)
  user_data <- replace(user_data, !is.na(user_data), 1)
  votes <- colsum(user_data[,-which(names(user_data)=="user_id")], similarity)
  votes <- votes[setdiff(names(votes), new_user)]
  recommended_advertiser <- which(votes==max(votes))[[1]] # If there are ties, only takes the first  
}

recommender <- function(user_data, test_data){
  test_tuples <- daply(test_data, .(user_id), function(test_user){get_advertiser(user_data, test_user)})
  data.frame(user_id=names(test_tuples), advertiser_id=test_tuples)
}
