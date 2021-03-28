source("R/packages.R")
source("R/functions.R")

# pull previous tweets from mongo -----------------------------------------
previous_tweets <- pull_mongo_data()

# search new tweets
search_terms <- "#epitwitter OR #blackepimatters"
epitwitter_tweets <- search_new_tweets(previous_tweets, search_terms)

# upload new data to mongo ------------------------------------------------
push_mongo_data(epitwitter_tweets, previous_tweets)

# retweet random 8 --------------------------------------------------------
retweet_epibot(epitwitter_tweets, n_tweets = 8)
