source("R/packages.R")
source("R/functions.R")

# pull new tweets ---------------------------------------------------------

## search terms
epitwitter <- ("#epitwitter OR #blackepimatters")

## use since_id from previous search
previous_tweets <- pull_mongo_data()
since_id <- arrange(previous_tweets, desc(created_at)) %>%
  slice(1) %>%
  pull(status_id)

## search for up to 100,000 tweets using the hashtag
epitwitter_tweets <- search_tweets(
  epitwitter,
  n = 1e5, verbose = FALSE,
  since_id = since_id,
  retryonratelimit = TRUE,
  include_rts = FALSE,
  token = epitwitter_token()
)

if (!is_empty(epitwitter_tweets)) {
  epitwitter_tweets <- distinct(epitwitter_tweets, status_id, .keep_all = TRUE)
}

# select tweets to retweet ------------------------------------------------

if (!is_empty(epitwitter_tweets)) {
  # don't retweet tweets from known spammers
  filtered_tweets <- epitwitter_tweets %>%
    filter(tolower(screen_name) %nin% get_spammers())

  # randomly select tweets
  if (nrow(filtered_tweets) > 8) {
    tweets_to_retweet <- filtered_tweets %>%
      sample_n(8) %>%
      arrange(desc(created_at)) %>%
      pull(status_id)

    # always tweet blackepimatters hashtagged tweets
    bem <- filtered_tweets$hashtags %>%
      map_lgl(~ "blackepimatters" %in% tolower(.x))

    bem_ids <- filtered_tweets %>%
      filter(bem) %>%
      pull(status_id)

    tweets_to_retweet <- unique(c(bem_ids, tweets_to_retweet))
  } else {
    tweets_to_retweet <- filtered_tweets$status_id
  }
}

# upload new data to mongo ------------------------------------------------

if (!is_empty(epitwitter_tweets)) {
  push_mongo_data(epitwitter_tweets, previous_tweets)
}

# retweet random 5 --------------------------------------------------------

if (!is.null(tweets_to_retweet)) {
  walk(tweets_to_retweet, function(.x) {
    post_tweet(
      retweet_id = .x,
      token = epitwitter_token()
    )
    Sys.sleep(20)
  })
}
