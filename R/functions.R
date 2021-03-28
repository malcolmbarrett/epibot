epitwitter_token <- function() {
  rtweet::create_token(
    "epi_twitter_bot",
    consumer_key = Sys.getenv("EPIBOT_CONSUMER_KEY"),
    consumer_secret = Sys.getenv("EPIBOT_CONSUMER_SECRET"),
    access_token = Sys.getenv("EPIBOT_ACCESS_TOKEN"),
    access_secret = Sys.getenv("EPIBOT_ACCESS_SECRET"),
    set_renv = FALSE
  )
}

# read vector of known spammers
get_spammers <- function() {
  tolower(readLines("spammers.txt"))
}

# not in
`%nin%` <- Negate(`%in%`)

mongo_url <- function() {
  user <- Sys.getenv("MONGODB_EPIBOT_USER")
  pw <- Sys.getenv("MONGODB_EPIBOT_PW")
  url <- Sys.getenv("MONGODB_EPIBOT_URL")
  glue::glue("mongodb+srv://{user}:{pw}@{url}")
}

con_epibot <- function() {
  mongolite::mongo(
    "epitweets",
    "epitweets",
    url = mongo_url()
  )
}

pull_mongo_data <- function() {
  mongo_con <- con_epibot()
  withr::defer(mongo_con$disconnect())

  mongo_con$find() %>%
    as_tibble() %>%
    mutate(across(where(is.character), ~na_if(., "NA")))
}

push_mongo_data <- function(new_tweets, old_tweets) {
  if (is_empty(new_tweets)) {
    message("No new tweets")
    return(invisible((list())))
  }

  mongo_con <- con_epibot()
  withr::defer(mongo_con$disconnect())

  new_tweets <- new_tweets %>%
    select(all_of(names(old_tweets)))

  uploaded <- mongo_con$insert(new_tweets)
  message(glue::glue("Uploaded {uploaded$nInserted} new tweet(s)"))

  invisible(uploaded)
}

search_new_tweets <- function(previous_tweets, search_terms) {
  # search since most recent saved tweet id
  since_id <- arrange(previous_tweets, desc(created_at)) %>%
    slice(1) %>%
    pull(status_id)

  # search for up to 100,000 tweets using the hashtag
  epitwitter_tweets <- search_tweets(
    search_terms,
    n = 1e5, verbose = FALSE,
    since_id = since_id,
    retryonratelimit = TRUE,
    include_rts = FALSE,
    token = epitwitter_token()
  )

  if (!is_empty(epitwitter_tweets)) {
    epitwitter_tweets <- distinct(epitwitter_tweets, status_id, .keep_all = TRUE)
  }

  epitwitter_tweets
}

select_tweets <- function(epitwitter_tweets, n_tweets = 8) {
  if (is_empty(epitwitter_tweets)) {
    return(NULL)
  }

  # don't retweet tweets from known spammers
  filtered_tweets <- epitwitter_tweets %>%
    filter(tolower(screen_name) %nin% get_spammers())

  # randomly select tweets
  if (nrow(filtered_tweets) > n_tweets) {
    tweets_to_retweet <- filtered_tweets %>%
      sample_n(n_tweets) %>%
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

  tweets_to_retweet
}

retweet_epibot <- function(epitwitter_tweets, n_tweets = 8) {
  tweets_to_retweet <- select_tweets(epitwitter_tweets, n_tweets = n_tweets)

  if (is.null(tweets_to_retweet)) {
    invisible(list())
  } else {
    walk(
      tweets_to_retweet,
      slow_retweet,
      epitwitter_tweets = epitwitter_tweets
    )
  }
}

slow_retweet <- function(.x, epitwitter_tweets) {
  tweet <- epitwitter_tweets %>%
    filter(status_id == .x) %>%
    pull(text)

  message(glue::glue("Retweeting: {tweet}"))
  post_tweet(
    retweet_id = .x,
    token = epitwitter_token()
  )
  Sys.sleep(20)
}
