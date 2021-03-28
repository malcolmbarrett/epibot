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
  mongo_con <- con_epibot()
  withr::defer(mongo_con$disconnect())

  new_tweets <- new_tweets %>%
    select(all_of(names(old_tweets)))

  uploaded <- mongo_con$insert(new_tweets)
  message(glue::glue("Uploaded {uploaded$nInserted} new tweets"))

  invisible(uploaded)
}
