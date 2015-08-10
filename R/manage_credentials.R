
# Just a wrapper to set up twitteR oath
load_twitter_oath <- function(consumer_key,consumer_secret,access_token,access_secret){
  library(twitteR)
  setup_twitter_oauth(consumer_key,
                      consumer_secret,
                      access_token,
                      access_secret)
}

# Just a wrapper to load bit.ly oath
load_bitly_oath <- function(oath){
  library(RBitly)
  rbitlyApi(oath)
}
