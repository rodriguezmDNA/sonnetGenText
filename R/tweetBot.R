#install.packages("twitteR")
library("twitteR")
#### Keys to connect to Twitter
## https://developer.twitter.com/en
source("botKeys.R") 
# consumerKey='#####' 
# consumerSecret='####'
# accessToken='#####'
# accessTokenSecret='#####'

source("sonnetFunction.R") 


getSonnet <- function(){
  tot = 0
  count = 0
  while (tot < 100 | tot > 110 | count > 100){
    outQuote <- makeSonnet()
    tot <- sum(nchar(outQuote)) 
    count = count + 1
  }
  
  outQuote[1] <- capitalizeQuote(outQuote[1])
  MarkQuote <- paste(outQuote,collapse = " ")
  return (MarkQuote)
}

verse <- getSonnet()

# print ("HMM output:")
# cat (verse)

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
tweet(verse)
