
####-------  Start - install and setup packages -------####

#  rvest for web scraping
install.packages('rvest')
library(rvest)

#  tidyverse for data manipulation & visualization
# install.packages('dplyr')
install.packages("tidyverse")
library(tidyverse)
# library(dplyr)


# httr & jsonlite for rest api
install.packages("httr")
install.packages("jsonlite")
require("httr")
require("jsonlite")

#  rlist to extend list functions
install.packages("rlist")
library(rlist)

####-------  Start - Web scraping and data cleanup  -------####


# Use coinranking.com to get top 50 cryptocurrency coins
crypto.url <- read_html("https://coinranking.com/")

# Get the cryptocurency ticket symbols
crypto.tickers <- crypto.url %>% 
  html_nodes(".profile__subtitle") %>% 
  html_text()

# Get the cryptocurrency names
crypto.name <- crypto.url %>% 
  html_nodes(".profile__link") %>% 
  html_text()

# Get the current price and market cap
crypto.market.cap<- crypto.url %>% 
  html_nodes(".valuta") %>% 
  html_text()

# Clean up data, remove characters and spaces from tickers and names
crypto.tickers <- str_trim(str_remove_all(crypto.tickers, "\n"))
crypto.name <- str_trim(str_remove_all(crypto.name, "\n"))

# Split current price and market cap into 2 separate vectors
crypto.unit.price <- crypto.market.cap[c(TRUE,FALSE)]
crypto.market.cap <- crypto.market.cap[c(FALSE,TRUE)]

#  track which indexes are billion of market cap
crypto.isbil <-  str_detect(crypto.market.cap,"billion")

# Remove all non numeric characters and format to numeric
crypto.unit.price <-  as.numeric(gsub("[^0-9.]", "", crypto.unit.price)) 

# For market cap find billion values and change them into million units
crypto.market.cap <- as.numeric(gsub("[^0-9.]", "", crypto.market.cap))
for(i in 1:length(crypto.isbil)){
     if(crypto.isbil[i]){
         crypto.market.cap[i] <- crypto.market.cap[i] * 1000
     }
}

# Create data frame of scraped and formatted data
crypto.currencies <- data.frame(Cryptocurrency = crypto.name, Ticker = crypto.tickers, Price=crypto.unit.price, market.cap = crypto.market.cap)
print(
  crypto.currencies
)

####-------  Start - Get historical price data from REST API  -------####

#  Setting up base URL for cryptocurrency price history API
api.base <- "https://api.tiingo.com/tiingo/crypto/prices"

# Set currency used for pricing cryptocurrencies
api.currency <- "usd"

#  Retrieve history for top 5 cryptocurrencies, using the tickers from our cryptocurrency data frame
api.tickers <- paste(crypto.currencies$Ticker[1:5],"usd",sep="")
api.tickers <- paste(api.tickers, collapse = ',')
# Start date to retrieve prices in YYYY-MM-DD format, set 1 year from today
api.startdate <- as.POSIXlt(Sys.Date())
api.startdate$year <- api.startdate$year-1
api.startdate <- as.Date(api.startdate)

# set the frequency in which you want data resampled 
api.resampleFreq = "1day"

# API token for authentication  (A free token can be requested at: api.tiingo.com, but you can use mine for now)
api.token = "20b7879009ee4863aef6a4dfd06fb50fb9ea447e"

# Combine variables into GET URL and make call
api.call <- paste(api.base,"?tickers=",api.tickers,"&startDate=",api.startdate,"&resampleFreq=",api.resampleFreq,"&token=",api.token,sep='')
api.prices <- GET(api.call)

#  Format response to JSON
api.prices_text <- content(get_prices, "text")
api.prices_json <- fromJSON(get_prices_text, flatten = TRUE)
api.prices_df <- as.data.frame(get_prices_json)

#  Create a single data frame with all data for easier data visualization and format to correct data types

returnNewDf <- function(index){
  
  #  Get coin and store dataframe
  temp.coin <- get_prices_df$baseCurrency[index]
  temp.df <- get_prices_df[index,][[4]][[1]]
  # Get length and store coin name as new column
  temp.rownumber <- length(temp.df[,1])
  temp.df$coin <- rep(c(temp.coin),each=length(temp.rownumber))
  temp.df$date <- as.Date(temp.df$date)
  return(temp.df)
}

#  Add first data frame, then loop to add the rest
crypto.all <- returnNewDf(1)
for(i in 1:length(api.prices_df[,1])) {
  crypto.all <- rbind(
    crypto.all,returnNewDf(i)
    
  )
}


#
#  Now we can access all coins from the same data frame with the different unique coins
unique(crypto.all[c('coin')])

print(
  head(filter(crypto.all, coin=='btc'))
)
print(
  head(filter(crypto.all, coin=='eth'))
)

####-------  Start - Visualizing data  -------####

#  TO DO: Format Date to better format


