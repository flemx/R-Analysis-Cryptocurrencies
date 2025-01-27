
####-------  Start - install and setup packages -------####

#  rvest for web scraping
install.packages('rvest')
library(rvest)

#  tidyverse & gridExtra for data manipulation & visualization
install.packages("tidyverse")
install.packages("gridExtra")
library(tidyverse)
library(gridExtra)

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
api.prices_text <- content(api.prices, "text")
api.prices_json <- fromJSON(api.prices_text, flatten = TRUE)
api.prices_df <- as.data.frame(api.prices_json)

#  Create a single data frame with formatted data for easier data visualization and  correct data types

returnNewDf <- function(index){
  
  #  Get coin and store dataframe
  temp.coin <- api.prices_df$baseCurrency[index]
  temp.df <- api.prices_df[index,][[4]][[1]]
  
  # Get length and store coin name as new column
  temp.rownumber <- length(temp.df[,1])
  temp.df$coin <- rep(c(temp.coin),each=length(temp.rownumber))
  temp.df$date <- as.Date(temp.df$date)
  
  #  To calculate the price value of max in terms of percentage add new column with percentages of close 
  temp.df$price.percentage <- temp.df$close/max(temp.df$close)
  
  return(temp.df)
}

#  Add first data frame, then loop to add the rest
crypto.all <- returnNewDf(1)
for(i in 1:length(api.prices_df[,1])) {
  crypto.all <- rbind(
    crypto.all,returnNewDf(i)
    
  )
}



#  Now we can access all coins from the same data frame with the different unique coins
unique(crypto.all[c('coin')])

print(
  head(filter(crypto.all, coin=='btc'))
)
print(
  head(filter(crypto.all, coin=='eth'))
)

####-------  Start - Visualizing data  -------####


# Plot a line graph for all coin prices
cPrice1 <- ggplot(filter(crypto.all, coin=='btc'), aes(x=date,y=close)) + geom_line()
cPrice2 <- ggplot(filter(crypto.all, coin=='eth'), aes(x=date,y=close)) + geom_line()
cPrice3 <- ggplot(filter(crypto.all, coin=='link'), aes(x=date,y=close)) + geom_line()
cPrice4 <- ggplot(filter(crypto.all, coin=='xrp'), aes(x=date,y=close)) + geom_line()
grid.arrange(cPrice1 , cPrice2, cPrice3, cPrice4, nrow = 2)

# Compare the different coins on the percentage growth
ggplot(crypto.all, aes(x=date,y=price.percentage)) + geom_line(aes(color=coin))       


# Compare  frequency & volume in USD of trades over the last year

cTradeComp <- ggplot(crypto.all, aes(x=date,y=tradesDone)) + geom_line(aes(color=coin))
cVolComp <- ggplot(crypto.all, aes(x=date,y=volumeNotional)) + geom_line(aes(color=coin))
grid.arrange(cTradeComp , cVolComp, nrow = 2)

# Compare total frequency & volume of trade 

cTradeTotal <-ggplot(crypto.all) + geom_bar(aes(y=tradesDone, x=coin, fill=factor(coin)), stat="identity")
cVolTotal <- ggplot(crypto.all) + geom_bar(aes(y=volumeNotional, x=coin, fill=factor(coin)), stat="identity")
grid.arrange(cTradeTotal , cVolTotal, nrow = 1)



# TODO: Compare which coin made the largest increase in percentage 
   



