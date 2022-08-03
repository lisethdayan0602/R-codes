library(tidyverse)
library(ggplot2)
library(readr)
library(lm.beta)

bitcoin_data <- read_csv("bitcoin_data.csv")
View(bitcoin_data)
head(bitcoin_data)

btc_stock_commodity_price_FY17_18 <- read_csv("btc_stock_commodity_price_FY17_18.csv")
View(btc_stock_commodity_price_FY17_18)

Bitcoin_Stock <-btc_stock_commodity_price_FY17_18

glimpse(bitcoin_data)

glimpse(Bitcoin_Stock)


names(bitcoin_data)[names(bitcoin_data)=="Volume (BTC)"] <- "Volume.BTC"

names(bitcoin_data)[names(bitcoin_data)=="Volume (Currency)"] <- "Volume.Currency"

names(bitcoin_data)[names(bitcoin_data)=="Weighted Price"] <- "Weighted.Price"

ggplot(bitcoin_data, aes(Open)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(bitcoin_data$Open, na.rm = TRUE), 
                                         sd = sd(bitcoin_data$Open, na.rm = TRUE)), colour = "black", size = 1)

ggplot(bitcoin_data, aes(High)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(bitcoin_data$High, na.rm = TRUE), 
                                         sd = sd(bitcoin_data$High, na.rm = TRUE)), colour = "black", size = 1)
ggplot(bitcoin_data, aes(Low)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(bitcoin_data$Low, na.rm = TRUE), 
                                         sd = sd(bitcoin_data$Low, na.rm = TRUE)), colour = "black", size = 1)
ggplot(bitcoin_data, aes(Close)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(bitcoin_data$Close, na.rm = TRUE), 
                                         sd = sd(bitcoin_data$Close, na.rm = TRUE)), colour = "black", size = 1)
ggplot(bitcoin_data, aes(Volume.BTC)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(bitcoin_data$Volume.BTC, na.rm = TRUE), 
                                         sd = sd(bitcoin_data$Volume.BTC, na.rm = TRUE)), colour = "black", size = 1)

  
ggplot(bitcoin_data, aes(Volume.Currency)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(bitcoin_data$Volume.Currency, na.rm = TRUE), 
                                       sd = sd(bitcoin_data$Volume.Currency, na.rm = TRUE)), colour = "black", size = 1)

ggplot(bitcoin_data, aes(Weighted.Price)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(bitcoin_data$Weighted.Price, na.rm = TRUE), 
                                         sd = sd(bitcoin_data$Weighted.Price, na.rm = TRUE)), colour = "black", size = 1)

head(Bitcoin_Stock)

ggplot(Bitcoin_Stock, aes(btc_price)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(Bitcoin_Stock$btc_price, na.rm = TRUE), 
                                         sd = sd(Bitcoin_Stock$btc_price, na.rm = TRUE)), colour = "black", size = 1)

ggplot(Bitcoin_Stock, aes(oil_price)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(Bitcoin_Stock$oil_price, na.rm = TRUE), 
                                         sd = sd(Bitcoin_Stock$oil_price, na.rm = TRUE)), colour = "black", size = 1)

ggplot(Bitcoin_Stock, aes(close_NVDA)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(Bitcoin_Stock$close_NVDA, na.rm = TRUE), 
                                         sd = sd(Bitcoin_Stock$close_NVDA, na.rm = TRUE)), colour = "black", size = 1)

ggplot(Bitcoin_Stock, aes(x = btc_price, y = oil_price)) + 
  geom_point()


ggplot(Bitcoin_Stock, aes(x = btc_price, y = close_NVDA)) + 
  geom_point()


mean(Bitcoin_Stock$btc_price)
median(Bitcoin_Stock$btc_price)
sd(Bitcoin_Stock$btc_price)
var(Bitcoin_Stock$btc_price)
min(Bitcoin_Stock$btc_price)
max(Bitcoin_Stock$btc_price)
range(Bitcoin_Stock$btc_price)
sum(Bitcoin_Stock$btc_price)
quantile(Bitcoin_Stock$btc_price, probs = c(0.20, 0.50, 0.90, 1))

mean(Bitcoin_Stock$oil_price)
median(Bitcoin_Stock$oil_price)
sd(Bitcoin_Stock$oil_price)
var(Bitcoin_Stock$oil_price)
min(Bitcoin_Stock$oil_price)
max(Bitcoin_Stock$oil_price)
range(Bitcoin_Stock$oil_price)
sum(Bitcoin_Stock$oil_price)
quantile(Bitcoin_Stock$oil_price, probs = c(0.20, 0.50, 0.90, 1))

mean(Bitcoin_Stock$close_NVDA)
median(Bitcoin_Stock$close_NVDA)
sd(Bitcoin_Stock$close_NVDA)
var(Bitcoin_Stock$close_NVDA)
min(Bitcoin_Stock$close_NVDA)
max(Bitcoin_Stock$close_NVDA)
range(Bitcoin_Stock$close_NVDA)
sum(Bitcoin_Stock$close_NVDA)
quantile(Bitcoin_Stock$close_NVDA, probs = c(0.20, 0.50, 0.90, 1))

cor(Bitcoin_Stock[ , c('btc_price', 'oil_price', 'close_NVDA')],
    use='complete')

shapiro.test(Bitcoin_Stock$btc_price)
shapiro.test(Bitcoin_Stock$oil_price)
shapiro.test(Bitcoin_Stock$close_NVDA)



Model1<- lm(btc_price ~ oil_price, data = Bitcoin_Stock)
summary(Model1)

Model2 <- lm(btc_price ~ close_NVDA, data = Bitcoin_Stock)
summary(Model2)

Model3 <- lm(btc_price ~ oil_price + close_NVDA, data = Bitcoin_Stock)
summary(Model3)

AIC(Model1)

AIC(Model2)

AIC(Model3)

