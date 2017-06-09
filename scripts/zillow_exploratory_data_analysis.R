#' ---
#' title: Zillow Data Explore
#' author: Carlos Blancarte
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'  html_document:
#'    keep_md: true
#'    theme: lumen
#'    highlight: kate
#' ---
#' 

#' # intro
#' 
#' let's do some exploratory analysis on each of the two datasets provided
#' by zillow. We'll break it up by file.  
#' 
#' - train_2016: this is a list of `parcelid`s that sold and the date in which
#' they sold. `parcelid` will be our join key to the `properties` dataset.
#' - properties: this dataset contains all of the features relating to homes
#' in the three counties in LA

# libraries
library(tidyverse)
library(readxl)

# read in the datasets
training2016 <- read_csv("~/Documents/code/zillow/data/train_2016.csv")
properties <- read_csv("~/Documents/code/zillow/data/properties_2016.csv")
dataDict <- read_xlsx("~/Documents/code/zillow/data/zillow_data_dictionary.xlsx")

#' # `training2016`
#' 
#' this dataset shouldn't be too difficult to explore as it only contains
#' three variables:  
#' 
#'  `parcelid`: a unique home identifier
#'  
#'  `logerror`: this is the difference between the log of the zillow estimate
#'  and the actual sales price. Negative values here mean that the zillow
#'  model under-estimated the actual sales price, while posititve values
#'  mean the opposite. More formally, logerror = log(Zestimate) − log(SalePrice)
#'  
#'  `transactiondate`: the date in which the the `parcelid` was sold  
#'  
#'  begin by checking for missing values, range of values

map_df(training2016, range)

#' as confirmed on the website, the date range of the sales data stretches
#' from jan-2016 to december 2016 (one whole year).  
map_df(training2016, ~ sum(is.na(.x)))
summary(training2016)

#' no missing values ... not really a surprise, but it's great to know.  
#' 
#' next we'll plot a couple of histograms to inspect distributions.

# histogram: logerror
training2016 %>% 
  qplot(logerror, data=.)
#' those are some long tails we're dealing with here. but most of the
#' errors hover around 0.

# histogram: transactiondate
training2016 %>% 
  qplot(transactiondate, data=.) +
  scale_x_date(date_breaks='months')

#' a majority of transactions occur in the summer months, april - september,
#' and a steep decline takes place in after October. Why aren't people
#' buying houses in October-December?  *UPDATE*: this is by design...
#' transactions for this time period were stripped from the training 
#' dataset and set aside for testing. 

# transactions over time
training2016 %>% 
  qplot(transactiondate, logerror, data=., geom='line')

training2016 %>% 
  group_by(transactiondate) %>%
  summarise(mean_logerror = mean(logerror)) %>%
  qplot(transactiondate, mean_logerror, data=., geom='line') +
  geom_hline(yintercept = 0, color = 'red')
