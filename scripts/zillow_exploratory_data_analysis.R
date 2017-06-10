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
#' 
#' # rules n stuff
#' You are asked to predict 6 time points for all properties:  
#' 
#'   - October 2016 (201610)
#'   - November 2016 (201611)
#'   - December 2016 (201612)
#'   - October 2017 (201710)
#'   - November 2017 (201711)
#'   - December 2017 (201712)
#'   

#+ message = FALSE, warning = FALSE
# libraries
library(tidyverse)
library(lubridate)
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

#+ message = FALSE, warning = FALSE
# histogram: logerror
training2016 %>% 
  qplot(logerror, data=.)

#' those are some long tails we're dealing with here. but most of the
#' errors hover around 0.  
#' 
#' we are being asked to make predictions for each listing for a series
#' of months. It would be good to know if the there was a particular
#' month where `logerror` was exceptionally big/small.  
#' 
#' what we find (in the aggregate) is that the model struggled more in the
#' fall/winter months compared to spring/summer. 

training2016 %>%
  mutate(transaction_month = month(transactiondate, label=TRUE)) %>%
  group_by(transaction_month) %>%
  summarise(monthly_logerror = mean(logerror),
            n = n()) %>%
  qplot(transaction_month, monthly_logerror, geom='col', data=., fill=n) +
  ggtitle('monthly mean log error', subtitle='colored by number of transactions used to calculate mean')

#' and a boxplot confirms this to be the case
training2016 %>%
  mutate(transaction_month = month(transactiondate, label=TRUE)) %>%
  qplot(data=., transaction_month, logerror, geom='boxplot')

#' lastly, we'll just plot the transaction date by logerror, including
#' blue bands for the top and bottom 1% of logerror.  
#' 
#' again, we find that the model tends to over estimate home values
#' with more regularity during the winter months. Specifically, 
#' some of the most extreme values occur during the first four months of
#' the year.
#' 
#' I'd like to get more information about these specific points, and
#' in order to do so would involve joining in the property data. i'll
#' remember to do that at some point.

# define upper and lower percentile value of logerror
percentileLower <- quantile(training2016$logerror, probs = .01)
percentileUpper <- quantile(training2016$logerror, probs = .99)

#plot
training2016 %>%
  mutate(transaction_month = week(transactiondate)) %>%
  ggplot(data=., aes(x=transactiondate, y=logerror)) +
  geom_point(alpha=.4) +
  geom_hline(yintercept=0, color = 'red') +
  geom_hline(yintercept= percentileLower, color='blue') +
  geom_hline(yintercept= percentileUpper, color='blue') +
  ggtitle(
    label='logerror by transactiondate',
    subtitle=paste(
      'lower bound', names(percentileLower), ":", round(percentileLower,2), '\n',
      'upper bound', names(percentileUpper), ":", round(percentileUpper,2)
      )
    )

#' switching gears to `transactiondate`:

# histogram: transactiondate
training2016 %>% 
  qplot(transactiondate, data=.) +
  scale_x_date(date_breaks='months')

#' a majority of transactions occur in the summer months, april - september,
#' and a steep decline takes place in after October. Why aren't people
#' buying houses in October-December?  *UPDATE*: this is by design...
#' transactions for this time period were stripped from the training 
#' dataset and set aside for testing.  
#' 
#' a couple of caveats here: mainly, `logerror` means are grouped by
#' `transactiondate`, and there are never more than 17 transactions on
#' the same day, so take the dramatic peaks with a grain of salt. 

training2016 %>% 
  group_by(transactiondate) %>%
  summarise(mean_logerror = mean(logerror), 
            n = n()) %>%
  qplot(transactiondate, mean_logerror, data=., geom='line', color=n) +
  geom_hline(yintercept = 0, color = 'red') +
  scale_x_date(date_breaks='months') +
  ggtitle("mean logerror over time", subtitle='grouped by transactiondate')

