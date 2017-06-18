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
#' in the three counties in LA (Los Angeles, Orange and Ventura)
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
library(ggthemes)
library(scales)
library(ggmap)
library(lubridate)
library(readxl)
library(knitr)
library(DT)

# source utils
source("~/Documents/code/zillow/scripts/utils.R")

# read in the datasets
training2016 <- read_csv("~/Documents/code/zillow/data/train_2016.csv")
properties <- read_csv("~/Documents/code/zillow/data/properties_2016.csv")
dataDictTabs <- excel_sheets("~/Documents/code/zillow/data/zillow_data_dictionary.xlsx")
dataDict <- purrr::map(dataDictTabs, ~ read_excel("~/Documents/code/zillow/data/zillow_data_dictionary.xlsx" , sheet=.x))
names(dataDict) <- gsub("\\s", "", dataDictTabs)

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
  qplot(logerror, data=.) +
  theme_tufte()

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
  ggtitle('monthly mean log error', subtitle='colored by number of transactions used to calculate mean') +
  theme_tufte()

#' and a boxplot confirms this to be the case
training2016 %>%
  mutate(transaction_month = month(transactiondate, label=TRUE)) %>%
  qplot(data=., transaction_month, logerror, geom='boxplot') +
  theme_tufte()

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
  ) +
  theme_tufte()

#' switching gears to `transactiondate`:

#+ message = FALSE, warning = FALSE
# histogram: transactiondate
training2016 %>% 
  qplot(transactiondate, data=.) +
  scale_x_date(date_breaks='months') +
  theme_tufte()

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
  ggtitle("mean logerror over time", subtitle='grouped by transactiondate') +
  theme_tufte()


#' # `properties`
#' 
#' there's a whole going on with the properties data:: mainly, there are
#' nearly 3 million records! What i'd like to do first is plot some map
#' data using a combination of `ggmap`, `maps`, and `mapdata` packages. But
#' first i'll explore `properties` dataset.

#' we'll beging by subsetting the `properties`data by joining it to
#' the properties that actually sold, using `parcelid` from the `train2016`
#' dataset.

# create new dataset
transactions <- inner_join(properties, training2016, by='parcelid')

#' right off the bat, we're down to 90,811 observations. I'm sure that
#' the rest of the `properites` data may be useful but for now i'm going to 
#' focus on the logerror.  
#' 
#' before digging in we're going to calculate some stats to give us information
#' about the data. Namely, what is the percentage of missing values? How
#' many unique values exist? Do we have any zero to near zero variance
#' features?

# calculate meta values
transactionsMeta <- transactions %>%
  gather(variable, value, everything()) %>%
  group_by(variable) %>%
  summarise(
    pct_missing = sum(is.na(value)) / n(),
    num_levels = length(unique(value))
  ) 

# calculate nearZeroVar
nzv <- caret::nearZeroVar(transactions, saveMetrics=TRUE) %>%
  rownames_to_column()
names(nzv) <- c('variable', 'freq_ratio', 'percent_unique', 'is_zero_var', 'is_near_zero_var')

#+ message = FALSE, warning = FALSE
# join meta vals to nearZeroVar, including data dictionary info
transactionsMeta <- inner_join(transactionsMeta, nzv, by='variable') %>%
  inner_join(., dataDict$DataDictionary %>%
                  mutate(Feature = gsub("'","", Feature)),
             by=c('variable'='Feature')
)

# plot missing values
transactionsMeta %>%
  mutate_if(is.numeric, round(., 3))
  qplot(reorder(variable, pct_missing), pct_missing, data = . , geom = 'col') +
  coord_flip() +
  scale_y_continuous(label=percent) +
  ggtitle("% Missing Values for Variables in the Transactions Dataset") +
  geom_hline(yintercept = .5, color='red') +
  theme_tufte()

# display table
transactionsMeta %>%
  mutate_if(is.numeric, round, digits=3) %>%
  datatable()

#' ## missing-values
#'
#'  there are *A* *LOT* of missing values in this dataset. In fact, several
#' variables are very close to having 100% missing values. Variables like
#' `buildingclasstypeid`, `finishedsquarefeet13`, `basementsqft`,
#' `storytypeid`, `fireplaceflag` all hover around 99%. However, it's a bit
#' unclear whether any of the missing values can be safely inputed to say, 0,
#' or some other value.  
#' 
#' ### fireplace stuff
#' 
#' Take `fireplaceflag` for example - it contains only two levels:
#' `true` and `NA`. Can we safely assume that missing values *should* have been
#'  labeled as `false`/`0`?  
#'  
#'  here's something that's troubling - `fireplaceflag` and `fireplacecnt`
#'  do not appear to agree in the data. I would expect records where the
#'  `fireplaceflag` is equal to `true` to have a value for `fireplacecnt`. This
#'  doesn't appear to be the case:

transactions %>% 
  xtabs(~  fireplacecnt + fireplaceflag, data = . , addNA=TRUE)

#' my conclusion is that the value of `true' should **actually** be `false`, 
#' and accordingly, recode missing values of `fireplactcnt` to 0.  
#' 
#' what about the rest of the variables? are there any other hidden issues?
#' 
#' ### pool stuff
#' 
#' the `transactions` dataset also contains a number of variables related to
#' number of pools, the total squarefeet of pools on property, etc. Let's
#' take a closer look at that group of variables:  
#' 
#'   - `poolcnt`: count of pools on property
#'   - `poolsizesum`: total square footage of all pools on property
#'   - `pooltypeid10`: spa or hot tub
#'   - `pooltypeid2`: pool with spa/hot tub
#'   - `pooltypeid7`: pool *without* hot tub  
#'   
#' we are going to assert that if there is a non-missing value in `poolcnt`
#' then there *should* be a corresponding non-missing value of `poolsizesum`
#' and at least *one* of the three id variables.  
#' 
#' begin by plotting the missing values for all pool variables subset to those
#' where `poolcnt` is not missing:

transactions %>% 
  filter(!is.na(poolcnt)) %>%
  select(matches('pool')) %>%
  ggplotMissing()

#' `pooltypeid10` is probably a dud (all values are missing).
#' Note that records exist where `poolsizesum` is missing even though
#' `poolcnt` is not missing. These might be actual missing values that could
#' be imputed.  
#' 
#' lastly, it looks like `pooltypeid2` and `pooltypeid7` share redundant
#' information. That is, when one variable is missing, the other is not missing.
#' that's not surprising given that one variable designates having a spa/hot tub
#' and the other designates *not* having a hot tub. Consider collapsing those
#' two variables into one binary variable.  
#' 
#' ### three quarter bathrooms
#' 
#' by definition:  
#' > 'Number of 3/4 bathrooms in house (shower + sink + toilet)'  
#' 
#' is it reasonable to assume that records with a missing value of
#' `threequarterbathnbr` can be recoded to 0? I don't think a home
#' **needs** to have a three-quarter bathroom.  
#' 
#' a table of `fullbathcnt` and `threequarterbathnbr` revales that records
#' with missing values of `threequarterbathnbr` have non-missing values of
#' `fullbathcnt`. It is perhaps reasonable then, to assume that missing values
#' of `threequarterbathnbr` exist not because the information is unknown, but
#' because it just wasn't filled in.

transactions %>% 
  xtabs(~ fullbathcnt + threequarterbathnbr, data=., addNA=TRUE)

#' ### garages aren't just for cars, **man**
#' 
#' there are a total of two variables related to garages: `garagecarcnt` and
#' `garagetotalsqft`. The same line of questioning applies - does a missing value
#' of `garagecarcnt` indicate that the record has no garage? For example, if
#' the unit was an apartment or condo with a dedicated parking spot but no
#' garage. are there situations where `garagecarcnt` is missing but
#' `garagetotalsqft` is not?

with(transactions, table(is.na(garagecarcnt), is.na(garagetotalsqft)))

#' NOPE.  
#' 
#' it occured to me that another way to test out this hypothesis is to plot
#' the total square feet of a record by the `garagecarcnt` (the idea being
#' that the total square footage would be smaller for records that do not
#' have a garage)

transactions %>% 
  qplot(
    as.factor(garagecarcnt),
    calculatedfinishedsquarefeet,
    data = . ,
    geom='boxplot') +
  theme_tufte()

#' hmmmm... well, apparently, `0` is already included in `garagecarcnt`. so, 
#' there goes that idea.
#' 
#' ### data cleanin
#' 
#' here we will keep track of the changes/updates made to the transactions
#'  dataset as a result of the exploratory analysis. This includes dropping
#'  variables and recoding variables when justified.

transactions2 <- transactions %>%
  select(
    -buildingclasstypeid
    , -finishedsquarefeet13
    # , -basementsqft *consider making a dummy variable? `has_basement`
    , -storytypeid
    # , - yardbuildingsqft26 * consider making a dummy variable? `has_shed`
    , -decktypeid
    #, -hashottuborspa *consider making a dummy var?
    #, -yardbuildingsqft17 *consider making dummy var: `has_patio`
  ) %>%
  mutate(
    fireplacecnt = ifelse(is.na(fireplacecnt), 0, fireplacecnt),
    fireplaceflag = ifelse(grepl('true', fireplaceflag), FALSE, TRUE),
    poolcnt = ifelse(is.na(poolcnt) & (!is.na(pooltypeid2) | !is.na(pooltypeid7)), 0, poolcnt),
    pooltypeid2 = ifelse(!is.na(poolcnt) & is.na(pooltypeid7), 0, pooltypeid2),
    pooltypeid7 = ifelse(!is.na(poolcnt) & is.na(pooltypeid2), 0, pooltypeid7),
    threequarterbathnbr = ifelse(is.na(threequarterbathnbr) & !is.na(fullbathcnt), 0, threequarterbathnbr),
    has_basement = ifelse(!is.na(basementsqft), 1, 0),
    has_shed = ifelse(!is.na(yardbuildingsqft26), 1, 0),
    has_patio = ifelse(!is.na(yardbuildingsqft17), 1, 0)
  )

#' ## `logerror`

#' ## `ggmap`
#' 
#+ message = FALSE, warning = FALSE
library(maps)
library(mapdata)

#' *note*: I was having issues plotting any of this data initially and
#' figured out that the lat/long coordinates seemed to be a little too
#' big. I'm not sure why they're missing a decimal. In order to confirm
#' this i pulled out some lat/long data from one of the datasets
#' included with the `maps` package.

# pull in CA counties data
caCounties <- map_data('county') %>%
  filter(region == 'california') %>%
  tbl_df()

# print
caCounties

# and now a summary
caCounties %>%
  summary()

#' notice the min and max values of `lat` and `long`. They don't match up
#' with what the `properites` data. In order to fix this I'm going to divide
#' lat/long values by 1000000 in order to have them be on the same scale.  
#' 
#' *update*: the data dictionary clearly states that the lat/long values
#' were multiple by 1e6. ::rollseyes::

# min/max
summary(transactions[,c('latitude', 'longitude')])

# fix lat/long   
transactions <- transactions %>%
  mutate(
    latitude = latitude / 1e6,
    longitude = longitude / 1e6
  )

# min/max
summary(transactions[,c('latitude', 'longitude')])

#' that's more like it! Now we can go ahead and grab some maps!  
#' 
#' first off, we can use the `make_bbox` function from `ggmap` to help
#' us figure out the coordinates we need to fit all our data onto.

# compute bounding box
boundingBox <- make_bbox(
  data=transactions,
  lon=longitude,
  lat=latitude,
  f=.05
)

# now use boundingBox to download a map
myMap <- get_map(
  location=boundingBox,
  maptype='hybrid',
  source='google'
)

# map
ggmap(myMap) +
  geom_point(
    data=transactions %>%
      filter(logerror > percentileUpper | logerror < percentileLower), 
    aes(longitude, latitude, color=as.factor(regionidcounty))) 
  scale_colour_gradient2()
  
#' let's create a plot (showing the logerror) for each of the counties, taking
#' advantage of the uber-awesome list-columns. 

  byCountyMap <- transactions %>%
  mutate(regionidcounty=ifelse(regionidcounty == 2061, 'ventura',
                        ifelse(regionidcounty == 1286, 'orange',
                        ifelse(regionidcounty == 3101, 'los angeles', NA))),
         county = regionidcounty) %>%
  filter(!is.na(regionidcounty)) %>%
  group_by(regionidcounty)  %>%
  filter(logerror > percentileUpper | logerror < percentileLower) %>%
  ungroup() %>%
  nest(-regionidcounty) %>%
  mutate(
    bounding_box = purrr::map(data, ~ make_bbox(data=.x, lon=longitude, lat=latitude)),
    google_map = purrr::map(bounding_box, ~ get_map(location=.x, maptype='hybrid', source='google')),
    maps = purrr::pmap(list(google_map, data, regionidcounty), ~ ggmap(.x) +
                         geom_point(data=.y, aes(longitude, latitude, color=logerror)) +
                         scale_color_gradient2() +
                         ggtitle(paste("logerror map of", .y$county, "county"),
                                 subtitle='logerror subset to quantiles: 1%-99%'))
  )

byCountyMap$maps