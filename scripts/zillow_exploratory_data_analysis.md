# Zillow Data Explore
Carlos Blancarte  
`r format(Sys.Date(), "%B %d, %Y")`  

# intro

let's do some exploratory analysis on each of the two datasets provided
by zillow. We'll break it up by file.  

- train_2016: this is a list of `parcelid`s that sold and the date in which
they sold. `parcelid` will be our join key to the `properties` dataset.
- properties: this dataset contains all of the features relating to homes
in the three counties in LA (Los Angeles, Orange and Ventura)

# rules n stuff
You are asked to predict 6 time points for all properties:  

  - October 2016 (201610)
  - November 2016 (201611)
  - December 2016 (201612)
  - October 2017 (201710)
  - November 2017 (201711)
  - December 2017 (201712)
  


```r
# libraries
library(tidyverse)
library(ggthemes)
library(scales)
library(ggmap)
library(lubridate)
library(readxl)
library(knitr)
library(DT)

# read in the datasets
training2016 <- read_csv("~/Documents/code/zillow/data/train_2016.csv")
properties <- read_csv("~/Documents/code/zillow/data/properties_2016.csv")
dataDictTabs <- excel_sheets("~/Documents/code/zillow/data/zillow_data_dictionary.xlsx")
dataDict <- purrr::map(dataDictTabs, ~ read_excel("~/Documents/code/zillow/data/zillow_data_dictionary.xlsx" , sheet=.x))
names(dataDict) <- gsub("\\s", "", dataDictTabs)
```

# `training2016`

this dataset shouldn't be too difficult to explore as it only contains
three variables:  

 `parcelid`: a unique home identifier
 
 `logerror`: this is the difference between the log of the zillow estimate
 and the actual sales price. Negative values here mean that the zillow
 model under-estimated the actual sales price, while posititve values
 mean the opposite. More formally, logerror = log(Zestimate) − log(SalePrice)
 
 `transactiondate`: the date in which the the `parcelid` was sold  
 
 begin by checking for missing values, range of values


```r
map_df(training2016, range)
```

```
## # A tibble: 2 × 3
##    parcelid logerror transactiondate
##       <int>    <dbl>          <date>
## 1  10711738   -4.605      2016-01-01
## 2 162960842    4.737      2016-12-30
```

as confirmed on the website, the date range of the sales data stretches
from jan-2016 to december 2016 (one whole year).  


```r
map_df(training2016, ~ sum(is.na(.x)))
```

```
## # A tibble: 1 × 3
##   parcelid logerror transactiondate
##      <int>    <int>           <int>
## 1        0        0               0
```

```r
summary(training2016)
```

```
##     parcelid            logerror        transactiondate     
##  Min.   : 10711738   Min.   :-4.60500   Min.   :2016-01-01  
##  1st Qu.: 11564405   1st Qu.:-0.02630   1st Qu.:2016-04-05  
##  Median : 12553718   Median : 0.00500   Median :2016-06-14  
##  Mean   : 12993598   Mean   : 0.01082   Mean   :2016-06-11  
##  3rd Qu.: 14237199   3rd Qu.: 0.03920   3rd Qu.:2016-08-19  
##  Max.   :162960842   Max.   : 4.73700   Max.   :2016-12-30
```

no missing values ... not really a surprise, but it's great to know.  

next we'll plot a couple of histograms to inspect distributions.


```r
# histogram: logerror
training2016 %>% 
  qplot(logerror, data=.) +
  theme_tufte()
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

those are some long tails we're dealing with here. but most of the
errors hover around 0.  

we are being asked to make predictions for each listing for a series
of months. It would be good to know if the there was a particular
month where `logerror` was exceptionally big/small.  

what we find (in the aggregate) is that the model struggled more in the
fall/winter months compared to spring/summer. 


```r
training2016 %>%
  mutate(transaction_month = month(transactiondate, label=TRUE)) %>%
  group_by(transaction_month) %>%
  summarise(monthly_logerror = mean(logerror),
            n = n()) %>%
  qplot(transaction_month, monthly_logerror, geom='col', data=., fill=n) +
  ggtitle('monthly mean log error', subtitle='colored by number of transactions used to calculate mean') +
  theme_tufte()
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

and a boxplot confirms this to be the case


```r
training2016 %>%
  mutate(transaction_month = month(transactiondate, label=TRUE)) %>%
  qplot(data=., transaction_month, logerror, geom='boxplot') +
  theme_tufte()
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

lastly, we'll just plot the transaction date by logerror, including
blue bands for the top and bottom 1% of logerror.  

again, we find that the model tends to over estimate home values
with more regularity during the winter months. Specifically, 
some of the most extreme values occur during the first four months of
the year.

I'd like to get more information about these specific points, and
in order to do so would involve joining in the property data. i'll
remember to do that at some point.


```r
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
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

switching gears to `transactiondate`:


```r
# histogram: transactiondate
training2016 %>% 
  qplot(transactiondate, data=.) +
  scale_x_date(date_breaks='months') +
  theme_tufte()
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

a majority of transactions occur in the summer months, april - september,
and a steep decline takes place in after October. Why aren't people
buying houses in October-December?  *UPDATE*: this is by design...
transactions for this time period were stripped from the training 
dataset and set aside for testing.  

a couple of caveats here: mainly, `logerror` means are grouped by
`transactiondate`, and there are never more than 17 transactions on
the same day, so take the dramatic peaks with a grain of salt. 


```r
training2016 %>% 
  group_by(transactiondate) %>%
  summarise(mean_logerror = mean(logerror), 
            n = n()) %>%
  qplot(transactiondate, mean_logerror, data=., geom='line', color=n) +
  geom_hline(yintercept = 0, color = 'red') +
  scale_x_date(date_breaks='months') +
  ggtitle("mean logerror over time", subtitle='grouped by transactiondate') +
  theme_tufte()
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

# `properties`

there's a whole going on with the properties data:: mainly, there are
nearly 3 million records! What i'd like to do first is plot some map
data using a combination of `ggmap`, `maps`, and `mapdata` packages. But
first i'll explore `properties` dataset.
we'll beging by subsetting the `properties`data by joining it to
the properties that actually sold, using `parcelid` from the `train2016`
dataset.


```r
# create new dataset
transactions <- inner_join(properties, training2016, by='parcelid')
```

right off the bat, we're down to 90,811 observations. I'm sure that
the rest of the `properites` data may be useful but for now i'm going to 
focus on the logerror.  

before digging in we're going to calculate some stats to give us information
about the data. Namely, what is the percentage of missing values? How
many unique values exist? Do we have any zero to near zero variance
features?


```r
# calculate meta values
transactionsMeta <- transactions %>%
  gather(variable, value, everything()) %>%
  group_by(variable) %>%
  summarise(
    pct_missing = sum(is.na(value)) / n(),
    num_levels = length(unique(value))
  ) 
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
# calculate nearZeroVar
nzv <- caret::nearZeroVar(transactions, saveMetrics=TRUE) %>%
  rownames_to_column()
names(nzv) <- c('variable', 'freq_ratio', 'percent_unique', 'is_zero_var', 'is_near_zero_var')
```

```r
# join meta vals to nearZeroVar, including data dictionary info
transactionsMeta <- inner_join(transactionsMeta, nzv, by='variable') %>%
  inner_join(., dataDict$DataDictionary %>%
                  mutate(Feature = gsub("'","", Feature)),
             by=c('variable'='Feature')
)

# plot missing values
transactionsMeta %>%
  qplot(reorder(variable, pct_missing), pct_missing, data = . , geom = 'col') +
  coord_flip() +
  scale_y_continuous(label=percent) +
  ggtitle("% Missing Values for Variables in the Transactions Dataset") +
  geom_hline(yintercept = .5, color='red') +
  theme_tufte()
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
# display table
datatable(transactionsMeta)
```

<!--html_preserve--><div id="htmlwidget-7654b8121f35626811b1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7654b8121f35626811b1">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58"],["airconditioningtypeid","architecturalstyletypeid","assessmentyear","basementsqft","bathroomcnt","bedroomcnt","buildingclasstypeid","buildingqualitytypeid","calculatedbathnbr","calculatedfinishedsquarefeet","censustractandblock","decktypeid","finishedfloor1squarefeet","finishedsquarefeet12","finishedsquarefeet13","finishedsquarefeet15","finishedsquarefeet50","finishedsquarefeet6","fips","fireplacecnt","fireplaceflag","fullbathcnt","garagecarcnt","garagetotalsqft","hashottuborspa","heatingorsystemtypeid","landtaxvaluedollarcnt","latitude","longitude","lotsizesquarefeet","numberofstories","parcelid","poolcnt","poolsizesum","pooltypeid10","pooltypeid2","pooltypeid7","propertycountylandusecode","propertylandusetypeid","propertyzoningdesc","rawcensustractandblock","regionidcity","regionidcounty","regionidneighborhood","regionidzip","roomcnt","storytypeid","structuretaxvaluedollarcnt","taxamount","taxdelinquencyflag","taxdelinquencyyear","taxvaluedollarcnt","threequarterbathnbr","typeconstructiontypeid","unitcnt","yardbuildingsqft17","yardbuildingsqft26","yearbuilt"],[0.683067029324641,0.997125898844854,0.00590236865577959,0.999526489081719,0.00590236865577959,0.00590236865577959,0.999823809890872,0.368314411249738,0.018918412967592,0.0131812225391197,0.0125645571571726,0.992754181762121,0.92450253823876,0.0574269636938256,0.999636607899924,0.960753653191794,0.92450253823876,0.995363997753576,0.00590236865577959,0.894208851350607,0.997555362235853,0.018918412967592,0.670337293940161,0.670337293940161,0.973956899494555,0.38245366750724,0.00591338053760007,0.00590236865577959,0.00590236865577959,0.117672969133695,0.773485590952638,0,0.802876303531511,0.989329486515951,0.987215205206418,0.986741694288137,0.816134609243374,0.00591338053760007,0.00590236865577959,0.357864135402099,0.00590236865577959,0.0257567915781128,0.00590236865577959,0.603440111880719,0.00628778451949654,0.00590236865577959,0.999526489081719,0.0100868837475636,0.00596843994670249,0.980365814714076,0.980365814714076,0.00591338053760007,0.867758311217804,0.996707447335675,0.35742366012928,0.970862560702999,0.998953871227054,0.0142273513120657],[7,7,2,40,24,18,2,9,23,5103,42399,2,1887,4983,12,1916,1899,361,4,6,2,15,16,871,2,13,57067,73313,71901,20017,5,90682,2,274,2,2,2,78,15,1997,42671,178,4,495,389,18,2,55451,85111,2,12,55939,5,4,11,568,74,131],[14.5488270594654,13.8125,0,1.5,1.83164544269528,1.58549894887507,0,1.22949788162255,1.83164544269528,1.05027932960894,1.01711795829443,0,1,1.04545454545455,5.66666666666667,1.25,1,1.25,2.39028769638849,7.38245931283906,0,2.12193962443546,3.28522233867345,9.49381443298969,0,2.46813583349443,1.32530120481928,1.63414634146341,1.07894736842105,2.41561181434599,1.49378418697166,1.5,0,2.5609756097561,0,0,0,2.00520054605734,2.65776901161517,2.74116387004641,1.15068493150685,6.853,2.39028769638849,1.02384105960265,1.01108647450111,13.0207360358677,0,1.21428571428571,1.3,0,1.21235521235521,1,132.355555555556,148,23.1533559898046,1.27586206896552,2,1.09863945578231],[0.00660712909229058,0.00660712909229058,0.00110118818204843,0.0429463390998888,0.0253273281871139,0.0187201990948233,0.00110118818204843,0.00880950545638744,0.0242261400050655,5.61826210481109,0.276398233694156,0.00110118818204843,2.07684091134334,5.48611952296528,0.0121130700025327,2.10877536862274,2.09005516952792,0.396427745537435,0.00330356454614529,0.00550594091024215,0.00110118818204843,0.015416634548678,0.0165178227307265,0.958033718382134,0.00110118818204843,0.0132142581845812,62.8404047967757,80.7303080023345,79.1754302892821,22.0413826518814,0.00440475272819372,99.8579467245158,0.00110118818204843,0.300624373699221,0.00110118818204843,0.00110118818204843,0.00110118818204843,0.0847914900177291,0.015416634548678,2.19797161136867,46.9876997280065,0.194910308222572,0.00330356454614529,0.543986961931925,0.427261014634791,0.0187201990948233,0.00110118818204843,61.0608846945855,93.7221261741419,0.00110118818204843,0.0121130700025327,61.5982645274251,0.00440475272819372,0.00330356454614529,0.0110118818204843,0.62437369922146,0.0803867372895354,0.143154463666296],[false,false,true,false,false,false,true,false,false,false,false,true,false,false,false,false,false,false,false,false,true,false,false,false,true,false,false,false,false,false,false,false,true,false,true,true,true,false,false,false,false,false,false,false,false,false,true,false,false,true,false,false,false,false,false,false,false,false],[false,false,true,false,false,false,true,false,false,false,false,true,false,false,false,false,false,false,false,false,true,false,false,false,true,false,false,false,false,false,false,false,true,false,true,true,true,false,false,false,false,false,false,false,false,false,true,false,false,true,false,false,true,true,true,false,false,false],["Type of cooling system present in the home (if any)","Architectural style of the home (i.e. ranch, colonial, split-level, etc…)","The year of the property tax assessment","Finished living area below or partially below ground level","Number of bathrooms in home including fractional bathrooms","Number of bedrooms in home","The building framing type (steel frame, wood frame, concrete/brick)","Overall assessment of condition of the building from best (lowest) to worst (highest)","Number of bathrooms in home including fractional bathroom","Calculated total finished living area of the home","Census tract and block ID combined - also contains blockgroup assignment by extension","Type of deck (if any) present on parcel","Size of the finished living area on the first (entry) floor of the home","Finished living area","Perimeter  living area","Total area","Size of the finished living area on the first (entry) floor of the home","Base unfinished and finished area","Federal Information Processing Standard code -  see https://en.wikipedia.org/wiki/FIPS_county_code for more details","Number of fireplaces in a home (if any)","Is a fireplace present in this home","Number of full bathrooms (sink, shower + bathtub, and toilet) present in home","Total number of garages on the lot including an attached garage","Total number of square feet of all garages on lot including an attached garage","Does the home have a hot tub or spa","Type of home heating system","The assessed value of the land area of the parcel","Latitude of the middle of the parcel multiplied by 10e6","Longitude of the middle of the parcel multiplied by 10e6","Area of the lot in square feet","Number of stories or levels the home has","Unique identifier for parcels (lots)","Number of pools on the lot (if any)","Total square footage of all pools on property","Spa or Hot Tub","Pool with Spa/Hot Tub","Pool without hot tub","County land use code i.e. it's zoning at the county level","Type of land use the property is zoned for","Description of the allowed land uses (zoning) for that property","Census tract and block ID combined - also contains blockgroup assignment by extension","City in which the property is located (if any)","County in which the property is located","Neighborhood in which the property is located","Zip code in which the property is located","Total number of rooms in the principal residence","Type of floors in a multi-story house (i.e. basement and main level, split-level, attic, etc.).  See tab for details.","The assessed value of the built structure on the parcel","The total property tax assessed for that assessment year","Property taxes for this parcel are past due as of 2015","Year for which the unpaid propert taxes were due","The total tax assessed value of the parcel","Number of 3/4 bathrooms in house (shower + sink + toilet)","What type of construction material was used to construct the home","Number of units the structure is built into (i.e. 2 = duplex, 3 = triplex, etc...)","Patio in  yard","Storage shed/building in yard","The Year the principal residence was built"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>variable<\/th>\n      <th>pct_missing<\/th>\n      <th>num_levels<\/th>\n      <th>freq_ratio<\/th>\n      <th>percent_unique<\/th>\n      <th>is_zero_var<\/th>\n      <th>is_near_zero_var<\/th>\n      <th>Description<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

there are *A* *LOT* of missing values in this dataset. In fact, several
variables are very close to having 100% missing values. Variables like
`buildingclasstypeid`, `finishedsquarefeet13`, `basementsqft`,
`storytypeid`, `fireplaceflag` all hover around 99%. However, it's a bit
unclear whether any of the missing values can be safely inputed to say, 0,
or some other value.  

Take `fireplaceflag` for example - it contains only two levels:
`true` and `NA`. Can we safely assume that missing values *should* have been
 labeled as `false`/`0`?  
 
 here's something that's troubling - `fireplaceflag` and `fireplacecnt`
 do not appear to agree in the data. I would expect records where the
 `fireplaceflag` is equal to `true` to have a value for `fireplacecnt`. This
 doesn't appear to be the case:


```r
transactions %>% 
  xtabs(~  fireplacecnt + fireplaceflag, data = . , addNA=TRUE)
```

```
##             fireplaceflag
## fireplacecnt  true  <NA>
##         1        0  8165
##         2        0  1106
##         3        0   312
##         4        0    21
##         5        0     3
##         <NA>   222 80982
```

my conclusion is that the value of `true' should **actually** be `false`, 
and accordingly, recode missing values of `fireplactcnt` to 0.  

what about the rest of the variables? 
## `ggmap`



```r
library(maps)
library(mapdata)
```

*note*: I was having issues plotting any of this data initially and
figured out that the lat/long coordinates seemed to be a little too
big. I'm not sure why they're missing a decimal. In order to confirm
this i pulled out some lat/long data from one of the datasets
included with the `maps` package.


```r
# pull in CA counties data
caCounties <- map_data('county') %>%
  filter(region == 'california') %>%
  tbl_df()

# print
caCounties
```

```
## # A tibble: 2,977 × 6
##         long      lat group order     region subregion
##        <dbl>    <dbl> <dbl> <int>      <chr>     <chr>
## 1  -121.4785 37.48290   157  6965 california   alameda
## 2  -121.5129 37.48290   157  6966 california   alameda
## 3  -121.8853 37.48290   157  6967 california   alameda
## 4  -121.8968 37.46571   157  6968 california   alameda
## 5  -121.9254 37.45998   157  6969 california   alameda
## 6  -121.9483 37.47717   157  6970 california   alameda
## 7  -121.9541 37.47717   157  6971 california   alameda
## 8  -121.9541 37.49436   157  6972 california   alameda
## 9  -121.9999 37.50582   157  6973 california   alameda
## 10 -122.0457 37.52873   157  6974 california   alameda
## # ... with 2,967 more rows
```

```r
# and now a summary
caCounties %>%
  summary()
```

```
##       long             lat            group           order     
##  Min.   :-124.4   Min.   :32.54   Min.   :157.0   Min.   :6965  
##  1st Qu.:-122.3   1st Qu.:36.78   1st Qu.:172.0   1st Qu.:7724  
##  Median :-121.3   Median :38.10   Median :187.0   Median :8483  
##  Mean   :-120.9   Mean   :37.89   Mean   :186.5   Mean   :8483  
##  3rd Qu.:-119.9   3rd Qu.:39.33   3rd Qu.:201.0   3rd Qu.:9241  
##  Max.   :-114.1   Max.   :42.02   Max.   :214.0   Max.   :9998  
##     region           subregion        
##  Length:2977        Length:2977       
##  Class :character   Class :character  
##  Mode  :character   Mode  :character  
##                                       
##                                       
## 
```

notice the min and max values of `lat` and `long`. They don't match up
with what the `properites` data. In order to fix this I'm going to divide
lat/long values by 1000000 in order to have them be on the same scale.  

*update*: the data dictionary clearly states that the lat/long values
were multiple by 1e6. ::rollseyes::


```r
# min/max
summary(transactions[,c('latitude', 'longitude')])
```

```
##     latitude          longitude         
##  Min.   :33339295   Min.   :-119447865  
##  1st Qu.:33811538   1st Qu.:-118411692  
##  Median :34021500   Median :-118173431  
##  Mean   :34005411   Mean   :-118198868  
##  3rd Qu.:34172742   3rd Qu.:-117921588  
##  Max.   :34816009   Max.   :-117554924  
##  NA's   :536        NA's   :536
```

```r
# fix lat/long   
transactions <- transactions %>%
  mutate(
    latitude = latitude / 1e6,
    longitude = longitude / 1e6
  )

# min/max
summary(transactions[,c('latitude', 'longitude')])
```

```
##     latitude       longitude     
##  Min.   :33.34   Min.   :-119.4  
##  1st Qu.:33.81   1st Qu.:-118.4  
##  Median :34.02   Median :-118.2  
##  Mean   :34.01   Mean   :-118.2  
##  3rd Qu.:34.17   3rd Qu.:-117.9  
##  Max.   :34.82   Max.   :-117.6  
##  NA's   :536     NA's   :536
```

that's more like it! Now we can go ahead and grab some maps!  

first off, we can use the `make_bbox` function from `ggmap` to help
us figure out the coordinates we need to fit all our data onto.


```r
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
```

```
## Warning: bounding box given to google - spatial extent only approximate.
```

```
## converting bounding box to center/zoom specification. (experimental)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=34.077652,-118.501395&zoom=9&size=640x640&scale=2&maptype=hybrid&language=en-EN&sensor=false
```

```r
# map
ggmap(myMap) +
  geom_point(
    data=transactions %>%
      filter(logerror > percentileUpper | logerror < percentileLower), 
    aes(longitude, latitude, color=as.factor(regionidcounty))) 
```

```
## Warning: Removed 112 rows containing missing values (geom_point).
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
  scale_colour_gradient2()
```

```
## <ScaleContinuous>
##  Range:  
##  Limits:    0 --    1
```

let's create a plot (showing the logerror) for each of the counties, taking
advantage of the uber-awesome list-columns. 


```r
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
```

```
## Warning: bounding box given to google - spatial extent only approximate.
```

```
## converting bounding box to center/zoom specification. (experimental)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=34.070092,-118.298828&zoom=10&size=640x640&scale=2&maptype=hybrid&language=en-EN&sensor=false
```

```
## Warning: bounding box given to google - spatial extent only approximate.
```

```
## converting bounding box to center/zoom specification. (experimental)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=33.676718,-117.836582&zoom=11&size=640x640&scale=2&maptype=hybrid&language=en-EN&sensor=false
```

```
## Warning: bounding box given to google - spatial extent only approximate.
```

```
## converting bounding box to center/zoom specification. (experimental)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=34.400979,-119.058614&zoom=10&size=640x640&scale=2&maptype=hybrid&language=en-EN&sensor=false
```

```r
byCountyMap$maps
```

```
## [[1]]
```

```
## Warning: Removed 157 rows containing missing values (geom_point).
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```
## 
## [[2]]
```

```
## Warning: Removed 70 rows containing missing values (geom_point).
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```
## 
## [[3]]
```

![](zillow_exploratory_data_analysis_files/figure-html/unnamed-chunk-18-3.png)<!-- -->


---
title: "zillow_exploratory_data_analysis.R"
author: "carlosblancarte"
date: "Sun Jun 18 15:02:54 2017"
---
