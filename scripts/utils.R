# =========================================================================== #
#  Utility Functions
# =========================================================================== #

# =================================== #
#  plotting
# =================================== #

ggplotMissing <- function(x) {
  #' plots missing values in a dataframe for quick and dirty exploration
  #' purposes.
  #' 
  #' args:
  #'     x: a dataframe
  #'     
  #' outputs:
  #'     the output is a `ggplot` raster plot displaying missing values
  #'     for every observation and variables in the input dataframe, `x`
  
  require(tidyr)
  require(ggplot2)

  x %>% 
    is.na %>%
    as.data.frame() %>%
    mutate(index = row_number()) %>%
    gather(., variable, value, -index)  %>%
    ggplot(data = .,
           aes(x = variable,
               y = index)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / Observations")
}

# =================================== #
#  z-scores
# =================================== #

#' define functions to calculate z-scores for continuous variables

getZscore <- function(x, ...) {
  #' calculate a z-score for an input variable, `x`. this function can handle
  #' missing values by passing 'na.rm=T'.
  #' 
  #' args:
  #'     x: a vector of numeric values
  #' returns:
  #'    a z-score
  
  stopifnot(is.numeric(x))
  score <- (x - mean(x, ...)) / sd(x, ...)
  return(score)
}

getRobustZscore <- function(x, ...) {
  #' calculate a robust z-score for an input variable, `x`, where distance is
  #'     calculated by taking the deviation of `x` from the median of `x` and
  #'     dividing by the median absolute deviaition `mad`. this function can
  #'     handle missing values by passing 'na.rm=T'.
  #' 
  #' args:
  #'     x: a vector of numeric values
  #' returns:
  #'    a robust z-score

  stopifnot(is.numeric(x))
  score <- (x - median(x, ...)) / mad(x, ...)
  return(score)
}
