####Take Time Series Object and Turn Into DF
###Input Time Series
###Output: Data Frame

library("dplyr")
library("scales")   ##use for rescaling function
timeSeriestoDF = function(ts) {
  newDF = data.frame(units=as.matrix(ts), date=as.Date(ts)) %>%
    mutate(monthNum = month(date)) %>%
    mutate(Month = month(date, label = TRUE)) %>% 
    mutate(Year = year(date)) %>% 
    mutate(yearAsFactor = as.factor(Year)) %>%
    mutate(scaledAge = rescale(Year, to = c(1,0))) %>%
    mutate(xPosJitterVal = rescale(Year, to = c(-0.07,0))) %>% 
    mutate(monthNumber = month(date)) %>% 
    mutate(xPosJitter = monthNumber+xPosJitterVal) %>% 
    tbl_df()
  return(newDF)
}

