##Takes a legend with possibly too many entries and 
##returns a smaller representation
###Input: Column of Date Attributes
######Output: Named Vector of Sizes
legendBreaks <- function(x){
  ##get up to seven evenly-spaced(roughly) entries for display
  ##possible value are unique entries
  numLegendValues = 7 
  breaks=c()
  possVal = sort(unique(x))
  positions = trunc(seq(from=1, to = length(possVal), length.out = numLegendValues) + 0.5)
  for (i in positions) {
    breaks = c(breaks,x[i])
  }
#  breaks <- c(min(x),median(x),max(x))
  names(breaks) <- attr(breaks,"labels")
  breaks
}