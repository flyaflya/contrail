##Takes column and provides named size vector
##for use with ggplot scale_size_manual
###Input: Column of Date Attributes
######Output: Named Vector of Sizes
library("dplyr")
dateColumnToSize = function(column) {
  ##create named vector data frame
  namedSizeVectorDF = data.frame(dateValue = unique(column)) %>% 
    mutate(dateValueSize = rescale(dateValue, to = c(2,6))) 
  namedSizeVector = namedSizeVectorDF$dateValueSize
  names(namedSizeVector) = namedSizeVectorDF$dateValue
  return(namedSizeVector)
}