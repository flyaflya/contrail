##Takes column and provides named color vector
##for use with ggplot scale_color_manual
###Input: Column of Date Attributes
###       Hue on HCL Scale (Value Between 0 and 360) 0 yields red, 120 yields green 240 yields blue
###Output: Data Frame
library("colorspace")  ## package for perceptually-based color pallettes
library("dplyr")
dateColumnToColor = function(column, colorHue = 0, power = 3) {
  ##grab the first six of the twelve colors
  ##to preserve the ability for reader to distinguish hue
  colorScaleVector = sequential_hcl(n = 12, h = colorHue, power = power)[1:6]
  ## create function that interpolates between the six colors
  ##rescale to rgb values between 0 and 1 (instead of 255)
  coloringFunction = colorRamp(colorScaleVector, space = "Lab")
  ##create named vector data frame
  namedColorVectorDF = data.frame(dateValue = unique(column)) %>% 
    mutate(scaledDateValue = rescale(dateValue, to = c(1,0))) %>%
    mutate(dateValueColor = rgb(coloringFunction(scaledDateValue)/255))
  namedColorVector = namedColorVectorDF$dateValueColor
  names(namedColorVector) = namedColorVectorDF$dateValue
  return(namedColorVector)
}

