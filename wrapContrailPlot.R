##Given a monthly time series, return a wrap-around plot
wrapContrailPlot = function(ts) {
  tsName = deparse(substitute(ts))
  timeSeriesDF = timeSeriestoDF(ts)
  namedColVector = dateColumnToColor(timeSeriesDF$Year)
  namedSizeVector = dateColumnToSize(timeSeriesDF$Year)
  ggplot(data = timeSeriesDF, aes(x = xPosJitter, y= units)) + 
    geom_point(aes(color = yearAsFactor, size = yearAsFactor, alpha = yearAsFactor)) + 
    scale_color_manual("Year", values = namedColVector, breaks = legendBreaks) +
    scale_size_manual("Year", values = namedSizeVector, breaks = legendBreaks) + 
    scale_x_continuous(breaks = 1:12, labels = levels(timeSeriesDF$Month)) + 
    theme_minimal(base_size = 20) +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend("Year", nrow = 1), alpha = FALSE) +
    ggtitle(paste0(tsName, " ",min(timeSeriesDF$Year),"-", max(timeSeriesDF$Year)))
}

