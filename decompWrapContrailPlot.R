###input: time series + s.window argument for stl decomposition
###output:  decomposition of the time series into three separate time series 
###         representing seasonality, trend, and remainder
decompWrapContrailPlot = function(ts, s.window) {
  stlTS = stl(ts, s.window = s.window)
  
  ###create four time series for plot
  Time_Series_Data = ts
  Seasonal_Component = stlTS$time.series[,1]
  Trend_Component = stlTS$time.series[,2]
  Remainder_Component = stlTS$time.series[,3]
  
  ###create four wrap_around_contrail_plots
  plot1 = wrapContrailPlot(Time_Series_Data)
  plot3 = wrapContrailPlot(Seasonal_Component)
  plot2 = wrapContrailPlot(Trend_Component)
  plot4 = wrapContrailPlot(Remainder_Component)
  
  ##output plot
  grid.arrange(plot1,plot2,plot3,plot4, ncol = 2)
}



