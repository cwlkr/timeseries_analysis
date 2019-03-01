require(TSexploreR)


series =  dt.data %>% filter(objNuc_TrackObjects_Label == 15 & Image_Metadata_Site == 15) %>%
  mutate(nuc_erk = objCyto_Intensity_MeanIntensity_imErk/objNuc_Intensity_MeanIntensity_imErk) %>% select(nuc_erk) %>% melt() %>% pull()
  


calc_derivatives = function(series){
  return(series - lag(series))
}

series_baseline = function(series, stim.time, robust = F){
  before = mean(series[1:stim.time])
  after = mean(series[(length(series) - 9) : (length(series))])
  if(robust){
    before = median(series[1:stim.time])
    after = median(series[(length(series) - 9) : (length(series))])
  }
  
  
  return(list(before = before, 
              after  = after,
              diff = abs(after - before),
              before.sd = sd(series[1:stim.time]),
              after.sd = sd(series[(length(series) - 9) : (length(series))])
  ))
}

dip_amplitude <- function(y, stim.time, baseline){
  # Shift the data to have basal at 0, this is important so that maximum peak is
  # measured in amplitude instead of absolute value
  y <- y - baseline$before
  if(  min(y[stim.time:which.max(y)]) > 0){dip = 0
  } else{dip = min(y[(stim.time + 1):which.max(y)])} # the real dip cannot already happen at stim time either
  return(list(min = abs(dip), time.min = stim.time -1 + which.min(y[stim.time:which.max(y)])))
}

response_delay = function(stim.time, max.amp){
  return( list(delay = abs(as.numeric(names(max.amp$extrema)) - stim.times)))
}


mp_amp_wrapper = function(series, stim.time, baseline, robust = F, basewin = 10){
  
  red = series[basewin : (length(series) - basewin)]
  
  amp = TSexploreR::MPFeatTrend_extrema(red, 5, "maxi", T)
  extrema = amp$extremes
  x.pos  = as.numeric(names(extrema)) + basewin - 1
  names(extrema) <- x.pos
  if (robust){
    l.m = mblm(extrema~x.pos)
  }else {
    l.m = lm(extrema~x.pos)
  }
  return( list(trend = l.m$coefficients[2],
               model = l.m,
               extrema = extrema,
               amplitude = extrema - baseline$before,
               diff = abs((extrema - baseline$before) - lag(extrema - baseline$before))[-1]))
}

all_features_df_multi = function(series, stim.time, basewin = stim.times[1], amp.window = 5){
  
  bline = series_baseline(series, stim.times)
  dip_amplitude(series, stim.time = 10, bline)
  am = mp_amp_wrapper(series, baseline = bline, basewin = basewin)
  
}

add_amp_to_plot = function(max.amp){
  x = rep(as.numeric(names(max.amp$amplitude[1])) , 2)
  y = c(max.amp$extrema[1], max.amp$extrema[1] - max.amp$amplitude[1])
  for(a in  2:(length(max.amp)) ){
    x = c(x, rep(as.numeric(names(max.amp$amplitude[a])) , 2))
    y = c(y, c(max.amp$extrema[a], max.amp$extrema[a] - max.amp$amplitude[a]))
  }
  lines(x, y, col = "green")
}

bline = series_baseline(series, stim.times)
plot(series, type = "l")
add_baseline_to_plot(bline, series_len = length(series))
add_dip_to_plot(dip_amplitude(series, stim.time = 10, bline), series)


basewin = stim.times[1]


am = mp_amp_wrapper(series, baseline = bline, basewin = basewin)
abline(am$model$coefficients)
add_amp_to_plot(am)
response_delay(stim.time = stim.times, am)
