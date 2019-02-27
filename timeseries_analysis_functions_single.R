# for singlepulse and non noisy signal.
require(tidyverse)
require(TSexploreR)
require(data.table)

calc_derivatives = function(series){
  return(series - lag(series))
}
  
series_baseline = function(series, stim.time, robust = F){
  before = mean(series[1:stim.time])
  after = mean(series[(length(series) - 9) : (length(series))])
  
  return(list(before = before, 
              after  = after,
              diff = abs(after - before),
              before.sd = sd(series[1:stim.time]),
              after.sd = sd(series[(length(series) - 9) : (length(series))])
         ))
}


# only possible if stim.time is known and max amplitude measured correctly
dip_amplitude <- function(y, stim.time, baseline){
  # Shift the data to have basal at 0, this is important so that maximum peak is
  # measured in amplitude instead of absolute value
  y <- y - baseline$before
  if(  min(y[stim.time:which.max(y)]) > 0){dip = 0
  } else{dip = min(y[(stim.time + 1):which.max(y)])} # the real dip cannot already happen at stim time either
  return(list(min = abs(dip), time.min = stim.time -1 + which.min(y[stim.time:which.max(y)])))
}

exp_decay = function(series, stim.time){
  series = single_timeseries_coralie
  peak = FeatMaxAmplitude(series)
  max.time = peak$time.max
  #follow until d/dt shifts
  y = series[-(1:max.time)]
  min = min(y)
  max = max(y)
  min.time = which.min(y) + max.time - 1
  return (log(max/min)/abs(min.time - max.time))
}

response_delay = function(stim.time, max.amp){
  return( c("delay" = max.amp$time.max -stim.time)) 
}
max_amp_wrapper = function(series, stim.time, baseline){
  max_amp = FeatMaxAmplitude(series[stim.time:(length(series) - stim.time)], basal = baseline$before)
  return( list(
    max = max_amp$max,
    time.max = (max_amp$time.max + stim.time - 1) 
  ))
}



# # plot stuff ---------
# # hardcoded helper function
# add_baseline_to_plot = function(baseline, begin = T){
#     lines(1:10, rep(baseline$before, 10), col = "red")
#     lines(31:40, rep(baseline$after, 10), col = "red")
# }
# #lambda = exp_decay(single_timeseries_coralie, 10)
# add_dec_to_plot = function(series, lambda){
#   lines(c(which.max(series),length(series)), max(series) + lambda * c(0, length(series) - which.max(series)), col = "yellow")
#   
# }
# add_growth_to_plot = function(series, lambda){
#   x = c(0,50)
#   reg = (( x )*  lambda)  - ((lambda * which.max(series)) - max(series)) 
#   lines(x, reg, col = "yellow")
# }
# 
# add_dip_to_plot = function(dip_amp, series){
#   x = rep(dip_amp$time.min,10)
#   y1 = series[dip_amp$time.min]
#   y =sample(c(y1, y1 + dip_amp$min), 10, replace = T)
#   lines(x,y, col = "green")
# }
# add_amp_to_plot = function(max_amp, series){
#   x = rep(max_amp$time.max,10)
#   y1 = series[max_amp$time.max]
#   y =sample(c(y1, y1 - max_amp$max), 10, replace = T)
#   lines(x,y, col = "green")
# }
# fwhm_to_plot = function(fmwh, series){
# 
#   x = c(fmwh$left, fmwh$right)
#   dec = fmwh$right - as.integer(fmwh$right)
#   y = rep( (series[as.integer(fmwh$right)] * (1 -dec)) + (  series[as.integer(fmwh$right) +1 ] * dec) ,2 ) 
#   lines(x, y, col = "blue")
# 
# }
# 
# 
# series_plot_feat = function(series, dip, amp, time.var, fmwh, dec, growth){
#   plot(series, type = "l")
#   add_baseline_to_plot(series_baseline(series, time.var))
#   add_dip_to_plot(dip, series)
#   add_amp_to_plot(amp, series)
#   fwhm_to_plot(fmwh, series)
#   add_dec_to_plot(series, lambda = dec)
#   add_growth_to_plot(series, lambda = growth)
#   rug(time.var, col = "red", lw = 2)
# }
# 
# 
# #plot(calc_derivatives(single_timeseries_coralie))
# #plot(rollex(calc_derivatives(single_timeseries_coralie[10:40]), k = 10))
# #single_timeseries_coralie[-(1:10)]
# baseline = series_baseline(single_timeseries_coralie, 10)
# max_amp = max_amp_wrapper(single_timeseries_coralie, 10, baseline)
# fmwh = FeatFWHM(single_timeseries_coralie,  basal = baseline$before)
# dip_amp = dip_amplitude(single_timeseries_coralie, stim.time = 10, baseline)
# response_delay(10, max_amp)
# 
# series_plot_feat(single_timeseries_coralie, dip_amp, max_amp, stim.time, fmwh)
# 
# 
# lambda = TSexploreR::FeatHalfMaxDec(series, basal = baseline$before)
# lambda = FeatHalfMaxGrow(single_timeseries_coralie)
# 
# series = single_timeseries_coralie
# 
# 
# dt.data %>% group_by(.dots = c("Image_Metadata_Site",stim.var, "objNuc_TrackObjects_Label")) %>%
#   mutate(nuc_erk = get(cyto.erk)/get(nuc.erk)) %>%
#   do(cbind(., FeatAllFeat(.$nuc_erk, basal = 0.5, 1, 20))) %>% View(.)
# 
# pdf("teststuff.pdf")
# 
# data = dt.data %>% unite("unique" , c(objNuc_TrackObjects_Label, Image_Metadata_Site), sep = "_")
# 
# k = sample(unique(data$unique), 50)
# for(i in 1:length(k)){
#   single_timeseries_coralie = data %>%  filter(unique == k[i]) %>%
#     mutate(nuc_erk = objCyto_ring_Intensity_MeanIntensity_imErk/objNuc_Intensity_MeanIntensity_imErk) %>% select(nuc_erk) %>% melt() %>% pull()
#   
#   baseline = series_baseline(single_timeseries_coralie, 10)
#   max_amp = FeatMaxAmplitude(single_timeseries_coralie, basal = baseline$before)
#   fmwh = FeatFWHM(single_timeseries_coralie,  basal = baseline$before)
#   dip_amp = dip_amplitude(single_timeseries_coralie, stim.time = 10, baseline)
#   dec = FeatHalfMaxDec(single_timeseries_coralie)
#   growth = FeatHalfMaxGrow(single_timeseries_coralie)
#   series_plot_feat(single_timeseries_coralie, dip_amp, max_amp, stim.time, fmwh, dec, growth)
#   single_timeseries_coralie_treat = data %>% filter(unique == k[i]) %>%
#     select(Stimulation_treatment) %>% pull()
#   
#   title(as.character(single_timeseries_coralie_treat)[1])
#   
# }
# dev.off()
# 
# ### end plot stuff ------


all_features = function(series, stim.time){
  baseline = series_baseline(single_timeseries_coralie, stim.time)
  max_amp = max_amp_wrapper(single_timeseries_coralie, stim.time, baseline)
  fmwh = FeatFWHM(single_timeseries_coralie,  basal = baseline$before)
  dip_amp = dip_amplitude(single_timeseries_coralie, stim.time = stim.time, baseline)
  delay = response_delay(10, max_amp)
  # rates!
  dec = FeatHalfMaxDec(series, basal = baseline$before)
  growth = FeatHalfMaxGrow(series, basal = baseline$before)
  return( c(baseline = baseline,
            amplitude = max_amp,
            FMWH = fmwh,
            dip = dip_amp,
            delay = delay,
            growth = growth,
            decay = dec))
}
all_features_df = function(series, stim.time){
  baseline = series_baseline(series, stim.time)
  max_amp = max_amp_wrapper(series, stim.time, baseline)
  fmwh = FeatFWHM(series,  basal = baseline$before)
  dip_amp = dip_amplitude(series, stim.time = stim.time, baseline)
  delay = response_delay(10, max_amp)
  # rates!
  dec = FeatHalfMaxDec(series, basal = baseline$before)
  growth = FeatHalfMaxGrow(series, basal = baseline$before)
  return( data.frame(c("baseline" = baseline,
            'amplitude' = max_amp,
            'FMWH' = fmwh,
            'dip' = dip_amp,
            'delay' = delay,
            'growth' = growth,
            'decay' = dec)))
}



run_feature_extraction_single = function(data, stim.time, group.var, erk.cyto, erk.nuc){
  dt.w.features <- data %>%
    group_by_at(.vars = group.var)  %>% 
    mutate(erk.ratio = (get(erk.cyto)/get(erk.nuc))) %>%
    do(all_features_df(.$erk.ratio, stim.time)) %>% select(-growth, -decay)

  return(left_join(dt.w.features,  dt.data[,.(Image_Metadata_Site, objNuc_TrackObjects_Label,Stimulation_treatment)] %>% distinct() ))
}


#single_timeseries_coralie = dt.data %>% filter(objNuc_TrackObjects_Label == 15 & Image_Metadata_Site == 15) %>%
#  mutate(nuc_erk = objCyto_ring_Intensity_MeanIntensity_imErk/objNuc_Intensity_MeanIntensity_imErk) %>% select(nuc_erk) %>% melt() %>% pull()



#all_features_df(single_timeseries_coralie, 10) %>% View()

dt.data.w.features <- run_feature_extraction_single(data = dt.data, stim.time =10, group.var, cyto.erk, nuc.erk) 

fwrite(dt.data.w.features, "extracted_features.csv")


