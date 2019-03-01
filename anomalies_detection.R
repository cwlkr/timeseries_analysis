# create oulier detection decosion tree

library(rpart)


# manually annotated set.

data = dt.data %>% unite("unique" , c(objNuc_TrackObjects_Label, Image_Metadata_Site), sep = "_")

set.seed(156)
k = sample(unique(data$unique), 200)

pdf("forannot2.pdf")
for(i in 1:length(k)){
   single_timeseries_coralie = data %>%  filter(unique == k[i]) %>%
     mutate(nuc_erk = objCyto_ring_Intensity_MeanIntensity_imErk/objNuc_Intensity_MeanIntensity_imErk) %>% select(nuc_erk) %>% melt() %>% pull()
   
   baseline = series_baseline(single_timeseries_coralie, 10, robust = T)
   dip_amp = dip_amplitude(single_timeseries_coralie, stim.time = 10, baseline)
   max_amp = max_amp_wrapper(single_timeseries_coralie, stim.time =  stim.times, baseline = baseline, dip.amp = dip_amp)
   fwhm = fwhm_wrapper(single_timeseries_coralie,  basal = baseline$before, dip_amp)
   dec = FeatHalfMaxDec(single_timeseries_coralie)
   growth = FeatHalfMaxGrow(single_timeseries_coralie)
   series_plot_feat(single_timeseries_coralie, dip_amp, max_amp, stim.times, fwhm, dec, growth)
   single_timeseries_coralie_treat = data %>% filter(unique == k[i]) %>%
     select(Stimulation_treatment) %>% pull()
   
   title(paste(as.character(single_timeseries_coralie_treat)[1], k[i] , sep = "_"))
   
 }
 dev.off()
 k = "38_78"
 single_timeseries_coralie = data %>%  filter(unique == k) %>%
   mutate(nuc_erk = objCyto_ring_Intensity_MeanIntensity_imErk/objNuc_Intensity_MeanIntensity_imErk) %>% select(nuc_erk) %>% melt() %>% pull()
 
 baseline = series_baseline(single_timeseries_coralie, 10, robust = T)
 dip_amp = dip_amplitude(single_timeseries_coralie, stim.time = 10, baseline)
 max_amp = max_amp_wrapper(single_timeseries_coralie, stim.time =  stim.times, baseline = baseline, dip.amp = dip_amp)
 fmwh = fwhm_wrapper(single_timeseries_coralie,  basal = baseline$before, dip_amp)
 dec = FeatHalfMaxDec(single_timeseries_coralie)
 growth = FeatHalfMaxGrow(single_timeseries_coralie)
 series_plot_feat(single_timeseries_coralie, dip_amp, max_amp, stim.times, fmwh, dec, growth)
 
 
 annot = list(
   class = c(
     1,     1,     1,     1,     0,     1,     0,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     0,     1,     1,     1,     1,     1,     1,     1,     0,     1,     1,     1,   1,     # 30
     1,     1,     1,     1,     0,     1,     0,     1,     0,     1,     1,     1,     1,     0,     0,     1,     0,     1,     1,     0,     # 50
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     0,     1,     1,     0,     1,     # 70
     1,     0,     1,     1,     1,     1,     1,     1,     0,     1,     1,     1,     1,     1,     1,     0,     0,     1,     1,     1,     # 90
     0,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     0,     1,     1,     1,     1,     1,     1,     1,     1      # 110
   )
 )

 #cyto.erk = "objCyto_Intensity_MeanIntensity_imErk"
 time.var = "RealTime"
 stim.var = "Stimulation_treatment"
 stim.time.var = "Stimulation_time"
 group.var = c("Image_Metadata_Site", "objNuc_TrackObjects_Label")
 erk.ratio.var = "erk.ratio"
 data.annot = data %>% filter(unique %in% k[1:110])
 data.annot %>% separate(unique, into = c('objNuc_TrackObjects_Label', 'Image_Metadata_Site'), sep = "_") %>% mutate(objNuc_TrackObjects_Label = as.numeric(objNuc_TrackObjects_Label),
                                                                                                                     Image_Metadata_Site = as.numeric(Image_Metadata_Site))-> data.annot
 features  = run_feature_extraction_single(data = data.annot, stim.time = stim.time, group.var = group.var, erk.cyto = cyto.erk, erk.nuc = nuc.erk)
 
 features$class = annot$class

 
 tree = rpart(formula = class ~ ., data =  features, method = "class") 
rpart.plot(x = tree)

