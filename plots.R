

#naming of the columns
# nuc.erk = "objNuc_Intensity_MeanIntensity_imErk"
# cyto.erk = "objCyto_ring_Intensity_MeanIntensity_imErk"
# time.var = "RealTime"
# stim.var = "Stimulation_treatment"
# stim.time.var = "Stimulation_time"
plate = "plate1"

#for each list element a plot will be created with those elements
experient.groups.plate1 = list(
  c("+CTRL 1","+CTRL 2","+CTRL 3","+CTRL 4","+CTRL 5","+CTRL 6","+CTRL 7","+CTRL 8","-CTRL 1","-CTRL 2","-CTRL 3","-CTRL 4","-CTRL 5","-CTRL 6","-CTRL 7","-CTRL 8"),
  c("+CTRL 1", "-CTRL 1", "ERK1","ERK2", "MEK1", "MEK2"),
  c("+CTRL 2", "-CTRL 2", "C-Raf","A-Raf", "B-Raf", "MAP3K1"),                      
  c("+CTRL 3", "-CTRL 3", "KRAS","HRAS", "NRAS", "RRAS"),
  c("+CTRL 4", "-CTRL 4", "CNKSR1","KSR1", "YWHAG", "YWHAZ"),
  c("+CTRL 5", "-CTRL 5", "DUSP1","DUSP2", "DUSP3", "DUSP4"),
  c("+CTRL 6", "-CTRL 6", "DUSP6","DUSP8", "DUSP9", "DUSP10"),
  c("+CTRL 7", "-CTRL 7", "DUSP14","DUSP16", "DUSP22", "DUSP26"),
  c("+CTRL 8", "-CTRL 8", "PP2A","RKIP", "PEA15", "RSK2")
  #c("WT 1", "WT 2", "WT 3","WT 4", "+CTRL 9", "-CTRL 9")
  
)

experient.groups.plate2 = list(
  c("+CTRL 1","+CTRL 2","+CTRL 3","+CTRL 4","+CTRL 5","+CTRL 6","+CTRL 7","+CTRL 8","-CTRL 1","-CTRL 2","-CTRL 3","-CTRL 4","-CTRL 5","-CTRL 6","-CTRL 7","-CTRL 8"),
  c("+CTRL 1", "-CTRL 1", "SOS1","SOS2", "RASA1", "NF1"),
  c("+CTRL 2", "-CTRL 2", "RASGRP1","RAP1A", "RAP1B", "SRC"),                      
  c("+CTRL 3", "-CTRL 3", "RAPGEF1","RAPGEF3", "DIRAS1", "DIRAS2"),
  c("+CTRL 4", "-CTRL 4", "FRS2","Shc1", "GAB1", "GRB2"),
  c("+CTRL 5", "-CTRL 5", "SPRY1","SPRY2", "SPRY3", "SPRY4"),
  c("+CTRL 6", "-CTRL 6", "PTPN6","PTPN11", "NCK1", "NCK2"),
  c("+CTRL 7", "-CTRL 7", "CRKL","SHIP", "PTK2", "PLCG1"),
  c("WT 1", "WT 2", "WT 3","WT 4", "+CTRL 9", "-CTRL 9")
)


# Create a new table with only positive controls
# dt.data.positiveCTRLs = dt.data[Stimulation_treatment %in% c("+CTRL 1","+CTRL 2","+CTRL 3","+CTRL 4","+CTRL 5","+CTRL 6","+CTRL 7","+CTRL 8")]
# 
# # Create a new table with only negative controls
# dt.data.negativeCTRLs = dt.data[Stimulation_treatment %in% c("-CTRL 1","-CTRL 2","-CTRL 3","-CTRL 4","-CTRL 5","-CTRL 6","-CTRL 7","-CTRL 8")]
# 
# # Create a new table for each experiment
# dt.data.exp1 = dt.data[Stimulation_treatment %in% c("+CTRL 1", "-CTRL 1", "ERK1","ERK2", "MEK1", "MEK2")]


create_plot = function(data, ci.lvl, stimulus.rug, stim.var, erk.ratio.var, time.var, nuc.erk, cyto.erk,stim.color = "red",  alpha = 0.1, xlab = "Real Time (min)", ylabel = "ERK-KTR Cytoplasmic ot Nuclear Ratio", vlines = F){
  a = ci.lvl
  #create unique id for each track..
  #data[,unique := paste(Image_Metadata_Site, objNuc_TrackObjects_Label, sep = "_")]
  #calc erk ratio
  # for each grouping calc mean, min, max, ci upper lower
  data.summary <- data %>% ungroup() %>% group_by(.dots = c(stim.var, time.var)) %>% summarise(ymin_ctn = min(get(erk.ratio.var)) , ymax_ctn = max(get(erk.ratio.var)), mean_ctn = mean(get(erk.ratio.var)),
          lower = mean(get(erk.ratio.var)) - (qnorm(1-(a/2)) * sd(get(erk.ratio.var))/sqrt(n())) , upper =  mean(get(erk.ratio.var)) + (qnorm(1-(a/2)) * sd(get(erk.ratio.var))/sqrt(n())))
    #get also rug of pulses
  
  ggp = ggplot(data.summary, aes(x = get(time.var), y = mean_ctn, group = get(stim.var)))+ 
    geom_ribbon(alpha = alpha, mapping = aes(ymin = lower, ymax = upper)) +
    geom_line(aes(color = get(stim.var)), size = 1.2) + 
    labs(x = time.var, y = ylabel, legend = "stim.var")  + ggplotTheme() + theme(legend.title = element_blank()) +
    #scale_color_brewer(palette = "Set3")
    geom_rug(data = data.frame(stim.times = stim.times), aes(x=stim.times,y =NULL, group = NULL), color = stim.color)
    
    if(vlines){
      ggp = ggp + geom_vline(data = data.frame(stim.times = stim.times), aes(xintercept=stim.times, group = NULL), color = stim.color, linetype="dotted")
    }
    
    return(ggp)
  }

custom_rug = function(ggplot.obj, stim.times, stim.color){
  rug = ggplot.obj + geom_rug(aes(x=stim.times[1],y = NULL), color = stim.color)
  for (ta in stim.times[-1]){
    rug = rug + geom_rug(aes(x=ta,y = NULL), color = stim.color)
  }
  return(rug)
}
#stolen from dmattek tca-package
ggplotTheme = function(in.font.base = 12,
                       in.font.axis.text = 12,
                       in.font.axis.title = 12,
                       in.font.strip = 14,
                       in.font.legend = 12) {
  loc.theme =
    theme_bw(base_size = in.font.base, base_family = "Helvetica") +
    theme(
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.25),
      axis.text = element_text(size = in.font.axis.text),
      axis.title = element_text(size = in.font.axis.title),
      strip.text = element_text(size = in.font.strip, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = in.font.legend),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines"))
  
  return(loc.theme)
}


#create_plot(data, 0.05, 10, nuc.erk = nuc.erk, cyto.erk = cyto.erk, time.var = time.var, stim.var = stim.var)

pdf(file = pdf.filename)
for(group in get(paste0("experient.groups.", plate))){
  
  #stim_vec = as.numeric(pull(stim_vec))
  gg = create_plot(data = dt.data[Stimulation_treatment %in% group],
                   ci.lvl = 0.05,
                   stimulus.rug = stim.times,
                   nuc.erk = nuc.erk,
                   cyto.erk = cyto.erk,
                   time.var = time.var,
                   stim.var = stim.var,
                   erk.ratio.var = erk.ratio.var,
                   vlines = T)
  plot(gg)
}
dev.off()

