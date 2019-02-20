if(!require(pacman)){
  install.packages("pacman")
}
pacman::p_load(data.table, ggplot2, tidyverse, readxl)
#laod dataset
mnt =  "/run/user/1000/gvfs/afp-volume:host=izbhelsinki,volume=imaging.data"
experiment = "Coralie/NIH3T3/siPOOLs/20181126_systIII_siPOOLs_plate1_singlePulses_I/20181126_101824_932/Merged"
metaname = "20181126_NIH3T3_syst_III_siPOOL_plate1_singlePulses.xlsx"

pdf.filename = "plots.pdf" 

path = paste(mnt, experiment, "cp.out", sep = "/") # or just the path to tCoursesSelected.csv
metadatapath = paste(mnt, experiment, metaname, sep = "/") # or just the path to the metadata file

# Read csv file 
s.file = list.files(path = path, pattern = "tCoursesSelected.csv", recursive = FALSE, full.names = TRUE)
dt.data = fread(file = s.file)

#load metadata
metadata = as.data.table(read_xlsx(metadatapath))[-(1:2)]
metanamecol = paste0(as.vector(metadata[1,]), "") # dirty hack to get "real" vector
metadata = `colnames<-`(metadata, metanamecol)[-1,]




#naming of the columns
nuc.erk = "objNuc_Intensity_MeanIntensity_imErk"
cyto.erk = "objCyto_ring_Intensity_MeanIntensity_imErk"
time.var = "RealTime"
stim.var = "Stimulation_treatment"


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
  c("+CTRL 8", "-CTRL 8", "PP2A","RKIP", "PEA15", "RSK2"),
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


create_plot = function(data, ci.lvl, stimulus.rug, stim.var, time.var, nuc.erk, cyto.erk,stim.color = "red",  alpha = 0.1, xlab = "Real Time (min)", ylab = "ERK-KTR Cytoplasmic ot Nuclear Ratio"){
  a = ci.lvl
  #create unique id for each track..
  #data[,unique := paste(Image_Metadata_Site, objNuc_TrackObjects_Label, sep = "_")]
  #calc erk ratio
  data[,"nuc_cyto" := (as.double(get(cyto.erk))/as.double(get(nuc.erk)))]
  # for each grouping calc mean, min, max, ci upper lower
  data.summary <- data%>% group_by(.dots = c(stim.var, time.var)) %>% summarise(ymin_ctn = quantile(nuc_cyto, 0), ymax_ctn = quantile(nuc_cyto, 1), mean_ctn = mean(nuc_cyto),
          lower = mean(nuc_cyto) - (qnorm(1-(a/2)) * sd(nuc_cyto)/sqrt(n())) , upper =  mean(nuc_cyto) + (qnorm(1-(a/2)) * sd(nuc_cyto)/sqrt(n())))
    #get also rug of pulses
  
  gg = ggplot(data.summary, aes(x = get(time.var), y = mean_ctn, group = get(stim.var)))+ 
    geom_ribbon(alpha = alpha, mapping = aes(ymin = lower, ymax = upper)) +
    geom_line(aes(color = get(stim.var)), size = 1.5) + 
    # vector of measurements 
    geom_rug(aes(x = stimulus.rug, y = NULL), color = stim.color) + 
    labs(x = time.var, y = ylab, legend = "stim.var")  + ggplotTheme() + theme(legend.title = element_blank())
    return(gg)
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


create_plot(data, 0.05, 10, nuc.erk = nuc.erk, cyto.erk = cyto.erk, time.var = time.var, stim.var = stim.var)

pdf(file = pdf.filename)
for(group in experient.groups.plate1){
  
  metadata %>% select(contains("Stimulation_time"), stim.var) %>% filter(get(stim.var %in% group))
  print(group)
  gg = create_plot(dt.data[Stimulation_treatment %in% group], 0.05, 10, nuc.erk = nuc.erk, cyto.erk = cyto.erk, time.var = time.var, stim.var = stim.var)
  plot(gg)
}
dev.off()



