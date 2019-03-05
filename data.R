if(!require(pacman)){
  install.packages("pacman")
}

rm(list  = ls())
pacman::p_load(data.table, ggplot2, tidyverse, readxl)

nuc.erk = "objNuc_Intensity_MeanIntensity_imErk"
cyto.erk = "objCyto_ring_Intensity_MeanIntensity_imErk"
#cyto.erk = "objCyto_Intensity_MeanIntensity_imErk" # alterntive in older Experiments
time.var = "RealTime"
stim.var = "Stimulation_treatment"
stim.time.var = "Stimulation_time"
group.var = c("Image_Metadata_Site", "objNuc_TrackObjects_Label")
erk.ratio.var = "erk.ratio"
meta.grouping = "Grouping"

#laod dataset
## example single ------ 
#### Only useful if imaging.data is mounted in os
mnt =  "/run/user/1000/gvfs/afp-volume:host=izbhelsinki,volume=imaging.data" # ubuntu
mnt = 'y:'                                                                   # windows
mnt = ""                                                                     # just exp path.


####### path to Merged folder
experiment = "Coralie/NIH3T3/siPOOLs/20190221_systIII_siPOOLs_plate1_singlePulses/20190221_090712_551/Merged"
metaname = "20190221_NIH3T3_syst_III_siPOOL_plate1_singlePulses_I.xlsx"
pdf.filename = "plots.pdf" # title of pdf the plots well be rendered in 
##########

metadatapath = paste(mnt, experiment, metaname, sep = "/") # or just the path to the metadata file



path = paste(mnt, experiment, "cp.out", sep = "/") # or just the path to tCoursesSelected.csv

# Read csv file 
s.file = list.files(path = path, pattern = "tCoursesSelected.csv", recursive = FALSE, full.names = TRUE)
dt.data = fread(file = s.file)

#load metadata
metadata = as.data.table(read_xlsx(metadatapath))[-(1:2)]
metanamecol = paste0(as.vector(metadata[1,]), "") # dirty hack to get "real" vector
metadata = `colnames<-`(metadata, metanamecol)[-1,]

# add cyttonucratio
dt.data = dt.data[, erk.ratio := get(cyto.erk)/get(nuc.erk)]
  
stim.times <- metadata %>% select(contains(stim.time.var), stim.var) %>% 
  select(contains(stim.time.var)) %>% slice(1)  %>% c(., recursive=TRUE) %>% as.numeric()

#just to make sure that grouping and stim_treatment are ok!
groups <- metadata %>% # group_by(.dots = meta.grouping) %>% 
 select(stim.var, meta.grouping, Position, Well) %>% rename("Image_Metadata_Site" = "Position", "Metadata_Well" = "Well") %>%
  mutate("Grouping" = as.numeric(get(meta.grouping)), Image_Metadata_Site = as.numeric(Image_Metadata_Site), Metadata_Well = as.numeric(Metadata_Well)) 

dt.data =   left_join(dt.data, groups)

source('~/MasterProjects/timeseries_analysis/plots.R')

