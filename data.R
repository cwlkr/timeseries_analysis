if(!require(pacman)){
  install.packages("pacman")
}

rm(list  = ls())
pacman::p_load(data.table, ggplot2, tidyverse, readxl)


nuc.erk = "objNuc_Intensity_MeanIntensity_imErk"
cyto.erk = "objCyto_ring_Intensity_MeanIntensity_imErk"
time.var = "RealTime"
stim.var = "Stimulation_treatment"
stim.time.var = "Stimulation_time"
group.var = c("Image_Metadata_Site", "objNuc_TrackObjects_Label")
erk.ratio.var = "erk.ratio"

#laod dataset
## example single ------
mnt =  "/run/user/1000/gvfs/afp-volume:host=izbhelsinki,volume=imaging.data"
experiment = "Coralie/NIH3T3/siPOOLs/20190212_systIII_siPOOLs_plate1_singlePulses/20190212_092457_353/Merged"
metaname = "20190212_NIH3T3_syst_III_siPOOL_plate1_singlePulses.xlsx"
# example multi -----------
#mnt =  "/run/user/1000/gvfs/afp-volume:host=izbhelsinki,volume=imaging.data"
#experiment = "Coralie/NIH3T3/siPOOLs/20181213_systIII_siPOOLs_plate1_multipulses_III/20181213_174451_769/Merged"
#metaname = "20181213_NIH3T3_syst_III_siPOOL_plate1_multiPulses.xlsx"
# ---------

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

# add cyttonucratio
dt.data = dt.data[, erk.ratio := get(cyto.erk)/get(nuc.erk)]
  
stim.times <- metadata %>% select(contains(stim.time.var), stim.var) %>% 
  select(contains(stim.time.var)) %>% slice(1)  %>% c(., recursive=TRUE) %>% as.numeric()

source('~/MasterProjects/timeseries_analysis/plots.R')
source('~/MasterProjects/timeseries_analysis/timeseries_analysis_functions_single.R')