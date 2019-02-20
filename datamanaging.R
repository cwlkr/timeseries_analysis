### plots for coralie

# test data set
# imaging.data/Coralie/NIH3T3/siPOOLs/20181126_systIII_siPOOLs_plate1_singlePulses_I/20181126_101824_932/Merged/cp.out

require(ggplot2)
require(readxl)

mnt =  "/run/user/1000/gvfs/afp-volume:host=izbhelsinki,volume=imaging.data"
experiment_path_plate_1 = paste0(mnt, "/Coralie/NIH3T3/siPOOLs/20181126_systIII_siPOOLs_plate1_singlePulses_I/20181126_101824_932")
experiment_path_plate_2 = paste0(mnt, "/Coralie/NIH3T3/siPOOLs/20181126_systIII_siPOOLs_plate2_singlePulses_I/20181126_171811_861")
cpfolder = "Merged/cp.out/output"

metafilename1 = "20181126_NIH3T3_syst_III_siPOOL_plate1_singlePulses.xlsx"
metafilename2 = "20181126_NIH3T3_syst_III_siPOOL_plate1_singlePulses.xlsx"
metadata = fread(paste(experiment_path_plate_1, "Merged", metafilename,sep = "/"))

#columns that can be droped
col_drop = c(
  'Image_ImageNumber',
  'Image_Metadata_C',
  'Image_Metadata_ChannelName',
  'Image_Metadata_ColorFormat',
  'Image_Metadata_FileLocation',
  'Image_Metadata_Frame',
  'Image_Metadata_Plate',
  'Image_Metadata_SizeC',
  'Image_Metadata_SizeT',
  'Image_Metadata_SizeX',
  'Image_Metadata_SizeY',
  'Image_Metadata_SizeZ',
  'Image_Metadata_Well',
  'Image_Metadata_Z',
  'Image_Metadata_Series'
)
# ----
cols = list(
  "Image_ImageNumber",
  "objNuc_ObjectNumber",
  "Image_Metadata_C",
  "Image_Metadata_ChannelName",
  "Image_Metadata_ColorFormat",
  "Image_Metadata_FileLocation",
  "Image_Metadata_Frame",
  "Image_Metadata_Plate",
  "Image_Metadata_Series",                                 
  "Image_Metadata_Site",
  "Image_Metadata_Site",
  "Image_Metadata_Site" ,                                     
  "Image_Metadata_SizeC",
  "Image_Metadata_SizeT",
  "Image_Metadata_SizeX" ,                                    
  "Image_Metadata_SizeY",
  "Image_Metadata_SizeZ",
  "Image_Metadata_T",                                   
  "Image_Metadata_T",
  "Image_Metadata_Well",  
  "Image_Metadata_Z",                                     
  "objNuc_Intensity_IntegratedIntensity_imErk",
  "objNuc_Intensity_IntegratedIntensity_imErkCorrOrig",
  "objNuc_Intensity_IntegratedIntensity_imNuc"               ,
  "objNuc_Intensity_MeanIntensity_imErk",
  "objNuc_Intensity_MeanIntensity_imErkCorrOrig",
  "objNuc_Intensity_MeanIntensity_imNuc"  ,                   
  "objNuc_Location_Center_X",
  "objNuc_Location_Center_Y",
  "objNuc_TrackObjects_Label"                                ,
  "objCyto_ring_Intensity_IntegratedIntensity_imErk",
  "objCyto_ring_Intensity_IntegratedIntensity_imErkCorrOrig",
  "objCyto_ring_Intensity_IntegratedIntensity_imNuc"         ,
  "objCyto_ring_Intensity_MeanIntensity_imErk",
  "objCyto_ring_Intensity_MeanIntensity_imErkCorrOrig",
  "objCyto_ring_Intensity_MeanIntensity_imNuc"               ,
  "objCyto_total_Intensity_IntegratedIntensity_imErk",
  "objCyto_total_Intensity_IntegratedIntensity_imErkCorrOrig",
  "objCyto_total_Intensity_IntegratedIntensity_imNuc"        ,
  "objCyto_total_Intensity_MeanIntensity_imErk",
  "objCyto_total_Intensity_MeanIntensity_imErkCorrOrig",
  "objCyto_total_Intensity_MeanIntensity_imNuc"          ,    
  "obj_totalCell_Intensity_IntegratedIntensity_imErk",
  "obj_totalCell_Intensity_IntegratedIntensity_imErkCorrOrig",
  "obj_totalCell_Intensity_IntegratedIntensity_imNuc"     ,   
  "obj_totalCell_Intensity_MeanIntensity_imErk" ,
  "obj_totalCell_Intensity_MeanIntensity_imErkCorrOrig",
  "obj_totalCell_Intensity_MeanIntensity_imNuc"     ,         
  "obj_ring_Intensity_IntegratedIntensity_imErk",
  "obj_ring_Intensity_IntegratedIntensity_imErkCorrOrig",
  "obj_ring_Intensity_IntegratedIntensity_imNuc",
  "obj_ring_Intensity_MeanIntensity_imErk" ,
  "obj_ring_Intensity_MeanIntensity_imErkCorrOrig",
  "obj_ring_Intensity_MeanIntensity_imNuc"      ,             
  "Position" = "Position"       
)
#----
merge_csv_from_remote = function(path, plate){
  require(readxl)
  require(data.table)
  folderlist = list.dirs(path, recursive = FALSE)
  dt = fread(paste0(folderlist[1],  "/objNuc.csv"))
  colnames <- colnames(dt)
  colnames_1 = dt[1,]
  dt = dt[-1,]
  names = paste(colnames, as.vector(colnames_1),  sep = "_")
  
  for (folder in folderlist[-1][1:2]) {
    dt = rbind(dt, fread(paste0(folder,  "/objNuc.csv"))[-1,])
  }
  dt = `colnames<-`(x=dt, names)
  
  #also get metadata and merge to together
  metadata = as.data.table(read_xlsx(paste(experiment_path_plate_1, "Merged", metafilename,sep = "/")))[-(1:2)]
  metaname = paste0(as.vector(metadata[1,]), "") # dirty hack to get "real" vector
  metadata = `colnames<-`(metadata, metaname)[-1,]
  da = dt[,"Position" := get("Image_Metadata_Site")]
  return(dt)
}








data = merge_csv_from_remote(experiment_path_plate_1)
data_plate_2 = merge_csv_from_remote(experiment_path_plate_2)

remove_zero_erk_trajectories = function(data){
  
}

summary_plot = function(data, columndescr){
  
}
