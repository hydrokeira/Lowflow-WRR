##make map of average SWE on April 1
require(dplyr)
require(data.table)
require(raster)
require(rgdal)
require(RColorBrewer)
install.packages("raster")
#install.packages("rgeos")
require(rgeos)

SWE_getoneday <- function(date_yyyymmdd, shapefile_path) {
  
  allSNODAS<-list.files(path = "/Users/keirajohnson/Documents/SNODAS")
  
  allSWE<-allSNODAS[allSNODAS %like% "us_ssmv11034tS__T0001TTNATS"]
  
  one_date<-allSWE[allSWE %like% date_yyyymmdd]
  
  #set output directory
  outDir<-c("/Users/keirajohnson/Documents/SNODAS")
  
  #scaling factors
  nCol <- 6935
  nRow <- 3351 #columns and rows number: masked version of contiguous US
  dataScaleFactor  <-  1000  #multiply the data this amount, both depth and SWE
  
  wgs84 <- CRS("+proj=longlat +datum=WGS84")
    
  file_name<-one_date
  
  #extract SWE
  sweFile0  <- paste0(outDir, "/", file_name)
  
  #extract from dat.gz file
  sweCon  <- gzcon(file(sweFile0, "rb"))
  
  #read binary data from file above
  sweData <- readBin(sweCon, integer(), n=nRow*nCol, size=2, signed=TRUE, endian='big')/dataScaleFactor
  
  #remove error values
  sweData[which(sweData==-9.999)]<-NA
  
  #convert to matrix
  sweDataMatrix<-matrix(sweData, ncol=nCol, nrow=nRow, byrow = T)
  
  #convert to mm
  swe_array<-sweDataMatrix*dataScaleFactor
  
  sweRaster <- raster(swe_array, ymn=24.9504, ymx=52.8754, xmn=-124.7337, xmx=-66.9421, crs=wgs84)
  
  watershed_shapefile<-readOGR(shapefile_path)
  
  shape_extent<-extent(bbox(watershed_shapefile))
  
  watershed_raster<-crop(sweRaster, shape_extent)
  
  watershed_raster_mask<-raster::mask(watershed_raster, watershed_shapefile)
  
  SWE_watershed_shapefile<-rasterToPolygons(watershed_raster_mask)
  
  return(SWE_watershed_shapefile)
  
}

Coal_shapefile<-SWE_getoneday("20190407", "/Users/keirajohnson/Box Sync/Keira_Johnson/CoalCshapefile/layers/globalwatershed.shp")

spplot(Coal_shapefile)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek")

raster::shapefile(Coal_shapefile, "Coal_shapefile_20190407.shp", overwrite = TRUE)

