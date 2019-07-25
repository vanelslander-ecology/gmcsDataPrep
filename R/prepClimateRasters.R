prepClimateRasters <- function(currentRas, 
                               stack, 
                               isATA,
                               rtm,
                               rasterToMatch){
  
  yearRas <- stack[[currentRas]]
  if (isATA == TRUE) {
    #ATA was stored as an integer
    yearRas[] <- yearRas[]/1000
  }
  
  rtmExt <- raster(rtm)
  ymax(rtmExt) <- ymax(rtmExt) + abs(ymax(rtmExt) * 0.05)
  ymin(rtmExt) <- ymin(rtmExt) - abs(ymin(rtmExt) * 0.05)
  xmax(rtmExt) <- xmax(rtmExt) + abs(xmax(rtmExt) * 0.05)
  xmin(rtmExt) <- xmin(rtmExt) - abs(xmin(rtmExt) * 0.05)
  
  rtmExt[] <- 1
  reprojRTM <- projectRaster(rtmExt, yearRas)
  yearRas <- crop(yearRas, reprojRTM) %>%
    projectRaster(., rasterToMatch)
  return(yearRas)
}