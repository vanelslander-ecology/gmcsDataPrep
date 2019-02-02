# To generate the elevation data I used the GTOPO Hydro1K DEM (available from
# Earth Explorer), masked using terrestrial Canada ecozones (excluding Arctic cordillera and northern Arctic)
# and resampled to 10 km using bilinear interpolation, then reprojected to NAD83.

rawDat <- read.csv("C:/Ian/Git/gmcsDataPrep/data/Canada10kmElev.txt")
rawDat

CleanedDat <- data.frame("ID1" = rawDat$OBJECTID, "ID2" = rawDat$pointid, "lat" = rawDat$POINT_Y,
                         "long" = rawDat$POINT_X, "el" = rawDat$grid_code)
CleanedDat <- CleanedDat[CleanedDat$lat > 14.453 & CleanedDat$lat < 83.203,]
CleanedDat <- CleanedDat[CleanedDat$long > -179.167 & CleanedDat$long < -52.625,]

write.csv(CleanedDat, "C:/Ian/Git/gmcsDataPrep/data/climateNAinput_10km.csv", row.names = FALSE)

summary(CleanedDat)
