# compute the influence area
library(raster)

filepath <- "/home/lagee/Documents/alber/test/hysplitsimulations/all_co2"


HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min", 
                      "V8", "V9", "lat", "lon", "height", "pressure") 


lon.range <- c(-80, -30) # range(hs.df[, "lon"])
lat.range <- c(-40, 10) # range(hs.df[, "lat"])


# build a list of sites and years
hsfiles <- list.files(filepath)
siteyear.mt <- do.call(rbind, unique(lapply(hsfiles, function(x){
  spl <- strsplit(x, split = "_")[[1]]
  return(c(spl[1], spl[2]))
})))



# analyze one site & one year
#siteyear <- siteyear.mt[1, ]

for(i in 1:nrow(siteyear.mt)){
  siteyear <- siteyear.mt[i, ]
  compInfArea(siteyear, filepath)
}



#siteyear <- siteyear.mt[1, ]
compInfArea <- function(siteyear, filepath){
  
  # filter files by site and year
  files <- list.files(filepath, pattern = paste0(siteyear[1], "_", siteyear[2]), full.names = TRUE)
  
  # read hyspit files into a dataframe  
  hs.df <- .files2df(file.vec = files, header = FALSE, skip = 7, cnames = HYSPLIT.COLNAMES)
  hs.df <- do.call(rbind, hs.df)

  # create grid (2D) 
  # - WGS84
  # - Origin site coordinates
  # - xy resolution 1 degree
  ll.res <- 2                                               # grid resolution in degrees
  grid.origin <- unlist(hs.df[1, c("lon", "lat")])

  lon.grid <- sort(c(seq(from = grid.origin[1], to = lon.range[2], by = ll.res), seq(from = grid.origin[1], to = lon.range[1], by = -ll.res)[-1]))
  lat.grid <- sort(c(seq(from = grid.origin[2], to = lat.range[2], by = ll.res), seq(from = grid.origin[2], to = lat.range[1], by = -ll.res)[-1]))
  #ll.grid <- expand.grid(lon.grid, lat.grid)
  #colnames(ll.grid) <- c("lon", "lat")
  
  # add a column with grid ID
  hs.df["gridX"] <- findInterval(hs.df$lon, lon.grid)
  hs.df["gridY"] <- findInterval(hs.df$lat, lat.grid)
  hs.df["gridXY"] <- paste0(hs.df$gridX, "-", hs.df$gridY)

  # sum the grid ID
  trajden <- as.data.frame(table(hs.df$gridXY), stringsAsFactors = FALSE)
  colnames(trajden) <- c("crid", "freq")
  trajden <- cbind(trajden, do.call(rbind, strsplit(trajden$crid, split = "-")), stringsAsFactors = FALSE)
  colnames(trajden) <- c("crid", "freq", "xcol", "ycol")
  trajden$xcol <- as.numeric(trajden$xcol)
  trajden$ycol <- as.numeric(trajden$ycol)
  trajden <- trajden[, c("xcol", "ycol", "freq")]
  # remove extreme values
  trajden <- trajden[trajden$xcol != 0,]
  trajden <- trajden[trajden$ycol != 0,]
  
  # build grid
  egrid <- expand.grid(1:(max(trajden$xcol) - 1), 1:(max(trajden$ycol) - 1))
  colnames(egrid) <- c("xcol", "ycol")
  # joins grids
  egrid <- merge(egrid, trajden, by = c("xcol", "ycol"), all.x = TRUE)
  #
  #---- filter ----
  #egrid[is.na(egrid$freq), "freq"] <- 0
  #egrid[egrid$freq < 2, "freq"] <- NA
  # - - - - - - 
  egrid$ycol <-  abs(egrid$ycol - (max(egrid$ycol) + 1)) # invert the y-axis
  egrid <- egrid[order(egrid$ycol, egrid$xcol),] # order by lon & lat
  
  # plot the grid
  raster.dat <- log(egrid$freq) # egrid$freq # 1:length(r1[])
  raster.dat[raster.dat < 2] <- NA
  r1 <- raster(nrows = 1:(max(trajden$xcol) - 1), 
               ncols = 1:(max(trajden$ycol) - 1),
               xmn = min(lon.grid), 
               xmx = max(lon.grid),
               ymn = min(lat.grid),
               ymx = max(lat.grid))
  r1[] <- raster.dat
  #plot(r1, main = paste(siteyear, collapse = " "))
  nbreaks <- 10
  #hist(log(egrid$freq), breaks = nbreaks)
  d <- r1
  intBreaks <- seq(2, 8,length.out=nbreaks) 
  plot( d, 
        col = rev(heat.colors(nbreaks)),
        breaks = intBreaks, main = paste(siteyear, collapse = " ")) 
  maps::map("world", xlim = lon.range, ylim = lat.range, add = TRUE)
}
# minimim value of a cell 0
# maximum value of a cell 12 flask * 14 days * 24 hours * 4 sample per hour = 16128

