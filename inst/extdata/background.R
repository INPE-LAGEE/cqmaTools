# background.R - Computation of background values

#---- TODO ----
# - hysplit. Should we run it here?

if(cqmaTools::get_os() == "windows") {
  warning(paste0("Processing takes longer in windows because it is unable to ", 
                 "use corrrectly the package parallel. See ?mclapply"))
}


#---- Constants ----
# @param cnames     A character vector. The name of the columns of the raw data file
RAW.DATA.COLNAMES <- c("site", "year", "month", "day", "hour", "min", "flask", 
                       "V8", "concentration", "flag", "V11", "ayear", "amonth", 
                       "aday", "ahour", "amin", "lat", "lon", "height", 
                       "eventnumber", "flat", "flon", "fheight")
#
# @param keepCols   A character vector. The column names of the raw data file to keep after filtering
RAW.DATA.COLNAMES.KEEP <- c("site", "lat", "lon", "height", "year", "month", 
                            "day", "hour", "min", "flask", "concentration", 
                            "eventnumber")
# @param cnamesTest A character vector. The column names used for testing duplicated rows in the filtered data
RAW.DATA.COLNAMES.TESTDUPLICATED <- c("site", "height", "year", "month", "day", 
                                      "hour", "min", "flask")
# column names of hysplit files
HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min", 
                      "V8", "V9", "lat", "lon", "height", "pressure") 
# spatial reference system assumed for geographic data 
SPATIAL.REFERENCE.SYSTEM <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# column used to build dates
# DATE.COLNAMES <- c("year", "month", "day", "hour", "min")
# the zone assumed for data
#TIME.ZONE <- "GMT"
# metadata included in the trajectory's file names
TRAJ.FILENAMES.METADATA <- c("site", "year", "month", "day", "hour", "height")
# column anmes that make a profile
PROFILE.COLNAMES <- c("site", "year", "month", "day")




#---- Script setup ----
#test.path <- "/home/lagee/Documents/alber/test"
test.path <- "/home/alber/Documents/tmp/cqmaTools/test"                         # base path
data.out.path <- file.path(test.path, "BKG_results", fsep = .Platform$file.sep) # path to the resulting numeric files

stationfile <- c(
  file.path(test.path, "stations", "rpbdaily", fsep = .Platform$file.sep), 
  file.path(test.path, "stations", "ascdaily", fsep = .Platform$file.sep), 
  file.path(test.path, "stations", "cptdaily", fsep = .Platform$file.sep)
)
limit.shp <- file.path(test.path, "shp", "limite.shp", fsep = .Platform$file.sep)
samerica.shp <- file.path(test.path, "shp", "continentalSouthAmericaLines.shp", fsep = .Platform$file.sep)
plot.path <- file.path(test.path, "plots", fsep = .Platform$file.sep) # plot.path <- "/Volumes/LVG/cqmaTools/plots/"
hysplit.sim.path <- file.path(test.path, "hysplitsimulations", fsep = .Platform$file.sep) # it still requires the gas to be attached to the path
rawdatafile.vec <- list.files(file.path(test.path, "rawdata", fsep = .Platform$file.sep), 
                              full.names = TRUE, recursive = FALSE, include.dirs = FALSE)

#--- Log setup ----
logger <- log4r::create.logger()
log4r::logfile(logger) <- file.path(test.path, "background.log")
log4r::level(logger) <- "DEBUG"
log4r::info(logger, "Start! ###############################################")

#---- Script parameters ----
#
# GENERAL
timezone <- "GMT"                           # time zone used for data's dates and also for date computations
tolerance.sec <- 10                         # date tolerance in seconds. A tolerance used when comparing dates
#
# RAW DATA PARAMETERS
flagcolname <- "flag"                       # name of a column name to filter raw data
keepFlags <- c("...", "..>")                # flags to keep in the raw data
#
# INTERPOLATION PARAMETER
limit.sp <- rgdal::readOGR(dsn = dirname(limit.shp), layer = strsplit(basename(limit.shp), split = '[.]')[[1]][[1]]) # shapefile used to intersect the trajectories
samerica.sp <- rgdal::readOGR(dsn = dirname(samerica.shp), layer = strsplit(basename(samerica.shp), split = '[.]')[[1]][[1]])
searchTranslation <- (2 * 24 * 3600) * (-1) # time offset for the trajectory (once over the sea) records to match station's data. i.e 2 days are (2 * 24 * 3600) * (-1) seconds
nsd <- 2                                    # number of +/- standard deviations used to filter the interpolated data into backgorund
maxfm.ppm <- 1.5                            # maximum number of units away from the central tendency
#
# TRAJECTORY PARAMETERS
backTrajTime <- (10 * 24 * 3600) * (-1)     # Time to modify the hysplit file search. 10 days into the past is (10 * 24 * 3600) * (-1)
keepAbove <- 0                              # keep trajectories above this height treshold
simHeaderLines <- 7                         # Number of lines to remove from the header of hysplit's simulation files
inbound.minx = -70                          # Filter trajectories which intersect west of this
inbound.maxx = NA
inbound.miny = NA
inbound.maxy = NA
plotAllTrajectories <- TRUE                # Are filtered trajectories included in plots?
#
# METEREOLOGICAL STATION DATA
name <- c("RPB", "ASC", "CPT")
lon <- c(-59.430, -14.400, 18.189)
lat <- c(13.162, -7.967, -34.352)
stations.df <- data.frame(name, lon, lat, stationfile)
rm(name, lon, lat, stationfile)
#
# PLOT
plot2file <- TRUE                           # plots stored as files. Use NA for not plotting
goldrat <- (1 + sqrt(5))/2                  # width - height proportion 
device <- "png"                             # image format for data plots
map.xlim <- c(-80, 0)                       # map's min & max longitude
map.ylim <- c(-45, 35)                      # map's min & max latitude
map.height <- 8                             # map image size
map.width <- map.height * goldrat           # map image size
sec.width <- map.width                      # crosssection map image size
sec.height <- sec.width / goldrat           # crosssection map image size
prof.height <- map.height                   # profile image size
prof.width <- prof.height / goldrat         # profile image size

#---- SCRIPT ----
flux.total.list <- list()
for(i in 1:length(rawdatafile.vec)){
  rawdatafile.path <- rawdatafile.vec[i]
  log4r::info(logger, paste("Processing raw data:", rawdatafile.path, sep = " "))
  #
  site <- unlist(strsplit(basename(rawdatafile.path), split = ".", fixed = TRUE))[1]
  gas <- unlist(strsplit(basename(rawdatafile.path), split = ".", fixed = TRUE))[2]
  base.path <-  file.path(test.path, "tmp", site, gas, fsep = .Platform$file.sep)
  #
  #---- Check directories ----
  log4r::debug(logger, "step 00 - Check directories")
  rawDataClean.path <- file.path(base.path, "rawDataFlag", fsep = .Platform$file.sep)
  hysplit.nohead.path <- file.path(base.path, "simNoHead", fsep = .Platform$file.sep)
  # create the missing folders
  folder.vec <- c(rawDataClean.path, hysplit.nohead.path)
  for(folder in folder.vec[!dir.exists(folder.vec)]){
    dir.create(folder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  }
  # delete the old files
  for(folder in folder.vec[dir.exists(folder.vec)]){
    files.vec <- list.files(path = folder, full.names = TRUE)
    file.remove(files.vec)
  }
  #
  #---- Drop columns and filter raw data ----
  log4r::debug(logger, "step 01 - Drop columns and filter raw data")
  filterRawfile <- cqmaTools::splitRawdata(file.in = rawdatafile.path, 
                                           path.out = rawDataClean.path, 
                                           colname = flagcolname, 
                                           keepFlags = keepFlags, 
                                           cnames = RAW.DATA.COLNAMES)
  wrongcoords <- filterRawfile[[3]]
  duplicatedRows <- filterRawfile[[2]]
  filterRawfile <- filterRawfile[[1]]
  if(nrow(wrongcoords)  > 0){
    log4r::warn(logger, paste("Wrong coords found in raw data file:", 
                              nrow(wrongcoords), "\n", cqmaTools::df2text(df = wrongcoords), 
                              sep  = " "))
  }
  if(nrow(duplicatedRows)  > 0){
    log4r::warn(logger, paste("Duplicated or inconsistent  rows in raw data file:", 
                              nrow(duplicatedRows), "\n", cqmaTools::df2text(df = duplicatedRows), 
                              sep  = " "))
  }
  #
  #---- Check Hysplit files ----
  log4r::debug(logger, "step 02 - Check Hysplit files")
  #
  #exec.file <- file.path(hysplit.exec.path, "hyts_std", fsep = .Platform$file.sep)
  #if(get_os() == "windows"){
  #  exec.file <- file.path(hysplit.exec.path, "hyts_std.exe", fsep = .Platform$file.sep)
  #}
  hysplit.gas.path <- file.path(hysplit.sim.path, gas, fsep = .Platform$file.sep)       # add gass to path of the hysplit files
  #if(file.exists(exec.file)){
  #  hysplit.files <- .runsimulation(path.in = rawDataClean.path, 
  #                                  backTrajTime = backTrajTime, 
  #                                  hysplit.exec.path = hysplit.exec.path, 
  #                                  hysplit.work.path = hysplit.work.path, 
  #                                  path.out = hysplit.gas.path, 
  #                                  timezone = timezone)
  #}else{
  #warn(logger, "Hysplit wasn't found. This script will try to continue...")
  if(length(list.files(hysplit.gas.path)) == 0){
    log4r::error(logger, paste("Unable to continue: No hysplit and no trajectory files in", hysplit.gas.path, sep = " "))
    stop()
    #}else{
    #  warn(logger, "Found trajectory files. This script is able to continue!")
    #}
  }
  #
  #---- Remove header from HYSPLIT files ----
  log4r::debug(logger, "step 03 - Remove header from HYSPLIT files")
  file.vec <- list.files(path = hysplit.gas.path, 
                         full.names = TRUE, 
                         pattern = paste(toupper(site), "_*", sep = ""), 
                         ignore.case = TRUE)
  if(length(file.vec) == 0){
    log4r::warn(logger, paste("No trajectories found for", rawdatafile.path, sep = " "))
    break
  }
  hysplit.nohead.files <- tryCatch({
    cqmaTools::removeHeaders(file.vec = file.vec, path.out = hysplit.nohead.path, 
                             skip = simHeaderLines, cnames = HYSPLIT.COLNAMES)
  }, error = function(e) {
    log4r::error(logger, "Error in stop 3. Something is wrong with HYSPLIT files")
  }, finally={
    # next
  })
  hysplit.nohead.files <- data.frame(as.vector(unlist(hysplit.nohead.files)), 
                                     rep(TRUE, times = length(hysplit.nohead.files)), 
                                     stringsAsFactors = FALSE)
  colnames(hysplit.nohead.files) <- c("file.vec", "keep")
  #
  #---- Check for trajectories that hit the gound ----
  log4r::debug(logger, "step 04 - Check for trajectories that hit the gound")
  # run the filter
  hysplit.traj.files <- cqmaTools::filterTrajHeight(
    file.vec = as.vector(unlist(
      hysplit.nohead.files[hysplit.nohead.files$keep == TRUE, "file.vec"]
    )), 
    above = keepAbove, cnames = HYSPLIT.COLNAMES)
  # merge results
  colnames(hysplit.traj.files) <- c("file.vec", "above" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, hysplit.traj.files, 
                                by = "file.vec", all = TRUE)
  # report
  if(sum(!hysplit.traj.files["above"]) > 0){
    warn(log4r::logger,
         paste("Some trajectories hit the gound: ", 
               sum(!hysplit.traj.files["above"]),  sep = "")
    )
    #debug(logger,paste("Some trajectories hit the gound: ", sum(!hysplit.traj.files["above"]), "\n", paste(hysplit.traj.files[!hysplit.traj.files["above"], 1], collapse = " \n"), sep = ""))
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["above"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  
  #
  #---- Intersect trajectories with the limit ----
  log4r::debug(logger, "step 05 - Intersect trajectories with limit.shp")
  traj.intersections <- cqmaTools::intersectTraj(file.vec = as.vector(unlist(
    hysplit.nohead.files[hysplit.nohead.files$keep == TRUE, "file.vec"]
  )), 
  limit.in = limit.sp, cnames = HYSPLIT.COLNAMES)
  #
  #---- Filter trajectories which do not reach the sea -----
  log4r::debug(logger, "step 06 - Filter trajectories which do not reach the sea")
  # run the filter
  traj.2thesea <- cqmaTools::trajreachthesea(traj.intersections = traj.intersections)
  # merge results
  colnames(traj.2thesea) <- c("file.vec", "sea" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, traj.2thesea, 
                                by = "file.vec", all = TRUE)
  # report
  traj.inland <- logical(length = length(traj.intersections[[1]]))
  if(nrow(traj.2thesea) > 0){
    traj.inland <- !traj.2thesea["sea"]
  }
  if(sum(traj.inland) > 0 | length(traj.inland) == 0){
    log4r::warn(logger, paste("Some trajectories don't reach the sea: ", 
                              sum(traj.inland), sep = ""))
    #debug(logger, paste("Some trajectories don't reach the sea: ", sum(traj.inland), " \n", paste(traj.2thesea[traj.inland, 1], collapse = " \n"), sep = ""))
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["sea"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  #
  #---- Filter trajectories falling out of bounds ----
  log4r::debug(logger, "step 07 - Filter trajectories falling out of bounds")
  # run the filter
  traj.inbound <- cqmaTools::trajinbound(traj.intersections = traj.intersections, 
                                         minx = inbound.minx, maxx = inbound.maxx, 
                                         miny = inbound.miny, maxy = inbound.maxy)
  # merge results
  colnames(traj.inbound) <- c("file.vec", "inBound" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, traj.inbound, 
                                by = "file.vec", all = TRUE)
  # report
  traj.out <- logical(length = length(traj.intersections[[1]]))
  if(nrow(traj.inbound) > 0){
    traj.out <- !traj.inbound["inBound"]
  }
  if(sum(traj.out, na.rm = TRUE) > 0 | length(traj.out) == 0){
    traj.out[is.na(traj.out)] <- FALSE
    log4r::warn(logger, paste("Some trajectories are out of bounds: ", 
                              sum(traj.out), sep = ""))
    #debug(logger, paste("Some trajectories are out of bounds: ", sum(traj.out), " \n", paste(traj.inbound[traj.out, 1], collapse = " \n"), sep = ""))
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["inBound"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  #
  #---- Filter trajectories falling out of the stations' range ----
  log4r::debug(logger, "step 08 - Filter trajectories falling out of the stations' range")
  # run the filter
  stations.df$stationfile <- paste(stations.df$stationfile, gas, "txt", sep = ".")
  traj.inStation <- cqmaTools::trajOutInterpolation(traj.intersections = traj.intersections, 
                                                    stations.df = stations.df)
  # merge results
  colnames(traj.inStation) <- c("file.vec", "inStation" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, traj.inStation, 
                                by = "file.vec", all = TRUE)
  # report
  traj.out <- logical(length = length(traj.intersections[[1]]))
  if(nrow(traj.inStation) > 0){
    traj.out <- !traj.inStation["inStation"]
  }
  if(sum(traj.out, na.rm = TRUE) > 0 | length(traj.out)  == 0){
    log4r::warn(logger, paste("Some trajectories are out of reach of stations: ", 
                              sum(traj.out), sep = ""))
    #debug(logger, paste("Some trajectories are out of reach of stations: ", sum(traj.out), " \n", paste(traj.inStation[traj.out, 1], collapse = " \n"), sep = ""))
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["inStation"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  #
  #---- Interpolate data for the trajectory's over-the-sea point to the metereological stations ----
  log4r::debug(logger, "step 09 - Interpolate data for the trajectory's over-the-sea point to the metereological stations")
  keepAaboveAseaAstation <- as.vector(unlist(hysplit.nohead.files["keep"]))
  traj.intersections[[1]] <- traj.intersections[[1]][keepAaboveAseaAstation]
  traj.intersections[[2]] <- traj.intersections[[2]][keepAaboveAseaAstation]
  traj.interpolations <- cqmaTools::crossdata(
    traj.intersections = traj.intersections, 
    stations.df = stations.df, 
    tolerance.sec = tolerance.sec, 
    timezone = timezone, 
    searchTranslation = searchTranslation
  )
  #
  #---- flush the filter summary of the trajectories -----
  t <- paste("\n", paste(colnames(hysplit.nohead.files), collapse = " "), sep = "")
  for(nr in 1:nrow(hysplit.nohead.files)){
    t <- paste(t, paste(hysplit.nohead.files[nr, ], collapse = " "), sep = "\n")
  }
  log4r::info(logger, t)
  #
  #---- Plot & save results ----
  log4r::debug(logger, "step 10 - Plot & save results")
  traj.plot <- NA
  if(plotAllTrajectories){
    traj.plot <- hysplit.nohead.files[hysplit.nohead.files$keep == FALSE, "file.vec"]
  }
  plot.result <- cqmaTools::plotTrajbackground(
    file.in = filterRawfile, 
    path.out = plot.path, 
    traj.plot = traj.plot, 
    traj.interpol = traj.interpolations, 
    traj.intersections = traj.intersections, 
    use.backgorund = use.backgorund, 
    device = device, 
    map.xlim = map.xlim, 
    map.ylim = map.ylim, 
    map.height = map.height, 
    map.width = map.width, 
    sec.width = sec.width, 
    sec.height = sec.height, 
    prof.height = prof.height, 
    prof.width  = prof.width, 
    nsd = nsd, 
    maxfm.ppm = maxfm.ppm, 
    stations.df = stations.df, 
    plot2file = plot2file,
    logger = logger, 
    trajCnames = HYSPLIT.COLNAMES, 
    obsCnames = RAW.DATA.COLNAMES.KEEP, 
    profileCnames = PROFILE.COLNAMES
  )
  plot.files <- plot.result[[1]]
  profile.df <- plot.result[[2]]
  #
  #---- plot trajectories by year ----
  log4r::debug(logger, "step 11 - plot trajectories by year")
  if(nrow(hysplit.nohead.files) > 0){
    siteyearplot.list <- cqmaTools::plotTrajYear(file.vec = as.vector(unlist(hysplit.nohead.files["file.vec"])), 
                                                 path.out = plot.path, 
                                                 device = device, 
                                                 map.xlim = map.xlim, 
                                                 map.ylim = map.ylim, 
                                                 map.height = map.height, 
                                                 map.width = map.width, 
                                                 stations.df = stations.df, 
                                                 plot2file = plot2file)
  }
  #
  #---- Time to the sea ----
  log4r::debug(logger, "step 12 - time to the sea")
  traj.intersectionsSA <- cqmaTools::intersectTraj(
    file.vec = as.vector(
      unlist(
        hysplit.nohead.files[, "file.vec"]
      )
    ), 
    limit.in = samerica.sp, 
    cnames = HYSPLIT.COLNAMES
  )
  intersect_rows <- unlist(
    lapply(traj.intersectionsSA[[2]], function(x){
      res <- NA
      if(!is.null(x)){
        res <- as.integer(rownames(x))
      }
      return(res)
    })
  )
  trajtime <- cqmaTools::computeTrajTime(file.vec = traj.intersectionsSA[[1]],  # total time of the trajectory over the main land
                                         line.vec = intersect_rows, 
                                         cnames = HYSPLIT.COLNAMES)
  trajtime.df <- data.frame(basename(traj.intersectionsSA[[1]]), 
                            as.double(trajtime), stringsAsFactors = FALSE)
  colnames(trajtime.df) <- c("file.vec", "trajtime.days")
  profile.df <- merge(profile.df, trajtime.df, by = "file.vec", all = TRUE)
  profile.list <- lapply(split(profile.df, f = as.factor(profile.df$profile)), 
                         function(x){
                           amean <- mean(as.double(x$trajtime), na.rm = TRUE)
                           x$trajtime[is.na(x$trajtime)] <- amean
                           return(x)
                         })
  profile.df <- do.call(rbind, profile.list)
  #
  #---- Write result table ----
  log4r::debug(logger, "step 13 - Write result table")
  write.table(profile.df, file = file.path(data.out.path, paste(basename(rawdatafile.vec), "_bkgTable.txt", sep = ""), fsep = .Platform$file.sep))
}
#
#---- END OF SCRIPT ----
log4r::debug(logger, "END OF SCRIPT")
