# util.R


# =======================================================================================
# PACKAGE UTIL 
# =======================================================================================






# Read text files into data.frames (one per file)
#
# @param file.vec A vector of character. The paths to the input files
# @param header A logical. Do the files have a header row?
# @param skip A numeric. Lines to skip from the top of the file
# @param cnames A vector of character. The column names of the data in the files
# @return A list of data.frames. The list names matches the file names
.files2df <- function(file.vec, header, skip, cnames){
  res <- list()
  if(length(file.vec) == 0){
    warning("Empty list")
    return(res)
  }
  for(i in 1:length(file.vec)){
    res[[i]] <- .file2df(
      file.in = file.vec[i],
      cnames = cnames, 
      header = header, 
      skip = skip)
  }
  names(res) <- basename(file.vec)
  return(res)
}



# Read a text file made of observations
#
# @param file.in A character. Path to a text file with data
# @param header A logical. Does the first column contain column names?
# @param skip A numeric. Number of lines to skip
# @param cnames A vector character. The names of the columns
# @return A data.frame
.file2df <- function(file.in, header, skip, cnames){
  file.dat <- utils::read.table(file = file.in, sep = "", header = header, skip = skip)
  if(!header){colnames(file.dat) <- cnames}
  return(file.dat)
}



# Filter a data.frame by columns and a row
#
# @param df A data.frame.
# @param keepCols A character vector. The column names to keep in the result data.frame
# @param flagName A character. The name of the column with the flags
# @param keepFlags A character vector. The flags which mark the rows to keep
# @return A filtered data.frame
.filterDataframe <- function(df, keepCols, flagName, keepFlags){
  df <- df[which(df$flag %in% keepFlags), ] # filter using flags
  df <- df[keepCols]                    # subset and reorder columns
  return(df)
}



# Get the week number respect the month of the given date
#
# @param adate A charactrer in a format parseable by as.POSIXlt
# @param weekfirstday A numeric value indicating which is the first day of the week. The defaul is 1, that is Monday
# @param tz A character representing a time zone. The default is GMT
# @return A numeric value representing the week of the month of the given date. The first week is the number 1
.monthWeek <- function(adate, weekfirstday = 1, tz = "GMT"){
  d <- as.POSIXlt(adate, tz = tz)           # tested date
  s <- as.POSIXlt(paste(strftime(d, "%Y"), strftime(d, "%m"), "01", sep = "-"), tz = tz) # first day of the month
  tdif <- as.numeric(d - s)                 # dates difference in days
  ddow <- strftime(s, "%u")                 # day of the week of first day of the month
  if(7 - as.numeric(ddow) >= tdif){return(1)} # first week
  wday <- vector()
  start <- as.numeric(ddow) + 1
  for(i in 1:tdif){
    wday <- append(wday, start)
    start <- start + 1
    if(start > 7) start <- 1
  }
  return(as.vector(table(wday)[weekfirstday] + 1))
}



# Test in which interval are the given value 
#
# @val A numeric vector of the values to test
# @vec A numeric vector with the limits of the consecutive intervals. Its length must be greater than 1
# @return A boolean matrix. The columns represent the interval's limits (vec) and the rows the values (val). The number of intervals is the number of element in vec minus one
.inInterval <- function(val, vec){
  if(is.null(val)){return(matrix(ncol = 2, nrow = 0))}
  int.mat <- matrix(NA, ncol = 2, nrow = length(vec) - 1, byrow = TRUE) # interval matrix
  # build a test matrix
  for(i in 2:length(vec)){
    int.mat[i - 1, ] <- c(vec[i - 1], vec[i])
  }
  res <- lapply(val, function(x, mat){return(x >= mat[, 1] & x <= mat[, 2])}, mat = int.mat)
  res <- matrix(unlist(res), ncol = nrow(int.mat), nrow = length(val), byrow = TRUE)
  colnames(res) <- paste(rep("int", nrow(int.mat)), 1:nrow(int.mat), sep = "")
  rownames(res) <- paste(rep("val", length(val)), 1:length(val), sep = "")
  return(res)
}



# Transforms a decimal year date to a date-format string
#
# @param year.dec A number repesenting a date as a decimal year. e.g. 2000.0013661202
# @return A character. e.g. 2000-01-01 11:59:59.999410
.ydec2date <- function(year.dec){
  daysperyear <- 365 # 365.25
  y <- floor(year.dec)
  m <- 0
  ss <- 0
  leap <- .isLeapYear(y)
  firstday <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366) + as.numeric(c(FALSE, FALSE, rep(leap, 11))) # first day of each month
  frac <- year.dec - y
  frac.secs <- frac * (daysperyear + as.numeric(leap)) * 24 * 3600 # fraction of seconds
  frac.days <- frac.secs / 3600 / 24                          # fraction of days
  # get the month
  for (i in 1:(length(firstday) - 1)){
    start <- firstday[i]
    end <- firstday[i + 1]
    if(ceiling(frac.days) >= start && ceiling(frac.days) < end){
      m <- i
      break
    }
  }
  # get the day of the month
  d <- ceiling(frac.days) - firstday[m] + 1
  #
  frac.hours <- frac.secs / 3600 - floor(frac.days) * 24
  frac.mins <- (frac.hours - floor(frac.hours)) * 60
  ss <- frac.secs - (floor(frac.days) * 24 * 3600) - (floor(frac.hours) * 3600) - (floor(frac.mins) * 60)
  return(paste(  
    paste(y, sprintf("%02d", m), sprintf("%02d", d), sep = "-"), 
    paste(sprintf("%02d", floor(frac.hours)), sprintf("%02d",floor(frac.mins)), sprintf("%02f",ss), sep = ":"), 
    sep = " "
  ))
}



# Transforms a date decimal year. 
# NOTE: use formatC to get more decimals from the result. i.e formatC(.date2ydec("2000-01-01 11:59:59.999410"), digits = 10, format="f")
#
# @param aDate A date in a format supported by as.POSIXlt, i.e "2000-01-01 11:59:59.999410"
# @param timezone A character. The time zone. i.e. "GMT"
# @return A number e.g. 2000.0013661202
.date2ydec <- function(adate, timezone){
  daysperyear <- 365 # 365.25
  d <- as.POSIXlt(adate, tz = timezone)
  y <- d$year + 1900
  leap <- .isLeapYear(y)
  firstday <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366) + as.numeric(c(FALSE, rep(leap, 12))) # first day of each month
  return(y + (((firstday[(d$mon + 1)] - 1) * 24 * 3600) + ((d$mday - 1) * 24 * 3600) + (d$hour * 3600) + (d$min * 60) + d$sec) / ((daysperyear + as.numeric(leap)) * 24 * 3600))
}



# Check wether the given year is leap or not
#
# @param year A numeric vector
# @return A boolean vector
.isLeapYear <- function(year){
  (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
}



# Interpolate the value of a point over a line
#
# @param line.xyv A line represented by a 2-row 3-column matrix (x, y value)
# @param point.xy A point on the line represented by 2-element vector (x, y)
# @return The interpolated value for the point
.lineInterpolation <- function(line.xyv, point.xy){
  dl <- as.vector(stats::dist(line.xyv[, 1:2]))                      # line's length
  hl <- line.xyv[2, 3] - line.xyv[1, 3]                       # value interval
  dp1 <- as.vector(stats::dist(rbind(line.xyv[1, 1:2], point.xy)))   # distance from line' start to point
  dp2 <- as.vector(stats::dist(rbind(line.xyv[2, 1:2], point.xy)))   # distance from line' end to point
  dp <- dp1/(dp1 + dp2)                                       # normalized distance from line' start to point
  return(line.xyv[1, 3] + (dp * hl))
}




# =======================================================================================
# PACKAGE RELATED 
# =======================================================================================



# Split a single raw data file. The output files are prefixed with "traj_specs_".
#
# @param file.in Path to a raw data file
# @param path.out Path to the folder for storing the resulting files
# @param colname A character. The name of a column where the flags are located
# @param keepFlags A character vector. Flags to keep in the raw data. i.e. c("...", "..>")
# @return A character. The name of the new file
.splitRawdata <- function(file.in, path.out, colname, keepFlags){
  cnames <- c("site", "year", "month", "day", "hour", "min", "flask", "V8", "concentration", "flag", "V11", "ayear", "amonth", "aday", "ahour", "amin", "lat", "lon", "height", "eventnumber", "flat", "flon", "fheight")
  file.dat <- .file2df(file.in = file.in, header = FALSE, skip = 0, cnames = cnames)
  # filter data
  keepCols <- c("site", "lat", "lon", "height", "year", "month", "day", "hour", "min", "flask", "concentration")
  file.dat <- .filterDataframe(df = file.dat, keepCols = keepCols, flagName = colname, keepFlags = keepFlags)
  # report duplicated rows
  testUnique.vec <- apply(file.dat[, c("site", "height", "year", "month", "day", "hour", "min", "flask")], 
                          MARGIN = 1, 
                          function(x){
                            gsub(" ", "0", paste(unlist(x), collapse = "___"))
                          })
  testUnique.df <- as.data.frame(table(as.vector(testUnique.vec)), stringsAsFactors = FALSE)
  dup.df <- testUnique.df[testUnique.df$Freq > 1, ]
  if(nrow(dup.df) > 0){
    for(ri in 1:nrow(dup.df)){
      warning(paste("Inconsistent row:",dup.df[ri, 1], sep = ""))
    }
    #stop("Inconsistent rows in raw data")
  }
  # store the results
  newfile <- file.path(path.out, paste("traj_specs_", basename(file.in), sep = ""), fsep = .Platform$file.sep)
  utils::write.table(file.dat, file = newfile, col.names = FALSE, row.names = FALSE, quote = FALSE)
  return(newfile)
}



# Get a GDAS file name from a row of a traj_spec file
#
# @param trajspecs.row A data.frame with a single row of a traj_spec file. It must contain at least the columns c("yr", "mo", "da")
# @param A character with the name of a GDAS file. i.e. "gdas1.feb15.w4"
.buildGDASfilename <- function(trajspecs.row, timezone){
  # get the week of the month
  adate  <- paste(unlist(trajspecs.row["yr"]), unlist(trajspecs.row["mo"]), unlist(trajspecs.row["da"]), sep = "-")
  wom <- .monthWeek(adate = adate, weekfirstday = 1, tz = timezone)
  adate <- as.POSIXlt(adate, tz  = timezone)
  # build the filename
  m <- tolower(month.abb[unlist(trajspecs.row["mo"])])
  return(paste("gdas1.", m, strftime(adate, "%y"), ".w", wom, sep = ""))
}



# Run HYSPLIT simulations for the traj_spec files in the given path
#
# @param path.in A character representing a folder with traj_spec files
# @param backTrajTime numeric. Time to modify the hysplit file search. For example, 10 days to the past is (10 * 24 * 3600) * (-1)
# @param hysplit.exec.path path to the HYSPLIT's exec folder.
# @param hysplit.work.path path to the HYSPLIT's working folder.
# @param path.out A character. The path to store the resulting files. Usually HYSPLIT's simulations folder.
# @param timezone A character. The time zone used for date computations.i.e. GMT
# @return A character list of the created files
.runsimulation <- function(path.in, backTrajTime, hysplit.exec.path, hysplit.work.path, path.out, timezone){
  exec.file <- file.path(hysplit.exec.path, "hyts_std", fsep = .Platform$file.sep)
  #if(get_os() == "windows"){
    #exec.file <- file.path(hysplit.exec.path, "hyts_std.exe", fsep = .Platform$file.sep)
  #}
  if(!file.exists(exec.file)){
    stop("ERROR: Hysplit's executables not found")
  }
  files.in  <- list.files(path = path.in, full.names = TRUE)
  for(i in 1:length(files.in)){
    file.out <- .runsim(file.in = files.in[i], 
                        backTrajTime = backTrajTime, 
                        MDL = hysplit.exec.path, 
                        MET = hysplit.work.path, 
                        SIM = path.out, 
                        timezone = timezone)
    files.out[[i]] <- file.out
  }  
  return(files.out)
}



# Run HYSPLIT simulation for a single traj_specs file and store the results
# This function replaces the bash script batch.sh
#
# @param file.in Path to a traj_specs_* file
# @param backTrajTime numeric. Time to modify the hysplit file search. For example, 10 days to the past is (10 * 24 * 3600) * (-1)
# @param MDL path to the HYSPLIT's exec folder.
# @param MET path to the HYSPLIT's working folder.
# @param SIM path to the HYSPLIT's simulations folder.
# @param timezone A character. The time zone used for date computations.i.e. GMT
# @return A character list with the result files
# @example
# ## filter raw data
# #rawdatafiles.path <- "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/01"
# #rawDataClean.path <- "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/03/test"
# #rawDataClean.files <- unlist(.splitRawdata(path.in = rawdatafiles.path, path.out = rawDataClean.path))
# #backTrajTime <- (10 * 24 * 3600) * (-1)
# #MDL <- "/Users/manuel/Documents/Models/Hysplit4/exec/"
# #MET <- "/Users/manuel/Documents/Models/Hysplit4/working/"
# #SIM <- "/Users/manuel/Documents/Models/Hysplit4/simulations/"
# #.runsims(file.in = rawDataClean.files[1], MDL = MDL, MET = MET, SIM = SIM)
.runsim <- function(file.in, backTrajTime, MDL, MET, SIM, timezone){
  cnames <- c("site", "lat", "lon", "height", "yr", "mo", "da", "hr", "mm", "flask")
  file.dat <- .file2df(file.in = file.in, header = FALSE, skip = 0, cnames = cnames)
  # format columns
  file.dat["mof"] <- sprintf("%02d", unlist(file.dat["mo"]))
  file.dat["daf"] <- sprintf("%02d", unlist(file.dat["da"]))
  file.dat["hrf"] <- sprintf("%02d", unlist(file.dat["hr"]))
  file.dat["mmf"] <- sprintf("%02d", unlist(file.dat["mm"]))
  # get the file names of the metereological data file
  met_files <- unlist(lapply(1:nrow(file.dat), function(x, file.dat){return(.buildGDASfilename(file.dat[x, ], timezone = "GMT"))}, file.dat = file.dat))
  # get the file names of the metereological data file backTrajTime seconds before the trajectory
  met_filesp1 <- unlist(lapply(1:nrow(file.dat), 
                               function(x, file.dat, backTrajTime, timezone){
                                 # get row's date
                                 adate <- as.POSIXlt(paste(unlist(file.dat[x, ]["yr"]), 
                                                           unlist(file.dat[x, ]["mo"]), 
                                                           unlist(file.dat[x, ]["da"]), 
                                                           unlist(file.dat[x, ]["hr"]), 
                                                           unlist(file.dat[x, ]["mm"]), 
                                                           sep = "-"), tz = timezone)
                                 # date backTrajTime seconds before or after the trajectory
                                 adate <- adate + backTrajTime
                                 # replace the old date
                                 tmprow <- file.dat[x, ]
                                 tmprow["yr"] <- as.numeric(strftime(adate, "%Y"))
                                 tmprow["mo"] <- as.numeric(strftime(adate, "%m"))
                                 tmprow["da"] <- as.numeric(strftime(adate, "%d"))
                                 tmprow["hr"] <- as.numeric(strftime(adate, "%H"))
                                 tmprow["mm"] <- as.numeric(strftime(adate, "%M"))
                                 # build the file names
                                 return(.buildGDASfilename(tmprow, timezone = timezone))
                               }, file.dat = file.dat, backTrajTime = backTrajTime, timezone = timezone))
  #
  control.files <- apply(cbind(file.dat, met_files, met_filesp1), 1, .runsim.buildcontrol, MET = MET)
  res <- list()
  for(i in 1:length(control.files)){
    # 1 - create CONTROL file
    
    
    
    
    #grep("//", control.files[[i]], fixed = TRUE, replacement = "\\")
    #gsub("\\", "", str, fixed=TRUE)
    
    write(control.files[[i]], file = file.path(MDL, "CONTROL", fsep = .Platform$file.sep)) # write a CONTROL file to directory where trajectory code executable resides
    
    
    
    # 2 - go to exec directory and run simulation   
    row.file.dat <- file.dat[i ,]
    file.out <- paste(as.character(unlist(row.file.dat["site"])), row.file.dat["yr"], row.file.dat["mof"], row.file.dat["daf"], row.file.dat["hrf"], row.file.dat["height"], sep = "_")
    print(paste("Simulation:", file.out, "\n"))
    system(file.path(MDL, "hyts_std", fsep = .Platform$file.sep), wait = TRUE)
    # 3 rename and move to trajectory storage directory
    path.file.res <- file.path(MDL, file.out, fsep = .Platform$file.sep)
    path.file.out <- file.path(SIM, file.out, fsep = .Platform$file.sep)
    system(paste("mv ", path.file.res, path.file.out), wait = TRUE)
    res[[i]] <- path.file.out
  }
  return(res)
}



# Build the content of a hysplit control file
#
# @param x
# @param MET path to the HYSPLIT's working folder.
# @return A character. The contents of a hysplit CONTROL file
.runsim.buildcontrol <- function(x, MET){
  # Explanation
  #------------
  # Starting time                               year month day hour
  # Number of starting locations                N
  # Starting location                           lat  lon  height(m)
  # Total runtime                               (+-) M (hr)
  # Vertical motion option                      0 data   1 isob   2 risen  3 ….
  # Top of model domain (m)                     e.g. 10000.0
  # Number of simultaneous input met fields     usually 1
  # Meteorological data grid # 1 directory      \main\sub\data\
  # Meteorological data grid # 1 filename       file_name
  
  # Example 
  #--------
  # 13 03 18 00        # Starting time
  # 1                  # Number of starting locations
  # 40.0 -90.0 1500.0  # lat lon height(m)
  # -48                # Total runtime
  # 0                  # Vertical motion option
  # 10000.0            # Top of model domain (m)
  # 1                  # Number of simultaneous input met fields
  # ../working/        # Meteorological data grid # 1 directory
  # RP201303.gbl       # Meteorological data grid # 1 filename
  # ./
  # tdump_001
  paste(
    paste(x["yr"], x["mof"], x["daf"], x["hrf"], "\n"),
    paste(1, "\n"),
    paste(x["lat"], x["lon"], x["height"], "\n"),
    paste(-240, "\n"),
    paste(0, "\n"),
    paste("10000.0", "\n"),
    paste(2, "\n"),
    paste(MET, "\n"),
    paste(x["met_files"], "\n"),
    paste(MET, "\n"),
    paste(x["met_filesp1"], "\n"),
    paste("./", "\n"),
    paste("tdump_001", "\n"),
    sep = ""
  )
}



# Remove headers from files
#
# @param file.vec A character vector. The paths to the input files
# @param path.out Path to the folder for storing the resulting files
# @param skip Numeric. Number of lines to remove from the beginning of the file
# @return A list of the created file paths
.removeHeaders <- function(file.vec, path.out, skip){
  cnames <- c("V1", "V2", "year", "month", "day", "hour", "min", "V8", "V9", "lat", "lon", "height", "pressure") # column names of hysplit files
  file.dat.list <- .files2df(file.vec = file.vec,  header = FALSE, skip = skip, cnames = cnames)
  res <- list()
  for(i in 1:length(file.dat.list)){
    file.dat <- file.dat.list[[i]]
    newfile <- file.path(path.out, names(file.dat.list)[[i]], fsep = .Platform$file.sep)  
    utils::write.table(file.dat, file = newfile, col.names = FALSE, row.names = FALSE, quote = FALSE)
    res[[i]] <- newfile
  }  
  return(res)
}



# Build a SpatialLines object from a trajectory data.frame. Each line is identified by its first row in the data.frame
#
# @param traj.dat data.frame with the trajectory. It uses the format of Hysplit output files
# @param crs A CRS object. It is the coordinate reference system of the data
# @return A SpatialLines object.
.traj2spLines <- function(traj.dat, crs){
  Lines.list <- list()
  for (i in 2:nrow(traj.dat)){
    l <- sp::Line(rbind(traj.dat[i - 1,][c("lon", "lat")], traj.dat[i,][c("lon", "lat")]))
    Lines.list[[i - 1]] <- sp::Lines(l, as.character(i - 1))
  }
  return(sp::SpatialLines(Lines.list, proj4string = crs))
}



# Invert the parameters of rgeos::gIntersection
#
# @param g2 SP object
# @param g1 SP object
# @param byid Logical vector to pass to rgeos::gIntersection
# @return The intersection resutl as SP objects
.intersectTraj.intersect <- function(g2, g1, byid){
  return(rgeos::gIntersection(spgeom1 = g1, spgeom2 = g2, byid = byid))
}



# Find trajectories' first point over the sea.
#
# @param file.vec A character vector. The paths to the input files
# @param limit.in A SpatialLinesDataFrame object which is used to intersect the trajectories.
# @return A list made of a character vector and a list. The character vector is the path to each trajectory file. The list contains the first row in the trajectory file which lies over the sea
# @examples
# #shapefile.in <- "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/shp/continentalSouthAmericaLines.shp"
# #samerica <- readOGR(dsn = dirname(shapefile.in), layer = strsplit(basename(shapefile.in), split = '[.]')[[1]][[1]])
# #path.in <- "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/07"
# #traj.intersections <- .intersectTraj(path.in = path.in, limit.in = samerica)
.intersectTraj <- function(file.vec, limit.in){
  #TODO: constants
  wgs84 <-  sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  cnames <- c("V1", "V2", "year", "month", "day", "hour", "min", "V8", "V9", "lat", "lon", "height", "pressure") # column names of the input file  
  intersect.dat <- list()
  if(length(file.vec) == 0){warning("No input files!"); return(list(file.vec, intersect.dat))}
  traj.dat.list <- .files2df(file.vec = file.vec, header = FALSE, skip = 0, cnames = cnames)
  traj.spl.list <- parallel::mclapply(traj.dat.list, .traj2spLines, crs = wgs84) # get SpatialLines from trajectories
  traj.intersect.list <- parallel::mclapply(traj.spl.list, .intersectTraj.intersect, g1 = limit.in, byid = c(FALSE, TRUE)) # intersetion of trajectories with the limit
  traj.rowid.list <- parallel::mclapply(traj.intersect.list, function(x){if(is.null(x)){return(NULL)};return(as.numeric(rownames(slot(x, "coords"))[1]))}) # row id so the initial point of the intersection line in the trajectory
  # get the data from the intersection
  intersect.dat <- parallel::mclapply(1:length(traj.dat.list),
                                      function(x, traj.list, rowid.list){
                                        if(is.null(rowid.list[[x]])){return(NULL)}; 
                                        return(traj.list[[x]][rowid.list[[x]] + 1, ])}, # return the next id after the intersection. This corresponds to the line's point falling on the sea
                                      traj.list = traj.dat.list,rowid.list = traj.rowid.list)
  return(list(file.vec, intersect.dat))
}



# Helper function for crossdata. Take the trajectories data and interpolates its matching value according to observations
#
# @param x A number indexing the current trajectory
# @param traj.records A data.frame of the trajectories' first rows over the sea. One row per trajectory
# @param traj.data.match A boolean matrix of the traj.records versus intervals. The intervals' limites are the ordered latitudes of the stations
# @param stations A data.frame made of the name, longitude, latitude, and data file path of each of station. i.e c("name","lon", "lat", "file"). The data frame must be ordered by latitude to match the intervals in traj.data.match
# @param stations.dat A list of the data on each file of stations["file"]
# @param searchTranslation A number used to modify records' search on station data (in seconds).
# @param timezone A character. The time zone. i.e. "GMT"
# @param tolerance.sec A numeric. A tolerance used when comparing dates
# @return A number resulting from interpolating the station's data to the latitude of traj.records[x, ] 
.crossdata.aux <- function(x, traj.records, traj.data.match, stations, stations.dat, searchTranslation, timezone, tolerance.sec){
  res <- NA
  # get trajectory's date
  d <- unlist(traj.records[x, c("year", "month", "day", "hour", "min")])
  trajdate <- paste(
    paste(d["year"] + 2000, sprintf("%02d", d["month"]), sprintf("%02d", d["day"]), sep = "-"), 
    paste(sprintf("%02d", d["hour"]), sprintf("%02d", d["min"]), "00", sep = ":"),
    sep = " "
  )
  d.noon <- paste(strsplit(trajdate, split = " ")[[1]][1], "12:00:00", sep = " ") # approximate date to noon
  trajdate <- as.POSIXct(d.noon, tz = "GMT")
  trajdate <- trajdate + searchTranslation                                        # modify the search date by searchTranslation seconds
  # get the matching stations
  interval <- match(TRUE, traj.data.match[x, ])                                   # get the first interval in which the trajectory record falls
  if(!is.na(interval)){
    stations.match <-  stations[interval:(interval + 1), ]
    # cast character to date
    stations.dat.match <- stations.dat[interval:(interval + 1)]
    stations.dat.match[[1]]["date"] <- as.POSIXct(unlist(stations.dat.match[[1]]["date"]), tz = timezone)
    stations.dat.match[[2]]["date"] <- as.POSIXct(unlist(stations.dat.match[[2]]["date"]), tz = timezone)
    # filter stations's data by date
    stations.rows <- rbind( # first row matching the trajectory date on each station data file
      stations.dat.match[[1]][stations.dat.match[[1]]$date > (trajdate - tolerance.sec) & stations.dat.match[[1]]$date < (trajdate + tolerance.sec), ][1,], 
      stations.dat.match[[2]][stations.dat.match[[2]]$date > (trajdate - tolerance.sec) & stations.dat.match[[2]]$date < (trajdate + tolerance.sec), ][1,]  
    )
    if(sum(is.na(stations.rows)) == 0){
      toInterpol <- cbind(stations.match, stations.rows)  
      res <- .lineInterpolation(
        line.xyv = as.matrix(toInterpol[, c("lon", "lat", "V2")]), 
        point.xy = traj.records[x, c("lon", "lat")]
      )
    }
  }
  # interpolate toInterpol' latitude to get the value on record's latitutde
  return(res)
}



# Check if the trajectories' intersections fall inside the metereological station coverage
#
# @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
# @param stations.df A data.frame of metereological station data. It must have at least 4 columns: name, longitude, latitude, and file c("name", "lon", "lat", "file")
# @return A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if meet the test
.trajOutInterpolation <- function(traj.intersections, stations.df){
  stations.df <- stations.df[order(stations.df$lat),]                           # sort by latitude. This is mandatory
  file.vec <- unlist(traj.intersections[[1]])                                   # The first list contains the paths to trajectory files
  trajrecords.df <- do.call("rbind", traj.intersections[[2]])                   # The second list contains the intersections. That is, the trajectories' first row over the sea. One row per trajectory
  matchInterval <- .inInterval(val = unlist(trajrecords.df["lat"]),              # which stations should be used for each trajectory interpolation? . This is the match of trajectories to stations for interpolation 
                               vec = unlist(stations.df["lat"]))
  keep <- matchInterval[,1] == TRUE | matchInterval[,2] == TRUE
  res <- data.frame(cbind(file.vec, keep), stringsAsFactors = FALSE)
  res["keep"] <- as.logical(res$keep)                                           # R, you motherfuking piece of shit! 
  rownames(res) <- NULL
  return(res)
}



# Get the stations' data matching the trajectory records. It takes a trajectory's first point over the sea and project its latitude to an straight line made by the corresponding metereological stations
#
# @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
# @param stations.df A data.frame of metereological station data. It must have at least 4 columns: name, longitude, latitude, and file c("name", "lon", "lat", "file")
# @param tolerance.sec A numeric. A tolerance used when comparing dates
# @param timezone A character. The time zone. i.e. "GMT"
# @param searchTranslation A numeric. The number of seconds for searching metereological station data
# @return A list of numeric. Each number is the interpolation result from the matching stations
# @example
# #name <- c("RPB", "ASC", "CPT")
# #lon <- c(-59.430, -14.400, 18.189)
# #lat <- c(13.162, -7.967, -34.352)
# #file <- c("/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/stations/rpbdaily.co2.txt", "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/stations/ascdaily.co2.txt", "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/stations/cptdaily.co2.txt")
# #stations.df <- data.frame(name, lon, lat, file)
# #stations.df <- data.frame(name, lon, lat, file)
# #samerica.shp <- "/home/user/continentalSouthAmericaLines.shp"
# #samerica <- readOGR(dsn = dirname(samerica.shp), layer = strsplit(basename(samerica.shp), split = '[.]')[[1]][[1]])
# #traj.intersections <- .intersectTraj(path.in = hysplit.nohead.path, limit.in = samerica, tolerance.sec = 10, timezone = "GMT")
.crossdata <- function(traj.intersections, stations.df, tolerance.sec, timezone, searchTranslation){
  interpolation.res <- list()
  stations.df <- stations.df[order(stations.df$lat),]                           # sort by latitude. This is mandatory
  if(length(traj.intersections[[1]]) == 0){return(interpolation.res)}
  if(length(traj.intersections[[2]]) == 0){return(interpolation.res)}
  # filter latitudes
  # joins all the data.frames from records into a single one. 
  # NOTE1: it assumes all the data.frames have the same columns, in the same order
  # NOTE2: it assumes a single row on each record. Otherwise the following calculations are WRONG
  trajfiles <- unlist(traj.intersections[[1]])                                  # The first list contains the paths to trajectory files
  trajrecords.df <- do.call("rbind", traj.intersections[[2]])                   # The second list contains the intersections. That is, the trajectories' first row over the sea. One row per trajectory
  matchInterval <- .inInterval(val = unlist(trajrecords.df["lat"]), vec = unlist(stations.df["lat"])) # which stations should be used for each trajectory interpolation? . This is the match of trajectories to stations for interpolation
  out <- matchInterval[,1] == FALSE & matchInterval[,2] == FALSE
  if(sum(out) > 0){
    warning("Trajectories falling out of interpolation zone:", sum(out), " \n", paste(trajfiles[out], collapse = " \n"))
  }
  # read station's data from files
  station.dat.list <- list()
  for(i in 1:nrow(stations.df)){
    station.dat <- utils::read.table(file = as.character(stations.df[i, "stationfile"]), sep = "", header = FALSE)
    colnames(station.dat) <- c("datedec", "V2")
    station.dat.list[[i]] <- station.dat
  }
  # add a column with normal dates intead of decimal year dates
  station.dat.list <- parallel::mclapply(station.dat.list, function(x){x["date"] <- unlist(lapply(unlist(x["datedec"]), .ydec2date)); return(as.data.frame(x))}) # stations' data
  # do the interpolation
  interpolation.res <- lapply(
    1:length(trajfiles), 
    .crossdata.aux, 
    traj.records = trajrecords.df, 
    traj.data.match = matchInterval, 
    stations = stations.df, 
    stations.dat = station.dat.list, 
    searchTranslation = searchTranslation, #-1 * (2 * 24 * 3600), # match station's data two days before the records' data
    timezone = timezone, 
    tolerance.sec = tolerance.sec
  )
  return(interpolation.res)
}



# Check if the trajectories are above the given treshold
#
# @param file.vec A character vector. The paths to the input files
# @param above A numeric treshold
# @return A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if they meet the test
.filterTrajHeight <- function(file.vec, above){
  # check trajectories' height and make a vector of those to keep
  cnames <- c("V1", "V2", "year", "month", "day", "hour", "min", "V8", "V9", "lat", "lon", "height", "pressure") # column names of the input file    
  file.dat.list <- .files2df(file.vec = file.vec, header = FALSE, 
                             skip = 0, cnames = cnames)
  keep <- vector(mode = "logical", length = length(file.dat.list))
  keep <- lapply(file.dat.list, function(x){if(sum(x$height < above) > 0){return(FALSE)}; return(TRUE)}) # test
  keep <- as.vector(unlist(keep))
  return(data.frame(file.vec, keep))
}



# Check if the trajectories reach to the sea
#
# @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
# @return A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if they were kept
.trajreachthesea <- function(traj.intersections){
  file.vec <- unlist(traj.intersections[[1]])
  trajintersect.list <- traj.intersections[[2]]
  keep <- logical()
  if(length(file.vec) == 0){warning("No input files!"); return(data.frame(file.vec, keep))}
  # get the files with at least one row, that is, the trajectories which reach to the sea
  keep <- unlist(lapply(trajintersect.list, is.null))
  keep <- !keep
  return(data.frame(file.vec, keep))
}



# Add a column with a human-readable date to a file with metereological data
#
# @param file.in A character. Path to a metereological station data file
# @param file.out A character. Path to the resulting file
.addData2Station <- function(file.in, file.out){
  cnames <- c("datedec", "concentration")
  file.dat <- .file2df(file.in = file.in, header = FALSE, skip = 0, cnames = cnames)
  file.dat["normdec"] <- unlist(lapply(unlist(file.dat["datedec"]), function(x){return(.ydec2date(x))}))
  utils::write.csv2(file.dat, file = file.out, quote = TRUE)
}



# Write intersection list to a file
#
# @param traj.intersections A list of two obejcts. A character vector and a list of data.frames. The data.frames are made of the rows in the matching trajectory file which first crossed to the sea
# @param file.out A character. The path to the resulting file
.writeTrajIntersections <- function(traj.intersections, file.out){
  filepaths <- traj.intersections[[1]]
  filerows <- traj.intersections[[2]]
  row.df <- do.call("rbind", filerows)
  res <- cbind(filepaths, row.df)
  utils::write.csv2(res, file = file.out)
}



# Mark the outliers in the given data vector
#
# @param data.vec A numeric vector. The data to test
# @param nsd A numeric. The number of standard deviations 
# @param nsd A logical. Must the median be used instead of the mean?
# @return A logical vector
.isOutlier <- function(data.vec, nsd, use.median){
  dm <- mean(data.vec, na.rm = TRUE)
  if(use.median){dm <- stats::median(data.vec, na.rm = TRUE)}
  dsd <- stats::sd(data.vec, na.rm = TRUE)
  return(data.vec < (dm - nsd * dsd) | data.vec > (dm + nsd * dsd))
}



# Get the metadata from file trajectories' file names adn add the profile as a colum
#
# @param trajfile.list A character vector. The names of trajectory files
# @return A data frame
.trajFilenames2metadata <- function(trajfile.list){
  
  trajfile.df <- as.data.frame(do.call("rbind",                                 # process trajectory filenames' metadata 
                                       parallel::mclapply(trajfile.list, 
                                                          function(x){
                                                            unlist(strsplit(x, split = "_"))
                                                          })), 
                               stringsAsFactors = FALSE)
  colnames(trajfile.df) <- c("site", "year", "month", "day", "hour", "height")  # add column names to trajectory filenames' metadata 
  trajfile.df["profile"] <- apply(trajfile.df[, c( "site", "year", "month", "day")], 
                                  MARGIN = 1,  
                                  function(x){
                                    paste(unlist(x), collapse = "-")
                                  })
  return(trajfile.df)
}



# Remove outliers of the interpolated values from trajectories from a single profile
#
# @param trProf.df A data.frame. The trajectory data interpolated for a single profile
# @param nsd A numeric. Number of standard deviation from the central tendency
# @return A data.frame with an additioanl column called concentration
.removeOutliers <- function(trProf.df, nsd){
  outlier1 <- .isOutlier(
    data.vec = as.vector(unlist(trProf.df["interpolvalue"])), 
    nsd = nsd, 
    use.median = TRUE)
  outlier1[is.na(outlier1)] <- TRUE
  trProf.df["outlier"] <- rep(FALSE, nrow(trProf.df))
  trProf.df[, "concentration"] <- unlist(trProf.df["interpolvalue"])  
  trProf.df[outlier1, "concentration"] <- stats::median(unlist(trProf.df["interpolvalue"]))
  outlier2 <- .isOutlier(
    data.vec = as.vector(unlist(trProf.df["concentration"])), 
    nsd = nsd, 
    use.median = TRUE)
  outlier2[is.na(outlier2)] <- TRUE
  trProf.df[outlier2, "concentration"] <- stats::median(unlist(trProf.df["concentration"]), na.rm = TRUE)
  trProf.df["outlier"] <- outlier1 | outlier2
  trProf.df[outlier1 | outlier2, "concentration"] <- stats::median(unlist(trProf.df["concentration"]), na.rm = TRUE)
  return(trProf.df)
}



# Match the number of rows between interpolated and observed data. The missing interpolated values are filled using the median
#
# @param trProf.df A data.frame of interpolated data from trajectories
# @param data.df A data.frame of observed data
# @return A data.frame. A modified version of trProf.df with additioanl rows to match data.df
.matchData2Interpolation <- function(trProf.df, data.df){
  if(is.null(trProf.df) | is.null(data.df)){return(NA)}
  if(nrow(data.df) > nrow(trProf.df)){
    miss <- !(unlist(data.df["height"]) %in% unlist(trProf.df["height"]))
    if(sum(miss > 0)){
      mheights <- as.vector(unlist(data.df[miss, "height"]))
      nr <- nrow(trProf.df)
      tmedian <- stats::median(unlist(trProf.df["concentration"]), na.rm = TRUE)
      for(j in 1:length(mheights)){
        trProf.df[nr + j, ] <- trProf.df[nr, ]
        trProf.df[nr + j, "height"] <- mheights[j]
        trProf.df[nr + j, "interpolvalue"] <- NA
        trProf.df[nr + j, "notes"] <- "missing"
        trProf.df[nr + j, "concentration"] <- tmedian
      }
    }else{
      # NOTE: when there are two measurements for the same height
      # identify duplicated heights
      t <- as.data.frame(table(as.vector(unlist(data.df["height"]))), stringsAsFactors = FALSE)
      t[, 1] <- as.numeric(t[, 1])
      for(i in 1:nrow(t)){
        if(t[i, "Freq"] > 1){
          n <- t[i, "Freq"] - 1
          # find the matching interpolation
          trProf.rowid <- match(t[i, 1], unlist(trProf.df["height"]))
          trProf.row <- trProf.df[trProf.rowid, ]
          for(i in 1:n)
            trProf.df <- rbind(trProf.df, trProf.row)
        }
      }
    }
  }else if(nrow(trProf.df) > nrow(data.df)){
    amatch <- unlist(trProf.df["height"]) %in% unlist(data.df["height"])
    trProf.df <- trProf.df[amatch, ]
  }
  return(trProf.df)
}


# Plot a world map with trajectories
#
# @param dtraj.df A data.frame of the trajectories of the same profile
# @param map.xlim A numeric vector. The minimum and maximum in the X axes
# @param map.ylim A numeric vector. The minimum and maximum in the Y axes
# @param file.out A character. The path to the resulting file
# @param device. A character. The type of output. i.e. "png"
# @param width A numeric. The width of the file
# @param height A numeric. The height of the file
# @param leg A logic. Include legend?
.plotmap <- function(dtraj.df, map.xlim, map.ylim, file.out, device, width, height, leg){
  lon <- 0; lat <- 0; filename <- 0; group <- 0; long <- 0; type <- 0           # avoid notes during package check
  p1 <- ggplot2::ggplot(data = ggplot2::map_data(map = "world"), 
                        mapping = ggplot2::aes(long, lat, group = group)) +
    ggplot2::geom_polygon(fill = "white", colour = "black") +
    ggplot2::geom_path(data = dtraj.df, mapping = ggplot2::aes(x = lon, y = lat, group = filename, colour = filename)) + 
    ggplot2::coord_quickmap(xlim = map.xlim, ylim = map.ylim, expand = TRUE) + 
    ggplot2::labs(x = "longitude", y = "latitude", color = "trajectory")
  if(!leg){  
    p1 <- p1 + ggplot2::theme(legend.position="none")
  }
  ggplot2::ggsave(filename = file.out, plot = p1, device = device, width = width, height = height)
}



# Plot the input data into map, section, and profile graphs. These plots are stored in disk
#
# @param stations.df A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
# @param map.xlim A numeric vector. Map's min & max longitude
# @param map.ylim A numeric vector. Map's min & max latitude
# @return A ggplot2 object
.buildbasemap <- function(stations.df, map.xlim, map.ylim){
  lon <- 0; lat <- 0; long <- 0; group <- 0; name <- 0                         # avoid notes during package check
  basemap <- ggplot2::ggplot(data = ggplot2::map_data(map = "world"), 
                             mapping = ggplot2::aes(long, lat, group = group)) +
    ggplot2::geom_polygon(fill = "white", colour = "black") +
    ggplot2::geom_point(data = stations.df, mapping = ggplot2::aes(x = lon, y = lat, group = name), shape = 17, size = 3) + 
    ggplot2::geom_label(data = stations.df, mapping = ggplot2::aes(x = lon, y = lat, group = name, label = name, hjust = c(1, 0, 0), vjust = c(0, 0, 1))) + 
    ggplot2::coord_quickmap(xlim = map.xlim, ylim = map.ylim, expand = TRUE) + 
    ggplot2::labs(x = "longitude", y = "latitude", color = "trajectory")
}


# Plot the trajectories grouped by year
#
# @param file.vec A character vector. The paths to  trajectory files
# @param path.out Path to the folder for storing the resulting files
# @param device A character. Image format, i.e. PNG
# @param map.xlim A numeric vector. Map's min & max longitude
# @param map.ylim A numeric vector. Map's min & max latitude
# @param map.height A numeirc. Map image size
# @param map.width A numeirc. Map image size
# @param stations.df A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
# @return A list. The path to the created files
.plotTrajYear <- function(file.vec, path.out, device, map.xlim, map.ylim, map.height, map.width, stations.df){
  lon <- 0; lat <- 0; filename <- 0                                             # avoid notes during package check
  #-----------------------------------
  # process data
  #-----------------------------------
  trajCnames <- c("V1", "V2", "year", "month", "day", "hour", "min",            # column names of the trajectory files
                  "V8", "V9", "lat", "lon", "height", "pressure")   
  traj.dat.list <- .files2df(file.vec = file.vec, header = FALSE, skip = 0,     # read all the trajectory files into a list of data.frames
                             cnames = trajCnames)
  traj.dat.list <- parallel::mclapply(1:length(traj.dat.list),                  # add file name as a column to each data.frame
                                      function(x, dat.list){
                                        dat.list[[x]]["filename"] <- names(dat.list)[x]
                                        return(dat.list[[x]])
                                      }, 
                                      dat.list = traj.dat.list)
  traj.dat <- do.call("rbind", traj.dat.list)                                  # collapse trajectory data into a single data.frame
  traj.dat["siteyear"] <- unlist(parallel::mclapply(unlist(traj.dat["filename"]),# add new column made of site and year
                                                    function(x){
                                                      paste(unlist(strsplit(x, split = "_"))[1:2], collapse = "-")
                                                    }))
  siteyear.vec <- as.vector(unlist(unique(traj.dat["siteyear"])))
  #-----------------------------------
  # base map
  #-----------------------------------
  basemap <- .buildbasemap(stations.df, map.xlim, map.ylim)
  #-----------------------------------
  # plot
  #-----------------------------------
  res <- list()
  for(i in 1:length(siteyear.vec)){
    dtraj.df <- traj.dat[traj.dat$siteyear == siteyear.vec[i], ]
    plot.map <- file.path(path.out, paste(siteyear.vec[i], "_map.", device, sep = ""), fsep = .Platform$file.sep)
    m <- basemap + ggplot2::geom_path(data = dtraj.df, mapping = ggplot2::aes(x = lon, y = lat, group = filename, colour = filename)) + 
      ggplot2::geom_point(data = dtraj.df[1, c("lon", "lat")], mapping = ggplot2::aes(x = lon, y = lat, group = NA), shape = 10, size = 3) + 
      ggplot2::theme(legend.position="none") + 
      ggplot2::ggtitle(label = dtraj.df[1, "siteyear"])
    ggplot2::ggsave(filename = plot.map, plot = m, device = device, width = map.width, height = map.height)
    res[[i]] <- plot.map
  }
  return(res)
}



# Plot the input data into map, section, and profile graphs. These plots are stored in disk
#
# @param file.in A character. The path to a filtered observations file or flag filtered raw data
# @param path.out A character. The path to a folder to store the results
# @param traj.interpol A list of numeric. The interpolated values of teh trajectories over the sea
# @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
# @param device A character. Image format, i.e. PNG
# @param map.xlim A numeric vector. Map's min & max longitude
# @param map.ylim A numeric vector. Map's min & max latitude
# @param map.height A numeirc. Map image size
# @param map.width A numeirc. Map image size
# @param sec.width A numeric. Crosssection map image size
# @param sec.height A numeric. Crosssection map image size
# @param prof.height A numeric. Profile image size
# @param prof.width A numeric. Profile image size
# @param nsd A numeric. Number of standard deviations to use to filter the interpolated data
# @param stations.df A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
# @return A character vector. The paths to the plot files
.plotTrajbackground <- function(file.in, path.out, traj.interpol, traj.intersections, device, map.xlim, map.ylim, map.height, map.width, sec.width, sec.height, prof.height, prof.width, nsd, stations.df){
  if(length(traj.interpol) == 0){warning("No interpolations!"); return()}
  lon <- 0; lat <- 0; filename <- 0; height <- 0; concentration <- 0; type <- 0 # avoid notes during package check
  #-----------------------------------
  # read observation data
  #-----------------------------------
  obsCnames <- c("site", "lat", "lon", "height", "year", "month", "day", "hour",# filtered observation column names
                 "min", "flask", "concentration")  
  file.dat <- .file2df(file.in = file.in, header = FALSE,                       # flag filtered raw data
                       skip = 0, cnames = obsCnames) 
  trajCnames <- c("V1", "V2", "year", "month", "day", "hour", "min",            # column names of the trajectory files
                  "V8", "V9", "lat", "lon", "height", "pressure") 
  traj.dat.list <- .files2df(file.vec = traj.intersections[[1]],                # read all the trajectory files into a list of data.frames 
                             header = FALSE, skip = 0, cnames = trajCnames)
  traj.filenames <- names(traj.dat.list)                                        # do not move!
  traj.dat.list <- parallel::mclapply(                                          # match intersections to trajectories
    1:length(traj.dat.list), 
    function(x, traj.dat.list, intersect.list){
      onSea <- rep(FALSE, times = nrow(traj.dat.list[[x]]))
      onSea[as.numeric(rownames(intersect.list[[x]])[1])] <- TRUE
      return(cbind(traj.dat.list[[x]], onSea))
    }, 
    traj.dat.list = traj.dat.list, 
    intersect.list = traj.intersections[[2]]
  )
  names(traj.dat.list) <- traj.filenames
  #-----------------------------------
  # process trajectory data
  #-----------------------------------
  traj.dat.list <- parallel::mclapply(1:length(traj.dat.list),                  # add file name as a column to each data.frame
                                      function(x, dat.list){
                                        dat.list[[x]]["filename"] <- names(dat.list)[x]
                                        return(dat.list[[x]])
                                      }, 
                                      dat.list = traj.dat.list)
  traj.dat <- do.call("rbind", traj.dat.list)                                   # collapse trajectory data into a single data.frame
  traj.dat["profile"] <- unlist(parallel::mclapply(unlist(traj.dat["filename"]),# add profile as new column to trajectory data
                                                   function(x){
                                                     paste(unlist(strsplit(x, split = "_"))[1:4], collapse = "-")
                                                   }))
  traj.prof.list <- split(traj.dat, traj.dat$profile)                           # split trajectory data.frame by profile. One data.frame per profile
  #-----------------------------------
  # process observations. Raw data input
  #-----------------------------------
  file.dat["profile"] <- apply(file.dat[, c( "site", "year", "month", "day")],  # add profile column to raw data
                               MARGIN = 1, 
                               function(x){
                                 gsub(" ", "0", paste(unlist(x), collapse = "-"))
                               })
  dataprofile.list <- split(file.dat, file.dat$profile)                         # split into a list of data frames. One per profile
  #-----------------------------------
  # process interpolated data
  #-----------------------------------
  trajfile.df <- .trajFilenames2metadata(traj.filenames)                        # get profiles from trajectory file names
  trajfile.df["interpolvalue"] <- unlist(traj.interpol)                         # add interpolated data to profile
  interpol.list <- split(trajfile.df, trajfile.df$profile)                      # list of data.frame. One per profile
  interpol.list <- parallel::mclapply(interpol.list,                            # cast height from character to numeric
                                      function(x){
                                        x[,"height"] <- as.numeric(x[,"height"])
                                        return(x)
                                      })
  interpol.list <- parallel::mclapply(interpol.list, .removeOutliers, nsd = nsd) # remove outliers
  #-----------------------------------
  # relate data
  #-----------------------------------
  traj2datamatch <- match(names(interpol.list), names(dataprofile.list))        # match profiles from raw data and trajectories
  traj2datamatch <- traj2datamatch[!is.na(traj2datamatch)]                      # get rid of NAs
  interpol.list <- parallel::mclapply(1:length(interpol.list),                  # complete the number of interpolation to match the profile
                                      function(x, interpol.list, dataprofile.list){
                                        .matchData2Interpolation(
                                          trProf.df = interpol.list[[x]], 
                                          data.df = dataprofile.list[[traj2datamatch[x]]])
                                      }, interpol.list = interpol.list, 
                                      dataprofile.list = dataprofile.list)
  #-----------------------------------
  # base map
  #-----------------------------------
  basemap <- .buildbasemap(stations.df, map.xlim, map.ylim)
  #-----------------------------------
  # plot
  #-----------------------------------
  filenames <- vector(mode = "character", length = 0)
  profile.dat.list <- list()
  for(i in 1:length(interpol.list)){
    if(sum(is.na(interpol.list[[i]]["concentration"])) == 0){
      tprof.df <- interpol.list[[i]][,c("concentration", "height", "interpolvalue")] # profile of interpolated values from trajectories
      dprof.df <- dataprofile.list[[traj2datamatch[i]]][,c("concentration", "height")] # profile of raw data
      dtraj.df <- traj.prof.list[[i]][, c("lon", "lat", "height", "filename", "profile", "onSea")] # trajectories of the same profile
      # join the observed, interpolated, and interpolated filtered data
      iftprof.df <- tprof.df[, c("concentration", "height")]
      iftprof.df["type"] <- "background"
      iptprof.df <- tprof.df[, c("interpolvalue", "height")]
      iptprof.df["type"] <- "interpolated"
      colnames(iptprof.df)[1] <- "concentration"
      dprof.df["type"] <- "observed"
      profile.dat <- rbind(dprof.df, iftprof.df, iptprof.df)
      profile.dat <- profile.dat[order(profile.dat$height),] 
      # collect the numeric results
      profile.dat.list[[i]] <- profile.dat
      profile.dat.list[[i]]["profile"] <- dtraj.df[1, "profile"]
      profile.dat.list[[i]]["date"] <- substr(profile.dat.list[[i]]$profile, start = 5, stop = 14)
      # plot 1 - map
      plot.map <- file.path(path.out, paste(dtraj.df[1, "profile"], "_map.", device, sep = ""), fsep = .Platform$file.sep)
      m <- basemap + ggplot2::geom_path(data = dtraj.df, mapping = ggplot2::aes(x = lon, y = lat, group = filename, colour = filename)) + 
        ggplot2::geom_point(data = dtraj.df[dtraj.df$onSea, ], mapping = ggplot2::aes(x = lon, y = lat, group = filename, colour = filename)) + 
        ggplot2::geom_point(data = dtraj.df[1, c("lon", "lat")], mapping = ggplot2::aes(x = lon, y = lat, group = NA), shape = 10, size = 3)
      ggplot2::ggsave(filename = plot.map, plot = m, device = device, width = map.width, height = map.height)
      # plot 2 - cross sections
      plot.lonsec <- file.path(path.out, paste(dtraj.df[1, "profile"], "_sectionlon.", device, sep = ""), fsep = .Platform$file.sep)
      plot.latsec <- file.path(path.out, paste(dtraj.df[1, "profile"], "_sectionlat.", device, sep = ""), fsep = .Platform$file.sep)
      slon <- ggplot2::ggplot(data = dtraj.df[, c("lon", "height", "filename", "onSea")], 
                              mapping = ggplot2::aes(x = lon, y = height, group = filename, colour = filename)) +
        ggplot2::geom_path() + ggplot2::xlim(map.xlim) + 
        ggplot2::labs(x = "longitude", y = "height", color = "trajectory") + 
        ggplot2::geom_point(data = dtraj.df[dtraj.df$onSea, ], mapping = ggplot2::aes(x = lon, y = height, group = filename, colour = filename)) + 
        ggplot2::geom_vline(xintercept = dtraj.df[1, "lon"], linetype="dotted")
      slat <- ggplot2::ggplot(data = dtraj.df[, c("lat", "height", "filename", "onSea")], 
                              mapping = ggplot2::aes(x = lat, y = height, group = filename, colour = filename)) +
        ggplot2::geom_path() + ggplot2::xlim(map.ylim) + 
        ggplot2::labs(x = "latitude", y = "height", color = "trajectory") + 
        ggplot2::geom_point(data = dtraj.df[dtraj.df$onSea, ], mapping = ggplot2::aes(x = lat, y = height, group = filename, colour = filename)) + 
        ggplot2::geom_vline(xintercept = dtraj.df[1, "lat"], linetype="dotted")
      ggplot2::ggsave(filename = plot.lonsec, plot = slon, device = device, width = sec.width, height = sec.height)
      ggplot2::ggsave(filename = plot.latsec, plot = slat, device = device, width = sec.width, height = sec.height)
      # plot 3 - profile
      plot.profile <- file.path(path.out, paste(dtraj.df[1, "profile"], "_profile.", device, sep = ""), fsep = .Platform$file.sep)    
      p <- ggplot2::ggplot(data = profile.dat, mapping = ggplot2::aes(x = concentration, y = height, 
                                                                      group = type, colour = type)) + 
        ggplot2::geom_path() +
        ggplot2::geom_point() + 
        ggplot2::geom_path()
      ggplot2::ggsave(filename = plot.profile, plot = p, device = device, 
                      width = prof.width, height = prof.height)
      # output file names
      filenames <- append(filenames, c(plot.map, plot.lonsec, plot.latsec, plot.profile))
    }
  }
  # write profile data
  csv.profile <- file.path(path.out, paste(basename(file.in), "_results.txt", sep = ""), fsep = .Platform$file.sep)    
  profilesresult.df <- do.call("rbind", profile.dat.list)
  profilesresult.df["split"] <- do.call(paste, c(profilesresult.df[c("profile", "type")], sep="_"))
  profres.list <- split(profilesresult.df, profilesresult.df$profile)
  profres.split.list <- lapply(profres.list, 
                               function(x){
                                 x.list <- split(x, f = x$split)
                                 for(j in 1:length(x.list)){
                                   l <- x.list[[j]]
                                   colnames(l)[colnames(l) == "concentration"] <- unique(l["type"])[1] # rename the concentration column
                                   x.list[[j]] <- l
                                 }
                                 dropcols <- c("type", "profile", "date", "split")
                                 df <- x.list[[1]]
                                 for(j in 2:length(x.list)){
                                   df <- merge(df, x.list[[j]][ , !(names(x.list[[j]]) %in% dropcols)], by = "height", all = TRUE)
                                 }
                                 return(df[,c("profile", "date", "height", "observed", "background", "interpolated")])
                               }
  )
  profile.df <- do.call("rbind", profres.split.list)                            # collapse list into data.frame
  profile.df <- profile.df[with(profile.df, order( profile, -height)), ]
  rownames(profile.df) <- NULL
  utils::write.table(profile.df, file = csv.profile)
  filenames <- append(filenames, csv.profile)
  return(filenames)
}
