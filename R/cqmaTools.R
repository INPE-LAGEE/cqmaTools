#---- NOTES: ----
# a vertical profile is made of jars (12 or 17). Each jar is taken at certain height . A vertical profile corresponds to one flight
# in the lab, each jar is analyzed and gas concentration is observed
# for each jar (height) in the profile, a hysplit trajectory is computed
# each trajectory reach the sea at some point. We 're intereted ONLY in the first point
# for each trajectory' point-over-the-sea, we interpolate a gas concentration

#---- TODO: ----

#---- How to install: ----
# sudo apt-get install gdal-bin libgdal1-dev libproj-dev
# install.packages(c("roxygen2", "log4r", "fpc", "ggplot2"))
# install.packages(c("rgdal", "rgeos", "sp", "maps"))
# library(cqmaTools)






#' @title Filter trajectories by height
#' @name filterTrajHeight
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Check if the trajectories are above the given treshold
#'
#' @param file.vec A character vector. The paths to the input files
#' @param above    A length-1 numeric. The treshold
#' @param cnames   A character vector. The column names of the hysplit files
#' @return         A data frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if they meet the test
#' @export
filterTrajHeight <- function(file.vec, above, cnames){
  # check trajectories' height and make a vector of those to keep
  #cnames <- HYSPLIT.COLNAMES                                                    # column names of the input file    
  file.dat.list <- .files2df(file.vec = file.vec, header = FALSE, 
                             skip = 0, cnames = cnames)
  keep <- vector(mode = "logical", length = length(file.dat.list))
  keep <- lapply(file.dat.list, function(x){if(sum(x$height < above) > 0){return(FALSE)}; return(TRUE)}) # test
  keep <- as.vector(unlist(keep))
  return(data.frame(file.vec, keep))
}



#' @title Split a raw data file.
#' @name splitRawdata
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Split a single raw data file. The output files are prefixed with "traj_specs_".
#'
#' @param file.in    A character.Path to a raw data file
#' @param path.out   A character. Path to the folder for storing the resulting files
#' @param colname    A character. The name of a column where the flags are located
#' @param keepFlags  A character vector. Flags to keep in the raw data. i.e. c("...", "..>")
#' @param cnames     A character vector. The name of the columns of the raw data file
#' @param keepCols   A character vector. The column names of the raw data file to keep after filtering
#' @param cnamesTest A character vector. The column names used for testing duplicated rows in the filtered data
#' @return           A list iof three. A character with the name of the new file, a data.frame of duplicated or inconsistent rows, and a data.frame of coordinates which were changed because they have the wrong sign
#' @export
splitRawdata <- function(file.in, path.out, colname, keepFlags, cnames, 
                          keepCols, cnamesTest){
  # raw data column names
  #cnames <- RAW.DATA.COLNAMES
  #keepCols <- RAW.DATA.COLNAMES.KEEP                                            # keep these columns
  #cnamesTest <- RAW.DATA.COLNAMES.TESTDUPLICATED
  
  file.dat <- .file2df(file.in = file.in, header = FALSE, skip = 0,             # read data
                       cnames = cnames)
  # filter data
  file.dat <- .filterDataframe(df = file.dat, keepCols = keepCols,              # filter using flag attribute
                               flagName = colname, keepFlags = keepFlags)
  # report duplicated rows
  testUnique.vec <- apply(file.dat[, cnamesTest], 
                          MARGIN = 1, 
                          function(x){
                            gsub(" ", "0", paste(unlist(x), collapse = "___"))
                          })
  testUnique.df <- as.data.frame(table(as.vector(testUnique.vec)), 
                                 stringsAsFactors = FALSE)
  dup.df <- testUnique.df[testUnique.df$Freq > 1, ]
  # test lat lon for missing signs
  file.dat.list <- split(file.dat, file.dat$site)
  nsd = 3                                                                       # number of standard deviationto identify an outlier 
  treshold = 10                                                                 # the minimum difference (in SDs) to accept a sign  change in coordinates
  #
  outll <- parallel::mclapply(file.dat.list, 
                              .checklonlat, 
                              nsd = nsd, 
                              treshold = treshold)
  #
  # replace coords
  file.dat <- do.call("rbind", file.dat.list)
  ll.df <- do.call("rbind", outll)
  file.dat[, c("lon", "lat")] <- ll.df[, c("lon", "lat")]
  # store the results
  newfile <- file.path(path.out, paste("traj_specs_", 
                                       basename(file.in), sep = ""), 
                       fsep = .Platform$file.sep)
  utils::write.table(file.dat, file = newfile, col.names = FALSE, 
                     row.names = FALSE, quote = FALSE)
  # report  
  wrongcoords <- ll.df[ll.df$changed == TRUE, c("lon", "lat")] * (-1)
  wrongcoords["idrow"] <- rownames(wrongcoords)
  colnames(dup.df) <- c("shYMDhmflask", "Freq")
  dup.df["idrow"] <- as.numeric(rownames(dup.df))
  return(list(newfile, dup.df, wrongcoords))
}
