#' The CQMATOOLS class
#'
#' Use this class for representing a CQMATOOLS object
#'
#'
#'
#' @note No notes
#' @name cqmaTools
#' @aliases cqmaTools-class
#' @exportClass cqmaTools
#' @author Alber Sanchez
#' @import methods
#' @import roxygen2
#' @import testthat
setClass (
  Class = "cqmaTools",
  representation = representation(),
  validity = function(object){}
)


#*******************************************************
# CONSTRUCTOR
#*******************************************************
setMethod (
  f="initialize",
  signature="cqmaTools",
  definition=function(.Object){
    validObject(.Object)
    return(.Object)
  }
)
#CONSTRUCTOR (USER FRIENDLY)
#' Creates a cqmaTools object
#'
#' @rdname cqmaTools
#' @docType methods
#' @export
cqmaTools <- function(){
  new (Class="cqmaTools")
}



#*******************************************************
# ACCESSORS
#*******************************************************



#*******************************************************
# FUNCTIONS
#*******************************************************



#' Split a single raw data file. The output files are prefixed with "traj_specs_".
#'
#' @param file.in Path to a raw data file
#' @param path.out Path to the folder for storing the resulting files
#' @param colname A character. The name of a column where the flags are located
#' @param keepFlags A character vector. Flags to keep in the raw data. i.e. c("...", "..>")
#' @return A character. The name of the new file
#' @docType methods
#' @aliases splitRawdata-generic
#' @export
setGeneric("splitRawdata",function(file.in, path.out, colname, keepFlags){standardGeneric ("splitRawdata")})

#' @rdname splitRawdata
setMethod(
  "splitRawdata",
  c("character", "character", "character", "character"),
  function(file.in, path.out, colname, keepFlags){
    .splitRawdata(file.in, path.out, colname, keepFlags)
  }
)



#' Run HYSPLIT simulations for the traj_spec files in the given path
#'
#' @param path.in A character representing a folder with traj_spec files
#' @param backTrajTime A numeric. Time to modify the hysplit file search. For example, 10 days to the past is (10 * 24 * 3600) * (-1)
#' @param hysplit.exec.path A path to the HYSPLIT's exec folder.
#' @param hysplit.work.path A path to the HYSPLIT's working folder.
#' @param path.out A character. The path to store the resulting files. Usually HYSPLIT's simulations folder.
#' @param timezone A character. The time zone used for date computations.i.e. GMT
#' @return A character list of the created files
#' @docType methods
#' @aliases runsimulation-generic
#' @export
setGeneric("runsimulation",function(path.in, backTrajTime, hysplit.exec.path, hysplit.work.path, path.out, timezone){standardGeneric ("runsimulation")})

#' @rdname runsimulation
setMethod("runsimulation",
          c("character", "numeric", "character", "character", "character", "character"),
          function(path.in, backTrajTime, hysplit.exec.path, hysplit.work.path, path.out, timezone){
            .runsimulation(path.in, backTrajTime, hysplit.exec.path, hysplit.work.path, path.out, timezone)
          }
)



#' Remove headers from hysplit files
#'
#' @param file.vec A character vector. The paths to the input files
#' @param path.out Path to the folder for storing the resulting files
#' @param skip Numeric. Number of lines to remove from the beginning of the file
#' @return A list of the created file paths
#' @docType methods
#' @aliases removeHeaders-generic
#' @export
setGeneric("removeHeaders",function(file.vec, path.out, skip){standardGeneric ("removeHeaders")})

#' @rdname removeHeaders
setMethod("removeHeaders",
          c("character", "character", "numeric"),
          function(file.vec, path.out, skip){
            .removeHeaders(file.vec, path.out, skip)
          }
)



#' Check if the trajectories are above the given treshold
#'
#' @param file.vec A character vector. The paths to the input files
#' @param above A numeric treshold
#' @return A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if they meet the test
#' @docType methods
#' @aliases filterTrajHeight-generic
#' @export
setGeneric("filterTrajHeight",function(file.vec, above){standardGeneric ("filterTrajHeight")})

#' @rdname filterTrajHeight
setMethod("filterTrajHeight",
          c("character", "numeric"),
          function(file.vec, above){
            .filterTrajHeight(file.vec, above)
          }
)



#' Find trajectories' first point over the sea.
#'
#' @param file.vec A character vector. The paths to the input files
#' @param limit.in A SpatialLinesDataFrame object which is used to intersect the trajectories.
#' @return A list made of a character vector and 2 lists. The character vector is the path to each trajectory file. The list contains the first row in the trajectory file which lies over the sea
#' @docType methods
#' @aliases intersectTraj-generic
#' @export
setGeneric("intersectTraj",function(file.vec, limit.in){standardGeneric ("intersectTraj")})

#' @rdname intersectTraj
setMethod("intersectTraj", 
          c("character", "SpatialLinesDataFrame"),
          function(file.vec, limit.in){
            .intersectTraj(file.vec, limit.in)
          }
)



#' Check if the trajectories reach the sea
#'
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @return A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if they were kept
#' @docType methods
#' @aliases trajreachthesea-generic
#' @export
setGeneric("trajreachthesea",function(traj.intersections){standardGeneric ("trajreachthesea")})

#' @rdname trajreachthesea
setMethod("trajreachthesea", 
          c("list"),
          function(traj.intersections){
            .trajreachthesea(traj.intersections)
          }
)



#' Check if the trajectories' intersections fall inside the metereological station coverage
#'
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @param stations.df A data.frame of metereological station data. It must have at least 4 columns: name, longitude, latitude, and file c("name", "lon", "lat", "file")
#' @return A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if meet the test
#' @docType methods
#' @aliases trajOutInterpolation-generic
#' @export
setGeneric("trajOutInterpolation",function(traj.intersections, stations.df){standardGeneric ("trajOutInterpolation")})

#' @rdname trajOutInterpolation
setMethod("trajOutInterpolation", 
          c("list", "data.frame"),
          function(traj.intersections, stations.df){
            .trajOutInterpolation(traj.intersections, stations.df)
          }
)



#' Get the stations' data matching the trajectory records. It takes a trajectory's first point over the sea and project its latitude to an straight line made by the corresponding metereological stations
#'
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @param stations.df A data.frame of metereological station data. It must have at least 4 columns: name, longitude, latitude, and file c("name", "lon", "lat", "file")
#' @param tolerance.sec A numeric. A tolerance used when comparing dates
#' @param timezone A character. The time zone. i.e. "GMT"
#' @param searchTranslation A numeric. The number of seconds for searching metereological station data
#' @return A list of numeric. Each number is the interpolation result from the matching stations
#' @docType methods
#' @aliases crossdata-generic
#' @export
setGeneric("crossdata",function(traj.intersections, stations.df, tolerance.sec, timezone, searchTranslation){standardGeneric ("crossdata")})

#' @rdname crossdata
setMethod("crossdata", 
          c("list", "data.frame", "numeric", "character", "numeric"),
          function(traj.intersections, stations.df, tolerance.sec, timezone, searchTranslation){
            .crossdata(traj.intersections, stations.df, tolerance.sec, timezone, searchTranslation)
          }
)



#' Plot the input data into map, section, and profile graphs. These plots are stored in disk
#'
#' @param file.in A character. The path to a filtered observations file or flag filtered raw data
#' @param path.out A character. The path to a folder to store the results
#' @param traj.interpol A list of numeric. The interpolated values of teh trajectories over the sea
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @param traj.plot A vector of character. The file path to trajectories to plot additioanl to those in traj.intersections[[1]]
#' @param device A character. Image format, i.e. PNG
#' @param map.xlim A numeric vector. Map's min & max longitude
#' @param map.ylim A numeric vector. Map's min & max latitude
#' @param map.height A numeirc. Map image size
#' @param map.width A numeirc. Map image size
#' @param sec.width A numeric. Crosssection map image size
#' @param sec.height A numeric. Crosssection map image size
#' @param prof.height A numeric. Profile image size
#' @param prof.width A numeric. Profile image size
#' @param nsd A numeric. Number of standard deviations to use to filter the interpolated data
#' @param stations.df A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
#' @return A character vector. The paths to the plot files
#' @docType methods
#' @aliases plotTrajbackground-generic
#' @export
setGeneric("plotTrajbackground",function(file.in, path.out, traj.interpol, traj.intersections, traj.plot, device, map.xlim, map.ylim, map.height, map.width, sec.width, sec.height, prof.height, prof.width, nsd, stations.df){standardGeneric ("plotTrajbackground")})

#' @rdname plotTrajbackground
setMethod("plotTrajbackground", 
          c("character", "character", "list", "list", "character", "numeric", 
            "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
            "numeric", "numeric", "data.frame"),
          function(file.in, path.out, traj.interpol, traj.intersections, traj.plot, device, map.xlim, map.ylim, map.height, map.width, sec.width, sec.height, prof.height, prof.width, nsd, stations.df){
            .plotTrajbackground(
              file.in, 
              path.out, 
              traj.interpol, 
              traj.intersections, 
              traj.plot, 
              device, 
              map.xlim, 
              map.ylim, 
              map.height, 
              map.width, 
              sec.width, 
              sec.height, 
              prof.height, 
              prof.width, 
              nsd, 
              stations.df)
          }
)



#' Plot the trajectories grouped by year
#'
#' @param file.vec A character vector. The paths to  trajectory files
#' @param path.out Path to the folder for storing the resulting files
#' @param device A character. Image format, i.e. PNG
#' @param map.xlim A numeric vector. Map's min & max longitude
#' @param map.ylim A numeric vector. Map's min & max latitude
#' @param map.height A numeirc. Map image size
#' @param map.width A numeirc. Map image size
#' @param stations.df A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
#' @return A list. The path to the created files
#' @docType methods
#' @aliases plotTrajYear-generic
#' @export
setGeneric("plotTrajYear",function(file.vec, path.out, device, map.xlim, map.ylim, map.height, map.width, stations.df){standardGeneric ("plotTrajYear")})

#' @rdname plotTrajYear
setMethod("plotTrajYear", 
          c("character", "character", "character", "numeric", "numeric", 
            "numeric", "numeric", "data.frame"),
          function(file.vec, path.out, device, map.xlim, map.ylim, map.height, map.width, stations.df){
            .plotTrajYear(
              file.vec, 
              path.out, 
              device, 
              map.xlim, 
              map.ylim, 
              map.height, 
              map.width, 
              stations.df)
          }
)

