# briefcases.R - Extract data from gas-analysis briefcases

#---- TODO ----
# - script description

#---- Script setup ----
test.path <- "/home/alber/Documents/tmp/cqmaTools/test"                         # base path
magicc.path <- "/home/alber/Documents/tmp/cqmaTools/test/briefcase"             # Path to flight logs

#---- Script parameters ----
site <- toupper("RBA")                                                          # site's name
data.out.path <- file.path(test.path, "BCS_results", fsep = .Platform$file.sep) # path to the resulting files

#---- Log setup ----
require(log4r)
logger <- create.logger()
logfile(logger) <- file.path(data.out.path, "briefcases.log")
level(logger) <- "DEBUG"
info(logger, "Start! ###############################################")

#---- SCRIPT ----
debug(logger, paste0("site: ", site))
data.out.path <- "/home/lagee/Documents/alber/test/briefcase"                   # Path to output directory
pat <- paste("^PFP_[0-9]{3,4}_", site, "_[0-9]{4}_[0-9]{2}_[0-9]{2}\\.(TXT|txt)", sep = "") # filter files by name
file.vec <- list.files(magicc.path, recursive = TRUE,                           # list and filter data files
                       pattern = pat, 
                       full.names = TRUE)
# call the script
bc.list <- lapply(file.vec, cqmaTools::getHistoryAC)





bc.list <- lapply(bc.list, function(x){
  #x <- bc.list[[1]]
  #x <- bc.list[[81]]
  #x <- bc.list[[90]]
  #x <- bc.list[[91]]

  # Criteria for filtering invalid observations:
  # - flight plan has more than 12 observations
  # - flight plan heigh is divisible by 100
  # - flight plan is not 0
  if(length(x[, 10]) > 12){
    if(sum(x[18:20, 2] %% 100 == 0) == 3){
      if(sum(x[18:20, 2] < 500) == 3){  
        x <- x[1:17,]
      }
    }else{
      x <- x[1:17,]
    }
  }
  
  # remove invalid pressure values (300 < p < 1200)
  if((sum((x[, 10] > 0), na.rm = TRUE) == length(x[, 10])) + 
     (sum((x[, 10] < 300), na.rm = TRUE)  == 0) +
     (sum((x[, 10] > 1200), na.rm = TRUE) == 0) != 3){
    x[, 10] <- NA
  }
  return(x)
})
bc.df <- do.call("rbind", bc.list)                                              # cast set of lists to data.frame
write.table(bc.df, file = file.path(data.out.path, paste(site, "_briefdata.txt", sep = '')))

#-------------------------------------------------------------------------------
# find the data file with errors in format
#-------------------------------------------------------------------------------
#for(i in 1:length(file.vec)){
#  print(file.vec[i])
#  .getHistoryAC(file.vec[i])
#}
#-------------------------------------------------------------------------------
# call script on a single file
#-------------------------------------------------------------------------------
# file.in <- "/home/lagee/home 2/magicc//Malas/SAN/2011 SAN/PFP_210_SAN_2011_05_24.txt"
# .getHistoryAC(file.in)
