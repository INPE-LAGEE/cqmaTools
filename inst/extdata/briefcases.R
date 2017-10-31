################################################################################
# TODO: script description
################################################################################
source("/home/lagee/Documents/ghProjects/cqmaTools/R/util.R")
magicc.path <- "/home/lagee/home 2/magicc/"                                     # Path to flight logs

# TODO: expose site & data.out.path as script parameters
site <- toupper("SAN")                                                          # site's name
data.out.path <- "/home/lagee/Documents/alber/test/briefcase"                   # Path to output directory
pat <- paste("^PFP_[0-9]{3,4}_", site, "_[0-9]{4}_[0-9]{2}_[0-9]{2}\\.(TXT|txt)", sep = "") # filter files by name
file.vec <- list.files(magicc.path, recursive = TRUE,                           # list and filter data files
                       pattern = pat, 
                       full.names = TRUE)
# call the script
bc.list <- lapply(file.vec, .getHistoryAC)

bc.list <- lapply(bc.list, function(x){
  #x <- bc.list[[1]]
  if(sum((x[, 10] > 0 & x[, 10] < 300) & (x[, 10] > 1050), na.rm = TRUE) > 0){
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
