source("/home/lagee/Documents/ghProjects/cqmaTools/R/util.R")
magicc.path <- "/home/lagee/home 2/magicc/"                                     # flight logs

# TODO: expose site & data.out.path as script parameters
site <- "RBA"
data.out.path <- "/home/lagee/Documents/alber/test/briefcase"
pat <- paste("^PFP_[0-9]{3,4}_", site, "_[0-9]{4}_[0-9]{2}_[0-9]{2}\\.(TXT|txt)", sep = "")
file.vec <- list.files(magicc.path, recursive = TRUE,                       # take one just file
                       pattern = pat, 
                       full.names = TRUE)
bc.list <- lapply(file.vec, .getHistoryAC)
bc.df <- do.call("rbind", bc.list)
write.table(bc.df, file = file.path(data.out.path, paste(site, "_briefdata.txt", sep = '')))
