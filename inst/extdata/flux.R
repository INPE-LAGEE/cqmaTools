# flux.R - compute the flux

#---- TODO ----
# - make sure path exists    data.out.path/gas

#---- Script setup ----
site <- "alf"
gas <- "co2"
test.path <- "/home/alber/Documents/tmp/cqmaTools/test"                         # base path

#---- SCRIPT ----
data.in.path <- file.path(test.path, "briefcase") # "/home/lagee/Documents/alber/test/briefcase"
data.out.path <- file.path(test.path, "flux_results", fsep = .Platform$file.sep) # path to the resulting files

# get data resulting from scripts background.R & briefcases.R
briefcase.df <- read.table(file.path(paste(data.in.path,'/',toupper(site),"_briefdata.txt", sep ='')))
profile.df <- read.table(file.path(paste(data.in.path,'/' ,site, ".", gas,"_bkgTable.txt", sep='')))
profbud.df <- merge(x = profile.df, y = briefcase.df, 
                    by.x = c("profile", "height"), 
                    by.y = c("profile", "planmts"), 
                    all.x = TRUE)
#---- MODELS OF TEMPERATURE AND PRESSURE ----
# h is height
ALFpres <- function(h){return(4E-6 *  h^2 - 0.1106 * h + 1004.5)}
ALFtemp <- function(h){return(4E-7 *  h^2 - 0.0074 * h + 32.639)}
RBApres <- function(h){return(4E-6 *  h^2 - 0.1209 * h + 1041.5)}
RBAtemp <- function(h){return(2E-11 * h^2 - 0.006 * h + 29.996)}
SANtemp <- function(h){return(3E-7 *  h^2 - 0.0067 * h + 29.92)}
SANpres <- function(h){return(0    *  h^2 - 0.1054 * h + 1093.7)}
TABtemp <- function(h){return(2E-7 *  h^2 - 0.0063 * h + 28.507)}
TABpres <- function(h){return(4E-6 *  h^2 - 0.1121 * h + 1006.6)}
#
tempfunc <- function(h){return(rep(NA, times = length(h)))}
presfunc <- function(h){return(rep(NA, times = length(h)))}
if(toupper(site) == "ALF"){
  tempfunc <- ALFtemp
  presfunc <- ALFpres
}else if(toupper(site) == "RBA"){
  tempfunc <- RBAtemp
  presfunc <- RBApres
}else if(toupper(site) == "SAN"){
  tempfunc <- SANtemp
  presfunc <- SANpres
}else if(toupper(site) == "TAB"){
  tempfunc <- TABtemp
  presfunc <- TABpres
}else{
  # error(logger, paste("There is no interpolation function for the given sample site: ", site, sep = ""))
  stop(paste("There is no interpolation function for the given sample site: ", site, sep = ""))
}

#---- SITE HEIGHTS (mts) ----
siteheights <- list()
siteheights["ALF"] <- 250
siteheights["RBA"] <- 153
siteheights["SAN"] <- 152
siteheights["TAB"] <- 188
siteheights["TEF"] <- 188

#---- flux computations ----

# select the height
profbud.df["heightcalc"] <- profbud.df$height
sel <- profbud.df$maxmts != profbud.df$minmts
sel[is.na(sel)] <- FALSE
profbud.df$heightcalc[sel] <- profbud.df$meanmts[sel]
# select the temperature and pressure
profbud.df["tempcalc"] <- tempfunc(profbud.df$heightcalc)
profbud.df["prescalc"] <- presfunc(profbud.df$heightcalc) 
# update key column names
colnames(profbud.df)[c(41,42,43)] <- c("tempBrief", "humidityBrief", "pressureBrief")
# temperature - fill in the gaps
sel <- (profbud.df$tempBrief > -5) & (profbud.df$tempBrief < 35)                # valid temperature range
sel[is.na(sel)] <- FALSE
profbud.df$tempcalc[sel] <- profbud.df$tempBrief[sel]                           # keep the field observations that make sense
# pressure - fill in the gaps
sel <- (profbud.df$pressureBrief > 300) & (profbud.df$pressureBrief < 1050)     # valid pressure range
sel[is.na(sel)] <- FALSE
profbud.df$prescalc[sel] <- profbud.df$pressureBrief[sel]                       # keep the field observations that make sense

#---- utilitary functions ----

# compute the concentration on the ground
#
# 
# @param dif_obs_bkg  A numeric. The difference between the observed conetration and the background concentration
# @param hfloor       A numeric. The hieght of the floor
# @param temp         A numeric. The tempearture of the first observation above the floor
# @param height       A numeric. The height of the first observaton above the floor
# @param molair       A numeric. The molarity of the air
# @return             A numeric. The concentration on the ground
.floorconcentration <- function(dif_obs_bkg, hfloor, temp, height, molair){
  ((dif_obs_bkg * exp(-hfloor / 1013.25 / 7) / 0.0000820574587 / (temp + 273 + ((height - hfloor) * 0.0059))) + (molair * dif_obs_bkg))/2
  # ((S16         * EXP(-P$1    / 1013.25 / 7)/  F$2             / (U16  + 273 + ((P16 -    P$1   ) * 0.0059))) + (Y16 *    S16        ))/2
}


#---- chemistry ----

profbud.df["obs_bkg"] <- profbud.df$observed - profbud.df$background            # observed concetration minus backgroud
profbud.df["airmol"]  <- (profbud.df$prescalc / 1013.25)/(0.0000820574587 * (profbud.df$tempcalc + 273))   # air molarity
profbud.df["mmolgasmt3"]  <- profbud.df$obs_bkg * profbud.df$airmol

# compute flux
uprof <- as.character(unique(profbud.df$profile))                               # unique profiles
flux.list <- lapply(uprof, 
                    function(x, profbud.df, hfloor){
                      prof.df <- profbud.df[profbud.df$profile == x, ]
                      prof.df <- prof.df[!is.na(prof.df$observed),]
                      prof.df <- prof.df[order(prof.df$heightcalc, decreasing = TRUE),]
                      # time computation
                      atime <- rep(NA, times = nrow(prof.df) - 1) # date of the sample
                      btime <- rep(NA, times = nrow(prof.df) - 1) # date of the sample
                      for(i in 1:(length(prof.df$trajtime) - 1)){
                        atime[i] <- prof.df$trajtime[i]
                        btime[i] <- prof.df$trajtime[i + 1] 
                      }
                      abtime <- as.data.frame(cbind(atime, btime), stringsAsFactors = FALSE)
                      abtime["abmean"] <- (abtime$atime + abtime$btime)/2
                      abtime[nrow(abtime) + 1, ] <- c(NA, NA, abtime$btime[nrow(abtime)])
                      # compute the area of each trapezoid
                      h <- rep(NA, times = nrow(prof.df) - 1)                   # height difference between two consequetive flask
                      a <- rep(NA, times = nrow(prof.df) - 1)                   # micromol gas/m3
                      b <- rep(NA, times = nrow(prof.df) - 1)                   # flask concentration
                      for(i in 1:(nrow(prof.df) - 1)){
                        h[i] <- prof.df$heightcalc[i] - prof.df$heightcalc[i + 1]
                        a[i] <- prof.df$mmolgasmt3[i]
                        b[i] <- prof.df$mmolgasmt3[i + 1]
                      }
                      h[nrow(prof.df)] <- prof.df$heightcalc[nrow(prof.df)] - hfloor
                      a[nrow(prof.df)] <- prof.df$mmolgasmt3[nrow(prof.df)]
                      b[nrow(prof.df)] <- .floorconcentration(dif_obs_bkg = prof.df$obs_bkg[nrow(prof.df)], 
                                                              hfloor  = hfloor, 
                                                              temp = prof.df$tempcalc[nrow(prof.df)], 
                                                              height = prof.df$heightcalc[nrow(prof.df)], 
                                                              molair = prof.df$airmol[nrow(prof.df)])
                      # compute time of the floor
                      area.param <- as.data.frame(cbind(h, a, b))
                      prof.df["umolmt2"] <- area.param$h * (area.param$a + area.param$b)/2
                      prof.df["fluxo"] <- prof.df$umolmt2 / abtime$abmean 
                      # filter columns
                      prof.df <- prof.df[, c("profile", "site", "year", "month", "day", "hour", "min.x", "flask", 
                                             "eventnumber", "observed", "background", "lat", "lon", "height", "heightcalc", 
                                             "tempcalc", "prescalc", "obs_bkg", "trajtime", "airmol", "mmolgasmt3", "fluxo")]
                      return(prof.df)
                    }, 
                    profbud.df = profbud.df, 
                    hfloor = as.vector(unlist(siteheights[toupper(site)]))
)
#
flux.df <- do.call("rbind", flux.list)
flux.df["gas"] <- gas
#
budget <- data.frame(
  profile = unique(flux.df$profile), 
  flux.mmolmt2day = rep(NA, times = length(unique(flux.df$profile))), 
  stringsAsFactors = FALSE
)
if(sum(is.nan(flux.df$fluxo)) != length(flux.df$fluxo)){ # test if at least one flux is not missing
  budget <- aggregate(fluxo ~ profile, data = flux.df, FUN = sum, na.rm = TRUE)  
}
names(budget) <- c("profile", "flux.mmolmt2day")
budget["gas"] <- gas
budget["mgCmt2day"] <- budget$flux.mmolmt2day * 28
budget["gCmt2day"] <- budget$mgCmt2day / 1000000
budget <- cbind(budget, cqmaTools::getDataFromProfile(prof.name = as.character(budget$profile)))
write.table(budget, paste(data.out.path, '/', toupper(gas), '/','fluxo_',site,'_',gas,'.txt', sep=''), col.names = TRUE, row.names = FALSE)
write.table(budget, paste(data.out.path, '/', toupper(gas), '/','fluxo_',site,'_',gas,'.txt', sep=''), col.names = TRUE, row.names = FALSE)
#
