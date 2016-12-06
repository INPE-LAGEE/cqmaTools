# test_cqmaTools.R

#library(testthat)
#source("/home/alber/Documents/ghProjects/cqmaTools/R/util.R")

val <- -2.878 
vec <- c(13.162, -7.967, -34.352) # must be ordered
expect_equal(as.vector(.inInterval(val = val, vec = vec)), c(F, F))
expect_equal(as.vector(.inInterval(val = val, vec = sort(vec))), c(F, T))

expect_equal(.monthWeek("2010-01-01", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2014-01-01", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2015-01-01", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2016-01-01", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2016-01-02", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2016-01-03", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2016-01-04", weekfirstday = 1, tz = "GMT"), 2)
expect_equal(.monthWeek("2016-01-05", weekfirstday = 1, tz = "GMT"), 2)
expect_equal(.monthWeek("2016-01-06", weekfirstday = 1, tz = "GMT"), 2)
expect_equal(.monthWeek("2016-01-07", weekfirstday = 1, tz = "GMT"), 2)
expect_equal(.monthWeek("2016-01-01", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2016-02-29", weekfirstday = 1, tz = "GMT"), 5)
expect_equal(.monthWeek("2016-08-18", weekfirstday = 1, tz = "GMT"), 3)
expect_equal(.monthWeek("2016-12-31", weekfirstday = 1, tz = "GMT"), 5)
expect_equal(.monthWeek("2016-02-01", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2016-03-01", weekfirstday = 1, tz = "GMT"), 1)
expect_equal(.monthWeek("2016-06-01", weekfirstday = 1, tz = "GMT"), 1)

trajspecs.row <- data.frame("yr" = 2014, "mo" = 12, "da" = 12)
expect_equal(.buildGDASfilename(trajspecs.row, timezone = "GMT"), "gdas1.dec14.w2")
trajspecs.row <- data.frame("yr" = 2014, "mo" = 12, "da" = 31)
expect_equal(.buildGDASfilename(trajspecs.row, timezone = "GMT"), "gdas1.dec14.w5")
trajspecs.row <- data.frame("yr" = 2014, "mo" = 1, "da" = 1)
expect_equal(.buildGDASfilename(trajspecs.row, timezone = "GMT"), "gdas1.jan14.w1")


date.dec <- 2000.0013661202
date.str <- "2000-01-01 11:59:59.999410"
expect_equal(as.character(.ydec2date(date.dec)), date.str)
expect_equal(.date2ydec(date.str, timezone = "GMT"), date.dec)


atest <- c(1,1,1,1100,1)
res <-   c(F,F,F,T,   F)
maxfm.ppm = 10000000
expect_equal(.isOutlier(data.vec = atest, nsd=2, use.median = TRUE, maxfm.ppm = maxfm.ppm), res)
atest <- c(1,1,1,-1100,1)
res <-   c(F,F,F,    T,F)
expect_equal(.isOutlier(data.vec = atest, nsd=2, use.median = TRUE, maxfm.ppm = maxfm.ppm), res)
atest <- c(28, 29, 33, 27, 29, 28)
res <-   c( F,  F,  T,  F,  F,  F)
expect_equal(.isOutlier(data.vec = atest, nsd=2, use.median = TRUE, maxfm.ppm = maxfm.ppm), res)
atest <- c(1,1,1,1100,1)
res <-   c(F,F,F,T,   F)
expect_equal(.isOutlier(data.vec = atest, nsd=2, maxfm.ppm = 1.5, use.median = TRUE), res)
atest <- c(1,1,1,-1100,1)
res <-   c(F,F,F,    T,F)
expect_equal(.isOutlier(data.vec = atest, nsd=2, maxfm.ppm = 1.5, use.median = TRUE), res)
atest <- c(28, 29, 33, 27, 29, 28)
res <-   c( F,  F,  T,  F,  F,  F)
expect_equal(.isOutlier(data.vec = atest, nsd=2, maxfm.ppm = 1.5, use.median = TRUE), res)
atest <- c(30, 35, 30, 35, 30, 30)
res <-   c( F,  F,  F,  F,  F,  F)
expect_equal(.isOutlier(data.vec = atest, nsd=2, maxfm.ppm = 5, use.median = TRUE), res)
atest <- c(30, 35, 30, 35, 30, 30)
res <-   c( F,  T,  F,  T,  F,  F)
expect_equal(.isOutlier(data.vec = atest, nsd=2, maxfm.ppm = 1.5, use.median = TRUE), res)
nsd  <-  2
maxfm.ppm <-  1.0
interpolated <- c(387.918385103308, 387.530746818852,387.61208078711, 388.202610296293, 388.211573638231, 388.227018617125, 387.531348727213, 387.536258479053, 387.506342657925, 387.48756476492)
io <- .isOutlier(data.vec = interpolated, nsd = nsd, maxfm.ppm = maxfm.ppm, use.median = TRUE)
res <- c(FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE, FALSE)
expect_equal(.isOutlier(data.vec = interpolated, nsd=2, maxfm.ppm = 1.5, use.median = TRUE), res)



#atest <- c(1,1,1,11000000,1,1,1,1,1,10000)
#atest [atest < median(atest) - sd(atest) | atest > median(atest) + sd(atest)] <- median(atest)
#atest [atest < median(atest) - sd(atest) | atest > median(atest) + sd(atest)] <- median(atest)
#median(atest)
#sd(atest)
#

interpolated <- c(1,2,3,4,5,5,6,7,NA,9,0)
res1 <- rep(median(interpolated, na.rm = TRUE), times = length(interpolated))
res2 <- rep(TRUE, times = length(interpolated))

interpolated <- c(1,2,3,4,5,5,6,7,8,9,0)
res1 <- rep(median(interpolated, na.rm = TRUE), times = length(interpolated))
res2 <- rep(TRUE, times = length(interpolated))
res2[5:6] <- FALSE






data.vec <- c(1,2,1,2,100,2,1,2)
.background.softrules(data.vec = data.vec, nsd = nsd, maxfm.ppm = maxfm.ppm)

data.vec <- c(1,2,1,2,100,100,1,2)
.background.softrules(data.vec = data.vec, nsd = nsd, maxfm.ppm = maxfm.ppm)



data.vec <- c(1, NA, 3)
expect_equal(.fillNAs(data.vec), c(1,2,3))

data.vec <- c(1, NA, NA, 3)
expect_equal(.fillNAs(data.vec), c(1,2,2,3))

data.vec <- c(1, NA, NA, NA, 3)
expect_equal(.fillNAs(data.vec), c(1,2,2,2,3))

data.vec <- c(1, NA, NA, NA, 5)
expect_equal(.fillNAs(data.vec), c(1,3,3,3,5))

data.vec <- c(NA, 5)
expect_equal(.fillNAs(data.vec), c(5,5))

data.vec <- c(5, NA)
expect_equal(.fillNAs(data.vec), c(5,5))

data.vec <- c(NA, 3, NA)
expect_equal(.fillNAs(data.vec), c(3,3,3))

data.vec <- c(NA)
expect_equal(.fillNAs(data.vec), NaN)

data.vec <- c(NA, NA, NA)
expect_equal(.fillNAs(data.vec), c(NaN,NaN,NaN))












x <- c(1,2,3,4,5)
y <- c(6,7,8,9,10)
xy.df <- as.data.frame(cbind(x, y))
minx = 3
maxx = NA
miny = NA
maxy = NA
res <- c(F,F,T,T,T)
expect_equal(.inbound(xy.df, minx = minx, maxx = maxx, miny = miny, maxy = maxy), res)






expect_equal(.recttrapezearea(height = 2, minorlength = 10, mayorlength = 12), 22)
expect_equal(.recttrapezearea(height = 11, minorlength = -1, mayorlength = 10), 49.5)

expect_true(abs(.floorconcentration(dif_obs_bkg = -0.87, 
                                    hfloor = 250, 
                                    temp = 32.06, 
                                    height = 476.4, 
                                    molair = 37.56) + 33.04107) < 2.03e-06)


expect_true(abs(.floorconcentration(dif_obs_bkg = -4.52, 
                                    hfloor = 250, 
                                    temp = 29.71, 
                                    height = 443.5, 
                                    molair = 38.05) + 173.4956) < 2.606759e-05)






# TODO: check function inversibility
#date.dec <- 2015.916438
#date.str <- "2015-12-01"
#expect_equal(as.character(.ydec2date(date.dec)), date.str)
#expect_equal(.date2ydec(date.str, timezone = "GMT"), date.dec)
#2015.919178, 12/2/2015
#2015.921918, 12/3/2015
#2015.924658, 12/4/2015
#2015.927397, 12/5/2015
#2015.930137, 12/6/2015

# TODO: use this data to test decimal dates
#year.dec <- 2000.08879781420773724676109850406646728515625
#adate <- .ydec2date(year.dec)
#adate.dec <- .date2ydec(as.character(adate), timezone = "GMT")
#year.dec - adate.dec



#Error in strptime(xx, f <- "%Y-%m-%d %H:%M:%OS", tz = tz) :argument "timezone" is missing, with no default
#formatC(.date2ydec(adate = adate, timezone = "GMT"), 12)


#year.dec <- 2000.086065573770383707596920430660247802734375
#(adate <- .ydec2date(year.dec))
#formatC(.date2ydec(adate), 12)

#year.dec <- 2015.916438 # 2015-12-01
#(adate <- .ydec2date(year.dec))
#formatC(.date2ydec(adate), 10)
#year.dec <- 2015.930137 # 2015-12-06
#(adate <- .ydec2date(year.dec))
#formatC(.date2ydec(adate), 10)
#year.dec <- 2015.921918 # 2015-12-03
#(adate <- .ydec2date(year.dec))
#formatC(.date2ydec(adate), 10)
