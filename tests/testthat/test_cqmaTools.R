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

interpolvalue <- c(1,1,1,11000000,1,1,1,1,1,10000, NA)
ol <- .background.median(data.vec = interpolvalue, nsd = 2, maxfm.ppm = 5)
res1 <- c(F,F,F, T,F,F,F,F,F, T, T) # identify the outliers after doble testing for  median + 2 * sd
res2 <- c(1,1,1, 1,1,1,1,1,1,1, 1) # replace the outliers by the median
expect_equal(as.vector(unlist(ol["outlier"])), res1)
expect_equal(as.vector(unlist(ol["background"])), res2)






x <- c(1,2,3,4,5)
y <- c(6,7,8,9,10)
xy.df <- as.data.frame(cbind(x, y))
minx = 3
maxx = NA
miny = NA
maxy = NA
res <- c(F,F,T,T,T)
expect_equal(.inbound(xy.df, minx = minx, maxx = maxx, miny = miny, maxy = maxy), res)







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

