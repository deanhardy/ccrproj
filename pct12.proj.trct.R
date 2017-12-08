## the code below projects populations using cohort change ratios (CCRs) by age/sex/race/ethnicity cohorts
## the study area is a 23 county region surrounding coastal Georgia extending into FL and SC
## tract scale data from US Censuses 2000 and 2010 pct12 tables

rm(list=ls())

data.out <- "tables/projections/outputs/middle/" ## define directory path for outputs dependent on scenario
data.in <- "tables/projections/inputs/" ## define directory path for data inputs 


##############################################################################
## prep data for tract projections
##############################################################################
## load race/ethnic specific variables tables for 2000 and 2010
pct1200 <- read.csv(paste(data.in, "pct1200tr.csv", sep = ""))
pct1210 <- read.csv(paste(data.in, "pct1210tr.csv", sep = ""))

## match rows by id10 so that we have same tracts for each data set
pct1210 <- pct1210[pct1210$geoid10 %in% pct1200$geoid10,] 
pct1200 <- pct1200[pct1200$geoid10 %in% pct1210$geoid10,] 
geoid10 <- pct1210$geoid10

## load race/ethnic specific variables as base and launch year data variables
b <- pct1200[,-1]
write.csv(b, paste(data.out, "pct1200tr.csv", sep = "")) ## save for later import
l <- pct1210[,-1]
write.csv(l, paste(data.out, "pct1210tr.csv", sep = "")) ## save for later import


## sum race/ethnicity data by county
## convert tract IDs to county IDs and sums tracts by age group within counties
tr.chk <- b
sub <- substring(tr.chk$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub.n <- as.numeric(sub) ## converts county id character string to numeric
tr.chk$geoid10 <- sub.n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum.chk <- aggregate(. ~ geoid10, data=tr.chk, FUN=sum, na.rm=TRUE) ## sum data.frame by id10
cntyid <- trtsum.chk$geoid10 ## save cntyid to reattach
trtsum.chk <- trtsum.chk[,-1]
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(trtsum.chk[, grep("w", names(trtsum.chk))])
bl <- rowSums(trtsum.chk[, grep("b", names(trtsum.chk))])
a <- rowSums(trtsum.chk[, grep("a", names(trtsum.chk))])
o <- rowSums(trtsum.chk[, grep("o", names(trtsum.chk))])
h <- rowSums(trtsum.chk[, grep("h", names(trtsum.chk))])
trtsum.chk <- data.frame(cbind(cntyid, w,bl,a,o,h))

write.csv(trtsum.chk, paste(data.out, "pct1200tr.re.sums.csv", sep = ""))

## sum race/ethnicity data by county
## convert tract IDs to county IDs and sums tracts by age group within counties
tr.chk <- l
sub <- substring(tr.chk$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub.n <- as.numeric(sub) ## converts county id character string to numeric
tr.chk$geoid10 <- sub.n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum.chk <- aggregate(. ~ geoid10, data=tr.chk, FUN=sum, na.rm=TRUE) ## sum data.frame by id10
cntyid <- trtsum.chk$geoid10 ## save cntyid to reattach
trtsum.chk <- trtsum.chk[,-1]
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(trtsum.chk[, grep("w", names(trtsum.chk))])
bl <- rowSums(trtsum.chk[, grep("b", names(trtsum.chk))])
a <- rowSums(trtsum.chk[, grep("a", names(trtsum.chk))])
o <- rowSums(trtsum.chk[, grep("o", names(trtsum.chk))])
h <- rowSums(trtsum.chk[, grep("h", names(trtsum.chk))])
trtsum.chk <- data.frame(cbind(cntyid, w,bl,a,o,h))

write.csv(trtsum.chk, paste(data.out, "pct1210tr.re.sums.csv", sep = ""))



##############################################################################
## starting 2020 asre tract projections here
##############################################################################

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 cohorts from launch year and 70up from base year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
b2 <- b %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
b2 <- b2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## calculate ccr
ccr <- l2/b2
ccr$geoid10 <- geoid10

## convert all Inf to NaN
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

names(ccr) <- c("geoid10", 
	"m00wccr", "m10wccr", "m20wccr", "m30wccr", "m40wccr", "m50wccr", "m60wccr", 
	"f00wccr", "f10wccr", "f20wccr", "f30wccr", "f40wccr", "f50wccr", "f60wccr", 
	"m00bccr", "m10bccr", "m20bccr", "m30bccr", "m40bccr", "m50bccr", "m60bccr", 
	"f00bccr", "f10bccr", "f20bccr", "f30bccr", "f40bccr", "f50bccr", "f60bccr",
	"m00accr", "m10accr", "m20accr", "m30accr", "m40accr", "m50accr", "m60accr", 
	"f00accr", "f10accr", "f20accr", "f30accr", "f40accr", "f50accr", "f60accr",
	"m00occr", "m10occr", "m20occr", "m30occr", "m40occr", "m50occr", "m60occr", 
	"f00occr", "f10occr", "f20occr", "f30occr", "f40occr", "f50occr", "f60occr",
	"m00hccr", "m10hccr", "m20hccr", "m30hccr", "m40hccr", "m50hccr", "m60hccr", 
	"f00hccr", "f10hccr", "f20hccr", "f30hccr", "f40hccr", "f50hccr", "f60hccr"
	)

## convert all Inf to NaN and NaN to 0
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

## set ceiling and floors for ccrgrowth and loss rates
## based on controlled county age/sex/race specific rates
ccr.co <- read.csv(paste(data.out, "pct1220cocl.ccr.csv", sep = ""))[,-1]

## create 5 digit county id as long as tracts
sub10 <- substring(ccr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
sub10n <- data.frame(sub10n)
names(sub10n) <- c("geoid10")
ccr.tr <- merge(sub10n, ccr.co, by="geoid10")
ccr.tr$geoid10 <- geoid10

ceiling <- ccr.tr
floor <- ccr.tr

ccr.cl <- ccr ## create new data frame with same dimensions
ccr.cl[ccr > ceiling] <- ceiling[ccr > ceiling]
ccr.cl[ccr < floor] <- floor[ccr < floor]

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l2 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l3 <- l2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]

## Hamilton-Perry Projection from launch year to target year
t.proj <- l3*ccr.cl
t.proj$geoid10 <- geoid10

names(t.proj) <- c("geoid10", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", 
	"m6069w", "m70upw", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", 
	"f6069w", "f70upw",
	"m1019b", "m2029b", "m3039b", "m4049b", "m5059b", 
	"m6069b", "m70upb", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", 
	"f6069b", "f70upb",
	"m1019a", "m2029a", "m3039a", "m4049a", "m5059a", 
	"m6069a", "m70upa", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", 
	"f6069a", "f70upa",
	"m1019o", "m2029o", "m3039o", "m4049o", "m5059o", 
	"m6069o", "m70upo", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", 
	"f6069o", "f70upo",
	"m1019h", "m2029h", "m3039h", "m4049h", "m5059h", 
	"m6069h", "m70uph", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", 
	"f6069h", "f70uph"
	)

## convert all Inf to NaN
is.na(t.proj) <- sapply(t.proj, is.infinite)

## convert all NaN to 0
t.proj <- replace(t.proj, is.na(t.proj), 0)

## convert all Inf to NaN# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE, na.action=NULL)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all NaN to 0
is.na(l.cwr) <- sapply(l.cwr, is.infinite)
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male and female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## convert all Inf to NaN and NaN to 0
is.na(t.cwrmerge) <- sapply(t.cwrmerge, is.infinite)
t.cwrmerge <- replace(t.cwrmerge, is.na(t.cwrmerge), 0) ## convert all NaN to 0

c.tfr <- read.csv(paste(data.out, "pct1220cocl.tfr.csv", sep = ""))[,-1]
names(c.tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## convert tract IDs to county IDs
sub10 <- substring(t.proj$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
t.proj$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame

t.tfrmerge <- merge(t.proj, c.tfr, by="geoid10")
t.tfrmerge$geoid10 <- geoid10

## convert all Inf to NaN and NaN to 0
is.na(t.tfrmerge) <- sapply(t.tfrmerge, is.infinite)
t.tfrmerge <- replace(t.tfrmerge, is.na(t.tfrmerge), 0) ## convert all NaN to 0

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5) & geography level (last two)
pct12tr <- aggregate(cbind(
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	m1019w, m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	f1019w, f2029w, f3039w, f4049w, f5059w, f6069w, f70upw,
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	m1019b, m2029b, m3039b, m4049b, m5059b, m6069b, m70upb, 
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	f1019b, f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	m1019a, m2029a, m3039a, m4049a, m5059a, m6069a, m70upa, 
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	f1019a, f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	m1019o, m2029o, m3039o, m4049o, m5059o, m6069o, m70upo, 
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	f1019o, f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	m1019h, m2029h, m3039h, m4049h, m5059h, m6069h, m70uph, 
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	f1019h, f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.tfrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)




######################################################################################
## starting 2020 growth/decline controlled tract projections here 
######################################################################################

geoid10 <- l$geoid10 ## save for reattachment

## calculate pop sum for launch year tracts
l.sum <- rowSums(l[,-1])
l.sum <- data.frame(geoid10, l.sum)

## calculate pop sum of target year tracts
t.sum <- rowSums(pct12tr[,-1])
t.sum <- data.frame(geoid10, t.sum)

## calculate growth rate for 
## ann.gr <- ((t.sum/l.sum)^(1/10)) - 1 ## geometric growth rate
ann.gr <- (log(t.sum/l.sum))/10 ## exponential growth rate
ann.gr <- data.frame(ann.gr)
ann.gr$geoid10 <- geoid10
is.na(ann.gr) <- sapply(ann.gr, is.infinite)
ann.gr <- replace(ann.gr, is.na(ann.gr), 0)
names(ann.gr) <- c("geoid10", "ann.gr")

## merge pop counts and growth rate data tables
lt.sum <- merge (l.sum, t.sum, by="geoid10")
lt.sum <- merge (lt.sum, ann.gr, by="geoid10")

## set annual growth rate ceiling and floorby generating target population counts
## under those rates
t.sum.gr <- l.sum[,-1]*((1.05)^10)
t.sum.gr <- data.frame(geoid10, t.sum.gr)
t.sum.de <- l.sum[,-1]*((0.98)^10)
t.sum.de <- data.frame(geoid10, t.sum.de)

## merge all pop counts and growth rate columns together into one dataframe
t.sum.rt <- merge(t.sum.gr, t.sum.de, by="geoid10")
t.sum.rt <- round (t.sum.rt, digits=0)
rt <- merge(lt.sum, t.sum.rt, by="geoid10")

## set ceiling and floors for growth and loss rates of tract totals
nms <- paste("col", 1:2, sep = ".")
ceiling <- as.data.frame(matrix(rt$t.sum.gr, nrow = 419, ncol = 2))
names(ceiling) <- nms

floor <- as.data.frame(matrix(rt$t.sum.de, nrow = 419, ncol = 2))
names(floor) <- nms

t.sum.cl <- t.sum ## create new data frame with same dimensions
t.sum.cl[ann.gr > 0.05] <- ceiling[ann.gr > 0.05]
t.sum.cl[ann.gr < -0.02] <- floor[ann.gr < -0.02]
t.sum.cl$geoid10 <- geoid10

af <- t.sum.cl/t.sum
af$geoid10 <- geoid10
names(af) <- c("geoid10", "af")
pct12tr.cl <- merge(pct12tr, af, by="geoid10")

## convert all NaN to 0
is.na(pct12tr.cl) <- sapply(pct12tr.cl, is.infinite)
pct12tr.cl <- replace(pct12tr.cl, is.na(pct12tr.cl), 0)

pct12tr <- aggregate(cbind(m0009w*af, m1019w*af, 
	m2029w*af, m3039w*af, m4049w*af, m5059w*af, m6069w*af, m70upw*af, 
	f0009w*af, f1019w*af, f2029w*af, f3039w*af, f4049w*af, 
	f5059w*af, f6069w*af, f70upw*af,
	m0009b*af, m1019b*af, 
	m2029b*af, m3039b*af, m4049b*af, m5059b*af, m6069b*af, m70upb*af, 
	f0009b*af, f1019b*af, f2029b*af, f3039b*af, f4049b*af, 
	f5059b*af, f6069b*af, f70upb*af,
	m0009a*af, m1019a*af, 
	m2029a*af, m3039a*af, m4049a*af, m5059a*af, m6069a*af, m70upa*af, 
	f0009a*af, f1019a*af, f2029a*af, f3039a*af, f4049a*af, 
	f5059a*af, f6069a*af, f70upa*af,
	m0009o*af, m1019o*af, 
	m2029o*af, m3039o*af, m4049o*af, m5059o*af, m6069o*af, m70upo*af, 
	f0009o*af, f1019o*af, f2029o*af, f3039o*af, f4049o*af, 
	f5059o*af, f6069o*af, f70upo*af,
	m0009h*af, m1019h*af, 
	m2029h*af, m3039h*af, m4049h*af, m5059h*af, m6069h*af, m70uph*af, 
	f0009h*af, f1019h*af, f2029h*af, f3039h*af, f4049h*af, 
	f5059h*af, f6069h*af, f70uph*af
	)~geoid10, data=pct12tr.cl, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)
	

## round population counts to digits=decimal places
pct12tr <- round (pct12tr, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12tr, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

tfr <- round (tfr, digits=1)

write.csv(pct12tr, paste(data.out, "pct1220tr.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1220tr.tfr.csv", sep = ""))



		
######################################################################################
## starting 2020 asre controlled tract projections here 
######################################################################################

## load target tract and county data
tr <- pct12tr
co <- read.csv(paste(data.out, "pct1220cocl.csv", sep = ""))[,-1]

## save geoid10 tract id to reattach
geoid10 <- tr$geoid10

## convert tract IDs to county IDs and sums tracts by age group within counties
sub10 <- substring(tr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
tr$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum <- aggregate(. ~ geoid10, data=tr, FUN=sum, na.rm=TRUE) ## sum data.frame by id10

cntyid <- trtsum$geoid10

## calculate adjustment factor for target years
af <- co/trtsum ## creates data frame of adjustment factors (i.e. county AG/sum of tracts AG)
af$geoid10 <- cntyid ## reattaches county IDs to data frame

## convert all Inf to NaN
is.na(af) <- sapply(af, is.infinite)

## convert all NaN to 0
af <- replace(af, is.na(af), 0)

control <- merge(tr, af, by="geoid10") ## merge trct data with adjustment factors by id
control$geoid10 <- geoid10 ## appends tract id 2010 back to data frame

pct12trcl <- aggregate(cbind(
	m0009w.x*m0009w.y, m1019w.x*m1019w.y, m2029w.x*m2029w.y, m3039w.x*m3039w.y, 
	m4049w.x*m4049w.y, m5059w.x*m5059w.y, m6069w.x*m6069w.y, m70upw.x*m70upw.y, 
	f0009w.x*f0009w.y, f1019w.x*f1019w.y, f2029w.x*f2029w.y, f3039w.x*f3039w.y, 
	f4049w.x*f4049w.y, f5059w.x*f5059w.y, f6069w.x*f6069w.y, f70upw.x*f70upw.y,
	m0009b.x*m0009b.y, m1019b.x*m1019b.y, m2029b.x*m2029b.y, m3039b.x*m3039b.y, 
	m4049b.x*m4049b.y, m5059b.x*m5059b.y, m6069b.x*m6069b.y, m70upb.x*m70upb.y, 
	f0009b.x*f0009b.y, f1019b.x*f1019b.y, f2029b.x*f2029b.y, f3039b.x*f3039b.y, 
	f4049b.x*f4049b.y, f5059b.x*f5059b.y, f6069b.x*f6069b.y, f70upb.x*f70upb.y,
	m0009a.x*m0009a.y, m1019a.x*m1019a.y, m2029a.x*m2029a.y, m3039a.x*m3039a.y, 
	m4049a.x*m4049a.y, m5059a.x*m5059a.y, m6069a.x*m6069a.y, m70upa.x*m70upa.y, 
	f0009a.x*f0009a.y, f1019a.x*f1019a.y, f2029a.x*f2029a.y, f3039a.x*f3039a.y, 
	f4049a.x*f4049a.y, f5059a.x*f5059a.y, f6069a.x*f6069a.y, f70upa.x*f70upa.y,
	m0009o.x*m0009o.y, m1019o.x*m1019o.y, m2029o.x*m2029o.y, m3039o.x*m3039o.y, 
	m4049o.x*m4049o.y, m5059o.x*m5059o.y, m6069o.x*m6069o.y, m70upo.x*m70upo.y, 
	f0009o.x*f0009o.y, f1019o.x*f1019o.y, f2029o.x*f2029o.y, f3039o.x*f3039o.y, 
	f4049o.x*f4049o.y, f5059o.x*f5059o.y, f6069o.x*f6069o.y, f70upo.x*f70upo.y,
	m0009h.x*m0009h.y, m1019h.x*m1019h.y, m2029h.x*m2029h.y, m3039h.x*m3039h.y, 
	m4049h.x*m4049h.y, m5059h.x*m5059h.y, m6069h.x*m6069h.y, m70uph.x*m70uph.y, 
	f0009h.x*f0009h.y, f1019h.x*f1019h.y, f2029h.x*f2029h.y, f3039h.x*f3039h.y, 
	f4049h.x*f4049h.y, f5059h.x*f5059h.y, f6069h.x*f6069h.y, f70uph.x*f70uph.y
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12trcl) <- c("geoid10", 
	"m0009w", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", "m6069w", "m70upw", 
	"f0009w", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", "m5059b", "m6069b", "m70upb", 
	"f0009b", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", "m5059a", "m6069a", "m70upa", 
	"f0009a", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", "m5059o", "m6069o", "m70upo", 
	"f0009o", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", "m5059h", "m6069h", "m70uph", 
	"f0009h", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", "f6069h", "f70uph"
	)

## round population counts to digits=decimal places
pct12trcl <- round (pct12trcl, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12trcl, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## convert tract IDs to county IDs and sums tracts by age group within counties
## to check for race/ethnicity tract sums against race/ethnicity county sums
tr.chk <- pct12trcl
sub <- substring(tr.chk$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub.n <- as.numeric(sub) ## converts county id character string to numeric
tr.chk$geoid10 <- sub.n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum.chk <- aggregate(. ~ geoid10, data=tr.chk, FUN=sum, na.rm=TRUE) ## sum data.frame by id10
trtsum.chk <- trtsum.chk[,-1]
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(trtsum.chk[, grep("w", names(trtsum.chk))])
b <- rowSums(trtsum.chk[, grep("b", names(trtsum.chk))])
a <- rowSums(trtsum.chk[, grep("a", names(trtsum.chk))])
o <- rowSums(trtsum.chk[, grep("o", names(trtsum.chk))])
h <- rowSums(trtsum.chk[, grep("h", names(trtsum.chk))])
trtsum.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(co[, grep("w", names(co))])
b <- rowSums(co[, grep("b", names(co))])
a <- rowSums(co[, grep("a", names(co))])
o <- rowSums(co[, grep("o", names(co))])
h <- rowSums(co[, grep("h", names(co))])
co.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
colSums(trtsum.chk - co.chk) ## compare tract projection sums to county projection sums by race/ethnicity


write.csv(pct12trcl, paste(data.out, "pct1220trcl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1220trcl.tfr.csv", sep = ""))
write.csv(trtsum.chk, paste(data.out, "pct1220trcl.re.sums.csv", sep = ""))




##############################################################################
##############################################################################
##############################################################################
## starting 2030 asre tract projections here
##############################################################################

## load race/ethnic specific variables as base and launch year data variables
b <- read.csv(paste(data.out, "pct1210tr.csv", sep = ""))[,-1]
l <- pct12trcl

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 cohorts from launch year and 70up from base year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
b2 <- b %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
b2 <- b2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## calculate ccr
geoid10 <- l$geoid10
ccr <- l2/b2
ccr$geoid10 <- geoid10

## convert all Inf to NaN
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

names(ccr) <- c("geoid10", 
	"m00wccr", "m10wccr", "m20wccr", "m30wccr", "m40wccr", "m50wccr", "m60wccr", 
	"f00wccr", "f10wccr", "f20wccr", "f30wccr", "f40wccr", "f50wccr", "f60wccr", 
	"m00bccr", "m10bccr", "m20bccr", "m30bccr", "m40bccr", "m50bccr", "m60bccr", 
	"f00bccr", "f10bccr", "f20bccr", "f30bccr", "f40bccr", "f50bccr", "f60bccr",
	"m00accr", "m10accr", "m20accr", "m30accr", "m40accr", "m50accr", "m60accr", 
	"f00accr", "f10accr", "f20accr", "f30accr", "f40accr", "f50accr", "f60accr",
	"m00occr", "m10occr", "m20occr", "m30occr", "m40occr", "m50occr", "m60occr", 
	"f00occr", "f10occr", "f20occr", "f30occr", "f40occr", "f50occr", "f60occr",
	"m00hccr", "m10hccr", "m20hccr", "m30hccr", "m40hccr", "m50hccr", "m60hccr", 
	"f00hccr", "f10hccr", "f20hccr", "f30hccr", "f40hccr", "f50hccr", "f60hccr"
	)
	
## convert all Inf to NaN and NaN to 0
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

## set ceiling and floors for ccrgrowth and loss rates
## based on controlled county age/sex/race specific rates
ccr.co <- read.csv(paste(data.out, "pct1230cocl.ccr.csv", sep = ""))[,-1]

## create 5 digit county id as long as tracts
sub10 <- substring(ccr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
sub10n <- data.frame(sub10n)
names(sub10n) <- c("geoid10")
ccr.tr <- merge(sub10n, ccr.co, by="geoid10")
ccr.tr$geoid10 <- geoid10

ceiling <- ccr.tr
floor <- ccr.tr

ccr.cl <- ccr ## create new data frame with same dimensions
ccr.cl[ccr > ceiling] <- ceiling[ccr > ceiling]
ccr.cl[ccr < floor] <- floor[ccr < floor]

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l2 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l3 <- l2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]
	
## Hamilton-Perry Projection from launch year to target year
t.proj <- l3*ccr.cl
t.proj$geoid10 <- geoid10

names(t.proj) <- c("geoid10", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", 
	"m6069w", "m70upw", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", 
	"f6069w", "f70upw",
	"m1019b", "m2029b", "m3039b", "m4049b", "m5059b", 
	"m6069b", "m70upb", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", 
	"f6069b", "f70upb",
	"m1019a", "m2029a", "m3039a", "m4049a", "m5059a", 
	"m6069a", "m70upa", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", 
	"f6069a", "f70upa",
	"m1019o", "m2029o", "m3039o", "m4049o", "m5059o", 
	"m6069o", "m70upo", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", 
	"f6069o", "f70upo",
	"m1019h", "m2029h", "m3039h", "m4049h", "m5059h", 
	"m6069h", "m70uph", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", 
	"f6069h", "f70uph"
	)

## convert all Inf to NaN
is.na(t.proj) <- sapply(t.proj, is.infinite)

## convert all NaN to 0
t.proj <- replace(t.proj, is.na(t.proj), 0)

## convert all Inf to NaN# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE, na.action=NULL)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all NaN to 0
is.na(l.cwr) <- sapply(l.cwr, is.infinite)
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male and female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## convert all Inf to NaN and NaN to 0
is.na(t.cwrmerge) <- sapply(t.cwrmerge, is.infinite)
t.cwrmerge <- replace(t.cwrmerge, is.na(t.cwrmerge), 0) ## convert all NaN to 0

c.tfr <- read.csv(paste(data.out, "pct1230cocl.tfr.csv", sep = ""))[,-1]
names(c.tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## convert tract IDs to county IDs
sub10 <- substring(t.proj$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
t.proj$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame

t.tfrmerge <- merge(t.proj, c.tfr, by="geoid10")
t.tfrmerge$geoid10 <- geoid10

## convert all Inf to NaN and NaN to 0
is.na(t.tfrmerge) <- sapply(t.tfrmerge, is.infinite)
t.tfrmerge <- replace(t.tfrmerge, is.na(t.tfrmerge), 0) ## convert all NaN to 0

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5) & geography level (last two)
pct12tr <- aggregate(cbind(
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	m1019w, m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	f1019w, f2029w, f3039w, f4049w, f5059w, f6069w, f70upw,
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	m1019b, m2029b, m3039b, m4049b, m5059b, m6069b, m70upb, 
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	f1019b, f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	m1019a, m2029a, m3039a, m4049a, m5059a, m6069a, m70upa, 
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	f1019a, f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	m1019o, m2029o, m3039o, m4049o, m5059o, m6069o, m70upo, 
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	f1019o, f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	m1019h, m2029h, m3039h, m4049h, m5059h, m6069h, m70uph, 
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	f1019h, f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.tfrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)



######################################################################################
## starting 2030 growth/decline controlled tract projections here 
######################################################################################

geoid10 <- l$geoid10 ## save for reattachment

## calculate pop sum for launch year tracts
l.sum <- rowSums(l[,-1])
l.sum <- data.frame(geoid10, l.sum)

## calculate pop sum of target year tracts
t.sum <- rowSums(pct12tr[,-1])
t.sum <- data.frame(geoid10, t.sum)

## calculate growth rate for 
## ann.gr <- ((t.sum/l.sum)^(1/10)) - 1 ## geometric growth rate
ann.gr <- (log(t.sum/l.sum))/10 ## exponential growth rate
ann.gr <- data.frame(ann.gr)
ann.gr$geoid10 <- geoid10
is.na(ann.gr) <- sapply(ann.gr, is.infinite)
ann.gr <- replace(ann.gr, is.na(ann.gr), 0)
names(ann.gr) <- c("geoid10", "ann.gr")

## merge pop counts and growth rate data tables
lt.sum <- merge (l.sum, t.sum, by="geoid10")
lt.sum <- merge (lt.sum, ann.gr, by="geoid10")

## set annual growth rate ceiling and floorby generating target population counts
## under those rates
t.sum.gr <- l.sum[,-1]*((1.05)^10)
t.sum.gr <- data.frame(geoid10, t.sum.gr)
t.sum.de <- l.sum[,-1]*((0.98)^10)
t.sum.de <- data.frame(geoid10, t.sum.de)

## merge all pop counts and growth rate columns together into one dataframe
t.sum.rt <- merge(t.sum.gr, t.sum.de, by="geoid10")
t.sum.rt <- round (t.sum.rt, digits=0)
rt <- merge(lt.sum, t.sum.rt, by="geoid10")

## set ceiling and floors for growth and loss rates of tract totals
nms <- paste("col", 1:2, sep = ".")
ceiling <- as.data.frame(matrix(rt$t.sum.gr, nrow = 419, ncol = 2))
names(ceiling) <- nms

floor <- as.data.frame(matrix(rt$t.sum.de, nrow = 419, ncol = 2))
names(floor) <- nms

t.sum.cl <- t.sum ## create new data frame with same dimensions
t.sum.cl[ann.gr > 0.05] <- ceiling[ann.gr > 0.05]
t.sum.cl[ann.gr < -0.02] <- floor[ann.gr < -0.02]
t.sum.cl$geoid10 <- geoid10

af <- t.sum.cl/t.sum
af$geoid10 <- geoid10
names(af) <- c("geoid10", "af")
pct12tr.cl <- merge(pct12tr, af, by="geoid10")

## convert all NaN to 0
is.na(pct12tr.cl) <- sapply(pct12tr.cl, is.infinite)
pct12tr.cl <- replace(pct12tr.cl, is.na(pct12tr.cl), 0)

pct12tr <- aggregate(cbind(m0009w*af, m1019w*af, 
	m2029w*af, m3039w*af, m4049w*af, m5059w*af, m6069w*af, m70upw*af, 
	f0009w*af, f1019w*af, f2029w*af, f3039w*af, f4049w*af, 
	f5059w*af, f6069w*af, f70upw*af,
	m0009b*af, m1019b*af, 
	m2029b*af, m3039b*af, m4049b*af, m5059b*af, m6069b*af, m70upb*af, 
	f0009b*af, f1019b*af, f2029b*af, f3039b*af, f4049b*af, 
	f5059b*af, f6069b*af, f70upb*af,
	m0009a*af, m1019a*af, 
	m2029a*af, m3039a*af, m4049a*af, m5059a*af, m6069a*af, m70upa*af, 
	f0009a*af, f1019a*af, f2029a*af, f3039a*af, f4049a*af, 
	f5059a*af, f6069a*af, f70upa*af,
	m0009o*af, m1019o*af, 
	m2029o*af, m3039o*af, m4049o*af, m5059o*af, m6069o*af, m70upo*af, 
	f0009o*af, f1019o*af, f2029o*af, f3039o*af, f4049o*af, 
	f5059o*af, f6069o*af, f70upo*af,
	m0009h*af, m1019h*af, 
	m2029h*af, m3039h*af, m4049h*af, m5059h*af, m6069h*af, m70uph*af, 
	f0009h*af, f1019h*af, f2029h*af, f3039h*af, f4049h*af, 
	f5059h*af, f6069h*af, f70uph*af
	)~geoid10, data=pct12tr.cl, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)
	

## round population counts to digits=decimal places
pct12tr <- round (pct12tr, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12tr, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

tfr <- round (tfr, digits=1)

write.csv(pct12tr, paste(data.out, "pct1230tr.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1230tr.tfr.csv", sep = ""))




######################################################################################
## starting 2030 asre controlled tract projections here 
######################################################################################

## load target tract and county data
tr <- pct12tr
co <- read.csv(paste(data.out, "pct1230cocl.csv", sep = ""))[,-1]

## save geoid10 tract id to reattach
geoid10 <- tr$geoid10

## convert tract IDs to county IDs and sums tracts by age group within counties
sub10 <- substring(tr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
tr$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum <- aggregate(. ~ geoid10, data=tr, FUN=sum, na.rm=TRUE) ## sum data.frame by id10

cntyid <- trtsum$geoid10

## calculate adjustment factor for target years
af <- co/trtsum ## creates data frame of adjustment factors (i.e. county AG/sum of tracts AG)
af$geoid10 <- cntyid ## reattaches county IDs to data frame

## convert all Inf to NaN
is.na(af) <- sapply(af, is.infinite)

## convert all NaN to 0
af <- replace(af, is.na(af), 0)

control <- merge(tr, af, by="geoid10") ## merge trct data with adjustment factors by id
control$geoid10 <- geoid10 ## appends tract id 2010 back to data frame

pct12trcl <- aggregate(cbind(
	m0009w.x*m0009w.y, m1019w.x*m1019w.y, m2029w.x*m2029w.y, m3039w.x*m3039w.y, 
	m4049w.x*m4049w.y, m5059w.x*m5059w.y, m6069w.x*m6069w.y, m70upw.x*m70upw.y, 
	f0009w.x*f0009w.y, f1019w.x*f1019w.y, f2029w.x*f2029w.y, f3039w.x*f3039w.y, 
	f4049w.x*f4049w.y, f5059w.x*f5059w.y, f6069w.x*f6069w.y, f70upw.x*f70upw.y,
	m0009b.x*m0009b.y, m1019b.x*m1019b.y, m2029b.x*m2029b.y, m3039b.x*m3039b.y, 
	m4049b.x*m4049b.y, m5059b.x*m5059b.y, m6069b.x*m6069b.y, m70upb.x*m70upb.y, 
	f0009b.x*f0009b.y, f1019b.x*f1019b.y, f2029b.x*f2029b.y, f3039b.x*f3039b.y, 
	f4049b.x*f4049b.y, f5059b.x*f5059b.y, f6069b.x*f6069b.y, f70upb.x*f70upb.y,
	m0009a.x*m0009a.y, m1019a.x*m1019a.y, m2029a.x*m2029a.y, m3039a.x*m3039a.y, 
	m4049a.x*m4049a.y, m5059a.x*m5059a.y, m6069a.x*m6069a.y, m70upa.x*m70upa.y, 
	f0009a.x*f0009a.y, f1019a.x*f1019a.y, f2029a.x*f2029a.y, f3039a.x*f3039a.y, 
	f4049a.x*f4049a.y, f5059a.x*f5059a.y, f6069a.x*f6069a.y, f70upa.x*f70upa.y,
	m0009o.x*m0009o.y, m1019o.x*m1019o.y, m2029o.x*m2029o.y, m3039o.x*m3039o.y, 
	m4049o.x*m4049o.y, m5059o.x*m5059o.y, m6069o.x*m6069o.y, m70upo.x*m70upo.y, 
	f0009o.x*f0009o.y, f1019o.x*f1019o.y, f2029o.x*f2029o.y, f3039o.x*f3039o.y, 
	f4049o.x*f4049o.y, f5059o.x*f5059o.y, f6069o.x*f6069o.y, f70upo.x*f70upo.y,
	m0009h.x*m0009h.y, m1019h.x*m1019h.y, m2029h.x*m2029h.y, m3039h.x*m3039h.y, 
	m4049h.x*m4049h.y, m5059h.x*m5059h.y, m6069h.x*m6069h.y, m70uph.x*m70uph.y, 
	f0009h.x*f0009h.y, f1019h.x*f1019h.y, f2029h.x*f2029h.y, f3039h.x*f3039h.y, 
	f4049h.x*f4049h.y, f5059h.x*f5059h.y, f6069h.x*f6069h.y, f70uph.x*f70uph.y
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12trcl) <- c("geoid10", 
	"m0009w", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", "m6069w", "m70upw", 
	"f0009w", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", "m5059b", "m6069b", "m70upb", 
	"f0009b", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", "m5059a", "m6069a", "m70upa", 
	"f0009a", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", "m5059o", "m6069o", "m70upo", 
	"f0009o", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", "m5059h", "m6069h", "m70uph", 
	"f0009h", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", "f6069h", "f70uph"
	)

## round population counts to digits=decimal places
pct12trcl <- round (pct12trcl, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12trcl, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## convert tract IDs to county IDs and sums tracts by age group within counties
## to check for race/ethnicity tract sums against race/ethnicity county sums
tr.chk <- pct12trcl
sub <- substring(tr.chk$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub.n <- as.numeric(sub) ## converts county id character string to numeric
tr.chk$geoid10 <- sub.n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum.chk <- aggregate(. ~ geoid10, data=tr.chk, FUN=sum, na.rm=TRUE) ## sum data.frame by id10
trtsum.chk <- trtsum.chk[,-1]
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(trtsum.chk[, grep("w", names(trtsum.chk))])
b <- rowSums(trtsum.chk[, grep("b", names(trtsum.chk))])
a <- rowSums(trtsum.chk[, grep("a", names(trtsum.chk))])
o <- rowSums(trtsum.chk[, grep("o", names(trtsum.chk))])
h <- rowSums(trtsum.chk[, grep("h", names(trtsum.chk))])
trtsum.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(co[, grep("w", names(co))])
b <- rowSums(co[, grep("b", names(co))])
a <- rowSums(co[, grep("a", names(co))])
o <- rowSums(co[, grep("o", names(co))])
h <- rowSums(co[, grep("h", names(co))])
co.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
colSums(trtsum.chk - co.chk)

write.csv(pct12trcl, paste(data.out, "pct1230trcl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1230trcl.tfr.csv", sep = ""))
write.csv(trtsum.chk, paste(data.out, "pct1230trcl.re.sums.csv", sep = ""))




##############################################################################
##############################################################################
##############################################################################
## starting 2040 asre tract projections here
##############################################################################

## load race/ethnic specific variables as base and launch year data variables
b <- read.csv(paste(data.out, "pct1220tr.csv", sep = ""))[,-1]
l <- pct12trcl

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 cohorts from launch year and 70up from base year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
b2 <- b %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
b2 <- b2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## calculate ccr
geoid10 <- l$geoid10
ccr <- l2/b2
ccr$geoid10 <- geoid10

## convert all Inf to NaN
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

names(ccr) <- c("geoid10", 
	"m00wccr", "m10wccr", "m20wccr", "m30wccr", "m40wccr", "m50wccr", "m60wccr", 
	"f00wccr", "f10wccr", "f20wccr", "f30wccr", "f40wccr", "f50wccr", "f60wccr", 
	"m00bccr", "m10bccr", "m20bccr", "m30bccr", "m40bccr", "m50bccr", "m60bccr", 
	"f00bccr", "f10bccr", "f20bccr", "f30bccr", "f40bccr", "f50bccr", "f60bccr",
	"m00accr", "m10accr", "m20accr", "m30accr", "m40accr", "m50accr", "m60accr", 
	"f00accr", "f10accr", "f20accr", "f30accr", "f40accr", "f50accr", "f60accr",
	"m00occr", "m10occr", "m20occr", "m30occr", "m40occr", "m50occr", "m60occr", 
	"f00occr", "f10occr", "f20occr", "f30occr", "f40occr", "f50occr", "f60occr",
	"m00hccr", "m10hccr", "m20hccr", "m30hccr", "m40hccr", "m50hccr", "m60hccr", 
	"f00hccr", "f10hccr", "f20hccr", "f30hccr", "f40hccr", "f50hccr", "f60hccr"
	)
	
## convert all Inf to NaN and NaN to 0
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

## set ceiling and floors for ccrgrowth and loss rates
## based on controlled county age/sex/race specific rates
ccr.co <- read.csv(paste(data.out, "pct1240cocl.ccr.csv", sep = ""))[,-1]

## create 5 digit county id as long as tracts
sub10 <- substring(ccr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
sub10n <- data.frame(sub10n)
names(sub10n) <- c("geoid10")
ccr.tr <- merge(sub10n, ccr.co, by="geoid10")
ccr.tr$geoid10 <- geoid10

ceiling <- ccr.tr
floor <- ccr.tr

ccr.cl <- ccr ## create new data frame with same dimensions
ccr.cl[ccr > ceiling] <- ceiling[ccr > ceiling]
ccr.cl[ccr < floor] <- floor[ccr < floor]

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l2 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l3 <- l2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]
	
## Hamilton-Perry Projection from launch year to target year
t.proj <- l3*ccr.cl
t.proj$geoid10 <- geoid10

names(t.proj) <- c("geoid10", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", 
	"m6069w", "m70upw", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", 
	"f6069w", "f70upw",
	"m1019b", "m2029b", "m3039b", "m4049b", "m5059b", 
	"m6069b", "m70upb", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", 
	"f6069b", "f70upb",
	"m1019a", "m2029a", "m3039a", "m4049a", "m5059a", 
	"m6069a", "m70upa", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", 
	"f6069a", "f70upa",
	"m1019o", "m2029o", "m3039o", "m4049o", "m5059o", 
	"m6069o", "m70upo", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", 
	"f6069o", "f70upo",
	"m1019h", "m2029h", "m3039h", "m4049h", "m5059h", 
	"m6069h", "m70uph", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", 
	"f6069h", "f70uph"
	)

## convert all Inf to NaN
is.na(t.proj) <- sapply(t.proj, is.infinite)

## convert all NaN to 0
t.proj <- replace(t.proj, is.na(t.proj), 0)

## convert all Inf to NaN# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE, na.action=NULL)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all NaN to 0
is.na(l.cwr) <- sapply(l.cwr, is.infinite)
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male and female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## convert all Inf to NaN and NaN to 0
is.na(t.cwrmerge) <- sapply(t.cwrmerge, is.infinite)
t.cwrmerge <- replace(t.cwrmerge, is.na(t.cwrmerge), 0) ## convert all NaN to 0

c.tfr <- read.csv(paste(data.out, "pct1240cocl.tfr.csv", sep = ""))[,-1]
names(c.tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## convert tract IDs to county IDs
sub10 <- substring(t.proj$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
t.proj$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame

t.tfrmerge <- merge(t.proj, c.tfr, by="geoid10")
t.tfrmerge$geoid10 <- geoid10

## convert all Inf to NaN and NaN to 0
is.na(t.tfrmerge) <- sapply(t.tfrmerge, is.infinite)
t.tfrmerge <- replace(t.tfrmerge, is.na(t.tfrmerge), 0) ## convert all NaN to 0

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5) & geography level (last two)
pct12tr <- aggregate(cbind(
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	m1019w, m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	f1019w, f2029w, f3039w, f4049w, f5059w, f6069w, f70upw,
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	m1019b, m2029b, m3039b, m4049b, m5059b, m6069b, m70upb, 
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	f1019b, f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	m1019a, m2029a, m3039a, m4049a, m5059a, m6069a, m70upa, 
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	f1019a, f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	m1019o, m2029o, m3039o, m4049o, m5059o, m6069o, m70upo, 
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	f1019o, f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	m1019h, m2029h, m3039h, m4049h, m5059h, m6069h, m70uph, 
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	f1019h, f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.tfrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)



######################################################################################
## starting 2040 growth/decline controlled tract projections here 
######################################################################################

geoid10 <- l$geoid10 ## save for reattachment

## calculate pop sum for launch year tracts
l.sum <- rowSums(l[,-1])
l.sum <- data.frame(geoid10, l.sum)

## calculate pop sum of target year tracts
t.sum <- rowSums(pct12tr[,-1])
t.sum <- data.frame(geoid10, t.sum)

## calculate growth rate for 
## ann.gr <- ((t.sum/l.sum)^(1/10)) - 1 ## geometric growth rate
ann.gr <- (log(t.sum/l.sum))/10 ## exponential growth rate
ann.gr <- data.frame(ann.gr)
ann.gr$geoid10 <- geoid10
is.na(ann.gr) <- sapply(ann.gr, is.infinite)
ann.gr <- replace(ann.gr, is.na(ann.gr), 0)
names(ann.gr) <- c("geoid10", "ann.gr")

## merge pop counts and growth rate data tables
lt.sum <- merge (l.sum, t.sum, by="geoid10")
lt.sum <- merge (lt.sum, ann.gr, by="geoid10")

## set annual growth rate ceiling and floorby generating target population counts
## under those rates
t.sum.gr <- l.sum[,-1]*((1.05)^10)
t.sum.gr <- data.frame(geoid10, t.sum.gr)
t.sum.de <- l.sum[,-1]*((0.98)^10)
t.sum.de <- data.frame(geoid10, t.sum.de)

## merge all pop counts and growth rate columns together into one dataframe
t.sum.rt <- merge(t.sum.gr, t.sum.de, by="geoid10")
t.sum.rt <- round (t.sum.rt, digits=0)
rt <- merge(lt.sum, t.sum.rt, by="geoid10")

## set ceiling and floors for growth and loss rates of tract totals
nms <- paste("col", 1:2, sep = ".")
ceiling <- as.data.frame(matrix(rt$t.sum.gr, nrow = 419, ncol = 2))
names(ceiling) <- nms

floor <- as.data.frame(matrix(rt$t.sum.de, nrow = 419, ncol = 2))
names(floor) <- nms

t.sum.cl <- t.sum ## create new data frame with same dimensions
t.sum.cl[ann.gr > 0.05] <- ceiling[ann.gr > 0.05]
t.sum.cl[ann.gr < -0.02] <- floor[ann.gr < -0.02]
t.sum.cl$geoid10 <- geoid10

af <- t.sum.cl/t.sum
af$geoid10 <- geoid10
names(af) <- c("geoid10", "af")
pct12tr.cl <- merge(pct12tr, af, by="geoid10")

## convert all NaN to 0
is.na(pct12tr.cl) <- sapply(pct12tr.cl, is.infinite)
pct12tr.cl <- replace(pct12tr.cl, is.na(pct12tr.cl), 0)

pct12tr <- aggregate(cbind(m0009w*af, m1019w*af, 
	m2029w*af, m3039w*af, m4049w*af, m5059w*af, m6069w*af, m70upw*af, 
	f0009w*af, f1019w*af, f2029w*af, f3039w*af, f4049w*af, 
	f5059w*af, f6069w*af, f70upw*af,
	m0009b*af, m1019b*af, 
	m2029b*af, m3039b*af, m4049b*af, m5059b*af, m6069b*af, m70upb*af, 
	f0009b*af, f1019b*af, f2029b*af, f3039b*af, f4049b*af, 
	f5059b*af, f6069b*af, f70upb*af,
	m0009a*af, m1019a*af, 
	m2029a*af, m3039a*af, m4049a*af, m5059a*af, m6069a*af, m70upa*af, 
	f0009a*af, f1019a*af, f2029a*af, f3039a*af, f4049a*af, 
	f5059a*af, f6069a*af, f70upa*af,
	m0009o*af, m1019o*af, 
	m2029o*af, m3039o*af, m4049o*af, m5059o*af, m6069o*af, m70upo*af, 
	f0009o*af, f1019o*af, f2029o*af, f3039o*af, f4049o*af, 
	f5059o*af, f6069o*af, f70upo*af,
	m0009h*af, m1019h*af, 
	m2029h*af, m3039h*af, m4049h*af, m5059h*af, m6069h*af, m70uph*af, 
	f0009h*af, f1019h*af, f2029h*af, f3039h*af, f4049h*af, 
	f5059h*af, f6069h*af, f70uph*af
	)~geoid10, data=pct12tr.cl, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)
	

## round population counts to digits=decimal places
pct12tr <- round (pct12tr, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12tr, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

tfr <- round (tfr, digits=1)

write.csv(pct12tr, paste(data.out, "pct1240tr.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1240tr.tfr.csv", sep = ""))




######################################################################################
## starting 2040 asre controlled tract projections here 
######################################################################################

## load target tract and county data
tr <- pct12tr
co <- read.csv(paste(data.out, "pct1240cocl.csv", sep = ""))[,-1]

## save geoid10 tract id to reattach
geoid10 <- tr$geoid10

## convert tract IDs to county IDs and sums tracts by age group within counties
sub10 <- substring(tr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
tr$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum <- aggregate(. ~ geoid10, data=tr, FUN=sum, na.rm=TRUE) ## sum data.frame by id10

cntyid <- trtsum$geoid10

## calculate adjustment factor for target years
af <- co/trtsum ## creates data frame of adjustment factors (i.e. county AG/sum of tracts AG)
af$geoid10 <- cntyid ## reattaches county IDs to data frame

## convert all Inf to NaN
is.na(af) <- sapply(af, is.infinite)

## convert all NaN to 0
af <- replace(af, is.na(af), 0)

control <- merge(tr, af, by="geoid10") ## merge trct data with adjustment factors by id
control$geoid10 <- geoid10 ## appends tract id 2010 back to data frame

pct12trcl <- aggregate(cbind(
	m0009w.x*m0009w.y, m1019w.x*m1019w.y, m2029w.x*m2029w.y, m3039w.x*m3039w.y, 
	m4049w.x*m4049w.y, m5059w.x*m5059w.y, m6069w.x*m6069w.y, m70upw.x*m70upw.y, 
	f0009w.x*f0009w.y, f1019w.x*f1019w.y, f2029w.x*f2029w.y, f3039w.x*f3039w.y, 
	f4049w.x*f4049w.y, f5059w.x*f5059w.y, f6069w.x*f6069w.y, f70upw.x*f70upw.y,
	m0009b.x*m0009b.y, m1019b.x*m1019b.y, m2029b.x*m2029b.y, m3039b.x*m3039b.y, 
	m4049b.x*m4049b.y, m5059b.x*m5059b.y, m6069b.x*m6069b.y, m70upb.x*m70upb.y, 
	f0009b.x*f0009b.y, f1019b.x*f1019b.y, f2029b.x*f2029b.y, f3039b.x*f3039b.y, 
	f4049b.x*f4049b.y, f5059b.x*f5059b.y, f6069b.x*f6069b.y, f70upb.x*f70upb.y,
	m0009a.x*m0009a.y, m1019a.x*m1019a.y, m2029a.x*m2029a.y, m3039a.x*m3039a.y, 
	m4049a.x*m4049a.y, m5059a.x*m5059a.y, m6069a.x*m6069a.y, m70upa.x*m70upa.y, 
	f0009a.x*f0009a.y, f1019a.x*f1019a.y, f2029a.x*f2029a.y, f3039a.x*f3039a.y, 
	f4049a.x*f4049a.y, f5059a.x*f5059a.y, f6069a.x*f6069a.y, f70upa.x*f70upa.y,
	m0009o.x*m0009o.y, m1019o.x*m1019o.y, m2029o.x*m2029o.y, m3039o.x*m3039o.y, 
	m4049o.x*m4049o.y, m5059o.x*m5059o.y, m6069o.x*m6069o.y, m70upo.x*m70upo.y, 
	f0009o.x*f0009o.y, f1019o.x*f1019o.y, f2029o.x*f2029o.y, f3039o.x*f3039o.y, 
	f4049o.x*f4049o.y, f5059o.x*f5059o.y, f6069o.x*f6069o.y, f70upo.x*f70upo.y,
	m0009h.x*m0009h.y, m1019h.x*m1019h.y, m2029h.x*m2029h.y, m3039h.x*m3039h.y, 
	m4049h.x*m4049h.y, m5059h.x*m5059h.y, m6069h.x*m6069h.y, m70uph.x*m70uph.y, 
	f0009h.x*f0009h.y, f1019h.x*f1019h.y, f2029h.x*f2029h.y, f3039h.x*f3039h.y, 
	f4049h.x*f4049h.y, f5059h.x*f5059h.y, f6069h.x*f6069h.y, f70uph.x*f70uph.y
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12trcl) <- c("geoid10", 
	"m0009w", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", "m6069w", "m70upw", 
	"f0009w", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", "m5059b", "m6069b", "m70upb", 
	"f0009b", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", "m5059a", "m6069a", "m70upa", 
	"f0009a", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", "m5059o", "m6069o", "m70upo", 
	"f0009o", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", "m5059h", "m6069h", "m70uph", 
	"f0009h", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", "f6069h", "f70uph"
	)

## round population counts to digits=decimal places
pct12trcl <- round (pct12trcl, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12trcl, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## convert tract IDs to county IDs and sums tracts by age group within counties
## to check for race/ethnicity tract sums against race/ethnicity county sums
tr.chk <- pct12trcl
sub <- substring(tr.chk$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub.n <- as.numeric(sub) ## converts county id character string to numeric
tr.chk$geoid10 <- sub.n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum.chk <- aggregate(. ~ geoid10, data=tr.chk, FUN=sum, na.rm=TRUE) ## sum data.frame by id10
trtsum.chk <- trtsum.chk[,-1]
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(trtsum.chk[, grep("w", names(trtsum.chk))])
b <- rowSums(trtsum.chk[, grep("b", names(trtsum.chk))])
a <- rowSums(trtsum.chk[, grep("a", names(trtsum.chk))])
o <- rowSums(trtsum.chk[, grep("o", names(trtsum.chk))])
h <- rowSums(trtsum.chk[, grep("h", names(trtsum.chk))])
trtsum.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(co[, grep("w", names(co))])
b <- rowSums(co[, grep("b", names(co))])
a <- rowSums(co[, grep("a", names(co))])
o <- rowSums(co[, grep("o", names(co))])
h <- rowSums(co[, grep("h", names(co))])
co.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
colSums(trtsum.chk - co.chk)

write.csv(pct12trcl, paste(data.out, "pct1240trcl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1240trcl.tfr.csv", sep = ""))
write.csv(trtsum.chk, paste(data.out, "pct1240trcl.re.sums.csv", sep = ""))




##############################################################################
##############################################################################
##############################################################################
## starting 2050 asre tract projections here
##############################################################################

## load race/ethnic specific variables as base and launch year data variables
b <- read.csv(paste(data.out, "pct1230tr.csv", sep = ""))[,-1]
l <- pct12trcl

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 cohorts from launch year and 70up from base year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
b2 <- b %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
b2 <- b2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## calculate ccr
geoid10 <- l$geoid10
ccr <- l2/b2
ccr$geoid10 <- geoid10

## convert all Inf to NaN
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

names(ccr) <- c("geoid10", 
	"m00wccr", "m10wccr", "m20wccr", "m30wccr", "m40wccr", "m50wccr", "m60wccr", 
	"f00wccr", "f10wccr", "f20wccr", "f30wccr", "f40wccr", "f50wccr", "f60wccr", 
	"m00bccr", "m10bccr", "m20bccr", "m30bccr", "m40bccr", "m50bccr", "m60bccr", 
	"f00bccr", "f10bccr", "f20bccr", "f30bccr", "f40bccr", "f50bccr", "f60bccr",
	"m00accr", "m10accr", "m20accr", "m30accr", "m40accr", "m50accr", "m60accr", 
	"f00accr", "f10accr", "f20accr", "f30accr", "f40accr", "f50accr", "f60accr",
	"m00occr", "m10occr", "m20occr", "m30occr", "m40occr", "m50occr", "m60occr", 
	"f00occr", "f10occr", "f20occr", "f30occr", "f40occr", "f50occr", "f60occr",
	"m00hccr", "m10hccr", "m20hccr", "m30hccr", "m40hccr", "m50hccr", "m60hccr", 
	"f00hccr", "f10hccr", "f20hccr", "f30hccr", "f40hccr", "f50hccr", "f60hccr"
	)
	
## convert all Inf to NaN and NaN to 0
is.na(ccr) <- sapply(ccr, is.infinite)
ccr <- replace(ccr, is.na(ccr), 0) ## convert all NaN to 0

## set ceiling and floors for ccrgrowth and loss rates
## based on controlled county age/sex/race specific rates
ccr.co <- read.csv(paste(data.out, "pct1250cocl.ccr.csv", sep = ""))[,-1]

## create 5 digit county id as long as tracts
sub10 <- substring(ccr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
sub10n <- data.frame(sub10n)
names(sub10n) <- c("geoid10")
ccr.tr <- merge(sub10n, ccr.co, by="geoid10")
ccr.tr$geoid10 <- geoid10

ceiling <- ccr.tr
floor <- ccr.tr

ccr.cl <- ccr ## create new data frame with same dimensions
ccr.cl[ccr > ceiling] <- ceiling[ccr > ceiling]
ccr.cl[ccr < floor] <- floor[ccr < floor]

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l2 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l3 <- l2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]
	
## Hamilton-Perry Projection from launch year to target year
t.proj <- l3*ccr.cl
t.proj$geoid10 <- geoid10

names(t.proj) <- c("geoid10", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", 
	"m6069w", "m70upw", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", 
	"f6069w", "f70upw",
	"m1019b", "m2029b", "m3039b", "m4049b", "m5059b", 
	"m6069b", "m70upb", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", 
	"f6069b", "f70upb",
	"m1019a", "m2029a", "m3039a", "m4049a", "m5059a", 
	"m6069a", "m70upa", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", 
	"f6069a", "f70upa",
	"m1019o", "m2029o", "m3039o", "m4049o", "m5059o", 
	"m6069o", "m70upo", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", 
	"f6069o", "f70upo",
	"m1019h", "m2029h", "m3039h", "m4049h", "m5059h", 
	"m6069h", "m70uph", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", 
	"f6069h", "f70uph"
	)

## convert all Inf to NaN
is.na(t.proj) <- sapply(t.proj, is.infinite)

## convert all NaN to 0
t.proj <- replace(t.proj, is.na(t.proj), 0)

## convert all Inf to NaN# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE, na.action=NULL)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all NaN to 0
is.na(l.cwr) <- sapply(l.cwr, is.infinite)
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male and female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## convert all Inf to NaN and NaN to 0
is.na(t.cwrmerge) <- sapply(t.cwrmerge, is.infinite)
t.cwrmerge <- replace(t.cwrmerge, is.na(t.cwrmerge), 0) ## convert all NaN to 0

c.tfr <- read.csv(paste(data.out, "pct1250cocl.tfr.csv", sep = ""))[,-1]
names(c.tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## convert tract IDs to county IDs
sub10 <- substring(t.proj$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
t.proj$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame

t.tfrmerge <- merge(t.proj, c.tfr, by="geoid10")
t.tfrmerge$geoid10 <- geoid10

## convert all Inf to NaN and NaN to 0
is.na(t.tfrmerge) <- sapply(t.tfrmerge, is.infinite)
t.tfrmerge <- replace(t.tfrmerge, is.na(t.tfrmerge), 0) ## convert all NaN to 0

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5) & geography level (last two)
pct12tr <- aggregate(cbind(
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	m1019w, m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(10*(tfrw/35)*((f1019w/2)+f2029w+f3039w+f4049w))/2, 
	f1019w, f2029w, f3039w, f4049w, f5059w, f6069w, f70upw,
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	m1019b, m2029b, m3039b, m4049b, m5059b, m6069b, m70upb, 
	(10*(tfrb/35)*((f1019b/2)+f2029b+f3039b+f4049b))/2, 
	f1019b, f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	m1019a, m2029a, m3039a, m4049a, m5059a, m6069a, m70upa, 
	(10*(tfra/35)*((f1019a/2)+f2029a+f3039a+f4049a))/2, 
	f1019a, f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	m1019o, m2029o, m3039o, m4049o, m5059o, m6069o, m70upo, 
	(10*(tfro/35)*((f1019o/2)+f2029o+f3039o+f4049o))/2, 
	f1019o, f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	m1019h, m2029h, m3039h, m4049h, m5059h, m6069h, m70uph, 
	(10*(tfrh/35)*((f1019h/2)+f2029h+f3039h+f4049h))/2, 
	f1019h, f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.tfrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)



######################################################################################
## starting 2050 growth/decline controlled tract projections here 
######################################################################################

geoid10 <- l$geoid10 ## save for reattachment

## calculate pop sum for launch year tracts
l.sum <- rowSums(l[,-1])
l.sum <- data.frame(geoid10, l.sum)

## calculate pop sum of target year tracts
t.sum <- rowSums(pct12tr[,-1])
t.sum <- data.frame(geoid10, t.sum)

## calculate growth rate for 
## ann.gr <- ((t.sum/l.sum)^(1/10)) - 1 ## geometric growth rate
ann.gr <- (log(t.sum/l.sum))/10 ## exponential growth rate
ann.gr <- data.frame(ann.gr)
ann.gr$geoid10 <- geoid10
is.na(ann.gr) <- sapply(ann.gr, is.infinite)
ann.gr <- replace(ann.gr, is.na(ann.gr), 0)
names(ann.gr) <- c("geoid10", "ann.gr")

## merge pop counts and growth rate data tables
lt.sum <- merge (l.sum, t.sum, by="geoid10")
lt.sum <- merge (lt.sum, ann.gr, by="geoid10")

## set annual growth rate ceiling and floorby generating target population counts
## under those rates
t.sum.gr <- l.sum[,-1]*((1.05)^10)
t.sum.gr <- data.frame(geoid10, t.sum.gr)
t.sum.de <- l.sum[,-1]*((0.98)^10)
t.sum.de <- data.frame(geoid10, t.sum.de)

## merge all pop counts and growth rate columns together into one dataframe
t.sum.rt <- merge(t.sum.gr, t.sum.de, by="geoid10")
t.sum.rt <- round (t.sum.rt, digits=0)
rt <- merge(lt.sum, t.sum.rt, by="geoid10")

## set ceiling and floors for growth and loss rates of tract totals
nms <- paste("col", 1:2, sep = ".")
ceiling <- as.data.frame(matrix(rt$t.sum.gr, nrow = 419, ncol = 2))
names(ceiling) <- nms

floor <- as.data.frame(matrix(rt$t.sum.de, nrow = 419, ncol = 2))
names(floor) <- nms

t.sum.cl <- t.sum ## create new data frame with same dimensions
t.sum.cl[ann.gr > 0.05] <- ceiling[ann.gr > 0.05]
t.sum.cl[ann.gr < -0.02] <- floor[ann.gr < -0.02]
t.sum.cl$geoid10 <- geoid10

af <- t.sum.cl/t.sum
af$geoid10 <- geoid10
names(af) <- c("geoid10", "af")
pct12tr.cl <- merge(pct12tr, af, by="geoid10")

## convert all NaN to 0
is.na(pct12tr.cl) <- sapply(pct12tr.cl, is.infinite)
pct12tr.cl <- replace(pct12tr.cl, is.na(pct12tr.cl), 0)

pct12tr <- aggregate(cbind(m0009w*af, m1019w*af, 
	m2029w*af, m3039w*af, m4049w*af, m5059w*af, m6069w*af, m70upw*af, 
	f0009w*af, f1019w*af, f2029w*af, f3039w*af, f4049w*af, 
	f5059w*af, f6069w*af, f70upw*af,
	m0009b*af, m1019b*af, 
	m2029b*af, m3039b*af, m4049b*af, m5059b*af, m6069b*af, m70upb*af, 
	f0009b*af, f1019b*af, f2029b*af, f3039b*af, f4049b*af, 
	f5059b*af, f6069b*af, f70upb*af,
	m0009a*af, m1019a*af, 
	m2029a*af, m3039a*af, m4049a*af, m5059a*af, m6069a*af, m70upa*af, 
	f0009a*af, f1019a*af, f2029a*af, f3039a*af, f4049a*af, 
	f5059a*af, f6069a*af, f70upa*af,
	m0009o*af, m1019o*af, 
	m2029o*af, m3039o*af, m4049o*af, m5059o*af, m6069o*af, m70upo*af, 
	f0009o*af, f1019o*af, f2029o*af, f3039o*af, f4049o*af, 
	f5059o*af, f6069o*af, f70upo*af,
	m0009h*af, m1019h*af, 
	m2029h*af, m3039h*af, m4049h*af, m5059h*af, m6069h*af, m70uph*af, 
	f0009h*af, f1019h*af, f2029h*af, f3039h*af, f4049h*af, 
	f5059h*af, f6069h*af, f70uph*af
	)~geoid10, data=pct12tr.cl, sum, na.rm=TRUE, na.action=NULL)
names(pct12tr) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
	"m5059w", "m6069w", "m70upw", "f0009w", "f1019w", "f2029w", "f3039w", 
	"f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", 
	"m5059b", "m6069b", "m70upb", "f0009b", "f1019b", "f2029b", "f3039b", 
	"f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", 
	"m5059a", "m6069a", "m70upa", "f0009a", "f1019a", "f2029a", "f3039a", 
	"f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", 
	"m5059o", "m6069o", "m70upo", "f0009o", "f1019o", "f2029o", "f3039o", 
	"f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", 
	"m5059h", "m6069h", "m70uph", "f0009h", "f1019h", "f2029h", "f3039h", 
	"f4049h", "f5059h", "f6069h", "f70uph"
	)
	

## round population counts to digits=decimal places
pct12tr <- round (pct12tr, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12tr, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

tfr <- round (tfr, digits=1)

write.csv(pct12tr, paste(data.out, "pct1250tr.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1250tr.tfr.csv", sep = ""))




######################################################################################
## starting 2050 asre controlled tract projections here 
######################################################################################

## load target tract and county data
tr <- pct12tr
co <- read.csv(paste(data.out, "pct1250cocl.csv", sep = ""))[,-1]

## save geoid10 tract id to reattach
geoid10 <- tr$geoid10

## convert tract IDs to county IDs and sums tracts by age group within counties
sub10 <- substring(tr$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub10n <- as.numeric(sub10) ## converts county id character string to numeric
tr$geoid10 <- sub10n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum <- aggregate(. ~ geoid10, data=tr, FUN=sum, na.rm=TRUE) ## sum data.frame by id10

cntyid <- trtsum$geoid10

## calculate adjustment factor for target years
af <- co/trtsum ## creates data frame of adjustment factors (i.e. county AG/sum of tracts AG)
af$geoid10 <- cntyid ## reattaches county IDs to data frame

## convert all Inf to NaN
is.na(af) <- sapply(af, is.infinite)

## convert all NaN to 0
af <- replace(af, is.na(af), 0)
control <- merge(tr, af, by="geoid10") ## merge trct data with adjustment factors by id
control$geoid10 <- geoid10 ## appends tract id 2010 back to data frame

pct12trcl <- aggregate(cbind(
	m0009w.x*m0009w.y, m1019w.x*m1019w.y, m2029w.x*m2029w.y, m3039w.x*m3039w.y, 
	m4049w.x*m4049w.y, m5059w.x*m5059w.y, m6069w.x*m6069w.y, m70upw.x*m70upw.y, 
	f0009w.x*f0009w.y, f1019w.x*f1019w.y, f2029w.x*f2029w.y, f3039w.x*f3039w.y, 
	f4049w.x*f4049w.y, f5059w.x*f5059w.y, f6069w.x*f6069w.y, f70upw.x*f70upw.y,
	m0009b.x*m0009b.y, m1019b.x*m1019b.y, m2029b.x*m2029b.y, m3039b.x*m3039b.y, 
	m4049b.x*m4049b.y, m5059b.x*m5059b.y, m6069b.x*m6069b.y, m70upb.x*m70upb.y, 
	f0009b.x*f0009b.y, f1019b.x*f1019b.y, f2029b.x*f2029b.y, f3039b.x*f3039b.y, 
	f4049b.x*f4049b.y, f5059b.x*f5059b.y, f6069b.x*f6069b.y, f70upb.x*f70upb.y,
	m0009a.x*m0009a.y, m1019a.x*m1019a.y, m2029a.x*m2029a.y, m3039a.x*m3039a.y, 
	m4049a.x*m4049a.y, m5059a.x*m5059a.y, m6069a.x*m6069a.y, m70upa.x*m70upa.y, 
	f0009a.x*f0009a.y, f1019a.x*f1019a.y, f2029a.x*f2029a.y, f3039a.x*f3039a.y, 
	f4049a.x*f4049a.y, f5059a.x*f5059a.y, f6069a.x*f6069a.y, f70upa.x*f70upa.y,
	m0009o.x*m0009o.y, m1019o.x*m1019o.y, m2029o.x*m2029o.y, m3039o.x*m3039o.y, 
	m4049o.x*m4049o.y, m5059o.x*m5059o.y, m6069o.x*m6069o.y, m70upo.x*m70upo.y, 
	f0009o.x*f0009o.y, f1019o.x*f1019o.y, f2029o.x*f2029o.y, f3039o.x*f3039o.y, 
	f4049o.x*f4049o.y, f5059o.x*f5059o.y, f6069o.x*f6069o.y, f70upo.x*f70upo.y,
	m0009h.x*m0009h.y, m1019h.x*m1019h.y, m2029h.x*m2029h.y, m3039h.x*m3039h.y, 
	m4049h.x*m4049h.y, m5059h.x*m5059h.y, m6069h.x*m6069h.y, m70uph.x*m70uph.y, 
	f0009h.x*f0009h.y, f1019h.x*f1019h.y, f2029h.x*f2029h.y, f3039h.x*f3039h.y, 
	f4049h.x*f4049h.y, f5059h.x*f5059h.y, f6069h.x*f6069h.y, f70uph.x*f70uph.y
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12trcl) <- c("geoid10", 
	"m0009w", "m1019w", "m2029w", "m3039w", "m4049w", "m5059w", "m6069w", "m70upw", 
	"f0009w", "f1019w", "f2029w", "f3039w", "f4049w", "f5059w", "f6069w", "f70upw",
	"m0009b", "m1019b", "m2029b", "m3039b", "m4049b", "m5059b", "m6069b", "m70upb", 
	"f0009b", "f1019b", "f2029b", "f3039b", "f4049b", "f5059b", "f6069b", "f70upb",
	"m0009a", "m1019a", "m2029a", "m3039a", "m4049a", "m5059a", "m6069a", "m70upa", 
	"f0009a", "f1019a", "f2029a", "f3039a", "f4049a", "f5059a", "f6069a", "f70upa",
	"m0009o", "m1019o", "m2029o", "m3039o", "m4049o", "m5059o", "m6069o", "m70upo", 
	"f0009o", "f1019o", "f2029o", "f3039o", "f4049o", "f5059o", "f6069o", "f70upo",
	"m0009h", "m1019h", "m2029h", "m3039h", "m4049h", "m5059h", "m6069h", "m70uph", 
	"f0009h", "f1019h", "f2029h", "f3039h", "f4049h", "f5059h", "f6069h", "f70uph"
	)

## round population counts to digits=decimal places
pct12trcl <- round (pct12trcl, digits=0)

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12trcl, sum, na.rm=TRUE)
names(tfr) <- c("geoid10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## convert tract IDs to county IDs and sums tracts by age group within counties
## to check for race/ethnicity tract sums against race/ethnicity county sums
tr.chk <- pct12trcl
sub <- substring(tr.chk$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
sub.n <- as.numeric(sub) ## converts county id character string to numeric
tr.chk$geoid10 <- sub.n ## attach new numeric "vector" to original data.frame as column with id10 header
trtsum.chk <- aggregate(. ~ geoid10, data=tr.chk, FUN=sum, na.rm=TRUE) ## sum data.frame by id10
trtsum.chk <- trtsum.chk[,-1]
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(trtsum.chk[, grep("w", names(trtsum.chk))])
b <- rowSums(trtsum.chk[, grep("b", names(trtsum.chk))])
a <- rowSums(trtsum.chk[, grep("a", names(trtsum.chk))])
o <- rowSums(trtsum.chk[, grep("o", names(trtsum.chk))])
h <- rowSums(trtsum.chk[, grep("h", names(trtsum.chk))])
trtsum.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
## combine all age/sex cohorts into race/ethnicity groups
w <- rowSums(co[, grep("w", names(co))])
b <- rowSums(co[, grep("b", names(co))])
a <- rowSums(co[, grep("a", names(co))])
o <- rowSums(co[, grep("o", names(co))])
h <- rowSums(co[, grep("h", names(co))])
co.chk <- data.frame(cbind(cntyid, w,b,a,o,h))
colSums(trtsum.chk - co.chk)

write.csv(pct12trcl, paste(data.out, "pct1250trcl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1250trcl.tfr.csv", sep = ""))
write.csv(trtsum.chk, paste(data.out, "pct1250trcl.re.sums.csv", sep = ""))



##################################################################
## population (Age/Sex/Race/Ethnicity) sums by tracts 2000 - 2050 
##################################################################

pop <- read.csv(paste(data.out, "pct1250trcl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop50 <- rowSums(pop, na.rm=TRUE)
pop50 <- data.frame(cbind(geoid10, pop50))

pop <- read.csv(paste(data.out, "pct1200tr.csv", sep = ""))[,-1]
pop <- pop[pop$geoid10 %in% pop50$geoid10,]
geoid10 <- pop$geoid10
pop <- pop[,-1]
pop00 <- rowSums(pop, na.rm=TRUE)
pop00 <- data.frame(cbind(geoid10, pop00))

pop <- read.csv(paste(data.out, "pct1210tr.csv", sep = ""))[,-1]
pop <- pop[pop$geoid10 %in% pop50$geoid10,]
geoid10 <- pop$geoid10
pop <- pop[,-1]
pop10 <- rowSums(pop)
pop10 <- data.frame(cbind(geoid10, pop10))

pop <- read.csv(paste(data.out, "pct1220trcl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop20 <- rowSums(pop, na.rm=TRUE)
pop20 <- data.frame(cbind(geoid10, pop20))

pop <- read.csv(paste(data.out, "pct1230trcl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop30 <- rowSums(pop, na.rm=TRUE)
pop30 <- data.frame(cbind(geoid10, pop30))

pop <- read.csv(paste(data.out, "pct1240trcl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop40 <- rowSums(pop, na.rm=TRUE)
pop40 <- data.frame(cbind(geoid10, pop40))

pop0010 <- merge(pop00, pop10, by="geoid10")
pop2030 <- merge(pop20, pop30, by="geoid10")
pop4050 <- merge(pop40, pop50, by="geoid10")
pop0030 <- merge(pop0010, pop2030, by="geoid10")
pop0050 <- merge(pop0030, pop4050, by="geoid10")

write.csv(pop0050, paste(data.out, "pct12trcl.sums.csv", sep = ""))

library(ggplot2)

dat <- pop0050

dat <- dat %>%
  group_by(geoid10) %>%
  gather(popyear, estimate, pop00:pop50) %>%
  mutate(year = substr(popyear, 4,5)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(estimate = sum(estimate))

ggplot(filter(dat), aes(year, estimate)) +
  geom_point()
