## the code below projects populations using cohort change ratios (CCRs) by age/sex/race/ethnicity cohorts
## the study area is a 23 county region surrounding coastal Georgia extending into FL and SC
## county scale data from US Censuses 2000 and 2010 pct12 tables

rm(list=ls())

data.out <- "tables/projections/outputs/middle/" ## define directory path for outputs dependent on scenario
data.in <- "tables/projections/inputs/" ## define directory path for data inputs 

library(dplyr)
library(tidyr)

##############################################################################
## load race/ethnic specific variables tables for 2000 and 2010 tracts
pct1200 <- read.csv(paste(data.in, "pct1200tr.csv", sep = ""))
pct1210 <- read.csv(paste(data.in, "pct1210tr.csv", sep = ""))

## match rows by geoid10 so that we have same tracts for each data set
pct1210tr <- pct1210[pct1210$geoid10 %in% pct1200$geoid10,] 
pct1200tr <- pct1200[pct1200$geoid10 %in% pct1210$geoid10,] 
geoid10 <- pct1210tr$geoid10

## sum tract data to county level
## copy base and launch year aggregated by tract data into new data frames
## create 5-digit county IDs & sum by them to create base and launch year county data frames
pct1200co <- pct1200tr[,-1]
b.sub <- substring(pct1200co$geoid10, 1, 5) ## submsaple tract id to 5 digits, being the county id
pct1200co$geoid10 <- b.sub ## attach new numeric "vector" to original data.frame as column with cntyid10 header
b <- aggregate(. ~ geoid10, data=pct1200co, FUN=sum, na.rm=TRUE) ## sum data.frame by cntyid10
geoid10 <- as.numeric(b$geoid10)
b$geoid10 <- geoid10
pct1200co <- b ## save for later

pct1210co <- pct1210tr[,-1]
l.sub <- substring(pct1210co$geoid10, 1, 5) ## subsample tract id to 5 digits, being the county id
pct1210co$geoid10 <- l.sub ## attach new numeric "vector" to original data.frame as column with cntyid10 header
l <- aggregate(. ~ geoid10, data=pct1210co, FUN=sum, na.rm=TRUE) ## sum data.frame by cntyid10
l$geoid10 <- geoid10
pct1210co <- l ## save for plotting

## save for later
write.csv(b, paste(data.out, "pct1200co.csv", sep = "")) ## save for use later
write.csv(l, paste(data.out, "pct1210co.csv", sep = "")) ## save for use later



#################################################################################
## starting 2020 asre county projections here 
#################################################################################

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 age cohorts male and female from launch year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
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

## change hispanic ccr rate to 0.5 of 2000 to 2010
ccr2 <- aggregate(cbind( 
	m00wccr, m10wccr, m20wccr, m30wccr, m40wccr, m50wccr, m60wccr, 
	f00wccr, f10wccr, f20wccr, f30wccr, f40wccr, f50wccr, f60wccr, 
	m00bccr, m10bccr, m20bccr, m30bccr, m40bccr, m50bccr, m60bccr, 
	f00bccr, f10bccr, f20bccr, f30bccr, f40bccr, f50bccr, f60bccr,
	m00accr, m10accr, m20accr, m30accr, m40accr, m50accr, m60accr, 
	f00accr, f10accr, f20accr, f30accr, f40accr, f50accr, f60accr,
	m00occr, m10occr, m20occr, m30occr, m40occr, m50occr, m60occr, 
	f00occr, f10occr, f20occr, f30occr, f40occr, f50occr, f60occr,
	m00hccr/2, m10hccr/2, m20hccr/2, m30hccr/2, m40hccr/2, m50hccr/2, m60hccr/2, 
	f00hccr/2, f10hccr/2, f20hccr/2, f30hccr/2, f40hccr/2, f50hccr/2, f60hccr/2
	)~geoid10, data=ccr, sum, na.rm=TRUE)


## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l3 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l4 <- l3[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	
	
## Hamilton-Perry Projection from launch year to target year
t.proj <- l4*ccr2
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

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35/2*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=l, sum, na.rm=TRUE)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	0.5*((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all Inf to NaN
is.na(l.cwr) <- sapply(l.cwr, is.infinite)

## convert all NaN to 0
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5) & geography level (last two)
pct12co <- aggregate(cbind((cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), m1019w, 
	m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), f1019w, f2029w, f3039w, f4049w, 
	f5059w, f6069w, f70upw,
	(cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), m1019b, m2029b, m3039b, m4049b, 
	m5059b, m6069b, m70upb, (cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), f1019b,
	f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), m1019a, m2029a, m3039a, m4049a, 
	m5059a, m6069a, m70upa, (cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), f1019a, 
	f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), m1019o, m2029o, m3039o, m4049o, 
	m5059o, m6069o, m70upo, (cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), f1019o, 
	f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), m1019h, m2029h, m3039h, m4049h, 
	m5059h, m6069h, m70uph, (cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), f1019h, 
	f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.cwrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12co) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
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
pct12co <- round (pct12co, digits=0)

write.csv(pct12co, paste(data.out, "pct1220co.csv", sep = ""))

## write iTFR for later use
tfr <- round (tfr, digits=1)
write.csv(tfr, paste(data.out, "pct1220co.tfr.csv", sep = ""))





#################################################################################################
############## starting 2020 asre county controlled projections here ############################

## load age/sex/race/ethnicity county target data
co.asre <- pct12co

## load age/sex county target data
co.as <- read.csv(paste(data.out, "p1220co.csv", sep = ""))[,-1]

## rename ID column to match co.asre
names(co.as) <- c("geoid10",
	"m0009", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
	"f0009", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

## sum by all age/sex cohorts in age/sex/race/ethnicity data
m0009 <- rowSums(co.asre[, grep("m0009", names(co.asre))])
m1019 <- rowSums(co.asre[, grep("m1019", names(co.asre))])
m2029 <- rowSums(co.asre[, grep("m2029", names(co.asre))])
m3039 <- rowSums(co.asre[, grep("m3039", names(co.asre))])
m4049 <- rowSums(co.asre[, grep("m4049", names(co.asre))])
m5059 <- rowSums(co.asre[, grep("m5059", names(co.asre))])
m6069 <- rowSums(co.asre[, grep("m6069", names(co.asre))])
m70up <- rowSums(co.asre[, grep("m70up", names(co.asre))])
f0009 <- rowSums(co.asre[, grep("f0009", names(co.asre))])
f1019 <- rowSums(co.asre[, grep("f1019", names(co.asre))])
f2029 <- rowSums(co.asre[, grep("f2029", names(co.asre))])
f3039 <- rowSums(co.asre[, grep("f3039", names(co.asre))])
f4049 <- rowSums(co.asre[, grep("f4049", names(co.asre))])
f5059 <- rowSums(co.asre[, grep("f5059", names(co.asre))])
f6069 <- rowSums(co.asre[, grep("f6069", names(co.asre))])
f70up <- rowSums(co.asre[, grep("f70up", names(co.asre))])

## extract geoid10 to reappend
geoid10 <- co.asre$geoid10

## combine into new data.frame
co.asre.sum <- data.frame(geoid10, m0009, m1019, m2029, m3039, m4049, m5059, m6069, m70up,
	 f0009, f1019, f2029, f3039, f4049, f5059, f6069, f70up)

## calculate adjustment factor for target years
af <- co.as/co.asre.sum ## creates data frame of adjustment factors (i.e. county AS cohorts/sum of county ASRE cohorts)
af$geoid10 <- geoid10 ## reattaches county IDs to data frame
control <- merge(co.asre, af, by="geoid10") ## merge trct data with adjustment factors by id

pct12cocl <- aggregate(cbind(
	m0009w*m0009, m1019w*m1019, m2029w*m2029, m3039w*m3039, 
	m4049w*m4049, m5059w*m5059, m6069w*m6069, m70upw*m70up, 
	f0009w*f0009, f1019w*f1019, f2029w*f2029, f3039w*f3039, 
	f4049w*f4049, f5059w*f5059, f6069w*f6069, f70upw*f70up,
	m0009b*m0009, m1019b*m1019, m2029b*m2029, m3039b*m3039, 
	m4049b*m4049, m5059b*m5059, m6069b*m6069, m70upb*m70up, 
	f0009b*f0009, f1019b*f1019, f2029b*f2029, f3039b*f3039, 
	f4049b*f4049, f5059b*f5059, f6069b*f6069, f70upb*f70up,
	m0009a*m0009, m1019a*m1019, m2029a*m2029, m3039a*m3039, 
	m4049a*m4049, m5059a*m5059, m6069a*m6069, m70upa*m70up, 
	f0009a*f0009, f1019a*f1019, f2029a*f2029, f3039a*f3039, 
	f4049a*f4049, f5059a*f5059, f6069a*f6069, f70upa*f70up,
	m0009o*m0009, m1019o*m1019, m2029o*m2029, m3039o*m3039, 
	m4049o*m4049, m5059o*m5059, m6069o*m6069, m70upo*m70up, 
	f0009o*f0009, f1019o*f1019, f2029o*f2029, f3039o*f3039, 
	f4049o*f4049, f5059o*f5059, f6069o*f6069, f70upo*f70up,
	m0009h*m0009, m1019h*m1019, m2029h*m2029, m3039h*m3039, 
	m4049h*m4049, m5059h*m5059, m6069h*m6069, m70uph*m70up, 
	f0009h*f0009, f1019h*f1019, f2029h*f2029, f3039h*f3039, 
	f4049h*f4049, f5059h*f5059, f6069h*f6069, f70uph*f70up
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12cocl) <- c("geoid10", 
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

pct12cocl <- round (pct12cocl, digits=0)

## check that the relative difference in sum of adjusted ASRE cohorts is small compared to sum of independent AS cohorts
diff20 <- sum(colSums(pct12cocl[,-1])) - sum(colSums(co.as[,-1])) 

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12cocl, sum, na.rm=TRUE)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## create ccr for controlled target population / launch population
## remove all 0-9 age cohorts male and female from launch year
l2 <- pct12cocl[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
b <- read.csv(paste(data.out, "pct1210co.csv", sep = ""))[,-1]

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
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

write.csv(pct12cocl, paste(data.out, "pct1220cocl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1220cocl.tfr.csv", sep = ""))
write.csv(ccr, paste(data.out, "pct1220cocl.ccr.csv", sep = ""))




#####################################################################################
############## starting 2030 asre county projections here ############################

## import base and launch years
b <- read.csv(paste(data.out, "pct1210co.csv", sep = ""))[,-1]
l <- pct12cocl

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 age cohorts male and female from launch year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
b2 <- b %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
b2 <- b2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## save geoid10 to reattach after ccr
geoid10 <- l2$geoid10

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

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l3 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l4 <- l3[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## Hamilton-Perry Projection from launch year to target year
t.proj <- l4*ccr
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

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=l, sum, na.rm=TRUE)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all Inf to NaN
is.na(l.cwr) <- sapply(l.cwr, is.infinite)

## convert all NaN to 0
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5), target year (6-7), & geography level (last two)
pct12co <- aggregate(cbind((cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), m1019w, 
	m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), f1019w, f2029w, f3039w, f4049w, 
	f5059w, f6069w, f70upw,
	(cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), m1019b, m2029b, m3039b, m4049b, 
	m5059b, m6069b, m70upb, (cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), f1019b, 
	f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), m1019a, m2029a, m3039a, m4049a, 
	m5059a, m6069a, m70upa, (cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), f1019a, 
	f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), m1019o, m2029o, m3039o, m4049o, 
	m5059o, m6069o, m70upo, (cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), f1019o, 
	f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), m1019h, m2029h, m3039h, m4049h, 
	m5059h, m6069h, m70uph, (cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), f1019h, 
	f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.cwrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12co) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
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
pct12co <- round (pct12co, digits=0)

write.csv(pct12co, paste(data.out, "pct1230co.csv", sep = ""))

## write iTFR for later use
tfr <- round (tfr, digits=1)
write.csv(tfr, paste(data.out, "pct1230co.tfr.csv", sep =""))




################################################################################################
############## starting 2030 asre county controlled projections here ############################

## load age/sex/race/ethnicity county target data
co.asre <- pct12co

## load age/sex county target data
co.as <- read.csv(paste(data.out, "p1230co.csv", sep = ""))[,-1]

## rename ID column to match co.asre
names(co.as) <- c("geoid10",
	"m0009", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
	"f0009", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

## sum by all age/sex cohorts in age/sex/race/ethnicity data
m0009 <- rowSums(co.asre[, grep("m0009", names(co.asre))])
m1019 <- rowSums(co.asre[, grep("m1019", names(co.asre))])
m2029 <- rowSums(co.asre[, grep("m2029", names(co.asre))])
m3039 <- rowSums(co.asre[, grep("m3039", names(co.asre))])
m4049 <- rowSums(co.asre[, grep("m4049", names(co.asre))])
m5059 <- rowSums(co.asre[, grep("m5059", names(co.asre))])
m6069 <- rowSums(co.asre[, grep("m6069", names(co.asre))])
m70up <- rowSums(co.asre[, grep("m70up", names(co.asre))])
f0009 <- rowSums(co.asre[, grep("f0009", names(co.asre))])
f1019 <- rowSums(co.asre[, grep("f1019", names(co.asre))])
f2029 <- rowSums(co.asre[, grep("f2029", names(co.asre))])
f3039 <- rowSums(co.asre[, grep("f3039", names(co.asre))])
f4049 <- rowSums(co.asre[, grep("f4049", names(co.asre))])
f5059 <- rowSums(co.asre[, grep("f5059", names(co.asre))])
f6069 <- rowSums(co.asre[, grep("f6069", names(co.asre))])
f70up <- rowSums(co.asre[, grep("f70up", names(co.asre))])

## extract geoid10 to reappend
geoid10 <- co.asre$geoid10

## combine into new data.frame
co.asre.sum <- data.frame(geoid10, m0009, m1019, m2029, m3039, m4049, m5059, m6069, m70up,
	 f0009, f1019, f2029, f3039, f4049, f5059, f6069, f70up)

## calculate adjustment factor for target years
af <- co.as/co.asre.sum ## creates data frame of adjustment factors (i.e. county AG/sum of tracts AG)
af$geoid10 <- geoid10 ## reattaches county IDs to data frame
control <- merge(co.asre, af, by="geoid10") ## merge trct data with adjustment factors by id

pct12cocl <- aggregate(cbind(
	m0009w*m0009, m1019w*m1019, m2029w*m2029, m3039w*m3039, 
	m4049w*m4049, m5059w*m5059, m6069w*m6069, m70upw*m70up, 
	f0009w*f0009, f1019w*f1019, f2029w*f2029, f3039w*f3039, 
	f4049w*f4049, f5059w*f5059, f6069w*f6069, f70upw*f70up,
	m0009b*m0009, m1019b*m1019, m2029b*m2029, m3039b*m3039, 
	m4049b*m4049, m5059b*m5059, m6069b*m6069, m70upb*m70up, 
	f0009b*f0009, f1019b*f1019, f2029b*f2029, f3039b*f3039, 
	f4049b*f4049, f5059b*f5059, f6069b*f6069, f70upb*f70up,
	m0009a*m0009, m1019a*m1019, m2029a*m2029, m3039a*m3039, 
	m4049a*m4049, m5059a*m5059, m6069a*m6069, m70upa*m70up, 
	f0009a*f0009, f1019a*f1019, f2029a*f2029, f3039a*f3039, 
	f4049a*f4049, f5059a*f5059, f6069a*f6069, f70upa*f70up,
	m0009o*m0009, m1019o*m1019, m2029o*m2029, m3039o*m3039, 
	m4049o*m4049, m5059o*m5059, m6069o*m6069, m70upo*m70up, 
	f0009o*f0009, f1019o*f1019, f2029o*f2029, f3039o*f3039, 
	f4049o*f4049, f5059o*f5059, f6069o*f6069, f70upo*f70up,
	m0009h*m0009, m1019h*m1019, m2029h*m2029, m3039h*m3039, 
	m4049h*m4049, m5059h*m5059, m6069h*m6069, m70uph*m70up, 
	f0009h*f0009, f1019h*f1019, f2029h*f2029, f3039h*f3039, 
	f4049h*f4049, f5059h*f5059, f6069h*f6069, f70uph*f70up
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12cocl) <- c("geoid10", 
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

pct12cocl <- round (pct12cocl, digits=0)

## check that the relative difference in sum of adjusted ASRE cohorts is small compared to sum of independent AS cohorts
diff30 <- sum(colSums(pct12cocl[,-1])) - sum(colSums(co.as[,-1])) 

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12cocl, sum, na.rm=TRUE)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## create ccr for controlled target population / launch population
## remove all 0-9 age cohorts male and female from launch year
l2 <- pct12cocl[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]

b <- read.csv(paste(data.out, "pct1220cocl.csv", sep = ""))[,-1]

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
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

write.csv(pct12cocl, paste(data.out, "pct1230cocl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1230cocl.tfr.csv", sep = ""))
write.csv(ccr, paste(data.out, "pct1230cocl.ccr.csv", sep = ""))




#####################################################################################
############## starting 2040 asre county projections here ############################

## import base and launch years
b <- read.csv(paste(data.out, "pct1220co.csv", sep = ""))[,-1]
l <- pct12cocl

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 age cohorts male and female from launch year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
b2 <- b %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
b2 <- b2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	
## save geoid10 to reattach after ccr
geoid10 <- l2$geoid10

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

## change hispanic ccr rate to 0.5 of 2000 to 2010
ccr2 <- aggregate(cbind( 
	m00wccr, m10wccr, m20wccr, m30wccr, m40wccr, m50wccr, m60wccr, 
	f00wccr, f10wccr, f20wccr, f30wccr, f40wccr, f50wccr, f60wccr, 
	m00bccr, m10bccr, m20bccr, m30bccr, m40bccr, m50bccr, m60bccr, 
	f00bccr, f10bccr, f20bccr, f30bccr, f40bccr, f50bccr, f60bccr,
	m00accr, m10accr, m20accr, m30accr, m40accr, m50accr, m60accr, 
	f00accr, f10accr, f20accr, f30accr, f40accr, f50accr, f60accr,
	m00occr, m10occr, m20occr, m30occr, m40occr, m50occr, m60occr, 
	f00occr, f10occr, f20occr, f30occr, f40occr, f50occr, f60occr,
	m00hccr/2, m10hccr/2, m20hccr/2, m30hccr/2, m40hccr/2, m50hccr/2, m60hccr/2, 
	f00hccr/2, f10hccr/2, f20hccr/2, f30hccr/2, f40hccr/2, f50hccr/2, f60hccr/2
	)~geoid10, data=ccr, sum, na.rm=TRUE)


## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l3 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l4 <- l3[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## Hamilton-Perry Projection from launch year to target year
t.proj <- l4*ccr
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

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=l, sum, na.rm=TRUE)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all Inf to NaN
is.na(l.cwr) <- sapply(l.cwr, is.infinite)

## convert all NaN to 0
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5), target year (6-7), & geography level (last two)
pct12co <- aggregate(cbind((cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), m1019w, 
	m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), f1019w, f2029w, f3039w, f4049w, 
	f5059w, f6069w, f70upw,
	(cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), m1019b, m2029b, m3039b, m4049b, 
	m5059b, m6069b, m70upb, (cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), f1019b, 
	f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), m1019a, m2029a, m3039a, m4049a, 
	m5059a, m6069a, m70upa, (cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), f1019a, 
	f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), m1019o, m2029o, m3039o, m4049o, 
	m5059o, m6069o, m70upo, (cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), f1019o, 
	f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), m1019h, m2029h, m3039h, m4049h, 
	m5059h, m6069h, m70uph, (cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), f1019h, 
	f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.cwrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12co) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
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
pct12co <- round (pct12co, digits=0)

write.csv(pct12co, paste(data.out, "pct1240co.csv", sep = ""))

## write iTFR for later use
tfr <- round (tfr, digits=1)
write.csv(tfr, paste(data.out, "pct1240co.tfr.csv", sep =""))




################################################################################################
############## starting 2040 asre county controlled projections here ############################

## load age/sex/race/ethnicity county target data
co.asre <- pct12co

## load age/sex county target data
co.as <- read.csv(paste(data.out, "p1240co.csv", sep = ""))[,-1]

## rename ID column to match co.asre
names(co.as) <- c("geoid10",
	"m0009", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
	"f0009", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

## sum by all age/sex cohorts in age/sex/race/ethnicity data
m0009 <- rowSums(co.asre[, grep("m0009", names(co.asre))])
m1019 <- rowSums(co.asre[, grep("m1019", names(co.asre))])
m2029 <- rowSums(co.asre[, grep("m2029", names(co.asre))])
m3039 <- rowSums(co.asre[, grep("m3039", names(co.asre))])
m4049 <- rowSums(co.asre[, grep("m4049", names(co.asre))])
m5059 <- rowSums(co.asre[, grep("m5059", names(co.asre))])
m6069 <- rowSums(co.asre[, grep("m6069", names(co.asre))])
m70up <- rowSums(co.asre[, grep("m70up", names(co.asre))])
f0009 <- rowSums(co.asre[, grep("f0009", names(co.asre))])
f1019 <- rowSums(co.asre[, grep("f1019", names(co.asre))])
f2029 <- rowSums(co.asre[, grep("f2029", names(co.asre))])
f3039 <- rowSums(co.asre[, grep("f3039", names(co.asre))])
f4049 <- rowSums(co.asre[, grep("f4049", names(co.asre))])
f5059 <- rowSums(co.asre[, grep("f5059", names(co.asre))])
f6069 <- rowSums(co.asre[, grep("f6069", names(co.asre))])
f70up <- rowSums(co.asre[, grep("f70up", names(co.asre))])

## extract geoid10 to reappend
geoid10 <- co.asre$geoid10

## combine into new data.frame
co.asre.sum <- data.frame(geoid10, m0009, m1019, m2029, m3039, m4049, m5059, m6069, m70up,
	 f0009, f1019, f2029, f3039, f4049, f5059, f6069, f70up)

## calculate adjustment factor for target years
af <- co.as/co.asre.sum ## creates data frame of adjustment factors (i.e. county AG/sum of tracts AG)
af$geoid10 <- geoid10 ## reattaches county IDs to data frame
control <- merge(co.asre, af, by="geoid10") ## merge trct data with adjustment factors by id

pct12cocl <- aggregate(cbind(
	m0009w*m0009, m1019w*m1019, m2029w*m2029, m3039w*m3039, 
	m4049w*m4049, m5059w*m5059, m6069w*m6069, m70upw*m70up, 
	f0009w*f0009, f1019w*f1019, f2029w*f2029, f3039w*f3039, 
	f4049w*f4049, f5059w*f5059, f6069w*f6069, f70upw*f70up,
	m0009b*m0009, m1019b*m1019, m2029b*m2029, m3039b*m3039, 
	m4049b*m4049, m5059b*m5059, m6069b*m6069, m70upb*m70up, 
	f0009b*f0009, f1019b*f1019, f2029b*f2029, f3039b*f3039, 
	f4049b*f4049, f5059b*f5059, f6069b*f6069, f70upb*f70up,
	m0009a*m0009, m1019a*m1019, m2029a*m2029, m3039a*m3039, 
	m4049a*m4049, m5059a*m5059, m6069a*m6069, m70upa*m70up, 
	f0009a*f0009, f1019a*f1019, f2029a*f2029, f3039a*f3039, 
	f4049a*f4049, f5059a*f5059, f6069a*f6069, f70upa*f70up,
	m0009o*m0009, m1019o*m1019, m2029o*m2029, m3039o*m3039, 
	m4049o*m4049, m5059o*m5059, m6069o*m6069, m70upo*m70up, 
	f0009o*f0009, f1019o*f1019, f2029o*f2029, f3039o*f3039, 
	f4049o*f4049, f5059o*f5059, f6069o*f6069, f70upo*f70up,
	m0009h*m0009, m1019h*m1019, m2029h*m2029, m3039h*m3039, 
	m4049h*m4049, m5059h*m5059, m6069h*m6069, m70uph*m70up, 
	f0009h*f0009, f1019h*f1019, f2029h*f2029, f3039h*f3039, 
	f4049h*f4049, f5059h*f5059, f6069h*f6069, f70uph*f70up
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12cocl) <- c("geoid10", 
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

pct12cocl <- round (pct12cocl, digits=0)

## check that the relative difference in sum of adjusted ASRE cohorts is small compared to sum of independent AS cohorts
diff40 <- sum(colSums(pct12cocl[,-1])) - sum(colSums(co.as[,-1])) 

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12cocl, sum, na.rm=TRUE)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## create ccr for controlled target population / launch population
## remove all 0-9 age cohorts male and female from launch year
l2 <- pct12cocl[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]

b <- read.csv(paste(data.out, "pct1230cocl.csv", sep = ""))[,-1]


## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
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


write.csv(pct12cocl, paste(data.out, "pct1240cocl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1240cocl.tfr.csv", sep = ""))
write.csv(ccr, paste(data.out, "pct1240cocl.ccr.csv", sep = ""))




#####################################################################################
############## starting 2050 asre county projections here ############################

## import base and launch years
b <- read.csv(paste(data.out, "pct1230co.csv", sep = ""))[,-1]
l <- pct12cocl

## convert all Inf to NaN
is.na(b) <- sapply(b, is.infinite)
b <- replace(b, is.na(b), 0) ## convert all NaN to 0

## convert all Inf to NaN
is.na(l) <- sapply(l, is.infinite)
l <- replace(l, is.na(l), 0) ## convert all NaN to 0

## remove all 0-9 age cohorts male and female from launch year
l2 <- l[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]
## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
b2 <- b %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
b2 <- b2[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## save geoid10 to reattach after ccr
geoid10 <- l2$geoid10

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

## change hispanic ccr rate to 0.5 of 2000 to 2010
ccr2 <- aggregate(cbind( 
	m00wccr, m10wccr, m20wccr, m30wccr, m40wccr, m50wccr, m60wccr, 
	f00wccr, f10wccr, f20wccr, f30wccr, f40wccr, f50wccr, f60wccr, 
	m00bccr, m10bccr, m20bccr, m30bccr, m40bccr, m50bccr, m60bccr, 
	f00bccr, f10bccr, f20bccr, f30bccr, f40bccr, f50bccr, f60bccr,
	m00accr, m10accr, m20accr, m30accr, m40accr, m50accr, m60accr, 
	f00accr, f10accr, f20accr, f30accr, f40accr, f50accr, f60accr,
	m00occr, m10occr, m20occr, m30occr, m40occr, m50occr, m60occr, 
	f00occr, f10occr, f20occr, f30occr, f40occr, f50occr, f60occr,
	m00hccr/2, m10hccr/2, m20hccr/2, m30hccr/2, m40hccr/2, m50hccr/2, m60hccr/2, 
	f00hccr/2, f10hccr/2, f20hccr/2, f30hccr/2, f40hccr/2, f50hccr/2, f60hccr/2
	)~geoid10, data=ccr, sum, na.rm=TRUE)

## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
l3 <- l %>%
  mutate(m6069a = m6069a+m70upa, m6069b = m6069b+m70upb, m6069h=m6069h+m70uph, m6069o=m6069o+m70upo, m6069w=m6069w+m70upw,
         f6069a = f6069a+f70upa, f6069b = f6069b+f70upb, f6069h=f6069h+f70uph, f6069o=f6069o+f70upo, f6069w=f6069w+f70upw)
l4 <- l3[,c(-9,-17,-25,-33,-41,-49,-57,-65,-73,-81)]	

## Hamilton-Perry Projection from launch year to target year
t.proj <- l4*ccr
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

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=l, sum, na.rm=TRUE)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind(
	(m0009w+f0009w)/((f1019w/2)+f2029w+f3039w+f4049w),
	(m0009b+f0009b)/((f1019b/2)+f2029b+f3039b+f4049b),
	(m0009a+f0009a)/((f1019a/2)+f2029a+f3039a+f4049a),
	((m0009o+f0009o)/((f1019o/2)+f2029o+f3039o+f4049o)),
	(m0009h+f0009h)/((f1019h/2)+f2029h+f3039h+f4049h)
	)~geoid10, data=l, sum, na.rm=TRUE)
names(l.cwr) <- c("geoid10", "cwrw",
	"cwrb", "cwra", "cwro", "cwrh"
	)

## convert all Inf to NaN
is.na(l.cwr) <- sapply(l.cwr, is.infinite)

## convert all NaN to 0
l.cwr <- replace(l.cwr, is.na(l.cwr), 0)

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="geoid10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to 
## table id (first 5), target year (6-7), & geography level (last two)
pct12co <- aggregate(cbind((cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), m1019w, 
	m2029w, m3039w, m4049w, m5059w, m6069w, m70upw, 
	(cwrw/2)*((f1019w/2)+f2029w+f3039w+f4049w), f1019w, f2029w, f3039w, f4049w, 
	f5059w, f6069w, f70upw,
	(cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), m1019b, m2029b, m3039b, m4049b, 
	m5059b, m6069b, m70upb, (cwrb/2)*((f1019b/2)+f2029b+f3039b+f4049b), f1019b, 
	f2029b, f3039b, f4049b, f5059b, f6069b, f70upb,
	(cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), m1019a, m2029a, m3039a, m4049a, 
	m5059a, m6069a, m70upa, (cwra/2)*((f1019a/2)+f2029a+f3039a+f4049a), f1019a, 
	f2029a, f3039a, f4049a, f5059a, f6069a, f70upa,
	(cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), m1019o, m2029o, m3039o, m4049o, 
	m5059o, m6069o, m70upo, (cwro/2)*((f1019o/2)+f2029o+f3039o+f4049o), f1019o, 
	f2029o, f3039o, f4049o, f5059o, f6069o, f70upo,
	(cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), m1019h, m2029h, m3039h, m4049h, 
	m5059h, m6069h, m70uph, (cwrh/2)*((f1019h/2)+f2029h+f3039h+f4049h), f1019h, 
	f2029h, f3039h, f4049h, f5059h, f6069h, f70uph
	)~geoid10, data=t.cwrmerge, sum, na.rm=TRUE, na.action=NULL)
names(pct12co) <- c("geoid10", "m0009w", "m1019w", "m2029w", "m3039w", "m4049w", 
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
pct12co <- round (pct12co, digits=0)

write.csv(pct12co, paste(data.out, "pct1250co.csv", sep = ""))

## write iTFR for later use
tfr <- round (tfr, digits=1)
write.csv(tfr, paste(data.out, "pct1250co.tfr.csv", sep =""))




################################################################################################
############## starting 2050 asre county controlled projections here ############################

## load age/sex/race/ethnicity county target data
co.asre <- pct12co

## load age/sex county target data
co.as <- read.csv(paste(data.out, "p1250co.csv", sep = ""))[,-1]

## rename ID column to match co.asre
names(co.as) <- c("geoid10",
	"m0009", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
	"f0009", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

## sum by all age/sex cohorts in age/sex/race/ethnicity data
m0009 <- rowSums(co.asre[, grep("m0009", names(co.asre))])
m1019 <- rowSums(co.asre[, grep("m1019", names(co.asre))])
m2029 <- rowSums(co.asre[, grep("m2029", names(co.asre))])
m3039 <- rowSums(co.asre[, grep("m3039", names(co.asre))])
m4049 <- rowSums(co.asre[, grep("m4049", names(co.asre))])
m5059 <- rowSums(co.asre[, grep("m5059", names(co.asre))])
m6069 <- rowSums(co.asre[, grep("m6069", names(co.asre))])
m70up <- rowSums(co.asre[, grep("m70up", names(co.asre))])
f0009 <- rowSums(co.asre[, grep("f0009", names(co.asre))])
f1019 <- rowSums(co.asre[, grep("f1019", names(co.asre))])
f2029 <- rowSums(co.asre[, grep("f2029", names(co.asre))])
f3039 <- rowSums(co.asre[, grep("f3039", names(co.asre))])
f4049 <- rowSums(co.asre[, grep("f4049", names(co.asre))])
f5059 <- rowSums(co.asre[, grep("f5059", names(co.asre))])
f6069 <- rowSums(co.asre[, grep("f6069", names(co.asre))])
f70up <- rowSums(co.asre[, grep("f70up", names(co.asre))])

## extract geoid10 to reappend
geoid10 <- co.asre$geoid10

## combine into new data.frame
co.asre.sum <- data.frame(geoid10, m0009, m1019, m2029, m3039, m4049, m5059, m6069, m70up,
	 f0009, f1019, f2029, f3039, f4049, f5059, f6069, f70up)

## calculate adjustment factor for target years
af <- co.as/co.asre.sum ## creates data frame of adjustment factors (i.e. county AG/sum of tracts AG)
af$geoid10 <- geoid10 ## reattaches county IDs to data frame
control <- merge(co.asre, af, by="geoid10") ## merge trct data with adjustment factors by id

pct12cocl <- aggregate(cbind(
	m0009w*m0009, m1019w*m1019, m2029w*m2029, m3039w*m3039, 
	m4049w*m4049, m5059w*m5059, m6069w*m6069, m70upw*m70up, 
	f0009w*f0009, f1019w*f1019, f2029w*f2029, f3039w*f3039, 
	f4049w*f4049, f5059w*f5059, f6069w*f6069, f70upw*f70up,
	m0009b*m0009, m1019b*m1019, m2029b*m2029, m3039b*m3039, 
	m4049b*m4049, m5059b*m5059, m6069b*m6069, m70upb*m70up, 
	f0009b*f0009, f1019b*f1019, f2029b*f2029, f3039b*f3039, 
	f4049b*f4049, f5059b*f5059, f6069b*f6069, f70upb*f70up,
	m0009a*m0009, m1019a*m1019, m2029a*m2029, m3039a*m3039, 
	m4049a*m4049, m5059a*m5059, m6069a*m6069, m70upa*m70up, 
	f0009a*f0009, f1019a*f1019, f2029a*f2029, f3039a*f3039, 
	f4049a*f4049, f5059a*f5059, f6069a*f6069, f70upa*f70up,
	m0009o*m0009, m1019o*m1019, m2029o*m2029, m3039o*m3039, 
	m4049o*m4049, m5059o*m5059, m6069o*m6069, m70upo*m70up, 
	f0009o*f0009, f1019o*f1019, f2029o*f2029, f3039o*f3039, 
	f4049o*f4049, f5059o*f5059, f6069o*f6069, f70upo*f70up,
	m0009h*m0009, m1019h*m1019, m2029h*m2029, m3039h*m3039, 
	m4049h*m4049, m5059h*m5059, m6069h*m6069, m70uph*m70up, 
	f0009h*f0009, f1019h*f1019, f2029h*f2029, f3039h*f3039, 
	f4049h*f4049, f5059h*f5059, f6069h*f6069, f70uph*f70up
	)~geoid10, 	data=control, sum, na.rm=TRUE, na.action=NULL)
names(pct12cocl) <- c("geoid10", 
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

pct12cocl <- round (pct12cocl, digits=0)

## check that the relative difference in sum of adjusted ASRE cohorts is small compared to sum of independent AS cohorts
diff50 <- sum(colSums(pct12cocl[,-1])) - sum(colSums(co.as[,-1])) 

## Hauer et al. (2013) iTFR, EQ(7)
tfr <- aggregate(cbind(35*(((m0009w+f0009w)/10)/((f1019w/2)+f2029w+f3039w+f4049w)),
	35*(((m0009b+f0009b)/10)/((f1019b/2)+f2029b+f3039b+f4049b)),
	35*(((m0009a+f0009a)/10)/((f1019a/2)+f2029a+f3039a+f4049a)),
	35*(((m0009o+f0009o)/10)/((f1019o/2)+f2029o+f3039o+f4049o)),
	35*(((m0009h+f0009h)/10)/((f1019h/2)+f2029h+f3039h+f4049h)))~geoid10,
	data=pct12cocl, sum, na.rm=TRUE, na.action=NULL)
names(tfr) <- c("id10", "tfrw", "tfrb", "tfra", "tfro", "tfrh")

## write iTFR for later use
tfr <- round (tfr, digits=1)

## create ccr for controlled target population / launch population
## remove all 0-9 age cohorts male and female from launch year
l2 <- pct12cocl[,c(-2,-10,-18,-26,-34,-42,-50,-58,-66,-74)]

b <- read.csv(paste(data.out, "pct1240cocl.csv", sep = ""))[,-1]


## add 60-69 and 70up together creating 60up column for base year and removing original 70up column
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

write.csv(pct12cocl, paste(data.out, "pct1250cocl.csv", sep = ""))
write.csv(tfr, paste(data.out, "pct1250cocl.tfr.csv", sep = ""))
write.csv(ccr, paste(data.out, "pct1250cocl.ccr.csv", sep = ""))


#######################################################
## population (Age/Sex) sums by counties 2000 - 2050 
#######################################################

pop <- read.csv(paste(data.out, "pct1250cocl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop50 <- rowSums(pop, na.rm=TRUE)
pop50 <- data.frame(cbind(geoid10, pop50))

pop <- read.csv(paste(data.out, "pct1200co.csv", sep = ""))[,-1]
pop <- pop[pop$geoid10 %in% pop50$geoid10,]
geoid10 <- pop$geoid10
pop <- pop[,-1]
pop00 <- rowSums(pop, na.rm=TRUE)
pop00 <- data.frame(cbind(geoid10, pop00))

pop <- read.csv(paste(data.out, "pct1210co.csv", sep = ""))[,-1]
pop <- pop[pop$geoid10 %in% pop50$geoid10,]
geoid10 <- pop$geoid10
pop <- pop[,-1]
pop10 <- rowSums(pop)
pop10 <- data.frame(cbind(geoid10, pop10))

pop <- read.csv(paste(data.out, "pct1220cocl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop20 <- rowSums(pop, na.rm=TRUE)
pop20 <- data.frame(cbind(geoid10, pop20))

pop <- read.csv(paste(data.out, "pct1230cocl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop30 <- rowSums(pop, na.rm=TRUE)
pop30 <- data.frame(cbind(geoid10, pop30))

pop <- read.csv(paste(data.out, "pct1240cocl.csv", sep = ""))[,-1]
geoid10 <- pop$geoid10
pop <- pop[2:81]
pop40 <- rowSums(pop, na.rm=TRUE)
pop40 <- data.frame(cbind(geoid10, pop40))

pop0010 <- merge(pop00, pop10, by="geoid10")
pop2030 <- merge(pop20, pop30, by="geoid10")
pop4050 <- merge(pop40, pop50, by="geoid10")
pop0030 <- merge(pop0010, pop2030, by="geoid10")
pop0050 <- merge(pop0030, pop4050, by="geoid10")

write.csv(pop0050, paste(data.out, "pct12cocl.sums.csv", sep = ""))

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





