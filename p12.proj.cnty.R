## the code below projects populations using cohort change ratios (CCRs) by age/sex cohorts
## the study area is a 23 county region surrounding coastal Georgia extending into FL and SC
## county scale data from US Censuses 2000 and 2010 p12 tables

rm(list=ls())

## define rate of change (ie scenario) for cohort CCRs
popchg <- 1.0 

data.out <- "tables/projections/outputs/middle/" ## define directory path for outputs dependent on scenario
data.in <- "tables/projections/inputs/" ## define directory path for data inputs



##############################################################################
## import data & limit to shared tracts/rows
p1200.un <- read.csv(paste(data.in, "p1200.10tr.raw.csv", sep = ""))
p1210 <- read.csv(paste(data.in, "p1210tr.raw.csv", sep = ""))
p1200 <- p1200.un[p1200.un$trtid10 %in% p1210$trtid10,] ## 2000 data normalized to 2010 boundaries

## sum raw data to 10-year age cohorts and rename columns accordingly; b=base year, l=launch year
p1200tr <- aggregate(cbind(VD03+VD04, VD05+VD06+VD07, VD08+VD09+VD10+VD11, 
                           VD12+VD13, VD14+VD15, VD16+VD17, VD18+VD19+VD20+VD21, VD22+VD23+VD24+VD25, VD27+VD28, 
                           VD29+VD30+VD31, VD32+VD33+VD34+VD35, VD36+VD37, VD38+VD39, VD40+VD41,
                           VD42+VD43+VD44+VD45, VD46+VD47+VD48+VD49)~trtid10, data=p1200,
                     sum, na.rm=TRUE)
p1200tr <- round (p1200tr, digits=0)
names(p1200tr) <- c("id10", "m09", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                    "f09", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

p1210tr <- aggregate(cbind(D003+D004, D005+D006+D007, D008+D009+D010+D011, 
                           D012+D013, D014+D015, D016+D017, D018+D019+D020+D021, D022+D023+D024+D025, D027+D028, 
                           D029+D030+D031, D032+D033+D034+D035, D036+D037, D038+D039, D040+D041,
                           D042+D043+D044+D045, D046+D047+D048+D049)~trtid10, data=p1210,
                     sum, na.rm=TRUE)
names(p1210tr) <- c("id10", "m09", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                    "f09", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

#################################################################################
## starting 2020 age/sex county projections here 
#################################################################################

## copy base and launch year aggregated by age cohort tract data into new data frames
## create 5-digit county IDs & sum by them to create base and launch year county data frames
p1200co <- p1200tr
b.sub <- substring(p1200co$id10, 1, 5) ## submsaple tract id to 5 digits, being the county id
p1200co$id10 <- as.numeric(b.sub) ## attach new numeric "vector" to original data.frame as column with cntyid10 header
b <- aggregate(. ~ id10, data=p1200co, FUN=sum, na.rm=TRUE) ## sum data.frame by cntyid10

p1210co <- p1210tr
l.sub <- substring(p1210co$id10, 1, 5) ## subsample tract id to 5 digits, being the county id
p1210co$id10 <- as.numeric(l.sub) ## attach new numeric "vector" to original data.frame as column with cntyid10 header
l <- aggregate(. ~ id10, data=p1210co, FUN=sum, na.rm=TRUE) ## sum data.frame by cntyid10

## export both years at county level to use later
write.csv (b, paste(data.out, "p1200co.csv", sep = ""))
write.csv (l, paste(data.out, "p1210co.csv", sep = ""))

## merge data frames into one; .x=launch year and .y equals most recent census in output data.frame
merge <- merge(b, l, by="id10")

## Calculate cohort-change ratio
ccr <- aggregate (cbind(m1019.y/m09.x, m2029.y/m1019.x, m3039.y/m2029.x, m4049.y/m3039.x, 
                        m5059.y/m4049.x, m6069.y/m5059.x, m70up.y/(m70up.x+m6069.x), f1019.y/f09.x, f2029.y/f1019.x, f3039.y/f2029.x, 
                        f4049.y/f3039.x, f5059.y/f4049.x, f6069.y/f5059.x, f70up.y/(f70up.x+f6069.x))~id10, data=merge, sum, 
                  na.rm=TRUE)
names(ccr) <- c("id10", "m00ccr", "m10ccr", "m20ccr", "m30ccr", "m40ccr", "m50ccr", "m60ccr", 
                "f00ccr", "f10ccr", "f20ccr", "f30ccr", "f40ccr", "f50ccr", "f60ccr")

id10 <- ccr$id10 ## save to reattach
ccr <- ccr*popchg ## change for each cohort
ccr$id10 <- id10 ## reattach id

## merge CCR into new data.frame with new projection launch year data
ccrmerge <- merge(ccr, l, by="id10")

## Hamilton-Perry Projection from launch year to target year
t.proj <- aggregate(cbind(m00ccr*m09, m10ccr*m1019, m20ccr*m2029, m30ccr*m3039, m40ccr*m4049, 
                          m50ccr*m5059, m60ccr*(m6069+m70up), f00ccr*f09, f10ccr*f1019, f20ccr*f2029, f30ccr*f3039, f40ccr*f4049, 
                          f50ccr*f5059, f60ccr*(f6069+f70up))~id10, data=ccrmerge, sum, na.rm=TRUE)
names(t.proj) <- c("id10", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                   "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind((m09+f09)/((f1019/2)+f2029+f3039+f4049))~id10, data=l,
                   sum, na.rm=TRUE)
names(l.cwr) <- c("id10", "cwr")

id10 <- l.cwr$id10 ## save to reattach
l.cwr$id10 <- id10 ## reattach id

## Hauer et al. (2013) iTFR, EQ(7)
tfr20 <- aggregate(cbind(35*(((m09+f09)/10)/((f1019/2)+f2029+f3039+f4049)))~id10,
                   data=l, sum, na.rm=TRUE)
names(tfr20) <- c("id10", "tfr")

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="id10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to table id (first 3), 
## target year (characters 4-5), & geography level (last two)
p1220co <- aggregate(cbind((cwr/2)*((f1019/2)+f2029+f3039+f4049), m1019, m2029, m3039, m4049, m5059, 
                           m6069, m70up, (cwr/2)*((f1019/2)+f2029+f3039+f4049), f1019, f2029, f3039, f4049, f5059, f6069, 
                           f70up)~id10, data=t.cwrmerge, sum, na.rm=TRUE)
names(p1220co) <- c("id10", "m09", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                    "f09", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

p1220co <- round (p1220co, digits=0)
write.csv(p1220co, paste(data.out, "p1220co.csv", sep = ""))



#################################################################################
## starting 2030 age/sex county projections here 
#################################################################################

## copy base and launch year aggregated by age cohort tract data into new data frames
## create 5-digit county IDs & sum by them to create base and launch year county data frames
# p1210co <- p1210tr
# b.sub <- substring(p1210co$id10, 1, 5) ## submsaple tract id to 5 digits, being the county id
# p1210co$id10 <- as.numeric(b.sub) ## attach new numeric "vector" to original data.frame as column with cntyid10 header
# b <- aggregate(. ~ id10, data=p1210co, FUN=sum) ## sum data.frame by cntyid10 for base year

## setup new base and launch years
b <- l
l <- p1220co

## merge data frames into one; .x=launch year and .y equals most recent census in output data.frame
merge <- merge(b, l, by="id10")

## Calculate cohort-change ratio between most target and launch years
ccr <- aggregate (cbind(m1019.y/m09.x, m2029.y/m1019.x, m3039.y/m2029.x, m4049.y/m3039.x, 
                        m5059.y/m4049.x, m6069.y/m5059.x, m70up.y/(m70up.x+m6069.x), f1019.y/f09.x, f2029.y/f1019.x, f3039.y/f2029.x, 
                        f4049.y/f3039.x, f5059.y/f4049.x, f6069.y/f5059.x, f70up.y/(f70up.x+f6069.x))~id10, data=merge, sum, 
                  na.rm=TRUE)
names(ccr) <- c("id10", "m00ccr", "m10ccr", "m20ccr", "m30ccr", "m40ccr", "m50ccr", "m60ccr", 
                "f00ccr", "f10ccr", "f20ccr", "f30ccr", "f40ccr", "f50ccr", "f60ccr")

id10 <- ccr$id10 ## save to reattach
ccr <- ccr*popchg ## change for each cohort
ccr$id10 <- id10 ## reattach id

## merge CCR into new data.frame with new projection launch year data
ccrmerge <- merge(ccr, l, by="id10")

## Hamilton-Perry Projection from launch year to target year
t.proj <- aggregate(cbind(m00ccr*m09, m10ccr*m1019, m20ccr*m2029, m30ccr*m3039, m40ccr*m4049, 
                          m50ccr*m5059, m60ccr*(m6069+m70up), f00ccr*f09, f10ccr*f1019, f20ccr*f2029, f30ccr*f3039, f40ccr*f4049, 
                          f50ccr*f5059, f60ccr*(f6069+f70up))~id10, data=ccrmerge, sum, na.rm=TRUE)
names(t.proj) <- c("id10", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                   "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind((m09+f09)/((f1019/2)+f2029+f3039+f4049))~id10, data=l,
                   sum, na.rm=TRUE)
names(l.cwr) <- c("id10", "cwr")

id10 <- l.cwr$id10 ## save to reattach
l.cwr$id10 <- id10 ## reattach id

## Hauer et al. (2013) iTFR, EQ(7)
tfr30 <- aggregate(cbind(35*(((m09+f09)/10)/((f1019/2)+f2029+f3039+f4049)))~id10,
                   data=l, sum, na.rm=TRUE)
names(tfr30) <- c("id10", "tfr")

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="id10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to table id (first 3), 
## target year (characters 4-5), & geography level (last two)
p1230co <- aggregate(cbind((cwr/2)*((f1019/2)+f2029+f3039+f4049), m1019, m2029, m3039, m4049, m5059, 
                           m6069, m70up, (cwr/2)*((f1019/2)+f2029+f3039+f4049), f1019, f2029, f3039, f4049, f5059, f6069, 
                           f70up)~id10, data=t.cwrmerge, sum, na.rm=TRUE)
names(p1230co) <- c("id10", "m09", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                    "f09", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

p1230co <- round (p1230co, digits=0)
write.csv(p1230co, paste(data.out, "p1230co.csv", sep = ""))



#################################################################################
## starting 2040 age/sex county projections here 
#################################################################################

## copy base and launch year aggregated by age cohort tract data into new data frames
## create 5-digit county IDs & sum by them to create base and launch year county data frames
# b.sub <- substring(p1220co$id10, 1, 5) ## submsaple tract id to 5 digits, being the county id
# p1220co$id10 <- as.numeric(b.sub) ## attach new numeric "vector" to original data.frame as column with cntyid10 header
# b <- aggregate(. ~ id10, data=p1220co, FUN=sum) ## sum data.frame by cntyid10 for base year
# 
# l.sub <- substring(p1230co$id10, 1, 5) ## subsample tract id to 5 digits, being the county id
# p1230co$id10 <- as.numeric(l.sub) ## attach new numeric "vector" to original data.frame as column with cntyid10 header
# l <- aggregate(. ~ id10, data=p1230co, FUN=sum, na.rm=TRUE) ## sum data.frame by cntyid10

## setup new base and launch years
b <- l
l <- p1230co

## merge data frames into one; .x=launch year and .y equals most recent census in output data.frame
merge <- merge(b, l, by="id10")

## Calculate cohort-change ratio between most target and launch years
ccr <- aggregate (cbind(m1019.y/m09.x, m2029.y/m1019.x, m3039.y/m2029.x, m4049.y/m3039.x, 
                        m5059.y/m4049.x, m6069.y/m5059.x, m70up.y/(m70up.x+m6069.x), f1019.y/f09.x, f2029.y/f1019.x, f3039.y/f2029.x, 
                        f4049.y/f3039.x, f5059.y/f4049.x, f6069.y/f5059.x, f70up.y/(f70up.x+f6069.x))~id10, data=merge, sum, 
                  na.rm=TRUE)
names(ccr) <- c("id10", "m00ccr", "m10ccr", "m20ccr", "m30ccr", "m40ccr", "m50ccr", "m60ccr", 
                "f00ccr", "f10ccr", "f20ccr", "f30ccr", "f40ccr", "f50ccr", "f60ccr")

id10 <- ccr$id10 ## save to reattach
ccr <- ccr*popchg ## change for each cohort
ccr$id10 <- id10 ## reattach id

## merge CCR into new data.frame with new projection launch year data
ccrmerge <- merge(ccr, l, by="id10")

## Hamilton-Perry Projection from launch year to target year
t.proj <- aggregate(cbind(m00ccr*m09, m10ccr*m1019, m20ccr*m2029, m30ccr*m3039, m40ccr*m4049, 
                          m50ccr*m5059, m60ccr*(m6069+m70up), f00ccr*f09, f10ccr*f1019, f20ccr*f2029, f30ccr*f3039, f40ccr*f4049, 
                          f50ccr*f5059, f60ccr*(f6069+f70up))~id10, data=ccrmerge, sum, na.rm=TRUE)
names(t.proj) <- c("id10", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                   "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind((m09+f09)/((f1019/2)+f2029+f3039+f4049))~id10, data=l,
                   sum, na.rm=TRUE)
names(l.cwr) <- c("id10", "cwr")

id10 <- l.cwr$id10 ## save to reattach
l.cwr$id10 <- id10 ## reattach id

## Hauer et al. (2013) iTFR, EQ(7)
tfr40 <- aggregate(cbind(35*(((m09+f09)/10)/((f1019/2)+f2029+f3039+f4049)))~id10,
                   data=l, sum, na.rm=TRUE)
names(tfr40) <- c("id10", "tfr")

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="id10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to table id (first 3), 
## target year (characters 4-5), & geography level (last two)
p1240co <- aggregate(cbind((cwr/2)*((f1019/2)+f2029+f3039+f4049), m1019, m2029, m3039, m4049, m5059, 
                           m6069, m70up, (cwr/2)*((f1019/2)+f2029+f3039+f4049), f1019, f2029, f3039, f4049, f5059, f6069, 
                           f70up)~id10, data=t.cwrmerge, sum, na.rm=TRUE)
names(p1240co) <- c("id10", "m09", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                    "f09", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

p1240co <- round (p1240co, digits=0)
write.csv(p1240co, paste(data.out, "p1240co.csv", sep = ""))


#################################################################################
## starting 2050 age/sex county projections here 
#################################################################################

## copy base and launch year aggregated by age cohort tract data into new data frames
## create 5-digit county IDs & sum by them to create base and launch year county data frames
# b.sub <- substring(p1230co$id10, 1, 5) ## submsaple tract id to 5 digits, being the county id
# p1230co$id10 <- as.numeric(b.sub) ## attach new numeric "vector" to original data.frame as column with cntyid10 header
# b <- aggregate(. ~ id10, data=p1230co, FUN=sum) ## sum data.frame by cntyid10 for base year
# 
# l.sub <- substring(p1240co$id10, 1, 5) ## subsample tract id to 5 digits, being the county id
# p1240co$id10 <- as.numeric(l.sub) ## attach new numeric "vector" to original data.frame as column with cntyid10 header
# l <- aggregate(. ~ id10, data=p1240co, FUN=sum, na.rm=TRUE) ## sum data.frame by cntyid10

## setup new base and launch years
b <- l
l <- p1240co

## merge data frames into one; .x=launch year and .y equals most recent census in output data.frame
merge <- merge(b, l, by="id10")

## Calculate cohort-change ratio between most target and launch years
ccr <- aggregate (cbind(m1019.y/m09.x, m2029.y/m1019.x, m3039.y/m2029.x, m4049.y/m3039.x, 
                        m5059.y/m4049.x, m6069.y/m5059.x, m70up.y/(m70up.x+m6069.x), f1019.y/f09.x, f2029.y/f1019.x, f3039.y/f2029.x, 
                        f4049.y/f3039.x, f5059.y/f4049.x, f6069.y/f5059.x, f70up.y/(f70up.x+f6069.x))~id10, data=merge, sum, 
                  na.rm=TRUE)
names(ccr) <- c("id10", "m00ccr", "m10ccr", "m20ccr", "m30ccr", "m40ccr", "m50ccr", "m60ccr", 
                "f00ccr", "f10ccr", "f20ccr", "f30ccr", "f40ccr", "f50ccr", "f60ccr")

id10 <- ccr$id10 ## save to reattach
ccr <- ccr*popchg ## change for each cohort
ccr$id10 <- id10 ## reattach id

## merge CCR into new data.frame with new projection launch year data
ccrmerge <- merge(ccr, l, by="id10")

## Hamilton-Perry Projection from launch year to target year
t.proj <- aggregate(cbind(m00ccr*m09, m10ccr*m1019, m20ccr*m2029, m30ccr*m3039, m40ccr*m4049, 
                          m50ccr*m5059, m60ccr*(m6069+m70up), f00ccr*f09, f10ccr*f1019, f20ccr*f2029, f30ccr*f3039, f40ccr*f4049, 
                          f50ccr*f5059, f60ccr*(f6069+f70up))~id10, data=ccrmerge, sum, na.rm=TRUE)
names(t.proj) <- c("id10", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                   "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

# Child-Woman Ratio (CWR) for launch population
l.cwr <- aggregate(cbind((m09+f09)/((f1019/2)+f2029+f3039+f4049))~id10, data=l,
                   sum, na.rm=TRUE)
names(l.cwr) <- c("id10", "cwr")

id10 <- l.cwr$id10 ## save to reattach
l.cwr$id10 <- id10 ## reattach id

## Hauer et al. (2013) iTFR, EQ(7)
tfr50 <- aggregate(cbind(35*(((m09+f09)/10)/((f1019/2)+f2029+f3039+f4049)))~id10,
                   data=l, sum, na.rm=TRUE)
names(tfr50) <- c("id10", "tfr")

## merge HP projected population with CWRs (male + female) for target year 
t.cwrmerge <- merge(t.proj, l.cwr, by="id10")

## calculate male and female population for target year using CWRs
## final projected population for target year with unique file name according to table id (first 3), 
## target year (characters 4-5), & geography level (last two)
p1250co <- aggregate(cbind((cwr/2)*((f1019/2)+f2029+f3039+f4049), m1019, m2029, m3039, m4049, m5059, 
                           m6069, m70up, (cwr/2)*((f1019/2)+f2029+f3039+f4049), f1019, f2029, f3039, f4049, f5059, f6069, 
                           f70up)~id10, data=t.cwrmerge, sum, na.rm=TRUE)
names(p1250co) <- c("id10", "m09", "m1019", "m2029", "m3039", "m4049", "m5059", "m6069", "m70up", 
                    "f09", "f1019", "f2029", "f3039", "f4049", "f5059", "f6069", "f70up")

p1250co <- round (p1250co, digits=0)
write.csv(p1250co, paste(data.out, "p1250co.csv", sep = ""))



#######################################################
## population (Age/Sex) sums by counties 2000 - 2050 
#######################################################

pathname <- file.path(data.out, "p1250co.csv")
pop <- read.csv(pathname)
id10 <- pop$id10
pop <- pop[3:18]
pop50 <- rowSums(pop, na.rm=TRUE)
pop50 <- data.frame(cbind(id10, pop50))

pop <- read.csv(paste(data.out, "p1200co.csv", sep = ""))[,-1]
pop <- pop[pop$id10 %in% pop50$id10,]
id10 <- pop$id10
pop <- pop[,-1]
pop00 <- rowSums(pop, na.rm=TRUE)
pop00 <- data.frame(cbind(id10, pop00))

pop <- read.csv(paste(data.out, "p1210co.csv", sep = ""))[,-1]
pop <- pop[pop$id10 %in% pop50$id10,]
id10 <- pop$id10
pop <- pop[,-1]
pop10 <- rowSums(pop, na.rm=TRUE)
pop10 <- cbind(id10, pop10)

pathname <- file.path(data.out, "p1220co.csv")
pop <- read.csv(pathname)
id10 <- pop$id10
pop <- pop[3:18]
pop20 <- rowSums(pop, na.rm=TRUE)
pop20 <- cbind(id10, pop20)

pathname <- file.path(data.out, "p1230co.csv")
pop <- read.csv(pathname)
id10 <- pop$id10
pop <- pop[3:18]
pop30 <- rowSums(pop, na.rm=TRUE)
pop30 <- cbind(id10, pop30)

pathname <- file.path(data.out, "p1240co.csv")
pop <- read.csv(pathname)
id10 <- pop$id10
pop <- pop[3:18]
pop40 <- rowSums(pop, na.rm=TRUE)
pop40 <- cbind(id10, pop40)

pop0010 <- merge(pop00, pop10, by="id10")
pop2030 <- merge(pop20, pop30, by="id10")
pop4050 <- merge(pop40, pop50, by="id10")
pop0030 <- merge(pop0010, pop2030, by="id10")
pop0050 <- merge(pop0030, pop4050, by="id10")

write.csv(pop0050, paste(data.out, "sums/p12co.sums.csv", sep = ""))

library(ggplot2)
library(dplyr)
library(tidyr)

dat <- pop0050

dat <- dat %>%
  group_by(id10) %>%
  gather(popyear, estimate, pop00:pop50) %>%
  mutate(year = substr(popyear, 4,5)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(estimate = sum(estimate))

ggplot(filter(dat), aes(year, estimate)) +
  geom_point()


#######################################################
## population (Age/Sex) iTFR by counties 2020 - 2050 
#######################################################

tfr2030 <- merge(tfr20, tfr30, by="id10")
tfr4050 <- merge(tfr40, tfr50, by="id10")
tfr2050 <- merge(tfr2030, tfr4050, by="id10")

names(tfr2050) <- c("id10", "tfr20", "tfr30", "tfr40", "tfr50")
tfr2050 <- round (tfr2050, digits=1)
write.csv(tfr2050, paste(data.out, "sums/p12co.tfr.sums.csv", sep = ""))


