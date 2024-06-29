# In this script Walmart stores are plotted geographically. These stores are then mapped against one another
# and against county and municipality centroids. This is used to create a dataset is created that contains
# neighboring units at 15, 25, and 50 mile radii from each Walmart. This is then manipulated to create a treatment
# period of 5 years for each host jurisdiction, allowing multiple comparison groups for spatial analysis.



#######################
####County Data Set####
#######################
county = read.csv('~/Walmart Papers/Data/CountyData.csv')

#open year for all stores
#county$minopen = county$open
county = county %>%
  group_by(FIPS) %>%
  fill(open, .direction = 'downup')

#open timeline for all counties#
county$time = county$year4 - county$open

county$treat = 0
county$treat = ifelse(!is.na(county$open), 1, 0)

county$post = 0
county$post = ifelse(!is.na(county$time) & county$time >= 0, 1, 0)

county$post5yr = 0
county$post5yr = ifelse(!is.na(county$time) & county$time >= 0 & county$time <= 5, 1, 0)
#Re do for min year
county$minopen = ifelse(is.na(county$minopen), 9999, county$minopen)
county$minopen = ave(county$minopen, county$FIPS, FUN = min)
county$minopen = ifelse(county$minopen == 9999, NA, county$minopen)

county$time2 = county$year4 - county$minopen

county$treat2 = 0
county$treat2 = ifelse(!is.na(county$minopen), 1, 0)

county$post2 = 0
county$post2 = ifelse(!is.na(county$time2) & county$time2 >= 0, 1, 0)

county$post5yr2 = 0
county$post5yr2 = ifelse(county$time2 > 0 & county$time2 <=5, 1, county$post5yr2)

county$lat = ifelse(county$FIPS == 2280, 56.8124, county$lat)
county$lon = ifelse(county$FIPS == 2280, -132.9554, county$lon)

county$lat = ifelse(county$FIPS == 46113, 43.2437, county$lat)
county$lon = ifelse(county$FIPS == 46113, -102.6216, county$lon)

county$lat = ifelse(county$FIPS == 46131, 43.3310, county$lat)
county$lon = ifelse(county$FIPS == 46131, -101.4112, county$lon)

county$lat = ifelse(county$FIPS == 51123, 36.7671, county$lat)
county$lon = ifelse(county$FIPS == 51123, -76.5308, county$lon)

########Create Radium for Estimation#####
county$row = ave(county$FIPS, county$FIPS, FUN = seq_along)

cnty = subset(county, row == 1)

radc1 = cnty[, c(661, 660)] ##lon then lat
radc2 = cnty[, c(661, 660)]

library(geosphere)
codm = distm(radc1, radc2)
codm = as.data.frame(codm)
codm = codm/1609.34

#get list of treatment stores to find distance from control stores
cnty$rownum = seq_along(cnty$FIPS)
treatgrp = subset(cnty, treat == 1)
list = treatgrp$rownum
treatgrp2 = subset(cnty, treat2 == 1)
list2 = treatgrp$rownum

#create new matrix with all stores on one axis and treatment stores on another#
codm2 = subset(codm[, c(list)])

#Create Indicators for radium#
cnty$rad25mx = 0
cnty$rad50mx = 0
cnty$rad15mx = 0

for (i in 1:1875) {
  cnty$rad50mx = ifelse(codm2[, i] <=50, 1, cnty$rad50mx)
}

for (i in 1:1875) {
  cnty$rad25mx = ifelse(codm2[, i] <=25, 1, cnty$rad25mx)
}

for (i in 1:1875) {
  cnty$rad15mx = ifelse(codm2[, i] <=15, 1, cnty$rad15mx)
}

#Dummy indicating within radium of treated county for a given year
cnty2 = subset(cnty, !is.na(open))
#For each year of open, get the column number in the codm distance matrix.
#This will give us distance-year control indicator for post estimations
#699 is the rownum variable, which corresponds to the columns in codm
ls64=unlist(c(cnty2[cnty2$open==1964,699]))
ls67=unlist(c(cnty2[cnty2$open==1967,699]))
ls68=unlist(c(cnty2[cnty2$open==1968,699]))
ls69=unlist(c(cnty2[cnty2$open==1969,699]))
ls70=unlist(c(cnty2[cnty2$open==1970,699]))
ls71=unlist(c(cnty2[cnty2$open==1971,699]))
ls72=unlist(c(cnty2[cnty2$open==1972,699]))
ls73=unlist(c(cnty2[cnty2$open==1973,699]))
ls74=unlist(c(cnty2[cnty2$open==1974,699]))
ls75=unlist(c(cnty2[cnty2$open==1975,699]))
ls76=unlist(c(cnty2[cnty2$open==1976,699]))
ls77=unlist(c(cnty2[cnty2$open==1977,699]))
ls78=unlist(c(cnty2[cnty2$open==1978,699]))
ls79=unlist(c(cnty2[cnty2$open==1979,699]))
ls80=unlist(c(cnty2[cnty2$open==1980,699]))
ls81=unlist(c(cnty2[cnty2$open==1981,699]))
ls82=unlist(c(cnty2[cnty2$open==1982,699]))
ls83=unlist(c(cnty2[cnty2$open==1983,699]))
ls84=unlist(c(cnty2[cnty2$open==1984,699]))
ls85=unlist(c(cnty2[cnty2$open==1985,699]))
ls86=unlist(c(cnty2[cnty2$open==1986,699]))
ls87=unlist(c(cnty2[cnty2$open==1987,699]))
ls88=unlist(c(cnty2[cnty2$open==1988,699]))
ls89=unlist(c(cnty2[cnty2$open==1989,699]))
ls90=unlist(c(cnty2[cnty2$open==1990,699]))
ls91=unlist(c(cnty2[cnty2$open==1991,699]))
ls92=unlist(c(cnty2[cnty2$open==1992,699]))
ls93=unlist(c(cnty2[cnty2$open==1993,699]))
ls94=unlist(c(cnty2[cnty2$open==1994,699]))
ls95=unlist(c(cnty2[cnty2$open==1995,699]))
ls96=unlist(c(cnty2[cnty2$open==1996,699]))
ls97=unlist(c(cnty2[cnty2$open==1997,699]))
ls98=unlist(c(cnty2[cnty2$open==1998,699]))
ls99=unlist(c(cnty2[cnty2$open==1999,699]))
ls00=unlist(c(cnty2[cnty2$open==2000,699]))
ls01=unlist(c(cnty2[cnty2$open==2001,699]))
ls02=unlist(c(cnty2[cnty2$open==2002,699]))
ls03=unlist(c(cnty2[cnty2$open==2003,699]))
ls04=unlist(c(cnty2[cnty2$open==2004,699]))
ls05=unlist(c(cnty2[cnty2$open==2005,699]))
ls06=unlist(c(cnty2[cnty2$open==2006,699]))
ls07=unlist(c(cnty2[cnty2$open==2007,699]))
ls08=unlist(c(cnty2[cnty2$open==2008,699]))
ls09=unlist(c(cnty2[cnty2$open==2009,699]))
ls10=unlist(c(cnty2[cnty2$open==2010,699]))
ls11=unlist(c(cnty2[cnty2$open==2011,699]))
ls12=unlist(c(cnty2[cnty2$open==2012,699]))
ls13=unlist(c(cnty2[cnty2$open==2013,699]))
ls14=unlist(c(cnty2[cnty2$open==2014,699]))
ls15=unlist(c(cnty2[cnty2$open==2015,699]))
ls16=unlist(c(cnty2[cnty2$open==2016,699]))
ls17=unlist(c(cnty2[cnty2$open==2017,699]))
ls18=unlist(c(cnty2[cnty2$open==2018,699]))

#For each ls'year', which is a host county, find all other counties within a radium
#skip 1968 and 1969 beause there were no openings
cnty$p64x = 0
for (i in ls64) {
  cnty$p64x = ifelse(codm[, i] <=50, 1, cnty$p64x)
}
cnty$p67x = 0
for (i in ls67) {
  cnty$p67x = ifelse(codm[, i] <=50, 1, cnty$p67x)
}
cnty$p70x = 0
for (i in ls70) {
  cnty$p70x = ifelse(codm[, i] <=50, 1, cnty$p70x)
}
cnty$p71x = 0
for (i in ls71) {
  cnty$p71x = ifelse(codm[, i] <=50, 2, cnty$p71x)
}
cnty$p72x = 0
for (i in ls72) {
  cnty$p72x = ifelse(codm[, i] <=50, 3, cnty$p72x)
}
cnty$p73x = 0
for (i in ls73) {
  cnty$p73x = ifelse(codm[, i] <=50, 4, cnty$p73x)
}
cnty$p74x = 0
for (i in ls74) {
  cnty$p74x = ifelse(codm[, i] <=50, 5, cnty$p74x)
}
cnty$p75x = 0
for (i in ls75) {
  cnty$p75x = ifelse(codm[, i] <=50, 6, cnty$p75x)
}
cnty$p76x = 0
for (i in ls76) {
  cnty$p76x = ifelse(codm[, i] <=50, 7, cnty$p76x)
}
cnty$p77x = 0
for (i in ls77) {
  cnty$p77x = ifelse(codm[, i] <=50, 8, cnty$p77x)
}
cnty$p78x = 0
for (i in ls78) {
  cnty$p78x = ifelse(codm[, i] <=50, 9, cnty$p78x)
}
cnty$p79x = 0
for (i in ls79) {
  cnty$p79x = ifelse(codm[, i] <=50, 10, cnty$p79x)
}
cnty$p80x = 0
for (i in ls80) {
  cnty$p80x = ifelse(codm[, i] <=50, 11, cnty$p80x)
}
cnty$p81x = 0
for (i in ls81) {
  cnty$p81x = ifelse(codm[, i] <=50, 12, cnty$p81x)
}
cnty$p82x = 0
for (i in ls82) {
  cnty$p82x = ifelse(codm[, i] <=50, 13, cnty$p82x)
}
cnty$p83x = 0
for (i in ls83) {
  cnty$p83x = ifelse(codm[, i] <=50, 14, cnty$p83x)
}
cnty$p84x = 0
for (i in ls84) {
  cnty$p84x = ifelse(codm[, i] <=50, 15, cnty$p84x)
}
cnty$p85x = 0
for (i in ls85) {
  cnty$p85x = ifelse(codm[, i] <=50, 16, cnty$p85x)
}
cnty$p86x = 0
for (i in ls86) {
  cnty$p86x = ifelse(codm[, i] <=50, 17, cnty$p86x)
}
cnty$p87x = 0
for (i in ls87) {
  cnty$p87x = ifelse(codm[, i] <=50, 18, cnty$p87x)
}
cnty$p88x = 0
for (i in ls88) {
  cnty$p88x = ifelse(codm[, i] <=50, 19, cnty$p88x)
}
cnty$p89x = 0
for (i in ls89) {
  cnty$p89x = ifelse(codm[, i] <=50, 20, cnty$p89x)
}
cnty$p90x = 0
for (i in ls90) {
  cnty$p90x = ifelse(codm[, i] <=50, 21, cnty$p90x)
}
cnty$p91x = 0
for (i in ls91) {
  cnty$p91x = ifelse(codm[, i] <=50, 22, cnty$p91x)
}
cnty$p92x = 0
for (i in ls92) {
  cnty$p92x = ifelse(codm[, i] <=50, 23, cnty$p92x)
}
cnty$p93x = 0
for (i in ls93) {
  cnty$p93x = ifelse(codm[, i] <=50, 24, cnty$p93x)
}
cnty$p94x = 0
for (i in ls94) {
  cnty$p94x = ifelse(codm[, i] <=50, 25, cnty$p94x)
}
cnty$p95x = 0
for (i in ls95) {
  cnty$p95x = ifelse(codm[, i] <=50, 26, cnty$p95x)
}
cnty$p96x = 0
for (i in ls96) {
  cnty$p96x = ifelse(codm[, i] <=50, 27, cnty$p96x)
}
cnty$p97x = 0
for (i in ls97) {
  cnty$p97x = ifelse(codm[, i] <=50, 28, cnty$p97x)
}
cnty$p98x = 0
for (i in ls98) {
  cnty$p98x = ifelse(codm[, i] <=50, 29, cnty$p98x)
}
cnty$p99x = 0
for (i in ls99) {
  cnty$p99x = ifelse(codm[, i] <=50, 30, cnty$p99x)
}
cnty$p00x = 0
for (i in ls00) {
  cnty$p00x = ifelse(codm[, i] <=50, 31, cnty$p00x)
}
cnty$p01x = 0
for (i in ls01) {
  cnty$p01x = ifelse(codm[, i] <=50, 32, cnty$p01x)
}
cnty$p02x = 0
for (i in ls02) {
  cnty$p02x = ifelse(codm[, i] <=50, 33, cnty$p02x)
}
cnty$p03x = 0
for (i in ls03) {
  cnty$p03x = ifelse(codm[, i] <=50, 34, cnty$p03x)
}
cnty$p04x = 0
for (i in ls04) {
  cnty$p04x = ifelse(codm[, i] <=50, 35, cnty$p04x)
}
cnty$p05x = 0
for (i in ls05) {
  cnty$p05x = ifelse(codm[, i] <=50, 36, cnty$p05x)
}
cnty$p06x = 0
for (i in ls06) {
  cnty$p06x = ifelse(codm[, i] <=50, 37, cnty$p06x)
}
cnty$p07x = 0
for (i in ls07) {
  cnty$p07x = ifelse(codm[, i] <=50, 38, cnty$p07x)
}
cnty$p08x = 0
for (i in ls08) {
  cnty$p08x = ifelse(codm[, i] <=50, 39, cnty$p08x)
}
cnty$p09x = 0
for (i in ls09) {
  cnty$p09x = ifelse(codm[, i] <=50, 40, cnty$p09x)
}
cnty$p10x = 0
for (i in ls10) {
  cnty$p10x = ifelse(codm[, i] <=50, 41, cnty$p10x)
}
cnty$p11x = 0
for (i in ls11) {
  cnty$p11x = ifelse(codm[, i] <=50, 42, cnty$p11x)
}
cnty$p12x = 0
for (i in ls12) {
  cnty$p12x = ifelse(codm[, i] <=50, 43, cnty$p12x)
}
cnty$p13x = 0
for (i in ls13) {
  cnty$p13x = ifelse(codm[, i] <=50, 44, cnty$p13x)
}
cnty$p14x = 0
for (i in ls14) {
  cnty$p14x = ifelse(codm[, i] <=50, 45, cnty$p14x)
}
cnty$p15x = 0
for (i in ls15) {
  cnty$p15x = ifelse(codm[, i] <=50, 46, cnty$p15x)
}
cnty$p16x = 0
for (i in ls16) {
  cnty$p16x = ifelse(codm[, i] <=50, 47, cnty$p16x)
}
cnty$p17x = 0
for (i in ls17) {
  cnty$p17x = ifelse(codm[, i] <=50, 48, cnty$p17x)
}
cnty$p18x = 0
for (i in ls18) {
  cnty$p18x = ifelse(codm[, i] <=50, 49, cnty$p18x)
}
cnty$p19x = 0
for (i in ls19) {
  cnty$p19x = ifelse(codm[, i] <=50, 50, cnty$p19x)
}


###Merge with indicators back to master set
county = left_join(county, cnty[, c(11,3, 699:754)], by = c('FIPS', 'year4'))

###Fill out the dumies for all county-year pairs
county[, 699:754][is.na(county[, 699:754])] = 0

county = county %>%
  group_by(FIPS)%>%
  mutate(p64x = max(p64x),
         p67x = max(p67x),
         p70x = max(p70x),
         p71x = max(p71x),
         p72x = max(p72x),
         p73x = max(p73x),
         p74x = max(p74x),
         p75x = max(p75x),
         p76x = max(p76x),
         p77x = max(p77x),
         p78x = max(p78x),
         p79x = max(p79x),
         p80x = max(p80x),
         p81x = max(p81x),
         p82x = max(p82x),
         p83x = max(p83x),
         p84x = max(p84x),
         p85x = max(p85x),
         p86x = max(p86x),
         p87x = max(p87x),
         p88x = max(p88x),
         p89x = max(p89x),
         p90x = max(p90x),
         p91x = max(p91x),
         p92x = max(p92x),
         p93x = max(p93x),
         p94x = max(p94x),
         p95x = max(p95x),
         p96x = max(p96x),
         p97x = max(p97x),
         p98x = max(p98x),
         p99x = max(p99x),
         p00x = max(p00x),
         p01x = max(p01x),
         p02x = max(p02x),
         p03x = max(p03x),
         p04x = max(p04x),
         p05x = max(p05x),
         p06x = max(p06x),
         p07x = max(p07x),
         p08x = max(p08x),
         p09x = max(p09x),
         p10x = max(p10x),
         p11x = max(p11x),
         p12x = max(p12x),
         p13x = max(p13x),
         p14x = max(p14x),
         p15x = max(p15x),
         p16x = max(p16x),
         p17x = max(p17x),
         p18x = max(p18x),
         p19x = max(p19x))

county[, 699:754][county[, 699:754] > 0] = 1

county$rad15mx = ifelse(is.na(county$rad15mx), 0, county$rad15mx)
county$rad25mx = ifelse(is.na(county$rad25mx), 0, county$rad25mx)
county$rad50mx = ifelse(is.na(county$rad50mx), 0, county$rad50mx)

county = county %>%
  group_by(FIPS)%>%
  mutate(rad15mx = max(rad15mx),
         rad25mx = max(rad25mx),
         rad50mx = max(rad50mx))

########Create Post-year dummies######
county$post50 = 0
county$post25 = 0
county$post15 = 0
county$post50.5 = 0
county$post25.5 = 0
county$post15.5 = 0

##50 Mile Control Group##
county$post50 = ifelse(county$rad50mx==1 & county$p64x==1 & county$year4 >=1964, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p67x==1 & county$year4 >=1967, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p70x==1 & county$year4 >=1970, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p71x==1 & county$year4 >=1971, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p72x==1 & county$year4 >=1972, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p73x==1 & county$year4 >=1973, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p74x==1 & county$year4 >=1974, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p75x==1 & county$year4 >=1975, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p76x==1 & county$year4 >=1976, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p77x==1 & county$year4 >=1977, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p78x==1 & county$year4 >=1978, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p79x==1 & county$year4 >=1979, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p80x==1 & county$year4 >=1980, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p81x==1 & county$year4 >=1981, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p82x==1 & county$year4 >=1982, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p83x==1 & county$year4 >=1983, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p84x==1 & county$year4 >=1984, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p85x==1 & county$year4 >=1985, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p86x==1 & county$year4 >=1986, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p87x==1 & county$year4 >=1987, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p88x==1 & county$year4 >=1988, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p89x==1 & county$year4 >=1989, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p90x==1 & county$year4 >=1990, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p91x==1 & county$year4 >=1991, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p92x==1 & county$year4 >=1992, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p93x==1 & county$year4 >=1993, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p94x==1 & county$year4 >=1994, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p95x==1 & county$year4 >=1995, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p96x==1 & county$year4 >=1996, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p97x==1 & county$year4 >=1997, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p98x==1 & county$year4 >=1998, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p99x==1 & county$year4 >=1999, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p00x==1 & county$year4 >=2000, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p01x==1 & county$year4 >=2001, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p02x==1 & county$year4 >=2002, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p03x==1 & county$year4 >=2003, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p04x==1 & county$year4 >=2004, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p05x==1 & county$year4 >=2005, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p06x==1 & county$year4 >=2006, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p07x==1 & county$year4 >=2007, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p08x==1 & county$year4 >=2008, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p09x==1 & county$year4 >=2009, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p10x==1 & county$year4 >=2010, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p11x==1 & county$year4 >=2011, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p12x==1 & county$year4 >=2012, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p13x==1 & county$year4 >=2013, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p14x==1 & county$year4 >=2014, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p15x==1 & county$year4 >=2015, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p16x==1 & county$year4 >=2016, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p17x==1 & county$year4 >=2017, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p18x==1 & county$year4 >=2018, 1, county$post50)
county$post50 = ifelse(county$rad50mx==1 & county$p19x==1 & county$year4 >=2019, 1, county$post50)

county$post50 = ifelse(county$post==1, 1, county$post50)

##25 Mile Control Group##
county$post25 = ifelse(county$rad25mx==1 & county$p64x==1 & county$year4 >=1964, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p67x==1 & county$year4 >=1967, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p70x==1 & county$year4 >=1970, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p71x==1 & county$year4 >=1971, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p72x==1 & county$year4 >=1972, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p73x==1 & county$year4 >=1973, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p74x==1 & county$year4 >=1974, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p75x==1 & county$year4 >=1975, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p76x==1 & county$year4 >=1976, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p77x==1 & county$year4 >=1977, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p78x==1 & county$year4 >=1978, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p79x==1 & county$year4 >=1979, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p80x==1 & county$year4 >=1980, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p81x==1 & county$year4 >=1981, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p82x==1 & county$year4 >=1982, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p83x==1 & county$year4 >=1983, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p84x==1 & county$year4 >=1984, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p85x==1 & county$year4 >=1985, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p86x==1 & county$year4 >=1986, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p87x==1 & county$year4 >=1987, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p88x==1 & county$year4 >=1988, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p89x==1 & county$year4 >=1989, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p90x==1 & county$year4 >=1990, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p91x==1 & county$year4 >=1991, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p92x==1 & county$year4 >=1992, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p93x==1 & county$year4 >=1993, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p94x==1 & county$year4 >=1994, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p95x==1 & county$year4 >=1995, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p96x==1 & county$year4 >=1996, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p97x==1 & county$year4 >=1997, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p98x==1 & county$year4 >=1998, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p99x==1 & county$year4 >=1999, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p00x==1 & county$year4 >=2000, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p01x==1 & county$year4 >=2001, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p02x==1 & county$year4 >=2002, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p03x==1 & county$year4 >=2003, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p04x==1 & county$year4 >=2004, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p05x==1 & county$year4 >=2005, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p06x==1 & county$year4 >=2006, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p07x==1 & county$year4 >=2007, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p08x==1 & county$year4 >=2008, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p09x==1 & county$year4 >=2009, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p10x==1 & county$year4 >=2010, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p11x==1 & county$year4 >=2011, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p12x==1 & county$year4 >=2012, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p13x==1 & county$year4 >=2013, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p14x==1 & county$year4 >=2014, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p15x==1 & county$year4 >=2015, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p16x==1 & county$year4 >=2016, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p17x==1 & county$year4 >=2017, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p18x==1 & county$year4 >=2018, 1, county$post25)
county$post25 = ifelse(county$rad25mx==1 & county$p19x==1 & county$year4 >=2019, 1, county$post25)

county$post25 = ifelse(county$post==1, 1, county$post25)

##15 Mile Control Group##
county$post15 = ifelse(county$rad15mx==1 & county$p64x==1 & county$year4 >=1964, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p67x==1 & county$year4 >=1967 & county$year4 <=1972, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p70x==1 & county$year4 >=1970 & county$year4 <=1975, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p71x==1 & county$year4 >=1971 & county$year4 <=1976, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p72x==1 & county$year4 >=1972 & county$year4 <=1977, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p73x==1 & county$year4 >=1973 & county$year4 <=1978, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p74x==1 & county$year4 >=1974 & county$year4 <=1979, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p75x==1 & county$year4 >=1975 & county$year4 <=1980, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p76x==1 & county$year4 >=1976 & county$year4 <=1981, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p77x==1 & county$year4 >=1977 & county$year4 <=1982, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p78x==1 & county$year4 >=1978 & county$year4 <=1983, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p79x==1 & county$year4 >=1979 & county$year4 <=1984, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p80x==1 & county$year4 >=1980 & county$year4 <=1985, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p81x==1 & county$year4 >=1981 & county$year4 <=1986, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p82x==1 & county$year4 >=1982 & county$year4 <=1987, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p83x==1 & county$year4 >=1983 & county$year4 <=1988, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p84x==1 & county$year4 >=1984 & county$year4 <=1989, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p85x==1 & county$year4 >=1985 & county$year4 <=1990, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p86x==1 & county$year4 >=1986 & county$year4 <=1991, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p87x==1 & county$year4 >=1987 & county$year4 <=1992, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p88x==1 & county$year4 >=1988 & county$year4 <=1993, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p89x==1 & county$year4 >=1989 & county$year4 <=1994, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p90x==1 & county$year4 >=1990 & county$year4 <=1995, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p91x==1 & county$year4 >=1991 & county$year4 <=1996, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p92x==1 & county$year4 >=1992 & county$year4 <=1997, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p93x==1 & county$year4 >=1993 & county$year4 <=1998, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p94x==1 & county$year4 >=1994 & county$year4 <=1999, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p95x==1 & county$year4 >=1995 & county$year4 <=2000, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p96x==1 & county$year4 >=1996 & county$year4 <=2001, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p97x==1 & county$year4 >=1997 & county$year4 <=2002, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p98x==1 & county$year4 >=1998 & county$year4 <=2003, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p99x==1 & county$year4 >=1999 & county$year4 <=2004, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p00x==1 & county$year4 >=2000 & county$year4 <=2005, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p01x==1 & county$year4 >=2001 & county$year4 <=2006, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p02x==1 & county$year4 >=2002 & county$year4 <=2007, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p03x==1 & county$year4 >=2003 & county$year4 <=2008, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p04x==1 & county$year4 >=2004 & county$year4 <=2009, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p05x==1 & county$year4 >=2005 & county$year4 <=2010, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p06x==1 & county$year4 >=2006 & county$year4 <=2011, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p07x==1 & county$year4 >=2007 & county$year4 <=2012, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p08x==1 & county$year4 >=2008 & county$year4 <=2013, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p09x==1 & county$year4 >=2009 & county$year4 <=2014, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p10x==1 & county$year4 >=2010 & county$year4 <=2015, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p11x==1 & county$year4 >=2011 & county$year4 <=2016, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p12x==1 & county$year4 >=2012 & county$year4 <=2017, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p13x==1 & county$year4 >=2013 & county$year4 <=2018, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p14x==1 & county$year4 >=2014 & county$year4 <=2019, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p15x==1 & county$year4 >=2015 & county$year4 <=2020, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p16x==1 & county$year4 >=2016 & county$year4 <=2021, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p17x==1 & county$year4 >=2017 & county$year4 <=2022, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p18x==1 & county$year4 >=2018, 1, county$post15)
county$post15 = ifelse(county$rad15mx==1 & county$p19x==1 & county$year4 >=2019, 1, county$post15)

county$post15 = ifelse(county$post==1, 1, county$post15)

##Clean post5yr##
county$post5yr = ifelse(is.na(county$post5yr), 0, county$post5yr)
##50.5 Mile Control Group##
county$post50.5 = ifelse(county$rad50mx==1 & county$p64x==1 & county$year4 >=1964 & county$year4 <= 1969, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p67x==1 & county$year4 >=1967 & county$year4 <= 1972, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p70x==1 & county$year4 >=1970 & county$year4 <= 1975, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p71x==1 & county$year4 >=1971 & county$year4 <= 1976, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p72x==1 & county$year4 >=1972 & county$year4 <= 1977, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p73x==1 & county$year4 >=1973 & county$year4 <= 1978, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p74x==1 & county$year4 >=1974 & county$year4 <= 1979, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p75x==1 & county$year4 >=1975 & county$year4 <= 1980, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p76x==1 & county$year4 >=1976 & county$year4 <= 1981, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p77x==1 & county$year4 >=1977 & county$year4 <= 1982, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p78x==1 & county$year4 >=1978 & county$year4 <= 1983, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p79x==1 & county$year4 >=1979 & county$year4 <= 1984, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p80x==1 & county$year4 >=1980 & county$year4 <= 1985, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p81x==1 & county$year4 >=1981 & county$year4 <= 1986, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p82x==1 & county$year4 >=1982 & county$year4 <= 1987, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p83x==1 & county$year4 >=1983 & county$year4 <= 1988, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p84x==1 & county$year4 >=1984 & county$year4 <= 1989, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p85x==1 & county$year4 >=1985 & county$year4 <= 1990, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p86x==1 & county$year4 >=1986 & county$year4 <= 1991, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p87x==1 & county$year4 >=1987 & county$year4 <= 1992, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p88x==1 & county$year4 >=1988 & county$year4 <= 1993, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p89x==1 & county$year4 >=1989 & county$year4 <= 1994, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p90x==1 & county$year4 >=1990 & county$year4 <= 1995, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p91x==1 & county$year4 >=1991 & county$year4 <= 1996, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p92x==1 & county$year4 >=1992 & county$year4 <= 1997, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p93x==1 & county$year4 >=1993 & county$year4 <= 1998, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p94x==1 & county$year4 >=1994 & county$year4 <= 1999, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p95x==1 & county$year4 >=1995 & county$year4 <= 2000, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p96x==1 & county$year4 >=1996 & county$year4 <= 2001, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p97x==1 & county$year4 >=1997 & county$year4 <= 2002, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p98x==1 & county$year4 >=1998 & county$year4 <= 2003, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p99x==1 & county$year4 >=1999 & county$year4 <= 2004, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p00x==1 & county$year4 >=2000 & county$year4 <= 2005, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p01x==1 & county$year4 >=2001 & county$year4 <= 2006, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p02x==1 & county$year4 >=2002 & county$year4 <= 2007, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p03x==1 & county$year4 >=2003 & county$year4 <= 2008, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p04x==1 & county$year4 >=2004 & county$year4 <= 2009, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p05x==1 & county$year4 >=2005 & county$year4 <= 2010, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p06x==1 & county$year4 >=2006 & county$year4 <= 2011, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p07x==1 & county$year4 >=2007 & county$year4 <= 2012, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p08x==1 & county$year4 >=2008 & county$year4 <= 2013, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p09x==1 & county$year4 >=2009 & county$year4 <= 2014, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p10x==1 & county$year4 >=2010 & county$year4 <= 2015, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p11x==1 & county$year4 >=2011 & county$year4 <= 2016, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p12x==1 & county$year4 >=2012 & county$year4 <= 2017, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p13x==1 & county$year4 >=2013 & county$year4 <= 2018, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p14x==1 & county$year4 >=2014 & county$year4 <= 2019, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p15x==1 & county$year4 >=2015 & county$year4 <= 2020, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p16x==1 & county$year4 >=2016 & county$year4 <= 2021, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p17x==1 & county$year4 >=2017 & county$year4 <= 2022, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p18x==1 & county$year4 >=2018 & county$year4 <= 2023, 1, county$post50.5)
county$post50.5 = ifelse(county$rad50mx==1 & county$p19x==1 & county$year4 >=2019 & county$year4 <= 2024, 1, county$post50.5)

county$post50.5 = ifelse(county$post5yr==1, 1, county$post50.5)

##25.5 Mile Control Group##
county$post25.5 = ifelse(county$rad25mx==1 & county$p64x==1 & county$year4 >=1964 & county$year4 <= 1969, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p67x==1 & county$year4 >=1967 & county$year4 <= 1972, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p70x==1 & county$year4 >=1970 & county$year4 <= 1975, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p71x==1 & county$year4 >=1971 & county$year4 <= 1976, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p72x==1 & county$year4 >=1972 & county$year4 <= 1977, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p73x==1 & county$year4 >=1973 & county$year4 <= 1978, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p74x==1 & county$year4 >=1974 & county$year4 <= 1979, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p75x==1 & county$year4 >=1975 & county$year4 <= 1980, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p76x==1 & county$year4 >=1976 & county$year4 <= 1981, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p77x==1 & county$year4 >=1977 & county$year4 <= 1982, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p78x==1 & county$year4 >=1978 & county$year4 <= 1983, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p79x==1 & county$year4 >=1979 & county$year4 <= 1984, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p80x==1 & county$year4 >=1980 & county$year4 <= 1985, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p81x==1 & county$year4 >=1981 & county$year4 <= 1986, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p82x==1 & county$year4 >=1982 & county$year4 <= 1987, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p83x==1 & county$year4 >=1983 & county$year4 <= 1988, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p84x==1 & county$year4 >=1984 & county$year4 <= 1989, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p85x==1 & county$year4 >=1985 & county$year4 <= 1990, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p86x==1 & county$year4 >=1986 & county$year4 <= 1991, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p87x==1 & county$year4 >=1987 & county$year4 <= 1992, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p88x==1 & county$year4 >=1988 & county$year4 <= 1993, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p89x==1 & county$year4 >=1989 & county$year4 <= 1994, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p90x==1 & county$year4 >=1990 & county$year4 <= 1995, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p91x==1 & county$year4 >=1991 & county$year4 <= 1996, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p92x==1 & county$year4 >=1992 & county$year4 <= 1997, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p93x==1 & county$year4 >=1993 & county$year4 <= 1998, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p94x==1 & county$year4 >=1994 & county$year4 <= 1999, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p95x==1 & county$year4 >=1995 & county$year4 <= 2000, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p96x==1 & county$year4 >=1996 & county$year4 <= 2001, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p97x==1 & county$year4 >=1997 & county$year4 <= 2002, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p98x==1 & county$year4 >=1998 & county$year4 <= 2003, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p99x==1 & county$year4 >=1999 & county$year4 <= 2004, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p00x==1 & county$year4 >=2000 & county$year4 <= 2005, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p01x==1 & county$year4 >=2001 & county$year4 <= 2006, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p02x==1 & county$year4 >=2002 & county$year4 <= 2007, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p03x==1 & county$year4 >=2003 & county$year4 <= 2008, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p04x==1 & county$year4 >=2004 & county$year4 <= 2009, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p05x==1 & county$year4 >=2005 & county$year4 <= 2010, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p06x==1 & county$year4 >=2006 & county$year4 <= 2011, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p07x==1 & county$year4 >=2007 & county$year4 <= 2012, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p08x==1 & county$year4 >=2008 & county$year4 <= 2013, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p09x==1 & county$year4 >=2009 & county$year4 <= 2014, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p10x==1 & county$year4 >=2010 & county$year4 <= 2015, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p11x==1 & county$year4 >=2011 & county$year4 <= 2016, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p12x==1 & county$year4 >=2012 & county$year4 <= 2017, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p13x==1 & county$year4 >=2013 & county$year4 <= 2018, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p14x==1 & county$year4 >=2014 & county$year4 <= 2019, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p15x==1 & county$year4 >=2015 & county$year4 <= 2020, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p16x==1 & county$year4 >=2016 & county$year4 <= 2021, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p17x==1 & county$year4 >=2017 & county$year4 <= 2022, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p18x==1 & county$year4 >=2018 & county$year4 <= 2023, 1, county$post25.5)
county$post25.5 = ifelse(county$rad25mx==1 & county$p19x==1 & county$year4 >=2019 & county$year4 <= 2024, 1, county$post25.5)

county$post25.5 = ifelse(county$post5yr==1, 1, county$post25.5)

##15.5 Mile Control Group##
county$post15.5 = ifelse(county$rad15mx==1 & county$p64x==1 & county$year4 >=1964 & county$year4 <= 1969, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p67x==1 & county$year4 >=1967 & county$year4 <= 1972, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p70x==1 & county$year4 >=1970 & county$year4 <= 1975, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p71x==1 & county$year4 >=1971 & county$year4 <= 1976, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p72x==1 & county$year4 >=1972 & county$year4 <= 1977, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p73x==1 & county$year4 >=1973 & county$year4 <= 1978, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p74x==1 & county$year4 >=1974 & county$year4 <= 1979, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p75x==1 & county$year4 >=1975 & county$year4 <= 1980, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p76x==1 & county$year4 >=1976 & county$year4 <= 1981, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p77x==1 & county$year4 >=1977 & county$year4 <= 1982, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p78x==1 & county$year4 >=1978 & county$year4 <= 1983, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p79x==1 & county$year4 >=1979 & county$year4 <= 1984, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p80x==1 & county$year4 >=1980 & county$year4 <= 1985, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p81x==1 & county$year4 >=1981 & county$year4 <= 1986, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p82x==1 & county$year4 >=1982 & county$year4 <= 1987, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p83x==1 & county$year4 >=1983 & county$year4 <= 1988, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p84x==1 & county$year4 >=1984 & county$year4 <= 1989, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p85x==1 & county$year4 >=1985 & county$year4 <= 1990, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p86x==1 & county$year4 >=1986 & county$year4 <= 1991, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p87x==1 & county$year4 >=1987 & county$year4 <= 1992, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p88x==1 & county$year4 >=1988 & county$year4 <= 1993, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p89x==1 & county$year4 >=1989 & county$year4 <= 1994, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p90x==1 & county$year4 >=1990 & county$year4 <= 1995, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p91x==1 & county$year4 >=1991 & county$year4 <= 1996, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p92x==1 & county$year4 >=1992 & county$year4 <= 1997, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p93x==1 & county$year4 >=1993 & county$year4 <= 1998, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p94x==1 & county$year4 >=1994 & county$year4 <= 1999, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p95x==1 & county$year4 >=1995 & county$year4 <= 2000, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p96x==1 & county$year4 >=1996 & county$year4 <= 2001, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p97x==1 & county$year4 >=1997 & county$year4 <= 2002, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p98x==1 & county$year4 >=1998 & county$year4 <= 2003, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p99x==1 & county$year4 >=1999 & county$year4 <= 2004, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p00x==1 & county$year4 >=2000 & county$year4 <= 2005, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p01x==1 & county$year4 >=2001 & county$year4 <= 2006, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p02x==1 & county$year4 >=2002 & county$year4 <= 2007, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p03x==1 & county$year4 >=2003 & county$year4 <= 2008, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p04x==1 & county$year4 >=2004 & county$year4 <= 2009, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p05x==1 & county$year4 >=2005 & county$year4 <= 2010, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p06x==1 & county$year4 >=2006 & county$year4 <= 2011, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p07x==1 & county$year4 >=2007 & county$year4 <= 2012, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p08x==1 & county$year4 >=2008 & county$year4 <= 2013, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p09x==1 & county$year4 >=2009 & county$year4 <= 2014, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p10x==1 & county$year4 >=2010 & county$year4 <= 2015, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p11x==1 & county$year4 >=2011 & county$year4 <= 2016, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p12x==1 & county$year4 >=2012 & county$year4 <= 2017, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p13x==1 & county$year4 >=2013 & county$year4 <= 2018, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p14x==1 & county$year4 >=2014 & county$year4 <= 2019, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p15x==1 & county$year4 >=2015 & county$year4 <= 2020, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p16x==1 & county$year4 >=2016 & county$year4 <= 2021, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p17x==1 & county$year4 >=2017 & county$year4 <= 2022, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p18x==1 & county$year4 >=2018 & county$year4 <= 2023, 1, county$post15.5)
county$post15.5 = ifelse(county$rad15mx==1 & county$p19x==1 & county$year4 >=2019 & county$year4 <= 2024, 1, county$post15.5)

county$post15.5 = ifelse(county$post5yr==1, 1, county$post15.5)

county$urban = 0
county$urban = ifelse(county$Population > 50000, 1, 0)

county = county %>%
  group_by(FIPS) %>%
  mutate(medpop = median(Population))

county$medpop = ifelse(!is.na(county$open), county$Population, county$medpop)
county$pctl = ecdf(county$Population)(county$Population)
county$pctl = county$pctl*100

county$bin = 0
county$bin = ifelse(county$pctl > 0 & county$pctl <= 12.5, 1, county$bin)
county$bin = ifelse(county$pctl > 12.5 & county$pctl <= 25, 2, county$bin)
county$bin = ifelse(county$pctl > 25 & county$pctl <= 37.5, 3, county$bin)
county$bin = ifelse(county$pctl > 37.5 & county$pctl <= 50, 4, county$bin)
county$bin = ifelse(county$pctl > 50 & county$pctl <= 62.5, 5, county$bin)
county$bin = ifelse(county$pctl > 62.5 & county$pctl <= 75, 6, county$bin)
county$bin = ifelse(county$pctl > 75 & county$pctl <= 87.5, 7, county$bin)
county$bin = ifelse(county$pctl > 87.5 & county$pctl <= 100, 8, county$bin)

county$pctl2 = ecdf(county$medpop)(county$medpop)
county$pctl2 = county$pctl2*100

county$bins = 0
county$bins = ifelse(county$pctl2 > 0 & county$pctl2 <= 12.5, 1, county$bins)
county$bins = ifelse(county$pctl2 > 12.5 & county$pctl2 <= 25, 2, county$bins)
county$bins = ifelse(county$pctl2 > 25 & county$pctl2 <= 37.5, 3, county$bins)
county$bins = ifelse(county$pctl2 > 37.5 & county$pctl2 <= 50, 4, county$bins)
county$bins = ifelse(county$pctl2 > 50 & county$pctl2 <= 62.5, 5, county$bins)
county$bins = ifelse(county$pctl2 > 62.5 & county$pctl2 <= 75, 6, county$bins)
county$bins = ifelse(county$pctl2 > 75 & county$pctl2 <= 87.5, 7, county$bins)
county$bins = ifelse(county$pctl2 > 87.5 & county$pctl2 <= 100, 8, county$bins)

county$n_county = ifelse(county$year4 == 1967 & county$open != 1967, 0, county$n_county)
county = county %>%
  group_by(FIPS) %>%
  fill(n_county, .direction = 'downup')
county$n_county = ifelse(is.na(county$n_county), 0, county$n_county)

county = county %>%
  group_by(FIPS) %>%
  fill(wave, .direction = 'downup')
county = county %>%
  group_by(FIPS) %>%
  fill(base, .direction = 'downup')
county = county %>%
  group_by(FIPS) %>%
  fill(departments, .direction = 'downup')
county = county %>%
  group_by(FIPS) %>%
  fill(grocery, .direction = 'downup')
county = county %>%
  group_by(FIPS) %>%
  fill(alcohol, .direction = 'downup')
county = county %>%
  group_by(FIPS) %>%
  fill(special, .direction = 'downup')

county$base = ifelse(is.na(county$base), 0, county$base)
county$departments = ifelse(is.na(county$departments), 0, county$departments)
county$grocery = ifelse(is.na(county$grocery), 0, county$grocery)
county$alcohol = ifelse(is.na(county$alcohol), 0, county$alcohol)
county$special = ifelse(is.na(county$special), 0, county$special)

county$storetype = county$base + county$departments + county$grocery +
  county$alcohol + county$special

county$n_county = ifelse(county$time < 0,0,county$n_county)

write.csv(county, file = '~/Walmart Papers/Data/CountyData.csv', row.names = F)

#######################
#####Muni Data Set#####
#######################
muni = read.csv('~/Walmart Papers/Data/MuniData.csv')

#open year for all stores
muni = muni %>%
  group_by(ID)%>%
  fill(open, .direction = 'downup')

#open timeline for all counties#
muni$time = muni$year4 - muni$open

muni$treat = 0
muni$treat = ifelse(!is.na(muni$open), 1, 0)

muni$post = 0
muni$post = ifelse(!is.na(muni$time) & muni$time >= 0, 1, 0)

muni$post5yr = 0
muni$post5yr = ifelse(muni$time > 0 & muni$time <=5, 1, muni$post5yr)

##get Coordinates for NA cities##
na = muni[, c(1,7:11,599,598)]
na = subset(na, is.na(latitude))
na = unique(na)

state = usa::states
state = state[2:3]
colnames(state)[2] = 'FIPSState'
state$FIPSState = as.numeric(state$FIPSState)

na = left_join(na, state, by = 'FIPSState')
na$adr = paste(na$Name, na$abb, sep = ', ')
na$adr = gsub(" CITY", '', na$adr)
na$adr = gsub(" TOWN", '', na$adr)

library(tidygeocoder)
nageo = geo(address = na$adr, method = 'cascade')
colnames(nageo) = c('adr', 'latitude', 'longitude', 'method')

na = left_join(na[,-c(7,8)], nageo[1:3], by = 'adr')

na2 = subset(na, is.na(latitude))

na2geo = geo(address = na2$adr, method = 'cascade')
na2 = subset(na2, Name != 'NOT AVAILABLE')

###manually find the 36 missing coordinates in na2
write.csv(na2, file = '~/Data Sets/na2.csv', row.names = F)
na2 = read.csv('~/Data Sets/na2.csv')
na2 = na2[2:4]

na = na[!duplicated(na$ID),]
na2 = na2[!duplicated(na2$ID),]

var = colnames(na)
na = left_join(na, na2, by = 'ID')
na$latitude.x = ifelse(is.na(na$latitude.x), na$latitude.y, na$latitude.x)
na$longitude.x = ifelse(is.na(na$longitude.x), na$longitude.y, na$longitude.x)
na = na[1:10]
colnames(na) = var

###Add in coordiantes to muni###
na = na[, c(1,9,10)]
muni = left_join(muni, na, by = "ID")
muni$latitude.x = ifelse(is.na(muni$latitude.x), muni$latitude.y, muni$latitude.x)
muni$longitude.x = ifelse(is.na(muni$longitude.x), muni$longitude.y, muni$longitude.x)
muni$latitude.y = NULL
muni$longitude.y = NULL
colnames(muni)[colnames(muni) == 'latitude.x'] = 'latitude'
colnames(muni)[colnames(muni) == 'longitude.x'] = 'longitude'

######Get Distiances for Muni#######
muni$row = ave(muni$ID, muni$ID, FUN = seq_along)

munloc = subset(muni, row == 1)

radm1 = munloc[, c(599,598)] ##lon then lat
radm2 = munloc[, c(599,598)]

library(geosphere)
mudm = distm(radm1, radm2)
mudm = as.data.frame(mudm)
mudm = mudm/1609.34

#get list of treatment stores to find distance from control stores
munloc$rownum = seq_along(munloc$ID)
treatgrp = subset(munloc, treat == 1)
list = treatgrp$rownum
#create new matrix with all stores on one axis and treatment stores on another#
mudm2 = subset(mudm[, c(list)])

munloc$rad25mx = 0
munloc$rad50mx = 0
munloc$rad15mx = 0

for (i in 1:1887) {
  munloc$rad50mx = ifelse(mudm2[, i] <=50, 1, munloc$rad50mx)
}

for (i in 1:1887) {
  munloc$rad25mx = ifelse(mudm2[, i] <=25, 1, munloc$rad25mx)
}

for (i in 1:1887) {
  munloc$rad15mx = ifelse(mudm2[, i] <=15, 1, munloc$rad15mx)
}

#Dummy indicating within radium of treated county for a given year
munloc2 = subset(munloc, !is.na(open))
#For each year of open, get the column number in the codm distance matrix.
#This will give us distance-year control indicator for post estimations
#669 is the rownum variable, which corresponds to the columns in codm
ls67=c(munloc2[munloc2$open==1967,669])
ls68=c(munloc2[munloc2$open==1968,669])
ls69=c(munloc2[munloc2$open==1969,669])
ls70=c(munloc2[munloc2$open==1970,669])
ls71=c(munloc2[munloc2$open==1971,669])
ls72=c(munloc2[munloc2$open==1972,669])
ls73=c(munloc2[munloc2$open==1973,669])
ls74=c(munloc2[munloc2$open==1974,669])
ls75=c(munloc2[munloc2$open==1975,669])
ls76=c(munloc2[munloc2$open==1976,669])
ls77=c(munloc2[munloc2$open==1977,669])
ls78=c(munloc2[munloc2$open==1978,669])
ls79=c(munloc2[munloc2$open==1979,669])
ls80=c(munloc2[munloc2$open==1980,669])
ls81=c(munloc2[munloc2$open==1981,669])
ls82=c(munloc2[munloc2$open==1982,669])
ls83=c(munloc2[munloc2$open==1983,669])
ls84=c(munloc2[munloc2$open==1984,669])
ls85=c(munloc2[munloc2$open==1985,669])
ls86=c(munloc2[munloc2$open==1986,669])
ls87=c(munloc2[munloc2$open==1987,669])
ls88=c(munloc2[munloc2$open==1988,669])
ls89=c(munloc2[munloc2$open==1989,669])
ls90=c(munloc2[munloc2$open==1990,669])
ls91=c(munloc2[munloc2$open==1991,669])
ls92=c(munloc2[munloc2$open==1992,669])
ls93=c(munloc2[munloc2$open==1993,669])
ls94=c(munloc2[munloc2$open==1994,669])
ls95=c(munloc2[munloc2$open==1995,669])
ls96=c(munloc2[munloc2$open==1996,669])
ls97=c(munloc2[munloc2$open==1997,669])
ls98=c(munloc2[munloc2$open==1998,669])
ls99=c(munloc2[munloc2$open==1999,669])
ls00=c(munloc2[munloc2$open==2000,669])
ls01=c(munloc2[munloc2$open==2001,669])
ls02=c(munloc2[munloc2$open==2002,669])
ls03=c(munloc2[munloc2$open==2003,669])
ls04=c(munloc2[munloc2$open==2004,669])
ls05=c(munloc2[munloc2$open==2005,669])
ls06=c(munloc2[munloc2$open==2006,669])
ls07=c(munloc2[munloc2$open==2007,669])
ls08=c(munloc2[munloc2$open==2008,669])
ls09=c(munloc2[munloc2$open==2009,669])
ls10=c(munloc2[munloc2$open==2010,669])
ls11=c(munloc2[munloc2$open==2011,669])
ls12=c(munloc2[munloc2$open==2012,669])
ls13=c(munloc2[munloc2$open==2013,669])
ls14=c(munloc2[munloc2$open==2014,669])
ls15=c(munloc2[munloc2$open==2015,669])
ls16=c(munloc2[munloc2$open==2016,669])
ls17=c(munloc2[munloc2$open==2017,669])
ls18=c(munloc2[munloc2$open==2018,669])
ls19=c(munloc2[munloc2$open==2019,669])

#For each ls'year', which is a host county, find all other counties within a radium

munloc$p67x = 0
for (i in ls67) {
  munloc$p67x = ifelse(mudm[, i] <=50, 1, munloc$p67x)
}
munloc$p70x = 0
for (i in ls70) {
  munloc$p70x = ifelse(mudm[, i] <=50, 1, munloc$p70x)
}
munloc$p71x = 0
for (i in ls71) {
  munloc$p71x = ifelse(mudm[, i] <=50, 2, munloc$p71x)
}
munloc$p72x = 0
for (i in ls72) {
  munloc$p72x = ifelse(mudm[, i] <=50, 3, munloc$p72x)
}
munloc$p73x = 0
for (i in ls73) {
  munloc$p73x = ifelse(mudm[, i] <=50, 4, munloc$p73x)
}
munloc$p74x = 0
for (i in ls74) {
  munloc$p74x = ifelse(mudm[, i] <=50, 5, munloc$p74x)
}
munloc$p75x = 0
for (i in ls75) {
  munloc$p75x = ifelse(mudm[, i] <=50, 6, munloc$p75x)
}
munloc$p76x = 0
for (i in ls76) {
  munloc$p76x = ifelse(mudm[, i] <=50, 7, munloc$p76x)
}
munloc$p77x = 0
for (i in ls77) {
  munloc$p77x = ifelse(mudm[, i] <=50, 8, munloc$p77x)
}
munloc$p78x = 0
for (i in ls78) {
  munloc$p78x = ifelse(mudm[, i] <=50, 9, munloc$p78x)
}
munloc$p79x = 0
for (i in ls79) {
  munloc$p79x = ifelse(mudm[, i] <=50, 10, munloc$p79x)
}
munloc$p80x = 0
for (i in ls80) {
  munloc$p80x = ifelse(mudm[, i] <=50, 11, munloc$p80x)
}
munloc$p81x = 0
for (i in ls81) {
  munloc$p81x = ifelse(mudm[, i] <=50, 12, munloc$p81x)
}
munloc$p82x = 0
for (i in ls82) {
  munloc$p82x = ifelse(mudm[, i] <=50, 13, munloc$p82x)
}
munloc$p83x = 0
for (i in ls83) {
  munloc$p83x = ifelse(mudm[, i] <=50, 14, munloc$p83x)
}
munloc$p84x = 0
for (i in ls84) {
  munloc$p84x = ifelse(mudm[, i] <=50, 15, munloc$p84x)
}
munloc$p85x = 0
for (i in ls85) {
  munloc$p85x = ifelse(mudm[, i] <=50, 16, munloc$p85x)
}
munloc$p86x = 0
for (i in ls86) {
  munloc$p86x = ifelse(mudm[, i] <=50, 17, munloc$p86x)
}
munloc$p87x = 0
for (i in ls87) {
  munloc$p87x = ifelse(mudm[, i] <=50, 18, munloc$p87x)
}
munloc$p88x = 0
for (i in ls88) {
  munloc$p88x = ifelse(mudm[, i] <=50, 19, munloc$p88x)
}
munloc$p89x = 0
for (i in ls89) {
  munloc$p89x = ifelse(mudm[, i] <=50, 20, munloc$p89x)
}
munloc$p90x = 0
for (i in ls90) {
  munloc$p90x = ifelse(mudm[, i] <=50, 21, munloc$p90x)
}
munloc$p91x = 0
for (i in ls91) {
  munloc$p91x = ifelse(mudm[, i] <=50, 22, munloc$p91x)
}
munloc$p92x = 0
for (i in ls92) {
  munloc$p92x = ifelse(mudm[, i] <=50, 23, munloc$p92x)
}
munloc$p93x = 0
for (i in ls93) {
  munloc$p93x = ifelse(mudm[, i] <=50, 24, munloc$p93x)
}
munloc$p94x = 0
for (i in ls94) {
  munloc$p94x = ifelse(mudm[, i] <=50, 25, munloc$p94x)
}
munloc$p95x = 0
for (i in ls95) {
  munloc$p95x = ifelse(mudm[, i] <=50, 26, munloc$p95x)
}
munloc$p96x = 0
for (i in ls96) {
  munloc$p96x = ifelse(mudm[, i] <=50, 27, munloc$p96x)
}
munloc$p97x = 0
for (i in ls97) {
  munloc$p97x = ifelse(mudm[, i] <=50, 28, munloc$p97x)
}
munloc$p98x = 0
for (i in ls98) {
  munloc$p98x = ifelse(mudm[, i] <=50, 29, munloc$p98x)
}
munloc$p99x = 0
for (i in ls99) {
  munloc$p99x = ifelse(mudm[, i] <=50, 30, munloc$p99x)
}
munloc$p00x = 0
for (i in ls00) {
  munloc$p00x = ifelse(mudm[, i] <=50, 31, munloc$p00x)
}
munloc$p01x = 0
for (i in ls01) {
  munloc$p01x = ifelse(mudm[, i] <=50, 32, munloc$p01x)
}
munloc$p02x = 0
for (i in ls02) {
  munloc$p02x = ifelse(mudm[, i] <=50, 33, munloc$p02x)
}
munloc$p03x = 0
for (i in ls03) {
  munloc$p03x = ifelse(mudm[, i] <=50, 34, munloc$p03x)
}
munloc$p04x = 0
for (i in ls04) {
  munloc$p04x = ifelse(mudm[, i] <=50, 35, munloc$p04x)
}
munloc$p05x = 0
for (i in ls05) {
  munloc$p05x = ifelse(mudm[, i] <=50, 36, munloc$p05x)
}
munloc$p06x = 0
for (i in ls06) {
  munloc$p06x = ifelse(mudm[, i] <=50, 37, munloc$p06x)
}
munloc$p07x = 0
for (i in ls07) {
  munloc$p07x = ifelse(mudm[, i] <=50, 38, munloc$p07x)
}
munloc$p08x = 0
for (i in ls08) {
  munloc$p08x = ifelse(mudm[, i] <=50, 39, munloc$p08x)
}
munloc$p09x = 0
for (i in ls09) {
  munloc$p09x = ifelse(mudm[, i] <=50, 40, munloc$p09x)
}
munloc$p10x = 0
for (i in ls10) {
  munloc$p10x = ifelse(mudm[, i] <=50, 41, munloc$p10x)
}
munloc$p11x = 0
for (i in ls11) {
  munloc$p11x = ifelse(mudm[, i] <=50, 42, munloc$p11x)
}
munloc$p12x = 0
for (i in ls12) {
  munloc$p12x = ifelse(mudm[, i] <=50, 43, munloc$p12x)
}
munloc$p13x = 0
for (i in ls13) {
  munloc$p13x = ifelse(mudm[, i] <=50, 44, munloc$p13x)
}
munloc$p14x = 0
for (i in ls14) {
  munloc$p14x = ifelse(mudm[, i] <=50, 45, munloc$p14x)
}
munloc$p15x = 0
for (i in ls15) {
  munloc$p15x = ifelse(mudm[, i] <=50, 46, munloc$p15x)
}
munloc$p16x = 0
for (i in ls16) {
  munloc$p16x = ifelse(mudm[, i] <=50, 47, munloc$p16x)
}
munloc$p17x = 0
for (i in ls17) {
  munloc$p17x = ifelse(mudm[, i] <=50, 48, munloc$p17x)
}
munloc$p18x = 0
for (i in ls18) {
  munloc$p18x = ifelse(mudm[, i] <=50, 49, munloc$p18x)
}
munloc$p19x = 0
for (i in ls19) {
  munloc$p19x = ifelse(mudm[, i] <=50, 50, munloc$p19x)
}

munloc$p67x = rowSums(munloc$p67x)
munloc$p70x = rowSums(munloc$p70x)
munloc$p71x = rowSums(munloc$p71x)
munloc$p72x = rowSums(munloc$p72x)
munloc$p73x = rowSums(munloc$p73x)
munloc$p74x = rowSums(munloc$p74x)
munloc$p75x = rowSums(munloc$p75x)
munloc$p76x = rowSums(munloc$p76x)
munloc$p77x = rowSums(munloc$p77x)
munloc$p78x = rowSums(munloc$p78x)
munloc$p79x = rowSums(munloc$p79x)
munloc$p80x = rowSums(munloc$p80x)
munloc$p81x = rowSums(munloc$p81x)
munloc$p82x = rowSums(munloc$p82x)
munloc$p83x = rowSums(munloc$p83x)
munloc$p84x = rowSums(munloc$p84x)
munloc$p85x = rowSums(munloc$p85x)
munloc$p86x = rowSums(munloc$p86x)
munloc$p87x = rowSums(munloc$p87x)
munloc$p88x = rowSums(munloc$p88x)
munloc$p89x = rowSums(munloc$p89x)
munloc$p90x = rowSums(munloc$p90x)
munloc$p91x = rowSums(munloc$p91x)
munloc$p92x = rowSums(munloc$p92x)
munloc$p93x = rowSums(munloc$p93x)
munloc$p94x = rowSums(munloc$p94x)
munloc$p95x = rowSums(munloc$p95x)
munloc$p96x = rowSums(munloc$p96x)
munloc$p97x = rowSums(munloc$p97x)
munloc$p98x = rowSums(munloc$p98x)
munloc$p99x = rowSums(munloc$p99x)
munloc$p00x = rowSums(munloc$p00x)
munloc$p01x = rowSums(munloc$p01x)
munloc$p02x = rowSums(munloc$p02x)
munloc$p03x = rowSums(munloc$p03x)
munloc$p04x = rowSums(munloc$p04x)
munloc$p05x = rowSums(munloc$p05x)
munloc$p06x = rowSums(munloc$p06x)
munloc$p07x = rowSums(munloc$p07x)
munloc$p08x = rowSums(munloc$p08x)
munloc$p09x = rowSums(munloc$p09x)
munloc$p10x = rowSums(munloc$p10x)
munloc$p11x = rowSums(munloc$p11x)
munloc$p12x = rowSums(munloc$p12x)
munloc$p13x = rowSums(munloc$p13x)
munloc$p14x = rowSums(munloc$p14x)
munloc$p15x = rowSums(munloc$p15x)
munloc$p16x = rowSums(munloc$p16x)
munloc$p17x = rowSums(munloc$p17x)
munloc$p18x = rowSums(munloc$p18x)
munloc$p19x = rowSums(munloc$p19x)

###Merge with indicators back to master set
muni = left_join(muni, munloc[, c(1,3, 670:723)], by = c('ID', 'year4'))

###Fill out the dumies for all muni-year pairs
muni[, 669:722][is.na(muni[, 669:722])] = 0

muni = muni %>%
  group_by(ID)%>%
  mutate(p67x = max(p67x),
         p70x = max(p70x),
         p71x = max(p71x),
         p72x = max(p72x),
         p73x = max(p73x),
         p74x = max(p74x),
         p75x = max(p75x),
         p76x = max(p76x),
         p77x = max(p77x),
         p78x = max(p78x),
         p79x = max(p79x),
         p80x = max(p80x),
         p81x = max(p81x),
         p82x = max(p82x),
         p83x = max(p83x),
         p84x = max(p84x),
         p85x = max(p85x),
         p86x = max(p86x),
         p87x = max(p87x),
         p88x = max(p88x),
         p89x = max(p89x),
         p90x = max(p90x),
         p91x = max(p91x),
         p92x = max(p92x),
         p93x = max(p93x),
         p94x = max(p94x),
         p95x = max(p95x),
         p96x = max(p96x),
         p97x = max(p97x),
         p98x = max(p98x),
         p99x = max(p99x),
         p00x = max(p00x),
         p01x = max(p01x),
         p02x = max(p02x),
         p03x = max(p03x),
         p04x = max(p04x),
         p05x = max(p05x),
         p06x = max(p06x),
         p07x = max(p07x),
         p08x = max(p08x),
         p09x = max(p09x),
         p10x = max(p10x),
         p11x = max(p11x),
         p12x = max(p12x),
         p13x = max(p13x),
         p14x = max(p14x),
         p15x = max(p15x),
         p16x = max(p16x))

muni[, 669:722][muni[, 669:722] > 0] = 1

muni$rad15mx = ifelse(is.na(muni$rad15mx), 0, muni$rad15mx)
muni$rad25mx = ifelse(is.na(muni$rad25mx), 0, muni$rad25mx)
muni$rad50mx = ifelse(is.na(muni$rad50mx), 0, muni$rad50mx)

muni = muni %>%
  group_by(ID)%>%
  mutate(rad15mx = max(rad15mx),
         rad25mx = max(rad25mx),
         rad50mx = max(rad50mx))

########Create Post-year dummies######
muni$post50 = 0
muni$post25 = 0
muni$post15 = 0
muni$post50.5 = 0
muni$post25.5 = 0
muni$post15.5 = 0

##50 Mile Control Group##
muni$post50 = ifelse(muni$rad50mx==1 & muni$p70x==1 & muni$year4 >=1970, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p71x==1 & muni$year4 >=1971, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p72x==1 & muni$year4 >=1972, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p73x==1 & muni$year4 >=1973, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p74x==1 & muni$year4 >=1974, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p75x==1 & muni$year4 >=1975, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p76x==1 & muni$year4 >=1976, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p77x==1 & muni$year4 >=1977, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p78x==1 & muni$year4 >=1978, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p79x==1 & muni$year4 >=1979, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p80x==1 & muni$year4 >=1980, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p81x==1 & muni$year4 >=1981, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p82x==1 & muni$year4 >=1982, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p83x==1 & muni$year4 >=1983, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p84x==1 & muni$year4 >=1984, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p85x==1 & muni$year4 >=1985, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p86x==1 & muni$year4 >=1986, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p87x==1 & muni$year4 >=1987, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p88x==1 & muni$year4 >=1988, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p89x==1 & muni$year4 >=1989, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p90x==1 & muni$year4 >=1990, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p91x==1 & muni$year4 >=1991, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p92x==1 & muni$year4 >=1992, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p93x==1 & muni$year4 >=1993, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p94x==1 & muni$year4 >=1994, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p95x==1 & muni$year4 >=1995, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p96x==1 & muni$year4 >=1996, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p97x==1 & muni$year4 >=1997, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p98x==1 & muni$year4 >=1998, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p99x==1 & muni$year4 >=1999, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p00x==1 & muni$year4 >=2000, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p01x==1 & muni$year4 >=2001, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p02x==1 & muni$year4 >=2002, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p03x==1 & muni$year4 >=2003, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p04x==1 & muni$year4 >=2004, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p05x==1 & muni$year4 >=2005, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p06x==1 & muni$year4 >=2006, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p07x==1 & muni$year4 >=2007, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p08x==1 & muni$year4 >=2008, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p09x==1 & muni$year4 >=2009, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p10x==1 & muni$year4 >=2010, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p11x==1 & muni$year4 >=2011, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p12x==1 & muni$year4 >=2012, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p13x==1 & muni$year4 >=2013, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p14x==1 & muni$year4 >=2014, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p15x==1 & muni$year4 >=2015, 1, muni$post50)
muni$post50 = ifelse(muni$rad50mx==1 & muni$p16x==1 & muni$year4 >=2016, 1, muni$post50)

muni$post50 = ifelse(muni$post==1, 1, muni$post50)

##25 Mile Control Group##
muni$post25 = ifelse(muni$rad25mx==1 & muni$p70x==1 & muni$year4 >=1970, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p71x==1 & muni$year4 >=1971, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p72x==1 & muni$year4 >=1972, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p73x==1 & muni$year4 >=1973, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p74x==1 & muni$year4 >=1974, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p75x==1 & muni$year4 >=1975, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p76x==1 & muni$year4 >=1976, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p77x==1 & muni$year4 >=1977, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p78x==1 & muni$year4 >=1978, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p79x==1 & muni$year4 >=1979, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p80x==1 & muni$year4 >=1980, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p81x==1 & muni$year4 >=1981, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p82x==1 & muni$year4 >=1982, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p83x==1 & muni$year4 >=1983, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p84x==1 & muni$year4 >=1984, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p85x==1 & muni$year4 >=1985, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p86x==1 & muni$year4 >=1986, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p87x==1 & muni$year4 >=1987, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p88x==1 & muni$year4 >=1988, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p89x==1 & muni$year4 >=1989, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p90x==1 & muni$year4 >=1990, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p91x==1 & muni$year4 >=1991, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p92x==1 & muni$year4 >=1992, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p93x==1 & muni$year4 >=1993, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p94x==1 & muni$year4 >=1994, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p95x==1 & muni$year4 >=1995, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p96x==1 & muni$year4 >=1996, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p97x==1 & muni$year4 >=1997, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p98x==1 & muni$year4 >=1998, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p99x==1 & muni$year4 >=1999, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p00x==1 & muni$year4 >=2000, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p01x==1 & muni$year4 >=2001, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p02x==1 & muni$year4 >=2002, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p03x==1 & muni$year4 >=2003, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p04x==1 & muni$year4 >=2004, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p05x==1 & muni$year4 >=2005, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p06x==1 & muni$year4 >=2006, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p07x==1 & muni$year4 >=2007, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p08x==1 & muni$year4 >=2008, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p09x==1 & muni$year4 >=2009, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p10x==1 & muni$year4 >=2010, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p11x==1 & muni$year4 >=2011, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p12x==1 & muni$year4 >=2012, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p13x==1 & muni$year4 >=2013, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p14x==1 & muni$year4 >=2014, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p15x==1 & muni$year4 >=2015, 1, muni$post25)
muni$post25 = ifelse(muni$rad25mx==1 & muni$p16x==1 & muni$year4 >=2016, 1, muni$post25)

muni$post25 = ifelse(muni$post==1, 1, muni$post25)

##15 Mile Control Group##
muni$post15 = ifelse(muni$rad15mx==1 & muni$p70x==1 & muni$year4 >=1970, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p71x==1 & muni$year4 >=1971, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p72x==1 & muni$year4 >=1972, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p73x==1 & muni$year4 >=1973, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p74x==1 & muni$year4 >=1974, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p75x==1 & muni$year4 >=1975, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p76x==1 & muni$year4 >=1976, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p77x==1 & muni$year4 >=1977, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p78x==1 & muni$year4 >=1978, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p79x==1 & muni$year4 >=1979, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p80x==1 & muni$year4 >=1980, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p81x==1 & muni$year4 >=1981, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p82x==1 & muni$year4 >=1982, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p83x==1 & muni$year4 >=1983, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p84x==1 & muni$year4 >=1984, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p85x==1 & muni$year4 >=1985, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p86x==1 & muni$year4 >=1986, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p87x==1 & muni$year4 >=1987, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p88x==1 & muni$year4 >=1988, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p89x==1 & muni$year4 >=1989, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p90x==1 & muni$year4 >=1990, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p91x==1 & muni$year4 >=1991, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p92x==1 & muni$year4 >=1992, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p93x==1 & muni$year4 >=1993, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p94x==1 & muni$year4 >=1994, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p95x==1 & muni$year4 >=1995, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p96x==1 & muni$year4 >=1996, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p97x==1 & muni$year4 >=1997, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p98x==1 & muni$year4 >=1998, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p99x==1 & muni$year4 >=1999, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p00x==1 & muni$year4 >=2000, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p01x==1 & muni$year4 >=2001, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p02x==1 & muni$year4 >=2002, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p03x==1 & muni$year4 >=2003, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p04x==1 & muni$year4 >=2004, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p05x==1 & muni$year4 >=2005, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p06x==1 & muni$year4 >=2006, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p07x==1 & muni$year4 >=2007, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p08x==1 & muni$year4 >=2008, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p09x==1 & muni$year4 >=2009, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p10x==1 & muni$year4 >=2010, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p11x==1 & muni$year4 >=2011, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p12x==1 & muni$year4 >=2012, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p13x==1 & muni$year4 >=2013, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p14x==1 & muni$year4 >=2014, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p15x==1 & muni$year4 >=2015, 1, muni$post15)
muni$post15 = ifelse(muni$rad15mx==1 & muni$p16x==1 & muni$year4 >=2016, 1, muni$post15)

muni$post15 = ifelse(muni$post==1, 1, muni$post15)

##Clean post5yr##
muni$post5yr = ifelse(is.na(muni$post5yr), 0, muni$post5yr)
##50.5 Mile Control Group##
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p70x==1 & muni$year4 >=1970 & muni$year4 <= 1975, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p71x==1 & muni$year4 >=1971 & muni$year4 <= 1976, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p72x==1 & muni$year4 >=1972 & muni$year4 <= 1977, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p73x==1 & muni$year4 >=1973 & muni$year4 <= 1978, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p74x==1 & muni$year4 >=1974 & muni$year4 <= 1979, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p75x==1 & muni$year4 >=1975 & muni$year4 <= 1980, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p76x==1 & muni$year4 >=1976 & muni$year4 <= 1981, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p77x==1 & muni$year4 >=1977 & muni$year4 <= 1982, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p78x==1 & muni$year4 >=1978 & muni$year4 <= 1983, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p79x==1 & muni$year4 >=1979 & muni$year4 <= 1984, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p80x==1 & muni$year4 >=1980 & muni$year4 <= 1985, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p81x==1 & muni$year4 >=1981 & muni$year4 <= 1986, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p82x==1 & muni$year4 >=1982 & muni$year4 <= 1987, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p83x==1 & muni$year4 >=1983 & muni$year4 <= 1988, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p84x==1 & muni$year4 >=1984 & muni$year4 <= 1989, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p85x==1 & muni$year4 >=1985 & muni$year4 <= 1990, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p86x==1 & muni$year4 >=1986 & muni$year4 <= 1991, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p87x==1 & muni$year4 >=1987 & muni$year4 <= 1992, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p88x==1 & muni$year4 >=1988 & muni$year4 <= 1993, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p89x==1 & muni$year4 >=1989 & muni$year4 <= 1994, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p90x==1 & muni$year4 >=1990 & muni$year4 <= 1995, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p91x==1 & muni$year4 >=1991 & muni$year4 <= 1996, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p92x==1 & muni$year4 >=1992 & muni$year4 <= 1997, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p93x==1 & muni$year4 >=1993 & muni$year4 <= 1998, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p94x==1 & muni$year4 >=1994 & muni$year4 <= 1999, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p95x==1 & muni$year4 >=1995 & muni$year4 <= 2000, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p96x==1 & muni$year4 >=1996 & muni$year4 <= 2001, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p97x==1 & muni$year4 >=1997 & muni$year4 <= 2002, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p98x==1 & muni$year4 >=1998 & muni$year4 <= 2003, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p99x==1 & muni$year4 >=1999 & muni$year4 <= 2004, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p00x==1 & muni$year4 >=2000 & muni$year4 <= 2005, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p01x==1 & muni$year4 >=2001 & muni$year4 <= 2006, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p02x==1 & muni$year4 >=2002 & muni$year4 <= 2007, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p03x==1 & muni$year4 >=2003 & muni$year4 <= 2008, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p04x==1 & muni$year4 >=2004 & muni$year4 <= 2009, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p05x==1 & muni$year4 >=2005 & muni$year4 <= 2010, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p06x==1 & muni$year4 >=2006 & muni$year4 <= 2011, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p07x==1 & muni$year4 >=2007 & muni$year4 <= 2012, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p08x==1 & muni$year4 >=2008 & muni$year4 <= 2013, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p09x==1 & muni$year4 >=2009 & muni$year4 <= 2014, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p10x==1 & muni$year4 >=2010 & muni$year4 <= 2015, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p11x==1 & muni$year4 >=2011 & muni$year4 <= 2016, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p12x==1 & muni$year4 >=2012 & muni$year4 <= 2017, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p13x==1 & muni$year4 >=2013 & muni$year4 <= 2018, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p14x==1 & muni$year4 >=2014 & muni$year4 <= 2019, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p15x==1 & muni$year4 >=2015 & muni$year4 <= 2020, 1, muni$post50.5)
muni$post50.5 = ifelse(muni$rad50mx==1 & muni$p16x==1 & muni$year4 >=2016 & muni$year4 <= 2021, 1, muni$post50.5)

muni$post50.5 = ifelse(muni$post5yr==1, 1, muni$post50.5)

##25.5 Mile Control Group##
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p70x==1 & muni$year4 >=1970 & muni$year4 <= 1975, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p71x==1 & muni$year4 >=1971 & muni$year4 <= 1976, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p72x==1 & muni$year4 >=1972 & muni$year4 <= 1977, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p73x==1 & muni$year4 >=1973 & muni$year4 <= 1978, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p74x==1 & muni$year4 >=1974 & muni$year4 <= 1979, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p75x==1 & muni$year4 >=1975 & muni$year4 <= 1980, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p76x==1 & muni$year4 >=1976 & muni$year4 <= 1981, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p77x==1 & muni$year4 >=1977 & muni$year4 <= 1982, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p78x==1 & muni$year4 >=1978 & muni$year4 <= 1983, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p79x==1 & muni$year4 >=1979 & muni$year4 <= 1984, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p80x==1 & muni$year4 >=1980 & muni$year4 <= 1985, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p81x==1 & muni$year4 >=1981 & muni$year4 <= 1986, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p82x==1 & muni$year4 >=1982 & muni$year4 <= 1987, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p83x==1 & muni$year4 >=1983 & muni$year4 <= 1988, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p84x==1 & muni$year4 >=1984 & muni$year4 <= 1989, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p85x==1 & muni$year4 >=1985 & muni$year4 <= 1990, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p86x==1 & muni$year4 >=1986 & muni$year4 <= 1991, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p87x==1 & muni$year4 >=1987 & muni$year4 <= 1992, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p88x==1 & muni$year4 >=1988 & muni$year4 <= 1993, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p89x==1 & muni$year4 >=1989 & muni$year4 <= 1994, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p90x==1 & muni$year4 >=1990 & muni$year4 <= 1995, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p91x==1 & muni$year4 >=1991 & muni$year4 <= 1996, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p92x==1 & muni$year4 >=1992 & muni$year4 <= 1997, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p93x==1 & muni$year4 >=1993 & muni$year4 <= 1998, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p94x==1 & muni$year4 >=1994 & muni$year4 <= 1999, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p95x==1 & muni$year4 >=1995 & muni$year4 <= 2000, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p96x==1 & muni$year4 >=1996 & muni$year4 <= 2001, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p97x==1 & muni$year4 >=1997 & muni$year4 <= 2002, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p98x==1 & muni$year4 >=1998 & muni$year4 <= 2003, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p99x==1 & muni$year4 >=1999 & muni$year4 <= 2004, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p00x==1 & muni$year4 >=2000 & muni$year4 <= 2005, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p01x==1 & muni$year4 >=2001 & muni$year4 <= 2006, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p02x==1 & muni$year4 >=2002 & muni$year4 <= 2007, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p03x==1 & muni$year4 >=2003 & muni$year4 <= 2008, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p04x==1 & muni$year4 >=2004 & muni$year4 <= 2009, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p05x==1 & muni$year4 >=2005 & muni$year4 <= 2010, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p06x==1 & muni$year4 >=2006 & muni$year4 <= 2011, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p07x==1 & muni$year4 >=2007 & muni$year4 <= 2012, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p08x==1 & muni$year4 >=2008 & muni$year4 <= 2013, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p09x==1 & muni$year4 >=2009 & muni$year4 <= 2014, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p10x==1 & muni$year4 >=2010 & muni$year4 <= 2015, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p11x==1 & muni$year4 >=2011 & muni$year4 <= 2016, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p12x==1 & muni$year4 >=2012 & muni$year4 <= 2017, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p13x==1 & muni$year4 >=2013 & muni$year4 <= 2018, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p14x==1 & muni$year4 >=2014 & muni$year4 <= 2019, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p15x==1 & muni$year4 >=2015 & muni$year4 <= 2020, 1, muni$post25.5)
muni$post25.5 = ifelse(muni$rad25mx==1 & muni$p16x==1 & muni$year4 >=2016 & muni$year4 <= 2021, 1, muni$post25.5)

muni$post25.5 = ifelse(muni$post5yr==1, 1, muni$post25.5)

##15.5 Mile Control Group##
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p70x==1 & muni$year4 >=1970 & muni$year4 <= 1975, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p71x==1 & muni$year4 >=1971 & muni$year4 <= 1976, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p72x==1 & muni$year4 >=1972 & muni$year4 <= 1977, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p73x==1 & muni$year4 >=1973 & muni$year4 <= 1978, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p74x==1 & muni$year4 >=1974 & muni$year4 <= 1979, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p75x==1 & muni$year4 >=1975 & muni$year4 <= 1980, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p76x==1 & muni$year4 >=1976 & muni$year4 <= 1981, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p77x==1 & muni$year4 >=1977 & muni$year4 <= 1982, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p78x==1 & muni$year4 >=1978 & muni$year4 <= 1983, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p79x==1 & muni$year4 >=1979 & muni$year4 <= 1984, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p80x==1 & muni$year4 >=1980 & muni$year4 <= 1985, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p81x==1 & muni$year4 >=1981 & muni$year4 <= 1986, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p82x==1 & muni$year4 >=1982 & muni$year4 <= 1987, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p83x==1 & muni$year4 >=1983 & muni$year4 <= 1988, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p84x==1 & muni$year4 >=1984 & muni$year4 <= 1989, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p85x==1 & muni$year4 >=1985 & muni$year4 <= 1990, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p86x==1 & muni$year4 >=1986 & muni$year4 <= 1991, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p87x==1 & muni$year4 >=1987 & muni$year4 <= 1992, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p88x==1 & muni$year4 >=1988 & muni$year4 <= 1993, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p89x==1 & muni$year4 >=1989 & muni$year4 <= 1994, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p90x==1 & muni$year4 >=1990 & muni$year4 <= 1995, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p91x==1 & muni$year4 >=1991 & muni$year4 <= 1996, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p92x==1 & muni$year4 >=1992 & muni$year4 <= 1997, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p93x==1 & muni$year4 >=1993 & muni$year4 <= 1998, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p94x==1 & muni$year4 >=1994 & muni$year4 <= 1999, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p95x==1 & muni$year4 >=1995 & muni$year4 <= 2000, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p96x==1 & muni$year4 >=1996 & muni$year4 <= 2001, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p97x==1 & muni$year4 >=1997 & muni$year4 <= 2002, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p98x==1 & muni$year4 >=1998 & muni$year4 <= 2003, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p99x==1 & muni$year4 >=1999 & muni$year4 <= 2004, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p00x==1 & muni$year4 >=2000 & muni$year4 <= 2005, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p01x==1 & muni$year4 >=2001 & muni$year4 <= 2006, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p02x==1 & muni$year4 >=2002 & muni$year4 <= 2007, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p03x==1 & muni$year4 >=2003 & muni$year4 <= 2008, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p04x==1 & muni$year4 >=2004 & muni$year4 <= 2009, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p05x==1 & muni$year4 >=2005 & muni$year4 <= 2010, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p06x==1 & muni$year4 >=2006 & muni$year4 <= 2011, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p07x==1 & muni$year4 >=2007 & muni$year4 <= 2012, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p08x==1 & muni$year4 >=2008 & muni$year4 <= 2013, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p09x==1 & muni$year4 >=2009 & muni$year4 <= 2014, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p10x==1 & muni$year4 >=2010 & muni$year4 <= 2015, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p11x==1 & muni$year4 >=2011 & muni$year4 <= 2016, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p12x==1 & muni$year4 >=2012 & muni$year4 <= 2017, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p13x==1 & muni$year4 >=2013 & muni$year4 <= 2018, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p14x==1 & muni$year4 >=2014 & muni$year4 <= 2019, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p15x==1 & muni$year4 >=2015 & muni$year4 <= 2020, 1, muni$post15.5)
muni$post15.5 = ifelse(muni$rad15mx==1 & muni$p16x==1 & muni$year4 >=2016 & muni$year4 <= 2021, 1, muni$post15.5)

muni$post15.5 = ifelse(muni$post5yr==1, 1, muni$post15.5)

####Bins####
muni$urban = 0
muni$urban = ifelse(muni$Population > 50000, 1, 0)

muni = muni %>%
  group_by(ID) %>%
  mutate(medpop = median(Population))

muni$medpop = ifelse(!is.na(muni$open), muni$Population, muni$medpop)
muni$pctl = ecdf(muni$Population)(muni$Population) * 100

muni$bin = 0
muni$bin = ifelse(muni$pctl > 0 & muni$pctl <= 12.5, 1, muni$bin)
muni$bin = ifelse(muni$pctl > 12.5 & muni$pctl <= 25, 2, muni$bin)
muni$bin = ifelse(muni$pctl > 25 & muni$pctl <= 37.5, 3, muni$bin)
muni$bin = ifelse(muni$pctl > 37.5 & muni$pctl <= 50, 4, muni$bin)
muni$bin = ifelse(muni$pctl > 50 & muni$pctl <= 62.5, 5, muni$bin)
muni$bin = ifelse(muni$pctl > 62.5 & muni$pctl <= 75, 6, muni$bin)
muni$bin = ifelse(muni$pctl > 75 & muni$pctl <= 87.5, 7, muni$bin)
muni$bin = ifelse(muni$pctl > 87.5 & muni$pctl <= 100, 8, muni$bin)

muni$pctl2 = ecdf(muni$medpop)(muni$medpop) *100

muni$bins = 0
muni$bins = ifelse(muni$pctl2 > 0 & muni$pctl2 <= 12.5, 1, muni$bins)
muni$bins = ifelse(muni$pctl2 > 12.5 & muni$pctl2 <= 25, 2, muni$bins)
muni$bins = ifelse(muni$pctl2 > 25 & muni$pctl2 <= 37.5, 3, muni$bins)
muni$bins = ifelse(muni$pctl2 > 37.5 & muni$pctl2 <= 50, 4, muni$bins)
muni$bins = ifelse(muni$pctl2 > 50 & muni$pctl2 <= 62.5, 5, muni$bins)
muni$bins = ifelse(muni$pctl2 > 62.5 & muni$pctl2 <= 75, 6, muni$bins)
muni$bins = ifelse(muni$pctl2 > 75 & muni$pctl2 <= 87.5, 7, muni$bins)
muni$bins = ifelse(muni$pctl2 > 87.5 & muni$pctl2 <= 100, 8, muni$bins)


####Fill store attributes to control for them###
county = county %>%
  group_by(FIPS) %>%
  fill(n_county, .direction = 'downup')

muni = muni %>%
  group_by(ID) %>%
  fill(n_zip, .direction = 'downup')
muni = muni %>%
  group_by(ID) %>%
  fill(n_muni, .direction = 'downup')

county = county %>%
  group_by(id) %>%
  fill(grocery, .direction = 'downup')
county = county %>%
  group_by(id) %>%
  fill(departments, .direction = 'downup')
county = county %>%
  group_by(id) %>%
  fill(alcohol, .direction = 'downup')
county = county %>%
  group_by(id) %>%
  fill(special, .direction = 'downup')
county = county %>%
  group_by(id) %>%
  fill(base, .direction = 'downup')
county = county %>%
  group_by(id) %>%
  fill(landarea_mi, .direction = 'downup')
county$base = ifelse(county$time < 0 & !is.na(county$time), 0, county$base)
county$departments = ifelse(county$time < 0 & !is.na(county$time), 0, county$departments)
county$special = ifelse(county$time < 0 & !is.na(county$time), 0, county$special)
county$grocery = ifelse(county$time < 0 & !is.na(county$time), 0, county$grocery)
county$alcohol = ifelse(county$time < 0 & !is.na(county$time), 0, county$alcohol)
county$n_county = ifelse(county$time < 0 & !is.na(county$time),0,county$n_county)

muni = muni %>%
  group_by(ID) %>%
  fill(grocery, .direction = 'downup')
muni = muni %>%
  group_by(ID) %>%
  fill(departments, .direction = 'downup')
muni = muni %>%
  group_by(ID) %>%
  fill(alcohol, .direction = 'downup')
muni = muni %>%
  group_by(ID) %>%
  fill(special, .direction = 'downup')
muni = muni %>%
  group_by(ID) %>%
  fill(base, .direction = 'downup')
muni = muni %>%
  group_by(State_Code) %>%
  fill(wave, .direction = 'downup')

muni[c(608,612:615)][is.na(muni[c(608,612:615)])] = 0
county$storetype = county$base + county$grocery + county$departments + county$alcohol + county$special
muni$storetype = muni$base + muni$grocery + muni$departments + muni$alcohol + muni$special
county = subset(county, orig == 1)
muni = subset(muni, orig == 1)
muni$distj = distHaversine(c(-94.1490057382907,36.3312809690636), muni[c(599,598)])/1609.34
muni$t62 = muni$year4 - 1962
county$distj = distHaversine(c(-94.1490057382907,36.3312809690636), county[c(661,660)])/1609.34
county$t62 = county$year4 - 1962


write.csv(county, file = '~/Walmart Papers/Data/CountyData x.csv', row.names = F)
write.csv(muni, file = '~/Walmart Papers/Data/MuniData x.csv', row.names = F)