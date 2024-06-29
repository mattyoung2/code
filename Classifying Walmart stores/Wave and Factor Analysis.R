# Walmart stores are classified into one of three main groups: Supermarkets, Base Stores, and Markets
# The Purpose of this analysis is to cluster store components to assess if variation between stores can
# be used to assess treatement effects of Walmart on host communities. 
# 
# The primary output is contained within the file folder.

stores = read.csv("research/Data Sets/walmart_04-07-20.csv")
library(tidyverse)

stores$open = str_sub(stores$open_date, -9, -5)

###Classify Types of Walmart Services###
stores$hourphoto = ifelse(grepl('1-Hour Photo', stores$store_services), 1,0)
stores$auto = ifelse(grepl('Auto Care Center', stores$store_services), 1,0)
stores$bake = ifelse(grepl('Bakery', stores$store_services), 1,0)
stores$deli = ifelse(grepl('Deli', stores$store_services), 1,0)
stores$garden = ifelse(grepl('Garden Center', stores$store_services), 1,0)
stores$merch = ifelse(grepl('General Retail', stores$store_services), 1,0)
stores$grocery = ifelse(grepl('Grocery', stores$store_services), 1,0)
stores$photo = ifelse(grepl('Photo Center', stores$store_services), 1,0)
stores$cell = ifelse(grepl('Wireless Services', stores$store_services), 1,0)
stores$beer = ifelse(grepl('Beer', stores$store_services), 1,0)
stores$wine = ifelse(grepl("Heritage Wine Cellars|Twisted Vine Winery|Starr Hill Winery|Wine", stores$store_services), 1,0)
stores$liquor = ifelse(grepl('liquor|Brown Jug Liquor', stores$store_services), 1,0)
stores$spirits = ifelse(grepl('Spirits', stores$store_services), 1,0)
stores$gas = ifelse(grepl('Walmart Fuel Station', stores$store_services), 1, stores$restaurant)
stores$pickup = ifelse(grepl('Grocery Pickup|Grocery Pickup and Delivery|PickUp Discount|Pickup Today|PickUp Together', stores$store_services), 1, stores$restaurant)
stores$bank = ifelse(grepl("Money Center|Academy Bank|America First Credit Union|Arvest Bank|American Heritage Bank|American National Bank|AllSouth Federal Credit Union|Axiom Bank|1ST SOUTHWEST BANK OF ALAMOSA|1st Summit Bank|Anchor Savings Bank|American Bank Of The North|America's Credit Union|American Savings Bank|Asteria Credit Union|Andrews Credit Union|BancFirst|Bank Of Commerce|Bank Of Kirksville|Baraboo National Bank|Hawaiian Tel Federal Credit Union|Bank Of Elk River|CorePlus Federal Credit Union|First Community Credit Union|City National Bank|First Convenience Bank|Branch Banking & Trust Company|Central Credit Union|Century Bank|Bristol County Savings Bank|Ft. Knox FCU|Denali Alaskan Fedral Credit Union|Fort Sill National Bank|Central National Bank|Central Bank Of The Lake Of The Ozarks|Chase Bank|Citizens Union Bank|Central Bank Of The Ozark|Community Bank|Central Bank of Branson|Chaco Credit Union|Central Bank|Citizens Bank Of Ada|Community First Bank & Trust|Citizens National Bank|Citizens Community Federal Bank|Columbia County Farmers Bank|First Citizen's Bank|Country Bank For Savings|Citizens State Bank|First National Bank & Trust Co|Citizens Bank|First National Bank Of Gilmer|Cornerstone Bank|International Bank of Commerce|Compass Bank|Citizens National Bank at Brow|Farmers & Merchants Savings|Cumberland Valley National Bank|Community Bank & Trust|Community 1st Credit Union|Desert Schools Credit Union|Community Trust Bank|First Choice Financial Federal Credit Union|Erie Federal Credit Union|Cyprus Federal Credit Union|First State Bank Southwest|Dade County Federal Credit Union|First State Bank - Monett|First National Bank Okmulgee|First National Bank Of Waterloo|First Texas Bank|First Financial Bank|FortKnox Federal CU|First State Bank|First United Bank and Trust Co.|First Independent Bank|Dutrac Community Credit Union|Five County Credit Union|Farmers & Mechanics Bank|Navy Federal CU|Marine Federal Credit Union|L & N Federal Credit Union|First State Commercial Bank|First Midwest Bank|Great Plains National Bank|Farmers Bank & Trust Company|FIRST FINANCIAL BANKSHARES|First Community Bank|First Security Bank|Fortera Federal Credit Union|German American Bancorp. Inc.|First Fidelity Bank|Herring Bank|JUNIATA BANK|Marquette Savings Bank|Miners & Merchants Bank|Granite Federal Credit Union|Median Savings and Loan Association|Guaranty Savings Bank|Hawthorn Bank|Macon Bank & Trust Company|Manitowoc Community CU|Lafayette Bank and Trust|Mid First Bank|Gulf Coast Bank|Kleberg First National Bank|HNB Bank|Mercantile Trust & Savings Bank|NBT Bank|Greater Nevada CU|Jersey Shore State Bank|MIDWEST BANK|Woodforest Bank|Landmark Credit Union|Ohio Valley Bank|Peoples Bank and Trust Company|Members Alliance Credit|Peoples National Bank|Peoples Federal Savings & Loan|Service Credit Union|ORNL Federal Credit Union|Pantex FCU|US Alliance FCU|People's Bank & Trust|Nutmeg State Financial Credit Union|US BANK|SAC Federal Credit Union|Reliant Credit Union|Numerica Credit Union|Security State Bank|PINNACLE BANK|Union First Market Bank|Texas Partners FCU|Second National Bank|Somerset Trust Bank|United Federal Credit Union|SUNFLOWER BANK|Space Coast Credit Union|Potlatch No 1 Federal C/U|The Savings Institute|RCB Bank|Wells Fargo|United National Bank|South State Bank|The Community State Bank|Wesbanco Bank|TruNorth Federal Credit Union|Rockland Federal Credit Union|SunTrust Bank|Rabobank|State Bank of Lincoln|Winnsboro Bank and Trust Co|United Bank|SUMMIT COMMUNITY BANK|Selco Credit Union|Valley 1st Community Premier Bank|Workers' Credit Union|THE BANK NATIONAL ASSOCIATION|Wood & Huston Bank|Southside Bank|Trius FCU|The Heritage Bank|Wilson Bank and Trust" ,stores$store_services),1, 0)
stores$salon = ifelse(grepl("American Nails|Angel Nail|Da-Vi Nails|COBE Nails|Cleopatra Beauty|Regal Nails|Big Airbrush|Beauty Touch|Claire's|Brow Arc|CustomInk|GROWN|La Biotique|Hair Zone|SmartStyle Hair Salon by Regis|Hair Works|Gunslingers Airbrushing|Nail Port|Sharkey's Hair It Is|Rose Nails|Nails II|Miracle Eyebrows|Prestige Nails|Star Cut Salon|Nick Nails|Sport Clips|Walmart Family Salon|Mountain Aire Air Brush|Smooth N Groove|The Barbers - Hairstyling For Me|US Nails|Shubh Beauty" ,stores$store_services),1, 0)
stores$techrepair = ifelse(grepl("Staymobile|iFixandRepair|My Tech Electronic Repair|iRepair|Cellairis|DrPhoneFix|Fast-Fix|Geek iT" ,stores$store_services),1, 0)
stores$eqip.rent = ifelse(grepl("Budget Truck Rental|Pickup, Inc. KY|Budget Car Rental|Thrifty Car Rental" ,stores$store_services),1, 0)
stores$healthcare = ifelse(grepl("Bellin FastCare Baptist Medical Center|Adena Health System|Baxter Regional Medical Center|Baptist Health Care|Baptist Healthcare System|Baptist Health Systems|Vision Center|Carson-Tahoe Hospital|Mamava Lactation Suite|Dubois Regional Medical Center|Cox Health System|Family Medicine Specialists|Family Medical Clinic|Connect Hearing|MedXM|Fairmont Hospital|InMed Clinical Services|National Hearing Center|Harrison Memorial Hospital|Heritage Valley Health System|Jackson General Hospital|Missouri Southern Healthcare|Hannibal Regional Healthcare System|Memorial Hospital Gulfport|Quest Diagnostics|Holzer Clinic|Medical Clinic|Medical Center Odessa|Smiley Dental|National Vision|Stillwater Medical Group|Rutherford Hospital|Pikeville Medical Center|Union General Hospital|Walmart Care Clinic|Value Dental|Southeastern Regional Medical Center|Pyramid Home Health Services|SJ Medical & Dr. Koka" ,stores$store_services),1, 0)
stores$home.servs = ifelse(grepl("Alenco|Appleby Systems|Bath Planet|Improveit|Jet.com Mattress Showroom|Rhode Island Home Improvement|Southern Industries" ,stores$store_services),1, 0)
stores$insur = ifelse(grepl("AIG Hawaii Insurance Co|K & K Group|Texas St. Low Cost Ins|SmartLife Insurance|ABC Insurance & Tags|Maryland Express Tag & Title|Fred Loya Insurance|Compass Insurance|Insurance4Less" ,stores$store_services),1, 0)
stores$money.serv = ifelse(grepl("Bill Payment|Coinstar|Check Cashing|EcoATM|Walmart MoneyCard" ,stores$store_services),1, 0)
stores$pharm = ifelse(grepl("Pharmacy|Pharmacy in-store transfer|Pharmacy Open 24 Hours|Pharmacy Drive-Thru|Prairie Pharmacy|Xavier University College of Pharmacy" ,stores$store_services),1, 0)
stores$photo = ifelse(grepl("Customer Digital Imaging|Picture That|Portraits in Minutes|Le Print Express|Photos Unlimited|Portrait Innovations|PortraitEFX|Portraits International|Photo Center" ,stores$store_services),1, 0)
stores$restaurant = ifelse(grepl("Burger King|Auntie Anne's|Auntie Anne's & Rita's|A Slice of Italy|Ben's Pretzels|Subway|Chobani|Dunkin Brands|Blimpie|Mini-melts|CDS Food Court|Churromania|McDonald's|Checkers & Auntie Anne's|Checkers|Coffee Bay|Dairy Queen Treat Center|Checkers & Rita's|Famous Wok|L&L Drive In|Mis Amigos|Papa John's|Subway & Auntie Anne's|PS Dogs|Hokulia Shaved Ice|Lift Off Coffee|Little Caesars|Marco's Pizza|Little Caesars & Nathan's|Little Caesars & Auntie Anne's|Wayback Burgers|Papa Murphy's Pizza|Noble Roman Pizza|Noble Romans & Auntie Anne's & Rita's|Stewart's|Subway & Mama Deluca Pizza|Philly CT & Auntie Anne's|Uncle Remus|Substation II|Pretzels-Plus|Pollo Campero|Taco Bell|Schlotzsky's|Popcorn Jungle|Remy Capillus|Sergio's|Tree City Juice|Sbarro|Wetzel's Pretzels|We're Rolling Pretzels|Seattleâ€™s Best Coffee|Rally's|Wendy's|Yogurtland|American Deli|Philly Pretzel|Judy's|Java Hut" ,stores$store_services),1, 0)
stores$shipping = ifelse(grepl("FedEx Office|GP Postal|Expedia Cruise Ship Centers|Postal Connections" ,stores$store_services),1, 0)


###What constitutes a money center?###
stores$amex = ifelse(grepl('Bluebird', stores$store_services), 1,0)
stores$check = ifelse(grepl('Check Cashing', stores$store_services), 1,0)
stores$taxes = ifelse(grepl('Tax', stores$store_services), 1,0)
stores$moneycenter = ifelse(grepl('Money Center', stores$store_services), 1,0)
stores$moneyorder = ifelse(grepl('Money Order', stores$store_services), 1,0)
stores$moneytrans = ifelse(grepl('Money Transfer', stores$store_services), 1,0)
##Money Center exists in 17% of stores, all others are >63%. All others excluding Jackson
##Hewitt are >80%. Therefore, "moneycenter" appears to mean banking services.##
stores$banking = ifelse(grepl('Money Center', stores$store_services), 1, 0)
stores$banking = ifelse(grepl('bank', stores$store_services), 1, stores$banking)
stores$banking = ifelse(grepl('BANK', stores$store_services), 1, stores$banking)


###What Corelates with store_type?##
stores$super = ifelse(grepl('Supercenter', stores$store_type), 1, 0)
stores$store = ifelse(grepl('Store', stores$store_type), 1, 0)
stores$gas = ifelse(grepl('Gas Station', stores$store_type), 1, 0)
stores$market = ifelse(grepl('Neighborhood Market', stores$store_type), 1, 0)
stores$pharmacy = ifelse(grepl('Pharmacy', stores$store_type), 1, 0)
stores$amigo = ifelse(grepl('Amigo', stores$store_type), 1, 0)
stores$PU = ifelse(grepl('Pickup only', stores$store_type), 1, 0)


super = subset(stores, store_type == 'Supercenter')
store = subset(stores, store_type == 'Store')
gas = subset(stores, store_type == 'Gas Station')
market = subset(stores, store_type == 'Neighborhood Market')
pharm = subset(stores, store_type == 'Pharmacy')
amigo = subset(stores, store_type == 'Amigo')
pu = subset(stores, store_type == 'Pickup only')

stats = data.frame(col = colnames(super[24:54]), super = sapply(super[24:54], mean),
                   store = sapply(store[24:54], mean),
                   market = sapply(market[24:54], mean),
                   amigo = sapply(amigo[24:54], mean),
                   pu = sapply(pu[24:54], mean),
                   pharm = sapply(pharm[24:54], mean),
                   gas = sapply(gas[24:54], mean))

cor(stats$super, stats$store)
cor(stats$super, stats$market)
cor(stats$market, stats$store)
cor(stats$super, stats$store+stats$market)
###It appears that supercenters are nearest to a combination of stores and markets. While
#stores lack many of the amenities of supermarkets

Store_Function = stores[, c(1,7, 23:51)]

hist(stores$open)
hist(super$open)
hist(market$open)
hist(store$open)
library(ggplot2)
ggplot()+
  geom_freqpoly(data = super, aes(x = open, color = 'Super Centers'))+
  geom_freqpoly(data = store, aes(x = open, color = 'Base Stores'))+
  geom_freqpoly(data = market, aes(x = open, color = 'Markets'))+
  scale_color_manual(values = c('Dodgerblue3', 'red', 'green'),
                    labels = c('Base Stores', 'Markets', 'Supercenters'),
                    name = 'Store Type')+
  xlab('Year')+ylab('Number of Openings')+
  cleanup
  
  
###Supercenters have largely followed wave spread. Stores were most common in the mass spread
#wave of 82-95. Vast majority of markets came in 2015.
spread = stores
functions = data.frame(aggregate(spread[,c(24:51)],list(spread$open), mean))
state = data.frame(Year = unique(spread$open), unclass(table(spread$open, spread$state)))
stores = stores %>%
  group_by(state) %>%
  mutate(first = min(open))

stores = stores %>%
  group_by(state) %>%
  mutate(med = median(open))

stores$row = ave(stores$open, stores$state, FUN = seq_along)
spread = subset(stores, row == 1)
spread = spread[, c(7,52,53)]
spread = subset(spread, state != 'PR')

library(maps)
library(ggplot2)
state = map_data('state')
spread$region = c("arkansas",
                  "georgia", 
                  "missouri",
                  "oklahoma", 
                  "louisiana",
                  "kansas", 
                  "tennessee",
                  "texas",
                  "kentucky",
                  "mississippi",
                  "florida",
                  "virginia",
                  "illinois",
                  "alabama",
                  "nebraska",
                  "south carolina",
                  "indiana",
                  "north carolina",
                  "new mexico",
                  "iowa",
                  "wisconsin",
                  "colorado",
                  "minnesota",
                  "arizona",
                  "ohio",
                  "wyoming",
                  "west virginia",
                  "michigan",
                  "utah",
                  "south dakota",
                  "pennsylvania",
                  "north dakota",
                  "california",
                  "nevada",
                  "new york",
                  "maryland",
                  "new hampshire",
                  "delaware",
                  "new jersey",
                  "oregon",
                  "maine",
                  "washington",
                  "montana",
                  "rhode island",
                  "idaho",
                  "connecticut",
                  "massachusetts",
                  "alaska",
                  "hawaii",
                  "vermont",
                  "district of columbia")

state = left_join(state, spread, by = 'region')
#save for remaking map
write.csv(state, file = '~/Walmart Papers/Data/statemapx.csv', row.names = F)
#now find state centroids and do it again.
cen = read.csv('~/R/State Centroids .csv')
cen$state = tolower(cen$state)
colnames(cen) = c('region', 'cenlat', 'cenlon')

labels = subset(spread, state != 'AK' & state!='HI')
labels = left_join(labels, cen, by = 'region' )
labels$cenlon = labels$cenlon*-1

library(ggplot2)

ggplot()+
  geom_polygon(data = state, color = 'grey', fill = 'NA',
               aes(x = long, y = lat, group = region)) +
  coord_fixed(1.3)+
  geom_text(data = labels, 
            aes(x = cenlon, y = cenlat, label = first))+
  coord_fixed(1.3)+
  blank+ cleanup

##Create Wave Boundries##
state$wave = 0
state$wave = ifelse(state$first >=1960 & state$first <=1970, 1, state$wave)
state$wave = ifelse(state$first >=1971 & state$first <=1977, 2, state$wave)
state$wave = ifelse(state$first >=1978 & state$first <=1984, 3, state$wave)
state$wave = ifelse(state$first >=1985 & state$first <=1990, 4, state$wave)
state$wave = ifelse(state$first >=1991, 5, state$wave)

ggplot()+
  geom_polygon(data = state, color = 'grey',
               aes(x = long, y = lat, fill = factor(wave), group = region)) +
  scale_fill_manual(values = c('white', 'cadetblue1', 'lightskyblue', 'cornflowerblue', 'dodgerblue2'),
                    labels = c('Wave 1', 'Wave 2', 'Wave 3', 'Wave 4', 'Wave 5'))+
  coord_fixed(1.3)+
  geom_text(data = labels, 
            aes(x = cenlon, y = cenlat, label = first))+
  cleanup+ yblank + xblank
  

############################
###FACTOR ANALYSIS###
###########################
library(psych)
tetcor = data.frame(tetrachoric(Store_Function[4:31])) #Output saved as CSV in Google Sheets#
write.csv(tetcor$rho, file = '~/Walmart Papers/Data/tetcorX.csv', row.names = F)
##How many factors should we consider?
princomp(Store_Function[4:31])
#3 factors > .6
#5 factors > .45
#6 factors > .4
#8 factors > .35

##What categories can we fit using 3,5,6, or 8 factors?
principal(Store_Function[4:31], nfactors = 3 ) #Not a particularly comprehensible fit
principal(Store_Function[4:31], nfactors = 5 ) #5 distinguishable and comprehensible categories
principal(Store_Function[4:31], nfactors = 6 ) #6th factor becomes all encompassing
principal(Store_Function[4:31], nfactors = 8 ) #Too segmented into finite functions

##We will use 5 factors, this best explains the varations in store functions.
##Base stores, Specialized Departments, Grocery, Alcohol, Specialty##

stores$departments = (stores$hourphoto+stores$auto+stores$garden+stores$photo+stores$vision+
                        stores$cell+stores$restaurant+stores$taxes)/8
stores$grocery = (stores$bake+stores$deli+stores$grocery+stores$liquor)/4
stores$base = (stores$merch+stores$amex+stores$check+stores$moneyorder+stores$moneytrans)/5
stores$alcohol = (stores$beer+stores$wine+stores$spirits)/3
stores$special = (stores$banking+stores$salon)/2

stores$size = stores$departments + stores$grocery + stores$base + stores$alcohol + stores$special

##############################
###Mark number of openings###
#############################
##Getting FIPS County Codes##
FIPS = read.csv('~/Data Sets/FIPS County.csv')
FIPS$County = paste(FIPS$County, 'County', sep = ' ')
stores$county = str_replace(stores$county, 'Parish', 'County')
colnames(FIPS) = c('FIPS', 'county', 'state')
stores = stores[!(stores$state == 'PR'),]

temp = left_join(stores, FIPS, by = c('state', 'county'))
fill = subset(temp, is.na(FIPS))  ##Manually fill in FIPS for mismatches in spelling
write.csv(fill, file = '~/Data Sets/fill.csv', row.names = F)

##Fill in the manually matched FIPS County codes##
temp = subset(temp, FIPS > 0)
filled = read.csv('~/Data Sets/fill-final.csv')
stores = rbind(temp, filled)

stores$n_county = ave(stores$FIPS, stores$FIPS, FUN = seq_along)
stores$n_zip = ave(stores$zip_code, stores$zip_code, FUN = seq_along)

##SIDE NOTE!! first and med both appear to be useless variables.##

write.csv(stores, file = '~/Walmart Papers/storesfinal.csv', row.names = F)