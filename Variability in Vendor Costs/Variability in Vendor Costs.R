# The purpose of this project was to assess vendor and customer activity to explain high levels of variabilty in transaction
# costs for business units (coded as customers) when making operational purchases

###Geocode Vendor Address###
data = read.csv('Projects/Vendor Costs/data.csv')
data$Vendor.Address = paste(data$Vendor.Street, data$Vendor.City, sep = ', ')
data$Vendor.Address = paste(data$Vendor.Address, data$Vendor.State, sep = ', ')
data$Vendor.Address = paste(data$Vendor.Address, data$Vendor.Zip, sep = ' ')

vend = as.data.frame(data[!duplicated(data[,29]),])

library(tidyverse)
library(tidygeocoder)
vend = vend %>%
  append(geo(address = vend$Vendor.Address, method = 'cascade', lat = lat, long = lon))
vend = as.data.frame(vend)

nas = subset(vend, is.na(lat))
nageo = geo(address = nas$Vendor.Address, method = 'bing', lat = lat1, long = lon1)
names(nageo)[1] = 'Vendor.Address'

vend = left_join(vend, nageo, by = 'Vendor.Address')
vend$lat = ifelse(is.na(vend$lat), vend$lat1, vend$lat)
vend$lon = ifelse(is.na(vend$lon), vend$lon1, vend$lon)
vend = vend[c(29,462,463)]

data = left_join(data, vend, by = 'Vendor.Number')
names(data)[c(461,462)] = c('vend.lat', 'vend.lon')

###Geocode Customer Zip###
library(tigris)
library(sf)
zips = usa::zipcodes
names(zips)[c(1,4:5)] = c('ZIP.Code', 'cust.lat', 'cust.lon')
zips$ZIP.Code = as.numeric(zips$ZIP.Code)
data = left_join(data, zips[c(1,4,5)], by = 'ZIP.Code')

###Distance Between Customer and Vendor###
library(geosphere)
data$dist.to.vend = distHaversine(data[,462:461], data[,464:463]) / 1609.34

###Count Vendors in Category###
data = data %>%
  group_by(code.category_2, year) %>%
  mutate(n.vend.cat = n_distinct(Vendor.Number))

###Count Transactions in Each Category for Each Vendor###
data = data %>%
  group_by(Vendor.Number, year, code.category_2) %>%
  mutate(n.trans.cat = max(seq_along(code.category_2)))

###Descriptive stats on competitiveness###
data$trans.date = as.numeric(str_sub(data$Date.Rendered, -4,-1))
data$trans.date = paste(str_sub(data$Date.Rendered,1,2), data$trans.date, sep = '.')
data$trans.city = paste(data$Vendor.City, data$Vendor.State, sep = ', ')

library(zipcodeR)
data$Vendor.Zip = as.character(str_sub(data$Vendor.Zip, 1,5))
vend = data[!duplicated(data[28]),]
vend = vend[c(28,35)]
zc = reverse_zipcode(vend$Vendor.Zip)
names(zc)[c(1,6)] = c('Vendor.Zip', 'Vendor.County')
vend = left_join(vend, zc[c(1,6)], by = 'Vendor.Zip') %>% distinct()
data = left_join(data, vend[c(1,3)], by = 'Vendor.Number')
na = subset(data, is.na(Vendor.County))
na = subset(na, !duplicated(Vendor.City)) #Manually look up the city and county for those R could not find

data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'Danbury, CT', 'Fairfield County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'Belfast, ME', 'Waldo County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'Manchester, NH', 'Hillsborough County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'Boston, MA', 'Suffolk County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'South River, NJ', 'Middlesex County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'Mayfield, KY', 'Graves County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'Mobile, AL', 'Mobile County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'Knoxville, TN', 'Knox County', data$Vendor.County)
data$Vendor.County = ifelse(is.na(data$Vendor.County) & data$trans.city == 'North Haven, CT', 'New Haven County', data$Vendor.County)

####Create variables to measure market share###
#Number of transactions in category in each city
data = data %>%
  group_by(trans.date, trans.city, code.category_2)%>%
  mutate(n.trans.cty = n_distinct(Trans.Number))

#Number of vendors with transactions in each category in each city
data = data %>%
  group_by(trans.date, trans.city, code.category_2)%>%
  mutate(n.vend.cty = n_distinct(Vendor.Number))  

#Vendor market share for each category in each city
data = data %>%
  group_by(trans.date, trans.city, code.category_2, Vendor.Number) %>%
  mutate(vend.share.cty = n_distinct(Trans.Number) / n.trans.cty*100)

#Number of transactions in category in each county
data = data %>%
  group_by(trans.date, Vendor.County, code.category_2)%>%
  mutate(n.trans.cnt = n_distinct(Trans.Number)) 

#Number of vendors with transactions in each category in each county
data = data %>%
  group_by(trans.date, Vendor.County, code.category_2)%>%
  mutate(n.vend.cnt = n_distinct(Vendor.Number))

#Vendor market share for each category in each county
data = data %>%
  group_by(trans.date, Vendor.County, code.category_2, Vendor.Number) %>%
  mutate(vend.share.cnt = n_distinct(Trans.Number)/ n.trans.cnt*100)


data$trans.month = str_sub(data$Date.Rendered,1,2)
data$trans.month = as.numeric(gsub('[/]','', data$trans.month))/12
data$trans.month = as.numeric(str_sub(data$Date.Rendered,-4,-1)) + data$trans.month
data$begin.month = str_sub(data$unit.start.date,1,2)
data$begin.month = as.numeric(gsub('[/]', '', data$begin.month))/12
data$begin.month = as.numeric(str_sub(data$unit.start.date,-4,-1)) + data$begin.month
data$tenure = data$trans.month - data$begin.month

data$Amount = gsub('[$]', '', data$Amount)
data$Amount = gsub('[,]', '', data$Amount)
data$Amount = as.numeric(data$Amount)

data$n.trans = as.numeric(ave(data$Invoice.., data$SSN, FUN = seq_along))


###Log Model for PctDelta###
#Some variables will be surpressed for privacy#
data$Amount = log(data$Amount)
data$combined.factors.of.interest = paste(data$factor1, data$factor2, data$factor1.2, sep = ';')

#County#
library(lfe)
county.all = felm(Amount ~ n.trans.cnt + n.vend.cnt + vend.share.cnt +
                    n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                    var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                    local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                    year + Vendor.County + trans.inter.cause, data = data)

county.transactions = felm(Amount ~ n.trans.cnt + n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                             var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                             var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                             local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                             year + Vendor.County + trans.inter.cause, data = data)

county.vendors = felm(Amount ~ n.vend.cnt + n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                        var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                        var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                        local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                        year + Vendor.County + trans.inter.cause, data = data)

county.market.share = felm(Amount ~ vend.share.cnt + n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                             var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                             var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                             local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                             year + Vendor.County + trans.inter.cause, data = data)

county.market.share2 = felm(Amount ~ vend.share.cnt*n.trans.cnt + n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                              var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                              var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                              local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                              year + Vendor.County + trans.inter.cause, data = data)


library(stargazer)
stargazer(county.all, county.transactions, county.vendors, county.market.share, county.market.share2,
          type = 'html', title = 'county.Delta', out = 'countydelta.html')

#City#
city.all = felm(Amount ~ n.trans.cty + n.vend.cty + vend.share.cty+
                  n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                  var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                  var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                  local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                  year + Vendor.County + trans.inter.cause, data = data)

city.transactions = felm(Amount ~ n.trans.cty + n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                           var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                           var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                           local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                           year + Vendor.County + trans.inter.cause, data = data)

city.vendors = felm(Amount ~ n.vend.cty + n.trans + dist.to.vend + tenure + race + Var1 + var2 + barriers + 
                      var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                      var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                      local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                      year + Vendor.County + trans.inter.cause, data = data)

city.market.share = felm(Amount ~ vend.share.cty + n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                           var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                           var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                           local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                           year + Vendor.County + trans.inter.cause, data = data)

city.market.share2 = felm(Amount ~ vend.share.cty*n.trans.cty + n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
                            var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
                            var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
                            local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
                            year + Vendor.County + trans.inter.cause, data = data)

##Create output table, not available because of privacy considerations
stargazer(city.all, city.transactions, city.vendors, city.market.share, city.market.share2,
          type = 'html', title = 'city.Delta', out = 'citydelta.html')

##Get the output to delta percent##
county.all$beta = (exp(county.all$beta)-1)*100
county.transactions$beta = (exp(county.transactions$beta)-1)*100
county.vendors$beta = (exp(county.vendors$beta)-1)*100
county.market.share$beta = (exp(county.market.share$beta)-1)*100
county.market.share2$beta = (exp(county.market.share2$beta)-1)*100

city.all$beta = (exp(city.all$beta)-1)*100
city.transactions$beta = (exp(city.transactions$beta)-1)*100
city.vendors$beta = (exp(city.vendors$beta)-1)*100
city.market.share$beta = (exp(city.market.share$beta)-1)*100
city.market.share2$beta = (exp(city.market.share2$beta)-1)*100

stargazer(county.all, county.transactions, county.vendors, county.market.share,
          type = 'html', title = 'county.Delta', out = 'countydelta.html')
stargazer(city.all, city.transactions, city.vendors, city.market.share,
          type = 'html', title = 'city.Delta', out = 'citydelta.html')


###Avg and IQ spread to assess correlation between monopolization and costs###
data$Amount = exp(data$Amount)
data = data %>%
  group_by(trans.date, trans.city, code.category_2, Vendor.Number)%>%
  mutate(avg.amount.cty = mean(Amount))  #for a vendor in a city, in a month, for a particular service

data = data %>%
  group_by(trans.date, trans.city, code.category_2, Vendor.Number)%>%
  mutate(q1.amount.cty = quantile(Amount, probs = .25))  #for a vendor in a city, in a month, for a particular service

data = data %>%
  group_by(trans.date, trans.city, code.category_2, Vendor.Number)%>%
  mutate(q3.amount.cty = quantile(Amount, probs = .75))  #for a vendor in a city, in a month, for a particular service

data$iqspread.cty = data$q3.amount.cty - data$q1.amount.cty

data = data %>%
  group_by(trans.date, Vendor.County, code.category_2, Vendor.Number)%>%
  mutate(avg.amount.cnt = mean(Amount))  #for a vendor in a city, in a month, for a particular service

data = data %>%
  group_by(trans.date, Vendor.County, code.category_2, Vendor.Number)%>%
  mutate(q1.amount.cnt = quantile(Amount, probs = .25))  #for a vendor in a city, in a month, for a particular service

data = data %>%
  group_by(trans.date, Vendor.County, code.category_2, Vendor.Number)%>%
  mutate(q3.amount.cnt = quantile(Amount, probs = .75))  #for a vendor in a city, in a month, for a particular service

data$iqspread.cnt = data$q3.amount.cnt - data$q1.amount.cnt

###Get monopoly scores for each vendor##
#unique vender-month observations#
vend = data[!duplicated(data[c(29,468)]),]
###Use Z score Estimations###
vend$z.n.trans.cnt = (vend$n.trans.cnt - mean(vend$n.trans.cnt))/sd(vend$n.trans.cnt)
vend$z.n.vend.cnt = (vend$n.vend.cnt - mean(vend$n.vend.cnt))/sd(vend$n.vend.cnt)
vend$z.vend.share.cnt = (vend$vend.share.cnt - mean(vend$vend.share.cnt))/sd(vend$vend.share.cnt)

vend$z.n.trans.cty = (vend$n.trans.cty - mean(vend$n.trans.cty))/sd(vend$n.trans.cty)
vend$z.n.vend.cty = (vend$n.vend.cty - mean(vend$n.vend.cty))/sd(vend$n.vend.cty)
vend$z.vend.share.cty = (vend$vend.share.cty - mean(vend$vend.share.cty))/sd(vend$vend.share.cty)

#Create 4 grid quantile categories for z scores the visually assess market share's impact on price#
vend$quad.cnt = 0
vend$quad.cnt = ifelse(vend$z.vend.share.cnt > 0 & vend$z.n.trans.cnt > 0, 1, vend$quad.cnt)
vend$quad.cnt = ifelse(vend$z.vend.share.cnt < 0 & vend$z.n.trans.cnt > 0, 2, vend$quad.cnt)
vend$quad.cnt = ifelse(vend$z.vend.share.cnt < 0 & vend$z.n.trans.cnt < 0, 3, vend$quad.cnt)
vend$quad.cnt = ifelse(vend$z.vend.share.cnt > 0 & vend$z.n.trans.cnt < 0, 4, vend$quad.cnt)

vend$quad.cnt2 = 0
vend$quad.cnt2 = ifelse(vend$z.vend.share.cnt >= 1 & vend$z.n.trans.cnt >= 0.1049, 1, vend$quad.cnt2)
vend$quad.cnt2 = ifelse(vend$z.vend.share.cnt <= -1 & vend$z.n.trans.cnt >= 0.1049, 2, vend$quad.cnt2)
vend$quad.cnt2 = ifelse(vend$z.vend.share.cnt <= -1 & vend$z.n.trans.cnt <= -0.5832, 3, vend$quad.cnt2)
vend$quad.cnt2 = ifelse(vend$z.vend.share.cnt >= 1 & vend$z.n.trans.cnt <= -0.5832, 4, vend$quad.cnt2)

vend$quad.cty = 0
vend$quad.cty = ifelse(vend$z.vend.share.cty > 0 & vend$z.n.trans.cty > 0, 1, vend$quad.cty)
vend$quad.cty = ifelse(vend$z.vend.share.cty < 0 & vend$z.n.trans.cty > 0, 2, vend$quad.cty)
vend$quad.cty = ifelse(vend$z.vend.share.cty < 0 & vend$z.n.trans.cty < 0, 3, vend$quad.cty)
vend$quad.cty = ifelse(vend$z.vend.share.cty > 0 & vend$z.n.trans.cty < 0, 4, vend$quad.cty)

vend$quad.cty2 = 0
vend$quad.cty2 = ifelse(vend$z.vend.share.cty >= .75 & vend$z.n.trans.cty >= 0.02402, 1, vend$quad.cty2)
vend$quad.cty2 = ifelse(vend$z.vend.share.cty <= -1.0677   & vend$z.n.trans.cty >= 0.02402, 2, vend$quad.cty2)
vend$quad.cty2 = ifelse(vend$z.vend.share.cty <= -1.0677   & vend$z.n.trans.cty <= -0.5, 3, vend$quad.cty2)
vend$quad.cty2 = ifelse(vend$z.vend.share.cty >= .75 & vend$z.n.trans.cty <= -0.5, 4, vend$quad.cty2)

#Merge for regs#
vend = vend[c(28,467,491:500)]
data = left_join(data, vend, by = c('Vendor.Number','trans.date'))
data$z.amount = (data$Amount - mean(data$Amount))/sd(data$Amount)

data$Amount = log(data$Amount)
#Monopoly regs#

data$quad.cnt <- factor(data$quad.cnt, levels = c('1','2','3','4'))
data$quad.cty <- factor(data$quad.cty, levels = c('1','2','3','4'))
data$quad.cnt2 <- factor(data$quad.cnt2, levels = c('0','1','2','4'))
data$quad.cty2 <- factor(data$quad.cty2, levels = c('0','1','2','4'))

m.1 = felm(Amount ~ quad.cnt +
             n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
             var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
             var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
             local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
             year + Vendor.County + trans.inter.cause, data = data)

m.2 = felm(Amount ~ quad.cnt2 +
             n.trans + dist.to.vend + tenure +Var1 + var2 + barriers + 
             var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
             var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
             local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
             year + Vendor.County + trans.inter.cause, data = data)

m.3 = felm(Amount ~ quad.cty +
             n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
             var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
             var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
             local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
             year + Vendor.County + trans.inter.cause, data = data)

m.4 = felm(Amount ~ quad.cty2 +
             n.trans + dist.to.vend + tenure + Var1 + var2 + barriers + 
             var3 + var4 + var5 + var6 + var7+  var8 + var9 + var10 + 
             var11 + var12 + var13 + var14 + total.local.retail.sales + local.unemployment.rate + local.med.income + 
             local.poverty.pct + var15+ local.prop.crime + var16 + var17 + local.med.yrs.edu |
             year + Vendor.County + trans.inter.cause, data = data)

m.1$beta = (exp(m.1$beta)-1)*100
m.2$beta = (exp(m.2$beta)-1)*100
m.3$beta = (exp(m.3$beta)-1)*100
m.4$beta = (exp(m.4$beta)-1)*100

stargazer(m.1,m.2,m.3,m.4, type = 'html', out = 'test.html')

##Results were visualized with tableau, can be found in repository as 'cost.n.png'##
