staff = read.csv('staff error/staffing workload.csv')

library(tidyverse)
staff$year = as.numeric(paste0(20,str_sub(staff$Date, 1, 2)))
staff$month = match(str_sub(staff$Date, -3,-1), month.abb)
staff$time = staff$year + ((staff$month-.5)/12)

staff$jr_sr_staff = staff$jr/staff$sr
staff$jr_tot = staff$jr / staff$Total.Positions
staff$sr_vac = staff$sr / staff$Vacant.Positions
staff$vacancy.rate = staff$Vacant.Positions / staff$Total.Positions

staff$tot.per.app = staff$Initial.Apps / staff$Total.Positions
staff$jr.per.app = staff$Initial.Apps / staff$jr
staff$sr.per.app = staff$Initial.Apps / staff$sr

staff$tot.per.cust =  staff$Individual.Cases / staff$Total.Positions
staff$jr.per.cust = staff$Individual.Cases / staff$jr
staff$sr.per.cust = staff$Individual.Cases / staff$sr

staff$tot.per.acct = staff$HH.Cases / staff$Total.Positions
staff$jr.per.acct = staff$HH.Cases / staff$jr
staff$sr.per.acct = staff$HH.Cases / staff$sr

staff$ot.per.jr = staff$OT.jr / staff$jr
staff$ot.per.sr = staff$OT.jr / staff$sr



staff2 = staff[1:44,]

library(ggplot2)
ggplot(staff) +
  geom_point(aes(x = time, y = Active.Error.Rate*20000), color  = 'red')+
  geom_line(aes(x = time, y = Active.Error.Rate*20000, group = 1), color  = 'red')+
  geom_smooth(aes(x = time, y = Active.Error.Rate*20000), color = 'red')+
  geom_line(aes(x = time, y = jr, group = 1), color  = 'blue')+
  geom_line(aes(x = time, y = sr, group = 1), color  = 'green')+
  geom_line(aes(x = time, y = Initial.Apps/10, group = 1), color = 'black')


ggplot(staff) +
  geom_smooth(aes(x = time, y = Active.Error.Rate*3000), color = 'grey')+
  geom_line(aes(x = time, y = tot.per.app, group = 1), color = 'red')+
  geom_line(aes(x = time, y = tot.per.cust, group = 1), color = 'blue')+
  geom_line(aes(x = time, y = tot.per.acct, group = 1), color = 'green')


ggplot(staff) +
  geom_smooth(aes(x = time, y = Active.Error.Rate*20000), color = 'grey')+
  geom_line(aes(x = time, y = jr.per.app, group = 1), color = 'red')+
  geom_line(aes(x = time, y = jr.per.cust, group = 1), color = 'blue')+
  geom_line(aes(x = time, y = jr.per.acct, group = 1), color = 'green')


ggplot(staff) +
  geom_smooth(aes(x = time, y = Active.Error.Rate*30), color = 'grey')+
  geom_line(aes(x = time, y = sr.per.app, group = 1), color = 'red')+
  geom_line(aes(x = time, y = sr.per.cust, group = 1), color = 'blue')+
  geom_line(aes(x = time, y = sr.per.acct, group = 1), color = 'green')

staff$Active.Error.Rate = staff$Active.Error.Rate / 100


lm1 = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ Total.Positions+Initial.Apps, data = staff)
lm2 = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ Total.Positions+ tot.per.app+sr.per.app, data = staff)
lm3 = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ OT.jr + OT.sr + Total.Positions+Initial.Apps, data = staff)
lm4 = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ OT.jr + OT.sr +Total.Positions+ tot.per.app+sr.per.app, data = staff)
#lm3
lm3a = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ OT.jr +I(OT.jr^2) + OT.sr+I(OT.sr^2) + Total.Positions+Initial.Apps, data = staff)
lm3b = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ OT.jr +I(OT.jr^2) + OT.sr+I(OT.sr^2) + tot.per.app+I(tot.per.app^2), data = staff)
lm3c = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ ot.per.jr +I(ot.per.jr^2) + ot.per.sr+I(ot.per.sr^2) + tot.per.app+I(tot.per.app^2), data = staff)

lm3b1 = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ OT.jr +I(OT.jr^2) + OT.sr+I(OT.sr^2), data = staff)
lm3c1 = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ ot.per.jr +I(ot.per.jr^2) + ot.per.sr+I(ot.per.sr^2), data = staff)

library(jtools)
effect_plot(lm1, pred = vacancy.rate, plot.points = T)
effect_plot(lm2, pred = vacancy.rate, plot.points = T)
effect_plot(lm3, pred = vacancy.rate, plot.points = T)
effect_plot(lm4, pred = vacancy.rate, plot.points = T)

effect_plot(lm1, pred = jr_sr_staff, plot.points = T)
effect_plot(lm2, pred = jr_sr_staff, plot.points = T)
effect_plot(lm3, pred = jr_sr_staff, plot.points = T)
effect_plot(lm4, pred = jr_sr_staff, plot.points = T)

effect_plot(lm1, pred = Total.Positions, plot.points = T)
effect_plot(lm2, pred = Total.Positions, plot.points = T)
effect_plot(lm3, pred = Total.Positions, plot.points = T)
effect_plot(lm4, pred = Total.Positions, plot.points = T)

effect_plot(lm3a, pred = OT.jr, plot.points = T)

effect_plot(lm3a, pred = vacancy.rate, plot.points = T)
effect_plot(lm3a, pred = jr_sr_staff, plot.points = T)
effect_plot(lm3a, pred = Total.Positions, plot.points = T)


effect_plot(lm3b, pred = OT.jr, plot.points = T)
effect_plot(lm3b, pred = OT.sr, plot.points = T)
effect_plot(lm3b, pred = vacancy.rate, plot.points = T)
effect_plot(lm3b, pred = jr_sr_staff, plot.points = T)
effect_plot(lm3b, pred = tot.per.app, plot.points = T)

effect_plot(lm3c, pred = ot.per.jr, plot.points = T)
effect_plot(lm3c, pred = ot.per.sr, plot.points = T)
effect_plot(lm3c, pred = vacancy.rate, plot.points = T)
effect_plot(lm3c, pred = jr_sr_staff, plot.points = T)
effect_plot(lm3c, pred = tot.per.app, plot.points = T)

#lmb3 has most accurate outcomes
lm3b = lm(Active.Error.Rate ~ jr_sr_staff + I(jr_sr_staff^2)+ vacancy.rate + I(vacancy.rate^2)+ OT.jr +I(OT.jr^2) + OT.sr+I(OT.sr^2) + tot.per.app+I(tot.per.app^2), data = staff)


##Three recommendations from analysis where change in slope is greatest, suggesting the optimum point##
# 1. Roughly 45 total Impressions per position
# 2. 7.5/1 ratio or JR to SR staff
# 3. Drop Rate becomes much larger if vacancy rate is > 12.5%

#Visualze these three results
effect_plot(lm3b, pred = vacancy.rate, plot.points = T, interval = T, y.label = 'Client Drop Rate', x.label = 'Percent of Positions Vacant')
effect_plot(lm3b, pred = jr_sr_staff, plot.points = T, interval = T, y.label = 'Client Drop Rate', x.label = 'JR to SR Staff Ratio')
effect_plot(lm3b, pred = tot.per.app, plot.points = T, interval = T, y.label = 'Client Drop Rate', x.label = 'Client Impressions per Staff')


