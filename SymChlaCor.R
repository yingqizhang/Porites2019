###############################
#Authors: Carly Kenkel & Yingqi Zhang
#Date: 07/30/2019
###############################

setwd("~/Github")

#Daily rls
zooxdaily=read.csv("~/Github/ZooxDailyRls.csv")
str(zooxdaily)
zooxdaily$Date=as.factor(zooxdaily$Date)
zooxdaily$Day=as.factor(zooxdaily$Day)
zooxdaily$Family=as.factor(zooxdaily$Family)
zooxdaily$Rep=as.factor(zooxdaily$Rep)
zooxdaily$DayFamRep<-with(zooxdaily,factor(Day:Family:Rep))
summary(zooxdaily)

chldaily=read.csv("~/Github/ChlDailyRls.csv")
str(chldaily)
chldaily$Date=as.factor(chldaily$Date)
chldaily$Day=as.factor(chldaily$Day)
chldaily$Family=as.factor(chldaily$Family)
chldaily$Rep=as.factor(chldaily$Rep)
chldaily$DayFamRep<-with(chldaily,factor(Day:Family:Rep))
summary(chldaily)

table(zooxdaily$DayFamRep==chldaily$DayFamRep) #should return TRUE if datasets align correctly, otherwise your rows are out of order

summary(lm(chldaily$Chla.Lar~zooxdaily$Zoox.Lar))
par(mfrow=c(1,2))
plot(chldaily$Chla.Lar~zooxdaily$Zoox.Lar,ylab="Chlorophyll a (µg/larvae)",xlab="Symbiont density (cells/larvae)",main="Daily Physiology")
abline(lm(chldaily$Chla.Lar~zooxdaily$Zoox.Lar))

#Mild stress
zooxexp=read.csv("~/Github/ZooxExp.csv")
str(zooxexp)
zooxexp$Date=as.factor(zooxexp$Date)
zooxexp$Family=as.factor(zooxexp$Family)
zooxexp$Trmt=as.factor(zooxexp$Trmt)
zooxexp$Rep=as.factor(zooxexp$Rep)
zooxexp$FamTrtRep<-with(zooxexp,factor(Family:Trmt:Rep))
summary(zooxexp)

chlexp=read.csv("~/Github/ChlExp.csv")
str(chlexp)
chlexp$Date=as.factor(chlexp$Date)
chlexp$Family=as.factor(chlexp$Family)
chlexp$Trmt=as.factor(chlexp$Trmt)
chlexp$Rep=as.factor(chlexp$Rep)
chlexp$Origin=as.factor(chlexp$Origin)
chlexp$FamTrtRep<-with(chlexp,factor(Family:Trmt:Rep))
summary(chlexp)

table(zooxexp$FamTrtRep==chlexp$FamTrtRep) #should return TRUE if datasets align correctly, otherwise your rows are out of order

summary(lm(chlexp$Chla.Lar~zooxexp$Zoox.Lar))
plot(chlexp$Chla.Lar~zooxexp$Zoox.Lar,ylab="Chlorophyll a (µg/larvae)",xlab="Symbiont density (cells/larvae)",main="Sub-lethal Heat Stress")
abline(lm(chlexp$Chla.Lar~zooxexp$Zoox.Lar))