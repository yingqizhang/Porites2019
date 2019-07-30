###############################
#Authors: Carly Kenkel & Yingqi Zhang
#Date: 07/30/2019
###############################

setwd("~/Github")
source('summarySE.R')
library(MCMCglmm)
library(dplyr)

fec=read.csv("~/Github/DailyReleaseData.csv")
str(fec)
fec$Date=as.factor(fec$Date)
fec$Day=as.factor(fec$Day)
fec$Family=as.factor(fec$Family)
fec$FamDay<-with(fec,factor(Family:Day))
summary(fec)

fec %>%
	group_by(Origin) %>%
	summarize(meanLar=mean(Nlarvae),sumLar=sum(Nlarvae))


larvalv=read.csv("~/Github/LarvalVolume.csv")
str(larvalv)
larvalv$Date=as.factor(larvalv$Date)
larvalv$Day=as.factor(larvalv$Day)
larvalv$Family=as.factor(larvalv$Family)
larvalv$Rep=as.factor(larvalv$Rep)
summary(larvalv)
larvalv$Volume=(4/3)*pi*(0.5*larvalv$Length)*(0.5*larvalv$Width)^2
all.marg=summarySE(larvalv,measurevar="Volume",groupvars=c("Family","Date","Day"), na.rm=T)
all.marg
all.marg$FamDay<-with(all.marg,factor(Family:Day))

list<-as.character(all.marg$FamDay)

dat1<-full_join(all.marg,fec,by="FamDay")
#insufficient larvae for family 7 on day 3 to do volume measures, remove
dat1<-dat1[1:54,] 

summary(lm(Volume~Nlarvae,dat1)) #NS
plot(Volume~Nlarvae,dat1)

zooxdaily=read.csv("~/Github/ZooxDailyRls.csv")
str(zooxdaily)
zooxdaily$Date=as.factor(zooxdaily$Date)
zooxdaily$Day=as.factor(zooxdaily$Day)
zooxdaily$Family=as.factor(zooxdaily$Family)
zooxdaily$Rep=as.factor(zooxdaily$Rep)
summary(zooxdaily)
zooxdaily<-zooxdaily[complete.cases(zooxdaily$Zoox.Lar),]

all.marg1=summarySE(zooxdaily,measurevar="Zoox.Lar",groupvars=c("Family","Date","Day"))
all.marg1
all.marg1$FamDay<-with(all.marg1,factor(Family:Day))

dat2<-full_join(all.marg1,fec,by="FamDay")
#For low release families, insufficient larvae for phys measures, remove
dat2<-dat2[1:39,] 

summary(lm(Zoox.Lar~Nlarvae,dat2)) #NS
plot(Zoox.Lar~Nlarvae,dat2)

chldaily=read.csv("~/Github/ChlDailyRls.csv")
str(chldaily)
chldaily$Date=as.factor(chldaily$Date)
chldaily$Day=as.factor(chldaily$Day)
chldaily$Family=as.factor(chldaily$Family)
chldaily$Rep=as.factor(chldaily$Rep)
summary(chldaily)
chldaily<-chldaily[complete.cases(chldaily$Chla.Lar),]


all.marg2=summarySE(chldaily,measurevar="Chla.Lar",groupvars=c("Family","Date","Day"), na.rm=T)
all.marg2
all.marg2$FamDay<-with(all.marg2,factor(Family:Day))


dat3<-full_join(all.marg2,fec,by="FamDay")
#For low release families, insufficient larvae for phys measures, remove
dat3<-dat3[1:39,] 


summary(lm(Chla.Lar~Nlarvae,dat3)) #NS
plot(Chla.Lar~Nlarvae,dat3)

prdaily=read.csv("~/Github/PrtDailyRls.csv")
str(prdaily)
prdaily$Date=as.factor(prdaily$Date)
prdaily$Day=as.factor(prdaily$Day)
prdaily$Family=as.factor(prdaily$Family)
prdaily$Rep=as.factor(prdaily$Rep)
summary(prdaily)
all.marg3=summarySE(prdaily,measurevar="Prt.Lar",groupvars=c("Family","Date","Day"), na.rm=T)
all.marg3
all.marg3$FamDay<-with(all.marg3,factor(Family:Day))

dat4<-full_join(all.marg3,fec,by="FamDay")
#For low release families, insufficient larvae for phys measures, remove
dat4<-dat4[1:39,] 

summary(lm(Prt.Lar~Nlarvae,dat4)) #NS
plot(Prt.Lar~Nlarvae,dat4)

cfec=read.csv("~/Github/CFec.csv")
str(cfec)
cfec$Family=as.factor(cfec$Family)
summary(cfec)

zooxexp=read.csv("~/Github/ZooxExp.csv")
str(zooxexp)
zooxexp$Date=as.factor(zooxexp$Date)
zooxexp$Family=as.factor(zooxexp$Family)
zooxexp$Trmt=as.factor(zooxexp$Trmt)
zooxexp$Rep=as.factor(zooxexp$Rep)
all.marg4=summarySE(zooxexp,measurevar="Zoox.Lar",groupvars=c("Family","Date","Trmt"), na.rm=T)
all.marg4
write.table(all.marg4,file = "symchange.csv", sep = ",",col.names = NA)

chlexp=read.csv("~/Github/ChlExp.csv")
str(chlexp)
chlexp$Date=as.factor(chlexp$Date)
chlexp$Family=as.factor(chlexp$Family)
chlexp$Trmt=as.factor(chlexp$Trmt)
chlexp$Rep=as.factor(chlexp$Rep)
all.marg5=summarySE(chlexp,measurevar="Chla.Lar",groupvars=c("Family","Date","Trmt"), na.rm=T)
all.marg5
write.table(all.marg5,file = "chlachange.csv", sep = ",",col.names = NA)

prexp=read.csv("~/Github/PrtExp.csv")
str(prexp)
prexp$Date=as.factor(prexp$Date)
prexp$Family=as.factor(prexp$Family)
prexp$Trmt=as.factor(prexp$Trmt)
prexp$Rep=as.factor(prexp$Rep)
all.marg6=summarySE(prexp,measurevar="Prt.Lar",groupvars=c("Family","Date","Trmt"), na.rm=T)
all.marg6
write.table(all.marg6,file = "prchange.csv", sep = ",",col.names = NA)

FecMildCor=read.csv("~/Github/FecMildCor.csv")
FecMildCor$Family=as.factor(FecMildCor$Family)
summary(FecMildCor)
summary(lm(SymChange~Nlarvae,FecMildCor)) #NS
plot(SymChange~Nlarvae,FecMildCor)
summary(lm(ChlaChange~Nlarvae,FecMildCor)) #NS
plot(ChlaChange~Nlarvae,FecMildCor)
summary(lm(ProtChange~Nlarvae,FecMildCor)) #NS
plot(ProtChange~Nlarvae,FecMildCor)

FecAcuteCor=read.csv("~/Github/SurvivalRiskCorrelations.csv")
summary(lm(HR_acuteStress1~CumulativeFec,FecAcuteCor)) #NS