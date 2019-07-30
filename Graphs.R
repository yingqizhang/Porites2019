#install .packages(MCMCglmm)
library(MCMCglmm)
install.packages("ggplot2")
library(ggplot2)
setwd("~/Github")
source('summarySE.R')

###Larval volume
larvalv=read.csv("~/Github/LarvalVolume.csv")
str(larvalv)
larvalv$Date=as.factor(larvalv$Date)
larvalv$Day=as.factor(larvalv$Day)
larvalv$Family=as.factor(larvalv$Family)
larvalv$Rep=as.factor(larvalv$Rep)
summary(larvalv)

larvalv$Volume=(4/3)*pi*(0.5*larvalv$Length)*(0.5*larvalv$Width)^2

#Generate s.d. and s.e. for volume in categories of family and date
all.marg=summarySE(larvalv,measurevar="Volume",groupvars=c("Family","Date","Day"), na.rm=T)
all.marg

pd=position_dodge(0.3)
quartz()

ggplot(all.marg,aes(x=Day,y=Volume,colour=Family,group=Family))+
  geom_errorbar(aes(ymax=Volume+se,ymin=Volume-se),lwd=0.3,width=1,position=pd)+
  #geom_line(aes(group=Family,linecol=Family),position=pd)+
  geom_point(aes(group=Family),position=pd,size=2.5)+
  theme(axis.text.x=element_text(size=8),axis.title=element_text(size=10))+
  xlab("Day")+ylab('Larval volume' ~(mm^3))+
  facet_wrap(~Family,scales="fixed",ncol=5)+
  guides(fill=FALSE, color=FALSE)+ #Remove the legend
  theme_bw()#+ scale_color_manual(values=c(red12(4),blu8(8)))

###Symbiont daily release
zooxdaily=read.csv("~/Github/ZooxDailyRls.csv")
str(zooxdaily)
zooxdaily$Date=as.factor(zooxdaily$Date)
zooxdaily$Day=as.factor(zooxdaily$Day)
zooxdaily$Family=as.factor(zooxdaily$Family)
zooxdaily$Rep=as.factor(zooxdaily$Rep)
zooxdaily=zooxdaily[-c(64,74), ] #Left out Apr15 Fam 15 and 29 due to the lack of replicates
summary(zooxdaily)

zooxdaily$Zoox.Lar1=zooxdaily$Zoox.Lar/1000

#Generate s.d. and s.e. for volume in categories of family and date
all.marg=summarySE(zooxdaily,measurevar="Zoox.Lar1",groupvars=c("Family","Date","Day"), na.rm=T)
all.marg

pd=position_dodge(0.3)
quartz()

ggplot(all.marg,aes(x=Day,y=Zoox.Lar1,colour=Family,group=Family))+
  geom_errorbar(aes(ymax=Zoox.Lar1+se,ymin=Zoox.Lar1-se),lwd=0.3,width=1,position=pd)+
  #geom_line(aes(group=Family,linecol=Family),position=pd)+
  geom_point(aes(group=Family),position=pd,size=2.5)+
  theme(axis.text.x=element_text(size=8),axis.title=element_text(size=10))+
  labs(x="Day",y=expression(Symbiont~density~(x~10^3~cells/larvae)))+
  facet_wrap(~Family,scales="fixed",ncol=4)+
  guides(fill=FALSE, color=FALSE)+ #Remove the legend
  theme_bw()#+ scale_color_manual(values=c(red12(4),blu8(8)))

###Symbiont mild stress
zooxexp=read.csv("~/Github/ZooxExp.csv")
str(zooxexp) #Looking at the structure of the data
zooxexp$Date=as.factor(zooxexp$Date)
zooxexp$Family=as.factor(zooxexp$Family)
zooxexp$Trmt=as.factor(zooxexp$Trmt)
zooxexp$Rep=as.factor(zooxexp$Rep)
#zooxexp=zooxexp[-c(23), ] #Left out 26HE due to insufficient extraction
summary(zooxexp)
zooxexp$Zoox.Lar1=zooxexp$Zoox.Lar/1000

#Summary graph for family average by trmt groups
all.marg=summarySE(zooxexp,measurevar="Zoox.Lar1",groupvars=c("Family","Date","Trmt"), na.rm=T)
all.marg

pd=position_dodge(0.3)
quartz()

ggplot(all.marg,aes(x=Family,y=Zoox.Lar1,colour=Trmt,group=Trmt,shape=Trmt))+
  geom_errorbar(aes(ymax=Zoox.Lar1+se,ymin=Zoox.Lar1-se),lwd=0.3,width=1,position=pd)+
  #geom_line(aes(group=Family,linecol=Family),position=pd)+
  geom_point(aes(group=Trmt),position=pd,size=2.5)+
  #scale_shape_identity()+
  #geom_bar(stat="identity")+
  theme(axis.text.x=element_text(size=18),axis.title=element_text(size=20))+
  labs(x="Family",y=expression(Symbiont~density~(x~10^3~cells/larvae)))+
  theme_bw()+
  scale_color_manual(values=c("turquoise","orange"))

###Chla daily release
chldaily=read.csv("~/Github/ChlDailyRls.csv")
str(chldaily)
chldaily$Date=as.factor(chldaily$Date)
chldaily$Day=as.factor(chldaily$Day)
chldaily$Family=as.factor(chldaily$Family)
chldaily$Rep=as.factor(chldaily$Rep)
chldaily=chldaily[-c(19,64,74), ] #Left out Apr15 Fam 15 and 29 due to the lack of replicates, Apr16 Fam 3.1 due to high variance
summary(chldaily)

#Generate s.d. and s.e. for volume in categories of family and date
all.marg=summarySE(chldaily,measurevar="Chla.Lar",groupvars=c("Family","Date","Day"), na.rm=T)
#all.marg=all.marg[-c(14,27), ] #Get rid of Apr15 Fam 15 and 29
all.marg

pd=position_dodge(0.3)
quartz()

ggplot(all.marg,aes(x=Day,y=Chla.Lar,colour=Family,group=Family))+
  geom_errorbar(aes(ymax=Chla.Lar+se,ymin=Chla.Lar-se),lwd=0.3,width=1,position=pd)+
  #geom_line(aes(group=Family,linecol=Family),position=pd)+
  geom_point(aes(group=Family),position=pd,size=2.5)+
  theme(axis.text.x=element_text(size=8),axis.title=element_text(size=10))+
  labs(x="Day",y=expression(Chlorophyll~a~concentration~(µg/larvae)))+
  facet_wrap(~Family,scales="fixed",ncol=4)+
  guides(fill=FALSE, color=FALSE)+ #Remove the legend
  theme_bw()#+ scale_color_manual(values=c(red12(4),blu8(8)))

###Chla mild stress
chlexp=read.csv("~/Github/ChlExp.csv")
str(chlexp)
chlexp$Date=as.factor(chlexp$Date)
chlexp$Family=as.factor(chlexp$Family)
chlexp$Trmt=as.factor(chlexp$Trmt)
chlexp$Rep=as.factor(chlexp$Rep)
#chlexp=chlexp[-c(23), ] #Left out 26HE due to insufficient extraction
summary(chlexp)

#Summary graph for family average by trmt groups
all.marg=summarySE(chlexp,measurevar="Chla.Lar",groupvars=c("Family","Date","Trmt"), na.rm=T)
all.marg

pd=position_dodge(0.3)
quartz()

ggplot(all.marg,aes(x=Family,y=Chla.Lar,colour=Trmt,group=Trmt,shape=Trmt))+
  geom_errorbar(aes(ymax=Chla.Lar+se,ymin=Chla.Lar-se),lwd=0.3,width=1,position=pd)+
  #geom_line(aes(group=Family,linecol=Family),position=pd)+
  geom_point(aes(group=Trmt),position=pd,size=2.5)+
  #scale_shape_identity()+
  #geom_bar(stat="identity")+
  theme(axis.text.x=element_text(size=18),axis.title=element_text(size=20))+
  labs(x="Family",y=expression(Chlorophyll~a~concentration~(µg/larvae)))+
  theme_bw()+ 
  scale_color_manual(values=c("turquoise","orange"))

###Protein daily release
prdaily=read.csv("~/Github/PrtDailyRls.csv")
str(prdaily) #Looking at the structure of the data
prdaily$Date=as.factor(prdaily$Date)
prdaily$Day=as.factor(prdaily$Day)
prdaily$Family=as.factor(prdaily$Family)
prdaily$Rep=as.factor(prdaily$Rep)
prdaily=prdaily[-c(64,74), ] #Left out Apr15 Fam 15 and 29 due to the lack of replicates

#Generate s.d. and s.e. for volume in categories of family and date
all.marg=summarySE(prdaily,measurevar="Prt.Lar",groupvars=c("Family","Date","Day"), na.rm=T)
all.marg

pd=position_dodge(0.3)
quartz()

#Panel plot by family
ggplot(all.marg,aes(x=Day,y=Prt.Lar,colour=Family,group=Family))+
  geom_errorbar(aes(ymax=Prt.Lar+se,ymin=Prt.Lar-se),lwd=0.3,width=1,position=pd)+
  #geom_line(aes(group=Family,linecol=Family),position=pd)+
  geom_point(aes(group=Family),position=pd,size=2.5)+
  theme(axis.text.x=element_text(size=8),axis.title=element_text(size=10))+
  labs(x="Day",y=expression(Protein~concentration~(µg/larvae)))+
  facet_wrap(~Family,scales="fixed",ncol=4)+
  guides(fill=FALSE, color=FALSE)+ #Remove the legend
  theme_bw()#+ scale_color_manual(values=c(red12(4),blu8(8)))

###Protein mild stress
prexp=read.csv("~/Github/PrtExp.csv")
str(prexp) #Looking at the structure of the data
prexp$Date=as.factor(prexp$Date)
prexp$Family=as.factor(prexp$Family)
prexp$Trmt=as.factor(prexp$Trmt)
prexp$Rep=as.factor(prexp$Rep)
#prexp=prexp[-c(23), ] #Left out 26HE due to insufficient extraction
summary(prexp)

#Summary graph for family average by trmt groups
all.marg=summarySE(prexp,measurevar="Prt.Lar",groupvars=c("Family","Date","Trmt"), na.rm=T)
all.marg

pd=position_dodge(0.3)
quartz()

ggplot(all.marg,aes(x=Family,y=Prt.Lar,colour=Trmt,group=Trmt,shape=Trmt))+
  geom_errorbar(aes(ymax=Prt.Lar+se,ymin=Prt.Lar-se),lwd=0.3,width=1,position=pd)+
  #geom_line(aes(group=Family,linecol=Family),position=pd)+
  geom_point(aes(group=Trmt),position=pd,size=2.5)+
  #scale_shape_identity()+
  #geom_bar(stat="identity")+
  theme(axis.text.x=element_text(size=18),axis.title=element_text(size=20))+
  labs(x="Family",y=expression(Protein~concentration~(µg/larvae)))+
  theme_bw()+
  scale_color_manual(values=c("turquoise","orange"))
