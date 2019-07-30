###############################
#Authors: Carly Kenkel & Yingqi Zhang
#Date: 07/30/2019
###############################

setwd("~/Github")
source('summarySE.R')

#install packages

library(MCMCglmm)
library(car)
library(nlme)
library(MuMIn)
library(ggplot2)
library(knitr)

###Volume analysis on daily release larvae

#Read in file
larvalv=read.csv("~/Github/LarvalVolume.csv")
str(larvalv)
larvalv$Date=as.factor(larvalv$Date)
larvalv$Day=as.factor(larvalv$Day)
larvalv$Family=as.factor(larvalv$Family)
larvalv$Rep=as.factor(larvalv$Rep)
summary(larvalv)

#Volume calculation based on an elliptical model
larvalv$Volume=(4/3)*pi*(0.5*larvalv$Length)*(0.5*larvalv$Width)^2
summary(larvalv)
hist(larvalv$Volume) 

#ANOVA with origin, family, day as fixed effects
modelVol <- aov(lm(Volume~Origin*Family*Day,data=larvalv))

#Diagnostics to determine residual is normally distributed- all good
qqPlot(residuals(modelVol),xlab="Theoretical Quantiles",ylab="Observed Quantiles")

#Check homoscedascitity by plotting residuals against fitted values - all good
plot(modelVol,which=1) #Exhibit a random scatter; no apparent trendline

summary(modelVol)

#Tukey test for differences among factor levels
TukeyHSD(modelVol)

###Symbiont density for daily release larvae

#Read in file
zooxdaily=read.csv("~/Github/ZooxDailyRls.csv")
str(zooxdaily)
zooxdaily$Date=as.factor(zooxdaily$Date)
zooxdaily$Day=as.factor(zooxdaily$Day)
zooxdaily$Family=as.factor(zooxdaily$Family)
zooxdaily$Rep=as.factor(zooxdaily$Rep)
zooxdaily=zooxdaily[-c(64,74), ] #Left out Apr15 Fam 15 and 29 due to the lack of replicates
summary(zooxdaily)

hist(zooxdaily$Zoox.Lar) #Not normally distributed
zooxdaily$Log=log(zooxdaily$Zoox.Lar) #Log transform
hist(zooxdaily$Log) #Looks good now

#ANOVA with origin, family, day as fixed effects
modelZoox <- aov(lm(Log~Origin*Family*Day,data=zooxdaily))

#Diagnostics to determine if models satisfy assumptions - all good
qqPlot(residuals(modelZoox),xlab="Theoretical Quantiles",ylab="Observed Quantiles")
plot(modelZoox,which=1)

summary(modelZoox)

TukeyHSD(modelZoox)

###Chla conc on daily release larvae

#Read in file
chldaily=read.csv("~/Github/ChlDailyRls.csv")
str(chldaily)
chldaily$Date=as.factor(chldaily$Date)
chldaily$Day=as.factor(chldaily$Day)
chldaily$Family=as.factor(chldaily$Family)
chldaily$Rep=as.factor(chldaily$Rep)
chldaily=chldaily[-c(64,74), ] #Left out Apr15 Fam 15 and 29 due to the lack of replicates
summary(chldaily)
hist(chldaily$Chla.Lar)

#ANOVA with origin, family, day as fixed effects
modelChl <- aov(lm(Chla.Lar~Origin*Family*Day,data=chldaily))

#Diagnostics to determine if models satisfy assumptions - all good
qqPlot(residuals(modelChl),xlab="Theoretical Quantiles",ylab="Observed Quantiles")
plot(modelChl,which=1)

summary(modelChl)

TukeyHSD(modelChl)

###Protein conc on daily release larvae

#Read in file
prdaily=read.csv("~/Github/PrtDailyRls.csv")
str(prdaily)
prdaily$Date=as.factor(prdaily$Date)
prdaily$Day=as.factor(prdaily$Day)
prdaily$Family=as.factor(prdaily$Family)
prdaily$Rep=as.factor(prdaily$Rep)
prdaily=prdaily[-c(64,74), ] #Left out Apr15 Fam 15 and 29 due to the lack of replicates
summary(prdaily)
hist(prdaily$Prt.Lar)

#ANOVA with origin, family, day as fixed effects
modelPrt <- aov(lm(Prt.Lar~Origin*Family*Day,data=prdaily))

#Diagnostics to determine if models satisfy assumptions - all good
qqPlot(residuals(modelPrt),xlab="Theoretical Quantiles",ylab="Observed Quantiles")
plot(modelPrt,which=1)

summary(modelPrt)

TukeyHSD(modelPrt)

#################################################

###Symbiont conc on mild stress larvae

#Read in file
zooxexp=read.csv("~/Github/ZooxExp.csv")
str(zooxexp)
zooxexp$Date=as.factor(zooxexp$Date)
zooxexp$Family=as.factor(zooxexp$Family)
zooxexp$Trmt=as.factor(zooxexp$Trmt)
zooxexp$Rep=as.factor(zooxexp$Rep)
summary(zooxexp)
hist(zooxexp$Zoox.Lar)

#Mixed model with origin and treatment as fixed effects, and family as random effect
modelZooxExp <- lme(Zoox.Lar~Trmt*Origin,data=zooxexp,random= ~1|Family,method="REML",na.action=na.omit)

#Diagnostics to determine if models satisfy assumptions - all good
qqPlot(residuals(modelZooxExp),xlab="Theoretical Quantiles",ylab="Observed Quantiles")
plot(modelZooxExp,which=1)

#How good is this model overall? the proportion of variance explained by the fixed factors alone, and by both fixed and random factors.
r.squaredGLMM(modelZooxExp)

summary(modelZooxExp)

#Wald Test for terms within model
table<-anova(modelZooxExp) 
table
kable(table,format="pandoc")

###Chla conc on mild stress larvae

#Read in file
chlexp=read.csv("~/Github/ChlExp.csv")
str(chlexp)
chlexp$Date=as.factor(chlexp$Date)
chlexp$Family=as.factor(chlexp$Family)
chlexp$Trmt=as.factor(chlexp$Trmt)
chlexp$Rep=as.factor(chlexp$Rep)
chlexp$Origin=as.factor(chlexp$Origin)
summary(chlexp)
hist(chlexp$Chla.Lar)

#Mixed model with origin and treatment as fixed effects, and family as random effect
modelChlExp <- lme(Chla.Lar~Trmt*Origin,data=chlexp,random= ~1|Family,method="REML",na.action=na.omit)

#Diagnostics to determine if models satisfy assumptions - all good
qqPlot(residuals(modelChlExp),xlab="Theoretical Quantiles",ylab="Observed Quantiles")
plot(modelChlExp,which=1) 

#How good is this model overall? the proportion of variance explained by the fixed factors alone, and by both fixed and random factors.
r.squaredGLMM(modelChlExp)

summary(modelChlExp)

#Wald Test for terms within model
table<-anova(modelChlExp) 
table
kable(table,format="pandoc")

###Protein conc on mild stress larvae
prexp=read.csv("~/Github/PrtExp.csv")
str(prexp)
prexp$Date=as.factor(prexp$Date)
prexp$Family=as.factor(prexp$Family)
prexp$Trmt=as.factor(prexp$Trmt)
prexp$Rep=as.factor(prexp$Rep)
summary(prexp)

hist(prexp$Prt.Lar) #Needs log transformation
prexp$Log=log(prexp$Prt.Lar) #Log transform
hist(prexp$Log)

#Mixed model with origin and treatment as fixed effects, and family as random effect
modelPrtExp <- lme(Log~Trmt*Origin,data=prexp,random= ~1|Family,method="REML",na.action=na.omit)

#Diagnostics to determine if models satisfy assumptions - all good
qqPlot(residuals(modelPrtExp),xlab="Theoretical Quantiles",ylab="Observed Quantiles")
plot(modelPrtExp,which=1)

#How good is this model overall? the proportion of variance explained by the fixed factors alone, and by both fixed and random factors.
r.squaredGLMM(modelPrtExp)

summary(modelPrtExp)

#Wald Test for terms within model
table<-anova(modelPrtExp) 
table
kable(table,format="pandoc")