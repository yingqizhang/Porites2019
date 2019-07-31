###############################
#Author: Carly Kenkel
#Date: 07/30/2019
###############################

#library(survival) # for survival analysis 
library(vegan) # for plotting ellipses in principal coordinates analysis plots
library(corrr) # for correlations
library(MCMCglmm) # for mcmcglmm stats
library(car) # for data transformations 
library(nlme) #for lme
library(MASS) # for stepAIC
library(coxme) # for mixed effects cox model
library(tidyverse) # for data wrangling and visualization
library(ggridges) # for ridge plots
library(reshape2) # for melt
library(corrplot) # for correlations
library(summarytools) # for dfSummary
library(ggplot2)
library(RColorBrewer)
#install.packages("survminer",dependencies=TRUE)


source('summarySE.R')


################### Kaplan-Meier survival curves

nw1k=read.csv("~/Github/NetweelSurvivalExptData_Rep1_14Apr18_ModelSurvFit.csv")

nw1kA=read.csv("~/Github/NetwellSurvivalExptData_Rep2_14Apr18_ModelSurvFit.csv")

str(nw1k)	

nw1k$Family=as.factor(nw1k$Family)
nw1k$Rep=as.factor(nw1k$Rep)

summary(nw1k)

died = Surv(nw1k$tod, nw1k$dead)

#stats

#nw1k$Family <- relevel(nw1k$Family,ref="27")

surT = survfit(died ~ Treatment, data=nw1k)
surT
#For Rep 1
1-(1/180) # 99% survival of ctrl
1-(112/170) # 34% survival of heats

#For Rep 2
1-(1/329) # 99.7% survival of ctrl
1-(263/350) # 25% survival of heats

#almost no mortality events in ctrl treatment, only analyze subset of heat-treated larvae or risk coefficients will be over-estimated

nw1k_heat<-subset(nw1k,Treatment=="Heat")
summary(nw1k_heat)

diedH = Surv(nw1k_heat$tod, nw1k_heat$dead)

mAll<-coxme(diedH ~ Origin+(1|Family)+(1|Rep),data=nw1k_heat) #does survival differ among inshore/offshore
(print(mAll))

exp(0.6525430434) #large family effect! Rep 2
exp(1.3485552893)

exp(ranef(mAll)[[1]])

#Rep2

        # 3         4        14        15        26        27        28        29        31        35        39        41 
# 0.7548047 1.7974714 0.7809570 0.9437920 0.5767322 3.4466989 0.5630795 1.3706804 0.4525081 0.7616936 2.2879970 0.8265222

#Rep1

       # 2         3         4        26        27        28 
# 0.7458576 1.0276262 1.3046949 0.2700261 9.8747908 0.3750302 


#plot differential survival in heat
#rep 1
nw1k_heat$Family<- factor(nw1k_heat$Family, levels=c("31", "28", "26","3","35","14","41", "15", "29","4","39","27"))
#rep 2
nw1k_heat$Family<- factor(nw1k_heat$Family, levels=c("26", "28", "2","3","4","27"))

surF2 <- survfit(diedH ~ Family, data=nw1k_heat)
surF2

fam1 <- c("31", "28", "26","3","35","14","41", "15", "29","4","39","27")
fam2 <- c("26", "28", "2","3","4","27")
colorF1 <- colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(12)
colorF2 <- colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(6)

#note, that color is reversed to go from red to blue, rather than blue to red so legend appears as 'top' to 'btm'

par(mfrow=c(1,2))

plot(surF2, col=rev(colorF2), lwd=3, xlab=expression(paste("Hours at 36",degree,"C")), ylab="Fraction Surviving", main="Replicate A")
legend("bottomleft", col=rev(colorF2), legend=fam2, lwd=3, bty="n")

#These plots aren't great for visualizing - too many overlapping lines...just do a ggplot

nw1=read.csv("~/Dropbox/CarlsLab/ResearchProjects/PoritesSpawnApril2018/NetwellSurvivalExptData_Rep2_18Apr18.csv")

nw1=read.csv("~/Dropbox/CarlsLab/ResearchProjects/PoritesSpawnApril2018/NetwellSurvivalExptData_Rep1_14Apr18.csv")

str(nw1)

nw1$Family=as.factor(nw1$Family)
nw1$Rep=as.factor(nw1$Rep)
nw1$Time=as.factor(nw1$Time)
summary(nw1)

nw1$prop=nw1$Nlarvae/(10)

nwCC=nw1[complete.cases(nw1),]  #summary function cannot handle NAs - must remove

nwCC_heats<-subset(nwCC,Treatment=="Heat")


colorF1 <- colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(12)
colorF2 <- colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(6)


# summarize by family
all.marg2=summarySE(nwCC_heats,measurevar="prop",groupvars=c("Family","Origin", "Time"), na.rm=T)

#order for rep 1
all.marg2$Family<- factor(all.marg2$Family, levels=c("31", "28", "26","3","35","14","41", "15", "29","4","39","27"))
#rep 2
all.marg2$Family<- factor(all.marg2$Family, levels=c("26", "28", "2","3","4","27"))

pd=position_dodge(0.3)
quartz()
ggplot(all.marg2,aes(x=Time,y=prop,colour=Family,group=Family))+
 	geom_errorbar(aes(ymin=prop+se,ymax=prop-se),lwd=0.3,width=1,position=pd)+
	geom_line(aes(group=Family,linecol=Family),position=pd)+
#	geom_point(aes(group=family,pch=family),position=pd,size=2.5)
	theme(axis.text.x=element_text(size=11),axis.title=element_text(size=12))+
	xlab("Hours at 36oC")+ylab("Fraction Surviving")+
	theme_bw()+ scale_color_manual(values=rev(colorF2))
	

################ survival test - converting risk scores to appropriate expt scale

nw2=read.csv("~/Dropbox/CarlsLab/ResearchProjects/PoritesSpawnApril2018/SurvivalRiskCorrelations.csv")
str(nw2)

nw2$Family=as.factor(nw2$Family)

summary(nw2)

plot(nw2$HR_acuteStress2~nw2$HR_acuteStress1)
abline(lm(nw2$HR_acuteStress2~nw2$HR_acuteStress1))
summary(lm(nw2$HR_acuteStress2~nw2$HR_acuteStress1))

# Call:
# lm(formula = nw2$HR_acuteStress2 ~ nw2$HR_acuteStress1)

# Residuals:
      # 1       2       5       6       7 
 # 0.5594 -2.4207  0.3581  0.9973  0.5058 

# Coefficients:
                    # Estimate Std. Error t value Pr(>|t|)  
# (Intercept)          -1.8898     1.1574  -1.633   0.2010  
# nw2$HR_acuteStress1   3.1239     0.6404   4.878   0.0165 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.587 on 3 degrees of freedom
  # (7 observations deleted due to missingness)
# Multiple R-squared:  0.888,	Adjusted R-squared:  0.8507 
# F-statistic:  23.8 on 1 and 3 DF,  p-value: 0.01647
###############################
#predicting value for family 2 on scale of 12 fam expt

(0.7458576+1.89)/3.124
	
################# mild temp stress survival - might be good as suppl plot? Not included right now.

#27 still 'dies' the most - can include if asked

nw2=read.csv("~/Dropbox/CarlsLab/ResearchProjects/PoritesSpawnApril2018/MildTempStressExptDataSurv.csv")
str(nw2)

nw2$Family=as.factor(nw2$Family)
nw2$Rep=as.factor(nw2$Rep)
nw2$Time=as.factor(nw2$Time)
summary(nw2)

nw2end<-nw2[nw2$Time=="96",]
summary(nw2end)

m1<-aov(lm(Nlarvae~Family,nw2end))

TukeyHSD(m1)

all.marg=summarySE(nw2end,measurevar="Nlarvae",groupvars=c("Family","Origin"), na.rm=T)


plot(Nlarvae~Family,nw2end)

