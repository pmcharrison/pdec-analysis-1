####load libraries ######
#http://www.personality-project.org/r/r.anova.html
library("lme4")
library("gdata") 
library("Hmisc")
library(languageR)
library (tidyr)
library(lmerTest)
library(plyr) # needed to access . function
library(ggplot2)
library(reshape2)
library(lsr) # for eta squared calculation
library(ez)
library(schoRsch)  #for formatted output of anova
library(pastecs) #for descriptives
library(car)  #for lmer
library(effects) #for lmer
library(lsmeans)#for lmer
library(readxl)
source("diagnostic_fcns.r")
library(lattice)
library(foreign)
library(multcomp)
library(lsmeans)
library(phia)
library(stats)
library(reshape2)
library(data.table)
library(plotrix)
library(HH)
library(effects)
library(grid)
library(data.table)
library(pander)
library(stargazer)
library(xtable)
library(robustHD)
######

ls()
rm(list=ls())

### LOAD FILE #####
dat=read.table(file="exp1_retention.txt",header=T, sep="\t")  # read data
dat$subj<-as.factor(dat$subj)
dat$block<-as.factor(dat$block)
dat$cond<-as.factor(dat$cond)
dat$trialN<-as.numeric(dat$trialN)

#### PLOT RTs ####
d=dat

ag= with(d, aggregate(cbind(RTadj) ~ subj+block+cond+day, FUN="mean"))
levels(ag$cond) <- c('RAN-REG', 'RAN-REGt')
a= ag
s=11
p<-ggplot(a, aes(block, RTadj, group=cond,  fill = cond, color=cond))+theme_bw()+
  stat_summary(fun.y=mean, geom="line", alpha=0.8)+
  #geom_point(aes(group=subj, fill=cond, color=cond), shape=21, size=1)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0, alpha=0.6)+
  stat_summary(fun.y = 'mean',  geom ="point", size=2, shape=21)+
  scale_color_manual(values=c("blue", "coral"))+
  scale_fill_manual(values=c("blue", "coral"))+
  xlab("Block") + ylab("Reaction times (s)")+coord_cartesian(ylim = c(1.4, 1.9))+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=s),
        axis.title=element_text(size=s),legend.title = element_text(size=s),
        legend.text = element_text(size = s))+
  theme(legend.position='none') +
  ggtitle("Mean RTs across blocks")

pdf(file= "exp1_RT.pdf", 3,3)
grid.arrange(p, ncol=1)
dev.off()

#### PLOT RANREG/TARGET dot plot (single subj per each block) 
s=10
sc= with(d, aggregate(cbind(RTadj) ~ subj+cond+block, FUN="mean"))
cast=dcast(sc, subj~cond+block, value.var="RTadj")
p1= ggplot(cast, aes(RANDREG_1, TARGET_1, label=subj, colour= subj))+theme_bw()+
  geom_point( size=1, shape=21, colour= 'black', fill='black', alpha=0.7)+
  xlab("RAN-REG (s)") + ylab("RAN-REGt (s)")+
  #geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1)+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=s),
        axis.title=element_text(size=s),legend.title = element_text(size=s),
        legend.text = element_text(size = s), plot.title = element_text(size=s))+
  geom_abline(slope=1, lty=2)+
  coord_equal(ratio=1)+
  scale_y_continuous(minor_breaks = seq(1 , 2.4, 0.5), breaks=seq(1, 2.4, 0.5), limits=c(1,2.4))+
  scale_x_continuous(minor_breaks = seq(1 , 2.4, 0.5),breaks=seq(1, 2.4, 0.5),limits=c(1,2.4))+
  theme(legend.position="none")+ggtitle('block 1')


p5=ggplot(cast, aes(RANDREG_5, TARGET_5, label=subj, colour= subj, fill = subj))+theme_bw()+
  geom_point( size=1, shape=21, colour= 'black', fill='black', alpha=0.7)+
  #xlab("RTs RAN-REG (s)") + ylab("RTs RAN-REGt (s)")+
  xlab("") + ylab("")+
  #geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1)+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=s),
        axis.title=element_text(size=s),legend.title = element_text(size=s),
        legend.text = element_text(size = s), plot.title = element_text(size=s))+
  geom_abline(slope=1, lty=2)+coord_equal(ratio=1)+ 
  scale_y_continuous(minor_breaks = seq(1 , 2.4, 0.5), breaks=seq(1, 2.4, 0.5), limits=c(1,2.4))+
  scale_x_continuous(minor_breaks = seq(1 , 2.4, 0.5),breaks=seq(1, 2.4, 0.5),limits=c(1,2.4))+
  theme(legend.position="none")+ggtitle('block 5')

p6=ggplot(cast, aes(RANDREG_6, TARGET_6, label=subj, colour= subj, fill = subj))+theme_bw()+
  geom_point( size=1, shape=21, colour= 'black', fill='black', alpha=0.7)+
  #xlab("RTs RAN-REG (s)") + ylab("RTs RAN-REGt (s)")+
  xlab("") + ylab("")+
  #geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1)+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=s),
        axis.title=element_text(size=s),legend.title = element_text(size=s),
        legend.text = element_text(size = s), plot.title = element_text(size=s))+
  geom_abline(slope=1, lty=2)+coord_equal(ratio=1)+  
  scale_y_continuous(minor_breaks = seq(1 , 2.4, 0.5), breaks=seq(1, 2.4, 0.5), limits=c(1,2.4))+
  scale_x_continuous(minor_breaks = seq(1 , 2.4, 0.5),breaks=seq(1, 2.4, 0.5),limits=c(1,2.4))+
  theme(legend.position="none")+ggtitle('after 24 hours')

p7=ggplot(cast, aes(RANDREG_7, TARGET_7, label=subj, colour= subj, fill = subj))+theme_bw()+
  geom_point( size=1, shape=21, colour= 'black', fill='black', alpha=0.7)+
  #xlab("RTs RAN-REG (s)") + ylab("RTs RAN-REGt (s)")+
  xlab("") + ylab("")+
  #geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1)+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=s),
        axis.title=element_text(size=s),legend.title = element_text(size=s),
        legend.text = element_text(size = s), plot.title = element_text(size=s))+
  geom_abline(slope=1, lty=2)+coord_equal(ratio=1)+
  scale_y_continuous(minor_breaks = seq(1 , 2.4, 0.5), breaks=seq(1, 2.4, 0.5), limits=c(1,2.4))+
  scale_x_continuous(minor_breaks = seq(1 , 2.4, 0.5),breaks=seq(1, 2.4, 0.5),limits=c(1,2.4))+
  theme(legend.position="none")+ggtitle('after 7 weeks')

pdf(file= "exp1_RTindividuals.pdf", 6,3)
grid.arrange(p1,p5,p6, p7, ncol=4)
dev.off()


#### PLOT TARGET seq againsts Ranreg per each subject (BLOCK 5)
sc= with(d[d$block==5,], aggregate(cbind(RTadj) ~ cond+setID+subj, FUN="mean"))
sc1=sc[sc$cond=="RANDREG",]
cast=with(sc1, aggregate(RTadj~subj, FUN="mean"))
sc1=sc[sc$cond=="TARGET",]
cast1=with(sc1, aggregate(RTadj~setID+subj, FUN="mean"))
sc=merge(cast, cast1, by="subj")

###check what patterns were learnt (BLOCK 5)
s=11
p3gg = ggplot(sc, aes(RTadj.x, RTadj.y, label=setID))+geom_point(size=1, alpha=0.6)+theme_bw()+
  xlab("RAN-REG (s)") + ylab("RAN-REGt (s)")+
  #geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text=element_text(size=s),
        axis.title=element_text(size=s),legend.title = element_text(size=s),
        legend.text = element_text(size = s))+geom_abline(slope=1, lty=2)+coord_equal(ratio=1)+
  scale_y_continuous(minor_breaks = seq(1 , 2.4, 0.5), breaks=seq(1, 2.4, 0.5), limits=c(1,2.4))+
  scale_x_continuous(minor_breaks = seq(1 , 2.4, 0.5),breaks=seq(1, 2.4, 0.5),limits=c(1,2.4))+
  theme(legend.position="none")

pdf(file="exp1_RT_indAllPatterns.pdf", 2,2)
grid.arrange(p3gg, ncol=1)
dev.off()

#### PLOT RTs by intra block repetition ####
ag= with(d, aggregate(cbind(RTadj) ~ subj+block+cond+day+rep, FUN="mean"))
ag$rep=as.factor(ag$rep)
levels(ag$cond) <- c('RAN-REG', 'RAN-REGt')
a= ag
s=11
p<-ggplot(a, aes(rep, RTadj, group=cond,  fill = cond, color=cond))+theme_bw()+facet_grid(~block)+
  stat_summary(fun.y=mean, geom="line", alpha=0.8)+
  #geom_point(aes(group=subj, fill=cond, color=cond), shape=21, size=1)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0, alpha=0.6)+
  stat_summary(fun.y = 'mean',  geom ="point", size=2, shape=21)+
  scale_color_manual(values=c("blue", "coral"))+
  scale_fill_manual(values=c("blue", "coral"))+
  xlab("intra block repetition") + ylab("Reaction times (s)")+coord_cartesian(ylim = c(1.4, 1.9))+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=s),
        axis.title=element_text(size=s),legend.title = element_text(size=s),
        legend.text = element_text(size = s))+
  theme(legend.position='none') +
  ggtitle("Mean RTs across blocks")

pdf(file= "exp1_RT_intrablock_Repetition.pdf", 6,3)
grid.arrange(p, ncol=1)
dev.off()
