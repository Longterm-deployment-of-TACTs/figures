library(hrbrthemes)
library(tidyverse)
library(tidyquant)
library(ggalt)
library(ggpattern)
library(ggplot2)
require(unikn)
library(RColorBrewer)
library(ggpubr)
library(gghighlight)
library(ggridges)
library(ggthemes)
library(ggrepel)

setwd("C:/moru_psu_dtact_comparion/A4")

moru_a1<-read.csv("A4_MORU.csv",header = T)
psu_a1<-read.csv("A4_PSU.csv",header = T)

moru_a1<-as.data.frame(moru_a1) 
moru_a1$P_SWITCH_YEAR<-as.factor(moru_a1$P_SWITCH_YEAR)
moru_a1$model<-rep('MORU',length(moru_a1$P_SWITCH_YEAR))
moru_a1$model<-as.factor(moru_a1$model)
moru_a1$TFR_10Y<-moru_a1$TFR_10Y*100
moru_a1$P_PFPR<-moru_a1$P_PFPR*100
moru_a1$PFPR_2032<-moru_a1$PFPR_2032*100
moru_a1$P_TACT<-as.factor(moru_a1$P_TACT)

psu_a1<-as.data.frame(psu_a1)
psu_a1$P_SWITCH_YEAR<-as.factor(psu_a1$P_SWITCH_YEAR)
psu_a1$model<-rep('PSU', length(psu_a1$P_SWITCH_YEAR))
psu_a1$model<-as.factor(psu_a1$model)
psu_a1$P_TACT<-factor(psu_a1$P_TACT,labels = c("baseline", "ALAQ", "ASMQ_PPQ"))


model_comp<-rbind(moru_a1[,-7],psu_a1[,-4])

subplot<-model_comp %>%
  filter(P_TACT == "ALAQ", model == "PSU", P_PFPR == 0.1)
g2<-ggplot(subplot)
g2<-g2+stat_ecdf(aes(x=PFPR_2032,color=P_SWITCH_YEAR,group=P_SWITCH_YEAR),size=1)+
  coord_cartesian(xlim=c(0.0006,0.08))+
  scale_x_log10( breaks=c(0.001,0.01, 0.05), labels=c("0.001%", "0.01%", "0.05%"))+
  geom_vline(xintercept = 0.001, col="grey80")+geom_vline(xintercept = 0.01,col="grey80")+geom_vline(xintercept = 0.05,col="grey80")+
  scale_color_tableau(palette = "Jewel Bright",direction=-1,name="TACT switch delay in years")+
  theme_minimal(base_size = 22)+
  labs(x="Definition of elimination threshold (PfPR)", y="Probability of elimination at year 10")+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),legend.position="top") 
g2+ geom_point(aes(x=.01, y=.96),size=2.5)+
  geom_label_repel(inherit.aes = FALSE, 
                   data = data.frame(x=.01, y=.96), 
                   aes(x=0.01,y=0.96,label="96%"),
                   nudge_x=-0.3,
                   size          = 8,
                   box.padding   = 0.25,
                   point.padding = 0.25,
                   force         = 50,
                   segment.size  = 0.1,
                   segment.color = "grey50",
                   direction     = "x")+
  geom_point(aes(x=.01, y=.56),size=2.5)+
  geom_label_repel(inherit.aes = FALSE, 
                   data = data.frame(x=.01, y=.56), 
                   aes(x=0.01,y=0.56,label="56%"),
                   nudge_x = 0.3,
                   size          = 8,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 50,
                   segment.size  = 0.1,
                   segment.color = "grey50",
                   direction     = "x")
ggsave("elim_prob.pdf", width = 27, height = 20, units = 'cm', dpi = 600)

  
