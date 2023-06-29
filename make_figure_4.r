library(hrbrthemes)
library(tidyverse)
library(tidyquant)
library(ggalt)
library(ggpattern)
library(ggplot2)
require(unikn)
library(RColorBrewer)
library(ggpubr)
library(ggridges)
library(ggthemes)
library(ggrepel)

setwd("C:/moru_psu_dtact_comparion/A4")

percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

f <- function(x) {
  r <- quantile(x, probs = c(0.10, 0.25, 0.5, 0.75, 0.90))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


#### A1 per month
moru_a1<-read.csv("A4_MORU.csv",header = T)
psu_a1<-read.csv("A4_PSU.csv",header = T)

moru_a1<-as.data.frame(moru_a1) #%>%
  # filter(P_TC == 0.5)
moru_a1$P_SWITCH_YEAR<-as.factor(moru_a1$P_SWITCH_YEAR)
moru_a1$model<-rep('MORU',length(moru_a1$P_SWITCH_YEAR))
moru_a1$model<-as.factor(moru_a1$model)
moru_a1$TFR_10Y<-moru_a1$TFR_10Y*100
moru_a1$P_PFPR<-moru_a1$P_PFPR*100
moru_a1$PFPR_2032<-moru_a1$PFPR_2032*100
moru_a1$P_TACT<-as.factor(moru_a1$P_TACT)

psu_a1<-as.data.frame(psu_a1) #%>%
  # filter(P_TC == 0.5)
psu_a1$P_SWITCH_YEAR<-as.factor(psu_a1$P_SWITCH_YEAR)
psu_a1$model<-rep('PSU', length(psu_a1$P_SWITCH_YEAR))
psu_a1$model<-as.factor(psu_a1$model)
psu_a1$P_TACT<-factor(psu_a1$P_TACT,labels = c("baseline", "ALAQ", "ASMQ_PPQ"))


plot(moru_a1$FREQ_580Y_2032[which(moru_a1$P_TACT == "ASMQ_PPQ" & moru_a1$P_PFPR == 10)])
plot(psu_a1$FREQ_580Y_2032[which(psu_a1$P_TACT == "ASMQ_PPQ" & psu_a1$P_PFPR == 10)])

model_comp<-rbind(moru_a1[,-7],psu_a1[,-4])

plot(model_comp$FREQ_580Y_2032[which(model_comp$P_TACT == "ASMQ_PPQ" &
         model_comp$P_PFPR == 10 & model_comp$model == "MORU")])

model_comp$P_SWITCH_YEAR<-factor(model_comp$P_SWITCH_YEAR,labels = c("0","1","2","3","4","5"))

colnames(model_comp)[2]<-"PfPR"
colnames(model_comp)[3]<-"FLT"
model_comp<-model_comp[-which(model_comp$FLT=="baseline"), ]
model_comp$PfPR <- as.factor(model_comp$PfPR)



prev.labs <- c("Pf Prevalence: 0.1%", "Pf Prevalence: 1%", "Pf Prevalence: 10%")
names(prev.labs) <- c("0.1", "1", "10")
model.labs <- c("MORU", "PSU")
names(model.labs) <- c("MORU", "PSU")


g2<-ggplot(data=model_comp, aes(x=P_SWITCH_YEAR))
g2<-g2+stat_summary(fun.data=f, aes(y=FREQ_580Y_2032, group=P_SWITCH_YEAR:FLT, color=FLT), 
            geom = "boxplot", lwd=.50, alpha = 0.9, width=0.7, position="dodge")+
  scale_fill_manual(name = "TACT: ",
                    values = c( "ALAQ" = "whitesmoke", "ASMQ_PPQ" = "whitesmoke"),
                    labels = c("ALAQ", "ASMQ_PPQ"))+
  scale_color_manual(name = "TACT: ",
                     values = c( "ALAQ" = "#e76f3d", "ASMQ_PPQ" = "#00a7c7"),
                     labels = c("ALAQ", "ASMQ_PPQ"))+
  facet_grid(model~PfPR, labeller=labeller(PfPR=prev.labs, model = model.labs) ,scales = "free") +
  theme_minimal(base_size = 16)+
  scale_y_continuous(trans='log2', breaks = c(0.01,0.05,.1, .2, .5, .9)) +
  scale_x_discrete(breaks=c(2022:2027),labels = c(0,1,2,3,4,5))+
  labs(x="Delay (number of years) before TACT is introduced as first-line therapy", y="580Y Frequency at year 10", title="")+
  theme(panel.spacing = unit(2, "lines"))+
  theme(legend.position = "top")
g2


gg2<-ggplot(data=model_comp, aes(x=P_SWITCH_YEAR))
gg2<-gg2+stat_summary(fun.data=f, aes(y=TFR_10Y, group=P_SWITCH_YEAR:FLT, color=FLT), 
        geom = "boxplot", lwd=.50, alpha = 0.9, width=0.7, position="dodge")+
  scale_fill_manual(name = "TACT: ",
        values = c( "ALAQ" = "whitesmoke", "ASMQ_PPQ" = "whitesmoke"),
        labels = c("ALAQ", "ASMQ_PPQ"))+
  scale_color_manual(name = "TACT: ",
        values = c( "ALAQ" = "#e76f3d", "ASMQ_PPQ" = "#00a7c7"),
        labels = c("ALAQ", "ASMQ_PPQ"))+
  facet_grid(model~PfPR, labeller=labeller(PfPR=prev.labs, model = model.labs) ,scales = "free")

gg2<-gg2+theme_minimal(base_size = 16)+
  scale_x_discrete(breaks=c(2022:2027),labels = c(0,1,2,3,4,5))+
  scale_y_continuous(trans='log2', breaks = c(7.5,10, 15, 20,25, 30)) +
  labs(x="Delay (number of years) before TACT is introduced as first-line therapy", y="Treatment failure rate (%) at year 10", title="")+
  theme(panel.spacing = unit(2, "lines"))+
  theme(legend.position = "none")
gg2

# tiff("Figure5_v77.tiff", width = 25, height = 30, units = 'cm', res = 300)
gg <- ggarrange(g2 + xlab(""), gg2+ theme(strip.text.x = element_blank()), labels = c("A", "B"), ncol = 1, nrow = 2)
gg
ggsave("Figure4.pdf", gg, width = 25, height = 30, units = 'cm', dpi = 300)
# dev.off()

