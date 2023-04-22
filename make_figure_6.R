library(hrbrthemes)
library(tidyverse)
library(tidyquant)
library(ggalt)
library(ggpattern)
library(ggplot2)
require(unikn)
library(RColorBrewer)
library(ggpubr)
library(wesanderson)
library(jcolors)
library(grid)
jj<-c(jcolors('pal2'),jcolors('pal4'))

percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}
setwd("C:/moru_psu_dtact_comparion/A8")

pal1 <- wes_palette("Zissou1", 5, type = "continuous")
pal2 <- wes_palette("Chevalier1", 5, type = "continuous")
pal3 <- wes_palette("Royal1", 5, type = "continuous")
pal4 <- wes_palette("Cavalcanti1", 5, type = "continuous")
pal5 <- wes_palette("Darjeeling1", 5, type = "continuous")
# wes_palette("Moonrise3", 5, type = "continuous")

# cols<-c(pal2[2],pal5[2])
cols<-c("#F4861D",jj["tiffany_blue"])
display_jcolors(jj)
cols<-c("#F4861D","#0BB19F")

#### A1 per month
moru_a1<-read.csv("A8_MORU_2.csv",header = T) 
psu_a1<-read.csv("A8_PSU.csv",header = T) 

model_comp<-rbind(moru_a1,psu_a1[,-c(1:3)])
model_comp$model<-c(rep("MORU",23),rep("PSU",24))

psu_a1$lower<-rep(0,length(psu_a1$ELIMINATION_PROBABILITY))
psu_a1$higher<-rep(0,length(psu_a1$ELIMINATION_PROBABILITY))
moru_a1$lower<-rep(0,length(moru_a1$ELIMINATION_PROBABILITY))
moru_a1$higher<-rep(0,length(moru_a1$ELIMINATION_PROBABILITY))


for (i in 1:length(psu_a1$ELIMINATION_PROBABILITY)){
  aux1<-prop.test(psu_a1$ELIMINATION_PROBABILITY[i]*100,100,correct=FALSE)
  psu_a1$lower[i]<-aux1$conf.int[1]
  psu_a1$upper[i]<-aux1$conf.int[2]
  
  aux2<-prop.test(moru_a1$ELIMINATION_PROBABILITY[i]*100,100,correct=FALSE)
  moru_a1$lower[i]<-aux2$conf.int[1]
  moru_a1$upper[i]<-aux2$conf.int[2]
}


g2<-ggplot(moru_a1)
g2<-g2+geom_line(aes(x=TRIPLE_MUT_FREQ_2022, y=TRIPLE_MUT_FREQ_2032_50,color="MORU"),size=1.2)+
  geom_ribbon(aes(x=TRIPLE_MUT_FREQ_2022,ymin = TRIPLE_MUT_FREQ_2032_25, ymax = TRIPLE_MUT_FREQ_2032_75), 
              fill = cols[1], alpha = 0.35) +
  geom_line(data=psu_a1,aes(x=TRIPLE_MUT_FREQ_2022, y=TRIPLE_MUT_FREQ_2032_50,color="PSU"),size=1.1)+
  geom_ribbon(data=psu_a1,aes(x=TRIPLE_MUT_FREQ_2022,ymin = TRIPLE_MUT_FREQ_2032_25, ymax = TRIPLE_MUT_FREQ_2032_75), 
              fill = cols[2], alpha = 0.35) +
  scale_x_log10(breaks = c(0.001,0.01,0.02,0.03,0.04,0.05,0.1,0.2))+
  coord_cartesian(ylim = c(0, 0.9), xlim = c(0.003, 0.25)) +
  labs(x = "Triple mutant frequency at year 0",
        y= "Triple mutant frequency at year 10", tag = "A") +
  theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(size = 0.5)) +
  scale_color_manual(name = "Model: ",
                     values = c("MORU" = cols[1], "PSU" = cols[2]),
                     labels = c("MORU", "PSU")) +
  theme(legend.position = c(0.195, 0.85),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
# g2


g11 <- ggplot(moru_a1)
g11 <- g11 +
    geom_point(aes(x = TRIPLE_MUT_FREQ_2022, y = ELIMINATION_PROBABILITY,
        color = "MORU"), size = 3.12) +
    geom_ribbon(aes(x = TRIPLE_MUT_FREQ_2022, ymin = lower, ymax = upper,
        fill = "MORU"), alpha = 0.35) +
    geom_point(data = psu_a1, aes(x = TRIPLE_MUT_FREQ_2022, y = ELIMINATION_PROBABILITY,
        color = "PSU"), size = 3.12) +
    geom_ribbon(data = psu_a1,aes(x = TRIPLE_MUT_FREQ_2022, ymin = lower, ymax = upper,
        fill = "PSU"), alpha = 0.35) +
    scale_x_log10(breaks=c(0.003, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2)) +
    coord_cartesian(ylim = c(0, 1.0), xlim = c(0.003, 0.25)) +
    labs(x = "Triple mutant frequency at year 0",
        y = "Elimination probability over 10 years", tag = "B") +
    theme_minimal(base_size = 18) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(size = 0.5)) +
    scale_color_manual(name = "Model: ",
                values = c("MORU" = cols[1], "PSU" = cols[2]),
                labels = c("MORU", "PSU"),
                drop = FALSE) +
    scale_fill_manual(name = "Model: ",
                values = c("MORU" = cols[1], "PSU" = cols[2]),
                labels = c("MORU", "PSU"),
                drop = FALSE) +
    theme(legend.position = "none") 
# g11


gg <- grid.arrange(arrangeGrob(g2, g11, ncol = 1, nrow = 2))
ggsave("Figure6.pdf", gg, width = 28, height = 30, units = 'cm', dpi = 600)



