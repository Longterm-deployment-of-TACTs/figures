library(hrbrthemes)
library(tidyverse)
library(tidyquant)
library(ggalt)
library(ggpattern)
library(ggplot2)

percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}
setwd("C:/moru_psu_dtact_comparion")

moru_a1 <- read.table("A1_MORU.txt", header = TRUE)
psu_a1 <- read.table("A1_PSU.txt", header = TRUE)
moru_a2 <- read.table("A2_MORU.txt", header = TRUE)
psu_a2 <-read.table("A2_PSU.txt", header = TRUE)
moru_a3 <- read.table("A3_MORU.txt", header = TRUE)
psu_a3 <- read.table("A3_PSU.txt", header = TRUE)

moru_a1_tf <- moru_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(MONTH == 120, P_TC == 0.5)
moru_a1_tf[, c(5:9)] <- moru_a1_tf[, c(5:9)]
moru_a1_tf$P_TACT <- recode(moru_a1_tf$P_TACT, '0'="ACT",'1'="ALAQ",'2'="ASMQ-PPQ")


psu_a1_tf <- psu_a1 %>% 
  filter(MONTH == 120, P_TC == 0.5) %>%
  select(MONTH,P_TC,P_PFPR,P_TACT,FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75)
psu_a1_tf$P_TACT <- recode(psu_a1_tf$P_TACT, '0'="ACT",'2'="ALAQ",'1'="ASMQ-PPQ")

mm_comp<-rbind(moru_a1_tf,psu_a1_tf)
colnames(mm_comp)<-c("Experiment","TreatmentCoverage","PfPrevalence",
                     "TACT","moru5","moru50","moru95","moru25","moru75")
mm_comp$model<-c(rep("MORU",9),rep("PSU",9))
mm_comp$model<-as.factor(mm_comp$model)
mm_comp$TACT <-as.factor(mm_comp$TACT)
levels(mm_comp$TACT)<-c("ACT","ALAQ","ASMQ-PPQ")
mm_comp$Experiment<-c(1:18)


moru_a2_tf <- moru_a2 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(MONTH == 120, P_TC == 0.5)
moru_a2_tf[, c(5:9)] <- moru_a2_tf[, c(5:9)]

psu_a2_tf <- psu_a2 %>% 
  filter(MONTH==120, P_TC==0.5)%>%
  select(MONTH,P_TC,P_PFPR,P_TACT,FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) 

mm_comp2<-rbind(moru_a2_tf,psu_a2_tf)
colnames(mm_comp2)<-c("Experiment","TreatmentCoverage","PfPrevalence",
                      "TACT","moru5","moru50","moru95","moru25","moru75")
mm_comp2$model<-c(rep("MORU",9),rep("PSU",9))
mm_comp2$model<-as.factor(mm_comp2$model)
mm_comp2$TACT <-as.factor(mm_comp2$TACT)
levels(mm_comp2$TACT)<-c("ACT","ALAQ","ASMQ-PPQ")
mm_comp2$Experiment<-c(19:36)


moru_a3_tf <- moru_a3 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(MONTH==120, P_TC==0.5)
moru_a3_tf$P_TACT<-as.factor(moru_a3_tf$P_TACT)
moru_a3_tf[,c(5:9)]<-moru_a3_tf[,c(5:9)]
levels(moru_a3_tf$P_TACT)<-c("ACT","ASMQ-PPQ","ALAQ")
colnames(moru_a3_tf)<-c("Experiment","TreatmentCoverage","PfPrevalence",
                        "TACT","moru5","moru50","moru95","moru25","moru75")
psu_a3_tf <- psu_a3 %>% 
  filter(MONTH==120, P_TC==0.5)%>%
  select(MONTH,P_TC,P_PFPR,P_TACT,FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) 
psu_a3_tf$P_TACT<-as.factor(psu_a3_tf$P_TACT)
levels(psu_a3_tf$P_TACT)<-c("ACT","ALAQ","ASMQ-PPQ")
colnames(psu_a3_tf)<-c("Experiment","TreatmentCoverage","PfPrevalence",
                       "TACT","moru5","moru50","moru95","moru25","moru75")

mm_comp3<-rbind(moru_a3_tf,psu_a3_tf)
mm_comp3$model<-c(rep("MORU",9),rep("PSU",9))
mm_comp3$model<-as.factor(mm_comp3$model)
mm_comp3$Experiment<-c(37:54)


mm_comp<-rbind(mm_comp,mm_comp2,mm_comp3)
mm_comp$FLT<-c(rep("ACT",18),rep("ASAQ",18),rep("AL",18))
mm_comp$FLT<-factor(mm_comp$FLT, levels = c("ACT", "ASAQ", "AL"))

colfunc <- as.character(pal_unikn_light[1, 1:6])
prev.labs<-c("Pf Prevalence: 0.1%", "Pf Prevalence: 1%", "Pf Prevalence: 10%")
names(prev.labs) <- c("0.1", "1", "10")
drug.labs<-c("baseline ACT: DHA-PPQ", "baseline ACT: ASAQ", "baseline ACT: AL")
names(drug.labs) <- c("ACT", "ASAQ", "AL")

mycols<-c("grey40", "grey80", "#075384", "#5295C1",
        "#CF4300", "#FF9663")
gg <- ggplot(mm_comp)
gg <- gg + geom_boxplot(aes(x=TACT,ymin=moru5,lower=moru25,middle=moru50,upper=moru75,ymax=moru95, 
                            group=Experiment,fill=(TACT:model)),stat = "identity",width=0.4)+
  scale_fill_manual(values=mycols,name="")+
  facet_grid(PfPrevalence~FLT,labeller = labeller(PfPrevalence=prev.labs, FLT = drug.labs ),scales = "free") 
gg <- gg + labs(x=NULL, y=NULL, title="580Y Frequency",
                subtitle="Expected 580Y frequency at year 10 for 50% treatment coverage")
gg <- gg + theme_bw(base_size = 24)
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(axis.title.x= element_blank())
gg <- gg + theme(axis.ticks.x= element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", size=36))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=22, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
gg + scale_y_continuous(trans = "log2", breaks = c( 0.01, 0.05, 0.2, 0.5, 1))


data_text <- data.frame(
  FLT   = factor(c("ACT", "ACT", "ACT", "ACT", "ASAQ", "ASAQ", "ASAQ", "ASAQ", "AL", "AL", "AL", "AL" ,
            "ACT", "ACT", "ACT", "ACT", "ASAQ", "ASAQ", "ASAQ", "ASAQ", "AL", "AL", "AL", "AL" ,
            "ACT", "ACT", "ACT", "ACT", "ASAQ", "ASAQ", "ASAQ", "ASAQ", "AL", "AL", "AL", "AL" ), 
            levels = c("ACT", "ASAQ", "AL")),
  PfPrevalence = c(0.1,0.1,0.1,0.1, 0.1,0.1,0.1,0.1, 0.1,0.1,0.1,0.1,
                   1,1,1,1, 1,1,1,1, 1,1,1,1,
                   10,10,10,10, 10,10,10,10, 10,10,10,10),
  x = c(1.6, 2.4, 2.6, 3.4, 1.6, 2.4, 2.6, 3.4, 1.6, 2.4, 2.6, 3.4,
        1.6, 2.4, 2.6, 3.4, 1.6, 2.4, 2.6, 3.4, 1.6, 2.4, 2.6, 3.4,
        1.6, 2.4, 2.6, 3.4, 1.6, 2.4, 2.6, 3.4, 1.6, 2.4, 2.6, 3.4),
  y = c(0.155, 0.006, 0.14, 0.006, 0.16, 0.02, 0.17, 0.033, 0.18, 0.006, 0.135, 0.0055,
        0.165, 0.058, 0.155, 0.017, 0.17, 0.048, 0.145, 0.046, 0.16, 0.0067, 0.13, 0.005,
        0.167, 0.24, 0.16, 0.085, 0.169, 0.039, 0.16, 0.046, 0.15, 0.0092, 0.13, 0.01)
)
data_text$label <- c("-32%","-89%","-36%","-89%",
                     "-33%","-68%","-26%","-78%",
                     "+0.4%","+11%","-26%","+8.4%",
                     "-70%","-93%","-72%","-97%",
                     "-33%","-89%","-44%","-88%",
                     "-18%","-66%","-34%","-69%",
                     "-73%","-76%","-70%","-90%",
                     "-33%","-81%","-39%","-84%",
                     "-18%","-38%","-23%","-35%")

gg <- gg + theme(text = element_text(size = 30),legend.position = "none",
           axis.text.y = element_text(size=24),
           strip.background = element_rect(fill="black"), 
           strip.text = element_text(colour = 'white'))+
  scale_y_continuous(trans='log2', breaks = c(.01,.05,.1, .25, .5, 1)) +
  geom_text(data = data_text, size = 6.5,mapping = aes(x = x, y = y, label = label)) 

ggsave("Figure2.pdf", gg, width = 45, height = 40, units = 'cm', dpi = 300)


# tiff("580Y_facet_TC_50_v2.tiff", width = 45, height = 40, units = 'cm', res = 300)
# gg + theme(text = element_text(size = 30),legend.position = "none",
#            axis.text.y = element_text(size=24),
#            strip.background = element_rect(fill="black"), 
#            strip.text = element_text(colour = 'white'))+
#   scale_y_continuous(trans='log2', breaks = c(.01,.05,.1, .25, .5, 1)) +
#   geom_text(data = data_text, size = 6.5,mapping = aes(x = x, y = y, label = label)) 
# dev.off()