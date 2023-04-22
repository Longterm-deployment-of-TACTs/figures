library(hrbrthemes)
library(tidyverse)
library(tidyquant)
library(ggalt)
library(ggpattern)
library(ggplot2)
library(unikn)
library(ggpubr)
library(epiR)

percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}
setwd("C:/moru_psu_dtact_comparion")


moru_a1<-read.table("A1_MORU.txt",header = TRUE) 
psu_a1 <-read.table("A1_PSU.txt",header = TRUE) 


moru_a1_tf <- moru_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TFR_05,TFR_50,TFR_95,TFR_25,TFR_75, 
      FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5)
moru_a1_tf$P_TACT <-as.factor(moru_a1_tf$P_TACT)
levels(moru_a1_tf$P_TACT)<-c("ACT","ALAQ","ASMQ-PPQ")



#### A1 per month
moru_a123<-read.csv("A123_580Y_monthly_MORU_20220623124913.csv", header = TRUE)

######   ALAQ   ##############################################
sample_dp_y2<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,P_BASELINE=="DP",P_TACT=="DP")

sample_alaq_y2<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,P_BASELINE=="DP",P_TACT=="ALAQ")

N<-1000
aux1<-sample(sample_dp_y2$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y2$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_alaq_y2<-p$massoc.summary$est[1]
min_580_alaq_y2<-p$massoc.summary$lower[1]
max_580_alaq_y2<-p$massoc.summary$upper[1]

sample_dp_y5<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,P_BASELINE=="DP",P_TACT=="DP")

sample_alaq_y5<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,P_BASELINE=="DP",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y5$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y5$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_alaq_y5<-p$massoc.summary$est[1]
min_580_alaq_y5<-p$massoc.summary$lower[1]
max_580_alaq_y5<-p$massoc.summary$upper[1]


sample_dp_y10<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,P_BASELINE=="DP",P_TACT=="DP")

sample_alaq_y10<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,P_BASELINE=="DP",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y10$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y10$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_alaq_y10<-p$massoc.summary$est[1]
min_580_alaq_y10<-p$massoc.summary$lower[1]
max_580_alaq_y10<-p$massoc.summary$upper[1]


######   ASMQ-PPQ  ##############################################
sample_asmq_y2<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,P_BASELINE=="DP",P_TACT=="ASMQ_PPQ") 

sample_asmq_y5<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,P_BASELINE=="DP",P_TACT=="ASMQ_PPQ") 

sample_asmq_y10<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,P_BASELINE=="DP",P_TACT=="ASMQ_PPQ") 

aux1<-sample(sample_dp_y2$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y2$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_asmq_y2<-p$massoc.summary$est[1]
min_580_asmq_y2<-p$massoc.summary$lower[1]
max_580_asmq_y2<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y5$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y5$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_asmq_y5<-p$massoc.summary$est[1]
min_580_asmq_y5<-p$massoc.summary$lower[1]
max_580_asmq_y5<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y10$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y10$X580Y,size=N,replace = T)
rt10<-aux2/aux1
quantile(rt10, probs = c(0.125,0.5,0.875))
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_asmq_y10<-p$massoc.summary$est[1]
min_580_asmq_y10<-p$massoc.summary$lower[1]
max_580_asmq_y10<-p$massoc.summary$upper[1]


#### TFR  per month
moru_a123<-read.csv("A123_TFR_monthly_MORU_20220726215107.csv",header = TRUE) 

######   ALAQ   ##############################################
sample_dp_y2<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,P_BASELINE=="DP",P_TACT=="DP")

sample_alaq_y2<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,P_BASELINE=="DP",P_TACT=="ALAQ") 

N<-1000
aux1<-sample(sample_dp_y2$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y2$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_alaq_y2<-p$massoc.summary$est[1]
min_tf_alaq_y2<-p$massoc.summary$lower[1]
max_tf_alaq_y2<-p$massoc.summary$upper[1]

sample_dp_y5<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,P_BASELINE=="DP",P_TACT=="DP")

sample_alaq_y5<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,P_BASELINE=="DP",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y5$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y5$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_alaq_y5<-p$massoc.summary$est[1]
min_tf_alaq_y5<-p$massoc.summary$lower[1]
max_tf_alaq_y5<-p$massoc.summary$upper[1]


sample_dp_y10<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,P_BASELINE=="DP",P_TACT=="DP")

sample_alaq_y10<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,P_BASELINE=="DP",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y10$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y10$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_alaq_y10<-p$massoc.summary$est[1]
min_tf_alaq_y10<-p$massoc.summary$lower[1]
max_tf_alaq_y10<-p$massoc.summary$upper[1]


######   ASMQ-PPQ  ##############################################
sample_asmq_y2<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,P_BASELINE=="DP",P_TACT=="ASMQ_PPQ") 

sample_asmq_y5<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,P_BASELINE=="DP",P_TACT=="ASMQ_PPQ") 

sample_asmq_y10<-moru_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,P_BASELINE=="DP",P_TACT=="ASMQ_PPQ") 

aux1<-sample(sample_dp_y2$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y2$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_asmq_y2<-p$massoc.summary$est[1]
min_tf_asmq_y2<-p$massoc.summary$lower[1]
max_tf_asmq_y2<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y5$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y5$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_asmq_y5<-p$massoc.summary$est[1]
min_tf_asmq_y5<-p$massoc.summary$lower[1]
max_tf_asmq_y5<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y10$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y10$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_asmq_y10<-p$massoc.summary$est[1]
min_tf_asmq_y10<-p$massoc.summary$lower[1]
max_tf_asmq_y10<-p$massoc.summary$upper[1]


data <- 
  structure(list(
    mean  = c(mean_tf_alaq_y2,mean_tf_alaq_y5,mean_tf_alaq_y10,mean_tf_asmq_y2,mean_tf_asmq_y5,mean_tf_asmq_y10), 
    lower = c(min_tf_alaq_y2,min_tf_alaq_y5,min_tf_alaq_y10,min_tf_asmq_y2,min_tf_asmq_y5,min_tf_asmq_y10),
    upper = c(max_tf_alaq_y2,max_tf_alaq_y5,max_tf_alaq_y10,max_tf_asmq_y2,max_tf_asmq_y5,max_tf_asmq_y10)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -6L), 
    class = "data.frame")
data<-cbind(Year=rep(c("Year 2","Year 5","Year 10"),2),data)
data<-cbind(Model=rep("MORU",6),TACT=c(rep("ALAQ",3),rep("ASMQ-PPQ",3)),data)
data$Year<-factor(data$Year,levels = c("Year 10","Year 5","Year 2"))
data$Outcome<-rep("Treatment Failure",6)
p1 = ggplot(data=data,
            aes(x = Year,y = mean, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=TACT),position = position_dodge(width=0.5),size=.9)+
  geom_hline(aes(fill=TACT),yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=TACT),width=0.5,cex=.9,position = position_dodge(width=0.5))+
  scale_color_manual(name = "TACT: ",
                     values = c("ALAQ" = "#075384", "ASMQ-PPQ" = "#CF4300"),
                     labels = c("ALAQ", "ASMQ-PPQ"))+  
  labs(x='Year', y="Relative risk (95% Confidence Interval)", title = "")+
  theme_bw(base_size = 18)+
  theme(
    legend.position="none",
    plot.title=element_text(size=16,face="bold"),
    strip.background = element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x=element_text(face="bold"),
    axis.title.y=element_blank(),
    strip.text.y = element_text(hjust=0,vjust = 1,angle=90,face="bold"))+
  coord_flip()
p1


data2 <- 
  structure(list(
    mean  = c(mean_580_alaq_y2,mean_580_alaq_y5,mean_580_alaq_y10,mean_580_asmq_y2,mean_580_asmq_y5,mean_580_asmq_y10), 
    lower = c(min_580_alaq_y2,min_580_alaq_y5,min_580_alaq_y10,min_580_asmq_y2,min_580_asmq_y5,min_580_asmq_y10),
    upper = c(max_580_alaq_y2,max_580_alaq_y5,max_580_alaq_y10,max_580_asmq_y2,max_580_asmq_y5,max_580_asmq_y10)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -6L), 
    class = "data.frame")
data2<-cbind(Year=rep(c("Year 2","Year 5","Year 10"),2),data2)
data2<-cbind(Model=rep("MORU",6),TACT=c(rep("ALAQ",3),rep("ASMQ-PPQ",3)),data2)
data2$Year<-factor(data2$Year,levels = c("Year 10","Year 5","Year 2"))
data2$Outcome<-rep("580Y Frequency",6)
dataf1<-rbind(data,data2)
p2 = ggplot(data=data2,
            aes(x = Year,y = mean, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=TACT),position = position_dodge(width=0.5),size=.9)+
  geom_hline(aes(fill=TACT),yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=TACT),width=0.5,cex=.9,position = position_dodge(width=0.5))+
  scale_color_manual(name = "TACT: ",
                     values = c("ALAQ" = "#075384", "ASMQ-PPQ" = "#CF4300"),
                     labels = c("ALAQ", "ASMQ-PPQ"))+  
  labs(x='Year', y="Risk Ratio (approximate 95% Confidence Interval)", title = "")+
  theme_bw(base_size = 18)+
  theme(
    legend.position="none",
    plot.title=element_text(size=16,face="bold"),
    strip.background = element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x=element_text(face="bold"),
    axis.title.y=element_blank(),
    strip.text.y = element_text(hjust=0,vjust = 1,angle=90,face="bold"))+
  coord_flip()
p2

####################   PSU   #######################################################################

psu_a1_tf <- psu_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TF_05,TF_50,TF_95,TF_25,TF_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5)
psu_a1_tf$P_TACT <-as.factor(psu_a1_tf$P_TACT)
levels(psu_a1_tf$P_TACT)<-c("ACT","ALAQ","ASMQ-PPQ")


psu_a123<-read.csv("A123_580Y_TFR_monthly_20220728.csv",header = T) 

######   ALAQ   ##############################################
sample_dp_y2<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,BASELINE=="DHA-PPQ",P_TACT=="DHA-PPQ")
sample_alaq_y2<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,BASELINE=="DHA-PPQ",P_TACT=="ALAQ") 

N<-1000
aux1<-sample(sample_dp_y2$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y2$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_alaq_y2<-p$massoc.summary$est[1]
min_580_alaq_y2<-p$massoc.summary$lower[1]
max_580_alaq_y2<-p$massoc.summary$upper[1]

sample_dp_y5<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,BASELINE=="DHA-PPQ",P_TACT=="DHA-PPQ")

sample_alaq_y5<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,BASELINE=="DHA-PPQ",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y5$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y5$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_alaq_y5<-p$massoc.summary$est[1]
min_580_alaq_y5<-p$massoc.summary$lower[1]
max_580_alaq_y5<-p$massoc.summary$upper[1]


sample_dp_y10<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,BASELINE=="DHA-PPQ",P_TACT=="DHA-PPQ")

sample_alaq_y10<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,BASELINE=="DHA-PPQ",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y10$X580Y,size=N,replace = T)
aux2<-sample(sample_alaq_y10$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_alaq_y10<-p$massoc.summary$est[1]
min_580_alaq_y10<-p$massoc.summary$lower[1]
max_580_alaq_y10<-p$massoc.summary$upper[1]


######   ASMQ-PPQ  ##############################################
sample_asmq_y2<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,BASELINE=="DHA-PPQ",P_TACT=="ASMQ_PPQ") 

sample_asmq_y5<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,BASELINE=="DHA-PPQ",P_TACT=="ASMQ_PPQ") 

sample_asmq_y10<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,BASELINE=="DHA-PPQ",P_TACT=="ASMQ_PPQ") 

aux1<-sample(sample_dp_y2$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y2$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_asmq_y2<-p$massoc.summary$est[1]
min_580_asmq_y2<-p$massoc.summary$lower[1]
max_580_asmq_y2<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y5$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y5$X580Y,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_asmq_y5<-p$massoc.summary$est[1]
min_580_asmq_y5<-p$massoc.summary$lower[1]
max_580_asmq_y5<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y10$X580Y,size=N,replace = T)
aux2<-sample(sample_asmq_y10$X580Y,size=N,replace = T)
rt10<-aux2/aux1
quantile(rt10, probs = c(0.125,0.5,0.875))
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_580_asmq_y10<-p$massoc.summary$est[1]
min_580_asmq_y10<-p$massoc.summary$lower[1]
max_580_asmq_y10<-p$massoc.summary$upper[1]


####               TFR  per month

########   ALAQ   ##############################################
sample_dp_y2<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,BASELINE=="DHA-PPQ",P_TACT=="DHA-PPQ")

sample_alaq_y2<-psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,BASELINE=="DHA-PPQ",P_TACT=="ALAQ") 

N<-1000
aux1<-sample(sample_dp_y2$TFR,size=N,replace = T)
aux2<-sample(sample_alaq_y2$TFR,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_alaq_y2<-p$massoc.summary$est[1]
min_tf_alaq_y2<-p$massoc.summary$lower[1]
max_tf_alaq_y2<-p$massoc.summary$upper[1]

sample_dp_y5<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,BASELINE=="DHA-PPQ",P_TACT=="DHA-PPQ")

sample_alaq_y5<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,BASELINE=="DHA-PPQ",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y5$TFR,size=N,replace = T)
aux2<-sample(sample_alaq_y5$TFR,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_alaq_y5<-p$massoc.summary$est[1]
min_tf_alaq_y5<-p$massoc.summary$lower[1]
max_tf_alaq_y5<-p$massoc.summary$upper[1]


sample_dp_y10<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,BASELINE=="DHA-PPQ",P_TACT=="DHA-PPQ")

sample_alaq_y10<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,BASELINE=="DHA-PPQ",P_TACT=="ALAQ") 

aux1<-sample(sample_dp_y10$TFR,size=N,replace = T)
aux2<-sample(sample_alaq_y10$TFR,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_alaq_y10<-p$massoc.summary$est[1]
min_tf_alaq_y10<-p$massoc.summary$lower[1]
max_tf_alaq_y10<-p$massoc.summary$upper[1]


######   ASMQ-PPQ  ##############################################
sample_asmq_y2<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==24,BASELINE=="DHA-PPQ",P_TACT=="ASMQ_PPQ") 

sample_asmq_y5<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==60,BASELINE=="DHA-PPQ",P_TACT=="ASMQ_PPQ") 

sample_asmq_y10<- psu_a123 %>%
  filter(P_TC==0.5,P_PFPR==0.01,MONTH==120,BASELINE=="DHA-PPQ",P_TACT=="ASMQ_PPQ") 

aux1<-sample(sample_dp_y2$TFR,size=N,replace = T)
aux2<-sample(sample_asmq_y2$TFR,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_asmq_y2<-p$massoc.summary$est[1]
min_tf_asmq_y2<-p$massoc.summary$lower[1]
max_tf_asmq_y2<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y5$TFR,size=N,replace = T)
aux2<-sample(sample_asmq_y5$TFR,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_asmq_y5<-p$massoc.summary$est[1]
min_tf_asmq_y5<-p$massoc.summary$lower[1]
max_tf_asmq_y5<-p$massoc.summary$upper[1]


aux1<-sample(sample_dp_y10$TFR,size=N,replace = T)
aux2<-sample(sample_asmq_y10$TFR,size=N,replace = T)
pos2<-sum(runif(N)<aux2)
pos1<-sum(runif(N)<aux1)
tab <- matrix(c(pos2,N-pos2,pos1,N-pos1),byrow=TRUE,nrow=2)
p<-epi.2by2(tab,method = "cohort.count", conf.level = 0.95)
mean_tf_asmq_y10<-p$massoc.summary$est[1]
min_tf_asmq_y10<-p$massoc.summary$lower[1]
max_tf_asmq_y10<-p$massoc.summary$upper[1]


data <- 
  structure(list(
    mean  = c(mean_tf_alaq_y2,mean_tf_alaq_y5,mean_tf_alaq_y10,mean_tf_asmq_y2,mean_tf_asmq_y5,mean_tf_asmq_y10), 
    lower = c(min_tf_alaq_y2,min_tf_alaq_y5,min_tf_alaq_y10,min_tf_asmq_y2,min_tf_asmq_y5,min_tf_asmq_y10),
    upper = c(max_tf_alaq_y2,max_tf_alaq_y5,max_tf_alaq_y10,max_tf_asmq_y2,max_tf_asmq_y5,max_tf_asmq_y10)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -6L), 
    class = "data.frame")
data<-cbind(Year=rep(c("Year 2","Year 5","Year 10"),2),data)
data<-cbind(Model=rep("PSU",6),TACT=c(rep("ALAQ",3),rep("ASMQ-PPQ",3)),data)
data$Year<-factor(data$Year,levels = c("Year 10","Year 5","Year 2"))
data$Outcome<-rep("Treatment Failure",6)
p3 = ggplot(data=data,
            aes(x = Year,y = mean, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=TACT),position = position_dodge(width=0.5),size=.9)+
  geom_hline(aes(fill=TACT),yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=TACT),width=0.5,cex=.9,position = position_dodge(width=0.5))+
  scale_color_manual(name = "TACT: ",
                     values = c("ALAQ" = "#075384", "ASMQ-PPQ" = "#CF4300"),
                     labels = c("ALAQ", "ASMQ-PPQ"))+  
  labs(x='Year', y="Risk Ratio (approximate 95% Confidence Interval)", title = "")+
  theme_bw(base_size = 18) +
  theme(
    legend.position="none",
    plot.title=element_text(size=16,face="bold"),
    strip.background = element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x=element_text(face="bold"),
    axis.title.y=element_blank(),
    strip.text.y = element_text(hjust=0,vjust = 1,angle=90,face="bold"))+
  coord_flip()
p3


data2 <- 
  structure(list(
    mean  = c(mean_580_alaq_y2,mean_580_alaq_y5,mean_580_alaq_y10,mean_580_asmq_y2,mean_580_asmq_y5,mean_580_asmq_y10), 
    lower = c(min_580_alaq_y2,min_580_alaq_y5,min_580_alaq_y10,min_580_asmq_y2,min_580_asmq_y5,min_580_asmq_y10),
    upper = c(max_580_alaq_y2,max_580_alaq_y5,max_580_alaq_y10,max_580_asmq_y2,max_580_asmq_y5,max_580_asmq_y10)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -6L), 
    class = "data.frame")

data2<-cbind(Year=rep(c("Year 2","Year 5","Year 10"),2),data2)
data2<-cbind(Model=rep("PSU",6),TACT=c(rep("ALAQ",3),rep("ASMQ-PPQ",3)),data2)
data2$Year<-factor(data2$Year,levels = c("Year 10","Year 5","Year 2"))
data2$Outcome<-rep("580Y Frequency",6)
dataf2<-rbind(data,data2)
p4 = ggplot(data=data2,
            aes(x = Year,y = mean, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=TACT),position = position_dodge(width=0.5),size=.9)+
  geom_hline(aes(fill=TACT),yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=TACT),width=0.5,cex=.9,position = position_dodge(width=0.5))+
  scale_color_manual(name = "TACT: ",
                     values = c("ALAQ" = "#075384", "ASMQ-PPQ" = "#CF4300"),
                     labels = c("ALAQ", "ASMQ-PPQ"))+  
  labs(x='Year', y="Risk Ratio (approximate 95% Confidence Interval)", title = "")+
  theme_bw(base_size = 18)+
  theme(
    legend.position="none",
    plot.title=element_text(size=16,face="bold"),
    strip.background = element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x=element_text(face="bold"),
    axis.title.y=element_blank(),
    strip.text.y = element_text(hjust=0,vjust = 1,angle=90,face="bold"))+
  coord_flip()
p4



mycols<-c("grey40" , "#075384" ,"#CF4300")
gg <- ggplot(moru_a1_tf, aes(color = P_TACT, fill=P_TACT))
gg <- gg + geom_line(aes(x=MONTH,y=TFR_50*100,
                         color=(P_TACT)),stat = "identity",size=1.5)+
  geom_ribbon(aes(x=MONTH,ymin = TFR_25*100, ymax = TFR_75*100),
              alpha=0.35) +
  scale_color_manual(values=mycols,name="")+
  scale_fill_manual(values=mycols,name="")+
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))
gg <- gg + labs(x="Year", y="Treatment failure rate (%)", title = "MORU")
gg <- gg + theme_bw(base_size = 18)
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(axis.ticks.x= element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", size=32))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=22, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
gg


moru_a1_act <- moru_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TFR_05,TFR_50,TFR_95,TFR_25,TFR_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5, P_TACT==0)
moru_a1_alaq <- moru_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TFR_05,TFR_50,TFR_95,TFR_25,TFR_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5, P_TACT==1)
moru_a1_asmq<- moru_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TFR_05,TFR_50,TFR_95,TFR_25,TFR_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5, P_TACT==2)
moru_a1_diff<-as.data.frame(cbind(moru_a1_alaq$MONTH,moru_a1_act$TFR_50,moru_a1_act$FREQ_580Y_50,
                                  moru_a1_alaq$TFR_50,moru_a1_alaq$FREQ_580Y_50,moru_a1_asmq$TFR_50,moru_a1_asmq$FREQ_580Y_50))
colnames(moru_a1_diff)<-c("MONTH","TFTACT","ACT580Y","TFTALAQ","ALAQ580Y","TFTASMQPPQ","ASMQPPQ580Y")
comp<-as.data.frame(cbind(moru_a1_diff$MONTH,(moru_a1_diff$TFTACT-moru_a1_diff$TFTALAQ)/moru_a1_diff$TFTACT,(moru_a1_diff$TFTACT-moru_a1_diff$TFTASMQPPQ)/moru_a1_diff$TFTACT))
colnames(comp)<-c("Month","ALAQ","ASMQ_PPQ")

t2<- ggplot(comp, aes( x=Month,y=ALAQ))+geom_line(color=mycols[2],size=1.5) +
  geom_line(aes( x=Month,y=ASMQ_PPQ), color=mycols[3],size=1.5)+
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))+
  theme_bw(base_family="Calibri",base_size = 14)+labs(x="Year",y="", title = "Proportional decrease relative to ACT")

gg1<-gg 
#+ annotation_custom(ggplotGrob(t2), xmin = -10, xmax = 60, 
#                      ymin = 18, ymax = 32)


gg <- ggplot(moru_a1_tf, aes(group=P_TACT, color = P_TACT, fill=P_TACT))
gg <- gg + geom_line(aes(x=MONTH,y=FREQ_580Y_50,
                         color=(P_TACT)),stat = "identity",size=1.5)+
  geom_ribbon(aes(x=MONTH,ymin = FREQ_580Y_25, ymax = FREQ_580Y_75), 
              alpha=0.35) +
  scale_color_manual(values=mycols,name="")+
  scale_fill_manual(values=mycols,name="")+
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))
gg <- gg + labs(x="Year", y="580Y Frequency", title = "MORU")+ coord_cartesian(ylim = c(0,.60))
gg <- gg + theme_bw(base_size = 18)
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(axis.ticks.x= element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", size=32))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=22, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))

comp<-as.data.frame(cbind(moru_a1_diff$MONTH,(moru_a1_diff$ACT580Y-moru_a1_diff$ALAQ580Y)/moru_a1_diff$ACT580Y,(moru_a1_diff$ACT580Y-moru_a1_diff$ASMQPPQ580Y)/moru_a1_diff$ACT580Y))
colnames(comp)<-c("Month","ALAQ","ASMQ_PPQ")

t2<- ggplot(comp, aes( x=Month,y=ALAQ))+geom_line(color=mycols[2],size=1.5) +
  geom_line(aes( x=Month,y=ASMQ_PPQ), color=mycols[3],size=1.5)+
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))+
  theme_bw(base_family="Calibri",base_size = 14)+labs(x="Year",y="", title = "Proportional decrease relative to ACT")
gg2<-gg 
#+ annotation_custom(ggplotGrob(t2), xmin = -10, xmax = 60, 
#                      ymin = .41, ymax = .74)

# tiff("try1.tiff", width = 40, height = 20, units = 'cm', res = 300)
# p1<-ggarrange(gg1, gg2+labs(title=""), ncol=2, nrow=1, common.legend = TRUE, legend="right")
# dev.off()
# 


psu_a1_tf <- psu_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TF_05,TF_50,TF_95,TF_25,TF_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5)
psu_a1_tf$P_TACT <-as.factor(psu_a1_tf$P_TACT)
levels(psu_a1_tf$P_TACT)<-c("ACT","ALAQ","ASMQ-PPQ")

mycols<-c("grey40","#075384", "#CF4300")
gg <- ggplot(psu_a1_tf, aes( group=P_TACT, color = P_TACT, fill=P_TACT))
gg <- gg + geom_line(aes(x=MONTH,y=TF_50,
                         color=(P_TACT)),stat = "identity",size=1.5)+
  geom_ribbon(aes(x=MONTH,ymin = TF_25, ymax = TF_75), 
              alpha=0.35) +
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))+
  scale_color_manual(values=mycols,name="")+
  scale_fill_manual(values=mycols,name="")
gg <- gg + labs(x="Year", y="Treatment failure rate (%)", title = "PSU")+
      coord_cartesian(ylim = c(9.85,53))
gg <- gg + theme_bw(base_size = 18)
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(axis.ticks.x= element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", size=32))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=22, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
# gg


psu_a1_act <- psu_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TF_05,TF_50,TF_95,TF_25,TF_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5, P_TACT==0)
psu_a1_alaq <- psu_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TF_05,TF_50,TF_95,TF_25,TF_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5, P_TACT==1)
psu_a1_asmq<- psu_a1 %>% 
  select(MONTH,P_TC,P_PFPR,P_TACT,TF_05,TF_50,TF_95,TF_25,TF_75, FREQ_580Y_05,FREQ_580Y_50,FREQ_580Y_95,FREQ_580Y_25,FREQ_580Y_75) %>%
  arrange(MONTH, P_TC) %>%
  filter(P_PFPR==1, P_TC==0.5, P_TACT==2)
psu_a1_diff<-as.data.frame(cbind(psu_a1_alaq$MONTH,psu_a1_act$TF_50,psu_a1_act$FREQ_580Y_50,
                                 psu_a1_alaq$TF_50,psu_a1_alaq$FREQ_580Y_50,psu_a1_asmq$TF_50,psu_a1_asmq$FREQ_580Y_50))
colnames(psu_a1_diff)<-c("MONTH","TFTACT","ACT580Y","TFTALAQ","ALAQ580Y","TFTASMQPPQ","ASMQPPQ580Y")
comp2<-as.data.frame(cbind(psu_a1_diff$MONTH,(psu_a1_diff$TFTACT-psu_a1_diff$TFTALAQ)/psu_a1_diff$TFTACT,(psu_a1_diff$TFTACT-psu_a1_diff$TFTASMQPPQ)/psu_a1_diff$TFTACT))
colnames(comp2)<-c("Month","ALAQ","ASMQ_PPQ")

t2<- ggplot(comp2, aes( x=Month,y=ALAQ))+geom_line(color=mycols[2],size=1.5) +
  geom_line(aes( x=Month,y=ASMQ_PPQ), color=mycols[3],size=1.5)+
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))+
  theme_bw(base_family="Calibri",base_size = 14)+labs(x="Year",y="", title = "Proportional decrease relative to ACT")

gg3<-gg 
#+ annotation_custom(ggplotGrob(t2), xmin = -10, xmax = 60, 
#                           ymin = 35, ymax = 56)


mycols<-c("grey40","#075384", "#CF4300")
gg <- ggplot(psu_a1_tf, aes( group=P_TACT, color = P_TACT, fill=P_TACT))
gg <- gg + geom_line(aes(x=MONTH,y=FREQ_580Y_50,
                         color=(P_TACT)),stat = "identity",size=1.5)+
  geom_ribbon(aes(x=MONTH,ymin = FREQ_580Y_25, ymax = FREQ_580Y_75), 
              alpha=0.35) +
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))+
  scale_color_manual(values=mycols,name="")+
  scale_fill_manual(values=mycols,name="")
gg <- gg + labs(x="Year", y="580Y Frequency", title = "PSU")+coord_cartesian(ylim = c(0,.8))
gg <- gg + theme_bw(base_size = 18)
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(axis.ticks.x= element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", size=32))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=22, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
# gg

comp<-as.data.frame(cbind(psu_a1_diff$MONTH,(psu_a1_diff$ACT580Y-psu_a1_diff$ALAQ580Y)/psu_a1_diff$ACT580Y,(psu_a1_diff$ACT580Y-psu_a1_diff$ASMQPPQ580Y)/psu_a1_diff$ACT580Y))
colnames(comp)<-c("Month","ALAQ","ASMQ_PPQ")
t2<- ggplot(comp, aes( x=Month,y=ALAQ))+geom_line(color=mycols[2],size=1.5) +
  geom_line(aes( x=Month,y=ASMQ_PPQ), color=mycols[3],size=1.5)+
  scale_x_continuous(breaks = c(0,24,48,72,96,120),labels = c(0,2,4,6,8,10),expand = c(0,0))+
  theme_bw(base_family="Calibri",base_size = 14)+labs(x="Year",y="", title = "Proportional decrease relative to ACT")
gg4<-gg 





# tiff("Figure1_v32.tiff", width = 30, height = 35, units = 'cm', res = 300)
plot1<-ggarrange(
  p2+  labs(x='Year', y="Relative risk (95% Confidence Interval)", title = "")+scale_y_continuous(position = "right", limits = c(0,1.4)),
  p1+  labs(x='Year', y="Relative risk (95% Confidence Interval)", title = "")+scale_y_continuous(position="right", limits = c(0,1.4)),
  NULL,
  NULL,
  gg2+theme(plot.title=element_blank(),legend.position = "none"),
  gg1+theme(plot.title=element_blank(),legend.position = "none"), 
  ncol=2, nrow=3, widths = c(1, 1), heights = c(1.2, -0.08, 3),common.legend = F)
annotate_figure(plot1, top = text_grob("MORU", face = "bold", size = 28))
plot2<-ggarrange(
  p4+  labs(x='Year', y="Relative risk (95% Confidence Interval)", title = "")+scale_y_continuous(position = "right",limits = c(0,1.4)),
  p3+  labs(x='Year', y="Relative risk (95% Confidence Interval)", title = "")+scale_y_continuous(position="right",limits = c(0,1.4)),
  NULL,
  NULL,
  gg4+theme(plot.title=element_blank(),legend.position = "bottom"),
  gg3+theme(plot.title=element_blank(),legend.position = "bottom"),
  ncol=2, nrow=3, widths = c(1, 1), heights = c(1.2,-0.08,3), common.legend = T, legend="bottom")
annotate_figure(plot2, top = text_grob("PSU", face = "bold", size = 28))
gt <- ggarrange(plot1+labs(title="MORU")+theme(plot.title=element_text(face="bold",size = 24)),
          plot2+labs(title="PSU")+theme(plot.title=element_text(face="bold",size = 24),legend.position = "bottom"), nrow=2, common.legend = T)
ggsave("Figure_1.pdf", gt, width = 30, height = 35, units = "cm", dpi = 600)

# dev.off()




