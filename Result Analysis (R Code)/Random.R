if(!require("csv"))
  install.packages("csv")
if(!require("ggplot2"))
  install.packages("ggplot2")
if(!require("gtable"))
  install.packages("gtable")
library("csv")
library("ggplot2")
if(!require("reshape2"))
  install.packages("reshape2")
library("reshape2")
#Descargamos DescTools
if (!("DescTools" %in% row.names(installed.packages()))) install.packages("DescTools")
library("DescTools")
library("gtable")
library(readr)
install.packages("RColorBrewer")
library("RColorBrewer")

Random_Capacity_7000 <- read_csv("Random 2 Capacity 7000.csv")
View(Random_Capacity_7000)
names (Random_Capacity_7000) = c("Time","MCS","Time","RCVD_SINR_ENB","Time","Capacity","Time","Server_throughput")

throughput<-Random_Capacity_7000[1:1915,7:8]
snrENB<-Random_Capacity_7000[1:246784,3:4]
capacity<-Random_Capacity_7000[,5:6]
mcs<-Random_Capacity_7000[,1:2]

FFTSmooth <- function(formula, data, weights, n = 0.5) {
  f <- fft(data$y)
  keep <- c(0, seq_len(floor(length(f)/2*n)))
  keep <- c(keep + 1, length(f) - keep[keep != 0] + 1)
  f[-keep] <- 0 + 0i
  
  model <- list(x = data$x, pred = Re(fft(f, inverse = T))/length(f))
  class(model) <- "my_smooth"
  return(model)
}

predictdf.my_smooth <- function(model, xseq, se, level) {
  data.frame(x = model$x, y = model$pred)
}

snrENBFINAL<-snrENB[which(snrENB$Time%%1==0),]
throughputFINAL<-throughput[which(throughput$Time%%1==0),]
capacityFINAL<-capacity[which(capacity$Time%%1==0),]
mcsFINAL<-mcs[which(mcs$Time%%1==0),]
mcsFINAL$MCS<-as.integer(mcsFINAL$MCS)

tam1<-dim(capacityFINAL)
tam2<-dim(mcsFINAL)

vector<-setdiff(capacityFINAL$Time, mcsFINAL$Time)
vector2<-setdiff(mcsFINAL$Time, capacityFINAL$Time)
vector3<-sort(as.numeric(rbind(vector,vector2)))
vector3<-vector3[!duplicated(vector3)]
mcsFINAL$MCS<-as.factor(mcsFINAL$MCS)


for(i in 1:tam1[1]){
  for(j in 1:length(vector3)){
    if(capacityFINAL[i,1]==vector3[j]){
      capacityFINAL<-capacityFINAL[-i,]
    }
  }
}

for(i in 1:tam2[1]){
  for(j in 1:length(vector3)){
    if(mcsFINAL[i,1]==vector3[j]){
      mcsFINAL<-mcsFINAL[-i,]
    }
  }
}

cap_mcs_FINAL<-cbind(mcsFINAL,capacityFINAL)
cap_mcs_FINAL<-cap_mcs_FINAL[,-3]
cap_mcs_FINAL$Capacity<-cap_mcs_FINAL$Capacity/1000


tam_snr<-dim(snrENBFINAL)
mean_snr<-sum(snrENBFINAL$RCVD_SINR_ENB)/tam_snr[1]
median_snr<-Median(snrENBFINAL$RCVD_SINR_ENB)
stddev_snr<-sd(snrENBFINAL$RCVD_SINR_ENB)

conf_intervals<-c(1.645,1.960,2.576)
conf_bound_snr<-c(0,0,0,0,0,0)

conf_bound_snr[1]<-mean_snr-stddev_snr/sqrt(tam_snr[1])*conf_intervals[1]/1000
conf_bound_snr[2]<-mean_snr+stddev_snr/sqrt(tam_snr[1])*conf_intervals[1]/1000
conf_bound_snr[3]<-mean_snr-stddev_snr/sqrt(tam_snr[1])*conf_intervals[2]/1000
conf_bound_snr[4]<-mean_snr+stddev_snr/sqrt(tam_snr[1])*conf_intervals[2]/1000
conf_bound_snr[5]<-mean_snr-stddev_snr/sqrt(tam_snr[1])*conf_intervals[3]/1000
conf_bound_snr[6]<-mean_snr+stddev_snr/sqrt(tam_snr[1])*conf_intervals[3]/1000

tam_cap<-dim(capacityFINAL)
mean_cap<-sum(capacityFINAL$Capacity)/tam_cap[1]
median_cap<-Median(capacityFINAL$Capacity)
stddev_cap<-sd(capacityFINAL$Capacity)
conf_bound_cap<-c(0,0,0,0,0,0)

conf_bound_cap[1]<-mean_cap-stddev_cap/sqrt(tam_cap[1])*conf_intervals[1]
conf_bound_cap[2]<-mean_cap+stddev_cap/sqrt(tam_cap[1])*conf_intervals[1]
conf_bound_cap[3]<-mean_cap-stddev_cap/sqrt(tam_cap[1])*conf_intervals[2]
conf_bound_cap[4]<-mean_cap+stddev_cap/sqrt(tam_cap[1])*conf_intervals[2]
conf_bound_cap[5]<-mean_cap-stddev_cap/sqrt(tam_cap[1])*conf_intervals[3]
conf_bound_cap[6]<-mean_cap+stddev_cap/sqrt(tam_cap[1])*conf_intervals[3]

tam_thr<-dim(throughputFINAL)
mean_thr<-sum(throughputFINAL$Server_throughput)/tam_thr[1]
median_thr<-Median(throughputFINAL$Server_throughput)
stddev_thr<-sd(throughputFINAL$Server_throughput)
conf_bound_thr<-c(0,0,0,0,0,0)

conf_intervals<-c(1.645,1.960,2.576)

conf_bound_thr[1]<-mean_thr-stddev_thr/sqrt(tam_thr[1])*conf_intervals[1]
conf_bound_thr[2]<-mean_thr+stddev_thr/sqrt(tam_thr[1])*conf_intervals[1]
conf_bound_thr[3]<-mean_thr-stddev_thr/sqrt(tam_thr[1])*conf_intervals[2]
conf_bound_thr[4]<-mean_thr+stddev_thr/sqrt(tam_thr[1])*conf_intervals[2]
conf_bound_thr[5]<-mean_thr-stddev_thr/sqrt(tam_thr[1])*conf_intervals[3]
conf_bound_thr[6]<-mean_thr+stddev_thr/sqrt(tam_thr[1])*conf_intervals[3]

throughputFINAL$Server_throughput<-throughputFINAL$Server_throughput/1000

p1<-ggplot(data=throughputFINAL, aes(x=Time, y=Server_throughput, group=1)) +
  geom_smooth(method = "FFTSmooth", method.args = list(n = 0.2),color = "green", size=2)+stat_smooth(color="black")+
  geom_point(color="blue")+scale_color_brewer(palette="Dark2")+
  labs(x = "Time [s]",y = "Throughput [KBps]")+
  ggtitle ("Throughput values-Random Movement")+
  scale_y_continuous(trans = "log10")

p2<-ggplot(data=snrENBFINAL, aes(x=Time, y=RCVD_SINR_ENB, group=2)) +
  geom_smooth(method = "FFTSmooth", method.args = list(n = 0.2),color = "blue", size=2)+stat_smooth(color="black")+
  geom_point(color="orange")+scale_color_brewer(palette="Dark2")+
  labs(x = "Time [s]",y = "SINR [dB]")+
  ggtitle ("SINR measured at the eNB-Random Movement")

nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

p3<-ggplot(data=cap_mcs_FINAL, aes(x=Time, y=Capacity, color=MCS, group=1)) +
  geom_smooth(method = "FFTSmooth", method.args = list(n = 0.2),color = "darkblue", size=1)+stat_smooth(color="burlywood4")+
  geom_point()+geom_hline(yintercept=7, size=2,linetype="dashed")+
  labs(x = "Time [s]",y = "Comp. Capacity [Kbit-iter/s prb]")+scale_y_continuous( breaks = seq(0,10,1), limits=c(0,10))+
  ggtitle ("Computational Capacity-Random Movement")+scale_fill_manual(values = mycolors)+  
  annotate("text", x=8, y=13000, label= "Dotted line at 7000 outlines the eNB capacity limit")
