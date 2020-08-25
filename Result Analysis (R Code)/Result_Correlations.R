# Get the library.
install.packages("ggplot")
library(ggplot2)


# Create data for the graph.
snrENB <-  c(23.606,24.519,15.814,21.964,21.383,
        24.083,21.049,18.894)
thr<- c(155.979,62.743,54.534,274.686,
        420.440,202.336,196.440,284.397)
cap<-c(6.999,5.786,6.604,6.517,6.014,
       5.933,6.265,6.410)

data<-data.frame(snrENB,thr,cap)

cor1<-cor(x=snrENB, y=thr, method="pearson")

ggplot(data = data) + geom_point(aes(x=snrENB, y=thr))+
  stat_smooth(aes(x=snrENB, y=thr),method = "glm",fill="grey", colour="darkblue")+
  scale_y_continuous(limits = c(0,500))+ggtitle("SINR and Throughput Relationship on Average")+
  xlab("SINR (dB)") + ylab("Throughput (Mbps)")


cor2<-cor(x=snrENB, y=cap, method="pearson")

ggplot(data = data) + geom_point(aes(x=snrENB, y=cap))+
  stat_smooth(aes(x=snrENB, y=cap),method = "glm",fill="grey", colour="darkred")+
  scale_y_continuous(limits = c(0,10))+ggtitle("SINR and Capacity Load Relationship on Average")+
  xlab("SINR (dB)") + ylab("Capacity Load (Kbit-iter/s)")

cor3<-cor(x=cap, y=thr, method="pearson")

ggplot(data = data) + geom_point(aes(x=cap, y=thr))+
  stat_smooth(aes(x=cap, y=thr),method = "glm",fill="grey", colour="chartreuse")+
  scale_y_continuous(limits = c(0,600))+ggtitle("Capacity Load and Threshold Relationship on Average")+
  xlab("Capacity Load (Kbit-iter/s)") + ylab("Throughput (Mbps)")




