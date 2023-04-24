require(reshape2)
require(ggplot2)
require(lubridate)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen/Data")

#read in output from WRTDS model
output<-read.csv("SagehenContCA.csv")
output<-output[c(2, 3, 6, 9, 17)]

output1<-output
output2<-output
output3<-output

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen")

load("SAGE.CA.RData")

#set end members
ROmin<-min(Sample$ConcAve)
RO1<-quantile(Sample$ConcAve, .01)
BFmax<-max(Sample$ConcAve)
BF99<-quantile(Sample$ConcAve, .99)

RO<-c(ROmin, RO1)
ROnames<-c("ROmin", "RO01")
BF<-c(BFmax, BF99)
BFnames<-c("BFmax", "BF99")

waterYearList<-as.integer(unique(output1$waterYear))

output1<-output

output1$ROmin<-""
output1$RO01<-""
output1$BFmax<-""
output1$BF99<-""

for (i in 1:length(waterYearList)) {
  
  oneWaterYear<-subset(output1, output1$waterYear==waterYearList[i])
  
  output1$ROmin[which(output1$waterYear %in% waterYearList[i])]<-min(oneWaterYear$ConcDay)
  output1$RO01[which(output1$waterYear %in% waterYearList[i])]<-quantile(oneWaterYear$ConcDay, 0.01)
  output1$BFmax[which(output1$waterYear %in% waterYearList[i])]<-max(oneWaterYear$ConcDay)
  output1$BF99[which(output1$waterYear %in% waterYearList[i])]<-quantile(oneWaterYear$ConcDay, 0.99)
}


output1[,c(6:9)]<-sapply(output1[,c(6:9)], as.numeric)


#output file with baseflow volume (Q) and fraction
for (i in 1:length(RO)) {
  
  QBF<-output1$Q*((output1$ConcDay-RO[i])/(output1$BFmax-RO[i]))
  
  BFPer<-QBF/output1$Q
  
  output1<-cbind(output1, QBF, BFPer)
  
  x<-which(colnames(output1)=="QBF")
  z<-which(colnames(output1)=="BFPer")
  
  names(output1)[x]<-paste("QBF", paste(ROnames[i]), paste(BFnames[1]), sep = "")
  names(output1)[z]<-paste("BFPer", paste(ROnames[i]), paste(BFnames[1]), sep = "")
  
  QBF<-output1$Q*((output1$ConcDay-RO[i])/(output1$BF99-RO[i]))
  
  BFPer<-QBF/output1$Q
  
  output1<-cbind(output1, QBF, BFPer)
  
  x<-which(colnames(output1)=="QBF")
  z<-which(colnames(output1)=="BFPer")
  
  names(output1)[x]<-paste("QBF", paste(ROnames[i]), paste(BFnames[2]), sep = "")
  names(output1)[z]<-paste("BFPer", paste(ROnames[i]), paste(BFnames[2]), sep = "")
  
}

output1$waterYear<-as.character(output1$waterYear)

output1 <-  output1 %>%
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

#write.csv(output1, "SagehenBaseflow.csv")


wateryearAGG<-aggregate(Q~CDate, output1, FUN = mean)

pdf("EndMemberAnalysisFlow_max_99AnnualBF_CA2.pdf")

#plots of individual end member outputs compared over all water years and to average total flow
ggplot()+geom_line(output1, mapping=aes(CDate, QBFROminBFmax, color=waterYear))+
  geom_line(wateryearAGG, mapping = aes(CDate, Q))+
  scale_x_date(date_labels = "%b %d")+ggtitle("QBFROminBFmax")+ylim(0, 1.5)

ggplot()+geom_line(output1, mapping=aes(CDate, QBFROminBF99, color=waterYear))+
  geom_line(wateryearAGG, mapping = aes(CDate, Q))+
  scale_x_date(date_labels = "%b %d")+ggtitle("QBFROminBF99")+ylim(0, 1.5)

ggplot()+geom_line(output1, mapping=aes(CDate, QBFRO01BFmax, color=waterYear))+
  geom_line(wateryearAGG, mapping = aes(CDate, Q))+
  scale_x_date(date_labels = "%b %d")+ggtitle("QBFRO01BFmax")+ylim(0, 1.5)

ggplot()+geom_line(output1, mapping=aes(CDate, QBFRO01BF99, color=waterYear))+
  geom_line(wateryearAGG, mapping = aes(CDate, Q))+
  scale_x_date(date_labels = "%b %d")+ggtitle("QBFRO01BF99")+ylim(0, 1.5)

output3<-output


#output file of only baseflow amount
for (i in 1:length(RO)) {
  
  QBF<-output3$Q*((output3$ConcDay-RO[i])/(output1$BFmax-RO[i]))
  
  output3<-cbind(output3, QBF)
  
  z<-which(colnames(output3)=="QBF")
  
  names(output3)[z]<-paste("QBF", paste(ROnames[i]), paste(BFnames[1]), sep = "")
  
  QBF<-output3$Q*((output3$ConcDay-RO[i])/(output1$BF99-RO[i]))
  
  output3<-cbind(output3, QBF)
  
  z<-which(colnames(output3)=="QBF")
  
  names(output3)[z]<-paste("QBF", paste(ROnames[i]), paste(BFnames[2]), sep = "")
  
}

waterYearList<-unique(output3$waterYear)

#annual plots of different end member result of baseflow Q compared to total Q
for (n in 1:length(waterYearList)) {
  
  oneYear<-subset(output3, output3$waterYear==waterYearList[[n]])
  oneYearmelt<-melt(oneYear, id=c(1:5))
  names(oneYearmelt)[6]<-"EndMember"
  names(oneYearmelt)[7]<-"ProportionBF"
  oneYearmelt$Date<-as.Date(oneYear$Date)
  p<-ggplot(oneYearmelt)+geom_line(aes(Date, ProportionBF, color=EndMember))+
    geom_line(aes(Date, Q))+ylab("Q (m^3/s)")+ggtitle(paste(waterYearList[n]))+
    theme_classic()+theme(axis.text.x=element_text(size = 20, colour = "black"),
                          axis.title.y = element_text(size = 22, colour="black"),
                          axis.text.y = element_text(size = 20, colour = "black"),
                          axis.title.x = element_text(size = 22, colour="black"))+
    scale_x_date(date_labels = "%b %d")
  print(p)
  
}

dev.off()

#final model plots

output9<-output3

output9<-output9[,c(1:5,7)]

output9$propGW<-output9$QBFROminBF99/output9$Q

min_sum<-output9 %>%
  group_by(waterYear) %>%
  slice(which.min(propGW))

max_sum<-output9 %>%
  group_by(waterYear) %>%
  slice(which.max(propGW))

(mean(max_sum$Day)/365)*12

annual_weighted_mean<-output9 %>%
  group_by(waterYear) %>%
  mutate(Q_weighted = Q/sum(Q)) %>%
  mutate(propGW_weighted = propGW*Q_weighted) %>%
  summarise(sum(propGW_weighted))

mean(annual_weighted_mean$`sum(propGW_weighted)`)

pdf("WRTDSPlots_CA_FinalModel_UPDATED.pdf", height = 10, width = 8)

#annual plots of different end member result of baseflow Q compared to total Q
for (n in 1:length(waterYearList)) {
  
  oneYear<-subset(output9, output9$waterYear==waterYearList[[n]])
  oneYearmelt<-melt(oneYear, id=c(1:5))
  names(oneYearmelt)[6]<-"EndMember"
  names(oneYearmelt)[7]<-"ProportionBF"
  oneYearmelt$Date<-as.Date(oneYear$Date)
  p<-ggplot(oneYearmelt)+geom_line(aes(Date, log(ProportionBF)), lty=2)+
    geom_line(aes(Date, log(Q)))+ylab(bquote('log(Q)'~(m^3/s)))+ggtitle(paste(waterYearList[n]))+
    theme_classic()+theme(axis.text.x=element_text(size = 25, colour = "black"),
                          axis.title.y = element_text(size = 30, colour="black"),
                          axis.text.y = element_text(size = 25, colour = "black"),
                          axis.title.x = element_text(size = 30, colour="black"),
                          plot.margin = margin(1,1,1,1, "cm"))+
    scale_x_date(date_labels = "%b %d")
  print(p)
  
}

dev.off()


output2<-output

#output file of only baseflow fraction
for (i in 1:length(RO)) {
  
  QBF<-output1$Q*((output1$ConcDay-RO[i])/(output1$BFmax-RO[i]))
  
  BFPer<-QBF/output2$Q
  
  output2<-cbind(output2, BFPer)
  
  z<-which(colnames(output2)=="BFPer")
  
  names(output2)[z]<-paste("BFPer", paste(ROnames[i]), paste(BFnames[1]), sep = "")
  
  QBF<-output1$Q*((output1$ConcDay-RO[i])/(output1$BF99-RO[i]))
  
  BFPer<-QBF/output2$Q
  
  output2<-cbind(output2, BFPer)
  
  z<-which(colnames(output2)=="BFPer")
  
  names(output2)[z]<-paste("BFPer", paste(ROnames[i]), paste(BFnames[2]), sep = "")
  
  
}

waterYearList<-unique(output2$waterYear)

pdf("EndMemberAnalysis_Max_99_CA.pdf")

for (m in 1:length(waterYearList)) {
  
  oneYear<-subset(output2, output2$waterYear==waterYearList[[m]])
  oneYearmelt<-melt(oneYear, id=c(1:5))
  names(oneYearmelt)[6]<-"EndMember"
  names(oneYearmelt)[7]<-"ProportionBF"
  oneYearmelt$Date<-as.Date(oneYear$Date)
  p<-ggplot(oneYearmelt)+geom_line(aes(Date, ProportionBF, color=EndMember))+geom_hline(yintercept = 0)+
    geom_hline(yintercept = 1)+ylab("Proportion of Baseflow")+ggtitle(paste(waterYearList[m]))
  print(p)
  
}

output2 <-  output2 %>%
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

outputAGG<-aggregate(output2, by=list(output2$CDate), FUN = mean)
outputAGG<-outputAGG[,-11]
outputAGGmelt<-melt(outputAGG, id=c(1:6))
names(outputAGGmelt)[7]<-"EndMember"
names(outputAGGmelt)[8]<-"ProportionBF"

p<-ggplot(outputAGGmelt)+geom_line(aes(Group.1, ProportionBF, color=EndMember))+geom_hline(yintercept = 0)+
  geom_hline(yintercept = 1)+ylab("Proportion of Baseflow")+ggtitle("Daily Mean (aggregated over all years)")+
  scale_x_date(date_labels = "%b %d")
print(p)

dev.off()


##C-Q HJA

pdf("Sagehen_C_Q_CA_EndMemberAnalysis.pdf")

Q<-Daily[c(1,2)]
total<-merge(Sample, Q, by="Date")

ROdata<-subset(total, total$ConcAve <= quantile(total$ConcAve, 0.05))

#compare RO C-Q values to all C-Q values
ggplot()+geom_point(total, mapping = aes(Q, ConcAve), color="blue")+
  geom_point(ROdata, mapping = aes(Q, ConcAve), color="red")+ggtitle("5th Percentile Discrete Calcium (RO)")

summary(ROdata$ConcAve)

BFdata<-subset(total, total$ConcAve >= quantile(total$ConcAve, 0.95))

ggplot()+geom_point(total, mapping = aes(Q, ConcAve), color="blue")+
  geom_point(BFdata, mapping = aes(Q, ConcAve), color="red")+ggtitle("95th Percentile Discrete Calcium (BF)")

#ggplot()+geom_point(total, mapping = aes(ConcAve, Q), color="blue")+
#geom_point(BFdata, mapping = aes(ConcAve, Q), color="red")+ggtitle("95th Percentile Discrete Specific Conductance")

summary(BFdata$ConcAve)
summary(BFdata$Q)

dev.off()

