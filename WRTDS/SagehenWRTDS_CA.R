require(dataRetrieval)
require(EGRET)
require(ggplot2)
require(lubridate)
require(tidyr)
require(dplyr)

Daily<-readNWISDaily("10343500", "00060", "2000-10-01", "2020-09-30")

Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen/Data", "SagehenWRTDS_CA.csv", )

Info<-readNWISInfo("10343500", "")

Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen/"

saveResults(savePath, eList)

load("/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen/SAGE.CA.RData")

eList<-modelEstimation(eList)

pdf("SagehenWRTDS_CA.pdf")

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

SageCont<-eList$Daily

ggplot(SageCont, aes(x=Date))+geom_line(aes(y=Q), color="blue")+
  geom_line(aes(y=ConcDay / 2), color="red")+scale_y_continuous(sec.axis = sec_axis(trans = ~ . *2, 
                                                                                  name = "Calcium (mg/L)"))+
  ylab("Discharge (m^3/s)")

SageCont <-  SageCont %>%
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

aggM<-aggregate(SageCont[,c(2, 5, 16)], by=list(SageCont$CDate), FUN = mean)

names(aggM)[1]<-"Date"

ggcols<-c("Calcium"="red", "Discharge"="blue")

ggplot(aggM, aes(x=Date))+geom_line(aes(y=Q, col="Discharge"))+geom_line(aes(y=ConcDay / 10, col="Calcium"))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . *10, name = "Calcium (mg/L)"))+labs(y="Discharge (cms)", color="")+
  theme_classic()+theme(legend.position = "bottom", text = element_text(size = 20))+scale_x_date(date_labels = "%b %d")+
  scale_color_manual(values = ggcols)

dev.off()

write.csv(SageCont, "SagehenContCA.csv")
