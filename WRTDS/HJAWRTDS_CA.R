require(dataRetrieval)
require(EGRET)
require(ggplot2)
require(lubridate)
require(tidyr)
require(dplyr)

Daily<-readNWISDaily("14161500", "00060", startDate = "2005-01-01", endDate = "2019-09-30")

Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/HJ_Andrews/Data", "HJAWRTDS_CA.csv")

Info<-readNWISInfo("14161500", "")

Sample <- removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/HJ_Andrews/"

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data")

saveResults(savePath, eList)

eList<-modelEstimation(eList)

load("/Users/keirajohnson/Box Sync/Keira_Johnson/HJ_Andrews/LOOK.CA.RData")

pdf("HJAWRTDS_Ca.pdf")

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

LookoutCreekCont<-eList$Daily

ggplot(LookoutCreekCont, aes(x=Date))+geom_line(aes(y=Q), color="blue")+
  geom_line(aes(y=ConcDay*10), color="red")+scale_y_continuous(sec.axis = sec_axis(trans = ~ . /10, 
                                                                                  name = "Calcium (mg/L)"))+
  ylab("Discharge (m^3/s)")

LookoutCreekCont <-  LookoutCreekCont %>%
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

aggM<-aggregate(LookoutCreekCont[,c(2, 5, 16)], by=list(LookoutCreekCont$CDate), FUN = mean)

names(aggM)[1]<-"Date"

ggcols<-c("Calcium" = "red", "Discharge" = "blue")

ggplot(aggM, aes(x=Date))+geom_line(aes(y=Q, col="Discharge"))+geom_line(aes(y=ConcDay * 2, col="Calcium"))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . /2, name = "Calcium (mg/L)"))+labs(y="Discharge (cms)", color="")+
  theme_classic()+theme(legend.position = "bottom", text=element_text(size=20))+
  scale_x_date(date_labels = "%b %d")+scale_color_manual(values = ggcols)

dev.off()

write.csv(LookoutCreekCont, "HJAContCa.csv")
