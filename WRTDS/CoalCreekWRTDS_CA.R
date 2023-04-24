require(dataRetrieval)
require(EGRET)
require(ggplot2)
require(lubridate)
require(tidyr)
require(dplyr)

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data", "CCWRTDS2015.csv", qUnit=2)

Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data", "WRTDSCoalCA.csv")

Info<-readNWISInfo("09111250", "00915")

Sample <- removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/"

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data")

saveResults(savePath, eList)

load("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/COAL.CA.RData")

eList<-modelEstimation(eList)

pdf("CoalCreekWRTDS_Ca_Updated.pdf")

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

CoalCreekCont<-eList$Daily

ggplot(CoalCreekCont, aes(x=Date))+geom_line(aes(y=Q), color="blue")+
  geom_line(aes(y=ConcDay/3), color="red")+scale_y_continuous(sec.axis = sec_axis(trans = ~ . *3, 
                                                                                     name = "Calcium (mg/L)"))+
  ylab("Discharge (m^3/s)")

CoalCreekCont <-  CoalCreekCont %>%
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

aggM<-aggregate(CoalCreekCont[,c(2, 5, 15)], by=list(CoalCreekCont$CDate), FUN = mean)

names(aggM)[1]<-"Date"

ggcols<-c("Calcium" = "red", "Discharge" = "blue")

ggplot(aggM, aes(x=Date))+geom_line(aes(y=Q, col="Discharge"))+geom_line(aes(y=ConcDay / 3, col="Calcium"))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . *3, name = "Calcium (mg/L)"))+labs(y="Discharge (cms)", color="")+
  theme_classic()+theme(legend.position = "bottom", text = element_text(size=20))+
  scale_x_date(date_labels = "%b %d")+scale_color_manual(values = ggcols)

dev.off()

write.csv(CoalCreekCont, "CCContCa.csv")
