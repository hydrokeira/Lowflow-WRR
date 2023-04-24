##calculate NSE between observed and modeled Ca values
install.packages("hydroGOF")
library(hydroGOF)

working_dirs_BF<-c("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data",
                   "/Users/keirajohnson/Box Sync/Keira_Johnson/HJ_Andrews/Data",
                   "/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen/Data")

BF_files<-c("CoalCreekBaseflow.csv", "HJABaseflow.csv", "SagehenBaseflow.csv")

Ca_files<-c("WRTDSCoalCa.csv", "HJAWRTDS_CA.csv", "SagehenWRTDS_CA.csv")

NSE_Ca<-list()

for (i in 1:length(BF_files)) {
  
  setwd(paste(working_dirs_BF[[i]]))
  
  BF<-read.csv(BF_files[[i]])
  
  Ca<-read.csv(Ca_files[[i]])
  
  colnames(Ca)<-c("Date", "Remarks", "Ca")
  
  Ca$Date<-as.Date(Ca$Date)
  
  Ca<-Ca[complete.cases(Ca$Date),]
  
  Ca_dates<-Ca$Date
  
  BF$Date<-as.Date(BF$Date)
  
  BF_Ca<-BF[BF$Date %in% Ca_dates,]
  
  NSE_Ca[[i]]<-NSE(BF_Ca$ConcDay, BF_Ca$ConcDay)
  
}

NSE_Ca_df<-as.data.frame(do.call(rbind, NSE_Ca))

NSE_Ca_df$site<-c("Coal", "HJA", "Sagehen")

mean(NSE_Ca_df$V1)

