#2 EMMA baseflow correlation with peak swe
require(ggpmisc)
require(EflowStats)
require(dplyr)
require(ggpubr)
require(corrplot)
require(GGally)
require(car)
require(QuantPsyc)
require(plot.matrix)
require(tibble)
require(raster)
#install.packages("QuantPsyc")

#define wds
working_dirs_BF<-c("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data",
                   "/Users/keirajohnson/Box Sync/Keira_Johnson/HJ_Andrews/Data",
                   "/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen/Data")

working_dirs_SP<-c("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek",
                   "/Users/keirajohnson/Box Sync/Keira_Johnson/HJ_Andrews",
                   "/Users/keirajohnson/Box Sync/Keira_Johnson/Sagehen")

BF_files<-c("CoalCreekBaseflow.csv", "HJABaseflow.csv", "SagehenBaseflow.csv")

Coal_shoulder<-c("06", "07")
Sage_shoulder<-c("05", "06", "07")
Lookout_shoulder<-c("04", "05", "06", "07")

shoulder_months<-list(c(Coal_shoulder), c(Lookout_shoulder), c(Sage_shoulder))

sites<-c("Coal", "Lookout", "Sagehen")

df_list<-list()

for (i in 1:3) {
  
  setwd(paste(working_dirs_BF[i]))
  
  BF<-read.csv(paste(BF_files[i]))
  
  if(i==3){
    BF<-BF[,c(2,3,14)]
  } else{
    BF<-BF[,c(2,3,18)]
  }
  
  BF$month<-format(as.Date(BF$Date), "%m")
  BF$WY<-get_waterYear(BF$Date)
  names(BF)[3]<-"BFPer"
  
  BF$gw_rate<-BF$Q*BF$BFPer
  
  BFagg<-aggregate(BFPer~month+WY, BF, FUN=mean)
  
  BFagg2<-aggregate(gw_rate~month+WY, BF, FUN=mean)
  
  BFagg<-merge(BFagg, BFagg2, by=c("month", "WY"))
  
  months_order<-c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08", "09")
  
  BFagg$month<-factor(BFagg$month, order = TRUE, levels = months_order)
  
  BFagg$WY<-as.character(BFagg$WY)
  
  setwd(paste(working_dirs_SP[i]))
  
  snow<-read.csv("SnowParamsALL.csv")  
  
  snow[,c(6:14)]<-sapply(snow[,c(6:14)], as.numeric)
  
  df_tot<-merge(BFagg, snow, by="WY")
  
  spring_months<-c("03", "04", "05", "06", "07", "08")
  
  spring_df<-df_tot[df_tot$month %in% spring_months,]
  
  spring_df <- spring_df %>%
    group_by(month) %>%
    mutate(BFlag=lag(BFPer, n=1))
  
  shoulder_df<-spring_df[spring_df$month %in% shoulder_months[[i]],]
  
  shoulder_agg<-aggregate(shoulder_df, by=list(shoulder_df$WY), FUN=mean)
  
  shoulder_agg<-shoulder_agg[,c(1,4,5,7,10:19)]
  
  names(shoulder_agg)[1]<-"WY"
  
  #shoulder_agg$site<-sites[i]
  #shoulder_agg$Q5_norm<-shoulder_agg$Q5/max(shoulder_agg$Q5)
  
  shoulder_agg[c(5:14)]<-scale(shoulder_agg[c(5:14)], center = TRUE)
  
  shoulder_agg$site<-sites[i]
  
  df_list[[i]]<-shoulder_agg
  
}

df_master<-do.call(rbind, df_list)


# df_master$scaledQ5<-case_when(
#   df_master$site=="Coal"~df_master$Q5/53,
#   df_master$site=="Lookout"~df_master$Q5/64,
#   df_master$site=="Sagehen"~df_master$Q5/27
# )
# 
# df_master$scaled_gw_rate<-case_when(
#   df_master$site=="Coal"~df_master$gw_rate/53,
#   df_master$site=="Lookout"~df_master$gw_rate/64,
#   df_master$site=="Sagehen"~df_master$gw_rate/27
# )

# df_master %>%
#   group_by(site) %>%
#   summarise(mean(BFPer))

cols<-c("firebrick", "darkgoldenrod", "blue3")
# 
df_master$site<-factor(df_master$site, levels = c("Lookout", "Sagehen", "Coal"))

p1<-ggplot(df_master, aes(PeakSWE, BFPer, col=site, shape=site))+geom_point(size=4)+
  geom_smooth(aes(col=site), method = "lm", se=FALSE)+
  stat_poly_eq(formula=y~x, mapping=aes(label=paste(..rr.label..)),
               parse=TRUE, size=7)+
  ylim(0,1.05)+
  labs(x="Normalized Peak SWE", y="RLGW",col="Site")+
  theme_classic()+theme(text = element_text(size = 20, family = "Times New Roman"), legend.position = "none")+
  scale_color_manual(values = cols)

p2<-ggplot(df_master, aes(PeakSWE, log(Q5), col=site, shape=site))+geom_point(size=4)+
  geom_smooth(aes(col=site), method = "lm", se=FALSE)+
  stat_poly_eq(formula=y~x, mapping=aes(label=paste(..rr.label..)),
               parse=TRUE, size=7)+ylim(c(-4,0))+
  labs(x="Normalized Peak SWE", y=bquote('Log(Q5)'~(m^3/s)), col="Site")+
  theme_classic()+theme(text = element_text(size = 20, family = "Times New Roman"), legend.position = "none",
                        legend.title = element_blank(),
                        legend.background = element_rect(fill="white",
                                                         linetype = "solid", colour=NA))+
  scale_shape_manual("Site", values=c(16,17,15)) +
  scale_color_manual("Site", values = cols)

pdf("SSGW_Lowflow.pdf", width = 9, height = 7)

p3<-ggplot(df_master, aes(BFPer, Q5, col=site, shape=site))+geom_point(size=4)+
  geom_smooth(aes(col=site), method = "lm", se=FALSE)+
  stat_poly_eq(formula=y~x, mapping=aes(label=paste(..rr.label..)),
               parse=TRUE, size=7)+
  labs(y=bquote('Q5'~(m^3/s)), x="RLGW", col="Site")+
  theme_classic()+theme(text = element_text(size = 20, family = "Times New Roman"), legend.position = c(0.84,0.9),
                        legend.title = element_blank(),
                        legend.background = element_rect(fill="white",
                                                         linetype = "solid", colour=NA))+
  scale_color_manual(values = cols)+
  scale_shape_manual("Site", values=c(16,17,15))

dev.off()

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson")

pdf("PeakSWE_SSGW_Q5.pdf", height = 6, width = 21)

gg_arr<-ggarrange(p1, p2, p3, ncol = 3)

ggsave("PeakSWE_SSGW_Q5.tiff", gg_arr, height = 6, width = 21)

dev.off()

p1<-ggplot(df_master, aes(PeakSWE, scaled_gw_rate, col=site, shape=site))+geom_point(size=4)+
  geom_smooth(aes(col=site), method = "lm", se=FALSE)+
  stat_poly_eq(formula=y~x, mapping=aes(label=paste(..rr.label..)),
               parse=TRUE, size=7)+
  labs(x="Normalized Peak SWE", y=bquote('DA Normalized RLGW'~(m^3~s^-1~km^-2)),col="Site")+
  theme_classic()+theme(text = element_text(size = 20, family = "Times New Roman"), legend.position = "none")+
  scale_color_manual(values = cols)

p2<-ggplot(df_master, aes(PeakSWE, scaledQ5, col=site, shape=site))+geom_point(size=4)+
  geom_smooth(aes(col=site), method = "lm", se=FALSE)+
  stat_poly_eq(formula=y~x, mapping=aes(label=paste(..rr.label..)),
               parse=TRUE, size=7)+
  labs(x="Z-Score Normalized Peak SWE", y=bquote('DA Normalized Q5'~(m^3~s^-1~km^-2)), col="Site")+
  theme_classic()+theme(text = element_text(size = 20, family = "Times New Roman"), legend.position = "none",
                        legend.title = element_blank(),
                        legend.background = element_rect(fill="white",
                                                         linetype = "solid", colour=NA))+
  scale_shape_manual("Site", values=c(16,17,15)) +
  scale_color_manual("Site", values = cols)

p3<-ggplot(df_master, aes(scaled_gw_rate, scaledQ5, col=site, shape=site))+geom_point(size=4)+
  geom_smooth(aes(col=site), method = "lm", se=FALSE)+
  stat_poly_eq(formula=y~x, mapping=aes(label=paste(..rr.label..)),
               parse=TRUE, size=7)+
  labs(y=bquote('DA Normalized Q5'~(m^3~s^-1~km^-2)), x=bquote('DA Normalized RLGW'~(m^3~s^-1~km^-2)), col="Site")+
  theme_classic()+theme(text = element_text(size = 20, family = "Times New Roman"), legend.position = c(0.87,0.15),
                        legend.title = element_blank(),
                        legend.background = element_rect(fill="white",
                                                         linetype = "solid", colour=NA))+
  scale_color_manual(values = cols)+
  scale_shape_manual("Site", values=c(16,17,15))

p2

gg_arr_scaled<-ggarrange(p1, p2, p3, ncol = 3)

ggsave("PeakSWE_SSGW_Q5_newscaled.tiff", gg_arr_scaled, height = 6, width = 21)

dev.off()