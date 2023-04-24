#2 EMMA baseflow correlation with peak swe
require(ggpmisc)
require(EflowStats)
require(dplyr)
require(ggpubr)
require(corrplot)
require(GGally)
require(car)
require(QuantPsyc)
require(PCAtools)
require(lme4)
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
  
  BFagg<-aggregate(BFPer~month+WY, BF, FUN=mean)
  
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
  
  shoulder_agg<-shoulder_agg[,c(1,4,6,9:18)]
  
  names(shoulder_agg)[1]<-"WY"
  
  shoulder_agg$site<-sites[i]
  shoulder_agg$Q5<-(shoulder_agg$Q5-min(shoulder_agg$Q5))/
                           (max(shoulder_agg$Q5)-min(shoulder_agg$Q5))
  
  shoulder_agg$Q5_norm<-shoulder_agg$Q5
  
  shoulder_agg$Q5lag<-(shoulder_agg$Q5lag-min(shoulder_agg$Q5lag, na.rm = TRUE))/
    (max(shoulder_agg$Q5lag, na.rm = TRUE)-min(shoulder_agg$Q5lag, na.rm = TRUE))
  shoulder_agg$BF_meta<-shoulder_agg$BFPer
  
  df_list[[i]]<-shoulder_agg
  
}

df_master<-do.call(rbind, df_list)

#remove lagged columns
#df_master<-df_master[,-c(8,9,13)]
df_master<-df_master[complete.cases(df_master),]
df_master$unique<-paste0(df_master$site, df_master$WY)

#scale data
df_master[c(2:13)]<-data.frame(sapply(df_master[c(2:13)], scale))

df_master$site<-factor(df_master$site, levels = c("Lookout", "Sagehen", "Coal"))

colnames(df_master)[c(1:12)]<-c("WY", "SSGW", "Q5", "Peak SWE", "Melt Rate", "Total Precipitation",
                                "Liquid Precipitation", "Q5lag", "SWElag", "PeakSWEDay", "ZeroDay",
                                "Snowfall Fraction")

final_mat<-df_master[c(2:13)]
final_mat_t<-t(final_mat)
colnames(final_mat_t)<-df_master$unique

metadata<-df_master[,c(1,14:17)]

rownames(metadata)<-df_master$unique

pca<-pca(final_mat_t, metadata = metadata)

screeplot(pca)

colors <- colorRampPalette(c("red4","indianred","goldenrod","lightslateblue","dodgerblue4"))(39)

p1<-biplot(pca, x="PC1", y="PC2", showLoadings = TRUE, shape = "site", colby = "Q5_norm",
       legendPosition = "right", lab=NULL, ntopLoadings = 3)+theme_classic()+
  scale_color_gradientn(colors = colors)+labs(col="Normalized Q5", shape="Site")+
  theme(text = element_text(size = 20))+xlim(-6.5,6)+ylim(-6,6)

p2<-biplot(pca, x="PC2", y="PC3", showLoadings = TRUE, colby = "Q5_norm", shape = "site", 
       legendPosition = "right", lab=NULL, ntopLoadings = 3)+theme_classic()+
  scale_color_gradientn(colors = colors)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson")

pdf("SSGW_Q5_PCA.pdf", width = 9, height = 7)

print(p1)

dev.off()

ggarrange(p1, p2, nrow = 2)

p1

plotloadings(pca, components = c("PC1", "PC2", "PC3"), rangeRetain = 1)

####THIS CODE TO CREATE CUSTOM PCA PLOT WITH SIGNIFICANCE OF EIGENVECTORS###

#use this to test significance of PC1 and PC2 eigenvectors
loadings<-pca$loadings[,c(1,2,3)]

#t test - copy and past min and max of 95% CI into ifelse statement below
#PC1
t.test(loadings$PC1, conf.level = 0.99)

loadings$PC1_sig<-ifelse(loadings$PC1 < -0.3731920, "sig",
                         ifelse(loadings$PC1 > 0.1061529, "sig", "not sig"))

#PC2 t test
t.test(loadings$PC2, conf.level = 0.99)

loadings$PC2_sig<-ifelse(loadings$PC2 < -0.1915400, "sig",
                         ifelse(loadings$PC2 > 0.3327023, "sig", "not sig"))

#PC3 t test
t.test(loadings$PC3, conf.level = 0.99)

loadings$PC3_sig<-ifelse(loadings$PC3 < -0.2418149, "sig",
                         ifelse(loadings$PC3 > 0.2964136, "sig", "not sig"))


loadings<-loadings[complete.cases(loadings$sig),]
#write.csv(loadings, "PCA_trends_sig.csv")

#create dataframe of loadings
pc_loadings<-pca$rotated[,c(1:3)]

sites<-c("Coal", "Sagehen", "Lookout")

cv_df_list<-list()
range_df_list<-list()

for (i in 1:length(sites)) {

  site<-grep(paste(sites[i]), rownames(pc_loadings))
  
  pc_site<-pc_loadings[site,]
  
  cv_pc1<-sd(pc_site$PC1)
  range_pc1<-diff(range(pc_site$PC1))
  
  cv_pc2<-sd(pc_site$PC2)
  range_pc2<-diff(range(pc_site$PC2))
  
  cv_df<-cbind(cv_pc1, cv_pc2)
  range_df<-cbind(range_pc1, range_pc2)
    
  cv_df_list[[i]]<-cv_df
  range_df_list[[i]]<-range_df
}




#for lmer
df_master<-do.call(rbind, df_list)

#scale data
df_master[c(4:13)]<-data.frame(sapply(df_master[c(4:13)], scale))


lm_best<-lmer(formula = Q5 ~ PeakSWE + MeltRate + TotPrecip + LiquidPrecip + SWElag +
                    PeakSWEWYDay + ZeroWYDay + SnowFrac + BFlag +Q5lag+(1|site), data = df_master)

lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}

#get stats
sum<-summary(lm_best)
r.squaredGLMM(lm_best)
#plot(lm_best)
#get beta coefficients and make df
beta_df<-as.data.frame(lm.beta.lmer(lm_best))
beta_df$name<-rownames(beta_df)

#get vif values and make df
vif_df<-as.data.frame(vif(lm_best))
vif_df$name<-rownames(vif_df)

plot(lm_best)


