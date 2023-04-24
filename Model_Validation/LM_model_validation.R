##validate LM models
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
#install.packages("QuantPsyc")
#install.packages("MuMIn")
require(MuMIn)
require(data.table)


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson")

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

for (i in 1:length(sites)) {
  
  setwd(paste(working_dirs_BF[i]))
  
  BF<-read.csv(paste(BF_files[i]))
  
  if(sites[i] == "Sagehen"){
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
  
  #shoulder_agg$site<-sites[i]
  #shoulder_agg$Q5_norm<-shoulder_agg$Q5/max(shoulder_agg$Q5)
  
  shoulder_agg[c(4:13)]<-scale(shoulder_agg[c(4:13)], center = TRUE)
  
  shoulder_agg$site<-sites[i]
  
  df_list[[i]]<-shoulder_agg
  
}

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson")

LM<-read.csv("MasterLM_Coefs_DF.csv")

LM_beta<-LM[,grep("beta", colnames(LM))]
rownames(LM_beta)<-LM$X

sites<-c("Coal", "Lookout", "Sagehen")

models<-c("_BFPer_beta","_Q5_beta", "Q5_onlyBFPer_beta")

model_rmse<-list()

site_rmse<-list()

for (i in 1:length(sites)) {
  
  for (k in 1:length(models)) {
    
    if(i==2&&k==2) next
    
    #select dataframe for site
    site_data<-df_list[[i]]
    
    input_vars<-LM_beta[,grep(sites[i], colnames(LM_beta))]
    
    input_vars_model<-as.data.frame(input_vars[,grep(models[k], colnames(input_vars))])
    
    rownames(input_vars_model)<-LM$X
    
    retained_vars<-which(!is.na(input_vars_model$`input_vars[, grep(models[k], colnames(input_vars))]`))
    
    vars_names<-rownames(input_vars_model)[retained_vars]
    
    use_collapse<-paste(vars_names, collapse = "+")
    
    dependent_v<-ifelse(models[k]=="_BFPer_", "BFPer", "Q5")
    
    lm_input<-formula(paste(dependent_v, "~", use_collapse))
    
    #put into model
    lm_mod<-lm(lm_input, site_data)
    
    sum<-summary(lm_mod)
    
    rmse<-sqrt(mean(lm_mod$residuals^2))
    
    rmse_loo<-loo(lm_mod, type = "rmse")
    
    model_rmse[[k]]<-c(rmse, rmse_loo)

  }
  
  site_rmse[[i]]<-model_rmse
  
}

rmse_df<-rbindlist(site_rmse)

colnames(rmse_df)<-models
rmse_df<-t(rmse_df)
colnames(rmse_df)<-c("Coal_model", "Coal_loo", "Lookout_model",
                     "Lookout_loo", "Sagehen_model", "Sagehen_loo")

rmse_df[2,c(3,4)]<-NA

for (i in 1:length(sites)) {
  
  site_df<-as.data.frame(rmse_df[,grep(sites[i], colnames(rmse_df))])
  
  site_df$percent_diff<-((site_df[,2]-site_df[,1])/site_df[,1])*100
  
  colnames(site_df)[3]<-paste0(sites[i], "_percent_diff")
  
  if(i==1){
    
    percent_diff_final<-site_df
    
  }else{
    
    percent_diff_final<-cbind(percent_diff_final, site_df)
    
  }
  
}

write.csv(percent_diff_final, "LM_Model_Validation.csv")


