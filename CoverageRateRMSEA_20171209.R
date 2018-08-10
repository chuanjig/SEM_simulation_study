

# ====== added by CG 12/06/17======
# ====== added by CG 12/09/17======
# ====== run it again by CG using the correct POP 12/25/17 =======

# 1. Coverage rates for 90% confidence intervals around population RMSEA for simulation 16.
getwd()
rm(list=ls())

# install.packages("readxl")
library(readxl)
# pop<-read_excel("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/POP.xlsx", sheet=1)
pop<-read_excel("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/POP.xlsx", sheet=1)

  con=c(rep(NA,27))
  df=c(rep(NA,27))
  cr.rmsea.ml.90=c(rep(NA,27))
  cr.rmsea.mlm.90=c(rep(NA,27))
  cr.rmsea.mlr.90=c(rep(NA,27))
  cr.rmsea.mlmv.90=c(rep(NA,27))

######################################
for(i in 1:27){
  # i=1

  # path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/c%d",i) #define path using condtion number
  path=sprintf("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/c%d",i) #define path using condtion number
  
  setwd(path) # Specify the directory 
  
  data<-read.csv("cond.csv")
  
  data<-data[, 2:10]
  # sum(is.na(data))
  # sum(data == 999)
  data[data == 999] <- NA
  # sum(is.na(data))
  # sum(data == 999)
  # data<-na.omit(data) # delete rows having NAs
  # names(data)
  
  c_parameter<-read.csv("c_parameter.csv")
  # c_parameter[c_parameter == 999] <- NA
  # c_parameter<-na.omit(c_parameter)
  
  con[i]=i
  df[i]=data$dfml[1]

  rmsea.ml.l.90=c(rep(NA,1000))
  rmsea.ml.u.90=c(rep(NA,1000))
  rmsea.mlm.l.90=c(rep(NA,1000))
  rmsea.mlm.u.90=c(rep(NA,1000))
  rmsea.mlr.l.90=c(rep(NA,1000))
  rmsea.mlr.u.90=c(rep(NA,1000))
  rmsea.mlmv.l.90=c(rep(NA,1000))
  rmsea.mlmv.u.90=c(rep(NA,1000))

##===================
for(j in 1:1000){
  
  # i=5
  # j=1
  # install.packages("GAIPE")
  library(GAIPE)
  
  #=============================
  if(is.na(data$rmseaml[j])==T){
    CI.ml.90$Lower.CI=NA
    CI.ml.90$Upper.CI=NA
  }
  
  if(is.na(data$rmseaml[j])==F){
    CI.ml.90=CI.RMSEA(data$rmseaml[j], pop$df[i], pop$N[i], clevel=.90)
    CI.ml.90$Lower.CI<-as.numeric(CI.ml.90$Lower.CI)
    CI.ml.90$Upper.CI<-as.numeric(CI.ml.90$Upper.CI)
  }
  
  if(is.na(data$rmseamlm[j])==T){
    CI.mlm.90$Lower.CI=NA
    CI.mlm.90$Upper.CI=NA
  }
  
  if(is.na(data$rmseamlm[j])==F){
    CI.mlm.90=CI.RMSEA(data$rmseamlm[j], pop$df[i], pop$N[i], clevel=.90)
    CI.mlm.90$Lower.CI<-as.numeric(CI.mlm.90$Lower.CI)*sqrt(c_parameter$c.mlm[j])
    CI.mlm.90$Upper.CI<-as.numeric(CI.mlm.90$Upper.CI)*sqrt(c_parameter$c.mlm[j])
  }
  
  if(is.na(data$rmseamlr[j])==T){
    CI.mlr.90$Lower.CI=NA
    CI.mlr.90$Upper.CI=NA
  }
  
  if(is.na(data$rmseamlr[j])==F){
    CI.mlr.90=CI.RMSEA(data$rmseamlr[j], pop$df[i], pop$N[i], clevel=.90)
    CI.mlr.90$Lower.CI<-as.numeric(CI.mlr.90$Lower.CI)*sqrt(c_parameter$c.mlr[j])
    CI.mlr.90$Upper.CI<-as.numeric(CI.mlr.90$Upper.CI)*sqrt(c_parameter$c.mlr[j])
  }
  
  if(is.na(data$rmseamlmv[j])==T){
    CI.mlmv.90$Lower.CI=NA
    CI.mlmv.90$Upper.CI=NA
  }
  
  if(is.na(data$rmseamlmv[j])==F){
    CI.mlmv.90=CI.RMSEA(data$rmseamlmv[j], pop$df[i], pop$N[i], clevel=.90)
    CI.mlmv.90$Lower.CI<-as.numeric(CI.mlmv.90$Lower.CI)*sqrt(c_parameter$c.mlmv[j])
    CI.mlmv.90$Upper.CI<-as.numeric(CI.mlmv.90$Upper.CI)*sqrt(c_parameter$c.mlmv[j])
  }
  
  #============================
  
  rmsea.ml.l.90[j]=CI.ml.90$Lower.CI
  rmsea.ml.u.90[j]=CI.ml.90$Upper.CI
  rmsea.mlm.l.90[j]=CI.mlm.90$Lower.CI
  rmsea.mlm.u.90[j]=CI.mlm.90$Upper.CI
  rmsea.mlr.l.90[j]=CI.mlr.90$Lower.CI
  rmsea.mlr.u.90[j]=CI.mlr.90$Upper.CI
  rmsea.mlmv.l.90[j]=CI.mlmv.90$Lower.CI
  rmsea.mlmv.u.90[j]=CI.mlmv.90$Upper.CI
  
  RMSEA_CI=data.frame(rmsea.ml.l.90,rmsea.ml.u.90,rmsea.mlm.l.90,rmsea.mlm.u.90,rmsea.mlr.l.90,rmsea.mlr.u.90,rmsea.mlmv.l.90,rmsea.mlmv.u.90)
  write.csv(RMSEA_CI,"RMSEA_CI.csv")
}
##===================
  
  # calculate the coverage rates for 90% CI around population RMSEA.
  cr.rmsea.ml.90[i]=mean(rmsea.ml.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.ml.u.90>=pop[i,"RMSEA"]$RMSEA, na.rm = T)
  cr.rmsea.mlm.90[i]=mean(rmsea.mlm.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.mlm.u.90>=pop[i,"RMSEA"]$RMSEA,na.rm = T)
  cr.rmsea.mlr.90[i]=mean(rmsea.mlr.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.mlr.u.90>=pop[i,"RMSEA"]$RMSEA,na.rm = T)
  cr.rmsea.mlmv.90[i]=mean(rmsea.mlmv.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.mlmv.u.90>=pop[i,"RMSEA"]$RMSEA,na.rm = T)
  
  cr.rmsea=data.frame(con, df,
                      cr.rmsea.ml.90, cr.rmsea.mlm.90,cr.rmsea.mlr.90, cr.rmsea.mlmv.90)
}
######################################
  # setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out")
  setwd("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out")
  write.csv(cr.rmsea,"coveragerateRMSEA.csv")

  

############## added by CG 12/10/17===============================================================  
# ====== run it again by CG using the correct POP 12/26/17 =======
# 2. Coverage rates for 90% confidence intervals around population RMSEA for simulation 32.
  getwd()
  rm(list=ls())
  
  # install.packages("readxl")
  library(readxl)
  # pop<-read_excel("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/POP.xlsx", sheet=1)
  pop<-read_excel("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/POP.xlsx", sheet=1)
  
  con=c(rep(NA,27))
  df=c(rep(NA,27))
  cr.rmsea.ml.90=c(rep(NA,27))
  cr.rmsea.mlm.90=c(rep(NA,27))
  cr.rmsea.mlr.90=c(rep(NA,27))
  cr.rmsea.mlmv.90=c(rep(NA,27))
  
  ######################################
  for(i in 1:27){
    # i=1
    
    # path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/c%d",i) #define path using condtion number
    path=sprintf("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/c%d",i) #define path using condtion number
    
    setwd(path) # Specify the directory 
    
    data<-read.csv("cond.csv")
    
    data<-data[, 2:10]
    # sum(is.na(data))
    # sum(data == 999)
    data[data == 999] <- NA
    # sum(is.na(data))
    # sum(data == 999)
    # data<-na.omit(data) # delete rows having NAs
    # names(data)
    
    c_parameter<-read.csv("c_parameter.csv")
    # c_parameter[c_parameter == 999] <- NA
    # c_parameter<-na.omit(c_parameter)
    
    con[i]=i
    df[i]=data$dfml[1]
    
    rmsea.ml.l.90=c(rep(NA,1000))
    rmsea.ml.u.90=c(rep(NA,1000))
    rmsea.mlm.l.90=c(rep(NA,1000))
    rmsea.mlm.u.90=c(rep(NA,1000))
    rmsea.mlr.l.90=c(rep(NA,1000))
    rmsea.mlr.u.90=c(rep(NA,1000))
    rmsea.mlmv.l.90=c(rep(NA,1000))
    rmsea.mlmv.u.90=c(rep(NA,1000))
    
    ##===================
    for(j in 1:1000){
      
      # i=5
      # j=1
      # install.packages("GAIPE")
      library(GAIPE)
      
      #=============================
      if(is.na(data$rmseaml[j])==T){
        CI.ml.90$Lower.CI=NA
        CI.ml.90$Upper.CI=NA
      }
      
      if(is.na(data$rmseaml[j])==F){
        CI.ml.90=CI.RMSEA(data$rmseaml[j], pop$df[i], pop$N[i], clevel=.90)
        CI.ml.90$Lower.CI<-as.numeric(CI.ml.90$Lower.CI)
        CI.ml.90$Upper.CI<-as.numeric(CI.ml.90$Upper.CI)
      }
      
      if(is.na(data$rmseamlm[j])==T){
        CI.mlm.90$Lower.CI=NA
        CI.mlm.90$Upper.CI=NA
      }
      
      if(is.na(data$rmseamlm[j])==F){
        CI.mlm.90=CI.RMSEA(data$rmseamlm[j], pop$df[i], pop$N[i], clevel=.90)
        CI.mlm.90$Lower.CI<-as.numeric(CI.mlm.90$Lower.CI)*sqrt(c_parameter$c.mlm[j])
        CI.mlm.90$Upper.CI<-as.numeric(CI.mlm.90$Upper.CI)*sqrt(c_parameter$c.mlm[j])
      }
      
      if(is.na(data$rmseamlr[j])==T){
        CI.mlr.90$Lower.CI=NA
        CI.mlr.90$Upper.CI=NA
      }
      
      if(is.na(data$rmseamlr[j])==F){
        CI.mlr.90=CI.RMSEA(data$rmseamlr[j], pop$df[i], pop$N[i], clevel=.90)
        CI.mlr.90$Lower.CI<-as.numeric(CI.mlr.90$Lower.CI)*sqrt(c_parameter$c.mlr[j])
        CI.mlr.90$Upper.CI<-as.numeric(CI.mlr.90$Upper.CI)*sqrt(c_parameter$c.mlr[j])
      }
      
      if(is.na(data$rmseamlmv[j])==T){
        CI.mlmv.90$Lower.CI=NA
        CI.mlmv.90$Upper.CI=NA
      }
      
      if(is.na(data$rmseamlmv[j])==F){
        CI.mlmv.90=CI.RMSEA(data$rmseamlmv[j], pop$df[i], pop$N[i], clevel=.90)
        CI.mlmv.90$Lower.CI<-as.numeric(CI.mlmv.90$Lower.CI)*sqrt(c_parameter$c.mlmv[j])
        CI.mlmv.90$Upper.CI<-as.numeric(CI.mlmv.90$Upper.CI)*sqrt(c_parameter$c.mlmv[j])
      }
      
      #============================
      
      rmsea.ml.l.90[j]=CI.ml.90$Lower.CI
      rmsea.ml.u.90[j]=CI.ml.90$Upper.CI
      rmsea.mlm.l.90[j]=CI.mlm.90$Lower.CI
      rmsea.mlm.u.90[j]=CI.mlm.90$Upper.CI
      rmsea.mlr.l.90[j]=CI.mlr.90$Lower.CI
      rmsea.mlr.u.90[j]=CI.mlr.90$Upper.CI
      rmsea.mlmv.l.90[j]=CI.mlmv.90$Lower.CI
      rmsea.mlmv.u.90[j]=CI.mlmv.90$Upper.CI
      
      RMSEA_CI=data.frame(rmsea.ml.l.90,rmsea.ml.u.90,rmsea.mlm.l.90,rmsea.mlm.u.90,rmsea.mlr.l.90,rmsea.mlr.u.90,rmsea.mlmv.l.90,rmsea.mlmv.u.90)
      write.csv(RMSEA_CI,"RMSEA_CI.csv")
    }
    ##===================
    
    # calculate the coverage rates for 90% CI around population RMSEA.
    cr.rmsea.ml.90[i]=mean(rmsea.ml.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.ml.u.90>=pop[i,"RMSEA"]$RMSEA, na.rm = T)
    cr.rmsea.mlm.90[i]=mean(rmsea.mlm.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.mlm.u.90>=pop[i,"RMSEA"]$RMSEA,na.rm = T)
    cr.rmsea.mlr.90[i]=mean(rmsea.mlr.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.mlr.u.90>=pop[i,"RMSEA"]$RMSEA,na.rm = T)
    cr.rmsea.mlmv.90[i]=mean(rmsea.mlmv.l.90<=pop[i,"RMSEA"]$RMSEA & rmsea.mlmv.u.90>=pop[i,"RMSEA"]$RMSEA,na.rm = T)
    
    cr.rmsea=data.frame(con, df,
                        cr.rmsea.ml.90, cr.rmsea.mlm.90,cr.rmsea.mlr.90, cr.rmsea.mlmv.90)
  }
  ######################################
  # setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out")
  setwd("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out")
  write.csv(cr.rmsea,"coveragerateRMSEA.csv")
  