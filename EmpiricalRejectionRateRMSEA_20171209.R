
# ====== added by CG 12/07/17======
# ====== run it again by CG using updated POP 12/26/17 ======
# Empirical rejection rates at a 5% significance level of a test that the RMSEA equal their population values for stimulation 16.

getwd()
rm(list=ls())

library(readxl)
# pop<-read_excel("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/POP.xlsx", sheet=1)
pop<-read_excel("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/POP.xlsx", sheet=1)

for(i in 1:27){
  # i=1
  # path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/c%d",i) #define path using condtion number
  path=sprintf("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/c%d",i) #define path using condtion number
  
  setwd(path) # Specify the directory 
  
  data<-read.csv("cond.csv")
  # sum(is.na(data))
  # sum(data == 999)
  data<-data[, 2:10]
  data[data == 999] <- NA
  # sum(is.na(data))
  # sum(data == 999)
  
  # data<-na.omit(data) # delete rows having NAs
  # names(data)
  
  c_parameter<-read.csv("c_parameter.csv")
  # c_parameter[c_parameter == 999] <- NA
  # c_parameter<-na.omit(c_parameter)

  lamda.ml=c(rep(NA,nrow(data)))
  rmsea.ml.p=c(rep(NA,nrow(data)))
  lamda.mlm=c(rep(NA,nrow(data)))
  rmsea.mlm.p=c(rep(NA,nrow(data)))
  lamda.mlr=c(rep(NA,nrow(data)))
  rmsea.mlr.p=c(rep(NA,nrow(data)))
  lamda.mlmv=c(rep(NA,nrow(data)))
  rmsea.mlmv.p=c(rep(NA,nrow(data)))

  for(j in 1:nrow(data)){
    # j=1
    lamda.ml[j]=pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df
    lamda.mlm[j]=(pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df)/(c_parameter$c.mlm[j])
    lamda.mlr[j]=(pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df)/(c_parameter$c.mlr[j])
    lamda.mlmv[j]=(pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df)/(c_parameter$c.mlmv[j])
    
    rmsea.ml.p[j]=(1-pchisq(q=data[j,"chiml"], df=pop[i,"df"]$df, ncp = lamda.ml[j], lower.tail = TRUE, log.p = FALSE))
    rmsea.mlm.p[j]=(1-pchisq(q=data[j,"chimlm"], df=pop[i,"df"]$df, ncp = lamda.mlm[j], lower.tail = TRUE, log.p = FALSE))
    rmsea.mlr.p[j]=(1-pchisq(q=data[j,"chimlr"], df=pop[i,"df"]$df, ncp = lamda.mlr[j], lower.tail = TRUE, log.p = FALSE))
    rmsea.mlmv.p[j]=(1-pchisq(q=data[j,"chimlmv"], df=pop[i,"df"]$df, ncp = lamda.mlmv[j], lower.tail = TRUE, log.p = FALSE))
  
    }
  
  pvalues=data.frame(lamda.ml,lamda.mlm,lamda.mlr,lamda.mlmv,rmsea.ml.p,rmsea.mlm.p,rmsea.mlr.p,rmsea.mlmv.p)
  write.csv(pvalues,"pvalue.csv")
}

# ==== Get the mean of Empirical rejection rates
con=c(rep(NA,27))
Mrmsea.ml.p=c(rep(NA,27))
Mrmsea.mlm.p=c(rep(NA,27))
Mrmsea.mlr.p=c(rep(NA,27))
Mrmsea.mlmv.p=c(rep(NA,27))

for(i in 1:27){

  # i = 1
  # path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/c%d",i) #define path using condtion number
  path=sprintf("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/c%d",i) #define path using condtion number
  
  setwd(path) # Specify the directory 
  
  data<-read.csv("pvalue.csv")
  # names(data)  

  # replace all the 999s (weird data) to NA.
  data[data == 999] <- NA
  con[i]=i

  Mrmsea.ml.p[i]=mean(data$rmsea.ml.p<0.05,na.rm=T)
  Mrmsea.mlm.p[i]=mean(data$rmsea.mlm.p<0.05,na.rm=T)
  Mrmsea.mlr.p[i]=mean(data$rmsea.mlr.p<0.05,na.rm=T)
  Mrmsea.mlmv.p[i]=mean(data$rmsea.mlmv.p<0.05,na.rm=T)
}

# put all of them into a dataframe
Mrmsea.p=data.frame(con,Mrmsea.ml.p, Mrmsea.mlm.p, Mrmsea.mlr.p, Mrmsea.mlmv.p)
# str(Mrmsea.p)
# setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out")
setwd("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out")
write.table(Mrmsea.p, "empiricalrejectionRMSEA.csv", col.names=TRUE, row.names=FALSE, sep = ",")



#################==========================
# ====== added by CG 12/10/17======
# ====== run it again by CG using updated POP 12/26/17 ======

# Empirical rejection rates at a 5% significance level of a test that the RMSEA equal their population values for stimulation 32.

getwd()
rm(list=ls())

library(readxl)
# pop<-read_excel("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/POP.xlsx", sheet=1)
pop<-read_excel("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/POP.xlsx", sheet=1)

for(i in 1:27){
  # i=1
  # path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/c%d",i) #define path using condtion number
  path=sprintf("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/c%d",i) #define path using condtion number
  setwd(path) # Specify the directory 
  
  data<-read.csv("cond.csv")
  # sum(is.na(data))
  # sum(data == 999)
  data<-data[, 2:10]
  data[data == 999] <- NA
  # sum(is.na(data))
  # sum(data == 999)
  
  # data<-na.omit(data) # delete rows having NAs
  # names(data)
  
  c_parameter<-read.csv("c_parameter.csv")
  # c_parameter[c_parameter == 999] <- NA
  # c_parameter<-na.omit(c_parameter)
  
  lamda.ml=c(rep(NA,nrow(data)))
  rmsea.ml.p=c(rep(NA,nrow(data)))
  lamda.mlm=c(rep(NA,nrow(data)))
  rmsea.mlm.p=c(rep(NA,nrow(data)))
  lamda.mlr=c(rep(NA,nrow(data)))
  rmsea.mlr.p=c(rep(NA,nrow(data)))
  lamda.mlmv=c(rep(NA,nrow(data)))
  rmsea.mlmv.p=c(rep(NA,nrow(data)))
  
  for(j in 1:nrow(data)){
    # j=1
    lamda.ml[j]=pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df
    lamda.mlm[j]=(pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df)/(c_parameter$c.mlm[j])
    lamda.mlr[j]=(pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df)/(c_parameter$c.mlr[j])
    lamda.mlmv[j]=(pop[i,"RMSEA"]$RMSEA*pop[i,"RMSEA"]$RMSEA*(pop[i,"N"]$N-1)*pop[i,"df"]$df)/(c_parameter$c.mlmv[j])
    
    rmsea.ml.p[j]=(1-pchisq(q=data[j,"chiml"], df=pop[i,"df"]$df, ncp = lamda.ml[j], lower.tail = TRUE, log.p = FALSE))
    rmsea.mlm.p[j]=(1-pchisq(q=data[j,"chimlm"], df=pop[i,"df"]$df, ncp = lamda.mlm[j], lower.tail = TRUE, log.p = FALSE))
    rmsea.mlr.p[j]=(1-pchisq(q=data[j,"chimlr"], df=pop[i,"df"]$df, ncp = lamda.mlr[j], lower.tail = TRUE, log.p = FALSE))
    rmsea.mlmv.p[j]=(1-pchisq(q=data[j,"chimlmv"], df=pop[i,"df"]$df, ncp = lamda.mlmv[j], lower.tail = TRUE, log.p = FALSE))
  }
  
  pvalues=data.frame(lamda.ml,lamda.mlm,lamda.mlr,lamda.mlmv,rmsea.ml.p,rmsea.mlm.p,rmsea.mlr.p,rmsea.mlmv.p)
  write.csv(pvalues,"pvalue.csv")
}

# ==== Get the mean of Empirical rejection rates
con=c(rep(NA,27))
Mrmsea.ml.p=c(rep(NA,27))
Mrmsea.mlm.p=c(rep(NA,27))
Mrmsea.mlr.p=c(rep(NA,27))
Mrmsea.mlmv.p=c(rep(NA,27))

for(i in 1:27){
  
  # i = 1
  # path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/c%d",i) #define path using condtion number
  path=sprintf("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/c%d",i) #define path using condtion number
  setwd(path) # Specify the directory 
  
  data<-read.csv("pvalue.csv")
  # names(data)  
  
  # replace all the 999s (weird data) to NA.
  data[data == 999] <- NA
  con[i]=i
  
  Mrmsea.ml.p[i]=mean(data$rmsea.ml.p<0.05,na.rm=T)
  Mrmsea.mlm.p[i]=mean(data$rmsea.mlm.p<0.05,na.rm=T)
  Mrmsea.mlr.p[i]=mean(data$rmsea.mlr.p<0.05,na.rm=T)
  Mrmsea.mlmv.p[i]=mean(data$rmsea.mlmv.p<0.05,na.rm=T)
}

# put all of them into a dataframe
Mrmsea.p=data.frame(con,Mrmsea.ml.p, Mrmsea.mlm.p, Mrmsea.mlr.p, Mrmsea.mlmv.p)
# str(Mrmsea.p)
# setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out")
setwd("/Volumes/Gao/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out")
write.table(Mrmsea.p, "empiricalrejectionRMSEA.csv", col.names=TRUE, row.names=FALSE, sep = ",")
