

# ====== added by CG 12/04/17 ======
# ====== modified by CG 12/06/17======
# ====== modified by CG 12/09/17======

# 1. average sample RMSEA for simulation 16.
# get the wrong and right RMSEA.
# (Notice that RMSEA for ML are the same, so there is no right or wrong for ML, but there is a wrong and a right RESEA for MLM, MLR, MLMV)
# (Another thing is that the MLM and MLMV results should be the same)

getwd()
#setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out")

con=c(rep(NA,27))
df=c(rep(NA,27))
Mrmsea.ml=c(rep(NA,27))
Mrmsea.mlm.w=c(rep(NA,27))
Mrmsea.mlr.w=c(rep(NA,27))
Mrmsea.mlmv.w=c(rep(NA,27))
Mrmsea.mlm.r=c(rep(NA,27))
Mrmsea.mlr.r=c(rep(NA,27))
Mrmsea.mlmv.r=c(rep(NA,27))

for(i in 1:27){
  
  # i = 1
  path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out/c%d",i) #define path using condtion number
  setwd(path) # Specify the directory 
  
  data<-read.csv("cond.csv")
  
  data<-data[, 2:10]
  
  # replace all the 999s (weird data) to NA.
  data[data == 999] <- NA
  
  # Step 1. get the mean of wrong RMSEA and c for MLM, MLR, MLMV.
  Mrmsea.ml[i]=mean(data$rmseaml,na.rm=T)
  Mrmsea.mlm.w[i]=mean(data$rmseamlm,na.rm=T)
  Mrmsea.mlr.w[i]=mean(data$rmseamlr,na.rm=T)
  Mrmsea.mlmv.w[i]=mean(data$rmseamlmv,na.rm=T)
  
  # get the df and condition number
  df[i]=data$dfml[1]
  con[i]=i
  
  c.mlm=c(rep(NA,1000))
  c.mlr=c(rep(NA,1000))
  c.mlmv=c(rep(NA,1000))
  
  # get the c parameter
  for(j in 1:1000){
  c.mlm[j]=data$chiml[j]/data$chimlm[j] # c for mlm
  c.mlr[j]=data$chiml[j]/data$chimlr[j] # c for mlr
  c.mlmv[j]=(data$chiml[j]*(data$chimlm[j]-df[i]))/(data$chimlm[j]*(data$chimlmv[j]-df[i])) # c for mlmv
  }

  c_parameter=data.frame(c.mlm,c.mlr,c.mlmv)
  # str(c_parameter)
  write.table(c_parameter, "c_parameter.csv", col.names=TRUE, row.names=FALSE, sep = ",")

  # Step 2. get the mean of right RMSEA
  Mrmsea.mlm.r[i]=mean(data$rmseamlm*sqrt(c.mlm),na.rm=T)
  Mrmsea.mlr.r[i]=mean(data$rmseamlr*sqrt(c.mlr),na.rm=T)
  Mrmsea.mlmv.r[i]=mean(data$rmseamlmv*sqrt(c.mlmv),na.rm=T)  
  
}

# put all of them into a dataframe
Mrmsea=data.frame(con, df,
                  Mrmsea.ml, Mrmsea.mlm.w, Mrmsea.mlr.w, Mrmsea.mlmv.w, Mrmsea.mlm.r, Mrmsea.mlr.r, Mrmsea.mlmv.r)

# str(Mrmsea)
setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation16/out")
write.table(Mrmsea, "Mrmsea.csv", col.names=TRUE, row.names=FALSE, sep = ",")



# 2. average sample RMSEA for simulation 32.
# added by CG 12/10/17
# get the wrong and right RMSEA.
# (Notice that RMSEA for ML are the same, so there is no right or wrong for ML, but there is a wrong and a right RESEA for MLM, MLR, MLMV)
# (Another thing is that the MLM and MLMV results should be the same)

getwd()
#setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out")

con=c(rep(NA,27))
df=c(rep(NA,27))
Mrmsea.ml=c(rep(NA,27))
Mrmsea.mlm.w=c(rep(NA,27))
Mrmsea.mlr.w=c(rep(NA,27))
Mrmsea.mlmv.w=c(rep(NA,27))
Mrmsea.mlm.r=c(rep(NA,27))
Mrmsea.mlr.r=c(rep(NA,27))
Mrmsea.mlmv.r=c(rep(NA,27))

for(i in 1:27){
  
  # i = 1
  path=sprintf("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out/c%d",i) #define path using condtion number
  setwd(path) # Specify the directory 
  
  data<-read.csv("cond.csv")
  
  data<-data[, 2:10]
  
  # replace all the 999s (weird data) to NA.
  data[data == 999] <- NA
  
  # Step 1. get the mean of wrong RMSEA and c for MLM, MLR, MLMV.
  Mrmsea.ml[i]=mean(data$rmseaml,na.rm=T)
  Mrmsea.mlm.w[i]=mean(data$rmseamlm,na.rm=T)
  Mrmsea.mlr.w[i]=mean(data$rmseamlr,na.rm=T)
  Mrmsea.mlmv.w[i]=mean(data$rmseamlmv,na.rm=T)
  
  # get the df and condition number
  df[i]=data$dfml[1]
  con[i]=i
  
  c.mlm=c(rep(NA,1000))
  c.mlr=c(rep(NA,1000))
  c.mlmv=c(rep(NA,1000))
  
  # get the c parameter
  for(j in 1:1000){
    c.mlm[j]=data$chiml[j]/data$chimlm[j] # c for mlm
    c.mlr[j]=data$chiml[j]/data$chimlr[j] # c for mlr
    c.mlmv[j]=(data$chiml[j]*(data$chimlm[j]-df[i]))/(data$chimlm[j]*(data$chimlmv[j]-df[i])) # c for mlmv
  }
  
  c_parameter=data.frame(c.mlm,c.mlr,c.mlmv)
  # str(c_parameter)
  write.table(c_parameter, "c_parameter.csv", col.names=TRUE, row.names=FALSE, sep = ",")
  
  # Step 2. get the mean of right RMSEA
  Mrmsea.mlm.r[i]=mean(data$rmseamlm*sqrt(c.mlm),na.rm=T)
  Mrmsea.mlr.r[i]=mean(data$rmseamlr*sqrt(c.mlr),na.rm=T)
  Mrmsea.mlmv.r[i]=mean(data$rmseamlmv*sqrt(c.mlmv),na.rm=T)  
  
}

# put all of them into a dataframe
Mrmsea=data.frame(con, df,
                  Mrmsea.ml, Mrmsea.mlm.w, Mrmsea.mlr.w, Mrmsea.mlmv.w, Mrmsea.mlm.r, Mrmsea.mlr.r, Mrmsea.mlmv.r)

# str(Mrmsea)
setwd("E:/UPDATE/P_7_FACTOR_ANALYSIS/simulation32/out")
write.table(Mrmsea, "Mrmsea.csv", col.names=TRUE, row.names=FALSE, sep = ",")
