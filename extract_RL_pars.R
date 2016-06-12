#standard RL model
library(rstan)

#model with episodic value, no decay
fit2<-load('stanfit_hybridrl')
fit_extract<-extract(hybrid1_fit,permute=T)
Qvals_hybrid<-apply(fit_extract$Q,c(2,3,4),median)


#Separate Q value arrays for chosen and unchosen options
Q_chosen_hyb<-matrix(0,dim(Qvals_hybrid)[2],dim(Qvals_hybrid)[1])
Q_unchosen_hyb<-Q_chosen_hyb

for(i in 1:dim(Qvals_hybrid)[1]){
  for(j in 1:dim(Qvals_hybrid)[2]){
    if(choice[i,j]>0){
      Q_chosen_hyb[j,i]<-Qvals_hybrid[i,j,choice[i,j]]
      Q_unchosen_hyb[j,i]<-Qvals_hybrid[i,j,unchoice[i,j]]
    } 
      else{
      Q_chosen_hyb[j,i]<-NA
      Q_unchosen_hyb[j,i]<-NA
    }
  }

}
pe_hyb<-t(apply(fit_extract$delta,c(2,3),median))

alpha_hyb<-apply(fit_extract$alpha,2,median)
beta_hyb<-apply(fit_extract$beta,c(2,3),median)
Sigma<-apply(fit_extract$Sigma,c(2,3),median)
Omega<-apply(fit_extract$Omega,c(2,3),median)


#Summary for group level effects and covariance
summary(fit_extract$b_mean)
pairs(hybrid1_fit,pars="b_mean",labels=c("Intercept","Inverse Temp","Familiarity Bias","Episodic Value"))

#plot median estimates of subject-level effects
hist(beta_hyb[,4],xlab="Episodic Effect",main=NULL)
hist(beta_hyb[,2],xlab="Incremental Effect",main=NULL)
hist(fit_extract$Omega[,4,2],xlab="Episodic-Incremental Correlation",main=NULL)
hist(alpha_hyb,xlab="Learning Rate",main=NULL)

#plot posterior uncertainty for subject-level estimates
plot(hybrid1_fit,pars=c("beta[1,2]","beta[2,2]",
                        "beta[3,2]","beta[4,2]",
                        "beta[5,2]","beta[6,2]",
                        "beta[7,2]","beta[8,2]",
                        "beta[9,2]","beta[10,2]",
                        "beta[11,2]","beta[12,2]",
                        "beta[13,2]","beta[14,2]",
                        "beta[15,2]","beta[16,2]",
                        "beta[17,2]","beta[18,2]",
                        "beta[19,2]","beta[20,2]",
                        "beta[21,2]","beta[22,2]",
                        "beta[23,2]","beta[24,2]",
                        "beta[25,2]","beta[26,2]",
                        "beta[27,2]","beta[28,2]",
                        "beta[29,2]","beta[30,2]","beta[31,2]"))

plot(hybrid1_fit,pars=c("beta[1,3]","beta[2,3]",
                        "beta[3,3]","beta[4,3]",
                        "beta[5,3]","beta[6,3]",
                        "beta[7,3]","beta[8,3]",
                        "beta[9,3]","beta[10,3]",
                        "beta[11,3]","beta[12,3]",
                        "beta[13,3]","beta[14,3]",
                        "beta[15,3]","beta[16,3]",
                        "beta[17,3]","beta[18,3]",
                        "beta[19,3]","beta[20,3]",
                        "beta[21,3]","beta[22,3]",
                        "beta[23,3]","beta[24,3]",
                        "beta[25,3]","beta[26,3]",
                        "beta[27,3]","beta[28,3]",
                        "beta[29,3]","beta[30,3]","beta[31,3]"))

plot(hybrid1_fit,pars=c("beta[1,4]","beta[2,4]",
                        "beta[3,4]","beta[4,4]",
                        "beta[5,4]","beta[6,4]",
                        "beta[7,4]","beta[8,4]",
                        "beta[9,4]","beta[10,4]",
                        "beta[11,4]","beta[12,4]",
                        "beta[13,4]","beta[14,4]",
                        "beta[15,4]","beta[16,4]",
                        "beta[17,4]","beta[18,4]",
                        "beta[19,4]","beta[20,4]",
                        "beta[21,4]","beta[22,4]",
                        "beta[23,4]","beta[24,4]",
                        "beta[25,4]","beta[26,4]",
                        "beta[27,4]","beta[28,4]",
                        "beta[29,4]","beta[30,4]","beta[31,4]"))



#get Q vals and PEs for each trial for each subject
library(reshape2)

#function for z normalizing columns in a matrix
colNorm<-function(X){
  c_means<-colMeans(X,na.rm=T)
  c_means<-t(matrix(c_means,length(c_means),dim(X)[1]))
  c_sds<-apply(X,2,sd,na.rm=T)
  c_sds<-t(matrix(c_sds,length(c_sds),dim(X)[1]))
  c_norm<-(X-c_means)/c_sds
  return(c_norm)
}

#normalized Q vals and prediction errors
Q_chosen_norm<-colNorm(Q_chosen_hyb)
Q_unchosen_norm<-colNorm(Q_unchosen_hyb)
Q_diff_norm<-colNorm(Q_chosen_hyb-Q_unchosen_hyb)
PE_norm<-colNorm(pe_hyb)

melted_vals<-cbind(melt(Q_chosen_norm),melt(Q_diff_norm)[,3],melt(PE_norm)[,3])
names(melted_vals)<-c("Trial","Sub","Q_chosen_norm","Q_diff_norm","PE_norm")

for(i in 1:dim(hybrid_data)[1]){
  hybrid_data$Q_chosen_norm[i]<-
    melted_vals$Q_chosen_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                                melted_vals$Trial==hybrid_data$Trial[i] ]
  hybrid_data$Q_diff_norm[i]<-
    melted_vals$Q_diff_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                               melted_vals$Trial==hybrid_data$Trial[i] ]
  hybrid_data$PE_norm[i]<- 
    melted_vals$PE_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                          melted_vals$Trial==hybrid_data$Trial[i] ]
  
  }
