#standard RL model
library(rstan)
fit<-load('stanfit_rl')
fit_extract<-extract(standard_fit,permute=T)
Qvals<-apply(fit_extract$Q,c(2,3,4),median)

Q_chosen<-matrix(0,dim(Qvals)[2],dim(Qvals)[1])
Q_unchosen<-Q_chosen
for(i in 1:dim(Qvals)[1]){
  for(j in 1:dim(Qvals)[2]){
    if(choice[i,j]>0){
    Q_chosen[j,i]<-Qvals[i,j,choice[i,j]]
    Q_unchosen[j,i]<-Qvals[i,j,unchoice[i,j]]
    }
    else{
      Q_chosen[j,i]<-NA
      Q_unchosen[j,i]<-NA
    }
  }
}

alpha<-apply(fit_extract$alpha,2,median)
beta<-apply(fit_extract$beta,2,median)
pe<-t(apply(fit_extract$delta,c(2,3),median))

#model with episodic value, no decay
fit2<-load('stanfit_hybridrl')
fit_extract<-extract(hybrid1_fit,permute=T)
Qvals_hybrid<-apply(fit_extract$Q,c(2,3,4),median)

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

alpha_hyb<-apply(fit_extract$alpha,2,median)
pe_hyb<-t(apply(fit_extract$delta,c(2,3),median))
beta_hyb<-apply(fit_extract$beta,c(2,3),median)
Sigma<-apply(fit_extract$Sigma,c(2,3),median)
Omega<-apply(fit_extract$Omega,c(2,3),median)

summary(fit_extract$b_mean)




hist(beta_hyb[,3],xlab="Episodic Effect",main=NULL)
hist(beta_hyb[,1],xlab="Incremental Effect",main=NULL)
hist(fit_extract$Omega[,3,1],xlab="Episodic-Incremental Correlation",main=NULL)
hist(alpha_hyb,xlab="Learning Rate",main=NULL)

plot(hybrid1_fit,pars=c("beta[1,1]","beta[2,1]",
                        "beta[3,1]","beta[4,1]",
                        "beta[5,1]","beta[6,1]",
                        "beta[7,1]","beta[8,1]",
                        "beta[9,1]","beta[10,1]",
                        "beta[11,1]","beta[12,1]",
                        "beta[13,1]","beta[14,1]",
                        "beta[15,1]","beta[16,1]",
                        "beta[17,1]","beta[18,1]",
                        "beta[19,1]","beta[20,1]",
                        "beta[21,1]","beta[22,1]",
                        "beta[23,1]","beta[24,1]",
                        "beta[25,1]","beta[26,1]",
                        "beta[27,1]","beta[28,1]",
                        "beta[29,1]","beta[30,1]","beta[31,1]"))

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




