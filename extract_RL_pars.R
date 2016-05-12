#standard RL model
fit<-load('stanfit_rl')
fit_extract<-extract(stanfit_rl,permute=T)
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
