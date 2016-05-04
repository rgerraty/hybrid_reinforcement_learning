fit<-load('stanfit_rl')
fit_extract<-extract(fit,permute=T)
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
