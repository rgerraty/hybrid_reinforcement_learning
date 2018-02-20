#standard RL model
library(rstan)
library(loo)
library(lme4)
fit<-load('~/Documents/Hybrid_RL/stanfit_rl')

#model with episodic value, no decay
fit2<-load('~/Documents/Hybrid_RL/stanfit_hybridrl')

fit_extract<-extract(hybrid1_fit,permute=T)
Qvals_hybrid<-apply(fit_extract$Q,c(2,3,4),mean)


#Separate Q value arrays for chosen and unchosen options
Q_chosen_hyb<-matrix(0,dim(Qvals_hybrid)[2],dim(Qvals_hybrid)[1])
Q_unchosen_hyb<-Q_chosen_hyb
Q_red<-Q_chosen_hyb
Q_blue<-Q_chosen_hyb

for(i in 1:dim(Qvals_hybrid)[1]){
  for(j in 1:dim(Qvals_hybrid)[2]){
    if(choice[i,j]>0){
      Q_chosen_hyb[j,i]<-Qvals_hybrid[i,j,choice[i,j]]
      Q_unchosen_hyb[j,i]<-Qvals_hybrid[i,j,unchoice[i,j]]
      Q_red[j,i]<-Qvals_hybrid[i,j,2]
      Q_blue[j,i]<-Qvals_hybrid[i,j,1]
    } 
      else{
      Q_chosen_hyb[j,i]<-NA
      Q_unchosen_hyb[j,i]<-NA
      Q_red[j,i]<-NA
      Q_blue[j,i]<-NA
    }
  }

}
pe_hyb<-t(apply(fit_extract$delta,c(2,3),mean))


lik_inc_hyb<-t(apply(fit_extract$log_lik_inc,c(2,3),mean))
lik_inc_hyb[lik_inc_hyb==0]<-NA
lik_ep_hyb<-t(apply(fit_extract$log_lik_ep,c(2,3),mean))
lik_ep_hyb[lik_ep_hyb==0]<-NA

library(lattice)
library(reshape2)
lik_hyb_melt<-melt(lik_inc_hyb)
names(lik_hyb_melt)<-c("Trial","Subject","Incrementalp")
lik_hyb_melt$Incrementalp<-exp(lik_hyb_melt$Incrementalp)
lik_hyb_melt$Episodicp<-exp(melt(lik_ep_hyb)$value)
lik_hyb_melt$IE_rat<-melt(exp(lik_inc_hyb-lik_ep_hyb))$value

print(xyplot(Incrementalp ~ Trial | Subject,data=lik_hyb_melt,type='l'),
      split=c(1,1,3,1),more=TRUE)
print(xyplot(Episodicp ~ Trial | Subject,data=lik_hyb_melt,type='l'),
      split=c(2,1,3,1),more=TRUE)
print(xyplot(IE_rat ~ Trial | Subject,data=lik_hyb_melt,type='l'),
      split=c(3,1,3,1))

plot(tapply(lik_hyb_melt$Incrementalp,lik_hyb_melt$Trial,mean,na.rm=1),
     type='l',col='green',ylab="Likelihood",xlab="Trial")
lines(tapply(lik_hyb_melt$Episodicp,lik_hyb_melt$Trial,mean,na.rm=1),
      col='purple')
plot(tapply(lik_hyb_melt$IE_rat,lik_hyb_melt$Trial,mean,na.rm=1),type='l',
     ylab="Incremental-Episodic Ratio",xlab="Trial")

lag<-21
subs<-unique(lik_hyb_melt$Subject)
a<-matrix(0,lag*2+1,length(subs))
b<-matrix(0,lag+1,length(subs));c<-b;d<-b;
for(i in seq(1,length(subs),1)){
  a[,i]<-ccf(lik_hyb_melt$Incrementalp[lik_hyb_melt$Subject==subs[i]],
             lik_hyb_melt$Episodicp[lik_hyb_melt$Subject==subs[i]],
             na.action=na.pass,lag.max=lag)$acf
  b[,i]<-acf(lik_hyb_melt$Incrementalp[lik_hyb_melt$Subject==subs[i]],
             na.action=na.pass,lag.max=lag)$acf
  c[,i]<-acf(lik_hyb_melt$Episodicp[lik_hyb_melt$Subject==subs[i]],
             na.action=na.pass,lag.max=lag)$acf
  d[,i]<-acf(lik_hyb_melt$IE_rat[lik_hyb_melt$Subject==subs[i]],
             na.action=na.pass,lag.max=lag)$acf
}

crossc_dat<-melt(a)
names(crossc_dat)<-c("Lag","Subject","CCF")
crossc_dat$Lag<-crossc_dat$Lag-lag-1

autoc_dat<-melt(b)
names(autoc_dat)<-c("Lag","Subject","Incremental_ACF")
autoc_dat$Lag<-autoc_dat$Lag-1
autoc_dat$Episodic_ACF<-melt(c)$value
autoc_dat$IE_Ratio_ACF<-melt(d)$value

hi_ci<-function(.){
  mean(.)+(sd(.)/sqrt(length(.)))
}
lo_ci<-function(.){
  mean(.)-(sd(.)/sqrt(length(.)))
}

g1<-ggplot(data=autoc_dat,aes(x=Lag,y=Incremental_ACF,subset=Lag>0))+ 
  stat_summary(fun.ymin=lo_ci,fun.ymax=hi_ci,
               geom="ribbon",position=position_dodge(.4),
               color = NA, size=.5,fill="orange",alpha=.5)+
  theme_classic()+stat_summary(fun.y=mean,geom="line",size=1)+
    xlim(c(1,lag+1))+ylim(-.3,.8)#+geom_vline(xintercept=0)

g2<-ggplot(data=autoc_dat,aes(x=Lag,y=Episodic_ACF,subset=Lag>0))+ 
  stat_summary(fun.ymin=lo_ci,fun.ymax=hi_ci,
               geom="ribbon",position=position_dodge(.4),
               color = NA, size=.5,fill="green",alpha=.5)+
  theme_classic()+stat_summary(fun.y=mean,geom="line",size=1)+
    xlim(c(1,lag+1))+ylim(-.3,.8)#+geom_vline(xintercept=0)

g3<-ggplot(data=autoc_dat,aes(x=Lag,y=IE_Ratio_ACF,subset=Lag>0))+ 
  stat_summary(fun.ymin=lo_ci,fun.ymax=hi_ci,
               geom="ribbon",position=position_dodge(.4),
               color = NA, size=.5,fill="violet",alpha=.5)+
  theme_classic()+stat_summary(fun.y=mean,geom="line",size=1)+
  xlim(c(1,lag+1))+ylim(-.3,.8)#+geom_vline(xintercept=0)

library(gridExtra)
grid.arrange(g1,g2,g3,nrow=1)

ggplot(data=crossc_dat,aes(x=Lag,y=CCF))+ 
  stat_summary(fun.ymin=lo_ci,fun.ymax=hi_ci,
               geom="ribbon",position=position_dodge(.4),
               color = NA, size=.5,fill="darkorchid4",alpha=.5)+
  theme_classic()+stat_summary(fun.y=mean,geom="line",size=1)+
  geom_vline(xintercept=0)+ylab("Incremental-Episodic Cross-Correlation")




alpha_hyb<-apply(fit_extract$alpha,2,mean)
beta_hyb<-apply(fit_extract$beta,c(2,3),mean)
Sigma<-apply(fit_extract$Sigma,c(2,3),mean)
Omega<-apply(fit_extract$Omega,c(2,3),mean)


#Summary for group level effects and covariance
summary(fit_extract$b_mean)


#pairs(hybrid1_fit,pars="b_mean",labels=c("Intercept","Inverse Temp","Familiarity Bias","Episodic Value"))


hist(fit_extract$b_mean[,2],xlab="Average Incremental Beta",main=NULL)
hist(fit_extract$b_mean[,4],xlab="Average Episodic Beta",main=NULL)
hist(fit_extract$b_mean[,3],xlab="Average Familiarity Beta",main=NULL)
hist(fit_extract$b_mean[,5],xlab="Average Stay Beta",main=NULL)
hist(fit_extract$alpha,xlab="Average Alpha",main=NULL)
hist(fit_extract$Omega[,4,2],xlab="Episodic-Incremental Correlation",main=NULL)

#plot mean estimates of subject-level effects
hist(beta_hyb[,4],xlab="Episodic Effect",main=NULL)
hist(beta_hyb[,2],xlab="Incremental Effect",main=NULL)
hist(alpha_hyb,xlab="Learning Rate",main=NULL)

plot(beta_hyb[,2],beta_hyb[,4])

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
Q_diff<-(Q_chosen_hyb-Q_unchosen_hyb)

tmp_ep_norm<-tapply(lik_hyb_melt$Episodicp,lik_hyb_melt$Subject,
                    function(x){(x-mean(na.omit(x)))/sd(na.omit(x))})

tmp_inc_norm<-tapply(lik_hyb_melt$Incrementalp,lik_hyb_melt$Subject,
                     function(x){(x-mean(na.omit(x)))/sd(na.omit(x))})

tmp_ierat_norm<-tapply(lik_hyb_melt$IE_rat,lik_hyb_melt$Subject,
                     function(x){(x-mean(na.omit(x)))/sd(na.omit(x))})

lik_norm<-NULL
k<-1
for(i in 1:dim(tmp_ep_norm)){
  for(j in 1:length(tmp_ep_norm[[i]])){
    lik_norm$Sub[k]<-i
    lik_norm$Ep_lik_norm[k]<-tmp_ep_norm[[i]][j]
    lik_norm$Inc_lik_norm[k]<-tmp_inc_norm[[i]][j]
    lik_norm$IE_rat_norm[k]<-tmp_ierat_norm[[i]][j]
    k<-k+1
  }
}


melted_vals<-cbind(melt(Q_chosen_norm),melt(Q_unchosen_norm)[,3],
                   melt(Q_diff_norm)[,3],melt(PE_norm)[,3],melt(Q_chosen_hyb)[,3],
                   melt(Q_unchosen_hyb)[,3],melt(Q_diff)[,3],
                   melt(Q_red)[,3],melt(Q_blue)[,3],melt(pe_hyb)[,3],
                   lik_hyb_melt$Incrementalp,lik_hyb_melt$Episodicp,
                   lik_norm$Inc_lik_norm,lik_norm$Ep_lik_norm,
                   lik_hyb_melt$IE_rat,lik_norm$IE_rat_norm)
names(melted_vals)<-c("Trial","Sub","Q_chosen_norm","Q_unchosen_norm",
                      "Q_diff_norm","PE_norm","Q_chosen","Q_unchosen",
                      "Q_diff","Q_red","Q_blue","PE","Inc_lik","Ep_lik",
                      "Inc_lik_norm","Ep_lik_norm","Lik_rat","Lik_rat_norm")

for(i in 1:dim(hybrid_data)[1]){
  hybrid_data$Q_chosen_norm[i]<-
    melted_vals$Q_chosen_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                                melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Q_unchosen_norm[i]<-
    melted_vals$Q_unchosen_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                                melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Q_diff_norm[i]<-
    melted_vals$Q_diff_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                               melted_vals$Trial==hybrid_data$Trial[i] ]
  hybrid_data$PE_norm[i]<- 
    melted_vals$PE_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                          melted_vals$Trial==hybrid_data$Trial[i] ]
  hybrid_data$Q_chosen[i]<- 
    melted_vals$Q_chosen[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                           melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Q_unchosen[i]<- 
    melted_vals$Q_unchosen[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                           melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Q_diff[i]<- 
    melted_vals$Q_diff[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                          melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Q_red[i]<- 
    melted_vals$Q_red[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                         melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Q_blue[i]<- 
    melted_vals$Q_blue[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                        melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$PE[i]<- 
    melted_vals$PE[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] & 
                          melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Inc_lik[i]<-
    melted_vals$Inc_lik[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] &
                          melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Ep_lik[i]<-
    melted_vals$Ep_lik[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] &
                         melted_vals$Trial==hybrid_data$Trial[i]]
  
  hybrid_data$Inc_lik_norm[i]<-
    melted_vals$Inc_lik_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] &
                               melted_vals$Trial==hybrid_data$Trial[i] ]
  
  hybrid_data$Ep_lik_norm[i]<-
    melted_vals$Ep_lik_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] &
                              melted_vals$Trial==hybrid_data$Trial[i]]
  
  
  hybrid_data$Lik_rat[i]<-
    hybrid_data$Inc_lik_norm[i]/hybrid_data$Ep_lik_norm[i]
  
  hybrid_data$Lik_rat_norm[i]<-
    melted_vals$Lik_rat_norm[melted_vals$Sub==as.numeric(as.factor(hybrid_data$Sub))[i] &
                               melted_vals$Trial==hybrid_data$Trial[i]]
}

enc_T<-as.numeric(row.names(hybrid_data))-hybrid_data$Delay

hybrid_data$pe_enc<-0
hybrid_data$encT<-0
hybrid_data$Ep_lik_enc<-0
hybrid_data$Ep_lik_norm_enc<-0

for(i in 1:length(enc_T)){
  if(!is.nan(enc_T[i])){
    hybrid_data$Ep_lik_enc[enc_T[i]]<-hybrid_data$Ep_lik[i]
    hybrid_data$Ep_lik_norm_enc[enc_T[i]]<-hybrid_data$Ep_lik_norm[i]
    hybrid_data$pe_enc[i]<-hybrid_data$PE[enc_T[i]]
    hybrid_data$encT[enc_T[i]]<-1
  }
}

write.csv(x = hybrid_data,file = "~/Documents/Hybrid_RL/hybrid_data.csv",row.names = F)

#glmer likelihood approximation for comparison
me_hybrid<-glmer(ChooseRed ~ LuckRed + OldRed + OldValRed + (LuckRed + OldRed + OldValRed | Sub),data=hybrid_data,family=binomial)

summary(me_hybrid)

me_old_pe<-glmer(OldObjC ~ ObjPP + I(pe_enc^2)+(ObjPP +I(pe_enc^2) | Sub),data=hybrid_data,family=binomial)

me_oldxpe<-glmer(OldObjC ~ ObjPP * I(pe_enc^2) +  
                 (ObjPP*I(pe_enc^2) | Sub),
               data=hybrid_data,family=binomial)
summary(me_old_pe)
summary(me_oldxpe)

#compare bayesian heirarchical fit in stan to MAP approximation, using standard RL
fit<-load('~/Documents/Hybrid_RL/stanfit_rl')
standard_fit_extract<-extract(standard_fit,permute=T)
cor(ranef(me_hybrid)$Sub$OldValRed,ranef(me_hybrid)$Sub$LuckRed)
cor(ranef(me_hybrid)$Sub$OldValRed,beta_hyb[,4])

plot(ranef(me_hybrid)$Sub$OldValRed,beta_hyb[,4])


maps<-read.csv('~/Documents/Hybrid_RL/indiv_fits_10StPts_MAP.csv',header=F)
alpha<-apply(standard_fit_extract$alpha,2,mean)
beta<-apply(standard_fit_extract$beta,c(2,3),mean)

cor(beta[,2],maps$V3)
cor(alpha,maps$V2)

plot(alpha,maps$V2,
     xlab="Stan Estimates (Alpha)",ylab="Indiv. Map Estimates (Alpha)")
plot(beta[,2],maps$V3,
     xlab="Stan Estimates (Beta)",ylab="Indiv. Map Estimates")


#RT plots
m_RT_noold<-lmer(log(RT)~Q_chosen+Q_unchosen+(Q_chosen+Q_unchosen|Sub),data=hybrid_data,subset=OldT==0)

m_RT_Qxoldt<-lmer(log(RT)~Q_chosen*OldT+Q_unchosen*OldT+(Q_chosen+Q_unchosen+OldT|Sub),data=hybrid_data)

m_RToldt_valxoldc<-lmer(log(RT)~Q_chosen*OldObjC+Q_unchosen*OldObjC+ObjPP*OldObjC+(Q_chosen+Q_unchosen+OldObjC+ObjPP|Sub),data=hybrid_data)

m_RToldt_val_oldc<-lmer(log(RT)~Q_chosen+Q_unchosen+ObjPP+(Q_chosen+Q_unchosen+ObjPP|Sub),data=hybrid_data,subset=OldObjC==1)

m_RToldt_val_newc<-lmer(log(RT)~Q_chosen+Q_unchosen+ObjPP+(Q_chosen+Q_unchosen+ObjPP|Sub),data=hybrid_data,subset=OldObjC==0)


ggdat_no<-data.frame(New_v_New=c("Qch","Qunch"))
ggdat_no$New_v_New<-factor(ggdat_no$New_v_New,c("Qch","Qunch"))
ggdat_no$RT_Effect<-fixef(m_RT_noold)[2:3]
ggdat_no$SE<-sqrt(diag(vcov(m_RT_noold)))[2:3]


g_no<-ggplot(data=ggdat_no,aes(x=New_v_New,y=RT_Effect))+
  geom_bar(stat = 'identity',aes(fill=New_v_New))+
  geom_errorbar(aes(ymin=RT_Effect-SE,ymax=RT_Effect+SE),width=.2)+
  scale_y_continuous(limits = c(-.2,.2),oob = squish)+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  scale_fill_brewer(palette = "Greens")

ggdat_oldc<-data.frame(Old_Ch=c("Qch","Qunch","Ep"))
ggdat_oldc$Old_Ch<-factor(ggdat_oldc$Old_Ch,c("Qch","Qunch","Ep"))
ggdat_oldc$RT_Effect<-fixef(m_RToldt_val_oldc)[2:4]
ggdat_oldc$SE<-sqrt(diag(vcov(m_RToldt_val_oldc)))[2:4]


g_oldc<-ggplot(data=ggdat_oldc,aes(x=Old_Ch,y=RT_Effect))+
  geom_bar(stat = 'identity',aes(fill=Old_Ch))+
  geom_errorbar(aes(ymin=RT_Effect-SE,ymax=RT_Effect+SE),width=.2)+
  scale_y_continuous(limits = c(-.2,.2),oob = squish)+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  scale_fill_brewer(palette = "Greens")

ggdat_newc<-data.frame(New_Ch=c("Qch","Qunch","Ep"))
ggdat_newc$New_Ch<-factor(ggdat_newc$New_Ch,c("Qch","Qunch","Ep"))
ggdat_newc$RT_Effect<-fixef(m_RToldt_val_newc)[2:4]
ggdat_newc$SE<-sqrt(diag(vcov(m_RToldt_val_newc)))[2:4]


g_newc<-ggplot(data=ggdat_newc,aes(x=New_Ch,y=RT_Effect))+
  geom_bar(stat = 'identity',aes(fill=New_Ch))+
  geom_errorbar(aes(ymin=RT_Effect-SE,ymax=RT_Effect+SE),width=.2)+
  scale_y_continuous(limits = c(-.2,.2),oob = squish)+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  scale_fill_brewer(palette = "Greens")

grid.arrange(g_no,g_oldc,g_newc,nrow=1)

###################
library(lme4)
m_eplik_oneback<-glmer(ChooseRed ~ oneback_outcome:oneback_choosered + Ep_lik + Ep_lik_enc+
                         oneback_outcome:oneback_choosered:Ep_lik+oneback_outcome:oneback_choosered:Ep_lik_enc+  
                         (oneback_outcome:oneback_choosered + Ep_lik + Ep_lik_enc+
                            oneback_outcome:oneback_choosered:Ep_lik+oneback_outcome:oneback_choosered:Ep_lik_enc | Sub),
                       data=hybrid_data,family=binomial)
summary(m_eplik_oneback)

#Lagged outcome variables for regression model
hybrid_data_wide<-dcast(hybrid_data,Trial~Sub,value.var = "Ep_lik")


nsub<-length(unique(hybrid_data$Sub))
cutpoint<-which(hybrid_data$Sub==17 & hybrid_data$Trial==240)

hybrid_data_oneback<-rbind(rep(NaN,nsub),
                           hybrid_data_wide[1:(nrow(hybrid_data_wide)-1),2:ncol(hybrid_data_wide)])

hybrid_data$oneback_eplik<-melt(hybrid_data_oneback)$value[-(cutpoint+1):-(cutpoint+61)]

hybrid_data_wide<-dcast(hybrid_data,Trial~Sub,value.var = "Ep_lik_enc")


nsub<-length(unique(hybrid_data$Sub))
cutpoint<-which(hybrid_data$Sub==17 & hybrid_data$Trial==240)

hybrid_data_oneback<-rbind(rep(NaN,nsub),
                           hybrid_data_wide[1:(nrow(hybrid_data_wide)-1),2:ncol(hybrid_data_wide)])

hybrid_data$oneback_eplik_enc<-melt(hybrid_data_oneback)$value[-(cutpoint+1):-(cutpoint+61)]

m_eplik_qnext<-glmer(ChooseRed ~ oneback_eplik*Q_red+oneback_eplik_enc*Q_red+
                       (oneback_eplik*Q_red+oneback_eplik_enc*Q_red|Sub),data=hybrid_data,family=binomial)

m_revt_oldval_lastoutcome<-glmer(ChooseRed ~  oneback_outcome:oneback_choosered+RevT+EncRevT+OldValRed+
                                   oneback_outcome:oneback_choosered:RevT+OldValRed:EncRevT+OldValRed:RevT+
                                   (oneback_outcome:oneback_choosered+RevT+EncRevT+OldValRed|Sub),
                                 data=hybrid_data,family=binomial)
