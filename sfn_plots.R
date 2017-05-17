#########some ML models for rough characterization/plotting of effects

me_rev2<-glmer(OldObjC ~ ObjPP * EncRevT + ( ObjPP * EncRevT | Sub ),data=hybrid_data,family=binomial)

#basic models for estimating episodic and incremental effects by value
#plot overall effects and see shape

hybrid_data<-read.csv(file = "~/Documents/Hybrid_RL/hybrid_data.csv")


#Episodic (Old) Value
m_mem<-glmer(OldObjC ~ 1+ (1 | Sub) + (1 | ObjPP),data=hybrid_data,family=binomial)
plot(.5+coef(m_mem)$ObjPP$"(Intercept)"/4,type='l')


bin_fun<-function(.,n){
  cutpoints<-quantile(.,(0:n)/n,na.rm=T)
  cut(.,cutpoints,include.lowest=TRUE)
}
Qbins<-bin_fun(hybrid_data$Q_red,n=7)

#Incremental (Q) value
m_inc<-glmer(ChooseRed ~ 1 + (1 | Sub) + (1 | Qbins),data=hybrid_data,family=binomial)
plot(.5+coef(m_inc)$Qbins$"(Intercept)"/4,type='l')

#grab random effects from model (for bootstrapping)
myfun<-function(.){
  unname(coef(.)[2])[[1]]$"(Intercept)"
}

#bootstrap 
boot_inc<-bootMer(m_inc,myfun,use.u=TRUE,type="parametric",nsim=100)
boot_inc_re<- melt(1/(1+exp(-boot_inc$t)))
boot_mem<-bootMer(m_mem,myfun,use.u=TRUE,type="parametric",nsim=100)
boot_mem_re<- melt(1/(1+exp(-boot_mem$t)))


#functions for getting means and CI's from ggplot
stat_sum_single <- function(fun, geom="point",color="black", ...) {
  stat_summary(fun.y=fun, colour=color, geom=geom, size = 2, ...)
}

hi_ci<-function(.){
  quantile(x=.,.975)
}
lo_ci<-function(.){
  quantile(x=.,.025)
}

#plot incremental effect

names(boot_inc_re)<-c(" ","Qval","ChooseRed")
ggplot(boot_inc_re,aes(x=Qval,y=ChooseRed))+
  stat_summary(fun.ymin=lo_ci,fun.ymax=hi_ci,geom="ribbon",
               position=position_dodge(.4),color = NA, size=.5,fill="darkturquoise",alpha=.5)  +
  theme_classic()+
  stat_sum_single(mean,geom="line")+
  theme(legend.position="none",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  xlab("Q-value of Red Deck")+
  ylab("Probability of Red Choice")+ylim(c(0.2,0.7))+
  scale_x_continuous(expand=c(0,0),limits = c(1, 6),labels=seq(0,1,.2))

#plot episodic effect
names(boot_mem_re)<-c(" ","EpVal","ChooseOld")
ggplot(boot_mem_re,aes(x=EpVal,y=ChooseOld))+
  stat_summary(fun.ymin=lo_ci,fun.ymax=hi_ci,geom="ribbon",
               position=position_dodge(.4),color = NA,fill="darkturquoise",alpha=.5)  +
  stat_sum_single(mean,geom="line",color="black")+
  theme_classic()+
  theme(legend.position="none",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  xlab("Value of Old Object")+
  ylab("Probability of Old Choice")+ylim(c(0.2,0.7))+
  scale_x_continuous(expand=c(0,0),limits = c(1, 6),labels=seq(0,1,.2))



#plot cross subject incremental-episodic correlation
beta_hyb<-as.data.frame(beta_hyb)
names(beta_hyb)<-c("Int","Inc","Fam","Ep")

ggplot(data=beta_hyb,aes(x=Inc,y=Ep))+theme_classic()+geom_point(size=3)+
  ylab("Episodic Value Beta")+
  xlab("Q-value Beta")+
  stat_smooth(method=lm,color="darkorchid4",alpha=.5)+
  theme(legend.position="none",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  scale_x_continuous(expand=c(.01,.1))

dat<-NULL
dat$betacorr<-fit_extract$Omega[,4,2]
dat<-as.data.frame(dat)

ggplot(dat,aes(x=betacorr))+geom_density(fill="purple",alpha=.4)+
  theme_classic()+xlab("Episodic-Incremental Correlation")+
  ylab("Density")+
  theme(legend.position="none",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  scale_y_continuous(expand=c(0,0))




m_rev2<-glmer(OldObjC~ pre_post_rev_enc +pre_post_rev+(pre_post_rev_enc+pre_post_rev | Sub)+
               (pre_post_rev_enc+pre_post_rev || ObjPP), data=hybrid_data,family=binomial)

m_rev3<-glmer(OldObjC~ EncRevT +RevT+(EncRevT +RevT | Sub)+
                (EncRevT +RevT | ObjPP), data=hybrid_data,family=binomial)

m_rev4<-glmer(OldObjC~ EncRevT*ObjPP +RevT+(EncRevT*ObjPP +RevT | Sub), data=hybrid_data,family=binomial)

Revbins<-bin_fun(hybrid_data$EncRevT,n=7)

hybrid_data$ObjPP_dm<-hybrid_data$ObjPP-.5
m_rev<-glmer(OldObjC~ ObjPP_dm +RevT+(ObjPP_dm | Sub)+
                (ObjPP_dm | Revbins), data=hybrid_data,family=binomial)

myfun_rev<-function(.){
  unname(coef(.)[2])[[1]]$"ObjPP_dm"
}

boot_rev<-bootMer(m_rev3,myfun_rev,use.u=TRUE,type="parametric",nsim=50)
boot_rev_re<- melt(boot_rev$t)

names(boot_rev_re)<-c(" ","RevTEffect","ObjPP")
ggplot(boot_rev_re,aes(x=RevTEffect,y=ObjPP))+
  stat_sum_single(mean,geom="line",color="black")+
  stat_summary(fun.ymin=lo_ci,fun.ymax=hi_ci,geom="ribbon",
               position=position_dodge(.4),color = NA,fill="blue",alpha=.3)  +
  theme_classic()+
  theme(legend.position="none",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  xlab("Trials Since Reversal (Bin)")+
  ylab("Effect of Object Value")+ylim(c(0.65,0.8))+scale_x_continuous(expand=c(0,0))

#means and CI's for table
library(gridExtra)
RL_data<-data.frame(alpha_mean=fit_extract$a1/(fit_extract$a1+fit_extract$a2),
                    beta_inc=fit_extract$b_mean[,2],beta_ep=fit_extract$b_mean[,4])

hi_ci<-function(.){
  quantile(x=.,.98)
}
lo_ci<-function(.){
  quantile(x=.,.02)
}

rl_table<-data.frame(Variable=c("α","Incremental β","Episodic β"),
                     Mean=unname(colMeans(RL_data)),
                     "2% C.I."=c(lo_ci(RL_data$alpha_mean),
                                 lo_ci(RL_data$beta_inc),
                                 lo_ci(RL_data$beta_ep)),
                     "98% C.I."=c(hi_ci(RL_data$alpha_mean),
                                  hi_ci(RL_data$beta_inc),
                                  hi_ci(RL_data$beta_ep)),
                     check.names=FALSE)

rl_table<-format(rl_table,digits=2)
ggplot()
grid.table(rl_table)

save.image(file="~/Documents/Hybrid_RL/sfnplots.RData")
