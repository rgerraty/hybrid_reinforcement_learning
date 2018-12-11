hybrid_data<-read.delim('https://raw.github.com/rgerraty/rl_flexibility/master/CardGame.txt')

#fix subject 13 runs
hybrid_data$Run[hybrid_data$Sub==13 & hybrid_data$Trial<121 & hybrid_data$Trial>60]<-2

library(rstan)
library(loo)
library(reshape2)
library(scales)
library(ggplot2)
library(gridExtra)
library(lme4)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#was originally coding in terms of chosen item 1 ~ p(Chosen | pars)

#abs(choice-3) is a hack to get unchosen from chosen [1,2];
hybrid_data$DeckUnC<-abs(hybrid_data$DeckC-3)

#code choices in terms of red deck (choose red, lucky red &c.)
hybrid_data$ChooseRed<-(hybrid_data$DeckC==2)+0
hybrid_data$LuckRed<-((hybrid_data$LuckyDeck==2)-(hybrid_data$LuckyDeck==1))/2
hybrid_data$LuckRed[is.na(hybrid_data$LuckRed)]<-0

hybrid_data$OldRed<-((hybrid_data$OldDeck==2)-(hybrid_data$OldDeck==1))
hybrid_data$OldRed[is.na(hybrid_data$OldRed)]<-0

#basically interacting OldRed with ep. value to get contribution of ep. value to choosing red
#need to zero-center to make OldRed interpretable as main effect
hybrid_data$OldValRed<-hybrid_data$OldRed*(hybrid_data$ObjPP-.5)
hybrid_data$OldValRed[is.na(hybrid_data$OldValRed)]<-0

hybrid_data$OldRed<-hybrid_data$OldRed/2

#trials old objects were first seen
hybrid_data$EncT<-hybrid_data$Trial-hybrid_data$Delay;

#pre vs post reversal
hybrid_data$pre_post_rev<- (hybrid_data$RevT>median(hybrid_data$RevT))-.5

#old objects were first seen pre vs post reversal
hybrid_data$pre_post_rev_enc<- (hybrid_data$EncRevT>median(hybrid_data$EncRevT,na.rm=T))-.5


#Lagged outcome variables for regression model
hybrid_data_wide<-dcast(hybrid_data,Trial~Sub,value.var = "Outcome")-.5


nsub<-length(unique(hybrid_data$Sub))
cutpoint<-which(hybrid_data$Sub==17 & hybrid_data$Trial==240)

hybrid_data_oneback<-rbind(rep(NaN,nsub),
                           hybrid_data_wide[1:(nrow(hybrid_data_wide)-1),2:ncol(hybrid_data_wide)])

hybrid_data$oneback_outcome<-melt(hybrid_data_oneback)$value[-(cutpoint+1):-(cutpoint+61)]

hybrid_data_twoback<-rbind(rep(NaN,nsub),
                           hybrid_data_oneback[1:(nrow(hybrid_data_oneback)-1),])

hybrid_data$twoback_outcome<-melt(hybrid_data_twoback)$value[-(cutpoint+1):-(cutpoint+61)]

hybrid_data_threeback<-rbind(rep(NaN,nsub),
                             hybrid_data_twoback[1:(nrow(hybrid_data_twoback)-1),])

hybrid_data$threeback_outcome<-melt(hybrid_data_threeback)$value[-(cutpoint+1):-(cutpoint+61)]


hybrid_data_fourback<-rbind(rep(NaN,nsub),
                             hybrid_data_threeback[1:(nrow(hybrid_data_threeback)-1),])

hybrid_data$fourback_outcome<-melt(hybrid_data_fourback)$value[-(cutpoint+1):-(cutpoint+61)]

#same but for choice
hybrid_data_wide<-2*(dcast(hybrid_data,Trial~Sub,value.var = "ChooseRed")-.5)

hybrid_data_oneback<-rbind(rep(NaN,nsub),
                           hybrid_data_wide[1:(nrow(hybrid_data_wide)-1),2:ncol(hybrid_data_wide)])

hybrid_data$oneback_choosered<-melt(hybrid_data_oneback)$value[-(cutpoint+1):-(cutpoint+61)]

hybrid_data_twoback<-rbind(rep(NaN,nsub),
                           hybrid_data_oneback[1:(nrow(hybrid_data_oneback)-1),])

hybrid_data$twoback_choosered<-melt(hybrid_data_twoback)$value[-(cutpoint+1):-(cutpoint+61)]

hybrid_data_threeback<-rbind(rep(NaN,nsub),
                             hybrid_data_twoback[1:(nrow(hybrid_data_twoback)-1),])

hybrid_data$threeback_choosered<-melt(hybrid_data_threeback)$value[-(cutpoint+1):-(cutpoint+61)]


hybrid_data_fourback<-rbind(rep(NaN,nsub),
                             hybrid_data_threeback[1:(nrow(hybrid_data_twoback)-1),])

hybrid_data$fourback_choosered<-melt(hybrid_data_fourback)$value[-(cutpoint+1):-(cutpoint+61)]

hybrid_data$RevT_c<-hybrid_data$RevT-12
hybrid_data$EncRevT_c<-hybrid_data$EncRevT-12

hybrid_data$oneback_outcomexchred<-hybrid_data$oneback_choosered*hybrid_data$oneback_outcome


m_rc_lag<-glmer(ChooseRed~oneback_outcome:oneback_choosered+twoback_outcome:twoback_choosered+threeback_outcome:threeback_choosered+fourback_outcome:fourback_choosered+OldValRed+
                  (oneback_outcome:oneback_choosered+twoback_outcome:twoback_choosered+threeback_outcome:threeback_choosered+fourback_outcome:fourback_choosered+OldValRed|Sub),
                data=hybrid_data,family=binomial)


m_rc_lagxrev<-glmer(ChooseRed~oneback_outcomexchred*RevT_c+twoback_outcome:twoback_choosered*RevT_c+threeback_outcome:threeback_choosered*RevT_c+fourback_outcome:fourback_choosered*RevT_c+
                  (oneback_outcomexchred+twoback_outcome:twoback_choosered+threeback_outcome:threeback_choosered+fourback_outcome:fourback_choosered+RevT_c|Sub),
                data=hybrid_data,family=binomial)


m_prerev_lag<-glmer(ChooseRed~oneback_outcome:oneback_choosered+twoback_outcome:twoback_choosered+threeback_outcome:threeback_choosered+OldValRed+fourback_outcome:fourback_choosered+OldT+
                      (oneback_outcome:oneback_choosered+twoback_outcome:twoback_choosered+threeback_outcome:threeback_choosered+fourback_outcome:fourback_choosered+OldValRed+OldT|Sub),
                    data=hybrid_data,family=binomial,subset=pre_post_rev==-.5)# & OldT==0)

m_postrev_lag<-glmer(ChooseRed~oneback_outcome:oneback_choosered+twoback_outcome:twoback_choosered+threeback_outcome:threeback_choosered+OldValRed+fourback_outcome:fourback_choosered+OldT+
                       (oneback_outcome:oneback_choosered+twoback_outcome:twoback_choosered+threeback_outcome:threeback_choosered+OldValRed+fourback_outcome:fourback_choosered+OldT|Sub),
                     data=hybrid_data,family=binomial,subset=pre_post_rev==.5)# & OldT==0)

summary(m_rc_lagxrev)

ggdat_pre<-data.frame(Lag=c("1","2","3","4"))
ggdat_pre$Lag<-factor(ggdat_pre$Lag,c("1","2","3","4"))
ggdat_pre$Choice_Effect<-fixef(m_prerev_lag)[c(4:7)]
ggdat_pre$SE<-sqrt(diag(vcov(m_prerev_lag)))[c(4:7)]
ggdat_pre$prepost<-"PRE"

  
g_pre<-ggplot(data=ggdat_pre,aes(x=Lag,y=Choice_Effect))+
  geom_bar(stat = 'identity',aes(fill=Lag))+
  geom_errorbar(aes(ymin=Choice_Effect-SE,ymax=Choice_Effect+SE),width=.2)+
  scale_y_continuous(limits = c(0,2),oob = squish)+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  scale_fill_brewer(palette = "Greens")

ggdat_post<-data.frame(Lag=c("1","2","3","4"))
ggdat_post$Lag<-factor(ggdat_post$Lag,c("1","2","3","4"))
ggdat_post$Choice_Effect<-fixef(m_postrev_lag)[c(4:7)]
ggdat_post$SE<-sqrt(diag(vcov(m_postrev_lag)))[c(4:7)]
ggdat_post$prepost<-"POST"

g_post<-ggplot(data=ggdat_post,aes(x=Lag,y=Choice_Effect))+
  geom_bar(stat = 'identity',aes(fill=Lag))+
  geom_errorbar(aes(ymin=Choice_Effect-SE,ymax=Choice_Effect+SE),width=.2)+
  scale_y_continuous(limits = c(0,2),oob = squish)+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  scale_fill_brewer(palette = "Oranges")


grid.arrange(g_pre,g_post,ncol=2)

ggdat_prepost<-rbind(ggdat_pre,ggdat_post)

g_prepost<-ggplot(data=ggdat_prepost,aes(x=Lag,y=Choice_Effect,group=prepost))+
  geom_line(aes(colour=prepost))+
  geom_errorbar(aes(ymin=Choice_Effect-SE,ymax=Choice_Effect+SE,colour=prepost),width=.1)+
  scale_y_continuous(limits = c(0,2),oob = squish)+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  xlab('Trials Back')+ylab('Effect of Feedback')
g_prepost

ggdat_all<-data.frame(Lag=c("1","2","3","4","Old Object"))
ggdat_all$Lag<-factor(ggdat_all$Lag,c("1","2","3","4","Old Object"))
ggdat_all$Choice_Effect<-fixef(m_rc_lag)[c(3:6,2)]
ggdat_all$SE<-sqrt(diag(vcov(m_rc_lag)))[c(3:6,2)]


g_all<-ggplot(data=ggdat_all,aes(x=Lag,y=Choice_Effect,group=1))+
  geom_line(stat = 'identity',position='identity',data=subset(ggdat_all,Lag != "Old Object"))+
  geom_errorbar(aes(ymin=Choice_Effect-SE,ymax=Choice_Effect+SE),width=.2)+
  geom_point(stat = 'identity',position='identity',data=subset(ggdat_all,Lag == "Old Object"))+
  scale_y_continuous(limits = c(0,2),oob = squish)+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  #scale_fill_brewer(palette = "Blues",direction=-1)+
  xlab('Trials Back')+ylab('Effect of Feedback')

g_all

m_mem<-glmer(OldObjC~ObjPP*EncRevT+(ObjPP*EncRevT|Sub),data=hybrid_data,family=binomial)

library(jtools)
interact_plot(m_mem, ObjPP, EncRevT)


m_rc_1backxrev<-glmer(ChooseRed~
                        oneback_outcomexchred*RevT_c+
                        (oneback_outcomexchred*RevT_c||Sub),
                      data=hybrid_data,family=binomial)

summary(m_rc_1backxrev)

m_epvalxrev<-glmer(OldObjC~ObjPP*EncRevT_c+(ObjPP*EncRevT_c|Sub),
                   family=binomial,data=hybrid_data)

summary(m_epvalxrev)

g_all

library(interplot)
int_ep<-interplot(m_epvalxrev,'ObjPP','EncRevT_c')+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  #scale_fill_brewer(palette = "Blues",direction=-1)+
  xlab('Trials Since Reversal (Encoding)')+ylab('Effect of Episodic Value')+
  scale_x_continuous(labels=c(2,7,12,17,25,30))
  
int_inc<-interplot(m_rc_1backxrev, 'oneback_outcomexchred','RevT_c')+
  theme_classic()+theme(text=element_text(size=20),legend.position="none")+
  #scale_fill_brewer(palette = "Blues",direction=-1)+
  xlab('Trials Since Reversal')+ylab('Effect of Previous Deck Feedback')+
  scale_x_continuous(labels=c(2,7,12,17,25,30))


#set up variables in subjects by trials format for Stan
subs = unique(hybrid_data$Sub);
NS = length(subs);
MT=max(hybrid_data$Trial);
NT = array(0,NS);
choice = array(0,c(NS,MT));
unchoice=choice;
red_choice=choice;
rew = array(0.0,c(NS,MT));
old_choice = array(0.0,c(NS,MT));
old_choice_val = array(0.0,c(NS,MT));
old_red = array(0.0,c(NS,MT));
old_red_val = array(0.0,c(NS,MT));
red_choice_prev = array(0.0,c(NS,MT));

old_enc_trial= array(0.0,c(NS,MT));

lagpad <- function(x, k=1) {
  c(rep(NA, k), x)[1 : length(x)] 
}

#convert data to subjects by trials format for Stan
for (i in 1:NS) {
  NT[i] = nrow(subset(hybrid_data,Sub==subs[i]));
  
  #choice and reward history
  choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckC;
  unchoice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckUnC;
  rew[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$Outcome;
  
  #based on choosing red
  red_choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$ChooseRed;
  old_red[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$OldRed;
  old_red_val[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$OldValRed;
  red_choice_prev[i,1:NT[i]] = lagpad(subset(hybrid_data,Sub==subs[i])$ChooseRed)-.5
  
  #old encoding trial
  old_enc_trial[i,1:NT[i]]=subset(hybrid_data,Sub==subs[i])$EncT
}

#for skipping missed trials in stan
choice[is.na(choice)]<--1
choice[choice==0]<--1
unchoice[is.na(unchoice)]<--1
unchoice[choice==0]<--1
rew[is.na(rew)]<--1
red_choice[is.na(red_choice)]<--1
red_choice_prev[is.na(red_choice_prev)]<-0
old_enc_trial[is.nan(old_enc_trial)]<--1;


#standard rl model fit heirarchically in Stan
#no information about individual objects/episidoc value
standard_standata = list(NS=NS, NC=2,K=3, MT=MT, NT= NT, 
                         choice=choice, red_choice=red_choice, 
                         red_choice_prev=red_choice_prev,rew=rew )
standard_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/standard_rl.stan', 
                     data = standard_standata, iter = 1250, warmup = 250, chains = 4)
save(standard_fit,file='~/Documents/Hybrid_RL/stanfit_rl')
#fit<-load('~/Documents/Hybrid_RL/stanfit_rl')
log_lik1<-extract_log_lik(standard_fit)
looc1<-loo(log_lik1,cores=2)
waic1<-waic(log_lik1)

#RL model with episodic value fit heirarchically in Stan
hybrid_standata = list(NS=NS, NC=2,K=5, MT=MT, NT= NT, 
                       choice=choice, red_choice=red_choice, 
                       red_choice_prev=red_choice_prev, rew=rew, 
                       old_red_val=old_red_val, old_red=old_red)
hybrid1_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/hybrid1_rl.stan', 
                    data = hybrid_standata, iter = 1250, warmup = 250, chains = 4)
save(hybrid1_fit,file='~/Documents/Hybrid_RL/stanfit_hybridrl')
#fit2<-load('~/Documents/Hybrid_RL/stanfit_hybridrl')
log_lik2<-extract_log_lik(hybrid1_fit)
looc2<-loo(log_lik2,cores=2)
waic2<-waic(log_lik2)


#"hybrid" RW/PH RL model fit heirarchically in Stan
hybrid_rwph_standata = list(NS=NS, NC=2,K=5, MT=MT, NT= NT, 
                            choice=choice, red_choice=red_choice, 
                            red_choice_prev=red_choice_prev, rew=rew, 
                            old_red_val=old_red_val, old_red=old_red,
                            old_enc_trial=old_enc_trial)
hybrid_rwph_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/RW_PH_hyb.stan', 
                        data = hybrid_rwph_standata, iter = 1250, warmup = 250, chains = 4)
save(hybrid_rwph_fit,file='~/Documents/Hybrid_RL/stanfit_hybrid_RWPH')
log_lik3<-extract_log_lik(hybrid_rwph_fit)
looc3<-loo(log_lik3,cores=2)
waic3<-waic(log_lik3)

compare(waic1,waic2,waic3)

#"hybrid" RW/PH RL model fit heirarchically in Stan
hybrid_rwph_standata = list(NS=NS, NC=2,K=5, MT=MT, NT= NT, 
                            choice=choice, red_choice=red_choice, 
                            red_choice_prev=red_choice_prev, rew=rew, 
                            old_red_val=old_red_val, old_red=old_red,
                            old_enc_trial=old_enc_trial)
hybrid_rwph_wkap_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/RW_PH_hyb_wkap.stan', 
                        data = hybrid_rwph_standata, iter = 1250, warmup = 250, chains = 4)
save(hybrid_rwph_wkap_fit,file='~/Documents/Hybrid_RL/stanfit_hybrid_RWPH_wkap')
log_lik4<-extract_log_lik(hybrid_rwph_wkap_fit)
looc4<-loo(log_lik4,cores=2)
waic4<-waic(log_lik4)

compare(waic1,waic2,waic3,waic4)
