hybrid_data<-read.delim('https://raw.github.com/rgerraty/rl_flexibility/master/CardGame.txt')

#fix subject 13 runs
hybrid_data$Run[hybrid_data$Sub==13 & hybrid_data$Trial<121 & hybrid_data$Trial>60]<-2

library(rstan)
library(loo)
library(reshape2)
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
hybrid_data$pre_post_rev_enc<- (hybrid_data$EncRevT>mean(hybrid_data$EncRevT,na.rm=T))-.5


lagpad <- function(x, k=1) {
  c(rep(NA, k), x)[1 : length(x)] 
}

hybrid_data$old_pair<-NaN
for(i in 1:length(hybrid_data$Delay)){
  if(i>1 & !is.nan(hybrid_data$Delay[i])){
    if(!is.nan(hybrid_data$Delay[i-1])){
    if(hybrid_data$Delay[i]==hybrid_data$Delay[i-1]){
      hybrid_data$old_pair[i]=1
    }
    }
  }
}


#Lagged outcome variables for regression model
hybrid_data_wide<-dcast(hybrid_data,Trial~Sub,value.var = "Outcome")


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

m_lag<-glmer(StayResp~oneback_outcome+twoback_outcome+threeback_outcome+
               (oneback_outcome+twoback_outcome+threeback_outcome|Sub),
             data=hybrid_data,family=binomial)

m_lagxrev<-glmer(StayResp~I(RevT-12)+oneback_outcome+twoback_outcome+threeback_outcome+I(RevT-12):oneback_outcome+I(RevT-12):twoback_outcome+I(RevT-12):threeback_outcome+
               (I(RevT-12)+oneback_outcome+twoback_outcome+threeback_outcome|Sub),
             data=hybrid_data,family=binomial)

m_prerev_lag<-glmer(StayResp~oneback_outcome+twoback_outcome+threeback_outcome+
                      (oneback_outcome+twoback_outcome+threeback_outcome|Sub),
                    data=hybrid_data,family=binomial,subset=pre_post_rev==-.5)

m_postrev_lag<-glmer(StayResp~oneback_outcome+twoback_outcome+threeback_outcome+
                      (oneback_outcome+twoback_outcome+threeback_outcome|Sub),
                    data=hybrid_data,family=binomial,subset=pre_post_rev==.5)

plot(c(1,2,3),fixef(m_postrev_lag)[2:4],'l',col='blue')
lines(fixef(m_prerev_lag)[2:4],col='red')

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
