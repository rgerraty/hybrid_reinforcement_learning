hybrid_data<-read.delim('~/Downloads/CardGame.txt')


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#abs(choice-3) is a hack to get unchosen from chosen [1,2];
hybrid_data$DeckUnC<-abs(hybrid_data$DeckC-3)

hybrid_data$ObjPP<-hybrid_data$ObjPP-.5

hybrid_data$OldObjC[hybrid_data$OldObjC==0]<--1

hybrid_data$OldObjC[is.na(hybrid_data$OldObjC)]<-0

hybrid_data$ObjPP=hybrid_data$ObjPP*hybrid_data$OldObjC

hybrid_data$ObjPP[is.na(hybrid_data$ObjPP)]<-0


#set up variables in subjects by trials format for Stan
subs = unique(hybrid_data$Sub);
NS = length(subs);
MT=max(hybrid_data$Trial);
NT = array(0,NS);
choice = array(0,c(NS,MT));
unchoice=choice;
rew = array(0.0,c(NS,MT));
old_choice = array(0.0,c(NS,MT));
old_val = array(0.0,c(NS,MT));

#convert data to subjects by trials format for Stan
for (i in 1:NS) {
  NT[i] = nrow(subset(hybrid_data,Sub==subs[i]));
  choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckC;
  #abs(choice-3) is a hack to get unchosen from chosen;
  unchoice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckUnC;
  rew[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$Outcome;
  old_choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$OldObjC;
  old_val[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$ObjPP;
}

#for skipping missed trials
choice[is.na(choice)]<--1
choice[choice==0]<--1
unchoice[is.na(unchoice)]<--1
unchoice[choice==0]<--1
rew[is.na(rew)]<--1






standard_standata = list(NS=NS, NC=2, MT=MT, NT= NT, choice=choice, unchoice=unchoice, rew=rew )

standard_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/standard_rl.stan', data = standata, iter = 1250, warmup = 250, chains = 4)

save(standard_fit,file='stanfit_rl')


hybrid_standata = list(NS=NS, NC=2,K=3, MT=MT, NT= NT, choice=choice, unchoice=unchoice, rew=rew, old_val=old_val, old_choice=old_choice)

hybrid1_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/hybrid1_rl.stan', data = hybrid_standata, iter = 1250, warmup = 250, chains = 4)

save(hybrid1_fit,file='stanfit_hybridrl')




