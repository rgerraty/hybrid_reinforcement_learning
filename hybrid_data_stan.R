hybrid_data<-read.delim('~/Downloads/CardGame.txt')


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#was originally coding in terms of chosen item 1 ~ p(Chosen | pars)

#abs(choice-3) is a hack to get unchosen from chosen [1,2];
hybrid_data$DeckUnC<-abs(hybrid_data$DeckC-3)

hybrid_data$OldObjC[hybrid_data$OldObjC==0]<--1
hybrid_data$OldObjC[is.na(hybrid_data$OldObjC)]<-0

hybrid_data$ObjPP_C=(hybrid_data$ObjPP-.5)*hybrid_data$OldObjC
hybrid_data$ObjPP_C[is.na(hybrid_data$ObjPP)]<-0

#.5/-.5 coding for interpreting regression coefficients
hybrid_data$OldObjC<-hybrid_data$OldObjC/2

#Now changed coding to p(Choose Red) because the first model weirds people out and doesn't work in lme4
hybrid_data$ChooseRed<-(hybrid_data$DeckC==2)+0

hybrid_data$LuckRed<-((hybrid_data$LuckyDeck==2)-(hybrid_data$LuckyDeck==1))
hybrid_data$LuckRed[is.na(hybrid_data$LuckRed)]<-0

hybrid_data$OldRed<-((hybrid_data$OldDeck==2)-(hybrid_data$OldDeck==1))
hybrid_data$OldRed[is.na(hybrid_data$OldRed)]<-0

hybrid_data$OldValRed<-hybrid_data$OldRed*(hybrid_data$ObjPP-.5)
hybrid_data$OldValRed[is.na(hybrid_data$OldValRed)]<-0

#.5/-.5 coding for interpreting regression coefficients
hybrid_data$LuckRed<-hybrid_data$LuckRed/2
hybrid_data$OldRed<-hybrid_data$OldRed/2

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

#convert data to subjects by trials format for Stan
for (i in 1:NS) {
  NT[i] = nrow(subset(hybrid_data,Sub==subs[i]));
  
  #choice and reward history
  choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckC;
  unchoice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckUnC;
  rew[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$Outcome;
  
  #based on choosing chosen 
  old_choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$OldObjC;
  old_choice_val[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$ObjPP_C;
  
  #based on choosing red
  red_choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$ChooseRed;
  old_red[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$OldRed;
  old_red_val[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$OldValRed;
}

#for skipping missed trials
choice[is.na(choice)]<--1
choice[choice==0]<--1
unchoice[is.na(unchoice)]<--1
unchoice[choice==0]<--1
rew[is.na(rew)]<--1
red_choice[is.na(red_choice)]<--1





#standard rl model fit heirarchically in Stan
#no information about individual objects/episidoc value
standard_standata = list(NS=NS, NC=2, MT=MT, NT= NT, choice=choice, red_choice=red_choice, rew=rew )
standard_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/standard_rl.stan', data = standard_standata, iter = 1250, warmup = 250, chains = 4)
save(standard_fit,file='stanfit_rl2')

#"hybrid" RL model fit heirarchically in Stan
hybrid_standata = list(NS=NS, NC=2,K=4, MT=MT, NT= NT, choice=choice, red_choice=red_choice, rew=rew, old_red_val=old_red_val, old_red=old_red)
hybrid1_fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/hybrid1_rl.stan', data = hybrid_standata, iter = 1250, warmup = 250, chains = 4)
save(hybrid1_fit,file='stanfit_hybridrl')




