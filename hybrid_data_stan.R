hybrid_data<-read.delim('~/Downloads/CardGame.txt')


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

subs = unique(hybrid_data$Sub)
NS = length(subs)
MT=max(hybrid_data$Trial) 
NT = array(0,NS)
choice = array(0,c(NS,MT))
unchoice=choice;
rew = array(0.0,c(NS,MT))



for (i in 1:NS) {
  NT[i] = nrow(subset(hybrid_data,Sub==subs[i]));
  choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckC;
  #abs(choice-3) is a hack to get unchosen from chosen;
  unchoice[i,1:NT[i]] = abs(choice[i,1:NT[i]]-3);
  rew[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$Outcome;
}

#for skipping missed trials
choice[is.na(choice)]<--1
unchoice[is.na(unchoice)]<--1
rew[is.na(rew)]<--1

standata = list(NS=NS, NC=2, MT=MT, NT= NT, choice=choice, unchoice=unchoice, rew=rew )

fit <- stan(file = '~/GitHub/hybrid_reinforcement_learning/standard_rl.stan', data = standata, iter = 1250, warmup = 250, chains = 4)

