hybrid_data<-read.csv2('~/Downloads/CardGame.txt')
library(rstan)

subs = unique(hybrid_data$Sub)

NS = length(subs)

MT=max(hybrid_data$Trial) 

NT = matrix(0,NS)

choice = array(0,c(NS,MT))

rew = array(0,c(NS,MT))



for (i in 1:NS) {
  NT[i] = nrow(subset(hybrid_data,Sub==subs[i]));
  choice[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$DeckC - 1;

  rew[i,1:NT[i]] = subset(hybrid_data,Sub==subs[i])$Outcome;

}

standata = list(NS= NS, MT=MT, NT= NT, choice=choice, rew=rew )

# run four chains in parallel

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fit <- stan(file = 'hybrid_rl.stan', data = standata, iter = 1250, warmup = 250, chains = 4)

