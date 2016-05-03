
data {
	int NS;
	int MT;
	int NC;
	int NT[NS];

	real<lower=-1,upper=1> rew[NS,MT];
	int choice[NS,MT];

}

parameters {

  real<lower=0> a1;
  real<lower=0> a2;
  real b_mean;
  real<lower=0> b_sd;
	

	real<lower=0,upper=1> alpha[NS];
	real beta[NS];

}


transformed parameters{
  real<lower=0, upper=1> Q[NS,MT, NC]; 
  real delta[NS,MT,NC];
  
  for (s in 1:NS) {
  	for (t in 1:NT[s]) {
		  if(t==1){
		    for (c in 1:NC){
		      Q[s,t,c]<-0.5;
		      delta[s,t,c]<-0;
		    }
		  }
		  else {
		    for (c in 1:NC){
		      Q[s,t,c]<-Q[s,t-1,c];
		      delta[s,t,c]<-0;
		    }
		  }
		  if (rew[s,t] >= 0){
		    delta[s,t,choice[s,t]]<-rew[s,t]-Q[s,t,choice[s,t]];
		    
		    Q[s,t,choice[s,t]]<- Q[s,t,choice[s,t]] + alpha[s]*delta[s,t,choice[s,t]];
		  }
		}
}
}


model {
  
  a1 ~ normal(1,1);
  a2 ~ normal(1,1);
	b_mean ~ normal (0,10);
	b_sd ~ cauchy (0,2.5);

  
	for (s in 1:NS) {
		alpha[s] ~ beta(a1,a2);
		beta[s] ~ normal(b_mean,b_sd);
		
		for (t in 1:NT[s]) {
		  if (choice[s,t]>0) {
		    #abs(choice-3) is a hack to get unchosen from chosen;
		    1 ~ bernoulli_logit(beta[s]*(Q[s,t,choice[s,t]]-Q[s,t,abs(choice[s,t]-3)]));
		  }
		}
	}
}
	
		
	