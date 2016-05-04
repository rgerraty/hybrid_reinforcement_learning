data {
	int NS;
	int MT;
	int NC;
	int NT[NS];

	real<lower=-1,upper=1> rew[NS,MT];
	int choice[NS,MT];
  int unchoice[NS,MT];
}

parameters {

  real<lower=1> a1;
  real<lower=1> a2;
  real b_mean;
  real<lower=0> b_sd;
	
	real<lower=0,upper=1> alpha[NS];
	vector[NS] beta;
	
}


transformed parameters{
  real<lower=0, upper=1> Q[NS,MT,NC]; 
  real delta[NS,MT,NC];
  Q<-rep_array(0.0,NS,MT,NC);
  delta<-rep_array(0.0,NS,MT,NC);
  
  for (s in 1:NS) {
  	for (t in 1:NT[s]) {
		  if(t == 1) {
		    for (c in 1:NC){
		      Q[s,t,c]<-0.5;
		      delta[s,t,c]<-0;
		    }
		  }
		  else {
		    for (c in 1:NC){
		      Q[s,t,c]<-Q[s,t - 1,c];
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
  
  a1 ~ normal(0,5);
  a2 ~ normal(0,5);
	b_mean ~ normal (0,5);
	b_sd ~ cauchy (0,2.5);
  alpha ~ beta(a1,a2);
	beta ~ normal(b_mean,b_sd);
  
	for (s in 1:NS) {
		for (t in 1:NT[s]) {
		  if (choice[s,t] > 0) {
		    1 ~ bernoulli_logit(beta[s]*(Q[s,t,choice[s,t]]-Q[s,t,unchoice[s,t]]));
		  }
		}
	}
}