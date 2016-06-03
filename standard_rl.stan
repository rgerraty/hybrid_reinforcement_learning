data {
	int NS;//number of subjects
	int MT;//maximum number of trials
	int NC;//number of choices (2)
	int NT[NS];//number of trials per subject
	real<lower=-1,upper=1> rew[NS,MT];//subject x trial reward, -1 for missed
	int choice[NS,MT];//chosen option, -1 for missed
	int red_choice[NS,MT];//1=chose red,0=chose blue, -1 for missed
}

parameters {
  //hyperpriors on alpha distribution
  real<lower=1> a1;
  real<lower=1> a2;
  
  //hyperpriors on beta distribution
  real b_mean;
  real<lower=0> b_sd;
  
  //subject-level alpha and betas
  real<lower=0,upper=1> alpha[NS];
  vector[NS] beta;
	
}


transformed parameters{
  //subject x trials x choice Q value matrix
  real<lower=0, upper=1> Q[NS,MT,NC]; 
  
  //prediction error matrix
  real delta[NS,MT,NC];
  
  //need to define because missing trials will recreate nan's otherwise
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
		    if (rew[s,t] >= 0){
		      //PE = reward-expected
		      delta[s,t,choice[s,t]]<-rew[s,t]-Q[s,t,choice[s,t]];
		      
		      //update value with alpha-weighted PE
		      Q[s,t,choice[s,t]]<- Q[s,t,choice[s,t]] + alpha[s]*delta[s,t,choice[s,t]];
		    }
		  }
		}
  }
}


model {
  
  //hyperpriors
  a1 ~ normal(0,5);
  a2 ~ normal(0,5);
  b_mean ~ normal (0,5);
  b_sd ~ cauchy (0,2.5);
  
  //distributions of subject effects
  alpha ~ beta(a1,a2);
  beta ~ normal(b_mean,b_sd);
  
  
  //data generating process
	for (s in 1:NS) {
		for (t in 1:NT[s]) {
		  if (choice[s,t] > 0) {
		    red_choice[s,t] ~ bernoulli_logit(beta[s]*(Q[s,t,2]-Q[s,t,1]));
		  }
		}
	}
}
