//reinforcement learning model with episodic value and familiarity bias
//next version will incorporate decay parameter

data {
	int NS;//number of subjects
	int MT;//maximum number of trials
	int NC;//number of choices (2)
	int K;//number of coefficients for softmax
	int NT[NS];//number of trials per subject
	real<lower=-1,upper=1> rew[NS,MT];//subject x trial reward, -1 for missed
	int choice[NS,MT];//chosen option, -1 for missed
	int unchoice[NS,MT];//unchosen option, -1 for missed
	int<lower=-1, upper=1> old_choice[NS,MT];//1=old chosen;0=both new;-1=old unchosen
	real<lower=-1,upper=.5> old_val[NS,MT];//positive=chosen val;0=both new;negitive=unchosen val
}

parameters {
  //hyperpriors on alpha distribution
  real<lower=1> a1;
  real<lower=1> a2;
  
  //hyperpriors on beta distribution
  vector[K] b_mean;
  vector[K] b_sd;
  
  //subject-level alpha and betas
  real<lower=0,upper=1> alpha[NS];
  matrix[NS,K] beta;
  
  //cholesky factorization of correlation matrix
  cholesky_factor_corr[K] Lcorr;
	
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
		  } else {
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
  Lcorr~lkj_corr_cholesky(2);
  
  //distributions of subject effects
  alpha ~ beta(a1,a2);
  
  for (s in 1:NS){
    beta[s]~multi_normal_cholesky(b_mean,diag_pre_multiply(b_sd,Lcorr));
  }
  
  
  //data generating process
	for (s in 1:NS) {
	  for (t in 1:NT[s]) {
		    if (choice[s,t] > 0) {
		      1 ~ bernoulli_logit(beta[s,1]*(Q[s,t,choice[s,t]]-Q[s,t,unchoice[s,t]])+beta[s,2]*old_choice[s,t]+beta[s,3]*old_val[s,t]);
		      }
    }
  }
}

generated quantities {
  //generate covariance matrix for subject-level effects
  //from cholesky factorization of correlation matrix
  matrix[K,K] Omega;
  matrix[K,K] Sigma;
  
  //get correlation matrix from cholesky
  Omega<-multiply_lower_tri_self_transpose(Lcorr);
  
  //diag_matrix(b_sd)*Omega*diag_matrix(b_sd) to get covariance
  Sigma<-quad_form_diag(Omega,b_sd);
}
