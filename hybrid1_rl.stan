//reinforcement learning model with episodic value and familiarity bias
//Raphael Gerraty 2016, Columbia University

data {
	int NS;//number of subjects
	int MT;//maximum number of trials
	int NC;//number of choices (2)
	int K;//number of coefficients for glm
	int NT[NS];//number of trials per subject
	
	real<lower=-1,upper=1> rew[NS,MT];//subject x trial reward history, -1 for missed
	int choice[NS,MT];//chosen option, -1 for missed
	
	//outcome coded in terms of choosing red deck
	int red_choice[NS,MT];//1=chose red,0=chose blue, -1 for missed
	
	//effect of having chosen red deck on last trial- "stay/switch"
	real red_choice_prev[NS,MT];//.5=chose red last,-.5=chose blue last,0=missed last
	
	//effect of familiarity for previously seen objects
	real old_red[NS,MT];//.5=red old;0=both new;-.5=blue old
	
	//difference in centered values for previously seen objects
	real old_red_val[NS,MT];//old_red*(old value-.5) 
}

transformed data{
  //get count of number trials that werent missed (for generating log_lik)
  int N;
  N=0;
  for (s in 1:NS) {
    for (t in 1:NT[s]) {
      if (choice[s,t] > 0) {
        N=N+1;
      }
    }
}
}

parameters {
  //hyperpriors on alpha distribution
  real<lower=0> a1;
  real<lower=0> a2;
  
  //hyperpriors on beta distribution
  vector[K] b_mean;
  vector<lower=0>[K] b_sd;
  
  //subject-level alpha and betas
  real<lower=0,upper=1> alpha[NS];
  matrix[NS,K] beta;
  
  //cholesky factorization of correlation matrix of subject-level estimates
  cholesky_factor_corr[K] Lcorr;
	
}


transformed parameters{
  //subject x trials x choice Q value matrix
  real<lower=0, upper=1> Q[NS,MT,NC]; 
  
  //prediction error matrix
  real delta[NS,MT];
  
  //need to define because missing trials will recreate nan's otherwise
  Q=rep_array(0.0,NS,MT,NC);
  delta=rep_array(0.0,NS,MT);
  
  for (s in 1:NS) {
  	for (t in 1:NT[s]) {
  	  
  	  //set initial values of Q and delta on first trial
		  if(t == 1) {
		    for (c in 1:NC){
		      Q[s,t,c]=0.5;
		    }
		    delta[s,t]=0;
		  }
		    if (rew[s,t] >= 0){
		      //PE = reward-expected
		      delta[s,t]=rew[s,t]-Q[s,t,choice[s,t]];
		      
		      if (t<NT[s]){
		        //update value with alpha-weighted PE
		        Q[s,t+1,choice[s,t]]= Q[s,t,choice[s,t]] + alpha[s]*delta[s,t];
		        
		        //value of unchosen option is not updated
		        Q[s,t+1,abs(choice[s,t]-3)]=Q[s,t,abs(choice[s,t]-3)];
		        
		      }
		    } else {
		        //if no response, keep Q value and set delta to 0
		        if (t<NT[s]){
		          for (c in 1:NC){
		            Q[s,t+1,c]=Q[s,t ,c];
		          }
		        }
		        delta[s,t]=0;
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
  Lcorr ~ lkj_corr_cholesky(2);
  
  //distributions of subject effects
  alpha ~ beta(a1,a2);
  
  for (s in 1:NS){
    beta[s]~multi_normal_cholesky(b_mean,diag_pre_multiply(b_sd,Lcorr));
  }
  
  
  //data generating process (likelihood)
  for (s in 1:NS) {
    for (t in 1:NT[s]) {
      if (choice[s,t] > 0) {
        
        //p(choose Red)=logistic(b0+b1*Qdiff+b2*Old+b3*OldValDiff...)
        red_choice[s,t] ~ bernoulli_logit(beta[s,1] + 
          beta[s,2]*(Q[s,t,2]-Q[s,t,1])+
          beta[s,3]*old_red[s,t]+
          beta[s,4]*old_red_val[s,t]+
          beta[s,5]*red_choice_prev[s,t]);
        }
      }
    }
}

generated quantities {
  //generate covariance matrix for subject-level effects
  //from cholesky factorization of correlation matrix
  matrix[K,K] Omega;
  matrix[K,K] Sigma;
  real log_lik_inc[NS,MT];
  real log_lik_ep[NS,MT];
  real log_lik[N];
  int n;
  
  log_lik_inc=rep_array(0,NS,MT);
  log_lik_ep=rep_array(0,NS,MT);
  log_lik=rep_array(0,N);
  
  //get correlation matrix from cholesky
  Omega=multiply_lower_tri_self_transpose(Lcorr);
  
  //diag_matrix(b_sd)*Omega*diag_matrix(b_sd) to get covariance
  Sigma=quad_form_diag(Omega,b_sd);
  
  n = 1;
  for (s in 1:NS) {
    for (t in 1:NT[s]) {
      if (choice[s,t] > 0) {
        
        log_lik_ep[s,t] = bernoulli_logit_lpmf(red_choice[s,t] | 
        beta[s,4]*old_red_val[s,t]);
        
        log_lik_inc[s,t] = bernoulli_logit_lpmf(red_choice[s,t] |  
        beta[s,2]*(Q[s,t,2]-Q[s,t,1]));
          
        log_lik[n]= bernoulli_logit_lpmf(red_choice[s,t] | 
        beta[s,1] + 
        beta[s,2]*(Q[s,t,2]-Q[s,t,1])+
        beta[s,3]*old_red[s,t]+
        beta[s,4]*old_red_val[s,t]+
        beta[s,5]*red_choice_prev[s,t]);
        n = n + 1; 
        }
      }
    }
}
