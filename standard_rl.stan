data {
	int NS;//number of subjects
	int MT;//maximum number of trials
	int NC;//number of choices (2)
	int NT[NS];//number of trials per subject
	int K;//number of coefficients for glm
	real rew[NS,MT];//subject x trial reward, -1 for missed
	int choice[NS,MT];//chosen option, -1 for missed
	int red_choice[NS,MT];//1=chose red,0=chose blue, -1 for missed
	real red_choice_prev[NS,MT];//.5=chose red last, -.5=chose blue last, 0=missed last
}

transformed data{
  //get count of number of non-missed trials
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
  Lcorr~lkj_corr_cholesky(2);
  
  //distributions of subject effects
  alpha ~ beta(a1,a2);
  
  for (s in 1:NS){
    beta[s]~multi_normal_cholesky(b_mean,diag_pre_multiply(b_sd,Lcorr));
  }
  
  
  //data generating process (likelihood)
	for (s in 1:NS) {
		for (t in 1:NT[s]) {
		  if (choice[s,t] > 0) {
		    red_choice[s,t] ~ bernoulli_logit(beta[s,1]+
		      beta[s,2]*(Q[s,t,2]-Q[s,t,1])+
		      beta[s,3]*red_choice_prev[s,t]);
		  }
		}
	}
}

generated quantities {
  //generate covariance matrix for subject-level effects
  //from cholesky factorization of correlation matrix
  matrix[K,K] Omega;
  matrix[K,K] Sigma;
  real log_lik[N];
  int n;
  
  log_lik=rep_array(0,N);
  
  //get correlation matrix from cholesky
  Omega=multiply_lower_tri_self_transpose(Lcorr);
  
  //diag_matrix(b_sd)*Omega*diag_matrix(b_sd) to get covariance
  Sigma=quad_form_diag(Omega,b_sd);
  
  n = 1;
  for (s in 1:NS) {
    for (t in 1:NT[s]) {
      if (choice[s,t] > 0) {
         log_lik[n]=bernoulli_logit_lpmf(red_choice[s,t] | 
        beta[s,1] + 
        beta[s,2]*(Q[s,t,2]-Q[s,t,1])+
        beta[s,3]*red_choice_prev[s,t]);
        n = n+1;
      }
    }
  }
}
