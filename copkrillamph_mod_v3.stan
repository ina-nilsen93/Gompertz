#Appendix S1. Model code.
#// The following is the STAN model code

data {
  int<lower=0> N; // number of years 
  int<lower=0> K; // number of species that are dynamically modelled 
  int<lower=0> cop_N; // number of years with copepod data
  int<lower=0> krill_N; // number of years with krill data
  int<lower=0> amph_N; // number of years with amphipod data
  int<lower=0> cap_N; // number of years with capelin data
  int<lower=0> pc_N; // number of years with polar cod data
  int<lower=0> cop_noNA[cop_N]; // rows (years) with copepod data
  int<lower=0> krill_noNA[krill_N]; // rows (years) with krill data
  int<lower=0> amph_noNA[amph_N]; // rows (years) with amph data
  int<lower=0> cap_noNA[cap_N]; // rows (years) with capelin data
  int<lower=0> pc_noNA[pc_N]; // rows (years) with polar cod data
  vector[cop_N] cop_obs; // observed copepod abundance 
  vector[krill_N] krill_obs; // observed krill biomass 
  vector[amph_N] amph_obs; // observed amphipod biomass 
  vector[cap_N] cap_obs; // observed capelin biomass 
  vector[pc_N] pc_obs; // observed polar cod biomass 
  vector<lower=0>[cop_N] cop_se; // s.e. of cop_obs
  vector<lower=0>[krill_N] krill_se; // s.e. of krill_obs
  vector<lower=0>[amph_N] amph_se; // s.e. of amph_obs
  vector<lower=0>[cap_N] cap_se; // s.e. of cap_obs
  real<lower=0> pc_se; // s.e. of pc_obs
  vector[N] her;  // herring biomass 
  vector[N] cod;  // cod biomass 
  vector[N] ice;  // ice anomaly dec-may 
  vector[N] cap_F;  // capelin fishing mortality 
}

parameters {
  real c10;  // intercept copepods
  real c11;  // autoregressive parameter copepods
  real c13;  // amphipod effect on copepods
  real c14;  // capelin effect on copepods
  real c15;  // polar cod effect on copepods
  real c16;  // ice effect on copepods
  
  real c20;  // intercept krill
  real c22;  // autoregressive parameter krill
  real c24;  // capelin effect on krill
  real c26;  // ice effect on krill
  
  real c30;  // intercept amphipods
  real c31;  // copepod effect on amphipods
  real c33;  // autoregressive parameter amphipods
  real c35;  // polar cod effect on amphipods
  real c36;  // ice effect on amphipods
  
  real c40;  // intercept capelin
  real c41;  // copepod effect on capelin
  real c42;  // krill effect on capelin
  real c44;  // autoregressive parameter capelin
  real c46;  // ice effect on capelin
  real c47;  // cod effect on capelin
  real c48;  // herring effect on capelin
  real c49;  // fishing on capelin
  
  real c50;  // intercept polar cod
  real c51;  // copepod effect on polar cod
  real c53;  // amphipod effect on polar cod
  real c55;  // autoregressive parameter polar cod
  real c56;  // ice effect on polar cod
  real c57;  // cod effect on polar cod
  
  real<lower=0.8> s1o;   // multiplication factor observation error copepods
  real<lower=0.8> s2o;   // multiplication factor observation error krill
  real<lower=0.8> s3o;   // multiplication factor observation error amphipods
  real<lower=0.8> s4o;   // multiplication factor observation error capelin
  real<lower=0.5> s5o;   // multiplication factor observation error polar cod
  
  // covariance matrix process errors of (cop,krill,amph,cap,pc)
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0.1>[K] L_sigma;     
  
  // latent values of (cop,krill,amph,cap,pc)
  matrix<lower = -4, upper = 4>[N,K] Latent;   
}

model {
  matrix[K,K] L_Sigma;
  
  // prior distribution of parameters 
  c10 ~ cauchy(0, 5);              
  c11 ~ cauchy(0, 5);
  c13 ~ cauchy(0, 5);
  c14 ~ cauchy(0, 5);
  c15 ~ cauchy(0, 5);
  c16 ~ cauchy(0, 5);
  c20 ~ cauchy(0, 5);
  c22 ~ cauchy(0, 5);
  c24 ~ cauchy(0, 5);
  c26 ~ cauchy(0, 5);
  c30 ~ cauchy(0, 5);
  c31 ~ cauchy(0, 5);
  c33 ~ cauchy(0, 5);
  c35 ~ cauchy(0, 5);
  c36 ~ cauchy(0, 5);
  c40 ~ cauchy(0, 5);
  c41 ~ cauchy(0, 5);
  c42 ~ cauchy(0, 5);
  c44 ~ cauchy(0, 5);
  c46 ~ cauchy(0, 1);
  c47 ~ cauchy(0, 5);
  c48 ~ cauchy(0, 5);
  c49 ~ normal(-1, .2);
  c50 ~ cauchy(0, 5);
  c51 ~ cauchy(0, 5);
  c53 ~ cauchy(0, 5);
  c55 ~ cauchy(0, 5);
  c56 ~ cauchy(0, 5);
  c57 ~ cauchy(0, 5);
  
  s1o ~ normal(1.2, .2);
  s2o ~ normal(1.2, .2);
  s3o ~ normal(1.2, .2);
  s4o ~ normal(1.2, .2);
  s5o ~ normal(1, .4);
  
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ normal(0, 5);  
  
  // process model
  Latent[1,1] ~ normal(0, 3);
  Latent[1,2] ~ normal(0, 3);
  Latent[1,3] ~ normal(0, 3);
  Latent[1,4] ~ normal(0, 3);
  Latent[1,5] ~ normal(0, 3);
  for(n in 2:N){                                
    // Expected:
      vector[K] Mu;   
    //cop:
      Mu[1] = c10 
      + c11 * Latent[n-1, 1] 
      + c13 * Latent[n-1, 3]  
      + c14 * Latent[n-1, 4] 
      + c15 * Latent[n-1, 5] 
      + c16 * ice[n]; 
      // krill:
        Mu[2] = c20 
        + c22 * Latent[n-1, 2] 
        + c24 * Latent[n-1, 4]  
        + c26 * ice[n];  
        //amph:
          Mu[3] = c30 
          + c31 * Latent[n-1, 1] 
          + c33 * Latent[n-1, 3] 
          + c35 * Latent[n-1, 5] 
          + c36 * ice[n]; 
          // cap:
            Mu[4] = c40 
            + c41 * Latent[n-1, 1] 
            + c42 * Latent[n-1, 2] 
            + c44 * Latent[n-1, 4] 
            + c46 * ice[n]     
            + c47 * cod[n]
            + c48 * her[n] 
            + c49 * cap_F[n] ;
            // pc:
              Mu[5] = c50 
              + c51 * Latent[n-1, 1] 
              + c53 * Latent[n-1, 3]
              + c55 * Latent[n-1, 5] 
              + c56 * ice[n]   
              + c57 * cod[n] ;
              // Latent = Expected + process error:
                Latent[n,] ~ multi_normal_cholesky(Mu, L_Sigma);  
  }
  
  // observation model:
    cop_obs ~ normal(Latent[cop_noNA,1], s1o * cop_se);
  krill_obs ~ normal(Latent[krill_noNA,2], s2o * krill_se);
  amph_obs ~ normal(Latent[amph_noNA,3], s3o * amph_se);
  cap_obs ~ normal(Latent[cap_noNA,4], s4o * cap_se);
  pc_obs ~ normal(Latent[pc_noNA,5], s5o * pc_se);
  
}

generated quantities {
  corr_matrix[K] Omega;
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}

