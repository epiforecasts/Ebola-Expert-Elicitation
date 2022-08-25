//
data {
  int<lower=0> R;         // Number of areas
  int<lower=0> T;         // Number of time steps

  int N[R, T];            // Number of cases

  real Nsum[R, T];

  int distrib;

  matrix[R, R] MIJ;              // matrix[R, R] MIJ;
  matrix[R, R] popmat;              // matrix[R, R] popmat;
                 // matrix[R, R] adjmat;
  matrix[R, R] con_mat;
}

parameters {
  real<lower=0> gamma;           //Local transmission
  real<lower=0> beta;             //real<lower=0> beta;
  real<lower=0> alpha_spat;                //real<lower=0> alpha_spat;          //spatial interaction
                       //real<lower=0> alpha_adj;      //adjacency term
  real<lower=0> alpha_con; 
  real<lower=0> epsilon;         //error
  real<lower=0> k;               //real<lower=0> k;            //distance exponent
}

transformed parameters {

  matrix[R, T] foi;             //force of incection matrix

  real s[2];                    //to offer 1 or grav
     matrix[R, R] space;           //spatial interaction matrix

     s[1] = 1.;

      for (i in 1:R) {
        space[i, i] = 0;
        for (j in 1:(i-1)) {
          s[2] = popmat[i,j] /pow(MIJ[i, j], k);
          space[i, j] = max(s);
          space[j, i] = space[i, j];
        }

      }
        space ./= rep_matrix(rep_row_vector(1, R) * space, R);

      


  // calculate force of infection matrix
  foi = (diag_matrix(rep_vector(gamma, R)) +  alpha_spat * space   +  alpha_con * con_mat) *
        to_matrix(Nsum) +
        epsilon;
}




model {
  gamma ~ gamma(1, 1);
   alpha_spat ~ gamma(1, 1);
   k ~ normal(1, 1);   //gamma ~ gamma(1, 1); alpha_spat ~ gamma(1, 1); k ~ normal(1, 1);
       //alpha_adj  ~ gamma(1, 1);
  alpha_con  ~ gamma(1, 1);

  //tau1 ~ normal(1, 1);
  //tau2 ~ normal(1, 1);


  beta ~ gamma(1, 1);    //  beta ~ gamma(1, 1);

  for (t in 2:T) {
    for (i in 1:R) {
      if (distrib == 1){
        N[i, t] ~ neg_binomial_2(foi[i, t], 1 / beta);
      }
      if (distrib == 0)
      {
        N[i, t] ~ poisson(foi[i, t]);
      }
    }
  }
}



generated quantities {

  vector[R * (T-1)] log_lik;
  if (distrib == 1){
      for (i in 1:R) {
        for (t in 2:T) {
          log_lik[(t-2) * R + i] = neg_binomial_2_lpmf(N[i, t] | foi[i, t], 1 / beta);
            }
        }
    }

  if (distrib == 0){
    for (i in 1:R) {
      for (t in 2:T) {
        log_lik[(t-2) * R + i] = poisson_lpmf(N[i, t] | foi[i, t]);
          }
      }
    }
}


