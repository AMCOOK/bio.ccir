
#' @export
ccir_stan_models <- function(type) {
	mod = switch(type,
            
            beta = '
            data {
              int<lower=1> n;                   // sample size
              vector<lower=0,upper=1>[n] p;     // response 
              vector<lower=0,upper=1>[n] Cuml;                      // predictor matrix
              real Ap; //priors
              real Bp;
              real App;
              real Bpp;
              real phip;
              real phipp;
            }

            parameters {
              real A;                  // reg coefficients
              real B;
              real<lower=0.1> phi;                // dispersion parameter
            }

            model {
              // model calculations
              vector[n] mu;                     // transformed linear predictor
              vector[n] al;             // parameter for beta distn
              vector[n] bet;             // parameter for beta distn
              vector[n] log_lik;             // parameter for beta distn

              for (i in 1:n) { 
                mu[i] = 1/(1 + 1/ (A + B * Cuml[i]));
              }
              al = mu * phi;
              bet = (1.0 - mu) * phi;
              // priors
              A ~ normal(Ap, App);   
              B ~ normal(Bp, Bpp);
             phi ~ uniform(phip,phipp);          // put upper on phi if using this
              
              // likelihood
               p ~ beta(al, bet);
            }
            generated quantities {
                vector[n] phat;
                vector[n] ERp;
          
              for(i in 1:n) {
                    phat[i] = 1/(1 + 1/ (A + B * Cuml[i]));
                    ERp[i] = 1 - (phat[i]/(1-phat[i]))/(phat[1]/(1-phat[1]));

              }
            }',
          normal =  '
          data {
            int<lower=1> n;                   // sample size
            vector<lower=0,upper=1>[n] p;     // response 
            vector<lower=0,upper=1>[n] Cuml;                      // predictor matrix
            real Ap;
            real Bp;
            }

          parameters {
            real A;                  // reg coefficients
            real B;
            real<lower=0> phi; //really sigma but for model call keep it as phi
          }

          model {
            // model calculations
            vector[n] mu;                     // transformed linear predictor

            for (i in 1:n) { 
              mu[i] = 1/(1 + 1/ (A + B * Cuml[i]));
            }
           
           // priors
            A ~ normal(Ap, 5);   
            B ~ normal(Bp, 5);
            phi ~ normal(1, 2); #SD         
            
            // likelihood
            p ~ normal(mu, phi);
          }
          generated quantities {
           vector[n] phat;
           vector[n] ERp;

            for(i in 1:n) {
              phat[i] = 1/(1 + 1/ (A + B * Cuml[i]));
              ERp[i] = 1 - (phat[i]/(1-phat[i]))/(phat[1]/(1-phat[1]));
            }
          }',
      
          binomial = '
          data {
             int<lower=1> n;                   // sample size
            vector<lower=0,upper=1>[n] p;     // response 
            vector<lower=0,upper=1>[n] Cuml;
            int N[n];     //Total
            int E[n];     //Exploitable
            real Ap;
            real Bp;
            real App;
            real Bpp;
            
            }

          parameters {
            real A;                  // reg coefficients
            real B;
          }
     
        transformed parameters {
             vector[n] pp;
          
          for(i in 1:n) {
             pp[i] = 1/(1 + 1/ (A + B * Cuml[i]));
              }
          }
            
        model {
              //priors
              A ~ normal(Ap, App);   
              B ~ normal(Bp, Bpp);
          
              // Likelihood
               E ~ binomial(N, pp);
              }
            
            generated quantities {
                    vector[n] phat;
                    vector[n] ERp;
                    
               for (i in 1:n) {
                                phat[i] = pp[i];
                                ERp[i] = 1 - (phat[i]/(1-phat[i]))/(phat[1]/(1-phat[1]));
                  }   
            }',
             
		)
	return((mod))
}