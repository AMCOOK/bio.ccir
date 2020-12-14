
#' @export
ccir_stan_models_binomial <- function() {
	        bino = '
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
            }'
	return((bino))
}