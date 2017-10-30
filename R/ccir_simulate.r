
#' @export
ccir_simulate <- function(a,b,d,phi,x) {

			mu = 1/(1+1/(a+b*x^d)) 
            al = mu * phi
            bet = (1.0 - mu) * phi
            p = rbeta(length(al),al,bet)
      
		return(data.frame(x,p))
}
