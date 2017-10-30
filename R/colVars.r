#' @export

colVars <- function(a) {
		#from http://www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf for columnwise variances
		
		n <-dim(a)[[1]]
		c <- dim(a)[[2]] 
		return(.colMeans(((a - matrix(.colMeans(a, n, c), nrow = n, ncol =c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))
}