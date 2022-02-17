
#' @export
ccir_inits <- function(dat,A0,B0) {
		p = dat$p
		Cuml = dat$Cuml
		n=length(p)

	suppressWarnings(Res.nls <- nls( p ~ 1/(1 + 1/ (A + B * Cuml/Cuml[n])), start=list(A=A0,B=B0) , nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024, warnOnly = FALSE)))
	return(coef(Res.nls))

}