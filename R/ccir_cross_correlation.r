#' @export

ccir_cross_correlation <- function(x) {
	#x is from ccir_stan_summarize

#needs testing
	ag = x$Temp
	bg = x$ERp[2,]
	n = length(x$Temp)
	agd = ag[2:n] - ag[1:(n-1)]
	bgd = bg[2:n] - bg[1:(n-1)]

	ar1model = arima(ag, order = c(1,1,0)) #account for the within time series autocorrelation with a arima model rather than diffs
	pwx=ar1model$residuals #use the residuals from this model
	newpwy = filter(bg, filter = c(1,-1.7445,.7445), sides =1)
	ccf (pwx,newpwy,ylab= 'cross-correlation',na.action=na.omit)




}

#load(file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','LFA.27.Year.2000.Grid.351.352.353.354.355.356.Sex.1.binomial.summary.rdata'))