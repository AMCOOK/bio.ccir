
#' @export
ccir_stan_summarize <- function(x=fit,dat=dat,save=T, fdir = file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary')){
	if(length(x)>1){ fname=x$file; dat = x$dat; x=x$fit}
	#x is the stan object from ccir_stan
	#dat is the data list input to stan
     				sampler_params <- get_sampler_params(x, inc_warmup = FALSE)
					mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
					 a = rstan::extract(x,'ERp')$ERp
					 b = apply(a,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975))
					 
			out = (list(LFA= dat$LFA, Yr = dat$Yr, Temp = dat$Temp, Grid = paste(dat$Grid, collapse='.'), Seas = paste(dat$Seas, collapse='.'), Sex = dat$Sex, nSamples = dat$N, dates = dat$dates, N.lobsters.Ref = dat$Nrec, N.lobsters.Exp =dat$Nexp , acceptance.rate = mean(mean_accept_stat_by_chain),ERp = b, ERf = b[,ncol(b)]))
	    #fdir = file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary')
	  	if(!dir.exists(fdir)) dir.create(fdir,recursive=T,showWarnings=F)
 		fname = paste(fname,'summary','rdata',sep='.')
  		if(save) save(out,file=file.path(fdir,fname))
  		return(out)

	}