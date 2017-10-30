#' @export
ccir_stan_run <- function(dat,inits=c(A0=0.5, B0 = 0.5),save =T){

#inital value sampler for stan

   ins <- ccir_inits(dat,A0=inits[1],B0=inits[2])
            initf<- function(chain_id=1) {
                 list(A = rnorm(inits[1],2,1), B = rnorm(inits[2],-3,2), phi = rlnorm(1,.1,.4),D = rnorm(1,1,0.25))
                  }

            init_ll <- function(n_chains) lapply(1:n_chains, function(id) initf(chain_id = id))

dat$Ap = as.numeric(ins[1])
dat$Bp = as.numeric(ins[2])
dat$phip = 0
dat$App = 5
dat$Bpp = 5
dat$phipp = 100
#Model choice
    ccir_stan <- ccir_stan_models(type = dat$method)

#Parameter section
    pars.to.monitor = c('A','B','phi','ERp','preds')
    
    test = stan(model_code = ccir_stan, model_name = dat$method, pars = pars.to.monitor, data = dat, iter = 2000,  warmup=200, thin=1, chains = 1, verbose = F)
    fit = stan(model_code = ccir_stan, model_name = dat$method, fit=test, data = dat, iter = 22000, warmup=2000, thin=20, cores = 4, verbose = F) 

  

  fdir = file.path(project.datadirectory('bio.lobster'),'outputs','ccir')
  if(!dir.exists(fdir)) dir.create(fdir,recursive=T,showWarnings=F)
  fname = paste('LFA',dat$LFA,'Year',dat$Yr,'Grid',paste(dat$Grid,collapse='.'),'Sex',dat$Sex,dat$method,sep='.')
  out = list(fit= fit,dat = dat,file=fname)
  if(save) save(out,file=file.path(fdir,paste(fname,'rdata',sep='.')))
return(out)    
    }