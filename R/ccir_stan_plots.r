
#' @export
ccir_stan_plots <- function(x,dat=dat,type = c('predicted','exploitation','trace','prior.posterior','')) {
		#x = s tan fitted object
		#dat = raw data file for stan
fdir = file.path(project.figuredirectory('bio.lobster'),'ccir')
if(!dir.exists(fdir)) dir.create(fdir,recursive=T, showWarnings=F)

	if(length(x)>1) {fname = x$file; dat = x$dat; x = x$fit;}
if(type == 'predicted') {
	fname = paste('predicted',fname,'png',sep='.')
	png(file.path(fdir,fname))
			a = extract(x,'phat')$phat
			b = apply(a,2,quantile,probs=c(0.025,0.5,0.975))
			  Ylab <- expression(p == c[e] / (c[e] + c[r]))
			 CC = dat$Cuml
			 if(grepl('fish',fname)) CC = dat$land
			plot(CC, dat$p,ylim=c(0,1),type='p',col='red',
		            xlab='Cumulative legal catch (proportion of total)',ylab=Ylab,pch=16)
			lines(CC,b[2,],col='blue',lwd=2)
			lines(CC,b[1,],col='blue',lty=2)
			lines(CC,b[3,],col='blue',lty=2)
		}

		
	if(type=='exploitation') {
			fname = paste('exploitation',fname,'png',sep='.')
			png(file.path(fdir,fname))
	
		 a = extract(x,'ERp')$ERp
		 b = apply(a,2,quantile,probs=c(0.025,0.5,0.975))
	     plot(as.Date(dat$dates), b[2,],ylim=c(0,1),type='l',col='blue',
		            xlab='Date',ylab='Exploitation Index',lwd=2)
			lines(as.Date(dat$dates),b[1,],col='blue',lty=2)
			lines(as.Date(dat$dates),b[3,],col='blue',lty=2)
		}
		
	if(type =='traceplot') {
		fname = paste('traceplot',fname,'png',sep='.')
		png(file.path(fdir,fname))
		if(grepl('beta',fname)) traceplot(x, pars=c('A','B','phi'),inc_warmup=F)
		if(grepl('binomial',fname)) traceplot(x, pars=c('A','B'),inc_warmup=F)
		}
	if(type == 'Temp.by.Expl') {
			par(mar=c(4,5,2,5))
			fname = paste('exploitation.by.temp',fname,'png',sep='.')
			png(file.path(fdir,fname))
	        a = extract(x,'ERp')$ERp
		 	b = apply(a,2,quantile,probs=c(0.025,0.5,0.975))
	        plot(as.Date(dat$dates), b[2,],ylim=c(0,1),type='l',col='blue',
		            xlab='Date',ylab='Exploitation Index',lwd=2)
			lines(as.Date(dat$dates),b[1,],col='blue',lty=2)
			lines(as.Date(dat$dates),b[3,],col='blue',lty=2)
			par(new=T)
			tran = c(0,18)
			plot(as.Date(dat$dates),dat$Temp,col='red',lty=1,type='b',yaxt='n',xaxt='n',xlab='',ylab='',ylim=tran)
			axis(side=4,at=round(seq(tran[1],tran[2],length.out=5),1))
			mtext(side=4,line=3,'Temperature')
			legend('topleft',paste('correlation =', round(cor.test(dat$Temp,b[2,])[[4]],2)),bty='n',cex=0.8)
		}

		
	if(type == 'prior.posterior') {
		  qnt =  c(0.005, 0.995) # quants
   		  addr = c(0.9, 1.1)  # additional range
		if(dat$method=='beta'){ 
			    #phi
					fname1 = paste('phi.pp',fname,'png',sep='.')
					png(file.path(fdir,fname1))
						  ph = extract(x,'phi')$phi
				  		  xrange = range( ph, dat$phip, dat$phipp, na.rm=T )* addr
				      	  xval = seq( xrange[1], xrange[2], length=500 )
				          postdat = hist( ph, breaks=75, plot=FALSE )
		    		      yrange = range( 0, postdat$density, na.rm=T ) * 1.02
		    	          dprior = dunif( xval, min=dat$phip, max=dat$phipp )
					    hist( ph, freq=FALSE, breaks=75, xlim=xrange, ylim=yrange, main="", xlab='phi', ylab="Density", col="lightgray", border="gray")  
		 	   			lines( xval, dprior, col="red", lwd=1, lty="solid" )
		 	   			dev.off()
		 	   			}
 		

 		fname1 = paste('A.pp',fname,'png',sep='.')
		png(file.path(fdir,fname1))
		    	a = extract(x,'A')$A
		      	xrange = c(-5,10)* addr
    			xval = seq( xrange[1], xrange[2], length=500 )
      			dprior = dnorm( xval, mean=dat$Ap, sd=dat$App )
  				postdat = hist( a, breaks=75, plot=FALSE )
    			yrange = range( 0, postdat$density, na.rm=T ) * 1.02
    	     hist( a, freq=FALSE, breaks=75, xlim=xrange, ylim=yrange, main="", xlab='A', ylab="Density", col="lightgray", border="gray")  
     		 lines( xval, dprior, col="red", lwd=1, lty="solid" )
     		 dev.off()

     	fname1 = paste('B.pp',fname,'png',sep='.')
		png(file.path(fdir,fname1))
		    	b = extract(x,'B')$B
		      	xrange = c(-5,10)* addr
    			xval = seq( xrange[1], xrange[2], length=500 )
      			dprior = dnorm( xval, mean=dat$Ap, sd=dat$App )
  				postdat = hist( a, breaks=75, plot=FALSE )
    			yrange = range( 0, postdat$density, na.rm=T ) * 1.02
   		     hist( b, freq=FALSE, breaks=75, xlim=xrange, ylim=yrange, main="", xlab='A', ylab="Density", col="lightgray", border="gray")  
     		lines( xval, dprior, col="red", lwd=1, lty="solid" )
     		dev.off()
			}
		graphics.off()
	}

