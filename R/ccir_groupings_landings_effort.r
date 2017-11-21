#' @export

ccir_groupings_landings_effort <- function(x,ccir_model_summary) {
		# x is the log data
		# ccir_model_summary is the output of ccir_stan_summarize
		fdir = file.path(project.figuredirectory('bio.lobster'),'ccir')

outputs = numeric(21)
				y  = ccir_model_summary
				gr = as.numeric(unlist(strsplit(y$Grid,"\\.")))
				if(y$LFA!=33) x  = subset(x,SYEAR == (y$Yr)) #syear in logs is for season ending
				if(y$LFA==33) x  = subset(x,SYEAR == (y$Yr+1)) #syear in logs is for season ending
				print('need to check the syear out')
				browser()
outputs= c(LFA=y$LFA,Year = y$Yr,gr = y$Grid,Sex = y$Sex,nSexRatioDates = length(y$dates),N.lobsters.Ref = y$N.lobsters.Ref,N.lobsters.Exp = y$N.lobsters.Exp,	ERl = y$ERf[1],ERm = y$ERf[2],ERu = y$ERf[3])

			if(nrow(x)>5) {
				x = subset(x,GRID_NUM %in% gr)

			if(nrow(x)>5) {
					x = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~DATE_FISHED,data=x,FUN=sum)
	outputs = c(outputs,N.log.dates = nrow(x))			

	fname = paste('exploitation.cumeff.cumland','LFA',y$LFA,'Yr',y$Yr,'Grid',y$Grid,'Sex',y$Sex,'png',sep='.')
	png(file.path(fdir,fname))

				
				plot(as.Date(y$dates), y$ERp[2,],ylim=c(0,1),type='l',col='blue',
				            xlab='Date',ylab='Exploitation Index',lwd=2)
					#lines(as.Date(y$dates), y$ERp[1,],col='blue',lty=2)
					#lines(as.Date(y$dates), y$ERp[3,],col='blue',lty=2)
				
					lines(x$DATE_FISHED,cumsum(x$NUM_OF_TRAPS) / sum(x$NUM_OF_TRAPS),col='red',lty=3)
					lines(x$DATE_FISHED,cumsum(x$WEIGHT_KG) / sum(x$WEIGHT_KG),col='black',lty=2)
				dev.off()
		w = data.frame(DATE_FISHED = as.Date(y$date), Exploitation = y$ERp[2,])
	
		x$CumLand = cumsum(x$WEIGHT_KG)
		x$CumEffort = cumsum(x$NUM_OF_TRAPS)
		w = merge(w,x,by='DATE_FISHED')

	outputs = c(outputs,NDatesMerged=nrow(w))
	
		if(nrow(w)>5){
		dE = diff(w$Exploitation)
		dL = diff(w$CumLand / max(w$CumLand))
		dT = diff(w$CumEffort / max(w$CumEffort))

		fname = paste('d1expld1cumland','LFA',y$LFA,'Yr',y$Yr,'Grid',y$Grid,'Sex',y$Sex,'png',sep='.')
		png(file.path(fdir,fname))

				plot(dL,dE,xlab='First Differences Cumulative Landings',ylab='First Differences Modelled Exploitation',pch=16)
				ax = lm(dE~dL)
				abline(ax,col='red')
				cc = round(cor.test(dL,dE)[[4]],2)
				outputs = c(outputs,CorExpLand = cc,aExpLand = coef(ax)[1],bExpLand = coef(ax)[2],R2Expland = summary(ax)$r.squared)
				legend('topleft',paste('correlation =', cc),bty='n',cex=0.8)
		dev.off()

		fname = paste('d1expld1cumeffort','LFA',y$LFA,'Yr',y$Yr,'Grid',y$Grid,'Sex',y$Sex,'png',sep='.')
		png(file.path(fdir,fname))

				plot(dT,dE,xlab='First Differences Cumulative Effort',ylab='First Differences Modelled Exploitation',pch=16)
				ax = lm(dE~dT)
				abline(ax,col='red')
				cc = round(cor.test(dT,dE)[[4]],2)
				outputs = c(outputs,CorExpEff = cc,aExpEff = coef(ax)[1],bExpEff = coef(ax)[2],R2ExpEff = summary(ax)$r.squared)
				legend('topleft',paste('correlation =', cc),bty='n',cex=0.8)
			dev.off()

		fname = paste('d1expld1cpue','LFA',y$LFA,'Yr',y$Yr,'Grid',y$Grid,'Sex',y$Sex,'png',sep='.')
		png(file.path(fdir,fname))
				dE = diff(w$Exploitation)
				dL = diff((w$WEIGHT_KG) / (w$NUM_OF_TRAPS))
				plot(dL,dE,xlab='First Differences CPUE',ylab='First Differences Modelled Exploitation',pch=16)
				abline(lm(dE~dL),col='red')
				cc = round(cor.test(dL,dE)[[4]],2)
				outputs = c(outputs,CorExpCPUE = cc)
				legend('topleft',paste('correlation =', cc),bty='n',cex=0.8)
			dev.off()
				}

			}	
		}
return(outputs)
	}
