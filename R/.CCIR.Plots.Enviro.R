CCIR.Plots.Enviro <-function(SublegalLegal, EnviroData, Title = NULL){
				if (is.null(Title)) { Title <- format(Sys.time(),"%Y %M %d - %H %M %S" ) }
				
				EnviroFields.n <- dim(EnviroData)[2]
				if (EnviroFields.n>3) {EnviroFields.n <- 3; print('Only the first 3 environmental fields used') }
				
				x11();
				layout(matrix(1:EnviroFields.n,nr=EnviroFields.n))
					ZeroCatch <- apply(SublegalLegal,1,sum)==0
					NonZeroCatch <- !ZeroCatch;
					SublegalLegalNo0s <- SublegalLegal[NonZeroCatch,];
					EnviroData <- EnviroData[NonZeroCatch,]
				
					y <- SublegalLegalNo0s[,2]/apply(SublegalLegalNo0s,1,sum)
					ER.hat <- CCIR.Estimate.ER(SublegalLegal)
					Residuals <- ER.hat$residuals
					Scale <- sqrt(apply(SublegalLegalNo0s,1,sum))
					Scale <- 2*Scale/max(Scale)
				
					ii <- 2
					Temp <- max(abs(Residuals),na.rm=T)
					Ylim <- c(-Temp,Temp)

				for (ii in 1:EnviroFields.n){
							Xlim <- c(min(EnviroData[,ii],na.rm=T),max(EnviroData[,ii],na.rm=T))
							X <- seq(Xlim[1],Xlim[2],length.out =30)
							Res.loess <- loess(Residuals ~ EnviroData[,ii],weights=apply(SublegalLegalNo0s,1,sum), span=0.85)
				
							Y.smooth <- predict(Res.loess,newdata=X)
				
							plot(EnviroData[,ii],Residuals,pch=19,ylab='Residual',xlab=names(EnviroData)[ii], cex=Scale,xlim=Xlim,ylim=Ylim)
				
							par(new=T)
							plot(X,Y.smooth,,ylab='',xlab='',xlim=Xlim,ylim=Ylim,type='l',col='red')
							abline(0,0)
							}
				savePlot(paste(Title,' CCIR Enviro','.png',sep=''),type='png')
				return(paste(Title,' CCIR Enviro',sep=''))
}

