CCIR.ER.Bootstrap.Extended <- function(CountMatrix, alpha = 0.05, nSamples=1000, SaveBootStrapValues=T , Title=NULL) {

# Here are the columns in the correct order:  c('RefNotExtended','RefAndExtended','NONRefAndExtended','Exp')
#CountMatrix <- DataToCCIR[,2:5]
#nSamples <- 100
#alpha <- .20
#
	nSamples <- max(nSamples,round(100/alpha))
	DataExtended <- data.frame(apply(CountMatrixSample[,c('RefAndExtended','NONRefAndExtended')],1,sum),CountMatrixSample[,'Exp'])

	Res <- CCIR.Estimate.ER(DataExtended)
	ExtendedRatio <- Res$p.hat[1]

	DataNotExtended <- data.frame(apply(CountMatrixSample[,c('RefNotExtended','RefAndExtended')],1,sum),CountMatrixSample[,'Exp'])
	Res <- CCIR.Estimate.ER(DataNotExtended)

	Res$ER.Partial.hat.Ext <- Res$ER.Partial.hat*ExtendedRatio
	Res$ER.final.hat.Ext <- Res$ER.final.hat*ExtendedRatio

	Temp <- CCIR.Estimate.ER(CountMatrix)
	ER.A.B.hat <- c(Temp$ER.final.hat,Temp$A,Temp$B,Temp$ER.final.hat)
	ER.hat <- ER.A.B.hat[1]



	ER.A.B.Bootstrap <- matrix(NaN,nr=nSamples,nc=7)
	T0 <- proc.time()
	T00 <- T0
	

for (ii in 1:nSamples)    {
		  CountMatrixSample <- CCIR.SelectBootstrapSample(CountMatrix)
  		  DataExtended <- data.frame(apply(CountMatrixSample[,c('RefAndExtended','NONRefAndExtended')],1,sum),CountMatrixSample[,'Exp'])

			Res <- CCIR.Estimate.ER(DataExtended)
			ExtendedRatio <- Res$p.hat[1]

			ER.A.B.Bootstrap[ii,5] <- ExtendedRatio
			ER.A.B.Bootstrap[ii,6] <- Res$ER.final.hat

			DataNotExtended <- data.frame(apply(CountMatrixSample[,c('RefNotExtended','RefAndExtended')],1,sum),CountMatrixSample[,'Exp'])
			Res <- CCIR.Estimate.ER(DataNotExtended)

			Temp1 <- CCIR.Estimate.ER(CountMatrixSample,A0=Temp$A,B0=Temp$B , Title = Title)
			ER.A.B.Bootstrap[ii,1:3] <- t(as.matrix(c(Temp$ER.final.hat,Temp$A,Temp$B)))

 T1 <- proc.time()
  if (T1[3]>T0[3]+5) { print(paste('Bootstrapping iteration ',as.character(ii),' - Time remaining: ',as.character(round((nSamples-ii)*(T1[3]-T00[3])/ii)),'sec.'))
      T0 <- T1
  			}
		}

ER.A.B.Bootstrap[,7] <- ER.A.B.Bootstrap[,1] * ER.A.B.Bootstrap[,5]

			T1 <- proc.time()
			T1[3]-T00[3]
			Index <- sort(ER.A.B.Bootstrap[,1],index.return = T)
			ER.A.B.Bootstrap <- ER.A.B.Bootstrap[Index$ix,]
			ER.Bootstrap <- ER.A.B.Bootstrap[,1]
			BSBias <- mean(ER.Bootstrap) - ER.hat
			BSSE <- sd(ER.Bootstrap)
			BSMedian <- median(ER.Bootstrap)

# CI based on reflection method
			Left <- ER.hat - ER.Bootstrap[round(nSamples*alpha/2)]
			Right <- ER.Bootstrap[round(nSamples*(1-alpha/2))] - ER.hat

			ER.CI.left <- ER.hat - Right
			ER.CI.right <- ER.hat + Left

# CI based on percentile method
			ER.CI.Percentile <- ER.A.B.Bootstrap[round(nSamples*c(alpha/2,1-alpha/2)),]

return(list(ConfInt = cbind(ER.hat,alpha,ER.CI.left,ER.CI.right), Bias=BSBias , BSSE=BSSE , nSamples=nSamples,  ER.CI.Percentile=ER.CI.Percentile , Corr.ER.A.B=cor(ER.A.B.Bootstrap)) )
	}

