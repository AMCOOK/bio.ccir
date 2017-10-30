CCIR.ER.Bootstrap <- function(SublegalLegal, alpha = 0.05, nSamples=1000, SaveBootStrapValues=T , Title=NULL ) {

# SaveBootStrapValues=T causes the estimators bootstrap sampling distribution to be saved in a data file and figures to be save in graphics files


            nSamples <- max(nSamples,round(100/alpha))
            Temp <- CCIR.Estimate.ER(SublegalLegal)
            ER.A.B.hat <- c(Temp$ER.final.hat,Temp$A,Temp$B)
            ER.hat <- ER.A.B.hat[1]
            
            ER.A.B.Bootstrap <- matrix(NaN,nr=nSamples,nc=3)
            T0 <- proc.time() + 5
            T00 <- proc.time()
            ii <- 5
            
            for (ii in 1:nSamples) {
                   Tries <- 0
                   Temp <- NA
                while (Tries < 5 & is.na(Temp)) {
                          SublegalLegalSample <- CCIR.SelectBootstrapSample(SublegalLegal)
                          save(SublegalLegalSample,file='Temp.RData')
                    
                  try(Temp <- CCIR.Estimate.ER(SublegalLegalSample,A0=ER.A.B.hat[2],B0=ER.A.B.hat[3]), silent=T) # ,A0=Temp$A,B0=Temp$B
                  Tries <- Tries + 1
                  }
              
                if (!is.na(Temp)) { ER.A.B.Bootstrap[ii,] <- t(as.matrix(c(Temp$ER.final.hat,Temp$A,Temp$B))) }
              
                    T1 <- proc.time()
              
                if (T1[3] > T0[3]+5) {
                      print(paste('Boostrapping iteration ',as.character(ii),' - Time remaining: ',as.character(round((nSamples-ii)*(T1[3]-T00[3])/ii)),'sec.'));
                      T0 <- T1
                  }
              }


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

if (SaveBootStrapValues) {
  if (is.null(Title)) {
          Title <- Sys.time()
          Title <- gsub('-','',Title)
          Title <- gsub(':','',Title)
          }
            x11()
            layout(matrix(1:6,nr=2,byrow=T))
            hist(ER.A.B.Bootstrap[,1],xlab='Exploitation rate',breaks=20,main='');
            hist(ER.A.B.Bootstrap[,2],xlab='A',breaks=20,main='');
            hist(ER.A.B.Bootstrap[,3],xlab='B',breaks=20,main='');
            qqnorm(ER.A.B.Bootstrap[,1],xlab='Exploitation rate');
            qqnorm(ER.A.B.Bootstrap[,2],xlab='A');
            qqnorm(ER.A.B.Bootstrap[,3],xlab='B');
            save(ER.A.B.Bootstrap,file=paste(Title,'BS Samples','RData',sep='.'))
            # print(paste('Saving bootstrap plot in:   ', paste(Title,'Sampling Dist','emf',sep='.')) )
            savePlot(filename = paste(Title,' BS Sampling Dist','.png',sep=''),type = "png")
            print('Done')
        }


return(list(ConfInt = cbind(ER.hat,alpha,ER.CI.left,ER.CI.right), Bias=BSBias , BSSE=BSSE , nSamples=nSamples,
  ER.CI.Percentile=ER.CI.Percentile , Corr.ER.A.B=cor(ER.A.B.Bootstrap)) )
}
