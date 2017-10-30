CCIR.Estimate.ER.Extended <- function(CountMatrix,A0=0.5,B0=-0.5)  {
          colnames(CountMatrix) <- c('RefNotExtended','RefAndExtended','NONRefAndExtended','Exp')
          DataExtended <- data.frame(apply(CountMatrix[,c('RefAndExtended','NONRefAndExtended')],1,sum),CountMatrix[,'Exp'])
          Res <- CCIR.Estimate.ER.NotExtended(DataExtended)
      
      if (is.na(Res)) { 
          return(NA)
        } else {
          ExtendingRatio <- Res$p.hat[1]
          DataNotExtended <- data.frame(apply(CountMatrix[,c('RefNotExtended','RefAndExtended')],1,sum),(CountMatrix[,'Exp']))
    
          Res <- CCIR.Estimate.ER.NotExtended(DataNotExtended)
      
      if (is.na(Res)) { 
          return(NA)
      } else {

          Res$ER.partial.hat <- Res$ER.partial.hat*ExtendingRatio
          Res$ER.final.hat <- Res$ER.final.hat*ExtendingRatio

      return( list( ER.final.hat=Res$ER.final.hat, ER.partial.hat = Res$ER.partial.hat ,A=Res$A, B=Res$B , p.hat=Res$p.hat , residuals=Res$residuals, NonZeroCatch=Res$NonZeroCatch, ExtendingRatio=ExtendingRatio ))
        }
      }
    }

