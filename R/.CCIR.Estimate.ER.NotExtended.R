
  CCIR.Estimate.ER.NotExtended <-function(SublegalLegal,A0=0.5,B0=-0.5) {
          NonZeroCatch <- (apply(SublegalLegal,1,sum) != 0)
          SublegalLegalNo0s <- SublegalLegal[NonZeroCatch,]
          Res <- CCIR.p.fit(SublegalLegalNo0s,A0=A0,B0=B0)
          
          if (class(Res)=='logical')  {
              return(NA)
            } else {
                  p.hat <- fitted(Res)
                  ER.partial.hat <- 1 - (p.hat/(1-p.hat))/(p.hat[1]/(1-p.hat[1]))
                  ER.final.hat <- ER.partial.hat[length(ER.partial.hat)]
                  return( list( ER.final.hat=ER.final.hat, ER.partial.hat = ER.partial.hat , A=coef(Res)[1], B=coef(Res)[2] , p.hat=p.hat , residuals=residuals(Res), NonZeroCatch=NonZeroCatch) )
                }
            }
