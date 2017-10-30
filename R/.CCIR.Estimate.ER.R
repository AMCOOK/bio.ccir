CCIR.Estimate.ER <- function(SublegalLegal,A0=0.5,B0=-0.5) {

if (dim(SublegalLegal)[2]==2) { 
        CCIR.Estimate.ER.NotExtended(SublegalLegal,A0,B0)
    } else if (dim(SublegalLegal)[2]==4) { 
        CCIR.Estimate.ER.Extended(SublegalLegal,A0,B0)
    } else {
        return(NA)
    } 
  }
