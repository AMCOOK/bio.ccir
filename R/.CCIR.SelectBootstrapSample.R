CCIR.SelectBootstrapSample <- function(SublegalLegal){

      nCol <- dim(SublegalLegal)[2]

      CumN <- cumsum(SublegalLegal);
      NTotal <- sum(SublegalLegal);
      Selection <- sample(1:NTotal,NTotal,replace=T)

      CumColTotal <- cumsum(apply(SublegalLegal,2,sum))
      CumColTotal <- c(0,CumColTotal)

      BootstrapSample <- SublegalLegal*NaN

      jj <- 2
      for (jj in 1:nCol) {
        NSub <- sum(SublegalLegal[,jj]);
        SelectionSub <- Selection[ CumColTotal[jj] < Selection & Selection <= CumColTotal[jj+1] ] - CumColTotal[jj] ;
        NonZero <- which(SublegalLegal[,jj]>0);
        a <- cumsum(SublegalLegal[NonZero,jj]);
        a <- c(0,a[1:length(a)-1]);
        a <- c(a,10^9)
        Res <- hist(SelectionSub,breaks=a,right=F,plot=F);
        SubSample <- Res$counts  # [1:length(Res$count)-1]
        b <- rep(0,dim(SublegalLegal)[1]);
        b[NonZero] <- SubSample;
        BootstrapSample[,jj] <- b;
      }

return(BootstrapSample)
}

