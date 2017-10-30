CCIR.Plots <- function(SublegalLegal, DateR=NULL, Title = NULL , ExtendedRatio=NULL) {
            
            if (is.null(Title)) { Title <- format(Sys.time(),"%Y %M %d - %H %M %S" ) }
            if (is.null(DateR)) { x11() ;layout(matrix(1:2,nr=2))  
              } else { x11(); layout(matrix(1:3,nr=3)) 
              }


        ZeroCatch <- apply(SublegalLegal,1,sum)==0
        NonZeroCatch <- !ZeroCatch;


          ## Creating Catch number plot
          Event.n <- dim(SublegalLegal)[1]
          Ylim <- c(min(SublegalLegal),max(SublegalLegal))
          Xlim <- c(1,Event.n)
          plot((1:Event.n)[!ZeroCatch],SublegalLegal[!ZeroCatch,1],ylim=Ylim,xlim=Xlim,pch=19,col='red',xlab='',ylab='')
          par(new=T)
          plot((1:Event.n)[!ZeroCatch],SublegalLegal[!ZeroCatch,2],ylim=Ylim,xlim=Xlim,pch=15,col='green',xlab='',ylab='')
          par(new=T)
          plot((1:Event.n)[ZeroCatch],SublegalLegal[ZeroCatch,2],ylim=Ylim,xlim=Xlim,col='blue',
            xlab='Sampling event (sequential no)',ylab='Sublegal (red) and legal (green) catch' )
          if (!is.null(Title)) {title(main=Title)}

          ## Creating the fitted plot
          SublegalLegalNo0s <- SublegalLegal[NonZeroCatch,];
          y <- SublegalLegalNo0s[,2]/apply(SublegalLegalNo0s,1,sum)
          x <- CCIR.RelCumCatch(SublegalLegalNo0s)
          ER.hat <- CCIR.Estimate.ER(SublegalLegal)
          Ylim <- range(y)
          Scale <- sqrt(apply(SublegalLegal,1,sum))
          Scale <- 2*Scale/max(Scale)
          plot(x,y,ylim=Ylim,cex=Scale,xlab='',ylab='')
          par(new=T)
          Ylab <- expression(p == c[e] / (c[e] + c[r]))
          plot(x,ER.hat$p.hat,ylim=Ylim,type='l',col='red',
            xlab='Cumulative legal catch (proportion of total)',ylab=Ylab)

          ## IF Date supplied, creating the partial ER plot
          if (is.null(ExtendedRatio)) { YLab <- 'Estimated exploitation rate'} else { YLab <- 'Est. Extended exploitation rate (red)'}


            if (!is.null(DateR)) {
                   YLim <- c(0,1)
                  if (is.null(ExtendedRatio)) {
                  # YLim <- c(0,max(c(ER.hat$ER.partial.hat)))
                  plot(DateR[NonZeroCatch],ER.hat$ER.partial.hat,type='l',
                    xlab='Date',ylab=YLab,ylim=YLim)
                    } else {
                    # YLim <- c(0,max(c(ER.hat$ER.partial.hat,ER.hat$ER.partial.hat*ExtendedRatio)))
                #  plot(DateR[NonZeroCatch],ER.hat$ER.partial.hat,type='l',
                #    xlab='Date',ylab=YLab,ylim=YLim,col='grey80',lty='dashed')
                #    par(new=T)
                  plot(DateR[NonZeroCatch],ER.hat$ER.partial.hat*ExtendedRatio,type='l',
                    xlab='Date',ylab=YLab,ylim=YLim,col='red',lty='dashed')
                    }
            }

      savePlot(paste(Title,' CCIR','.png',sep=''),type='png')
      return(paste(Title,' CCIR',sep=''))
 
  }

