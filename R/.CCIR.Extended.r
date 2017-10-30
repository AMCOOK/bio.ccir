CCIR.Extended <- function(Title=NULL, SublegalLegalFields=c('RefNotExtended','RefAndExtended','NONRefAndExtended','Exp') ,
   DateField='DateField', Bootstrap.CI = F, Figure = T , alpha=0.05 , Dataframe='DataToCCIR' ,  ExtendedOutput=F) {

  #### ONLY WORKS FOR DATA FRAME with proper variable names: c('DateField','RefNotExtended','RefAndExtended','NONRefAndExtended','Exp')
  #### ONLY WORKS WITH SINGLE SUBSET

#  DateField='DateField'
#  SublegalLegalFields <- c('RefNotExtended','RefAndExtended','NONRefAndExtended','Exp')
#  DateField <- 'DateField'
#  Dataframe <- DataToCCIR
#  Title <- NULL
#  Figure <- T
#  SubsettingFields <- NULL
#  Bootstrap.CI = T
#  alpha=0.05
#  rm(Res)


  print('Starting CCIR Extended')

  if (is.null(Title)) { Title <- 'Extended CCIR' }
  ReportFileName <- paste(Title,' CCIR Report','.csv',sep='')
  

  Data <- Dataframe[,c(DateField,SublegalLegalFields)]

  Names <- names(Data)
  
  Report <- data.frame(matrix(NA,nr=1,nc=8))
          names(Report)[1] <- 'n.Obs'
          names(Report)[2] <- 'Expl.rate'
          names(Report)[3] <- 'SE'
          names(Report)[4] <- 'alpha'
          names(Report)[5] <- 'Conf.Int.L'
          names(Report)[6] <- 'Conf.Int.R'
          names(Report)[7] <- 'Boot.Bias'
          names(Report)[8] <- 'PlotFileName1'
          
  

      
    ii <- 1
      Keep <- rep(T,dim(Data)[1])
      Report[ii,1] <- sum(apply(Data[,c('RefNotExtended','RefAndExtended','NONRefAndExtended','Exp')],1,sum)!=0)
         # Count observations wit
         #h positive catches

    if (Report[ii,1]>=20)  { 
    

      rm(Res)
      Res <- CCIR.Estimate.ER(Data[2:5])
      Res
      Report[ii,2] <- round(Res$ER.final.hat,4)
      ExtendedRatio <- Res$ExtendingRatio
    if (Bootstrap.CI) {
      rm(ER.BS)
      ER.BS <- CCIR.ER.Bootstrap.Extended(Data[2:5], alpha = alpha, nSamples=round(100/alpha)  , SaveBootStrapValues=T , Title=Title )
      Report[ii,2:7] <- round(c(ER.BS$ConfInt[1],ER.BS$BSSE,ER.BS$ConfInt[2:4],ER.BS$Bias),4)
    }

    Data$Sublegal <- apply(Data[,c(2,3)],1,sum)
    Data$Legal <- Data[,5]
   
    if (Figure) {
      if (!is.null(DateField)) {
        Data$DateR <- Data[,DateField]
        PlotFileName <- CCIR.Plots(Data[Keep,c('Sublegal','Legal')],DateR=Data$DateR[Keep],Title=paste(Title),
        ExtendedRatio=ExtendedRatio)
        } else {
        PlotFileName <- CCIR.Plots(Data[Keep,c('Sublegal','Legal')],Title=paste(Title),ExtendedRatio=ExtendedRatio)
      }
      Report[ii,8] <- PlotFileName
    }
  } 
  
 write.csv(Report,file=ReportFileName)
return(Report)
}
