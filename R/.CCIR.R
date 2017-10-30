CCIR <-function(Filename=NULL, Title=NULL, SublegalLegalFields=c('Sublegal','Legal') , ExtendedField = NULL ,   DateField=NULL , SubsettingFields=NULL ,  EnviroFields=NULL ,
   Bootstrap.CI = F, Figure = T , alpha=0.05 , Dataframe=NULL , ExtendedOutput=F , nSamples=NULL){


print('Starting CCIR')

if (is.null(Title)) { 
    Temp <- as.integer(regexpr('.xls',Filename, ignore.case = T)) 
    Title=substr(Filename,1,Temp-1) 
  }

ReportFileName <- paste(Title,' CCIR Report','.xls',sep='')

Data <- CCIR.ReadFile(Filename, SublegalLegalFields=SublegalLegalFields ,   DateField=DateField , SubsettingFields=SubsettingFields ,  EnviroFields=EnviroFields , Dataframe=Dataframe)


      Names <- names(Data)
      SublegalColumns <- which(Names %in% SublegalLegalFields[1])
      LegalColumns <- which(Names %in% SublegalLegalFields[2])
      SublegalLegalColumns <- c(SublegalColumns,LegalColumns)
      SubsettingColumns <- which(Names %in% SubsettingFields)
      SubsettingColumns.n <- length(SubsettingColumns)
      EnviroColumns <- which(Names %in% EnviroFields)
n <- dim(Data)[1]

if (!is.null(SubsettingFields)) {
          SubsetDataFrame <- unique(Data[,SubsettingColumns]) 
          SubsettingFields.n <- length(SubsettingColumns)
          Digits.n <- ceiling(log(dim(SubsetDataFrame)[1],10))
          Subset.ID <- c()
          for (ii in 1:dim(SubsetDataFrame)[1]) { Subset.ID <- c(Subset.ID, 
          paste('S',substr('0000000000',1,Digits.n-floor(log(ii,10))),as.character(ii),sep='')) }
} else {
     SubsetDataFrame <- data.frame('N/A') ; Subset.ID <- 'S1' 
   }

Report <- data.frame(matrix(NA,nr=dim(SubsetDataFrame)[1],nc=9))
rownames(Report) <- Subset.ID
names(Report)[1] <- 'Subset.ID'
names(Report)[2] <- 'n.Obs'
names(Report)[3] <- 'Expl.rate'
names(Report)[4] <- 'SE'
names(Report)[5] <- 'alpha'
names(Report)[6] <- 'Conf.Int.L'
names(Report)[7] <- 'Conf.Int.R'
names(Report)[8] <- 'PlotFileName1'
names(Report)[9] <- 'PlotFileName2'
# names(Report)[10] <- 'Res.3'

Report[,1] <- Subset.ID

ii <- 1

print('Processing:')

for (ii in 1:dim(Report)[1]) {
  if (dim(Report)[1]>1)  {

      SubsetDesc <- SubsetDataFrame[ii,]
      Keep = apply(as.matrix(Data[,SubsettingColumns]) != matrix(SubsetDesc,nr=n,nc=SubsettingColumns.n,byrow=T),1,sum)==0 
      } else { 
        Keep <- 1:dim(Data)[1] ; SubsetDesc=''
      }
  

      Report[ii,2] <- sum(apply(Data[Keep,SublegalLegalColumns],1,sum)!=0) # Count observations with positive catches
  
  if (Report[ii,2]>=20) {
        Comment=''
    } else {
        Comment='Not processed due to small number of observations'
      }
  print(as.character(c(ii,SubsetDesc,Report[ii,2])))
  
  if (Report[ii,2]>=20)  {
      if (!Bootstrap.CI) {
            Res <- CCIR.Estimate.ER(Data[Keep,SublegalLegalColumns])
              if (!is.null(Res)) {
              Report[ii,3] <- round(Res$ER.final.hat,4)
            }
  } else if (is.null(nSamples)) { 
          nSamples <- round(100/alpha) 
        }
  # print(nSamples)
  
        ER.BS <- CCIR.ER.Bootstrap(Data[Keep,SublegalLegalColumns], alpha = alpha, nSamples=nSamples, SaveBootStrapValues = T , Title=paste(Title,Subset.ID[ii]) )
        Report[ii,3:7] <- round(c(ER.BS$ConfInt[1],ER.BS$BSSE,ER.BS$ConfInt[2:4]),4)
  }

  
  if (Figure)  {
            if (!is.null(DateField)){
                PlotFileName <- CCIR.Plots(Data[Keep,c(SublegalColumns,LegalColumns)],DateR=Data$DateR[Keep],Title=paste(Title,Subset.ID[ii])) 
             } else {
                PlotFileName <- CCIR.Plots(Data[Keep,c(SublegalColumns,LegalColumns)],Title=paste(Title,Subset.ID[ii]))
              }
            Report[ii,8] <- PlotFileName
      if (dim(Report)[1] > 5) {
              graphics.off()
            }
          }

  if (!is.null(EnviroFields))  {
          PlotFileName <- CCIR.Plots.Enviro(Data[Keep,c(SublegalColumns,LegalColumns)],
          Data[Keep, EnviroColumns], Title =paste(Title,Subset.ID[ii]))
          Report[ii,9] <- PlotFileName
          
          if (dim(Report)[1] > 5) {
            graphics.off()
          }
        }
      }   # End Skipping less than 10 observations
    }      # Looping subsets

  if (!is.null(SubsettingFields)) {
    Report <- cbind(SubsetDataFrame,Report) 
    rownames(Report) <- Subset.ID 
    }


write.table(Report,file=ReportFileName,sep='\t')

if (ExtendedOutput) {
      ER.hat <- CCIR.Estimate.ER(Data[Keep,c(SublegalColumns,LegalColumns)])
      Report <- list(Report,ER.hat)
    }
  return(Report)
  
}

