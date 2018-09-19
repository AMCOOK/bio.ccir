#' @export
ccir_compile_data <- function(x=ccir_data, log.data = logs, size.defns = inp, area.defns = Groupings,season.defns = NULL,sexs=c(1,2,1.5)) {
				#sex 1.5 is both males and females combined but not berried if want both males and females sep sexs = c(1,2)
				x = subset(x, YEAR > 2000)
				lf = sapply(area.defns,'[','lfa')
					out = list()
					m=0
					 for(i in 1:length(lf)){
					 	arn = area.defns[[i]]
					 	u = length(arn)
					 		 	for(k in 2:u){					 						
					 						uu = arn[[k]]
					 							for(l in 1:length(sexs)){
					 										ss = sexs[l]										
					 								if(sexs == 1.5)	{ss = c(1,2)}										
					 								ki = subset(x, LFA == lf[i] & Grid %in% uu & Sex %in% ss)
					 								ki$mns = as.numeric(month(ki$DATE))
					 								yr = unique(ki$YEAR)
					 									for(j in 1:length(yr)){		
					 									#if(j==18)browser()	
					 										lfl = unlist(sapply(season.defns,'[','lfa'))
															iw = season.defns[[which(arn$lfa == lfl)]]
					 											for(b in 2:length(iw)){
					 												#print(paste("i =",i))
					 												#print(paste("k =",k))
					 												#print(paste("l =",l))
					 												#print(paste("j =",j))
					 												#print(paste("b =",b))
					 													#if(yr[j]==2016 & lf == 27) browser()
					 													ko = subset(ki,YEAR==yr[j] & mns %in% iw[[b]] )
					 													m = m+1
							 											if(nrow(subset(ko,Temperature > -99))>0)aenv = aggregate(Temperature~DATE,data=subset(ko,Temperature > -99),FUN=mean,na.rm=T)
																		else aenv = data.frame(DATE=sort(unique(ko$DATE)),Temperature=NA)
																		iy = subset(size.defns,Year==yr[j] & LFA == lf[i])
																		ko$Ref = ko$Exp = 0 
																		ko$Ref = ifelse(ko$Size >= iy$FSRSRefLower & ko$Size <= iy$FSRSRefUpper & ko$Short==1,1,0)
																		ko$Exp = ifelse(ko$Size >= iy$FSRSExpLower & ko$Size <= iy$FSRSExpUpper & ko$Short==0,1,0)
													 					a = aggregate(cbind(Ref,Exp)~DATE,data=ko,FUN=sum)
																		a $ Total = rowSums(a[,2:3])
																	    ik = which(a$Total<10)
																	    if(length(ik)>0) a = a[-ik,]
																		aenv = aenv[which(aenv$DATE %in% a$DATE),] 
										          						CumLegal <- cumsum(a$Exp)
										          						CumLegal <- c(0,CumLegal[1:length(CumLegal)-1])
										          						p = a$Exp/a$Total
																        n = length(p)
										          						io = which(p==1)
								          								p[io] = 0.99
								          								io = which(p==0)
								          								p[io] = 0.01
								          							yrs = yr[j]
																	#yrs = year(as.Date(a$DATE[1]))
								          							if(n<10) {m = m-1; next}
								          					 lll = NULL
								          					 #print(j)
								          					 #if(j == 18)browser()
								          					 lands = subset(log.data,LFA == lf[i]$lfa & GRID_NUM %in% uu & month(log.data$DATE_FISHED) %in% iw[[b]] & SYEAR==yr[j])
								          					 if(nrow(lands)>0){
								          							 jk = aggregate(WEIGHT_KG~DATE_FISHED,data=lands,FUN=sum)
								          					 		 jk$WEIGHT_KG = cumsum(jk$WEIGHT_KG)
								          					 		 a$DATE = as.Date(a$DATE)
								          					 		 jk = merge(jk,a,all.y=T,by.x='DATE_FISHED',by.y='DATE')
								          					 		 if(all(c(any(is.na(jk$WEIGHT_KG)) , ! all(is.na(jk$WEIGHT_KG)) , length(is.na(jk$WEIGHT_KG))<5))) {
								          					 		  		 	jk$WEIGHT_KG = fillNA(jk[,'DATE_FISHED'],jk[,'WEIGHT_KG'])
								          					 		 		}
								          					 		 lll = jk$WEIGHT_KG/jk$WEIGHT_KG[nrow(jk)]
								          					 		 }
								          					 if(any(is.na(lll))) lll <- NULL
								          					 		 
								          					
								          					 out[[m]] = list(LFA = lf[i]$lfa,Yr = yrs,Seas = iw[[b]], Grid = uu, Sex = sexs[l], n = n, Cuml=CumLegal/CumLegal[n], p=as.numeric(p), dates = a$DATE, Nrec = sum(a$Ref),Nexp = sum(a$Exp),N = a$Total, E = a$Exp,Temp = aenv$Temperature,land = lll)
 													}
 												}
 											}
 										}
 									}
 									return(out)
								}

