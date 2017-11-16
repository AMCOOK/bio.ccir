#' @export
ccir_compile_data <- function(x=ccir_data, size.defns = inp, area.defns = Groupings,season.defns = NULL,sexs=c(1,2,1.5)) {
				#sex 1.5 is both males and females combined but not berried if want both males and females sep sexs = c(1,2)
				x = subset(x, YEAR > 1999)
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
					 										
					 										lfl = unlist(sapply(season.defns,'[','lfa'))
															iw = season.defns[[which(arn$lfa == lfl)]]
					 											for(b in 2:length(iw)){
					 													ko = subset(ki,YEAR==yr[j] & mns %in% iw[[b]] )
					 													m = m+1
							 											aenv = aggregate(Temperature~DATE,data=subset(ko,Temperature > -99),FUN=mean)
																		iy = subset(size.defns,Year==yr[j] & LFA == lf[i])
																		ko$Ref = ko$Exp = 0 
																		ko$Ref = ifelse(ko$Size >= iy$FSRSRefLower & ko$Size <= iy$FSRSRefUpper & ko$Short==1,1,0)
																		ko$Exp = ifelse(ko$Size >= iy$FSRSExpLower & ko$Size <= iy$FSRSExpUpper & ko$Short==0,1,0)
													 					a = aggregate(cbind(Ref,Exp)~DATE,data=ko,FUN=sum)
																		a $ Total = rowSums(a[,2:3])
																	    ik = which(a$Total<10)
																	    a = a[-ik,]
																		aenv = aenv[which(aenv$DATE %in% a$DATE),] 
										          						CumLegal <- cumsum(a$Exp)
										          						CumLegal <- c(0,CumLegal[1:length(CumLegal)-1])
										            
										          						p = a$Exp/a$Total
																        n = length(p)

										          						io = which(p==1)
								          								p[io] = 0.99
								          								io = which(p==0)
								          								p[io] = 0.01
								          		
																	yrs = year(as.Date(a$DATE[1]))
								          							if(n<10) {m = m-1; next}
													         out[[m]] = list(LFA = lf[i]$lfa,Yr = yrs,Seas = iw[[b]], Grid = uu, Sex = sexs[l], n = n, Cuml=CumLegal/CumLegal[n], p=as.numeric(p), dates = a$DATE, Nrec = sum(a$Ref),Nexp = sum(a$Exp),N = a$Total, E = a$Exp,Temp = aenv$Temperature)
 													}
 												}
 											}
 										}
 									}
 									return(out)
								}

