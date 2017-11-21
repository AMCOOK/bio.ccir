#' @export

ccir_collapse_summary <- function(x) {
		#x is the list of out from ccir_stan_summarize
		x = collapse_list(x)
		LFA = unlist(sapply(x,"[",'LFA'))
		da = data.frame(LFA=LFA, Grid = NA, Seas = NA,Sex=NA, Yr= NA, Acceptance.rate=NA, Model = NA, NRef = NA, NExp = NA, ERfl = NA, ERfm=NA, ERfu=NA, Ndates=NA)		 
		 da$Grid = unlist(sapply(x,"[",'Grid'))
		 da$Seas = unlist(sapply(x,"[",'Seas'))
		
		 da$Sex = unlist(sapply(x,"[",'Sex'))
		 da$Yr = unlist(sapply(x,"[",'Yr'))
		 da$Acceptance.rate = unlist(sapply(x,"[",'acceptance.rate'))
		 da$Ndates =  unlist(lapply(sapply(x,"[[",'dates'),function(x) length(x)))
    	 da$NRef = unlist(sapply(x,"[",'N.lobsters.Ref'))
		 da$NExp = unlist(sapply(x,"[",'N.lobsters.Exp'))
		 da$Model = attr(x,'model')
    	  ERf = as.data.frame(t(sapply(x,"[[",'ERf')))
    	  da$ERfl = ERf[,1]
		  da$ERfm = ERf[,2]
		  da$ERfu = ERf[,3]
			return(da)
		}