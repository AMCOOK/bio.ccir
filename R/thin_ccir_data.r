#' @export
thin_ccir_data = function(ccir_data,p=0.5){
	
	lfas = unique(ccir_data$LFA)
	yrs = unique(ccir_data$YEAR)
	ccir_data_list = list()
	j=1

	for(i in 1:length(lfas)){
		for(y in 1:length(yrs)){
		
			vc = unique(subset(ccir_data,LFA==lfas[i]&YEAR==yrs[y])$Vessel.Code)
			new.vc = sample(vc,round(length(vc)*p))
			ccir_data_list[[j]] = subset(ccir_data,LFA==lfas[i]&YEAR==yrs[y]&Vessel.Code%in%new.vc)
			j=j+1
		}
	}

	thined_ccir_data = do.call("rbind",ccir_data_list)
	return(thined_ccir_data)
}