CCIR.RelCumCatch <-function(SublegalLegal) {
			CumLegal <- cumsum(SublegalLegal[,2])
			CumLegal <- c(0,CumLegal[1:length(CumLegal)-1])
			return(CumLegal/CumLegal[length(CumLegal)])
			}

