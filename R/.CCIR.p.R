CCIR.p <-function(SublegalLegal,A,B) {

		CumLegal <- cumsum(SublegalLegal[,2])
		CumLegal <- c(0,CumLegal[1:length(CumLegal)-1])
		p.hat <- 1/(1 + 1/ (A + B * CumLegal/CumLegal[length(CumLegal)]))
		return(p.hat)
	}

