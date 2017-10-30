CCIR.p.fit <-function(SublegalLegal,A0=0.5,B0=-0.5){

			Total <- apply(SublegalLegal,1,sum);
			p <- SublegalLegal[,2]/Total;
	
	try(Res.nls <- nls( p ~ CCIR.p(SublegalLegal,A,B), start=list(A=A0,B=B0) , weights=Total , nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024, warnOnly = FALSE)),silent=T)
	
	if (!('Res.nls' %in% ls())) {Res.nls <- NA}
	return(Res.nls)
		}

