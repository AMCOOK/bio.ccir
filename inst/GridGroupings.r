
lf = unique(ccir_data$LFA)

for(i in 1:length(lf)){
	g = subset(ccir_data,LFA == lf[i])
	if(lf[i]==31.1) lf[i] = '31a'
	if(lf[i]==31.2) lf[i] = '31b'
	y = unique(g$YEAR)
	pdf(paste('~/tmp/',lf[i],'.pdf',sep=''))
	for(j in y){
		LobsterMap(lf[i],labels='grid',labcex=0.8)
		h = makePBS(subset(g,YEAR==j),polygon=F)
		l = unique(h$Vessel.Code)
		l = data.frame(Vessel.Code=l,cols=1:length(l))
		h = merge(h,l)
		addPoints(h,pch=16,col=h$cols)
		title(j)
		}
		dev.off()

}


Groupings = list()
#Groupings[[1]] <- list(lfa = 27,G1 = 351:356, G2 = 357:361)
#Groupings[[2]] <- list(lfa = 29,G1 = 343:344, G2 = 342)
#Groupings[[3]] <- list(lfa = 30,G1 = 345:347)
#Groupings[[4]] <- list(lfa = '31a',G1 = 338:339, G2 = 337)
#Groupings[[5]] <- list(lfa = '31b',G1 = 331:334, G2 = 335:336)
#Groupings[[6]] <- list(lfa = 32,G1 = 323:326, G2 = 327:330)
#Groupings[[7]] <- list(lfa = 33,G1 = c(301:310,469:478),G2 = c(311:322,479:484))

Groupings[[1]] <- list(lfa = 27,G1 = 351:355, G2 = 356:361)
Groupings[[2]] <- list(lfa = 29,G1 = 341:344) 
Groupings[[3]] <- list(lfa = 30,G1 = 345:347)
Groupings[[4]] <- list(lfa = '31a',G1 = 337:340)
Groupings[[5]] <- list(lfa = '31b',G1 = 331:336)
Groupings[[6]] <- list(lfa = 32,G1 = 323:330)
Groupings[[7]] <- list(lfa = 33,G1 = c(301:312,469:480,485:492),G2 = c(313:322,481:484))
Groupings[[8]] <- list(lfa = 34, G1 = c(81,91,92,102,103,113,114), G2 = c(124,125,126,127,138,139,140), G3 = c(155:158,173:177))
Groupings[[9]] <- list(lfa = 35, G1 = c(1,2,3,4,6,7), G2 = 14:15, G3 = c(47,56))
save(Groupings, file = file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata'))