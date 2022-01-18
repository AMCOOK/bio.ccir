
Seasons = list()



Seasons[[1]] <- list(lfa = 27, G1 = 4:7)
Seasons[[2]] <- list(lfa = 28, G1 = 4:7)
Seasons[[3]] <- list(lfa = 29, G1 = 4:7)
Seasons[[4]] <- list(lfa = 30, G1 = 4:7)
Seasons[[5]] <- list(lfa = '31a', G1 = 4:7)
Seasons[[6]] <- list(lfa = '31b', G1 = 4:7)
Seasons[[7]] <- list(lfa = 32, G1 = 4:7)
Seasons[[8]] <- list(lfa = 33, G1 = c(11,12,1,2,3,4,5))
Seasons[[9]] <- list(lfa = 34, G1 = c(11,12,1,2,3,4,5))
Seasons[[10]] <- list(lfa = 35, G1 = c(10,12,1,2,3,4,5,6))

save(Seasons, file = file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))