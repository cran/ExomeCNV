guesstimate.contamination <-
function(logR, region.idx=NULL) {
	if (is.null(region.idx)) region.idx = 1:length(logR)
	med.logR = median(logR[region.idx], na.rm=TRUE)
	if (med.logR < 0) { rho = 0.5 } else { rho = 1.5 }
	return((2**med.logR - rho)/(1 - rho))
}

