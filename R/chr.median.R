chr.median <-
function(logR, is.chr) {
	logR = logR[is.chr]
	logR = subset(logR, logR != -Inf & logR != Inf)
	median(logR, na.rm=TRUE)
}

