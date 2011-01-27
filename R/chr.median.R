chr.median <-
function(logR, is.chr) {
	median(logR[is.chr], na.rm=TRUE)
}

