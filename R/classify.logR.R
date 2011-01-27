classify.logR <-
function(logR, log.r.cutoff) {
	logR.cutoff = c(log.r.cutoff, Inf)
	id = (logR < logR.cutoff)
	cnv = which(id)[1]
	if (cnv > 1 && is.na(id[2])) { # if we call amplification, but not enough power for even 3 copy, we don't have enough power for anything beyond that
		return(NaN) # NaN for not enough power
	}
	return(cnv)
}

