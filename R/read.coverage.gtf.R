read.coverage.gtf <- 
function(file) {
	gtf = read.gtf(file)
	return(data.frame( probe=gtf$src, 
			   chr=gtf$chr, 
			   probe_start=gtf$start, 
			   probe_end=gtf$end, 
			   targeted.base=gtf$end-gtf$start+1, 
			   sequenced.base=NA, 
			   coverage=round(as.numeric(gtf$avgCoverage)*as.numeric(gtf$length)), 
			   average.coverage=as.numeric(gtf$avgCoverage), 
			   base.with..10.coverage=NA))
}

