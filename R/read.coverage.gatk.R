read.coverage.gatk <- 
function(file) {
	gatk = read.table(file, header=TRUE)
	chrpos = matrix(unlist(strsplit(as.character(gatk$Target),":")), ncol=2, byrow=TRUE)
	chr = factor(paste("chr",chrpos[,1],sep=""))
	pos = matrix(as.integer(unlist(strsplit(chrpos[,2],"-"))), ncol=2, byrow=TRUE)
	start = pos[,1]
	end = pos[,2]
	return(data.frame( probe=gatk$Target, 
			   chr=chr, 
			   probe_start=start, 
			   probe_end=end, 
			   targeted.base=end-start+1, 
			   sequenced.base=NA, 
			   coverage=as.numeric(gatk$total_coverage), 
			   average.coverage=as.numeric(gatk$average_coverage), 
			   base.with..10.coverage=NA))
}

