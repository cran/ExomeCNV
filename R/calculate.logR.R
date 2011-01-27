calculate.logR <-
function(normal, tumor, normal.chrs = c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY")) {
	total.cov.normal = sum(as.numeric(normal$coverage), na.rm=TRUE)
	total.cov.tumor = sum(as.numeric(tumor$coverage), na.rm=TRUE)
	log.ratio = log2(tumor$average.coverage/normal$average.coverage) + log2(total.cov.normal/total.cov.tumor)
	norm.log.ratio = normalize.logR(log.ratio, chr.median, normal$chr %in% normal.chrs)
	return(norm.log.ratio)
}

