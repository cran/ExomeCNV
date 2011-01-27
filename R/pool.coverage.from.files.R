pool.coverage.from.files <-
function(infile.prefix.list, infile.suffix="exon_parsed.coverage", exome, chr.list=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY")) {
	`%+%` <- function(x,y) paste(x,y,sep="")

	if (length(infile.prefix.list) == 0) {
		return()
	}

	all.data = list()
	for (i in 1:length(infile.prefix.list)) {
		one.data = read.all.coverage(infile.prefix.list[i], infile.suffix, chr.list=chr.list)
		stopifnot(all(exome$probe_start == one.data$probe_start))
		all.data[[i]] = one.data
	}

	return(pool.coverage(all.data))
}

