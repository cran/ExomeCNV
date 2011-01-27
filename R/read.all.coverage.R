read.all.coverage <-
function(prefix, suffix, chr.list=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY"), header=TRUE) {
	`%+%` <- function(x,y) paste(x,y,sep="")
	all.cover = c()
	for (chr in chr.list) {
		cover = read.table(prefix %+% chr %+% suffix, header=header, sep='\t')
		all.cover = rbind(all.cover, cover)
	}
	if (!header) {
		data(COVERAGE_HEADER)
		colnames(all.cover) = COVERAGE_HEADER
	}
	return(all.cover)
}

