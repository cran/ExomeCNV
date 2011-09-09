read.gtf <- 
function(file) {
	GTF_HEADER = c("chr", "src", "feature", "start", "end", "score", "strand", "frame", "group")
	gtf = read.table(file, sep="\t")
	names(gtf) = GTF_HEADER
	group.fields = matrix(unlist(strsplit(as.character(gtf$group), ";\\s*|\\s+")), nrow=nrow(gtf), byrow=TRUE)
	name.cols = which(apply(group.fields, 2, function(col) { all(col == col[1]) & col[1] != "" } ))
	for (i in name.cols) {
		gtf[,group.fields[1,i]] = group.fields[,i+1]
	}
	return(gtf)
}

