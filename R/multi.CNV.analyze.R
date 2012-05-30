multi.CNV.analyze <-
function(normal, tumor, logR=NULL, all.cnv.ls=NULL, coverage.cutoff=10, admix=0.3, c=admix, read.len=70, l=read.len, sdundo=c(1,2), alpha=c(0.05,0.01), 
			     min.spec=0.99, min.sens=0.9, option="auc") {
	stopifnot(length(sdundo) == length(alpha))
	if (is.null(all.cnv.ls)) { all.cnv.ls = list() }
	for (i in 1:length(sdundo)) {
		cna = CNV.analyze(normal, tumor, logR=logR, coverage.cutoff=coverage.cutoff, c=c, sdundo=sdundo[i], alpha=alpha[i], plot.cnv=FALSE)
		ecnv = classify.eCNV(cna$cnv, cna$cnv, cna$cnv$seg.mean, min.spec=min.spec, min.sens=min.sens, option=option, c=c, l=l)
		all.cnv.ls[[length(all.cnv.ls) + 1]] = ecnv
	}
	the.cnv = combine.CNV(all.cnv.ls)
	return(the.cnv)
}

