multi.LOH.analyze <-
function(normal=NULL,tumor=NULL,all.loh.ls=NULL,min.spec=0.95,test.alpha=NULL,method=c("deviation.half.norm","variance.f","deviation.wilcox","deviation.t","CHM","mantelhaen","two.sample.fisher","two.sample.prop","only.tumor","only.normal"),
				     sdundo=c(1,2), alpha=c(0.05,0.01)) {
	stopifnot(!is.null(normal) || !is.null(tumor))
	stopifnot(length(sdundo) == length(alpha))
	method = method[1]
	library(DNAcopy)
	if (is.null(test.alpha)) { test.alpha = 1 - min.spec }
	if (is.null(all.loh.ls)) { all.loh.ls = list() }
	for (i in 1:length(sdundo)) {
		# DNAcopy segmentation
		cna = do.DNAcopy(normal, tumor, method=method, sdundo=sdundo[i], alpha=alpha[i])
		# call LOH
		cna[["LOH"]] = LOH.analyze(cna$normal, cna$tumor, cna$strata, alpha=test.alpha, method=method)
		# keep in the ls
		#this.loh = data.frame(chr=cna$normal$chr, position=cna$normal$position, normal.baf=cna$normal$baf, normal.coverage=cna$normal$coverage, tumor.baf=cna$tumor$baf, tumor.coverage=cna$tumor$coverage, LOH=cna$LOH)
		all.loh.ls[[length(all.loh.ls) + 1]] = cna[["LOH"]]
	}
	the.loh = combine.LOH(all.loh.ls)
	return(the.loh)
}

