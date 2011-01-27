LOH.analyze <-
function(normal=NULL,tumor=NULL,strata=NULL,alpha=0.05,method=c("deviation.half.norm","variance.f","deviation.wilcox","deviation.t","CMH","mantelhaen","two.sample.fisher","two.sample.prop","only.tumor","only.normal")) {
	stopifnot(!is.null(normal) || !is.null(tumor) || !is.null(strata)) # they cannot all be null
	method = method[1]

	old.warn = options()$warn
	options(warn=-1) # suppress warning

	if (method == "two.sample.fisher") {
		pval = two.sample.loh.test(normal$coverage, normal$baf, tumor$coverage, tumor$baf, test="fisher")
	} else if (method == "two.sample.prop") {
		pval = two.sample.loh.test(normal$coverage, normal$baf, tumor$coverage, tumor$baf, test="prop")
	} else if (method == "only.normal") {
		pval = one.sample.loh.test(normal$coverage, normal$baf)
	} else if (method == "only.tumor") {
		pval = one.sample.loh.test(tumor$coverage, tumor$baf)
	} else if (method %in% c("CMH","mantelhaen")) {
		pval = two.sample.loh.CMH.test(strata)
	} else if (method == "deviation.wilcox") {
		pval = two.sample.loh.deviation.test(strata, "wilcox.test")
	} else if (method == "deviation.t") {
		pval = two.sample.loh.deviation.test(strata, "t.test")		
	} else if (method == "variance.f") {
		pval = two.sample.loh.deviation.test(strata, "f.test")
	} else if (method == "deviation.half.norm") {
		pval = two.sample.loh.deviation.test(strata, "half.norm.test")
	}

	options(warn=old.warn)

	LOH = (pval < alpha)
	data.loh = data.frame(chr=if(is.null(normal)) tumor$chr else normal$chr, 
				    position=if(is.null(normal)) tumor$position else normal$position, 
				    end.position=get.end.position(normal,tumor),
				    normal.baf=normal$baf, normal.coverage=normal$coverage, 
				    tumor.baf=tumor$baf, tumor.coverage=tumor$coverage, 
				    LOH=LOH, pval=pval)
	return(data.loh)
}

