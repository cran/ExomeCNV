two.sample.loh.deviation.test <-
function(strata.ls, test=c("half.norm.test","f.test","wilcox.test","t.test")) {
	test = test[1]
	pval = rep(NA,length(strata.ls))
	for (i in 1:length(strata.ls)) {
		this.segment = strata.ls[[i]]
		num.strata = dim(this.segment)[3]
		if (num.strata == 1) {
			pval[i] = NA
			next()
		}
		dim(this.segment) = c(4,num.strata)
		normal.baf = this.segment[1,]
		normal.coverage = normal.baf + this.segment[2,]
		tumor.baf = this.segment[3,]
		tumor.coverage = tumor.baf + this.segment[4,]
		normal.ratio = normal.baf/normal.coverage
		tumor.ratio = tumor.baf/tumor.coverage
		normal.dev = abs(normal.ratio - 0.5)
		tumor.dev = abs(tumor.ratio - 0.5)
		total.dev = abs(tumor.ratio - normal.ratio)
		if (test == "wilcox.test")
			pval[i] = wilcox.test(normal.dev, tumor.dev, alterative="less")$p.value
		else if (test == "t.test") 
			pval[i] = t.test(normal.dev, tumor.dev, alterative="less")$p.value
		else if (test == "f.test")
			pval[i] = var.test(normal.ratio, tumor.ratio, alternative="less")$p.value
		else if (test == "half.norm.test")
			pval[i] = phalfnorm(total.dev)
		else stop("misspecified test")
	}
	return(pval)
}

