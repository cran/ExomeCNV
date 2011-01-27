two.sample.loh.CMH.test <-
function(strata.ls) {
	pval = rep(NA,length(strata.ls))
	for (i in 1:length(strata.ls)) {
		num.strata = dim(strata.ls[[i]])[3]
		if (num.strata == 1) {
			pval[i] = prop.test(strata.ls[[i]][,,1])$p.value
		} else {
			pval[i] = mantelhaen.test(strata.ls[[i]], alternative="two.sided", exact=TRUE)$p.value
		}
	}
	return(pval)
}

