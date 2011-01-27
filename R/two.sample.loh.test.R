two.sample.loh.test <-
function(normal.cover, normal.bcount, tumor.cover, tumor.bcount, test=c("fisher","prop")) {
	stopifnot(length(normal.cover) == length(normal.bcount)
		 && length(tumor.cover) == length(tumor.bcount)
		 && length(normal.cover) == length(tumor.cover))
	test = test[1] # use Fisher's exact as default
	pval = rep(NA,length(normal.cover))
	for (i in 1:length(normal.cover)) {
		if (test == "fisher") {
			pval[i] = fisher.test(matrix(c(normal.bcount[i],tumor.bcount[i],
								 normal.cover[i]-normal.bcount[i],tumor.cover[i]-tumor.bcount[i]),
							     nrow=2))$p.value
		} else if (test == "prop") {
			pval[i] = prop.test(c(normal.bcount[i],tumor.bcount[i]), 
						  c(normal.cover[i],tumor.cover[i]), 
						  alternative="two.sided")$p.value
		}
	}
	return(pval)
}

