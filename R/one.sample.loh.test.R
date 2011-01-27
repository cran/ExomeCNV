one.sample.loh.test <-
function(cover, bcount) {
	stopifnot(length(cover) == length(bcount))
	min.bcount = find.min.of.2lists(bcount,cover-bcount)
	pval = rep(NA,length(cover))
	for (i in 1:length(cover)) {
		pval[i] = binom.test(min.bcount[i],cover[i],0.5,alternative="less")$p.value
	}
	return(pval)
}

