combine.LOH <-
function(loh.ls) {
	the.loh = loh.ls[[1]]
	for (i in 2:length(loh.ls)) {
		cur.loh = loh.ls[[i]]
		for (j in 1:nrow(cur.loh)) {
			if (is.na(cur.loh$LOH[j]) || !cur.loh$LOH[j]) next() # skip if no LOH is detected
			cur.chunk = cur.loh[j,]
			cur.pos = (the.loh$chr == as.character(cur.chunk$chr) & the.loh$position >= cur.chunk$position & the.loh$position <= cur.chunk$end.position)
			the.loh$LOH[cur.pos] = cur.chunk$LOH
			the.loh$pval[cur.pos] = cur.chunk$pval
		}
	}
	the.loh = do.merge.loh.intervals(the.loh)
	return(the.loh)
}

