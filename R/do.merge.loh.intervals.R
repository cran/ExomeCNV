do.merge.loh.intervals <-
function(the.loh) {
	new.loh = c()
	cur.chr = the.loh$chr[1]
	cur.loh = the.loh$LOH[1]
	cur.idx.start = 1
	cur.idx.end = 1
	for (i in 2:nrow(the.loh)) {
		if (the.loh$chr[i] != cur.chr | the.loh$LOH[i] != cur.loh) {
			new.loh = rbind(new.loh, make.loh.chunk(the.loh, cur.idx.start, cur.idx.end))
			cur.chr = the.loh$chr[i]
			cur.loh = the.loh$LOH[i]
			cur.idx.start = i
		}
		cur.idx.end = i
	}
	new.loh = rbind(new.loh, make.loh.chunk(the.loh, cur.idx.start, cur.idx.end))
	return(new.loh)
}

