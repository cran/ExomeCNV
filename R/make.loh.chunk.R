make.loh.chunk <-
function(the.loh, cur.idx.start, cur.idx.end) {
	new.chunk = data.frame( chr = the.loh$chr[cur.idx.start], 
				position.start = the.loh$position[cur.idx.start],
				position.end = the.loh$position[cur.idx.end],
				normal.coverage = sum(the.loh$normal.coverage[cur.idx.start:cur.idx.end]),
				normal.baf = sum(the.loh$normal.baf[cur.idx.start:cur.idx.end]),
				tumor.coverage = sum(the.loh$tumor.coverage[cur.idx.start:cur.idx.end]),
				tumor.baf = sum(the.loh$tumor.baf[cur.idx.start:cur.idx.end]),
				LOH = the.loh$LOH[cur.idx.start], 
				pval = the.loh$pval[cur.idx.start])
	return(new.chunk)
}

