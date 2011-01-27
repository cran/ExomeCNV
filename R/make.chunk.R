make.chunk <-
function(the.cnv, cur.idx.start, cur.idx.end) {
	new.chunk = data.frame( chr = the.cnv$chr[cur.idx.start], 
				probe_start = the.cnv$probe_start[cur.idx.start],
				probe_end = the.cnv$probe_end[cur.idx.end],
				coverage = sum(the.cnv$coverage[cur.idx.start:cur.idx.end]),
				targeted.base = sum(the.cnv$targeted.base[cur.idx.start:cur.idx.end]),
				sequenced.base = sum(the.cnv$sequenced.base[cur.idx.start:cur.idx.end]),
				copy.number = the.cnv$copy.number[cur.idx.start],
				logR = the.cnv$logR[cur.idx.start],
				ratio = the.cnv$ratio[cur.idx.start],
				spec = the.cnv$spec[cur.idx.start],
				sens = the.cnv$sens[cur.idx.start])
	new.chunk$average.coverage = new.chunk$coverage / new.chunk$targeted.base
	return(new.chunk)
}

