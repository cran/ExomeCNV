do.merge.cnv.intervals <-
function(the.cnv) {
	if (nrow(the.cnv) == 1) return (the.cnv) # new in ExomeCNV 1.3
	new.cnv = c()
	cur.chr = the.cnv$chr[1]
	cur.logR = the.cnv$logR[1]
	cur.cn = the.cnv$copy.number[1]
	cur.idx.start = 1
	cur.idx.end = 1
	for (i in 2:nrow(the.cnv)) {
		if (the.cnv$chr[i] != cur.chr | the.cnv$logR[i] != cur.logR | the.cnv$copy.number[i] != cur.cn | the.cnv$copy.number[i] == 0 | is.nan(the.cnv$copy.number[i])) {
			new.cnv = rbind(new.cnv, make.chunk(the.cnv, cur.idx.start, cur.idx.end))
			cur.chr = the.cnv$chr[i]
			cur.logR = the.cnv$logR[i]
			cur.cn = the.cnv$copy.number[i]
			cur.idx.start = i
		}
		cur.idx.end = i
	}
	new.cnv = rbind(new.cnv, make.chunk(the.cnv, cur.idx.start, cur.idx.end))
	return(new.cnv)
}

