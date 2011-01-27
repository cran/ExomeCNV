get.proper.cnv.positions <-
function(exons, cnv) {
	data(chr.hash)
	`%+%` <- function(x,y) paste(x,y,sep="")
	order.by.chr = order(strip.chr.name(exons$chr))
	exons = exons[order.by.chr,]
	cnv$chr = as.character(chr.hash[cnv$chrom,"chr"])
	cnv$probe = "cnv" %+% as.character(1:nrow(cnv))
	end.idx = cumsum(cnv$num.mark)
	start.idx = c(1, 1 + end.idx[-length(end.idx)])
	cnv$probe_start = exons$probe_start[start.idx]
	cnv$probe_end = exons$probe_end[end.idx]
	cnv$size = cnv$probe_end - cnv$probe_start + 1
	sum.chunk = function(i, colName) { sum(exons[start.idx[i]:end.idx[i],colName]) }
	cnv$targeted.base = sapply(1:nrow(cnv), sum.chunk, colName="targeted.base")
	cnv$sequenced.base = sapply(1:nrow(cnv), sum.chunk, colName="sequenced.base")
	cnv$coverage = sapply(1:nrow(cnv), sum.chunk, colName="coverage")
	cnv$average.coverage = cnv$coverage / cnv$targeted.base
	cnv$base.with..10.coverage = sapply(1:nrow(cnv), sum.chunk, colName="base.with..10.coverage")
	return(cnv)
}

