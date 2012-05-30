combine.CNV <-
function(cnv.ls) {
	my.cols = c("copy.number", "ratio", "logR", "lower.cutoff", "upper.cutoff", "spec", "sens")
	cat("start\n")
	the.cnv = cnv.ls[[1]]
	the.cnv$original.ratio = the.cnv$ratio
	the.cnv$original.logR = the.cnv$logR
	if (length(cnv.ls) > 1) { # new in ExomeCNV 1.3
		for (i in 2:length(cnv.ls)) {
			cur.cnv = cnv.ls[[i]]
			cat("set: ", i, "; number of lines: ", nrow(cur.cnv), "; ")
			for (j in 1:nrow(cur.cnv)) {
				cur.chunk = cur.cnv[j,]
				cur.probes = (cur.chunk$chr == the.cnv$chr & cur.chunk$probe_start <= the.cnv$probe_start & cur.chunk$probe_end >= the.cnv$probe_end)
				probe.to.merge = (the.cnv$copy.number %in% c(0,2,cur.chunk$copy.number) | is.na(the.cnv$copy.number)) & cur.probes
				the.cnv[probe.to.merge,my.cols] = cur.chunk[my.cols]
			}
			cat(" number of merging events: ", nrow(cnv.ls[[i-1]]) - nrow(cnv.ls[[i]]), '\n')
		}
	}
	the.cnv = do.merge.cnv.intervals(the.cnv)
	return(the.cnv)
}

