CNV.analyze <-
function(normal, tumor, logR=NULL, coverage.cutoff=15, normal.chrs=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY"), normal.chr=normal.chrs, c=0.5, write.file=FALSE, file=NULL, 
		       doDNAcopy=TRUE, sdundo=1, smooth=TRUE, alpha=0.01, plot.cnv=TRUE) {
	`%+%` <- function(x,y) paste(x,y,sep="")
	normal.chrs = intersect(levels(normal$chr), normal.chrs)

    # first, do it for exons with enough coverage
	well.covered.exon.idx = (normal$average.coverage > coverage.cutoff) & (tumor$average.coverage > coverage.cutoff)
	if (is.null(logR)) norm.log.ratio = calculate.logR(normal, tumor, normal.chrs)
	else norm.log.ratio = logR

	if (doDNAcopy) {
		library(DNAcopy)

		CNA.obj = CNA(norm.log.ratio[well.covered.exon.idx], strip.chr.name(normal$chr[well.covered.exon.idx]), (normal$probe_start[well.covered.exon.idx] + normal$probe_end[well.covered.exon.idx])/2, data.type="logratio")
		smoothed.CNA.obj = if (smooth) smooth.CNA(CNA.obj) else CNA.obj
		segment.smoothed.CNA.obj = segment(smoothed.CNA.obj, undo.splits="sdundo", undo.SD=sdundo, verbose=1, alpha=alpha)

		if (plot.cnv) {
			if (write.file && !is.null(file)) png(file, width=2000, height=1000, units="px")
			else if (write.file) png("CNV detection for exons with > " %+% coverage.cutoff %+% " coverage.png", width=2000, height=1000, units="px")
			plot(segment.smoothed.CNA.obj, plot.type="s")
			if (write.file) dev.off()

			if (write.file && !is.null(file)) png("allchr." %+% file, width=2000, height=1000, units="px")
			else if (write.file) png("all chromosome CNV detection for exons with > " %+% coverage.cutoff %+% " coverage.png", width=2000, height=1000, units="px") else x11()
			plot(segment.smoothed.CNA.obj, plot.type="w")
			abline(h=log2(c + (1-c)*c(1,3,4,5)/2), col="purple")
			if (write.file) dev.off()
		}

		cnv = get.proper.cnv.positions(normal[well.covered.exon.idx,], print(segment.smoothed.CNA.obj))

		return(list(cnv=cnv, cna=segment.smoothed.CNA.obj, logR=norm.log.ratio))

	} else {

		logR.mean = mean(norm.log.ratio[well.covered.exon.idx])
		logR.sd = sd(norm.log.ratio[well.covered.exon.idx])
		logR.min = min(norm.log.ratio[well.covered.exon.idx])
		logR.max = max(norm.log.ratio[well.covered.exon.idx])

		if (plot.cnv) {
			if (write.file && !is.null(file)) png(file, width=2000, height=1000, units="px")
			else if (write.file) png("CNV detection for exons with > " %+% coverage.cutoff %+% " coverage.noDNAcopy.png", width=2000, height=1000, units="px")
			par(mfrow=c(4,6))
			for (chr in levels(normal$chr)) {
				plot((normal$probe_start[well.covered.exon.idx & normal$chr==chr] + normal$probe_end[well.covered.exon.idx & normal$chr==chr])/2, norm.log.ratio[well.covered.exon.idx & normal$chr==chr], pch="*", pc=20, ylim=c(logR.min, logR.max), main=chr, xlab="position", ylab="log ratio")
				abline(h=logR.mean + logR.sd, col="red")
				abline(h=logR.mean - logR.sd, col="red")
				abline(h=0, col="gray")
			}
			if (write.file) dev.off()
		}

		return(list(logR=norm.log.ratio))

	}
}

