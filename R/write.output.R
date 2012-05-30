write.output <-
function(eCNV, cnv, name) {
	`%+%` <- function(x,y) paste(x,y,sep="")
	write.table(cnv, file=name %+% ".cnv.txt", quote=FALSE, sep="\t", row.names=FALSE)
	write.table(eCNV[!is.nan(eCNV$logR) & !eCNV$logR %in% c(-Inf,Inf),c("chr","probe_start","probe_end","logR")], file=name %+% ".exon.lrr.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
	write.table(cnv[!is.nan(cnv$logR) & !cnv$logR %in% c(-Inf,Inf),c("chr","probe_start","probe_end","logR")], file=name %+% ".segment.lrr.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
	write.table(cnv[!is.nan(cnv$logR) & !cnv$logR %in% c(-Inf,Inf) & !is.na(cnv$copy.number),c("chr","probe_start","probe_end","copy.number")], file=name %+% ".segment.copynumber.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
	png(filename=name %+% ".cnv.png", res=70, width=2000, height=1200, pointsize=16)
	do.plot.eCNV(cnv, style="bp", lim.quantile=0.999, bg.cnv=data.frame(chr=eCNV$chr, probe_end=(eCNV$probe_end+eCNV$probe_start)/2, logR=eCNV$logR), line.plot=TRUE)
	dev.off()
}

