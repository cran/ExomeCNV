do.plot.eCNV <-
function(all.ecnv, pch="*", lim.quantile=0.99, style="idx", bg.cnv=NULL, line.plot=FALSE) {
	if (is.null(bg.cnv)) {
		lim.logR = quantile(all.ecnv$logR[all.ecnv$logR != Inf & all.ecnv$logR != -Inf], c(1-lim.quantile,lim.quantile), na.rm=TRUE)
	} else {
		lim.logR = quantile(bg.cnv$logR[bg.cnv$logR != Inf & bg.cnv$logR != -Inf], c(1-lim.quantile,lim.quantile), na.rm=TRUE)
	}
	if (lim.logR[1] == -Inf)	lim.logR[1] = min(all.ecnv$logR[all.ecnv$logR != -Inf], na.rm=TRUE)
	if (lim.logR[2] == Inf)	lim.logR[2] = max(all.ecnv$logR[all.ecnv$logR != Inf], na.rm=TRUE)
	max.cn = max(all.ecnv$copy.number, na.rm=TRUE)
	reds = rainbow(2^(max.cn-2), start=3/4, end=0)[2^(max.cn:3-2)]
	colors = c("green4", "gold", reds) # (green, yellow, reds)
	chr.list = unique(as.character(all.ecnv$chr))
	dimx = floor(sqrt(length(chr.list)))
	dimy = ceiling(length(chr.list)/dimx)
	par(mfrow=c(dimx,dimy))
	for (chr in chr.list) {
		do.plot.one.eCNV(all.ecnv[all.ecnv$chr == chr,], colors=colors, ylim=lim.logR, main=chr, pch=pch, style=style, bg.cnv=bg.cnv[bg.cnv$chr == chr,], line.plot=line.plot)
	}
	#legend(1,0,paste("copy number:", 1:max(ecnv$copy.number, na.rm=T)), col=colors, lty=1)
}

