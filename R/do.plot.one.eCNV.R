do.plot.one.eCNV <-
function(ecnv,  pch="*", colors, style="idx", bg.cnv=NULL, line.plot=FALSE, ...) {
	ecnv$idx = 1:nrow(ecnv)
	plot.cols = if (style == "bp") c("probe_end", "probe_start", "logR") else c("idx", "logR")
	plot(c(0,max(ecnv[,plot.cols[1]])), c(0,0), col="black", type='l', xlab=plot.cols[1], ylab="log2 ratio", ...)
	if (style=="bp" && !is.null(bg.cnv)) points(bg.cnv$probe_end,bg.cnv$logR,pch=pch,col="lightgray")
	cnv.points(ecnv[is.nan(ecnv$copy.number), plot.cols], col="darkgray", pch=pch, style=style, line.plot=line.plot)
	for (cn in 1:max(ecnv$copy.number, na.rm=TRUE)) {
		cnv.points(ecnv[ecnv$copy.number==cn, plot.cols], col=colors[cn], pch=pch, style=style, line.plot=line.plot)
	}
}

