cnv.points <-
function(cnv, col, pch, style, line.plot=FALSE) {
	if (nrow(cnv) == 0) return()
	if (style == "bp" && line.plot) {
		sapply(1:nrow(cnv), function(i){lines(cnv[i,c("probe_start","probe_end")],rep(cnv[i,"logR"],2),lwd=3,col=col)})
	} else if (style == "bp") {
		points(cnv$probe_start, cnv$logR, col=col, pch=pch)
	} else {
		points(cnv, col=col, pch=pch)
	}
}

