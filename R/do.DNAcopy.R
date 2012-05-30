do.DNAcopy <-
function(normal, tumor, method, smooth=FALSE, sdundo=1, alpha=0.01) {
	data = normal
	if (method %in% c("deviation.half.norm","two.sample.fisher","two.sample.prop","CMH","mantelhaen","variance.f","deviation.wilcox","deviation.t")) {
		data$baf = abs(normal$baf/normal$coverage - tumor$baf/tumor$coverage)
	} else if (method == "only.tumor") {
		data$baf = abs(tumor$baf/tumor$coverage - 0.5)
	} else if (method == "only.normal") {
		data$baf = abs(normal$baf/normal$coverage - 0.5)
	}

	CNA.obj = CNA(data$baf, strip.chr.name(data$chr), data$position, data.type="binary")
	smoothed.CNA.obj = if (smooth) smooth.CNA(CNA.obj) else CNA.obj
	segment.obj = segment(smoothed.CNA.obj, undo.splits="sdundo", undo.SD=sdundo, verbose=1, alpha=alpha)
	segment.output = segment.obj$output

	data(chr.hash)
	cna.normal = data.frame(chr = chr.hash[segment.output$chrom,1],
				  position = segment.output$loc.start,
				  end.position = segment.output$loc.end,
				  baf = rep(NA,nrow(segment.output)),
				  coverage = rep(NA,nrow(segment.output)))
	cna.tumor = cna.normal
	cna.strata = list()
	for (i in 1:nrow(segment.output)) {
		cur.pos = (data$chr == as.character(cna.normal$chr[i]) & data$position >= segment.output$loc.start[i] & data$position <= segment.output$loc.end[i])
		cna.normal$coverage[i] = sum(normal$coverage[cur.pos])
		cna.tumor$coverage[i] = sum(tumor$coverage[cur.pos])
		cna.baf = combine.baf(normal[cur.pos,], tumor[cur.pos,])
		cna.normal$baf[i] = cna.baf[["normal"]]
		cna.tumor$baf[i] = cna.baf[["tumor"]]
		cna.strata[[i]] = make.loh.strata(normal[cur.pos,], tumor[cur.pos,])
	}
	# note: i tried to be clever by using the num.mark to build idx, but the chromosome order is not the same between data and cna

	return(list(normal=cna.normal,tumor=cna.tumor,strata=cna.strata, cna=segment.obj))
}

