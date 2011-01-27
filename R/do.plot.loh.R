do.plot.loh <-
function(the.loh, normal, tumor, method, lim.quantile=0.99, color="red", plot.style=c("dev","baf")) {
	plot.style = plot.style[1]
	bg.data = normal
	if (method %in% c("deviation.half.norm","two.sample.fisher","two.sample.prop","variance.f","deviation.wilcox","deviation.t")) {
		bg.data$value = (normal$baf/normal$coverage - tumor$baf/tumor$coverage)
		if (is.null(the.loh$value)) the.loh$value = (the.loh$normal.baf/the.loh$normal.coverage - the.loh$tumor.baf/the.loh$tumor.coverage)
	} else if (method == "only.tumor") {
		bg.data$value = (tumor$baf/tumor$coverage - 0.5)
		if (!is.null(the.loh$value)) the.loh$value = (the.loh$tumor.baf/the.loh$tumor.coverage - 0.5)
	} else if (method == "only.normal") {
		bg.data$value = (normal$baf/normal$coverage - 0.5)
		if (!is.null(the.loh$value)) the.loh$value = (the.loh$normal.baf/the.loh$normal.coverage - 0.5)
	}

	limits = quantile(bg.data$value, c(1-lim.quantile,lim.quantile), na.rm=TRUE)
	if (plot.style == "baf") { 
		limits = c(0,1)
		bg.data$normal.baf = normal$baf/normal$coverage
		bg.data$tumor.baf = tumor$baf/tumor$coverage
		the.loh$value = rep(0.5,nrow(the.loh))
	}
	chr.list = unique(as.character(the.loh$chr))
	dimx = floor(sqrt(length(chr.list)))
	dimy = ceiling(length(chr.list)/dimx)
	par(mfrow=c(dimx,dimy))
	data(chr.hash)
	chr.order = sort(chr.hash[chr.list,"number"])
	for(chr in chr.hash[chr.order,"chr"]) {
		do.plot.one.loh(the.loh[the.loh$chr==chr,], bg.data[bg.data$chr == chr,], color=color, ylim=limits, main=chr, plot.style=plot.style)
	}
}

