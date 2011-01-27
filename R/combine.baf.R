combine.baf <-
function(this.normal, this.tumor) {
	#use.inverse = (this.tumor$baf/this.tumor$coverage > 0.5)
	use.inverse = rep(FALSE,nrow(this.normal))
	use.as.is = !use.inverse
	normal.baf.sum = sum(this.normal$baf[use.as.is]) + sum(this.normal$coverage[use.inverse] - this.normal$baf[use.inverse])
	tumor.baf.sum = sum(this.tumor$baf[use.as.is]) + sum(this.tumor$coverage[use.inverse] - this.tumor$baf[use.inverse])
	return(list(normal=normal.baf.sum, tumor=tumor.baf.sum))
}

