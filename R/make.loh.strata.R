make.loh.strata <-
function(this.normal, this.tumor) {
	#use.inverse = (this.normal$baf/this.normal$coverage > 0.5)
	#use.inverse = (this.tumor$baf/this.tumor$coverage > 0.5)
	#use.inverse = (this.tumor$baf/this.tumor$coverage > this.normal$baf/this.normal$coverage) # or > 0.5?
	use.inverse = rep(FALSE,nrow(this.normal))
	use.as.is = !use.inverse
	normal.baf = this.normal$baf
	normal.baf[use.inverse] = this.normal$coverage[use.inverse] - this.normal$baf[use.inverse]
	tumor.baf = this.tumor$baf
	tumor.baf[use.inverse] = this.tumor$coverage[use.inverse] - this.tumor$baf[use.inverse]
	strata = rbind(normal.baf,
	 		   this.normal$coverage-normal.baf,
			   tumor.baf,
			   this.tumor$coverage-tumor.baf)
	dim(strata) = c(2,2,nrow(this.normal))
	return(strata)
}

