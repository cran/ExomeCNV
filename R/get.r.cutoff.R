get.r.cutoff <-
function(chi, W, l, rho, min.spec, min.sens, option) {
	stopifnot(option %in% c("auc", "spec", "sens"))
	r.min.spec = get.alpha.inv(chi=chi, W=W, l=l, rho=rho, alpha=1-min.spec)
	r.min.sens = get.power.inv(chi=chi, W=W, l=l, rho=rho, power=min.sens)
	opt.lower = if (rho < 1) { r.min.sens } else { r.min.spec }
	opt.upper = if (rho < 1) { r.min.spec } else { r.min.sens }
	if (is.nan(opt.lower) || is.nan(opt.upper) || opt.lower > opt.upper) {
		# print("not enough power!")
		return(c(cutoff=NaN, spec=NaN, sens=NaN))
	}
	if (option == "auc") {
		old.warn = options()$warn
		options(warn=-1) # suppress warning
		# note optimize -auc because optim do minimization
		r.opt = optim(rho, function(this.r){-get.AUC(chi=chi, r=this.r, W=W, l=l, rho=rho)}, method="L-BFGS-B", lower=opt.lower, upper=opt.upper, control=list(abstol=1e-10))
		r.cutoff = r.opt$par
		options(warn=old.warn) # setback old value
	} else if (option == "spec") {
		r.cutoff = r.min.sens
	} else if (option == "sens") {
		r.cutoff = r.min.spec
	}
	return(c(cutoff=r.cutoff,
		 spec=get.specificity(chi, r.cutoff, W, l, rho),
		 sens=get.sensitivity(chi, r.cutoff, W, l, rho)))
}

