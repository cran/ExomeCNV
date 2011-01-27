get.power.inv <-
function(chi, W, l, rho, power, overdisperse="no", phi=1, od.alpha=0) {
	t = if (rho < 1) { qnorm(power) } else { qnorm(1-power) }
	if (overdisperse %in% c("ql", "quasi-likelihood")) { t = t*sqrt(phi) }
	c = W*chi/l
	# k = rho*c/(c-t^2)
	# r = k + t*sqrt((k^2+k)/c)
	oldWarn = getOption("warn")
	options(warn=-1)
	if (overdisperse %in% c("nb", "negative binomial")) {
		A = rho + c*od.alpha*rho^2
		B = 1 + c*od.alpha
		r = (rho*c + t*sqrt(c*B*rho^2 - A*B*t^2 + c*A))/(c-B*t^2)
	} else {
		r = (rho*c + t*sqrt(rho*(c - t^2 + rho*c)))/(c-t^2) 
	}
	options(warn=oldWarn)
	if (any(r < 0 | is.nan(r))) { r[r<0] = NaN }
	return(r)
}

