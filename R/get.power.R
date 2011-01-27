get.power <-
function(chi, r, W, l, rho, overdisperse="no", phi=1, od.alpha=0) {
	C = W*chi/l
	if (overdisperse == "no") {
		T = (r-rho)*sqrt(C /(rho+r^2))
	} else if (overdisperse %in% c("ql", "quasi-likelihood")) {
		T = (r-rho)*sqrt(C /((rho+r^2)*phi))
	} else if (overdisperse %in% c("nb", "negative binomial")) {
		T = (r-rho)*sqrt(C /((rho+r^2)+(rho^2+r^2)*od.alpha*C))
	}

	power = if (rho > 1) { 1-pnorm(T) } else { pnorm(T) }	
	if (any(is.nan(power))) power[is.nan(power)] = 0
	return(power)
}

