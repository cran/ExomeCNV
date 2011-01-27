get.alpha <-
function(chi, r, W, l, rho, overdisperse="no", phi=1, od.alpha=0) {
	C = W*chi/l
	if (overdisperse == "no") {
		T = (r-1)*sqrt(C /(1+r^2))
	} else if (overdisperse %in% c("ql", "quasi-likelihood")) {
		T = (r-1)*sqrt(C /((1+r^2)*phi))
	} else if (overdisperse %in% c("nb", "negative binomial")) {
		T = (r-1)*sqrt(C /((1+r^2)*(1+od.alpha*C)))
	}

	alpha = if (rho > 1) { (1-pnorm(T)) } else { pnorm(T) }
	if (any(alpha > 1)) alpha[alpha>1] = 1
	if (any(alpha < 0)) alpha[alpha<0] = 0
	return(alpha)
}

