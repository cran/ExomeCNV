get.AUC <-
function(chi, r, W, l, rho, overdisperse="no", phi=1, od.alpha=0) {
	(get.power(chi, r, W, l, rho, overdisperse, phi, od.alpha) + get.specificity(chi, r, W, l, rho, overdisperse, phi, od.alpha))/2
}

