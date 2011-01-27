get.specificity <-
function(chi, r, W, l, rho, overdisperse="no", phi=1, od.alpha=0) {
	1 - get.alpha(chi, r, W, l, rho, overdisperse, phi, od.alpha)
}

