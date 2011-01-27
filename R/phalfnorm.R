phalfnorm <-
function(total.dev) {
	#2*pnorm(mean(total.dev), sd=sd(total.dev)/sqrt(1-2/pi), lower.tail=FALSE)
	2*pnorm(mean(total.dev), sd=sd(total.dev), lower.tail=FALSE)
}

