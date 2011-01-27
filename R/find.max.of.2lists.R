find.max.of.2lists <-
function(ls1, ls2) {
	stopifnot(length(ls1)==length(ls2))
	max.ls = ls1
	max.ls[ls2 > ls1] = ls2[ls2 > ls1]
	return(max.ls)
}

