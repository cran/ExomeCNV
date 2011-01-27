expand.loh <-
function(the.loh, data) {
	all.loh = rep(NA, nrow(data))
	for (i in 1:nrow(the.loh)) {
		cur.pos = (data$chr == the.loh$chr[i] & the.loh$position.start[i] <= data$position & data$position <= the.loh$position.end[i])
		all.loh[cur.pos] = the.loh$LOH[i]
	}
	data$LOH = all.loh
	return(data)
}

