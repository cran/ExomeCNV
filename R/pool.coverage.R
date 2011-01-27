pool.coverage <-
function(all.data) {
	pool = all.data[[1]]
	if (length(all.data) == 1) {
		return(pool)
	}
	for (i in 2:length(all.data)) {
		pool$sequenced.base = find.max.of.2lists(pool$sequenced.base, all.data[[i]]$sequenced.base)
		pool$coverage = pool$coverage + all.data[[i]]$coverage
		pool$average.coverage = pool$average.coverage + all.data[[i]]$average.coverage
		pool$base.with..10.coverage = pool$base.with..10.coverage + all.data[[i]]$base.with..10.coverage
	}
	return(pool)
}

