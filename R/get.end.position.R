get.end.position <-
function(normal, tumor) {
	data = if (is.null(normal)) normal else tumor
	endpos = if (is.null(data$end.position)) data$position+1 else data$end.position
	return(endpos)
}

