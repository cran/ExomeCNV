find.min.of.2lists <-
function(x,y) {
	sapply(1:length(x),function(i){ ifelse(x[i]>y[i],y[i],x[i]) })
}

