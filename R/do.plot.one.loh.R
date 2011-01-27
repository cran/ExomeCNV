do.plot.one.loh <-
function(a.loh, bg.data, color, plot.style, ...) {
	`%+%` <- function(x,y) paste(x,y,sep="")

	if (plot.style == "dev") {
		plot(c(0,max(a.loh$position.end)), c(0,0), col="black", type='l', ...)
		points(bg.data$position, bg.data$value, pch="*", col="lightgray")
	} else {
		plot(c(0,max(a.loh$position.end)), c(0.5,0.5), col="black", type='l', ...)
		points(bg.data$position, bg.data$normal.baf, pch="*", col="lightgray")
		points(bg.data$position, bg.data$tumor.baf, pch="*", col="darkgray")
	}
	sapply(1:nrow(a.loh), function(i){if (a.loh$LOH[i]) lines(a.loh[i,c("position.start","position.end")],rep(a.loh[i,"value"],2),lwd=3,col=color)
						    else lines(a.loh[i,c("position.start","position.end")],rep(a.loh[i,"value"],2),lwd=3,col="gold")
						    })
}

