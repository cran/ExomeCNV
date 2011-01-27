write.loh.output <-
function(loh, name) {
	`%+%` <- function(x,y) paste(x,y,sep="")
	write.table(loh, file=name %+% ".loh.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
}

