save.logR <-
function(all.logR, exome, name) {
	`%+%` <- function(x,y) paste(x,y,sep="")
	for (chr in levels(exome$chr)) {
		logR = all.logR[exome$chr == chr]
		save(logR, file=name %+% ".logR." %+% chr %+% ".RData")
	}
}

