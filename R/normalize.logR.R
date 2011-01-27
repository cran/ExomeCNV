normalize.logR <-
function(logR, func, ...) {
	logR - func(logR, ...)
}

