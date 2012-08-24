classify.eCNV <-
function(normal, tumor, logR=NULL, min.spec=0.9, min.sens=0.9, option="auc", admix=0.3, c=admix, read.len=70, l=read.len, normal.chrs = c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY"),test.num.copy=c(1,3)) {
	`%+%` <- function(x,y) paste(x,y,sep="")
	normal.chrs = intersect(levels(normal$chr), normal.chrs)
	print("analyzing eCNV with min.spec " %+% min.spec %+% " and min.sens " %+% min.sens %+% " and option to maximize " %+% option)

	covered.exon = (normal$average.coverage > 0 & tumor$average.coverage > 0)
	covered.exon[is.na(covered.exon)] = FALSE
	norm.log.ratio = if (is.null(logR)) { calculate.logR(normal, tumor, normal.chrs) } else { logR }

	eCNV = normal[,c("probe", "chr", "probe_start", "probe_end", "coverage", "average.coverage", "targeted.base")]
	eCNV$tumor.average.coverage = tumor$average.coverage
	eCNV$logR = norm.log.ratio
	eCNV$ratio = 2^norm.log.ratio
	eCNV$copy.number = NA
	eCNV$lower.cutoff = NA
	eCNV$upper.cutoff = NA
	eCNV$spec = NA
	eCNV$sens = NA
	for (i in 1:nrow(normal)) {
		if (covered.exon[i]) {
			rho = c + (1-c)*test.num.copy/2
			chi = normal$average.coverage[i]
			W = normal$targeted.base[i]
			r = matrix(NA, nrow=length(rho), ncol=3, dimnames=list(rho,c("cutoff","spec","sens")))
			for (j in 1:length(rho)) {
				r[j,] = get.r.cutoff(chi, W, l, rho[j], min.spec, min.sens, option)
			}
			r = data.frame(r)
			cn = classify.logR(norm.log.ratio[i], log2(r$cutoff))
			eCNV$copy.number[i] = cn
			eCNV$lower.cutoff[i] = if (is.nan(cn) || cn > 1) { r$cutoff[cn-1] } else { 0 }
			eCNV$upper.cutoff[i] = r$cutoff[cn]
			eCNV$spec[i] = if (is.nan(cn) || cn == 1) r$spec[cn] else r$spec[cn-1]
			eCNV$sens[i] = if (is.nan(cn) || cn == 1) r$sens[cn] else r$sens[cn-1]
		} else {
			eCNV$copy.number[i] = 0 # zero to signify "no coverage"
			eCNV$lower.cutoff[i] = NA
			eCNV$upper.cutoff[i] = NA
			eCNV$spec[i] = NA
			eCNV$sens[i] = NA
		}
	}

	return(eCNV)
}

