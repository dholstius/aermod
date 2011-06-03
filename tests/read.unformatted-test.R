library(aermod)

# 24-hour averages from the "aertest" case
filename <- system.file('extdata','AERMOD','output','aertest-24h.bin',package='aermod')
records.24h <- read.unformatted(filename, n.receptors=144)

# 1-hour averages from the "aertest" case
filename <- system.file('extdata','AERMOD','output','aertest-1h.bin',package='aermod')
records.1h <- read.unformatted(filename, n.receptors=144)

as.matrix.records <- function(records) {
	concs <- lapply(records, function(x) x$concentrations)
	do.call(cbind, concs)
}

conc.24h <- as.matrix.records(records.24h)
dim(conc.24h)

conc.1h <- as.matrix.records(records.1h)
dim(conc.1h)

# The receptor means should be the same
avg <- function(conc) apply(conc, 1, mean)
stopifnot(all.equal(avg(conc.24h), avg(conc.1h)))