library(aermod)
filename <- system.file('extdata','AERMOD','output','aertest-24h.bin',package='aermod')
stopifnot(file.exists(filename))
read.unformatted(filename, n.receptors=144)