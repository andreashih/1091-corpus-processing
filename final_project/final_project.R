library(zipfR)

gossiping <- readLines("Gossiping_2005_seg.txt", encoding="UTF-8")
gossiping <- unlist(strsplit(gossiping, "\\s"))

### Discriptive statistics: type-freqency list ###

gossiping.tfl <- vec2tfl(gossiping) # making type-freq list
N(gossiping.tfl) # sample size
V(gossiping.tfl) # type count

par(mar = rep(2, 4))
plot(gossiping.tfl) # Zipf ranking
plot(gossiping.tfl, log="xy") # logarithmic scale recommended

### Discriptive statistics: frequency spectrum ###

gossiping.spc <- tfl2spc(gossiping.tfl) # or directly with vec2spc
N(gossiping.spc) # sample size
V(gossiping.spc) # type count

plot(gossiping.spc) # barplot of frequency spectrum
plot(gossiping.spc, log="x")
plot.default(gossiping.spc, main="Frequency Spectrum")

### Discriptive statistics: vocabulary growth ###
# VGC lists vocabulary size V(N) at different sample sizes N

gossiping.vgc <- vec2vgc(gossiping, m.max=2) 
plot(gossiping.vgc, add.m=1:2) # plot all three VGCs

### LNRE Modelling ###

zm <- lnre("zm", spc=gossiping.spc)
zm.spc <- lnre.spc(zm, N(gossiping.spc))
plot(gossiping.spc, zm.spc, legend=c("observed", "ZM model"))

zm.vgc <- lnre.vgc(zm, N(gossiping.vgc), m.max=1, variances=TRUE)
plot(gossiping.vgc, zm.vgc, add.m=1)

fzm <- lnre("fzm", spc=gossiping.spc)
fzm
fzm$S

fzm.spc <- lnre.spc(fzm, N(gossiping.spc))
plot(gossiping.spc, zm.spc, fzm.spc, legend=c("observed", "ZM", "fZM"))

fzm.vgc <- lnre.vgc(fzm, N(gossiping.vgc), m.max=1, variances=TRUE)
plot(gossiping.vgc, zm.vgc, fzm.vgc, add.m=1, legend=c("observed", "ZM", "fZM"))

# "Baayen's P measure" of productivity proposed by Harald Baayen (see, e.g., Baayen, 1992).
Vm(gossiping.spc,1) / N(gossiping.spc)

### Estimating the proportion of OOV types and tokens given a fixed size lexicon ###













