library(zipfR)

gossiping <- readLines("Gossiping_2005_seg.txt", encoding="UTF-8")
gossiping <- unlist(strsplit(gossiping, "\\s"))

### Discriptive statistics: type-freqency list ###

gossiping.tfl <- vec2tfl(gossiping) # making type-freq list
N(gossiping.tfl) # sample size
V(gossiping.tfl) # type count

plot(gossiping.tfl) # Zipf ranking
plot(gossiping.tfl, log="xy") # logarithmic scale recommended

### Discriptive statistics: frequency spectrum ###

gossiping.spc <- tfl2spc(gossiping.tfl) # or directly with vec2spc
N(gossiping.spc) # sample size
V(gossiping.spc) # type count

plot(gossiping.spc) # barplot of frequency spectrum

### Discriptive statistics: vocabulary growth ###

gossiping.vgc <- vec2vgc(gossiping, m.max=2)
plot(gossiping.vgc, add.m=1:2) # plot all three VGCs

