library(zipfR)
library(readr)

# load files
g05 <- readLines("data\\Gossiping_2005_seg.txt", encoding="UTF-8")
g10 <- readLines("data\\Gossiping_2010_seg.txt", encoding="UTF-8")
g15 <- readLines("data\\Gossiping_2015_seg.txt", encoding="UTF-8")
g20 <- readLines("data\\Gossiping_2020_seg.txt", encoding="UTF-8")

g05 <- unlist(strsplit(g05, "\\s"))
g10 <- unlist(strsplit(g10, "\\s"))
g15 <- unlist(strsplit(g15, "\\s"))
g20 <- unlist(strsplit(g20, "\\s"))

gossip_corpus <- c(g05, g10, g15, g20)
dcard_corpus <- readLines("data\\dcard_corpus.txt", encoding="UTF-8")

### type-freqency list ###
gossip.tfl <- vec2tfl(gossip_corpus)
dcard.tfl <- vec2tfl(dcard_corpus)

dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(1,2)) # 1*2 plot area
plot(gossip.tfl, main="PTT-Gossiping",
     xlab="rank", ylab="frequency")
plot(dcard.tfl, main="Dcard",
     xlab="rank", ylab="frequency")

# log
dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(1,2)) # 1*2 plot area
plot(gossip.tfl, main="PTT-Gossiping", log="xy",
     xlab="rank", ylab="frequency")
plot(dcard.tfl, main="Dcard", log="xy",
     xlab="rank", ylab="frequency")

### frequency spectra ###
gossip.spc <- tfl2spc(gossip.tfl)
dcard.spc <- tfl2spc(dcard.tfl)
asbc.spc <- read_table2("data/asbc_frequency_spectrum.txt")
data(Brown.spc)

par(mfrow=c(2,2)) # 2*2 plot area
plot(asbc.spc, log="x", main="ASBC",
     xlab="m", ylab="Vm", type = "o")
plot(Brown.spc, log="x", main="Brown",
     xlab="m", ylab="Vm")
plot(dcard.spc, log="x", main="Dcard",
     xlab="m", ylab="Vm")
plot(gossip.spc, log="x", main="PTT-Gossiping",
     xlab="m", ylab="Vm")

### vocab growth curve ###
gossip.vgc <- vec2vgc(gossip_corpus, m.max=2)
dcard.vgc <- vec2vgc(dcard_corpus, m.max=2)

par(mfrow=c(1,2))
plot(gossip.vgc, add.m=1:2, main="PTT-Gossiping",
     xlab="N", ylab="V(N)/V1(N)")
plot(dcard.vgc, add.m=1:2, main="Dcard",
     xlab="N", ylab="V(N)/V1(N)")

### fitting LNRE model: Zipf-Mandelbrot ###

zm_g <- lnre("zm", spc=gossip.spc)
zm_d <- lnre("zm", spc=dcard.spc)

# frequency spectra
zm_g.spc <- lnre.spc(zm_g, N(gossip.spc))
zm_d.spc <- lnre.spc(zm_d, N(dcard.spc))

par(mfrow=c(1,2)) # 1*2 plot area
plot(gossip.spc, zm_g.spc,
     main="PTT-Gossiping", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM model"), 
       fill = 1:2,
       cex = 0.75)
plot(dcard.spc, zm_d.spc,
     main="Dcard", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM model"), 
       fill = 1:2,
       cex = 0.75)

# vocab growth curve
zm_g.vgc <- lnre.vgc(zm_g, N(gossip.vgc), m.max=1, variances=TRUE)
zm_d.vgc <- lnre.vgc(zm_d, N(dcard.vgc), m.max=1, variances=TRUE)

par(mfrow=c(1,2)) # 1*2 plot area
plot(gossip.vgc, zm_g.vgc, add.m=1,
     main="PTT-Gossiping", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM model"),
       fill = 1:2,
       cex = 0.5)
plot(dcard.vgc, zm_d.vgc, add.m=1,
     main="Dcard", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM model"),
       fill = 1:2,
       cex = 0.5)

### fitting LNRE model: finite Zipf-Mandelbrot ###



