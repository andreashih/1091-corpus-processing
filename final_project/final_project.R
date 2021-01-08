library(zipfR)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

## load files ##

#asbc
asbc_corpus <- paste0(readLines("data\\asbc.txt", n=10000, encoding = 'UTF-8'),collapse=" ")
asbc_corpus <- unlist(strsplit(asbc_corpus, "\\s"))

# gossiping
g05 <- readLines("data\\Gossiping_2005_seg.txt", encoding="UTF-8")
g10 <- readLines("data\\Gossiping_2010_seg.txt", encoding="UTF-8")
g15 <- readLines("data\\Gossiping_2015_seg.txt", encoding="UTF-8")
g20 <- readLines("data\\Gossiping_2020_seg.txt", encoding="UTF-8")

g05 <- unlist(strsplit(g05, "\\s"))
g10 <- unlist(strsplit(g10, "\\s"))
g15 <- unlist(strsplit(g15, "\\s"))
g20 <- unlist(strsplit(g20, "\\s"))

gossip_corpus <- c(g10, g15, g20)

# dcard
dcard_corpus <- readLines("data\\dcard_corpus.txt", encoding="UTF-8")

## type-freq list ##

# asbc
asbc.tfl <- vec2tfl(asbc_corpus)

# gossiping
gossip.tfl <- vec2tfl(gossip_corpus)

# dcard
dcard.tfl <- vec2tfl(dcard_corpus)

# visualization
par(mfrow=c(1,3)) # 1*3 plot area
plot(asbc.tfl, main="ASBC", log="xy", # logarithmic scale
     xlab="rank", ylab="frequency")
plot(gossip.tfl, main="PTT-Gossiping", log="xy", 
     xlab="rank", ylab="frequency")
plot(dcard.tfl, main="Dcard", log="xy",
     xlab="rank", ylab="frequency")

## frequency spectrum ##

# asbc
asbc.spc <- tfl2spc(asbc.tfl)

# gossiping
gossip.spc <- tfl2spc(gossip.tfl)

# dcard
dcard.spc <- tfl2spc(dcard.tfl)

# plot frequency spectrum
par(mfrow=c(1,3))
plot(asbc.spc, log="x", main="ASBC",
     xlab="m", ylab="Vm")
plot(gossip.spc, log="x", main="PTT-Gossiping",
     xlab="m", ylab="Vm")
plot(dcard.spc, log="x", main="Dcard",
     xlab="m", ylab="Vm")

## vocabulary growth curve ##

# asbc
asbc.vgc <- vec2vgc(asbc_corpus, m.max=2)

# gossiping
gossip.vgc <- vec2vgc(gossip_corpus, m.max=2)

# dcard
dcard.vgc <- vec2vgc(dcard_corpus, m.max=2)

# plot VGC
par(mfrow=c(1,3))
plot(asbc.vgc, add.m=1, main="ASBC",
     xlab="N", ylab="V(N)/V1(N)")
plot(gossip.vgc, add.m=1, main="PTT-Gossiping",
     xlab="N", ylab="V(N)/V1(N)")
plot(dcard.vgc, add.m=1, main="Dcard",
     xlab="N", ylab="V(N)/V1(N)")

## fitting LNRE model ##

asbc.fzm <- lnre("fzm", spc=asbc.spc)
gossip.fzm <- lnre("fzm", spc=gossip.spc)
dcard.fzm <- lnre("fzm", spc=dcard.spc)

asbc.fzm.spc <- lnre.spc(asbc.fzm, N(asbc.spc))
gossip.fzm.spc <- lnre.spc(gossip.fzm, N(gossip.spc))
dcard.fzm.spc <- lnre.spc(dcard.fzm, N(dcard.spc))

asbc.fzm.vgc <- lnre.vgc(asbc.fzm, N(asbc.vgc), m.max=1, variances=TRUE)
gossip.fzm.vgc <- lnre.vgc(gossip.fzm, N(gossip.vgc), m.max=1, variances=TRUE)
dcard.fzm.vgc <- lnre.vgc(dcard.fzm, N(dcard.vgc), m.max=1, variances=TRUE)

# plot frequency spectrum
par(mfrow=c(1,3)) # 1*3 plot area
plot(asbc.spc, asbc.fzm.spc,
     main="ASBC", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
       fill = 1:2, cex = 0.75)
plot(gossip.spc, gossip.fzm.spc,
     main="PTT-Gossiping", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
       fill = 1:2, cex = 0.75)
plot(dcard.spc, dcard.fzm.spc,
     main="Dcard", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
       fill = 1:2, cex = 0.75)

# plot VGC
par(mfrow=c(1,3)) # 1*3 plot area
plot(asbc.vgc, asbc.fzm.vgc, add.m=1,
     main="ASBC", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
       fill = 1:2, cex = 0.5)
plot(gossip.vgc, gossip.fzm.vgc, add.m=1,
     main="PTT-Gossiping", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
       fill = 1:2, cex = 0.5)
plot(dcard.vgc, dcard.fzm.vgc, add.m=1,
     main="Dcard", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
       fill = 1:2, cex = 0.5)

## Out-of-Vocabulary (OOV) type

# first 100,000 lemma
asbc100k <- head(asbc_corpus, 100000)
gossip100k <- head(gossip_corpus, 100000)
dcard100k <- head(dcard_corpus, 100000)

# convert to spc object
asbc100k.tfl <- vec2tfl(asbc100k)
gossip100k.tfl <- vec2tfl(gossip100k)
dcard100k.tfl <- vec2tfl(dcard100k)

asbc100k.spc <- tfl2spc(asbc100k.tfl)
gossip100k.spc <- tfl2spc(gossip100k.tfl)
dcard100k.spc <- tfl2spc(dcard100k.tfl)

# lexical of seen types
asbc_Vseen <- V(asbc100k.spc) - Vm(asbc100k.spc, 1)
gossip_Vseen <- V(gossip100k.spc) - Vm(gossip100k.spc, 1)
dcard_Vseen <- V(dcard100k.spc) - Vm(dcard100k.spc, 1)

# fitting LNRE model
asbc100k.zm <- lnre("fzm",asbc100k.spc)
gossip100k.zm <- lnre("fzm", gossip100k.spc)
dcard100k.zm <- lnre("fzm", dcard100k.spc)

# expected OOV
1 - (asbc_Vseen / EV(asbc100k.zm, c(1e6, 10e6, 100e6)))
1 - (gossip_Vseen / EV(gossip100k.zm, c(1e6, 10e6, 100e6)))
1 - (dcard_Vseen / EV(dcard100k.zm, c(1e6, 10e6, 100e6)))



