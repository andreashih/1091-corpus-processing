library(readr)
library(zipfR)

## load files ##

#asbc
asbc_corpus <- paste0(readLines("data\\asbc.txt", n=10000, encoding = 'UTF-8'),collapse=" ")
asbc_corpus <- unlist(strsplit(asbc_corpus, "\\s"))

# PTT Gossiping board (sampled)
g05 <- sample(readLines("data\\Gossiping_2005_seg.txt", encoding="UTF-8"), 1250)
g10 <- sample(readLines("data\\Gossiping_2010_seg.txt", encoding="UTF-8"), 1250)
g15 <- sample(readLines("data\\Gossiping_2015_seg.txt", encoding="UTF-8"), 1250)
g20 <- sample(readLines("data\\Gossiping_2020_seg.txt", encoding="UTF-8"), 1250)

g05 <- unlist(strsplit(g05, "\\s"))
g10 <- unlist(strsplit(g10, "\\s"))
g15 <- unlist(strsplit(g15, "\\s"))
g20 <- unlist(strsplit(g20, "\\s"))

# PTT WomenTalk board (sampled)
w05 <- sample(readLines("data\\WomenTalk_2005_seg.txt", encoding="UTF-8"), 1250)
w10 <- sample(readLines("data\\WomenTalk_2010_seg.txt", encoding="UTF-8"), 1250)
w15 <- sample(readLines("data\\WomenTalk_2015_seg.txt", encoding="UTF-8"), 1250)
w20 <- sample(readLines("data\\WomenTalk_2020_seg.txt", encoding="UTF-8"), 1250)

w05 <- unlist(strsplit(w05, "\\s"))
w10 <- unlist(strsplit(w10, "\\s"))
w15 <- unlist(strsplit(w15, "\\s"))
w20 <- unlist(strsplit(w20, "\\s"))

ptt_corpus <- c(g05, g10, g15, g20, w05, w10, w15, w20)

# dcard
dcard_corpus <- readLines("data\\dcard_corpus.txt", encoding="UTF-8")

## type-freq list ##

asbc.tfl <- vec2tfl(asbc_corpus)
data("Brown.tfl")
ptt.tfl <- vec2tfl(ptt_corpus)
dcard.tfl <- vec2tfl(dcard_corpus)

# visualization
par(mfrow=c(2,2)) # 2*2 plot area
plot(asbc.tfl, main="ASBC", log="xy", # logarithmic scale
     xlab="rank", ylab="frequency")
plot(Brown.tfl, main="Brown", log="xy", 
     xlab="rank", ylab="frequency")
plot(ptt.tfl, main="PTT", log="xy", 
     xlab="rank", ylab="frequency")
plot(dcard.tfl, main="Dcard", log="xy",
     xlab="rank", ylab="frequency")

## frequency spectrum ##

asbc.spc <- tfl2spc(asbc.tfl)
data("Brown.spc")
ptt.spc <- tfl2spc(ptt.tfl)
dcard.spc <- tfl2spc(dcard.tfl)

# plot frequency spectrum
par(mfrow=c(2,2))
plot(asbc.spc, log="x", main="ASBC",
     xlab="m", ylab="Vm")
plot(Brown.spc, log="x", main="Brown",
     xlab="m", ylab="Vm")
plot(ptt.spc, log="x", main="PTT",
     xlab="m", ylab="Vm")
plot(dcard.spc, log="x", main="Dcard",
     xlab="m", ylab="Vm")

## vocabulary growth curve ##

asbc.vgc <- vec2vgc(asbc_corpus, m.max=2)
data("Brown.emp.vgc")
ptt.vgc <- vec2vgc(ptt_corpus, m.max=2)
dcard.vgc <- vec2vgc(dcard_corpus, m.max=2)

# plot VGC
par(mfrow=c(2,2))
plot(asbc.vgc, add.m=1, main="ASBC",
     xlab="N", ylab="V(N)/V1(N)")
plot(Brown.emp.vgc, add.m=1, main="Brown",
     xlab="N", ylab="V(N)/V1(N)")
plot(ptt.vgc, add.m=1, main="PTT",
     xlab="N", ylab="V(N)/V1(N)")
plot(dcard.vgc, add.m=1, main="Dcard",
     xlab="N", ylab="V(N)/V1(N)")

## fitting LNRE model ##

asbc.fzm <- lnre("fzm", spc=asbc.spc)
Brown.fzm <- lnre("fzm", spc=Brown.spc)
ptt.fzm <- lnre("fzm", spc=ptt.spc)
dcard.fzm <- lnre("fzm", spc=dcard.spc)

# frequency spectrum
asbc.fzm.spc <- lnre.spc(asbc.fzm, N(asbc.spc))
Brown.fzm.spc <- lnre.spc(Brown.fzm, N(Brown.spc))
ptt.fzm.spc <- lnre.spc(ptt.fzm, N(ptt.spc))
dcard.fzm.spc <- lnre.spc(dcard.fzm, N(dcard.spc))

# VGC
asbc.fzm.vgc <- lnre.vgc(asbc.fzm, N(asbc.vgc), m.max=1, variances=TRUE)
Brown.fzm.vgc <- lnre.vgc(Brown.fzm, N(Brown.emp.vgc), m.max=1, variances=TRUE)
ptt.fzm.vgc <- lnre.vgc(ptt.fzm, N(ptt.vgc), m.max=1, variances=TRUE)
dcard.fzm.vgc <- lnre.vgc(dcard.fzm, N(dcard.vgc), m.max=1, variances=TRUE)

# plot frequency spectrum
par(mfrow=c(2,2)) 
plot(asbc.spc, asbc.fzm.spc,
     main="ASBC", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
       fill = 1:2, cex = 0.5)
plot(Brown.spc, Brown.fzm.spc,
     main="Brown", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
       fill = 1:2, cex = 0.5)
plot(ptt.spc, ptt.fzm.spc,
     main="PTT", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
       fill = 1:2, cex = 0.5)
plot(dcard.spc, dcard.fzm.spc,
     main="Dcard", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
       fill = 1:2, cex = 0.5)

# plot VGC
par(mfrow=c(2,2)) 
plot(asbc.vgc, asbc.fzm.vgc, add.m=1,
     main="ASBC", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
       fill = 1:2, cex = 0.5)
plot(Brown.emp.vgc, Brown.fzm.vgc, add.m=1,
     main="Brown", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
       fill = 1:2, cex = 0.5)
plot(ptt.vgc, ptt.fzm.vgc, add.m=1,
     main="PTT", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
       fill = 1:2, cex = 0.5)
plot(dcard.vgc, dcard.fzm.vgc, add.m=1,
     main="Dcard", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
       fill = 1:2, cex = 0.5)

## Out-of-Vocabulary (OOV) type

# first 100,000 lemma
asbc100k <- head(asbc_corpus, 100000)
data("Brown100k.spc")
ptt100k <- head(ptt_corpus, 100000)
dcard100k <- head(dcard_corpus, 100000)

# convert to spc object
asbc100k.tfl <- vec2tfl(asbc100k)
ptt100k.tfl <- vec2tfl(ptt100k)
dcard100k.tfl <- vec2tfl(dcard100k)

asbc100k.spc <- tfl2spc(asbc100k.tfl)
ptt100k.spc <- tfl2spc(ptt100k.tfl)
dcard100k.spc <- tfl2spc(dcard100k.tfl)

# lexical of seen types
asbc_Vseen <- V(asbc100k.spc) - Vm(asbc100k.spc, 1)
Brown_Vseen <- V(Brown100k.spc) - Vm(Brown100k.spc, 1)
ptt_Vseen <- V(ptt100k.spc) - Vm(ptt100k.spc, 1)
dcard_Vseen <- V(dcard100k.spc) - Vm(dcard100k.spc, 1)

# fitting LNRE model
asbc100k.fzm <- lnre("fzm", asbc100k.spc)
Brown100k.fzm <- lnre("fzm", Brown100k.spc)
ptt100k.fzm <- lnre("fzm", ptt100k.spc)
dcard100k.fzm <- lnre("fzm", dcard100k.spc)

# expected OOV
1 - (asbc_Vseen / EV(asbc100k.fzm, c(1e6, 10e6, 100e6)))
1 - (Brown_Vseen / EV(Brown100k.fzm, c(1e6, 10e6, 100e6)))
1 - (ptt_Vseen / EV(ptt100k.fzm, c(1e6, 10e6, 100e6)))
1 - (dcard_Vseen / EV(dcard100k.fzm, c(1e6, 10e6, 100e6)))