library(zipfR)
library(ggplot2)
library(gridExtra)

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





