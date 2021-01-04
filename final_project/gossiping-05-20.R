library(zipfR)
library(ggplot2)
library(gridExtra)

# load files
gossip05 <- readLines("data\\Gossiping_2005_seg.txt", encoding="UTF-8")
gossip10 <- readLines("data\\Gossiping_2010_seg.txt", encoding="UTF-8")
gossip15 <- readLines("data\\Gossiping_2015_seg.txt", encoding="UTF-8")
gossip20 <- readLines("data\\Gossiping_2020_seg.txt", encoding="UTF-8")

gossip05 <- unlist(strsplit(gossip05, "\\s"))
gossip10 <- unlist(strsplit(gossip10, "\\s"))
gossip15 <- unlist(strsplit(gossip15, "\\s"))
gossip20 <- unlist(strsplit(gossip20, "\\s"))

### type-freqency list ###

gossip05.tfl <- vec2tfl(gossip05)
gossip10.tfl <- vec2tfl(gossip10)
gossip15.tfl <- vec2tfl(gossip15)
gossip20.tfl <- vec2tfl(gossip20)

write.csv(gossip05.tfl, file="gossip05.tfl.csv")
write.csv(gossip10.tfl, file="gossip10.tfl.csv")
write.csv(gossip15.tfl, file="gossip15.tfl.csv")
write.csv(gossip20.tfl, file="gossip20.tfl.csv")

g05 <- ggplot(gossip05.tfl, aes(x=k, y=f)) + 
  geom_point() +
  ggtitle("Zipf ranking: Gossiping 2005") +
  xlab("rank") + ylab("frequency")

g10 <- ggplot(gossip10.tfl, aes(x=k, y=f)) + 
  geom_point() +
  ggtitle("Zipf ranking: Gossiping 2010") +
  xlab("rank") + ylab("frequency")

g15 <- ggplot(gossip15.tfl, aes(x=k, y=f)) + 
  geom_point() +
  ggtitle("Zipf ranking: Gossiping 2015") +
  xlab("rank") + ylab("frequency")

g20 <- ggplot(gossip20.tfl, aes(x=k, y=f)) + 
  geom_point() +
  ggtitle("Zipf ranking: Gossiping 2020") +
  xlab("rank") + ylab("frequency")

grid.arrange(g05, g10, g15, g20, nrow=2, ncol=2)

# basic r-plot
par(mfrow=c(2,2))
plot(gossip05.tfl, main="Zipf ranking: Gossiping 2005",
     xlab="rank", ylab="frequency")
plot(gossip10.tfl, main="Zipf ranking: Gossiping 2010",
     xlab="rank", ylab="frequency")
plot(gossip15.tfl, main="Zipf ranking: Gossiping 2015",
     xlab="rank", ylab="frequency")
plot(gossip20.tfl, main="Zipf ranking: Gossiping 2020",
     xlab="rank", ylab="frequency")

# logarithmic scale
g05 <- ggplot(gossip05.tfl, aes(x=k, y=f)) + 
  geom_point() +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  ggtitle("Zipf ranking: Gossiping 2005") +
  xlab("rank") + ylab("frequency")

g10 <- ggplot(gossip10.tfl, aes(x=k, y=f)) + 
  geom_point() +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  ggtitle("Zipf ranking: Gossiping 2010") +
  xlab("rank") + ylab("frequency")

g15 <- ggplot(gossip15.tfl, aes(x=k, y=f)) + 
  geom_point() +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  ggtitle("Zipf ranking: Gossiping 2015") +
  xlab("rank") + ylab("frequency")

g20 <- ggplot(gossip20.tfl, aes(x=k, y=f)) + 
  geom_point() +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  ggtitle("Zipf ranking: Gossiping 2020") +
  xlab("rank") + ylab("frequency")

grid.arrange(g05, g10, g15, g20, nrow=2, ncol=2)

### frequency spectra ###

gossip05.spc <- tfl2spc(gossip05.tfl)
gossip10.spc <- tfl2spc(gossip10.tfl)
gossip15.spc <- tfl2spc(gossip15.tfl)
gossip20.spc <- tfl2spc(gossip20.tfl)

dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(2,2)) # 2*2 plot area

plot(gossip05.spc, log="x", main="Gossiping 2005",
     xlab="m", ylab="Vm")
plot(gossip10.spc, log="x", main="Gossiping 2010",
     xlab="m", ylab="Vm")
plot(gossip15.spc, log="x", main="Gossiping 2015",
     xlab="m", ylab="Vm")
plot(gossip20.spc, log="x", main="Gossiping 2020",
     xlab="m", ylab="Vm")

### vocabulary growth curve ###

gossip05.vgc <- vec2vgc(gossip05, m.max=2)
gossip10.vgc <- vec2vgc(gossip10, m.max=2)
gossip15.vgc <- vec2vgc(gossip15, m.max=2)
gossip20.vgc <- vec2vgc(gossip20, m.max=2)

dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(2,2)) # 2*2 plot area

plot(gossip05.vgc, add.m=1:2, main="Gossiping 2005",
     xlab="N", ylab="V(N)/V1(N)")
plot(gossip10.vgc, add.m=1:2, main="Gossiping 2010",
     xlab="N", ylab="V(N)/V1(N)")
plot(gossip15.vgc, add.m=1:2, main="Gossiping 2015",
     xlab="N", ylab="V(N)/V1(N)")
plot(gossip20.vgc, add.m=1:2, main="Gossiping 2020",
     xlab="N", ylab="V(N)/V1(N)")











