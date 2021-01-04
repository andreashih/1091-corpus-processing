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

### fitting LNRE model: Zipf-Mandelbrot ###

zm05 <- lnre("zm", spc=gossip05.spc)
zm10 <- lnre("zm", spc=gossip10.spc)
zm15 <- lnre("zm", spc=gossip15.spc)
zm20 <- lnre("zm", spc=gossip20.spc)

# frequency spectra
zm05.spc <- lnre.spc(zm05, N(gossip05.spc))
zm10.spc <- lnre.spc(zm10, N(gossip10.spc))
zm15.spc <- lnre.spc(zm15, N(gossip15.spc))
zm20.spc <- lnre.spc(zm20, N(gossip20.spc))

dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(2,2)) # 2*2 plot area
plot(gossip05.spc, zm05.spc,
     main="Gossiping 2005", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM model"), 
       fill = 1:2,
       cex = 0.75)

plot(gossip10.spc, zm10.spc,
     main="Gossiping 2010", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM model"), 
       fill = 1:2,
       cex = 0.75)

plot(gossip15.spc, zm15.spc,
     main="Gossiping 2015", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM model"), 
       fill = 1:2,
       cex = 0.75)

plot(gossip20.spc, zm20.spc,
     main="Gossiping 2020", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM model"), 
       fill = 1:2,
       cex = 0.75)

# vocab growth curve
zm05.vgc <- lnre.vgc(zm05, N(gossip05.vgc), m.max=1, variances=TRUE)
zm10.vgc <- lnre.vgc(zm10, N(gossip10.vgc), m.max=1, variances=TRUE)
zm15.vgc <- lnre.vgc(zm15, N(gossip15.vgc), m.max=1, variances=TRUE)
zm20.vgc <- lnre.vgc(zm20, N(gossip20.vgc), m.max=1, variances=TRUE)

dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(2,2)) # 2*2 plot area

plot(gossip05.vgc, zm05.vgc, add.m=1,
     main="Gossiping 2005", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM model"),
       fill = 1:2,
       cex = 0.75)

plot(gossip10.vgc, zm10.vgc, add.m=1,
     main="Gossiping 2010", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM model"),
       fill = 1:2,
       cex = 0.75)

plot(gossip15.vgc, zm15.vgc, add.m=1,
     main="Gossiping 2015", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM model"),
       fill = 1:2,
       cex = 0.75)

plot(gossip20.vgc, zm20.vgc, add.m=1,
     main="Gossiping 2020", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM model"),
       fill = 1:2,
       cex = 0.75)

### fitting LNRE model: finite Zipf-Mandelbrot ###

fzm05 <- lnre("fzm", spc=gossip05.spc)
fzm10 <- lnre("fzm", spc=gossip10.spc)
fzm15 <- lnre("fzm", spc=gossip15.spc)
fzm20 <- lnre("fzm", spc=gossip20.spc)

# frequency spectra
fzm05.spc <- lnre.spc(fzm05, N(gossip05.spc))
fzm10.spc <- lnre.spc(fzm10, N(gossip10.spc))
fzm15.spc <- lnre.spc(fzm15, N(gossip15.spc))
fzm20.spc <- lnre.spc(fzm20, N(gossip20.spc))

dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(2,2)) # 2*2 plot area

plot(gossip05.spc, zm05.spc, fzm05.spc,
     main="Gossiping 2005", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.75)

plot(gossip10.spc, zm10.spc, fzm10.spc,
     main="Gossiping 2010", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.75)

plot(gossip15.spc, zm15.spc, fzm15.spc,
     main="Gossiping 2015", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.75)

plot(gossip20.spc, zm20.spc, fzm20.spc,
     main="Gossiping 2020", xlab="m", ylab="Vm")
legend("topright", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.75)

# vocab growth curve
fzm05.vgc <- lnre.vgc(fzm05, N(gossip05.vgc), m.max=1, variances=TRUE)
fzm10.vgc <- lnre.vgc(fzm10, N(gossip10.vgc), m.max=1, variances=TRUE)
fzm15.vgc <- lnre.vgc(fzm15, N(gossip15.vgc), m.max=1, variances=TRUE)
fzm20.vgc <- lnre.vgc(fzm20, N(gossip20.vgc), m.max=1, variances=TRUE)

dev.new(width=10, height=10) # open new-sized plot window
par(mfrow=c(2,2)) # 2*2 plot area

plot(gossip05.vgc, zm05.vgc, fzm05.vgc, add.m=1, 
     main="Gossiping 2005", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.5)    
     
plot(gossip10.vgc, zm10.vgc, fzm10.vgc, add.m=1, 
     main="Gossiping 2010", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.5) 

plot(gossip15.vgc, zm15.vgc, fzm15.vgc, add.m=1, 
     main="Gossiping 2015", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.5) 

plot(gossip20.vgc, zm20.vgc, fzm20.vgc, add.m=1, 
     main="Gossiping 2020", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", 
       legend = c("observed", "ZM", "fZM"), 
       fill = 1:3,
       cex = 0.5) 


