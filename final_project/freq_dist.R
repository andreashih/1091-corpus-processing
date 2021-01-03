library(zipfR)

#####CASE1#####

data(ItaRi.spc)
summary(ItaRi.spc)
Vm(ItaRi.spc,1:5)

# "Baayen's P measure" of productivity proposed by Harald Baayen (see, e.g., Baayen, 1992).
Vm(ItaRi.spc,1) / N(ItaRi.spc)
par(mfrow=c(1,3), mar = rep(2, 4))
plot(ItaRi.spc)
plot(ItaRi.spc, log="x")
plot.default(ItaRi.spc, main="Frequency Spectrum")

#A spectrum is often characterized by very high
# values corresponding to the lowest frequency classes, 
# and a very long tail of frequency classes with only one 
# member (i.e., just one word with frequency 100, 
# just one word with frequency 103, etc.)
Vm(ItaRi.spc,100)
# ItaRi.spc$Vm[20]

data(ItaRi.emp.vgc)
head(ItaRi.emp.vgc)
summary(ItaRi.emp.vgc)

par(mfrow=c(1,2), mar = rep(2, 4))
plot(ItaRi.emp.vgc, add.m=1)

ItaRi.bin.vgc <- vgc.interp(ItaRi.spc, N(ItaRi.emp.vgc),m.max=1)
head(ItaRi.bin.vgc)

plot(ItaRi.emp.vgc,ItaRi.bin.vgc,
     legend=c("observed","interpolated"))

ItaRi.fzm <- lnre("fzm", ItaRi.spc, exact=FALSE)
#ItaRi.zm <- lnre("zm", ItaRi.spc, exact=FALSE)
#ItaRi.gigp <- lnre("gigp", ItaRi.spc, exact=FALSE)
summary(ItaRi.fzm)

ItaRi.fzm.spc <- lnre.spc(ItaRi.fzm, N(ItaRi.fzm))

plot(ItaRi.spc,ItaRi.fzm.spc,legend=c("observed","fZM"))

ItaRi.fzm.vgc <- lnre.vgc(ItaRi.fzm, (1:100)*28e+3)

plot(ItaRi.emp.vgc,ItaRi.fzm.vgc,N0=N(ItaRi.fzm), 
     legend=c("observed","fZM"))

## Evaluating extrapolation quality
ItaRi.sub.spc <- sample.spc(ItaRi.spc, N=700000)
ItaRi.sub.fzm <- lnre("fzm", ItaRi.sub.spc, exact=FALSE)
ItaRi.sub.fzm

ItaRi.sub.fzm.vgc <- lnre.vgc(ItaRi.sub.fzm,N=N(ItaRi.emp.vgc))

plot(ItaRi.bin.vgc, ItaRi.sub.fzm.vgc, N0=N(ItaRi.sub.fzm),
     legend=c("interpolated","fZM"))

data(ItaUltra.spc)
summary(ItaUltra.spc)

ItaUltra.fzm <- lnre("fzm",ItaUltra.spc,exact=FALSE)

ItaUltra.fzm 
ItaUltra.ext.vgc <- lnre.vgc(ItaUltra.fzm,N(ItaRi.emp.vgc))
plot(ItaUltra.ext.vgc,ItaRi.bin.vgc,
     legend=c("ultra-","ri-"))

data(package="zipfR")

#####CASE2#####

data(Brown100k.spc)
summary(Brown100k.spc)

Vseen <- V(Brown100k.spc) - Vm(Brown100k.spc,1)
Vseen
Vseen / V(Brown100k.spc)

Vm(Brown100k.spc,1) / N(Brown100k.spc)

Brown100k.zm <- lnre("zm", Brown100k.spc)
Brown100k.zm


EV(Brown100k.zm, c(1e6, 10e6,100e6))
Vseen / EV(Brown100k.zm, c(1e6,10e6,100e6))
1 - (Vseen / EV(Brown100k.zm, c(1e6,10e6,100e6)))


data(Brown.spc)
EV(Brown100k.zm,N(Brown.spc))

1 - (Vseen / V(Brown.spc))
1 - (Vseen / EV(Brown100k.zm, N(Brown.spc)))


Brown.zm.spc <- lnre.spc(Brown100k.zm, N(Brown.spc))

EV(Brown100k.zm, N(Brown.spc)) - Vseen

sum(Vm(Brown.zm.spc, 1))
sum(Vm(Brown.zm.spc, 1:2))
sum(Vm(Brown.zm.spc, 1:6))

Noov.zm <- sum(Vm(Brown.zm.spc, 1:6) * c(1:6))
Noov.zm

Noov.zm / N(Brown.spc)
V(Brown.spc) - Vseen


sum(Vm(Brown.spc, 1))
sum(Vm(Brown.spc, 1:2))
sum(Vm(Brown.spc, 1:13))

Noov.emp <- sum(Vm(Brown.spc, 1:13) * c(1:13))
Noov.emp
Noov.emp / N(Brown.spc)

Brown10M.zm.spc <- lnre.spc(Brown100k.zm, 10e6)
sum(Vm(Brown10M.zm.spc, 1:18) * c(1:18))
sum(Vm(Brown10M.zm.spc, 1:18))

EV(Brown100k.zm, 10e6) - sum(Vm(Brown10M.zm.spc, 1:18))