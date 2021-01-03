## 
## This script illustrates basic LNRE modelling with the zipfR package.
## 

## load the zipfR package
library(zipfR)

## The easiest and most flexible way to read your own data is in the form of 
## token vector in "vertical" (one token per line) format. R easily handles
## samples of several million tokens. For larger data sets, it may be necessary
## to pre-compute frequency spectra and vocabulary growth curves using external
## software; see ?read.tfl, ?read.spc and ?read.vgc for more information and the
## required file formats.

## On the tutorial homepage, you will find a "vertical" file that contains all
## adverb tokens from the Brown Corpus in their original ordering (brown_adverbs.txt).
## Please download this file and put it into your working directory.

## read token vector in "vertical" format
adv <- readLines("brown_adverbs.txt")

## compile type-frequency list from token vector
adv.tfl <- vec2tfl(adv)

adv.tfl # printing shows N, V and most frequent types
plot(adv.tfl, log="xy") # plot Zipf ranking

## obtain frequency spectrum from type-frequency list
adv.spc <- tfl2spc(adv.tfl) # or directly with vec2spc(adv)

adv.spc # shows first spectrum elements
plot(adv.spc) # barplot of frequency spectrum

## compile vocabulary growth curve (with V1 and V2)
adv.vgc <- vec2vgc(adv, m.max = 2)

adv.vgc # prints a few sample sizes
plot(adv.vgc, add.m = 1:2) # vocabulary growth curve

## list of built-in data sets for further experiments
data(package="zipfR")


## LNRE modelling usually consists of three steps:
##  1. Estimate model paramters from observed frequency spectrum
##  2. Determine goodness-of-fit (obs vs. exp plot, chisq test)
##  3. Compute and extrapolate expectations and variances/confidence intervals

## The zipfR package currently supports three types of LNRE models:
##  - zm = Zipf-Mandelbrot (infinite population diversity)
##  - fzm = finite Zipf-Mandelbrot (finite population diversity)
##  - gigp = Generalized Inverse Gauss-Poisson (finite population diversity)
##           (limited support because of its mathematical complexity)

## LNRE models are fitted with the lnre() function, which takes the type
## of model as its first argument. Instead of an observed frequency spectrum
## for automatic parameter estimation, the model parameters can also be
## specified directly. It is also possible to specify some parameters and
## estimate the others from the frequency spectrum.

## since adverb formation is a highly productive process in English,
## it is reasonable to fit a ZM model with infinite population diversity
zm <- lnre("zm", spc=adv.spc)

## resulting LNRE object prints as summary of model parameters, observed
## vs. expected frequency spectrum and goodness-of-fit test
zm 

## Note that p << .001, so the model is not consistent with the observed data.
## Since the Zipf-Mandelbrot law is a massive simplification, it is common
## to work with such poorly fitting models. The comparison of observed and
## expected frequency spectrum looks reasonable except for V1.

## compute expected frequency spectrum (at the observed sample size)
zm.spc <- lnre.spc(zm, N(adv.spc)) # lnre.* are convenience functions

## all plot methods take multiple objects for comparative plots
plot(adv.spc, zm.spc, legend=c("observed", "ZM model"))

## expected vocabulary growth curve; specify variances=TRUE to include
## variance data for confidence intervals
zm.vgc <- lnre.vgc(zm, N(adv.vgc), m.max=1, variances=TRUE)
plot(adv.vgc, zm.vgc, add.m=1)

## if the ZM model overestimates hapaxes, i.e. E[V1] >> V1, an fZM model
## often achieves a considerably better goodness-of-fit
fzm <- lnre("fzm", spc=adv.spc)
fzm # p > .05, so this model is actually consistent with the data

## estimated population diversity (also printed in the summary above)
fzm$S

## spectrum and vocabulary growth curve fit much better now
fzm.spc <- lnre.spc(fzm, N(adv.spc))
plot(adv.spc, zm.spc, fzm.spc, legend=c("observed", "ZM", "fZM"))

fzm.vgc <- lnre.vgc(fzm, N(adv.vgc), m.max=1, variances=TRUE)
plot(adv.vgc, zm.vgc, fzm.vgc, add.m=1, legend=c("observed", "ZM", "fZM"))

## Note that the vocabulary growth curves for hapaxes (thin lines) still look 
## quite different from the observed data, which has relatively steep growth
## for the first 10,000 tokens and is much flatter afterwards. If you are 
## familiar with the Brown Corpus, can you guess why this might be the case?

