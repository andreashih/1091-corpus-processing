---
layout: post
title: "研究 | 透過 `zipfR` 探討 PTT 八卦板與 Dcard 語料斷詞問題"
author: "Andrea Shih"
categories: journal
tags: [research]
image: cards2.jpg
---
這學期修了老闆的課──語料處理方法，認識了一個強大的 R 套件
[`zipfR`](https://cran.r-project.org/web/packages/zipfR/index.html)
(Evert and Baroni 2007) 以及 LNRE model (Baayen
2002)。這篇文章將參考老師的研究 (Hsieh 2014)，並依據 [The zipfR package
for lexical statistics: A tutorial
introduction](https://rdrr.io/cran/zipfR/f/inst/doc/zipfr-tutorial.pdf)
(Baroni and Evert 2014) 所提供的方法，以 PTT 八卦板與 Dcard
的語料進行中文斷詞問題的分析。

&nbsp;

### 1. Introduction

在使用 `zipfR` 之前，要先認識 [Zipf’s
law](https://nlp.stanford.edu/IR-book/html/htmledition/zipfs-law-modeling-the-distribution-of-terms-1.html)
(Zipf
1949)。簡單說明，即是在自然語言的語料庫中，一個單詞出現的頻率越高，在頻率表裡的排名越前面。且第一名的單詞出現的頻率大約是第二名單詞的兩倍、第三名單詞的三倍，以此類推。

接下來要了解 token 和 type 的意義。這裡有一個範例詞表：

> 蘋果, 橘子, 香蕉, 香蕉, 葡萄, 蘋果, 香蕉, 蘋果, 葡萄, 蘋果, 葡萄, 蘋果

這個詞表總共有 12 個 token (sample size = 12)，以及 4 個 type
(vocabulary size = 4)。在這裡我們把它表示成

> N = 12

> V = 4

根據範例詞表，我們可以得到一個 **type-frequency list**：

<table>
<thead>
<tr class="header">
<th>w</th>
<th>fw</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>蘋果</td>
<td>5</td>
</tr>
<tr class="even">
<td>橘子</td>
<td>1</td>
</tr>
<tr class="odd">
<td>香蕉</td>
<td>3</td>
</tr>
<tr class="even">
<td>葡萄</td>
<td>3</td>
</tr>
</tbody>
</table>

接著，我們又可以得到一個 **Zipf Ranking**，這裡的 `r`
將頻率由大到小排序，`fr` 則是指頻率：

<table>
<thead>
<tr class="header">
<th>w</th>
<th>r</th>
<th>fr</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>蘋果</td>
<td>1</td>
<td>5</td>
</tr>
<tr class="even">
<td>香蕉</td>
<td>2</td>
<td>3</td>
</tr>
<tr class="odd">
<td>葡萄</td>
<td>3</td>
<td>3</td>
</tr>
<tr class="even">
<td>橘子</td>
<td>4</td>
<td>1</td>
</tr>
</tbody>
</table>

透過 **type-frequency
list**，我們可以得到一個計算上非常重要的資料：**frequency
spectrum**，其中 `m` 代表出現次數，`Vm` 代表出現該次數的單詞共有幾個：

<table>
<thead>
<tr class="header">
<th>m</th>
<th>Vm</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>1</td>
</tr>
<tr class="even">
<td>3</td>
<td>2</td>
</tr>
<tr class="odd">
<td>5</td>
<td>1</td>
</tr>
</tbody>
</table>

在範例中，出現一次的單詞有一個 (橘子)，出現三次的單詞有兩個
(香蕉、葡萄)，出現五次的單詞有一個
(蘋果)。只出現過一次的單詞，我們稱之為 *hapax legomena*。

&nbsp;

### 2. Lexical Richness

首先，將 PTT 八卦板和 Dcard 語料匯入並整理。

```r
library(zipfR)

## Warning: package 'zipfR' was built under R version 3.6.3

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

gossip_corpus <- c(g10, g15, g20)
dcard_corpus <- readLines("data\\dcard_corpus.txt", encoding="UTF-8")
```
&nbsp;

#### 2.1 Type-frequency List

使用 `zipfR` 套件，可以簡單地得到兩個 corpus 的 type-frequency list
和它的 Zipf ranking。

```r
gossip.tfl <- vec2tfl(gossip_corpus)
dcard.tfl <- vec2tfl(dcard_corpus)
```

看一下資料的樣子：

```r
head(gossip.tfl)

##    k      f type
## 的 1 182488   的
## 是 2  97224   是
## 了 3  82140   了
## 不 4  77104   不
## 我 5  63054   我
## 有 6  61004   有
## 
##        N      V
##  5601001 137129
```
以下是兩個 corpus 的 sample size 和 type count：

```r
N(gossip.tfl) # sample size

## [1] 5601001

V(gossip.tfl) # type count

## [1] 137129

N(dcard.tfl) # sample size

## [1] 399977

V(dcard.tfl) # type count

## [1] 37097
```
`zipfR` 也可以讓我們輕鬆將資料視覺化：

```r
par(mfrow=c(1,2)) # 1*2 plot area
plot(gossip.tfl, main="PTT-Gossiping", log="xy", # logarithmic scale
        xlab="rank", ylab="frequency")
plot(dcard.tfl, main="Dcard", log="xy",
        xlab="rank", ylab="frequency")
```

![](https://andreashih.github.io/img/rmd_posts/zipfr/tfl.png)

&nbsp;

#### 2.2 Frequency Spectrum

透過剛剛的 frequency list，我們可以得到兩個 corpus 的 frequency
spectrum：

```r
gossip.spc <- tfl2spc(gossip.tfl)
dcard.spc <- tfl2spc(dcard.tfl)
```
看一下資料的樣子：

```r
head(gossip.spc)

##   m    Vm
## 1 1 67652
## 2 2 17569
## 3 3  9178
## 4 4  5761
## 5 5  4044
## 6 6  3095
## 
##        N      V
##  5601001 137129
```
取 log 值後，下圖提供前五十筆 spectrum element 的資料：

```r
par(mfrow=c(1,2))
plot(gossip.spc, log="x", main="PTT-Gossiping",
        xlab="m", ylab="Vm")
plot(dcard.spc, log="x", main="Dcard",
        xlab="m", ylab="Vm")
```

![](https://andreashih.github.io/img/rmd_posts/zipfr/spc.png)

從上圖可以得知，*hapax legomena* (只出現過一次的單詞)
占兩個語料庫的比例都相當的高。接下來，我們可以透過 Vocabulary growth
curves (VGC) 來觀察新詞增加的情況。

&nbsp;

#### 2.3 Vocabulary Growth Curves (VGC)

同樣地，透過 `zipfR`，我們可以將兩個 corpus 轉成 `vgc` object。

```r
gossip.vgc <- vec2vgc(gossip_corpus, m.max=2)
dcard.vgc <- vec2vgc(dcard_corpus, m.max=2)
```

來看一下 `dcard.vgc` 裡面是什麼 :

```r
head(dcard.vgc)

##       N    V   V1  V2
## 1     1    1    1   0
## 2  2010  756  504 101
## 3  4020 1310  868 178
## 4  6030 1815 1182 265
## 5  8040 2311 1501 313
## 6 10050 2690 1725 369
```

以 row 2 為例，表示在前 `2010` 個 token 中，共有 `756` 個
type，而在其中有 `504` 個單詞只出現過一次。

將資料畫成圖表：

```r
par(mfrow=c(1,2))
plot(gossip.vgc, add.m=1, main="PTT-Gossiping",
        xlab="N", ylab="V(N)/V1(N)")
plot(dcard.vgc, add.m=1, main="Dcard",
        xlab="N", ylab="V(N)/V1(N)")
```

![](https://andreashih.github.io/img/rmd_posts/zipfr/vgc.png)

上圖中顏色較深的曲線是 V，較淺的曲線是 V1。從圖中可以發現兩 corpus 的 V1
曲線都十分陡峭，而且呈現持續上升的趨勢，其中 Dcard corpus 的 V1 曲線又比
PTT 八卦板還來得陡。V1 曲線不斷上升，有可能是因為中文斷詞錯誤所造成
(Hsieh 2014)。如果用內建的 BrownNoun corpus
(英文語料，字與字之間由空白分隔，所以沒有斷詞錯誤的問題)
進行比較，就可以很明顯地發現兩 corpus 的 V1 曲線與逐漸穩定的 BrownNoun
corpus 有所不同：

```r
data(BrownNoun.emp.vgc)

par(mfrow=c(1,3))
plot(BrownNoun.emp.vgc, add.m=1, main="BrownNoun",
        xlab="N", ylab="V(N)/V1(N)")
plot(gossip.vgc, add.m=1, main="PTT-Gossiping",
        xlab="N", ylab="V(N)/V1(N)")
plot(dcard.vgc, add.m=1, main="Dcard",
        xlab="N", ylab="V(N)/V1(N)")
```
![](https://andreashih.github.io/img/rmd_posts/zipfr/vgc_compare.png)

透過 VGC，我們可以推測如果持續進行抽樣，就會不斷的出現新詞 (new
types)，vocabulary size 也會一直增加。所以在這裡的 V
值並不穩定。如果我們要推得 corpus 的真實情況，須借助 Large-Number-
of-Rare-Events (LNRE) models (Baayen 2002)。

&nbsp;

#### 2.4 Fitting the LNRE Model

`zipfR` 套件中提供了三種 LNRE models，分別是 Generalized In- verse Gauss
Poisson (`lnre.gigp`; Baayen, 2001, ch. 4)、Zipf-Mandelbrot (`lnre.zm`;
Evert, 2004)，和 finite Zipf-Mandelbrot (`lnre.fzm`; Evert,
2004)，我們首先採用 `fzm`。

```r
gossip_fzm <- lnre("fzm", spc=gossip.spc)
dcard_fzm <- lnre("fzm", spc=dcard.spc)
```
畫出 observed 和 expected spectra：

```r
gossip_fzm.spc <- lnre.spc(gossip_fzm, N(gossip.spc))
dcard_fzm.spc <- lnre.spc(dcard_fzm, N(dcard.spc))

par(mfrow=c(1,2)) # 1*2 plot area
plot(gossip.spc, gossip_fzm.spc,
        main="PTT-Gossiping", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
        fill = 1:2, cex = 0.75)
plot(dcard.spc, dcard_fzm.spc,
        main="Dcard", xlab="m", ylab="Vm")
legend("topright", legend = c("observed", "fZM model"), 
        fill = 1:2, cex = 0.75)
```

![](https://andreashih.github.io/img/rmd_posts/zipfr/spc_exp.png)

比較八卦板與 Dcard 的 observed 和 expected VGC：

```r
gossip_fzm.vgc <- lnre.vgc(gossip_fzm, N(gossip.vgc), m.max=1, variances=TRUE)
dcard_fzm.vgc <- lnre.vgc(dcard_fzm, N(dcard.vgc), m.max=1, variances=TRUE)

par(mfrow=c(1,2)) # 1*2 plot area
plot(gossip.vgc, gossip_fzm.vgc, add.m=1,
        main="PTT-Gossiping", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
        fill = 1:2, cex = 0.5)
plot(dcard.vgc, dcard_fzm.vgc, add.m=1,
        main="Dcard", xlab="N", ylab="V(N)/V1(N)")
legend("topleft", legend = c("observed", "fZM model"),
        fill = 1:2, cex = 0.5)
```
![](https://andreashih.github.io/img/rmd_posts/zipfr/vgc_exp.png)

上方的紅色曲線代表透過 fZM model 所產生的 expected value。

&nbsp;

### 3. Lexical Coverage Estimation

#### 3.1 Out-Of-Vocabulary (OOV) types

我們可以藉由 Out-Of-Vocabulary (OOV) types 的比例來了解分析兩個 corpus
的 lexical
coverage，也就是可被辨認的單詞所佔的比例。基本上，一個實用的語言資源中，OOV
所佔的比例應保持在一定標準之下。

我們先採兩 corpus 的前十萬個 lemma：

```r
gossip100k <- head(gossip_corpus, 100000)
dcard100k <- head(dcard_corpus, 100000)
```

同樣地，將他們轉成 `spc` object：

```r
gossip100k.tfl <- vec2tfl(gossip100k)
dcard100k.tfl <- vec2tfl(dcard100k)

gossip100k.spc <- tfl2spc(gossip100k.tfl)
dcard100k.spc <- tfl2spc(dcard100k.tfl)
```

首先，我們要計算 lexical of seen
types。也就是說，我們想要知道語料中至少出現過**兩次**的 type
總共有幾個：

```r
gossip_Vseen <- V(gossip100k.spc) - Vm(gossip100k.spc, 1)
dcard_Vseen <- V(dcard100k.spc) - Vm(dcard100k.spc, 1)

gossip_Vseen

## [1] 5277

dcard_Vseen

## [1] 5948
```

這樣就可以算出在八卦板與 Dcard 語料前十萬個 lemma 中，*OOV types*
所佔的比例大約是 0.545 與 0.591。

```r
1 - gossip_Vseen / V(gossip100k.spc)

## [1] 0.5193113

1 - dcard_Vseen / V(dcard100k.spc)

## [1] 0.5912589
```

如果我們再進一步看 *hapax legomena* (只出現一次的詞，在這裡也稱為 *OOV
lemmas*) 在整個語料中的佔比，

```r
Vm(gossip100k.spc, 1) / N(gossip100k.spc)

## [1] 0.05701

Vm(dcard100k.spc, 1) / N(dcard100k.spc)

## [1] 0.08604
```

就可以發現兩語料中 *OOV lemmas* 佔所有 tokens 的比例分別是 5% 和 8%。

&nbsp;

#### 3.2 Fitting the LNRE Model

```r
gossip100k.zm <- lnre("zm", gossip100k.spc)
dcard100k.zm <- lnre("zm", dcard100k.spc)
```

以八卦板語料為例，假設我們用 `1, 10, or 100 million tokens`
進行計算，expected OOV 分別會是：

```r
1 - (gossip_Vseen / EV(gossip_fzm, c(1e6, 10e6, 100e6)))

## [1] 0.9081713 0.9709728 0.9885432
```

相同的計算方式在 Dcard 語料中再試一次，expected OOV 分別會是：

```r
1 - (dcard_Vseen / EV(dcard_fzm, c(1e6, 10e6, 100e6)))

## [1] 0.9022872 0.9564876 0.9599495
```

可以發現八卦板和 Dcard 語料的 OOV ratio 隨著 corpus size
的增加不斷地上升。我們用 Brown corpus 的資料來比較看看：

```r
data("Brown100k.spc")
Brown_fzm <- lnre("fzm", Brown100k.spc)
Brown_Vseen <- V(Brown100k.spc) - Vm(Brown100k.spc, 1)
1 - (Brown_Vseen / EV(Brown_fzm, c(1e6, 10e6, 100e6)))

## [1] 0.7299295 0.7347806 0.7347806
```

與八卦板與 Dcard 語料不同的是，Brown corpus 的 OOV ratio 相對穩定。

&nbsp;

### 4. 結論

利用 `zipfR` 並採用 LNRE model 對資料進行分析，取得兩語料的 `VGC` 和
`OOV ratio`，就可以分析八卦板與 Dcard 語料的 lexical richness 和 lexical
coverage。目前的發現是：

1.  不同於 BrownNoun 最後達成平緩的趨勢，八卦板與 Dcard 的 `VGC` V1
    曲線都隨著 sample size 增加持續上升，且 Dcard 的斜率又較八卦板更大。

2.  從 expected OOV ratio 來看，八卦板與 Dcard 語料的 OOV 也隨著 corpus
    size 的增加持續上升，並不如 Brown corpus 穩定。

可能的原因是，Brown Corpus
是英文語料，字詞與字詞之間已以空白分隔，然而中文語料並無此特性，且前處理通常採用機器自動斷詞，若出現新詞則很有可能被斷開而成為
OOV。這說明了在建造中文 Web-as-Corpus (Kilgarriff and Grefenstette 2003)
時，斷詞問題是很重要的考量之一。

&nbsp;

### 5. References

Baayen, R. H. (2002). Word frequency distributions (Vol. 18). Springer
Science & Business Media.

Evert S, Baroni M (2007). “zipfR: Word Frequency Distributions in R.” In
Proceedings of the 45th Annual Meeting of the Association for
Computational Linguistics, Posters and Demonstrations Sessions, 29–32.
(R package version 0.6-70 of 2020-10-10).

Kilgarriff, A., & Grefenstette, G. (2003). Introduction to the special
issue on the web as corpus. Computational linguistics, 29(3), 333-347.

Hsieh, S.-K. (2014, may). Why Chinese Web-as-Corpus is Wacky? Or: How
Big Data is Killing Chinese Corpus Linguistics. Paper presented at the
Proceedings of the Ninth International Conference on Language Resources
and Evaluation (LREC?14), Reykjavik, Iceland.

&nbsp;

### 6. Special Thanks

謝謝 Jessy 和 Yongfu 在 [HOCOR
2020](https://github.com/lopentu/Hands-on_Corpus_Linguistics) 提供的
corpus data。