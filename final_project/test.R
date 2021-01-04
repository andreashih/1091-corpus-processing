library(zipfR)
gossiping <- readLines("Gossiping_2005_seg.txt", encoding="UTF-8")
gossiping <- unlist(strsplit(gossiping, "\\s"))