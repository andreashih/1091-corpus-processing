library(tidyverse)
library(tidytext)
library(quanteda)
library(stringr)
library(jiebaR)
library(readtext)
library(readr)

boy_girl <- read_csv("C:/Users/user/Desktop/1091-corpus-processing/ptt-collostruction/bg_content.txt", col_names = FALSE)
boy_girl <- boy_girl %>%
  mutate(doc_id = row_number()) %>%
  rename(text = X1)

# Initialize the segmenter
segmenter <- worker(user="demo_data/dict-ch-user.txt", 
                    bylines = F, 
                    symbol = T)

# Define own tokenization function
word_seg_text <- function(text, jiebar){
  segment(text, jiebar) %>% # vector output
    str_c(collapse=" ")
}

# Tokenization
boy_girl <- boy_girl %>% # create doccument index
  mutate(text_tag = map_chr(text, word_seg_text, segmenter))

# Define regex
pattern_qilai <- "[^\\s]+\\s起來\\b"

pattern_juede <- "覺得\\s[^\\s]+"

# Extract patterns
boy_girl %>%
  select(-text) %>%
  unnest_tokens(output = construction, 
                input = text_tag, 
                token = function(x) str_extract_all(x, pattern=pattern_qilai)) -> boy_girl_qilai

boy_girl %>%
  select(-text) %>%
  unnest_tokens(output = construction, 
                input = text_tag, 
                token = function(x) str_extract_all(x, pattern=pattern_juede)) -> boy_girl_juede

# Print
boy_girl_qilai
boy_girl_juede

# word freq
boy_girl %>%
  select(-text) %>%
  unnest_tokens(word,
                text_tag,
                token = function(x) str_split(x, "\\s+|\u3000")) %>%
  filter(nzchar(word)) %>%
  count(word, sort = T) -> boy_girl_word

boy_girl_word %>%
  head(100)

# Joint frequency table
boy_girl_qilai %>%
  count(construction, sort=T) %>%
  tidyr::separate(col="construction",
                  into = c("w1","construction"),
                  sep="\\s") %>%
  mutate(w1_freq = boy_girl_word$n[match(w1,boy_girl_word$word)]) -> boy_girl_qilai_table

boy_girl_juede %>%
  count(construction, sort=T) %>%
  tidyr::separate(col="construction",
                  into = c("construction","w2"),
                  sep="\\s") %>%
  mutate(w2_freq = boy_girl_word$n[match(w2,boy_girl_word$word)]) -> boy_girl_juede_table

# prepare for coll analysis
boy_girl_qilai_table %>%
  select(w1, w1_freq, n) %>%
  write_tsv("qilai.tsv")

boy_girl_juede_table %>%
  select(w2, w2_freq, n) %>%
  write_tsv("juede.tsv")

# corpus information
cat("Corpus Size: ", sum(boy_girl_word$n), "\n")

cat("QILAI Construction Size: ", sum(boy_girl_qilai_table$n), "\n")

cat("JUEDE Construction Size: ", sum(boy_girl_juede_table$n), "\n")

# save info in a text
sink("info.txt")
cat("Corpus Size: ", sum(boy_girl_word$n), "\n")
cat("QILAI Construction Size: ", sum(boy_girl_qilai_table$n), "\n")
cat("JUEDE Construction Size: ", sum(boy_girl_juede_table$n), "\n")
sink()

# Create new file
file.create("qilai_results.txt")
file.create("juede_results.txt")

# load the output txt
results <-readLines("qilai_results.txt", encoding = "utf-8")
# subset lines 
results<-results[-c(1:17, (length(results)-17):length(results))]
# convert into CSV
collo_table<-read_tsv(results)

# auto-print
collo_table %>%
  filter(relation =="attraction") %>%
  arrange(desc(coll.strength)) %>%
  head(100) %>%
  select(words, coll.strength, everything())
