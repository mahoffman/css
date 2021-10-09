library(xlsx)

personA <- c("Mark", "Mark", "Peter", "Peter", "Bob", "Jill")
personB <- c("Peter", "Jill", "Bob", "Aaron", "Jill", "Aaron")

edgelist <- data.frame(PersonA = personA, 
                       PersonB = personB, 
                       stringsAsFactors = F)

adjacency <- matrix(
  c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0), 
  nrow = 5, 
  ncol = 5, 
  dimnames = list(c("Mark", "Peter", "Bob", "Jill", "Aaron"), c("Mark", "Peter", "Bob", "Jill", "Aaron")))

install.packages("igraph")
library(igraph)

n1 <- graph.edgelist(as.matrix(edgelist))
n2 <- graph.adjacency(adjacency)

plot(n1, vertex.size = degree(n1)*10)

###
library(textmineR)
install.packages(c("gutenbergr", "textmineR"))

library(gutenbergr)
library(tidyverse)
library(tidytext)
library(textmineR)

View(gutenberg_metadata)

book <- gutenberg_download(c(61), 
                                   meta_fields = "title")

tokenized_words <- unnest_tokens(book, 
                                 word, 
                                 text, 
                                 strip_punct = F)

data(stop_words)

# 
word_counts <- anti_join(tokenized_words, stop_words, by = "word") 


remove_punctuation <- data.frame(word = c("*", ";", ",", ":", "[", "]", "-"), 
                                 lexicon = "bad punctuation",
                                 stringsAsFactors = F)
word_counts <- anti_join(word_counts, 
                         remove_punctuation, 
                         by = "word")



word_counts <- anti_join(word_counts, 
                         data.frame(word = c("*", ";", ",", ":", "[", "]", "-"), 
                                    lexicon = "bad punctuation",
                                    stringsAsFactors = F), 
                         by = "word")

word_counts <- word_counts[17:nrow(word_counts),]

communist_manifesto <- paste0(word_counts$word, collapse = " ")

library(textmineR)
skip_gram_manifesto <- CreateTcm(doc_vec = communist_manifesto,
                                    skipgram_window = 10,
                                    verbose = FALSE,
                                    cpus = 2)

install.packages("proxy")
library(proxy)

simil_constitution <- simil(as.matrix(skip_gram_manifesto), 
                            method = "cosine")

simil_manifesto <- as.matrix(simil_constitution)
simil_manifesto[] <- ifelse(simil_manifesto[] < 0.4, 
                            0, 
                            simil_manifesto[])
diag(simil_manifesto) <- 0


library(igraph)
manifesto_network <- graph.adjacency(simil_manifesto, 
                                     weighted = T, 
                                     mode = "undirected")


plot(manifesto_network, 
     vertex.label.cex = 0.3, 
     vertex.size = 1, 
     edge.width = 0.3, 
     edge.arrow.size = 0.2, 
     vertex.color = membership(cluster_louvain(manifesto_network)))


communities(cluster_louvain(manifesto_network))


library(word2vec)
w2v_cm <- word2vec(communist_manifesto)