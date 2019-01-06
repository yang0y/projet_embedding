library(text2vec)
library(Matrix)
library(sparsesvd)
library(tictoc)
library(MASS)

rm(list=ls())

#setwd("/home/david/Nextcloud/6. Cours/Word Embedding/Projet/projet_embedding")

tic("Chargement et préparation du corpus")
max_vocabulary_size <- 30000
#corpus <- readLines("../../../../7. Programmation/Données/text8", n=1, warn=FALSE)
#corpus <- readLines('../Data/text9', n=1, warn=FALSE)
corpus <- readLines('text9', n=1, warn=FALSE)
iterator <- itoken(corpus, tokenizer=space_tokenizer, progressbar=FALSE)
vocabulary <- create_vocabulary(iterator)
print(sum(vocabulary$term_count))

pruned_vocabulary <- prune_vocabulary(vocabulary, vocab_term_max=max_vocabulary_size)
vectorizer <- vocab_vectorizer(pruned_vocabulary)
toc()

tic("Création de la matrice de cooccurrence")
l <- 5
X <- create_tcm(iterator, vectorizer, skip_grams_window=l, weights=rep(1, l))
toc()

print(nnzero(X) / max_vocabulary_size**2)

tic("Calcul de la matrice SPPMI diminuée")
k <- 5
total <- sum(X)
word_prob <- colSums(X) / total
M <- X / total # calcul des p_ij
p_i_p_j <- word_prob %*% t(word_prob) # calcul des p_i * p_j
M <- log(M * 1/p_i_p_j) - log(k) # calcul de la PMI diminuée de log(k)
M[is.na(M)] <- 0 # gestion des NA causé par les log(p_ij) quand p_ij = 0
M[M<0] <- 0 # seuillage à 0 pour obtenir l'information mutuelle diminuée positive
toc()

M <- drop0(M) # retrait des 0 explicites pour préserver une représentation creuse efficace
print(nnzero(M) / max_vocabulary_size**2)

tic("Calcul des représentations des mots par décomposition spectrale")
decomposition <- sparsesvd(M, rank=100)
vectors <- decomposition$u %*% sqrt(diag(decomposition$d))
toc()

words <- pruned_vocabulary$term

cosine_similarity <- function(v1, v2){
  dot_product <- v1 %*% v2
  norm_prod <- sqrt(sum(v1**2)) * sqrt(sum(v2**2))
  return(as.numeric(dot_product / norm_prod))
}

find_closest_words <- function(v, n=5){
  similarity <- numeric(nrow(vectors))
  for(i in 1:nrow(vectors)){
    similarity[i] <- cosine_similarity(v, vectors[i, ])
  }
  ordered_words <- words[order(-similarity)]
  return(ordered_words[1:n])
}

resolve_analogy <- function(word_a, word_b, word_c, n=1){
  word_d <- vectors[match(word_b, words), ] - vectors[match(word_a, words), ] + vectors[match(word_c, words), ]
  return(find_closest_words(word_d, n))
}

cosine_similarity(vectors[match('car', words), ], vectors[match('truck', words), ])
cosine_similarity(vectors[match('car', words), ], vectors[match('bike', words), ])
cosine_similarity(vectors[match('car', words), ], vectors[match('boat', words), ])
cosine_similarity(vectors[match('car', words), ], vectors[match('table', words), ])

find_closest_words(vectors[match('car', words), ])

resolve_analogy('father','mother', 'son')
resolve_analogy('france','paris', 'spain')

vectors_skipgram <- vectors
words_skipgram <- words
# Exportation pour Shiny
save(vectors_skipgram, words_skipgram, file="save_skipgram.RData")