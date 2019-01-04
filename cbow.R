library(text2vec)
library(Matrix)
library(sparsesvd)
library(NLP)
library(tm)
library(MASS)

rm(list=ls())

setwd("/home/david/Nextcloud/6. Cours/Word Embedding/Projet/projet_embedding")
#corpus <- readLines("../../../7. Programmation/Données/text9", n=1, warn=FALSE)
corpus <- readLines("../../../../7. Programmation/Données/text8", n=1, warn=FALSE)

# Generation du jeu de donnees d’apprentissage
max_vocabulary_size <- 30000
#corpus <- readLines('text8', n=1, warn=FALSE)
# Tokenisation
corpus <- word_tokenizer(corpus)[[1]]
# Largeur de la fenêtre contexte
l <- 3
# Ajout NA au début et a la fin
corpus <- c(rep("NA", l),corpus,rep("NA", l))
#Suppression des stopwords
stop_words <- tm::stopwords(kind = "en")
corpus <- corpus[!(corpus %in% stop_words)]

dict <- unique(corpus)
#dict1<- unique(corpus[1:30000])
#trouver l'indice de chaque mot correspond
require(parallel)
cls <- makeCluster(detectCores())
clusterExport(cls, c("dict"), envir = environment())
corpus_indice <- unlist(parLapply(cls, corpus[1:max_vocabulary_size], function(x){which(x == dict)}))
stopCluster(cls)
#Construction jeu de donnees
D <- matrix(0, nrow = length(corpus_indice)-2*l, ncol = 1+2*l)
colnames(D) <- c("target", paste0("context_", 1:(2*l)))

for(w in 1:(max_vocabulary_size-2*l)){
  D[w,] <- c(corpus_indice[w+l], corpus_indice[w+l-(l:1)], corpus_indice[w+l+(1:l)])
}

#function softmax 
#   input:
#         U: matrice représentations cibles wi des mots
#         alpha: la moyenne des vecteurs mots contextes
#   output:
#         probabilité observer wi sachant mots contextes c
softmax <- function(U, alpha) {
  # scalaires Ui * alpha
  scal <- exp(U %*% alpha)
  total <- sum(scal)
  
  res <- as.vector(scal/total)
  return(res)
}

#Le gradient de l en Ui
#   input: 
#         p_wi_c:probabilité observer wi sachant mots contextes c
#         alpha:la moyenne des vecteurs mots contextes
#   output:
#         mettre à jour U par gradient stochastique descent
grad_u <- function(p_wi_c, alpha) {
  return(alpha * (1-p_wi_c))
}

#Le gradient de l en Vj
#   input:
#         U: matrice représentations cibles wi des mots
#         i: indice de mot cible
#         e: esperance de probabilite wi sachant mots contextes c
#         l: longeur de fenetre
#   output:
#          mettre à jour V par gradient stochastique descent
grad_v <- function(U, i, e, l) {
  ui <- U[i,]
  return((ui - e)/(2*l))
}

#Algorithme cbow avec calcul explicite du softmax
#   input:
#         D: le jeu de donnee en un multi-ensemble de paires (mots, contexte)
#         vocab: tous les mot identique
#         d: dimension vecteur de mot
#         n_iter: nombre de iteration
#         eta: vitesse de gradient
#   output:
#         U: matrice représentations cibles des mots
#         V: matrice représentations contexte des mots,
my_sgd <- function(D, vocab, d, n_iter, eta = 0.025) {
  # Initialiser aléatoirement U et V
  n <- length(vocab)
  U <- matrix(runif(n*d,-0.2,0.2), nrow = n, ncol = d)
  V <- matrix(runif(n*d,-0.2,0.2), nrow = n, ncol = d)
  # Répéter iter fois
  for(iter in 1:n_iter){
    # Mélanger aléatoirement D
    order <- sample(1:nrow(D), nrow(D), replace = F)
    Dp <- D[order, ]
    # Pour chaque paire wi et contexte, mettre à jour les vecteurs selon les formule
    for(row in 1:nrow(D)){
      # ID du mot cible
      i <- Dp[row, 1]
      # ID des mots contexte
      j <- Dp[row, -1]
      # Calcul du alpha contexte
      alpha <- apply(V[j,], 2, mean)
      # probabilité observer wi sachant mots contextes c
      p_wi <- softmax(U,alpha)
      # MAJ de Ui
      U[i,] <- U[i,] + eta * grad_u(p_wi[i], alpha)
      # Pour chaque mots contexte
      for(word in 2:ncol(D)){
        # ID du mot cible
        jl <- Dp[row, word]
        # MAJ de Vjl
        s_ui <- colSums(U * p_wi)
        V[jl,] <- V[jl,] + eta * grad_v(U, i, s_ui, l)
      }
    }
  }
  return(list(U=U,V=V))
}

# test my_sgd
#vocab<-unique(corpus[1:30000])
vocab<-unique(corpus[1:max_vocabulary_size])
res <- my_sgd(D, vocab,3,5)
U<-res$U
V<-res$V

cosine_similarity <- function(v1, v2){
  dot_product <- v1 %*% v2
  norm_prod <- sqrt(sum(v1**2)) * sqrt(sum(v2**2))
  return(as.numeric(dot_product / norm_prod))
}

find_closest_words <- function(v, n=5){
  similarity <- numeric(nrow(U))
  for(i in 1:nrow(U)){
    similarity[i] <- cosine_similarity(v, U[i, ])
  }
  ordered_words <- dict[order(-similarity)]
  return(ordered_words[1:n])
}

resolve_analogy <- function(word_a, word_b, word_c, n=1){
  word_d <- U[match(word_b, dict), ] - U[match(word_a, dict), ] + U[match(word_c, dict), ]
  return(find_closest_words(word_d, n))
}

cosine_similarity(U[match('cat', dict), ], U[match('car', dict), ])
resolve_analogy('father','mother', 'son')

# Exportation pour Shiny
vectors_cbow <- U
words_cbow <- dict
# Exportation pour Shiny
save(vectors_cbow, words_cbow, file="save_cbow.RData")
