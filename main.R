rm(list=ls())
source("cbow.R")

# on choisit l=5 comme skip-gram en seance 3
data<-generation_data("text8",5 ,size_text = 100000)
res <- my_sgd(data$D, data$vocab, d = 100, n_iter = 3)

# Exportation pour Shiny
vectors_cbow <- res$U
words_cbow <- data$vocab
# Exportation pour Shiny
save(vectors_cbow, words_cbow, file="save_cbow100.RData")
