function(input, output) {
  load("save_cbow.RData")
  load("save_skipgram.RData")
  
  # Fonctions
  cosine_similarity <- function(v1, v2){
    dot_product <- v1 %*% v2
    norm_prod <- sqrt(sum(v1**2)) * sqrt(sum(v2**2))
    return(as.numeric(dot_product / norm_prod))
  }

  find_closest_words <- function(vectors, words, v, n=5){
    similarity <- numeric(nrow(vectors))
    for(i in 1:nrow(vectors)){
      similarity[i] <- cosine_similarity(v, vectors[i, ])
    }
    ordered_words <- words[order(-similarity)]
    return(ordered_words[1:n])
  }

  resolve_analogy_skipgram <- function(word_a, word_b, word_c, n=1){
    word_d <- vectors_skipgram[match(word_b, words_skipgram), ] - vectors_skipgram[match(word_a, words_skipgram), ] + vectors_skipgram[match(word_c, words_skipgram), ]
    return(find_closest_words(vectors_skipgram, words_skipgram, word_d, n))
  }
  
  resolve_analogy_cbow <- function(word_a, word_b, word_c, n=1){
    word_d <- vectors_cbow[match(word_b, words_cbow), ] - vectors_cbow[match(word_a, words_cbow), ] + vectors_cbow[match(word_c, words_cbow), ]
    return(find_closest_words(vectors_cbow, words_cbow, word_d, n))
  }
  
  # Similarités
  similarity_skipgram <- reactive({
    cosine_similarity(vectors_skipgram[match(input$similarity1, words_skipgram), ], vectors_skipgram[match(input$similarity2, words_skipgram), ])
  })
  similarity_cbow <- reactive({
    cosine_similarity(vectors_cbow[match(input$similarity1, words_cbow), ], vectors_cbow[match(input$similarity2, words_cbow), ])
  })

  # Analogies
  analogy_skipgram <- reactive({
    resolve_analogy_skipgram(input$analogy1,input$analogy2, input$analogy3)
  })
  analogy_cbow <- reactive({
    resolve_analogy_cbow(input$analogy1,input$analogy2, input$analogy3)
  })
  
  # Mots les plus proches
  closest_words_skipgram <- reactive({
    find_closest_words(vectors_skipgram, words_skipgram, vectors_skipgram[match(input$closest_words1, words_skipgram), ], 10)
  })
  
  closest_words_cbow <- reactive({
    find_closest_words(vectors_cbow, words_cbow, vectors_cbow[match(input$closest_words1, words_cbow), ], 10)
  })
  
  # Envoi des valeurs
  output$value_similarity_skipgram <- similarity_skipgram
  output$value_similarity_cbow <- similarity_cbow
  output$value_analogy_skipgram <- analogy_skipgram
  output$value_analogy_cbow <- analogy_cbow
  output$value_closest_words_skipgram <- closest_words_skipgram
  output$value_closest_words_cbow <- closest_words_cbow

}