function(input, output) {
  load("save.RData")

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
  similarity <- reactive({
    cosine_similarity(U[match(input$similarity1, dict), ], U[match(input$similarity2, dict), ])
  })

  analogy <- reactive({
    resolve_analogy(input$analogy1,input$analogy2, input$analogy3)
  })
  # You can access the value of the widget with input$text, e.g.
  output$value_similarity <- similarity
  output$value_analogy <- analogy

}