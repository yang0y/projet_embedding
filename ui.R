fluidPage(
  titlePanel("Implémentation en R de CBow - Démo"),

  navbarPage("CBOW",
             tabPanel("Similarité",
                      fluidRow(
                        column(6,
                               h2("Calcul de similarité")),
                        column(3,
                               h2("Skip-gram")),
                        column(3,
                               h2("CBOW"))),
                      fluidRow(
                        column(3,
                               textInput("similarity1", label = "" ,value ="mother")
                               ),
                        column(3,
                               textInput("similarity2", label = "",value ="father")
                               ),
                        column(3,
                               verbatimTextOutput("value_similarity_skipgram", placeholder=TRUE)
                               ),
                        column(3,
                               verbatimTextOutput("value_similarity_cbow", placeholder=TRUE)
                               )
                      ),
                      fluidRow(
                        submitButton("Envoyer"))),
             tabPanel("Analogie",
                      fluidRow(
                        column(6,
                               h2("Calcul d'analogie")),
                        column(3,
                               h2("Skip-gram")),
                        column(3,
                               h2("CBOW"))),
                      fluidRow(
                        column(2,
                               textInput("analogy1", label = "",value ="mother")
                               ),
                        column(2,
                               textInput("analogy2", label = "",value ="father")
                               ),
                        column(2,
                               textInput("analogy3", label = "",value ="son")
                               ),
                        column(3,
                               verbatimTextOutput("value_analogy_skipgram", placeholder=TRUE)
                               ),
                        column(3,
                               verbatimTextOutput("value_analogy_cbow", placeholder=TRUE)
                               )
                      ),
                      fluidRow(
                        submitButton("Envoyer"))),
             tabPanel("Mots les plus proches",
                      fluidRow(
                        column(2,
                               h2("Mots les plus proches")),
                        column(5,
                               h2("Skip-gram")),
                        column(5,
                               h2("CBOW"))),
                      fluidRow(
                        column(2,
                               textInput("closest_words1", label = "")
                               ),
                        column(5,
                               verbatimTextOutput("value_closest_words_skipgram", placeholder=TRUE)
                               ),
                        column(5,
                               verbatimTextOutput("value_closest_words_cbow", placeholder=TRUE)
                               )
                      ),
                      fluidRow(
                        submitButton("Envoyer"))
                      ),
             tabPanel("Vraisemblance",
                      fluidRow(
                        column(6,
                               h2("Calcul Vraisemblance d'une phrase")),
                        column(3,
                               h2("Skip-gram")),
                        column(3,
                               h2("CBOW"))),
                      fluidRow(
                        column(6,
                               textInput("sentence", label = "", value = "i like stay at home we are family")
                        ),
                        column(3,
                               verbatimTextOutput("value_vraisemblance_skipgram", placeholder=TRUE)
                        ),
                        column(3,
                               verbatimTextOutput("value_vraisemblance_cbow", placeholder=TRUE)
                        )
                      ),
                      fluidRow(
                        submitButton("Envoyer"))

                      ),
             tabPanel("Projections",
                      fluidRow(
                        column(12,
                               h2("les projections en 2 dimensions")
                        )),
                      fluidRow(
                        column(10,
                              textInput("visu_mots", label = "", value = "big large cat dog mother father animal")
                        ),
                        column(2,
                               submitButton("Envoyer")
                        )
                      ),
                      fluidRow(
                        column(6,
                               h2("Skip-gram")),
                        column(6,
                               h2("CBOW"))),
                      fluidRow(
                        column(6,
                               plotOutput("projection_skipgram")
                        ),
                        column(6,
                               plotOutput("projection_cbow")
                        )
                      ))
                      
             )
  )
