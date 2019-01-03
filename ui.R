fluidPage(
  titlePanel("Implémentation en R de CBow - Démo"),

  navbarPage("CBOW",
             tabPanel("Similarité et Analogie",
                      fluidRow(h3("Calcul de similarité")),
                      fluidRow(
                        column(3,
                               textInput("similarity1", label = "")
                        ),
                        column(3,
                              textInput("similarity2", label = "")
                        ),
                        column(6,
                              verbatimTextOutput("value_similarity", placeholder=FALSE)
                        )
                      ),
                      fluidRow(h3("Calcul d'analogie")),
                      fluidRow(
                        column(2,
                              textInput("analogy1", label = "")
                              ),
                        column(2,
                              textInput("analogy2", label = "")
                              ),
                        column(2,
                              textInput("analogy3", label = "")
                              ),
                        column(6,
                              verbatimTextOutput("value_analogy", placeholder=FALSE)
                              )
                      ),
                      submitButton("Envoyer")
                      ),
             tabPanel("Autres..."))
)