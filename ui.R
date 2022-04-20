ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  waiter::use_waitress(),
  # fluidRow(h2("Aplication name")), 
  sidebarLayout(
      # Todos
      sidebarPanel(width = 3,
        # fluidRow(HTML("<br><br>")), 
        tabsetPanel(id = "SETUP_var",
                    type = "hidden",
                    tabPanel(title = "select", value = "select",
                             do.call("tabsetPanel",
                                     map(.x = as.character(unique(GRUPO$DIM_LAB)),
                                         .f = ~make_panel(.x))
                             )
                    )#,
                    # Para desarrollar setup de indicadores por dimensión
                    # tabPanel(title = "setup", value = "setup",
                    #           radioGroupButtons(
                    #             inputId = "idxxx",
                    #             label = "Label",
                    #             choices = unique(GRUPO$DIM_LAB)
                    #             justified = TRUE,
                    #             checkIcon = list(
                    #               yes = icon("ok", 
                    #                          lib = "glyphicon"))
                    #           )
                    # )
        ),
        fluidRow(
          reactableOutput("resumen")
        )
    ),
    mainPanel(width = 9,
      fluidRow(
        column(3,
               # Elección del número de clúster
               sliderInput(
                 inputId = "k", label = "N Clúster",
                 min = 2, max = 8, value = 4, step = 1) 
        ),
        column(3, 
               HTML("<br>"),
               # Elección de imputación (default: TRUE)
               switchInput(
                 inputId = "IMPUTA",
                 label = "Imputar", 
                 value = T,
                 size = "mini"
               )
        ),
        column(3),
        column(2,
               HTML("<br>"),
               actionButton("start", "Calcular resultados", width = "100%")
        ),
        
      ),
      tabsetPanel(id = "RESULT", 
                  # Varianza explicada
                  tabPanel(title = "Varianza explicada", 
                           value = "eig",
                           shiny::plotOutput("eig")),
                  # Círculo de variables por grupo
                  tabPanel(title = "Variables",
                           value = "var",
                           shiny::plotOutput("var")),
                  # Distribución de individuos por clúster
                  tabPanel(title = "Escuelas",
                           value = "ind",
                           plotOutput("ind", height = 550),
                           HTML("<br>"),
                           reactableOutput("cluster"),
                           HTML("<br>"),
                           htmlOutput("MPC")),
                  # Tabla con valores típicos/atípicos
                  navbarMenu(title = "Descripción de Clústers", 
                             tabPanel(title = "Cualitativa",
                                      HTML("<h3>Proporción sobre la media</h3>"),
                                      reactableOutput("desc_CUALI1"),
                                      HTML("<br>"),
                                      HTML("<h3>Proporción debajo de la media</h3>"),
                                      reactableOutput("desc_CUALI2")),
                             tabPanel(title = "Cuantitativa",
                                      HTML("<h3>Porcentaje sobre la media</h3>"),
                                      reactableOutput("desc_CUANTI1"),
                                      HTML("<br>"),
                                      HTML("<h3>Porcentaje debajo de la media</h3>"),
                                      reactableOutput("desc_CUANTI2")
                             )
                  )
      )

    )
    
  )
)
  