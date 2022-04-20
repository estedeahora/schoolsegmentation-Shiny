server <- function(input, output, session) {

  # UI ----------------------------------------------------------------------

  # Armado de grupos --------------------------------------------------------

  GRUPO_tab <- reactive({
    estado <- map_lgl(GRUPO$VAR[GRUPO$CLASE != "n"],
        .f = \(x) c(input[[x]]) )
    
    estado_df <- data.frame(VAR = GRUPO$VAR[GRUPO$CLASE != "n"],
                            estado) 
    GRUPO |>
      left_join(estado_df, by = "VAR") |> 
      mutate(ACTIVA_l = ifelse(CLASE == "n", F, estado),
             ACTIVA = ifelse(ACTIVA_l, 1L, 2L)) |> 
      arrange(ACTIVA, DIM_LAB, CLASE)
      
  })
  
  GRUPO_res <- reactive({
    
    GRUPO_tab() |>
      count(ACTIVA, DIM_LAB, CLASE) |>
      mutate(GRUPO = case_when(ACTIVA == 1 ~ as.character(DIM_LAB),
                               CLASE == "s" ~ paste0(DIM_LAB, " (sup.)"),
                               CLASE == "n" ~ paste0(DIM_LAB, " (sup. cualit.)") ),
             id_g = 1:n())
    })

  # Análisis ----------------------------------------------------------------

  # Calcular MFA
  MFA_res <- eventReactive(
    input$start,{
      req(GRUPO_res())
      
      db <- SCHOOLS
      
      if(input$IMPUTA == T){
        
        gr <- GRUPO_tab()
        
        # Active variables names
        varact <- gr$VAR[gr$ACTIVA_l & gr$CLASE == "s"]
        
        # Imputation
        db[varact] <- db[varact] |>
          imputePCA(scale = T, ncp = 4) |>
          pluck("completeObs")
      }

      db |> 
        select(-ID_s) |>
        select(all_of(GRUPO_tab()[["VAR"]])) |> 
        MFA(group = GRUPO_res()$n, ncp = 5,
            type = as.character(GRUPO_res()$CLASE),
            name.group = GRUPO_res()$GRUPO,
            num.group.sup = GRUPO_res()$id_g[GRUPO_res()$ACTIVA != 1],
            graph = F)
    }
  )   
  
  # Calcular clúster
  CLU_res <- reactive({
    
    db_clu <- MFA_res()$ind$coord[ , 1:4]
    
    k <- input$k
    
    Fclust(db_clu,
           type = "gk",
           k = k, noise = T)
  })
  
  CLU_db <- reactive({

    CLU <- CLU_res()
    k <- input$k
    
    CL_ORDEN <- order(CLU$H[, 1])
    
    CLU_f <- cbind(SCHOOLS, 
                   cl = CLU$clus[ , "Cluster"],
                   cl_p = CLU$clus[ , "Membership degree"],
                   CLU$U[ , CL_ORDEN]) %>%
      mutate(cl = factor(cl, levels = CL_ORDEN,
                         labels = 1:k) )
    
    colnames(CLU_f)[str_starts(colnames(CLU_f), "Clus.")] <- paste0("Clus.", 1:k)
    
    CLU_f
  })
  
  # Data description
  DESC <- reactive({
    data_desc(db = CLU_db())
    })
  
  # Outputs -----------------------------------------------------------------

  # Side panel
  output$resumen <- renderReactable({
    GRUPO_res() |> 
      mutate(Tipo = case_when(ACTIVA == 1 ~ "Activas", 
                              CLASE == "s" ~ "Cuantit. Sup.",
                              CLASE == "n" ~ "Cuanlit. Sup.")) |> 
      select(Tipo, "Dimensión" = DIM_LAB, "Total" = n) |> 
      reactable(resizable = TRUE, 
                groupBy = "Tipo",
                columns = list(Total = colDef(aggregate = "sum")
                )
      )
        
  })
  
  # Panel 1
  output$eig <- renderPlot({
    req(input$RESULT == "eig")
    
    p1 <- fviz_eig(MFA_res(),
             barcolor = "tomato1", barfill = "tomato1",
             linecolor = "grey30",
             addlabels = T, hjust = 0.5,
             main = "Varianza explicada",
             xlab = "Dimensión",
             ylab = "Porcentaje de varianza explicada [%]")
    
    p2 <- MFA_res()$eig[1:10, ] |>
      as.data.frame() |>
      rename(var = 'cumulative percentage of variance') |>
      mutate(Dim = 1:n()) |>
      ggplot(aes(x = Dim, y = var)) +
      geom_point(color = "tomato1", shape = 3) +
      geom_line(color = "grey30", linetype = 2) +
      scale_x_continuous("Dimensión", breaks = 0:10 ) +
      scale_y_continuous("Porcentaje de varianza acumulada [%]", limits = c(0, 100)) +
      labs(title = "Varianza acumulada") +
      theme_minimal()
    
    p1 + p2
  })
  
  # Panel 2
  output$var <- renderPlot({
    
    req(input$RESULT == "var")
    
    # Seleccionar nombres de grupos válidos
    sup <- MFA_res()[["call"]][["num.group.sup"]]
    cua <- MFA_res()[["call"]][["type"]] == "c"
    
    d1 <- data.frame(lab = MFA_res()[["call"]][["name.group"]],
               tip = cua) |> 
      mutate(group = 1:n(),
             act = ! group %in% sup,
             sel = tip & act)
    
    d2 <- MFA_res()$quanti.var$coord |> 
      as_tibble(rownames = "variable") |> 
      left_join(MFA_res()$summary.quanti, by = "variable") |> 
      left_join(d1, by = "group")
    
    fviz_mfa_var(MFA_res(), choice = c("quanti.var"), alpha.var = 0,
                      geom = "arrow", col.var.sup = NA, title = "") +
      geom_segment(data = d2,
                   aes(xend = Dim.1, yend = Dim.2,
                       colour = factor(group)),
                   x = 0, y = 0,
                   arrow = arrow(angle = 25, length = unit(0.25, "cm") ) ) +
      ggrepel::geom_text_repel(data = d2,
                      aes(x = Dim.1, y = Dim.2, label = variable),
                      size = 3, colour = "black") +
      facet_wrap(vars(lab)) +
      scale_color_discrete("Dimensión de análisis") +
      theme(legend.position = "none",
            strip.text.x = element_text(size = 16))
    
  }, 
  height = function(){
              r <- MFA_res()[["group"]][["coord"]] |> nrow()
              ceiling(r / 3) * 350 
            }
  )
  
  # Panel 3
  output$ind <- renderPlot({
    
    req(input$RESULT == "ind")
    
    dat <- bind_cols(MFA_res()$ind$coord |> data.frame(),
                     CLU_db() |> select(cl, cl_p, SECTOR))
    
    colores <- ggsci::pal_futurama()(input$k)
      
    fviz_mfa_var(MFA_res(), col.var = NA, col.var.sup = NA, title = "" ) +
      geom_point(data = dat |> filter(cl_p >= 0.5),
                 mapping = aes(x = Dim.1, y = Dim.2,
                               color = cl, shape = SECTOR)) +
      geom_point(data = dat |> filter(cl_p < 0.5),
                 mapping = aes(x = Dim.1, y = Dim.2,
                               shape = SECTOR), color = "grey80") +
      geom_point(data = data.frame(CLU_res()$H) |> add_row() |> arrange(Dim.1), 
                 shape = 23,
                 mapping = aes(x = Dim.1, y = Dim.2,
                               fill = factor(c(1:input$k, "Sin asignar"))),
                 color = "black", size = 3) +
      scale_color_manual(values = colores) +
      scale_shape("Sector", solid = T) +
      scale_fill_manual("Clúster", values = c(colores, "grey80")) +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            legend.margin = margin(0, 0.8, 0, 0.8, unit="cm")) +
      guides(shape = guide_legend(order = 1),
             fill = guide_legend(order = 2,
                                 direction = "horizontal",
                                 nrow = 2,
                                 title.position = "top"),
             color = "none")
               
  })
  
  output$cluster <- renderReactable({
    
    # Descripción de clusters:
    CLU_db()  |> 
      group_by(cl) |> 
      summarise(Cl.Size = n(),
                Cl.Student = sum(n),
                No.Priv.Size = sum(SECTOR == "Privado"),
                P.Priv.Size = round(No.Priv.Size/Cl.Size * 100, 1),
                Priv.Size = paste0(No.Priv.Size, " (", P.Priv.Size, "%)"),
                No.Priv.Student = sum(n * (SECTOR == "Privado")),
                P.Priv.Student = round(No.Priv.Student/Cl.Student * 100, 1),
                Priv.Student = paste0(format(No.Priv.Student, big.mark = "."), 
                                      " (", P.Priv.Student, "%)"),
                No.Asig = sum(cl_p <= 0.5),
                P.Asig = round(No.Asig/Cl.Size * 100, 1),
                Asig = paste0(No.Asig, " (", P.Asig, "%)"),
                deg.Min = min(cl_p),
                deg.Max = max(cl_p),
                deg.Av = mean(cl_p),
                across(.cols = starts_with("deg."),
                       .fns = ~round(.x, 2)) ) |> 
      select(-P.Asig, -No.Asig, -P.Priv.Size, -No.Priv.Size, 
             -P.Priv.Student, -No.Priv.Student) |> 
      reactable(
        defaultColDef = colDef(
          cell = function(value) format(value, big.mark = ".", 
                                        nsmall = 1, decimal.mark = ","),
          align = "center",
          headerStyle = list(background = "#f7f7f8")
        ),
        columns = list(
          cl = colDef(name = "Clúster", 
                      # sticky = "left", # No disponible aún
                      style = list(fontWeight = "bold")),
          Cl.Size = colDef(name = "Escuelas"),
          Cl.Student = colDef(name = "Estudiantes"),
          Priv.Size = colDef(name = "Escuelas (%)"),
          Priv.Student = colDef(name = "Estudiantes (%)"),
          Asig = colDef(name = "Escuelas difusas (%)"),
          deg.Min = colDef(name = "Min", 
                           style = list(color = "#AFAFAC")),
          deg.Max = colDef(name = "Max", 
                           style = list(color = "#AFAFAC")),
          deg.Av = colDef(name = "Promedio", 
                          style = list(color = "#AFAFAC"))
        ),
        columnGroups = list(
          colGroup(name = "Tamaño del clúster", 
                   columns = c("Cl.Size", "Cl.Student")),
          colGroup(name = "Educación privada", 
                   columns = c("Priv.Size", "Priv.Student")),
          colGroup(name = "Probabilidad de asignación", 
                   columns = c("deg.Min", "deg.Max", "deg.Av"))
        ),
        highlight = TRUE
      )
  })
  
  output$MPC <- renderUI({
    # el indice PC y MPC son mediciones de "difusidad" (fuzzy)
    INDEX <- Fclust.index(CLU_res(), alpha = 1) |> round(3)
    
    tex1 <- paste0("Coeficiente de Partición Modificado (MPC): ", INDEX["MPC"])
    tex2 <- paste0("Fuzzy silhouette: ", INDEX["SIL.F"])
    
    HTML(paste(tex1, tex2, sep = '<br/>'))
  })
  
  # Panel 4
  output$desc_CUALI1 <- renderReactable({
    DESC()$CUALI[1:5, ] |> 
      reactable()
      
  })
  output$desc_CUALI2 <- renderReactable({
    DESC()$CUALI[6:10, ] |> 
      reactable()
    
  })
  
  output$desc_CUANTI1 <- renderReactable({
    DESC()$CUANTI[1:5, ] |> 
      reactable()
  })
  
  output$desc_CUANTI2 <- renderReactable({
    DESC()$CUANTI[6:10, ] |> 
    reactable()
  })
  
}
