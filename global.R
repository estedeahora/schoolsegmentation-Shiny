
# Packages ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinyFeedback)
library(waiter)
library(reactable)

library(tidyverse)
library(ggsci)
library(patchwork)

library(FactoMineR)
library(factoextra)
library(missMDA)
library(fclust)

# library(sf)
# library(leaflet)
# library(leaflet.extras)

# functions ---------------------------------------------------------------
# 
# make_ui <- function(x, var) {
#   html(var)
#   switchInput(
#     inputId = var,
#     size = "mini"
#   )
# }

# Genera un switch (paral seleccionar indicadores)
make_switch <- function(var, act, ind){
    prettySwitch(
      inputId = var,
      label = ind, 
      status = "success",
      fill = TRUE,
      inline = F,
      value = act,
      width = "50%"
    )
}

# Genera un panel de indicadores (grupo), en base a los grupos de indicadores
make_panel <- function(d, MIXTO = F){
  GR <- GRUPO |> filter(DIM_LAB == d)
  
  if(!MIXTO){
    GR <- GR |> filter(CLASE != "n")
  }
  
  tabPanel(title = d,
           # pmap(list(var = GRUPO$VAR[GRUPO$DIM_LAB == d],
           #           act = GRUPO$ACTIVA_l[GRUPO$DIM_LAB == d],
           #           ind = GRUPO$IND_LAB[GRUPO$DIM_LAB == d]),
           pmap(list(var = GR$VAR,
                     act = GR$ACTIVA_l,
                     ind = GR$IND_LAB),
                .f = make_switch)
           )
}

# Descripción de los cluster según variables cuanti y cuali más (5) y menos comunes (5)
data_desc <- function(db, labels = LAB){
  # Preparar data
  dat <- db %>%
    select(-ID_s, -SECTOR, -COOP, -LQ_PRI,
           -cl_p, -starts_with("Clus.")) |>
    select(cl, everything()) |>
    mutate(SUBS = factor(SUBS,
                         levels = c("Estatal", "SI", "NO"),
                         labels = c("Estatal", "Priv. Con Subsidio",
                                    "Priv. Independiente")))
  
  # Generar descripción de variables
  CL_DESC <- catdes(dat, 1)
  
  k <- length(CL_DESC$quanti)
  
  # Agregar Nombres Human-readable a variables
  # for(i in c("test.chi2", "quanti.var")){
  #   CL_DESC[[i]] <- CL_DESC[[i]] %>%
  #     as_tibble(rownames = "VAR") %>%
  #     left_join(LAB, by = "VAR")
  # }
  
  # Agregar Nombres Human-readable a descripción de
  #   clúster por variables cuantitativas
  for(i in 1:k){
    CL_DESC$quanti[[i]] <- CL_DESC$quanti[[i]] %>%
      as_tibble(rownames = "VAR") %>%
      left_join(LAB, by = "VAR")
  }
  
  names(CL_DESC$quanti) <- paste0("Clus.", 1:k)
  
  # Agregar Nombres Human-readable a descripción de
  #   clúster por variables cualitativas
  for(i in 1:k){
    CL_DESC$category[[i]] <- CL_DESC$category[[i]] %>%
      as_tibble(rownames = "V") %>%
      # Generar nombre de indicadores
      mutate(VAR = str_split(V, "=", simplify = T)[ , 1],
             CAT = str_split(V, "=", simplify = T)[ , 2],
             CAT = case_when(CAT %in% c("FALSE", "NO") ~ "No",
                             CAT %in% c("TRUE", "SI") ~ "Sí",
                             T ~ CAT)  ) %>%
      left_join(LAB, by = "VAR") %>%
      # Filtar variables poco informativas
      filter(CAT != "NA" &
               !(str_starts(VAR, "T_" ) & CAT == "No") &
               !(str_starts(VAR, "TIT_" ) & CAT == "No") &
               !(str_starts(VAR, "CONTINUIDAD_" ) &
                   CAT == "No")) %>%
      mutate(IND_LAB = paste(IND_LAB, CAT, sep = ": "))
  }
  names(CL_DESC$category) <- paste0("Clus.", 1:k)
  
  # Crear tablas cuanti y cuali
  C_CUANTI <- C_CUALI <- data.frame()
  for(i in 1:k){
    c <- paste("Clúster", i)
    b <- CL_DESC$quanti[[i]]
    C_CUANTI[1:10, c] <- b[c(1:5, ((nrow(b)-4):nrow(b))), ]  %>%
      mutate(X_ = `Mean in category`,
             X_ = ifelse(str_starts(IND_LAB, "%"),
                         X_ * 100, X_),
             X_ = ifelse(X_ > 1, round(X_, 1), round(X_, 3)),
             IND_LAB = paste(IND_LAB,
                             round(X_, 3),
                             sep = ": ")) %>%
      select(IND_LAB)
    
    b <- CL_DESC$category[[i]]
    C_CUALI[1:10, c] <- b[c(1:5, ((nrow(b)-4):nrow(b))), ]  %>%
      mutate(IND_LAB = paste(IND_LAB, " (",
                             round(`Mod/Cla`, 1), "%)",
                             sep = "")) %>%
      select(IND_LAB)
  }
  return(list(CUALI = C_CUANTI, CUANTI = C_CUALI))
}

# database ----------------------------------------------------------

GRUPO <- read.csv(here::here("data/dimensions.csv")) |>
  mutate(CLASE = factor(CLASE, levels = c("s", "n")),
         DIM_LAB = factor(DIM_LAB, levels = unique(DIM_LAB) ),
         ACTIVA_l = ifelse(ACTIVA == 1, T, F))

SCHOOLS <- read.csv(here::here("data/schools.csv")) |>
  mutate(across(.cols = c(LQ_PRI, LQ_SUP),
                .fns = ~factor(.x, levels = c( "Alto", "Medio", "Bajo"))),
         COOP = factor(COOP, levels = c("c/PJ","s/PJ",  "NO") ),
         
         SUBS = factor(SUBS, levels = c("Estatal", "NO", "SI") ),
         TIPO_HABITAT = factor(TIPO_HABITAT,
                               levels = c("Ciudad Central",
                                          "Centro Administrativo y de Negocios",
                                          "Residencial alto",
                                          "Residencial medio",
                                          "Residencial bajo",
                                          "Conjunto Habitacional",
                                          "Popular de Origen Informal")),
         SECTOR  = factor(SECTOR, levels = c("Estatal", "Privado") ) ) |> 
  select(-starts_with("N_"))

LAB <- GRUPO |> select(VAR, IND_LAB)

# Icon --------------------------------------------------------------

