# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(readxl)
library(haven)
library(gt)
library(gtsummary)
source("modules/mod_map_page.R")  # Importer le module avant d'appeler ses fonctions


# UI
ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Tableaux de Bord"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Spatial Analysis", tabName = "page2", icon = icon("map")),
      menuItem("EHCVM Analysis", tabName = "page3", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    # Include CSS for styling
    tags$head(
      tags$style(HTML("
      .dark-box {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .red-title-box {
        background-color: #28a745;
        color: white;
        padding: 10px;
        margin-bottom: 15px;
        border-radius: 3px;
      }
    "))
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tags$head(
      tags$style(HTML("
        .skin-blue .sidebar-menu>li.active>a {
          color: #fff;
          background-color: #12e6ac;
        }
        .skin-blue .sidebar-menu>li:hover>a {
          color: #fff;
          background-color: #128fe6;
        }
        /* Fixer l'en-tête */
        .main-header {
          position: fixed;
          width: 100%;
        }
        
        /* Fixer la barre latérale */
        .main-sidebar {
          position: fixed;
        }
        /* Ajuster le contenu principal */
        .content-wrapper {
          margin-top: 50px;
          padding-top: 15px;
        }
      "))
    ),
    tabItems(
      # Page 2 - Spatial Analysis
      tabItem(tabName = "page2",
              mod_map_page_ui("map_page")  
              
      ),
      
      
      
      
      
      tabItem(tabName = "page3",
              div(id = "page3", 
                  tabsetPanel(
                    tabPanel("Chargement", icon = icon("file-upload"),
                             fluidRow(
                               box(
                                 title = "Chargement des bases EHCVM",
                                 status = "warning",
                                 width = 12,
                                 fluidRow(
                                   column(6,
                                          fileInput("file_menage", "Base menage",
                                                    accept = c(".csv", ".xlsx", ".xls", ".rds",".dta", ".Rdata"))
                                   ),
                                   column(6,
                                          fileInput("file_individu", "Base individu",
                                                    accept = c(".csv", ".xlsx", ".xls", ".rds",".dta", ".Rdata"))
                                   )
                                 )
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Aperçu de la base ménage",
                                 status = "danger",
                                 width = 6,
                                 DTOutput("raw_preview_menage")
                               ),
                               box(
                                 title = "Aperçu de la base individu",
                                 status = "danger",
                                 width = 6,
                                 DTOutput("raw_preview_individu")
                               )
                             )
                    ),
                    tabPanel("Traitement",icon = icon("tools"),
                             # Fusion des bases
                             fluidRow(
                               box(
                                 title = "Fusion des bases MENAGE et INDIVIDU",
                                 width = 12,
                                 status = "info",
                                 actionButton("merge_bases", "Fusionner les bases"),
                                 downloadButton("download_finale", "Télécharger base finale")
                               )
                             ),
                             # Traitement des deux bases
                             fluidRow(
                               box(
                                 title = "Traitement de la base ménage",
                                 width = 6,
                                 status = "primary",
                                 tabsetPanel(
                                   tabPanel("Nettoyage NA",
                                            actionButton("remove_duplicates_menage", "Supprimer les doublons",
                                                         style = "background-color:#052d90; color:white; font-weight:bold;"),
                                            selectInput("var_na_menage", "Variable à traiter (valeurs manquantes)", choices = NULL),
                                            verbatimTextOutput("na_info_menage"),
                                            actionButton("remove_na_menage", "Supprimer les lignes",
                                                         style = "background-color:#16a085; color:white;"),
                                            actionButton("mean_na_menage", "Remplacer par moyenne",
                                                         style = "background-color:#16a085; color:white;"),
                                            actionButton("median_na_menage", "Remplacer par médiane", 
                                                         style = "background-color:#16a085; color:white;")
                                   ),
                                   tabPanel("Gestion des variables",
                                            selectInput("var_drop_menage", "Variable à supprimer", choices = NULL),
                                            actionButton("drop_var_menage", "Supprimer variable",
                                                         style = "background-color:#052d90; color:white; font-weight:bold;"),
                                            tags$hr(),
                                            h4("Renommer une variable"),
                                            selectInput("rename_var_menage", "Sélectionner la variable à renommer", choices = NULL),
                                            textInput("new_name_menage", "Nouveau nom", value = ""),
                                            actionButton("rename_var_menage_btn", "Valider le renommage",
                                                         style = "background-color:#16a085; color:white; font-weight:bold;")
                                   ),
                                   tabPanel("Modalités",
                                            h4("Révéler modalités"),
                                            selectInput("modalities_var_menage", "Sélectionner la variable", choices = NULL),
                                            actionButton("reveal_modalities_menage", "Révéler modalités",
                                                         style = "background-color:#16a085; color:white; font-weight:bold;"),
                                            verbatimTextOutput("modalities_menage_out")
                                   )
                                 )
                               ),
                               box(
                                 title = "Traitement de la base individu",
                                 width = 6,
                                 status = "primary",
                                 tabsetPanel(
                                   tabPanel("Nettoyage NA",
                                            actionButton("remove_duplicates_individu", "Supprimer les doublons",
                                                         style = "background-color:#052d90; color:white; font-weight:bold;"),
                                            selectInput("var_na_individu", "Variable à traiter (valeurs manquantes)", choices = NULL),
                                            verbatimTextOutput("na_info_individu"),
                                            actionButton("remove_na_individu", "Supprimer les lignes",
                                                         style = "background-color:#16a085; color:white;"),
                                            actionButton("mean_na_individu", "Remplacer par moyenne",
                                                         style = "background-color:#16a085; color:white;"),
                                            actionButton("median_na_individu", "Remplacer par médiane", 
                                                         style = "background-color:#16a085; color:white;")
                                   ),
                                   tabPanel("Gestion des variables",
                                            selectInput("var_drop_individu", "Variable à supprimer", choices = NULL),
                                            actionButton("drop_var_individu", "Supprimer variable",
                                                         style = "background-color:#052d90; color:white; font-weight:bold;"),
                                            tags$hr(),
                                            h4("Renommer une variable"),
                                            selectInput("rename_var_individu", "Sélectionner la variable à renommer", choices = NULL),
                                            textInput("new_name_individu", "Nouveau nom", value = ""),
                                            actionButton("rename_var_individu_btn", "Valider le renommage",
                                                         style = "background-color:#16a085; color:white; font-weight:bold;")
                                   ),
                                   tabPanel("Modalités",
                                            h4("Révéler modalités"),
                                            selectInput("modalities_var_individu", "Sélectionner la variable", choices = NULL),
                                            actionButton("reveal_modalities_individu", "Révéler modalités",
                                                         style = "background-color:#16a085; color:white; font-weight:bold;"),
                                            verbatimTextOutput("modalities_individu_out")
                                   )
                                 )
                               )
                             ),
                             # Aperçu des données après traitement
                             fluidRow(
                               box(
                                 title = "Aperçu des données ménage (après traitement)",
                                 status = "success",
                                 width = 6,
                                 DTOutput("preview_menage")
                               ),
                               box(
                                 title = "Aperçu des données individu (après traitement)",
                                 status = "success",
                                 width = 6,
                                 DTOutput("preview_individu")
                               )
                             )
                    ),
                    tabPanel("Graphiques", icon = icon("chart-simple"),
                             fluidPage(
                               fluidRow(
                                 valueBoxOutput("box_menage_nrow", width = 3),
                                 valueBoxOutput("box_menage_nvar", width = 3),
                                 valueBoxOutput("box_individu_nrow", width = 3),
                                 valueBoxOutput("box_individu_nvar", width = 3)
                               ),
                               br(),
                               fluidRow(
                                 # --- Statistiques et graphiques pour MENAGE ---
                                 column(6,
                                        conditionalPanel(
                                          condition = "output.base_menage_loaded == true", 
                                          box(
                                            title = "Statistiques sur la base menage",
                                            width = NULL,
                                            status = "primary",
                                            selectInput("menage_var1", "Choisir une variable numérique", choices = NULL),
                                            verbatimTextOutput("summary_menage")
                                          ),
                                          selectInput("menage_plot_type_uni", "Type de graphique",
                                                      choices = c("Barres", "Boxplot")),
                                          box(title = "Graphique univarié de la base menage",
                                              width = NULL, status = "info",
                                              selectInput("menage_var2", "Variable :", choices = NULL),
                                              plotlyOutput("plot_menage_uni")),
                                          selectInput("menage_plot_type_bi", "Type de graphique bivarié",
                                                      choices = c("Barres", "Boxplot")),
                                          box(title = "Graphique bivarié de la base menage",
                                              width = NULL, status = "success",
                                              selectInput("menage_var3", "Variable X :", choices = NULL),
                                              selectInput("menage_var4", "Variable Y :", choices = NULL),
                                              plotlyOutput("plot_menage_bi"))
                                        ),
                                        conditionalPanel(
                                          condition = "output.base_menage_loaded == false", 
                                          box(
                                            title = "Statistiques menage",
                                            width = NULL,
                                            status = "primary",
                                            "Base menage non chargée"
                                          ),
                                          box(title = "Graphique univarié de la base menage",
                                              width = NULL, status = "info",
                                              ""),
                                          box(title = "Graphique bivarié de la base menage",
                                              width = NULL, status = "success",
                                              "")
                                        )
                                 ),
                                 # --- Statistiques et graphiques pour INDIVIDU ---
                                 column(6,
                                        conditionalPanel(
                                          condition = "output.base_individu_loaded == true", 
                                          box(
                                            title = "Statistiques sur les individus",
                                            width = NULL,
                                            status = "primary",
                                            selectInput("individu_var1", "Choisir une variable numérique", choices = NULL),
                                            verbatimTextOutput("summary_individu")
                                          ),
                                          selectInput("individu_plot_type_uni", "Type de graphique",
                                                      choices = c("Barres", "Boxplot")),
                                          box(title = "Graphique univarié de la base individu",
                                              width = NULL, status = "info",
                                              selectInput("individu_var2", "Variable :", choices = NULL),
                                              plotlyOutput("plot_individu_uni")),
                                          selectInput("individu_plot_type_bi", "Type de graphique bivarié",
                                                      choices = c("Barres", "Boxplot")),
                                          box(title = "Graphique bivarié de la base individu",
                                              width = NULL, status = "success",
                                              selectInput("individu_var3", "Variable X :", choices = NULL),
                                              selectInput("individu_var4", "Variable Y :", choices = NULL),
                                              plotlyOutput("plot_individu_bi"))
                                        ),
                                        conditionalPanel(
                                          condition = "output.base_individu_loaded == false", 
                                          box(
                                            title = "Statistiques sur les individus",
                                            width = NULL,
                                            status = "primary",
                                            "Base individu non chargée"
                                          ),
                                          box(title = "Graphique univarié de la base individu",
                                              width = NULL, status = "info",
                                              ""),
                                          box(title = "Graphique bivarié de la base individu",
                                              width = NULL, status = "success",
                                              "")
                                        )
                                 )
                               )
                             )
                    )
                    ,
                    tabPanel("Tableaux", icon = icon("table"),
                             fluidRow(
                               # Colonne Ménage
                               column(6,
                                      box(
                                        title = "Base Ménage - Tableau Univarié",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        selectInput("vars_tab_univ_menage_1", "Choisissez la variable (Ménage) :", choices = NULL),
                                        gt_output("tab_univ_menage")
                                      ),
                                      box(
                                        title = "Base Ménage - Tableau Bivarié",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        selectInput("vars_tab_bi_menage_1", "Choisissez la 1ère variable (Ménage) :", choices = NULL),
                                        selectInput("vars_tab_bi_menage_2", "Choisissez la 2ème variable (Ménage) :", choices = NULL),
                                        gt_output("tab_bi_menage")
                                      )
                               ),
                               
                               # Colonne Individu
                               column(6,
                                      box(
                                        title = "Base Individu - Tableau Univarié",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        selectInput("vars_tab_univ_individu_1", "Choisissez la variable (Individu) :", choices = NULL),
                                        gt_output("tab_univ_individu")
                                      ),
                                      box(
                                        title = "Base Individu - Tableau Bivarié",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        selectInput("vars_tab_bi_individu_1", "Choisissez la 1ère variable (Individu) :", choices = NULL),
                                        selectInput("vars_tab_bi_individu_2", "Choisissez la 2ème variable (Individu) :", choices = NULL),
                                        gt_output("tab_bi_individu")
                                      )
                               )
                             )
                    )
                    
                    
                    
                  ))   ) )
  )
)


# SERVER
server <- function(input, output, session) {
  mod_map_page_server("map_page")  
  values <- reactiveValues(
    base_menage = NULL,
    base_individu = NULL,
    raw_menage = NULL,
    raw_individu = NULL
  )
  options(shiny.maxRequestSize = 100*1024^2)  # Augmenter à 100MB par exemple
  
  # Chargement des fichiers 
  observeEvent(input$file_menage, {
    req(input$file_menage)
    ext <- tools::file_ext(input$file_menage$name)
    
    if (ext == "csv") {
      data <- read.csv(input$file_menage$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      data <- read_excel(input$file_menage$datapath)
    } else if (ext == "rds") {
      data <- readRDS(input$file_menage$datapath)
    } else if (ext == "Rdata") {
      e <- new.env()
      load(input$file_menage$datapath, envir = e)
      dfs <- ls(e)[sapply(ls(e, e), function(x) is.data.frame(get(x, e)))]
      data <- if (length(dfs) > 0) get(dfs[1], e) else NULL
    } else if (ext == "dta") {
      # Chargement des fichiers Stata (.dta)
      data <- haven::read_dta(input$file_menage$datapath)
      # Convertir les variables "labelled" en facteurs
      data <- as.data.frame(lapply(data, haven::as_factor))
    }
    
    values$raw_menage <- data
    values$base_menage <- data
  })
  
  # Chargement des fichiers
  observeEvent(input$file_individu, {
    req(input$file_individu)
    ext <- tools::file_ext(input$file_individu$name)
    
    if (ext == "csv") {
      data <- read.csv(input$file_individu$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      data <- read_excel(input$file_individu$datapath)
    } else if (ext == "rds") {
      data <- readRDS(input$file_individu$datapath)
    } else if (ext == "Rdata") {
      e <- new.env()
      load(input$file_individu$datapath, envir = e)
      dfs <- ls(e)[sapply(ls(e, e), function(x) is.data.frame(get(x, e)))]
      data <- if (length(dfs) > 0) get(dfs[1], e) else NULL
    } else if (ext == "dta") {
      # Chargement des fichiers 
      data <- haven::read_dta(input$file_individu$datapath)
      # Convertir les variables "labelled" en facteurs
      data <- as.data.frame(lapply(data, haven::as_factor))
    }
    
    values$raw_individu <- data
    values$base_individu <- data
  })
  
  
  
  # Mise à jour des choix pour la page Traitement
  observe({
    if (!is.null(values$base_menage)) {
      updateSelectInput(session, "var_na_menage", choices = names(values$base_menage))
      updateSelectInput(session, "var_drop_menage", choices = names(values$base_menage))
      updateSelectInput(session, "rename_var_menage", choices = names(values$base_menage))
      updateSelectInput(session, "modalities_var_menage", choices = names(values$base_menage))
    }
    if (!is.null(values$base_individu)) {
      updateSelectInput(session, "var_na_individu", choices = names(values$base_individu))
      updateSelectInput(session, "var_drop_individu", choices = names(values$base_individu))
      updateSelectInput(session, "rename_var_individu", choices = names(values$base_individu))
      updateSelectInput(session, "modalities_var_individu", choices = names(values$base_individu))
    }
  })
  
  # Fusion des bases MENAGE et INDIVIDU pour créer base_finale
  observeEvent(input$merge_bases, {
    req(values$base_menage, values$base_individu)
    
    if (!("hhid" %in% names(values$base_menage)) || !("hhid" %in% names(values$base_individu))) {
      showNotification("La variable 'hhid' est manquante dans l'une des bases.", type = "error")
      return()
    }
    
    # Vérification des doublons dans la base ménage
    duplicated_hhids <- values$base_menage$hhid[duplicated(values$base_menage$hhid)]
    if (length(duplicated_hhids) > 0) {
      showNotification("Erreur : La base Ménage contient des doublons sur 'hhid'. Veuillez corriger avant la fusion.", type = "error", duration = NULL)
      return()
    }
    
    # Fusion des bases si aucun doublon détecté
    values$base_finale <- merge(
      values$base_menage, values$base_individu,
      by = "hhid", all = TRUE, suffixes = c("_menage", "_individu")
    )
    showNotification("Fusion réalisée avec succès.", type = "message")
  })
  
  
  # Télécharger la base_finale
  output$download_finale <- downloadHandler(
    filename = function() {
      paste("base_finale_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$base_finale)
      write.csv(values$base_finale, file, row.names = FALSE)
    }
  )
  
  
  # Traitements doublons
  observeEvent(input$remove_duplicates_menage, {
    req(values$base_menage)
    if ("hhid" %in% names(values$base_menage)) {
      values$base_menage <- values$base_menage[!duplicated(values$base_menage$hhid), ]
    } else {
      showNotification("La variable 'hhid' est manquante dans la base MENAGE.", type = "error")
    }
  })
  
  # Suppressions des valeurs manquantes
  observeEvent(input$remove_na_menage, {
    req(values$base_menage, input$var_na_menage)
    values$base_menage <- values$base_menage[!is.na(values$base_menage[[input$var_na_menage]]), ]
  })
  
  # Traitement des valeurs manquantes
  observeEvent(input$mean_na_menage, {
    req(values$base_menage, input$var_na_menage)
    var <- input$var_na_menage
    if (is.numeric(values$base_menage[[var]])) {
      moy <- mean(values$base_menage[[var]], na.rm = TRUE)
      values$base_menage[[var]][is.na(values$base_menage[[var]])] <- moy
    } else {
      showNotification("La variable sélectionnée n'est pas numérique.", type = "error")
    }
  })
  
  # Suppression des variables 
  observeEvent(input$drop_var_menage, {
    req(values$base_menage, input$var_drop_menage)
    values$base_menage[[input$var_drop_menage]] <- NULL
  })
  
  # Traitements des doublons de la base individu
  observeEvent(input$remove_duplicates_individu, {
    req(values$base_individu)
    if (all(c("hhid", "numind") %in% names(values$base_individu))) {
      values$base_individu <- values$base_individu[!duplicated(values$base_individu[c("hhid", "numind")]), ]
    } else {
      showNotification("Les variables 'hhid' et/ou 'numind' sont manquantes dans la base INDIVIDU.", type = "error")
    }
  })
  
  # Pourcentage de NA
  output$na_info_menage <- renderPrint({
    req(values$base_menage, input$var_na_menage)
    var <- input$var_na_menage
    total <- nrow(values$base_menage)
    nb_na <- sum(is.na(values$base_menage[[var]]))
    pourcentage <- round(100 * nb_na / total, 2)
    cat("Nombre de NA :", nb_na, "\nPourcentage de NA :", pourcentage, "%")
  })
  
  output$na_info_individu <- renderPrint({
    req(values$base_individu, input$var_na_individu)
    var <- input$var_na_individu
    total <- nrow(values$base_individu)
    nb_na <- sum(is.na(values$base_individu[[var]]))
    pourcentage <- round(100 * nb_na / total, 2)
    cat("Nombre de NA :", nb_na, "\nPourcentage de NA :", pourcentage, "%")
  })
  
  
  # Suppressions des valeurs manquantes
  
  observeEvent(input$remove_na_individu, {
    req(values$base_individu, input$var_na_individu)
    values$base_individu <- values$base_individu[!is.na(values$base_individu[[input$var_na_individu]]), ]
  })
  
  #Traitement des valeurs manquantes 
  observeEvent(input$mean_na_individu, {
    req(values$base_individu, input$var_na_individu)
    var <- input$var_na_individu
    if (is.numeric(values$base_individu[[var]])) {
      moy <- mean(values$base_individu[[var]], na.rm = TRUE)
      values$base_individu[[var]][is.na(values$base_individu[[var]])] <- moy
    } else {
      showNotification("La variable sélectionnée n'est pas numérique.", type = "error")
    }
  })
  
  # Imputation par la médiane
  observeEvent(input$median_na_menage, {
    req(values$base_menage, input$var_na_menage)
    var <- input$var_na_menage
    if (is.numeric(values$base_menage[[var]])) {
      med <- median(values$base_menage[[var]], na.rm = TRUE)
      values$base_menage[[var]][is.na(values$base_menage[[var]])] <- med
    } else {
      showNotification("La variable sélectionnée n'est pas numérique.", type = "error")
    }
  })
  
  observeEvent(input$median_na_individu, {
    req(values$base_individu, input$var_na_individu)
    var <- input$var_na_individu
    if (is.numeric(values$base_individu[[var]])) {
      med <- median(values$base_individu[[var]], na.rm = TRUE)
      values$base_individu[[var]][is.na(values$base_individu[[var]])] <- med
    } else {
      showNotification("La variable sélectionnée n'est pas numérique.", type = "error")
    }
  })
  
  
  # Suppression des variables
  observeEvent(input$drop_var_individu, {
    req(values$base_individu, input$var_drop_individu)
    values$base_individu[[input$var_drop_individu]] <- NULL
  })
  
  observeEvent(input$rename_var_menage_btn, {
    req(values$base_menage, input$rename_var_menage, input$new_name_menage)
    old_name <- input$rename_var_menage
    new_name <- input$new_name_menage
    if (new_name != "" && old_name %in% names(values$base_menage)) {
      names(values$base_menage)[names(values$base_menage) == old_name] <- new_name
      updateSelectInput(session, "var_na_menage", choices = names(values$base_menage))
      updateSelectInput(session, "var_drop_menage", choices = names(values$base_menage))
      updateSelectInput(session, "rename_var_menage", choices = names(values$base_menage))
      updateSelectInput(session, "modalities_var_menage", choices = names(values$base_menage))
      showNotification(paste("Variable", old_name, "renommée en", new_name), type = "message")
    } else {
      showNotification("Veuillez fournir un nouveau nom valide.", type = "error")
    }
  })
  
  # Révéler les modalités pour MENAGE (toujours afficher les modalités uniques et, s'il y a des labels, les afficher également)
  observeEvent(input$reveal_modalities_menage, {
    req(values$base_menage, input$modalities_var_menage)
    var <- input$modalities_var_menage
    vec <- values$base_menage[[var]]
    uni_vals <- sort(unique(vec))
    res <- "Modalités:\n"
    res <- paste0(res, paste0(uni_vals, collapse = ", "), "\n")
    labs <- attr(vec, "labels")
    if(!is.null(labs)){
      res <- paste0(res, "Labels:\n")
      for(i in seq_along(labs)){
        res <- paste0(res, names(labs)[i], " correspond à ", labs[i], "\n")
      }
    }
    output$modalities_menage_out <- renderPrint({ res })
  })
  
  
  observeEvent(input$rename_var_individu_btn, {
    req(values$base_individu, input$rename_var_individu, input$new_name_individu)
    old_name <- input$rename_var_individu
    new_name <- input$new_name_individu
    if (new_name != "" && old_name %in% names(values$base_individu)) {
      names(values$base_individu)[names(values$base_individu) == old_name] <- new_name
      updateSelectInput(session, "var_na_individu", choices = names(values$base_individu))
      updateSelectInput(session, "var_drop_individu", choices = names(values$base_individu))
      updateSelectInput(session, "rename_var_individu", choices = names(values$base_individu))
      updateSelectInput(session, "modalities_var_individu", choices = names(values$base_individu))
      showNotification(paste("Variable", old_name, "renommée en", new_name), type = "message")
    } else {
      showNotification("Veuillez fournir un nouveau nom valide.", type = "error")
    }
  })
  
  # Révéler les modalités pour INDIVIDU
  observeEvent(input$reveal_modalities_individu, {
    req(values$base_individu, input$modalities_var_individu)
    var <- input$modalities_var_individu
    vec <- values$base_individu[[var]]
    uni_vals <- sort(unique(vec))
    res <- "Modalités:\n"
    res <- paste0(res, paste0(uni_vals, collapse = ", "), "\n")
    labs <- attr(vec, "labels")
    if(!is.null(labs)){
      res <- paste0(res, "Labels:\n")
      for(i in seq_along(labs)){
        res <- paste0(res, names(labs)[i], " correspond à ", labs[i], "\n")
      }
    }
    output$modalities_individu_out <- renderPrint({ res })
  })
  
  
  # Aperçus bruts
  output$raw_preview_menage <- renderDT({
    req(values$raw_menage)
    datatable(head(values$raw_menage, 10), options = list(scrollX = TRUE))
  })
  
  output$raw_preview_individu <- renderDT({
    req(values$raw_individu)
    datatable(head(values$raw_individu, 10), options = list(scrollX = TRUE))
  })
  
  # Aperçus après traitement
  output$preview_menage <- renderDT({
    req(values$base_menage)
    datatable(head(values$base_menage, 10), options = list(scrollX = TRUE))
  })
  
  output$preview_individu <- renderDT({
    req(values$base_individu)
    datatable(head(values$base_individu, 10), options = list(scrollX = TRUE))
  })
  
  #################################
  
  # ValueBoxes
  output$box_menage_nrow <- renderValueBox({
    req(values$base_menage)
    valueBox(format(nrow(values$base_menage), big.mark = ","), "Obs. menage", icon = icon("users"), color = "green")
  })
  
  output$box_individu_nrow <- renderValueBox({
    req(values$base_individu)
    valueBox(format(nrow(values$base_individu), big.mark = ","), "Obs. individu", icon = icon("user"), color = "orange")
  })
  
  # Nombre de variables pour MENAGE
  output$box_menage_nvar <- renderValueBox({
    req(values$base_menage)
    n_vars_menage <- length(names(values$base_menage))
    valueBox(n_vars_menage, "Variables menage", icon = icon("list"), color = "green")
  })
  
  # Nombre de variables pour INDIVIDU
  output$box_individu_nvar <- renderValueBox({
    req(values$base_individu)
    n_vars_individu <- length(names(values$base_individu))
    valueBox(n_vars_individu, "Variables individu", icon = icon("list"), color = "orange")
  })
  
  
  # Update selectInputs

# Dans la partie serveur, modifiez les blocs observe qui mettent à jour les sélecteurs

# Pour la base ménage
observe({
  req(values$base_menage)
  current_selection <- input$menage_var1  # Conserve la sélection actuelle
  updateSelectInput(session, "menage_var1", 
                    choices = names(values$base_menage),
                    selected = current_selection)
  
  # Faites de même pour tous les autres selectInput de la base ménage
  current_selection <- input$menage_var2
  updateSelectInput(session, "menage_var2", 
                    choices = names(values$base_menage),
                    selected = current_selection)
  
  current_selection <- input$menage_var3
  updateSelectInput(session, "menage_var3", 
                    choices = names(values$base_menage),
                    selected = current_selection)
  
  current_selection <- input$menage_var4
  updateSelectInput(session, "menage_var4", 
                    choices = names(values$base_menage),
                    selected = current_selection)
})

# Pour la base individu
observe({
  req(values$base_individu)
  current_selection <- input$individu_var1
  updateSelectInput(session, "individu_var1", 
                    choices = names(values$base_individu),
                    selected = current_selection)
  
  # Faites de même pour tous les autres selectInput de la base individu
  current_selection <- input$individu_var2
  updateSelectInput(session, "individu_var2", 
                    choices = names(values$base_individu),
                    selected = current_selection)
  
  current_selection <- input$individu_var3
  updateSelectInput(session, "individu_var3", 
                    choices = names(values$base_individu),
                    selected = current_selection)
  
  current_selection <- input$individu_var4
  updateSelectInput(session, "individu_var4", 
                    choices = names(values$base_individu),
                    selected = current_selection)
})
  
  
  # Résumé statistique pour la base Ménage
  output$summary_menage <- renderPrint({
    req(values$base_menage, input$menage_var1)
    
    var <- input$menage_var1
    
    # Vérifier si la variable est déjà numérique
    if (!is.numeric(values$base_menage[[var]])) {
      # Si la variable n'est pas numérique, tenter de la convertir
      values$base_menage[[var]] <- as.numeric(values$base_menage[[var]])
      
      # Vérifier après conversion si c'est bien numérique
      if (any(is.na(values$base_menage[[var]]))) {
        print("La conversion a échoué, des valeurs manquantes ont été générées.")
        return(NULL)
      }
    }
    
    # Afficher les statistiques si la conversion a réussi
    stats <- summary(values$base_menage[[var]])
    stats <- c(stats, 
               Q1 = quantile(values$base_menage[[var]], 0.25, na.rm = TRUE),
               Q3 = quantile(values$base_menage[[var]], 0.75, na.rm = TRUE))
    print(stats)
  })
  
  # Résumé statistique pour la base Individu
  output$summary_individu <- renderPrint({
    req(values$base_individu, input$individu_var1)
    
    var <- input$individu_var1
    
    # Vérifier si la variable est déjà numérique
    if (!is.numeric(values$base_individu[[var]])) {
      # Si la variable n'est pas numérique, tenter de la convertir
      values$base_individu[[var]] <- as.numeric(values$base_individu[[var]])
      
      # Vérifier après conversion si c'est bien numérique
      if (any(is.na(values$base_individu[[var]]))) {
        print("La conversion a échoué, des valeurs manquantes ont été générées.")
        return(NULL)
      }
    }
    
    # Afficher les statistiques si la conversion a réussi
    stats <- summary(values$base_individu[[var]])
    stats <- c(stats, 
               Q1 = quantile(values$base_individu[[var]], 0.25, na.rm = TRUE),
               Q3 = quantile(values$base_individu[[var]], 0.75, na.rm = TRUE))
    print(stats)
  })
  
  
  # Fonction pour convertir une variable en numérique si possible
  convert_to_numeric <- function(data, var_name) {
    if (!is.numeric(data[[var_name]])) {
      # Essayer de convertir en numérique
      converted <- tryCatch({
        as.numeric(as.character(data[[var_name]]))
      }, warning = function(w) {
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(converted)) {
        data[[var_name]] <- converted
        return(data[[var_name]])
      } else {
        return(NULL)
      }
    } else {
      return(data[[var_name]])
    }
  }
  
  # Graphique univarié pour la base Ménage
  output$plot_menage_uni <- renderPlotly({
    req(values$base_menage, input$menage_var2, input$menage_plot_type_uni)
    
    data <- values$base_menage
    var <- input$menage_var2
    plot_type <- input$menage_plot_type_uni
    
    # Conversion pour boxplot
    if (plot_type == "Boxplot") {
      numeric_var <- convert_to_numeric(data, var)
      if (is.null(numeric_var)) {
        showNotification("La variable sélectionnée ne peut pas être convertie en numérique pour le boxplot.", type = "error")
        return(NULL)
      }
      data[[var]] <- numeric_var
    }
    
    p <- switch(plot_type,
                "Barres" = ggplot(data, aes_string(x = var)) + geom_bar(stat = "count", fill = "skyblue"),
                "Boxplot" = ggplot(data, aes_string(y = var)) + geom_boxplot(fill = "skyblue")
    )
    
    p <- p + theme_minimal() + labs(x = var, y = "Fréquence")
    ggplotly(p)
  })
  
  # Graphique univarié pour la base Individu
  output$plot_individu_uni <- renderPlotly({
    req(values$base_individu, input$individu_var2, input$individu_plot_type_uni)
    
    data <- values$base_individu
    var <- input$individu_var2
    plot_type <- input$individu_plot_type_uni
    
    # Conversion pour boxplot
    if (plot_type == "Boxplot") {
      numeric_var <- convert_to_numeric(data, var)
      if (is.null(numeric_var)) {
        showNotification("La variable sélectionnée ne peut pas être convertie en numérique pour le boxplot.", type = "error")
        return(NULL)
      }
      data[[var]] <- numeric_var
    }
    
    p <- switch(plot_type,
                "Barres" = ggplot(data, aes_string(x = var)) + geom_bar(stat = "count", fill = "skyblue"),
                "Boxplot" = ggplot(data, aes_string(y = var)) + geom_boxplot(fill = "skyblue")
    )
    
    p <- p + theme_minimal() + labs(x = var, y = "Fréquence")
    ggplotly(p)
  })
  
  # Graphique bivarié pour la base Ménage
  output$plot_menage_bi <- renderPlotly({
    req(values$base_menage, input$menage_var3, input$menage_var4, input$menage_plot_type_bi)
    
    data <- values$base_menage
    var_x <- input$menage_var3
    var_y <- input$menage_var4
    plot_type <- input$menage_plot_type_bi
    
    # Conversion pour boxplot
    if (plot_type == "Boxplot") {
      numeric_var_y <- convert_to_numeric(data, var_y)
      if (is.null(numeric_var_y)) {
        showNotification("La variable Y ne peut pas être convertie en numérique pour le boxplot.", type = "error")
        return(NULL)
      }
      data[[var_y]] <- numeric_var_y
    }
    
    # Nettoyer les données
    data_clean <- data[!is.na(data[[var_x]]) & !is.na(data[[var_y]]) & 
                         !is.infinite(data[[var_x]]) & !is.infinite(data[[var_y]]), ]
    
    p <- switch(plot_type,
                "Barres" = ggplot(data_clean, aes_string(x = var_x, fill = var_y)) + 
                  geom_bar(position = "dodge"),
                "Boxplot" = ggplot(data_clean, aes_string(x = var_x, y = var_y)) + 
                  geom_boxplot()
    )
    
    p <- p + labs(x = var_x, y = var_y) + theme_minimal()
    ggplotly(p)
  })
  
  # Graphique bivarié pour la base Individu
  output$plot_individu_bi <- renderPlotly({
    req(values$base_individu, input$individu_var3, input$individu_var4, input$individu_plot_type_bi)
    
    data <- values$base_individu
    var_x <- input$individu_var3
    var_y <- input$individu_var4
    plot_type <- input$individu_plot_type_bi
    
    # Conversion pour boxplot
    if (plot_type == "Boxplot") {
      numeric_var_y <- convert_to_numeric(data, var_y)
      if (is.null(numeric_var_y)) {
        showNotification("La variable Y ne peut pas être convertie en numérique pour le boxplot.", type = "error")
        return(NULL)
      }
      data[[var_y]] <- numeric_var_y
    }
    
    # Nettoyer les données
    data_clean <- data[!is.na(data[[var_x]]) & !is.na(data[[var_y]]) & 
                         !is.infinite(data[[var_x]]) & !is.infinite(data[[var_y]]), ]
    
    p <- switch(plot_type,
                "Barres" = ggplot(data_clean, aes_string(x = var_x, fill = var_y)) + 
                  geom_bar(position = "dodge"),
                "Boxplot" = ggplot(data_clean, aes_string(x = var_x, y = var_y)) + 
                  geom_boxplot()
    )
    
    p <- p + labs(x = var_x, y = var_y) + theme_minimal()
    ggplotly(p)
  })
  
  observe({
    if (!is.null(values$base_menage)) {
      updateSelectInput(session, "vars_tab_univ_menage_1", choices = names(values$base_menage))
      updateSelectInput(session, "vars_tab_bi_menage_1", choices = names(values$base_menage))
      updateSelectInput(session, "vars_tab_bi_menage_2", choices = names(values$base_menage))
      updateSelectInput(session, "menage_var_univ", choices = names(values$base_menage))
      updateSelectInput(session, "menage_var_x_bi", choices = names(values$base_menage))
      updateSelectInput(session, "menage_var_y_bi", choices = names(values$base_menage))
    }
  })
  
  # Mise à jour des choix pour Individu
  observe({
    if (!is.null(values$base_individu)) {
      updateSelectInput(session, "vars_tab_univ_individu_1", choices = names(values$base_individu))
      updateSelectInput(session, "vars_tab_bi_individu_1", choices = names(values$base_individu))
      updateSelectInput(session, "vars_tab_bi_individu_2", choices = names(values$base_individu))
      updateSelectInput(session, "individu_var_univ", choices = names(values$base_individu))
      updateSelectInput(session, "individu_var_x_bi", choices = names(values$base_individu))
      updateSelectInput(session, "individu_var_y_bi", choices = names(values$base_individu))
    }
  })
  
  # Tableau croisé
  
  # Tableau univarié pour base Ménage
  output$tab_univ_menage <- render_gt({
    req(input$vars_tab_univ_menage_1)
    
    # Récupérer la base Ménage
    base_menage <- values$base_menage
    
    # Afficher le tableau univarié pour la base Ménage
    base_univariate_menage <- base_menage[, input$vars_tab_univ_menage_1, drop = FALSE]
    tbl_summary(base_univariate_menage, missing = "no") %>%
      bold_labels() %>%
      italicize_levels() %>%
      as_gt()
  })
  
  # Tableau bivarié pour base Ménage
  output$tab_bi_menage <- render_gt({
    req(input$vars_tab_bi_menage_1, input$vars_tab_bi_menage_2)
    
    # Récupérer la base Ménage
    base_menage <- values$base_menage
    
    # Afficher le tableau bivarié pour la base Ménage
    base_bivariate_menage <- base_menage[, c(input$vars_tab_bi_menage_1, input$vars_tab_bi_menage_2), drop = FALSE]
    tbl_summary(base_bivariate_menage, by = all_of(input$vars_tab_bi_menage_1), missing = "no") %>%
      bold_labels() %>%
      italicize_levels() %>%
      as_gt()
  })
  
  # Tableau univarié pour base Individu
  output$tab_univ_individu <- render_gt({
    req(input$vars_tab_univ_individu_1)
    
    # Récupérer la base Individu
    base_individu <- values$base_individu
    
    # Afficher le tableau univarié pour la base Individu
    base_univariate_individu <- base_individu[, input$vars_tab_univ_individu_1, drop = FALSE]
    tbl_summary(base_univariate_individu, missing = "no") %>%
      bold_labels() %>%
      italicize_levels() %>%
      as_gt()
  })
  
  # Tableau bivarié pour base Individu
  output$tab_bi_individu <- render_gt({
    req(input$vars_tab_bi_individu_1, input$vars_tab_bi_individu_2)
    
    # Récupérer la base Individu
    base_individu <- values$base_individu
    
    # Afficher le tableau bivarié pour la base Individu
    base_bivariate_individu <- base_individu[, c(input$vars_tab_bi_individu_1, input$vars_tab_bi_individu_2), drop = FALSE]
    tbl_summary(base_bivariate_individu, by = all_of(input$vars_tab_bi_individu_1), missing = "no") %>%
      bold_labels() %>%
      italicize_levels() %>%
      as_gt()
  })
  
  
  
  # Si une seule base est chargé
  
  output$base_menage_loaded <- reactive({
    !is.null(values$base_menage)
  })
  
  outputOptions(output, "base_menage_loaded", suspendWhenHidden = FALSE)
  
  output$base_individu_loaded <- reactive({
    !is.null(values$base_individu)
  })
  
  outputOptions(output, "base_individu_loaded", suspendWhenHidden = FALSE)
  
}

# Run the app
shinyApp(ui, server)
