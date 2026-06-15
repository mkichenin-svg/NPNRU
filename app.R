# Configurer le miroir CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Installer les dépendances GitHub si nécessaire
if (!requireNamespace("shinymanager", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cran.rstudio.com/")
  }
  remotes::install_github("datastorm-open/shinymanager", upgrade = "never")
}

library(shiny)
library(bslib)
library(ggplot2)
library(sf)
library(tibble)
library(bsicons)
library(shinymanager)

# Charger les données précompilées (TRÈS rapide)
# Ces données ont été générées une fois avec load_data.R
if (file.exists("app_data.Rdata")) {
  load("app_data.Rdata", envir = .GlobalEnv)
} else {
  # Fallback : charger depuis les fichiers bruts si app_data.Rdata n'existe pas
  # (moins performant mais plus robuste)
  
  read_csv2_utf8 <- function(file) {
    con <- file(file, "r", encoding = "UTF-8")
    on.exit(close(con))
    df <- read.csv2(con, stringsAsFactors = FALSE)
    lines <- readLines(file, n = 1, encoding = "UTF-8")
    lines <- gsub("^\xef\xbb\xbf", "", lines)
    col_names <- strsplit(lines, ";")[[1]]
    colnames(df) <- col_names
    df
  }

  shapefile  <- read_sf("QPV/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")
  shapefile1 <- read_sf("communes/communesPolygon.shp")
  shapefile2 <- read_sf("QPV2/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")

  iris  <- read_sf("iris/georef-france-iris-millesime.shp")
  iris2 <- read_sf("iris2/georef-france-iris-millesime.shp")

  genre <- read.csv2("genre.csv")
  age <- read.csv2("age.csv")
  quali_le_port <- read.csv2("quali_le_port.csv", fileEncoding = "UTF-8-BOM")

  heures_saint_denis                  <- read_csv2_utf8("heures_saint_denis.csv")
  participants_saint_benoit           <- read_csv2_utf8("participants_saint_benoit.csv")
  heure_echelle_saint_benoit          <- read_csv2_utf8("heure_echelle_saint_benoit.csv")
  heure_ANRU_saint_benoit             <- read_csv2_utf8("heure_ANRU_saint_benoit.csv")
  heure_echelle_saint_andré           <- read_csv2_utf8("heure_echelle_saint_andré.csv")
  heure_anru_saint_andré              <- read_csv2_utf8("heure_anru_saint_andré.csv")
  heure_conventionné_saint_pierre     <- read_csv2_utf8("heure_conventionné_saint_pierre.csv")
  heure_non_conventionné_saint_pierre <- read_csv2_utf8("heure_non_conventionné_saint_pierre.csv")
  heures_le_port                      <- read_csv2_utf8("heures_le_port.csv")
  heure_saint_louis1                  <- read_csv2_utf8("heure_saint_louis1.csv")
}



genre_le_port <- data.frame(
  genre       = c("Masculin", "Féminin"),
  nombre      = c(29, 1),
  pourcentage = c("96,7%", "3,3%")
)

age_le_port <- data.frame(
  age         = factor(c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans"),
                       levels = c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans")),
  nombre      = c(2, 4, 14, 10),
  pourcentage = c("7%", "13%", "47%", "33%")
)

contrat_le_port <- data.frame(
  contrat     = factor(c("Intérim", "Contrat de chantier", "CDD 6 mois"),
                       levels = c("Intérim", "Contrat de chantier", "CDD 6 mois")),
  nombre      = c(14, 15, 1),
  pourcentage = c("47%", "50%", "3%")
)

# Dataframes pour Saint-André
genre_saint_andre <- data.frame(
  genre       = c("Masculin"),
  nombre      = c(41),
  pourcentage = c("100%")
)

age_saint_andre <- data.frame(
  age         = factor(c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans"),
                       levels = c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans")),
  nombre      = c(5, 20, 10, 6),
  pourcentage = c("12,2%", "48,8%", "24,4%", "14,6%")
)

modalite_saint_andre <- data.frame(
  modalite    = factor(c("ETTI", "Embauche directe"),
                       levels = c("ETTI", "Embauche directe")),
  nombre      = c(33, 8),
  pourcentage = c("80,5%", "19,5%")
)

metier_saint_andre <- data.frame(
  metier      = factor(c("Autre / Métier non défini", "Entretien des espaces verts", "Manoeuvre et conduite d'engins lourds de manutention"),
                       levels = c("Autre / Métier non défini", "Entretien des espaces verts", "Manoeuvre et conduite d'engins lourds de manutention")),
  nombre      = c(7, 10, 24),
  pourcentage = c("17,1%", "24,4%", "58,5%")
)

# Dataframes pour Saint-Benoît
genre_saint_benoit <- data.frame(
  genre       = c("Masculin", "Féminin"),
  nombre      = c(35, 1),
  pourcentage = c("97,2%", "2,8%")
)

age_saint_benoit <- data.frame(
  age         = factor(c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans"),
                       levels = c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans")),
  nombre      = c(8, 18, 5, 5),
  pourcentage = c("22,2%", "50,0%", "13,9%", "13,9%")
)

modalite_saint_benoit <- data.frame(
  modalite    = factor(c("ETTI", "Embauche directe", "AI", "EI"),
                       levels = c("ETTI", "Embauche directe", "AI", "EI")),
  nombre      = c(34, 6, 1, 1),
  pourcentage = c("81,0%", "14,3%", "2,4%", "2,4%")
)

metier_saint_benoit <- data.frame(
  metier      = factor(c("Préparation du gros oeuvre et travaux publics", "Manoeuvre et conduite d'engins lourds", "Montage d'agencements", "Maçonnerie et béton", "Électricité et équipements", "Menuiseries et fermetures", "Engins de terrassement", "Études et autres"),
                       levels = c("Préparation du gros oeuvre et travaux publics", "Manoeuvre et conduite d'engins lourds", "Montage d'agencements", "Maçonnerie et béton", "Électricité et équipements", "Menuiseries et fermetures", "Engins de terrassement", "Études et autres")),
  nombre      = c(17, 6, 4, 4, 3, 4, 2, 2),
  pourcentage = c("40,5%", "14,3%", "9,5%", "9,5%", "7,1%", "9,5%", "4,8%", "4,8%")
)

ui <- page_fillable(
  
  passwordInput(
    "password", 
    "mot de passe",
    value = "mypassword1"
  ),
  
  
  
  theme = theme_bootswatch("minty"),
  
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      // Remove all title attributes that might contain 'toggle'
      document.querySelectorAll('[title*=\"toggle\" i], [title*=\"sidebar\" i]').forEach(function(el) {
        el.removeAttribute('title');
      });
    });
  ")),
  
  # Header navbar
  tags$nav(
    class = "navbar navbar-expand-lg navbar-light bg-light w-100",
    style = "border-bottom: 1px solid #dee2e6; position: relative; z-index: 100; padding: 10px 20px; display: flex; align-items: center; justify-content: center; min-height: 130px; gap: 0;",
    # Logos on the left with negative margin
    tags$div(
      style = "position: absolute; left: 10px; display: flex; align-items: center; gap: 10px; line-height: 0.9;",
      tags$img(src = "img/anru.png", style = "height: 150px; object-fit: contain; display: block;"),
      tags$img(src = "img/logo_pref.png", style = "height: 90px; object-fit: contain; display: block;"),
      tags$img(src = "img/logo_mden.png", style = "height: 50px; object-fit: contain; display: block;")
    ),
    # Title centered
    tags$div(
      style = "font-size: 23px; font-weight: bold; color: #2c3e50; line-height: 1; display: flex; align-items: center;",
      "OBSERVATOIRE ANRU"
    ),
    # Text on the right
    tags$div(
      style = "position: absolute; right: 20px; top: 50%; transform: translateY(-50%); text-align: right; font-size: 14px; color: #555; max-width: 300px; line-height: 1.4;",
      "Cette page présente un point d'étape sur la mise en œuvre du volet insertion des NPNRU de La Réunion."
    )
  ),

  # Tabbed content
  navset_card_tab(
    id = "main_tabs",
  
  # Page saint-denis
  
  nav_panel("Saint-Denis - Prunel",p(
    
    layout_column_wrap(width = 200,
    
    value_box( 
      title = "", 
      "74 615 heures à réaliser à l'échelle du projet", 
      showcase = bsicons::bs_icon("clock"),
      showcase_layout = "left center",
      theme = "primary",
      height = 100
    ) ,
    
  value_box( 
    title = "", 
    "59 264 heures réalisées", 
    showcase = bsicons::bs_icon("calendar2-check"),
    showcase_layout = "left center",
    theme = "primary",
    height = 100
  ) ,
  
  value_box( 
    title = "", 
    "126 bénéficiaires", 
    theme = "primary", 
    class = "border",
    showcase = bsicons::bs_icon("person-fill-check"),
    height = 100
  ) ,
  
  ),
    

  
    layout_column_wrap(
      
      navset_card_tab( 
      
      nav_panel("Présentation",layout_column_wrap( width = 0.5,
      plotOutput("map_saint_denis", width = 450, height = 500),
      div(style = "max-width: 280px; font-size: 0.785 rem; margin-left: 116px;white-space: pre-line;", textOutput("info_saint_denis")))),
      
  

      ),
      
      card(
        
        nav_panel(width=200,"Avancement des heures d'insertion",
                tableOutput("heures_saint_denis"),
                div(style = "font-size: 1.7rem; color: #666; margin-top: 15px; font-style: italic;", 
                    "* Il convient également de noter que la ville de Saint-Denis nous a informé que les opérations MOA concernant la SEDRE et la SODIAC sont terminées.")
                ),
      
      ),
     
    ),
  
  

 
)
),


## Page 2

nav_panel("Saint-Benoît - Rive Droite",p(layout_column_wrap(width = 200,
                                              
                                              value_box( 
                                                title = "", 
                                                "34 025 heures à réaliser à l'échelle du projet", 
                                                showcase = bsicons::bs_icon("clock"),
                                                showcase_layout = "left center",
                                                theme = "primary",
                                                height = 100
                                              ) ,
                                              
                                              value_box( 
                                                title = "", 
                                                "14 863 heures réalisées", 
                                                "44% des objectifs conventionnés ANRU / LBU", 
                                                showcase = bsicons::bs_icon("calendar2-check"),
                                                showcase_layout = "left center",
                                                theme = "primary",
                                                height = 100
                                              ) ,
                                              
                                              value_box( 
                                                title = "", 
                                                "36 bénéficiaires", 
                                                theme = "primary", 
                                                class = "border",
                                                showcase = bsicons::bs_icon("person-fill-check"),
                                                height = 100
                                              ) ,
                                              
                                              value_box( 
                                                title = "", 
                                                "12 bénéficiaires issus d'un QPV (33,3%)", 
                                                showcase = bsicons::bs_icon("buildings"),
                                                height = 50
                                              ) ,
                                              
),

layout_column_wrap(
  
  navset_card_tab(
    
    nav_panel("Présentation",
      layout_column_wrap(
      plotOutput("map", width = 500, height = 500),
      div(style = "max-width: 300px; font-size: 0.785 rem; margin-left: 110px; white-space: pre-line;", textOutput("info_saint_benoit")))),
    
    nav_panel("Suivi des heures à l'échelle du projet",
              tableOutput("heure_echelle_saint_benoit")),
    
    nav_panel("Suivi des heures conventionnées ANRU",
              tableOutput("heure_ANRU_saint_benoit")),

    
  ),
  
  
  card(

    navset_card_tab(
      
      nav_panel("Profil des bénéficiaires",
                card(
                  
                  value_box( 
                    title = "", 
                    "Bénéficiaires majoritairement masculins",
                    showcase = bsicons::bs_icon("arrow-90deg-right"),
                    height = 50,
                  ) ,
                  
                  plotOutput("genre_saint_benoit", height = 170, width = 700),
                  
                  value_box( 
                    title = "",
                    "72 % des bénéficiaires ont moins de 41 ans",
                    showcase = bsicons::bs_icon("arrow-90deg-right"),
                    showcase_layout = "left center",
                    height = 50
                  ) ,
                  
                  plotOutput("age_saint_benoit", height = 170, width = 700)
                )
      ),
      
      nav_panel("Modalités d'embauche des bénéficiaires",
                card(
                  
                  value_box("",
                            "Type d'embauche des bénéficiaires",
                            showcase = bsicons::bs_icon("arrow-90deg-right"),
                            showcase_layout = "left center",
                            height = 50),
                  plotOutput("modalite_saint_benoit", height = 180, width = 700),
                  
                  value_box("",
                            "Métiers des bénéficiaires",
                            showcase = bsicons::bs_icon("arrow-90deg-right"),
                            showcase_layout = "left center",
                            height = 50),
                  plotOutput("metier_saint_benoit", height = 280, width = 700)
                )
      ),
      
    ),
  ),
  
),

),
),

#page Saint-André - Centre ville

nav_panel("Saint-André - Centre Ville",
         
          p(layout_column_wrap(width = 200,
                              
                              value_box( 
                                title = "", 
                                "22 781 heures à réaliser à l'échelle du projet", 
                                showcase = bsicons::bs_icon("clock"),
                                showcase_layout = "left center",
                                theme = "primary",
                                height = 100
                              ) ,
                              
                              value_box( 
                                title = "", 
                                "30 058,5 heures réalisées", 
                                showcase = bsicons::bs_icon("calendar2-check"),
                                showcase_layout = "left center",
                                theme = "primary",
                                height = 100
                              ) ,
                              
                              value_box( 
                                title = "", 
                                "41 bénéficiaires", 
                                theme = "primary", 
                                class = "border",
                                showcase = bsicons::bs_icon("person-fill-check"),
                                height = 100
                              ) ,
                              
                              
         ),
         
         layout_column_wrap(
           navset_card_tab(
             
             nav_panel("Présentation",layout_column_wrap(
               plotOutput("map_saint_andré", width = 500, height = 500),
               div(style = "max-width: 300px; font-size: 0.785 rem; margin-left: 110px; white-space: pre-line;", textOutput("info_saint_andré")))),
            
             
             nav_panel("Suivi des heures conventionnées",
                       tableOutput("heure_ANRU_saint_andre")),
           ),
           
           
           
           card(
             
             navset_card_tab(
               
               nav_panel("Profil des bénéficiaires",
                         card(
                         
                          
                           value_box( 
                             title = "", 
                             "9 bénéficiaires issus d'un QPV (22%)", 
                             showcase = bsicons::bs_icon("buildings"),
                             height = 50
                           ) ,
                           
                           value_box("",
                                     "Des bénéficiaires en totalité masculins",
                                     showcase = bsicons::bs_icon("arrow-right"),
                                     showcase_layout = "left center",
                                     height = 100),
                           
                       
                           value_box(title = "",
                                     "61% des bénéficiaires ont moins de 41 ans",
                                     showcase = bsicons::bs_icon("arrow-90deg-right"),
                                     height = 100),
                           
                           plotOutput("age_saint_andre", height = 150, width = 750),
                           
                           
                         )
               ),
           
               nav_panel("Modalités d'embauche des bénéficiaires",
                         card(
                           
                           value_box("",
                                     "Type d'embauche des bénéficiaires",
                                     showcase = bsicons::bs_icon("arrow-90deg-right"),
                                     showcase_layout = "left center",
                                     height = 70),
                           plotOutput("modalite_saint_andre", height = 150, width = 750),
                           
                           value_box("",
                                     "Métiers des bénéficiaires",
                                     showcase = bsicons::bs_icon("arrow-90deg-right"),
                                     showcase_layout = "left center",
                                     height = 70),
                           plotOutput("metier_saint_andre", height = 250, width = 750)
                         )
               ),
               
             ),
           ),
           
         ),
         
        
         
          )
),

           
#page Saint-Pierre


nav_panel("Saint-Pierre - Bois d'olives",
          
          p(layout_column_wrap(width = 200,
                               
                               value_box( 
                                 title = "", 
                                 "16 470 heures à réaliser à l'échelle du projet", 
                                 showcase = bsicons::bs_icon("clock"),
                                 showcase_layout = "left center",
                                 theme = "primary",
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "3448,7 heures réalisées", 
                                 showcase = bsicons::bs_icon("calendar2-check"),
                                 showcase_layout = "left center",
                                 theme = "primary",
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "8 bénéficiaires", 
                                 theme = "primary", 
                                 class = "border",
                                 showcase = bsicons::bs_icon("person-fill-check"),
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "5 bénéficiaires issus d'un QPV (62,5%)", 
                                 showcase = bsicons::bs_icon("buildings"),
                                 height = 50
                               ) ,
                               
          ),
          
          layout_column_wrap(
            
            navset_card_tab(
              
              nav_panel("Présentation",
                        layout_column_wrap(
                plotOutput("map_saint_pierre", width = 400, height = 500),
                div(style = "max-width: 300px; font-size: 0.785 rem; margin-left: 110px; white-space: pre-line;", textOutput("info_saint_pierre")))),
              
            
            ),
            
            
            card(
              
              navset_card_tab(
                
                nav_panel("Suivi des heure conventionnés",
                          tableOutput("heure_conventionne_saint_pierre")),
             
                
                nav_panel("Suivi des heures non conventionnées",
                          tableOutput("heure_non_conventionne_saint_pierre")),
                
               
              ),
             
              
        
            ),
            
          ),
          
          
        
          
          ),
),

#page le Port

nav_panel("Le Port - Ariste Bolon",
          
          p(layout_column_wrap(width = 200,
                               
                               value_box( 
                                 title = "", 
                                 "67 054 heures à réaliser à l'échelle du projet", 
                                 showcase = bsicons::bs_icon("clock"),
                                 showcase_layout = "left center",
                                 theme = "primary",
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "28 038,4 heures réalisées", 
                                 showcase = bsicons::bs_icon("calendar2-check"),
                                 showcase_layout = "left center",
                                 theme = "primary",
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "33 bénéficiaires", 
                                 theme = "primary", 
                                 class = "border",
                                 showcase = bsicons::bs_icon("person-fill-check"),
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "30 bénéficiaires issus d'un QPV (90%)",
                                 showcase = bsicons::bs_icon("buildings"),
                                 height = 50
                               ) ,
                               
          ),
          
          layout_column_wrap(
            
            navset_card_tab(
              
              nav_panel("Présentation",
                        layout_column_wrap(
                          plotOutput("map_le_port", width = 500, height = 500),
                          div(style = "max-width: 300px; font-size: 0.785 rem; margin-left: 125px; white-space: pre-line;", textOutput("info_le_port")))),
                  
              
              nav_panel("Suivi des heures d'insertion",
                        tableOutput("heures_le_port")),
              
            ),
            
            
            
            navset_card_tab(
              
              nav_panel("Profil des bénéficiaires",
                card(
                  value_box( 
                    title = "", 
                    "Bénéficiaires majoritairement masculins",
                    showcase = bsicons::bs_icon("arrow-90deg-right"),
                    height = 100,
                  ) ,
                  
                  plotOutput("genre_le_port",height = 150, width = 550),
                  
                  value_box( 
                    title = "",
                    "20% des bénéficiaires ont moins de 41 ans",
                    showcase = bsicons::bs_icon("arrow-90deg-right"),
                    showcase_layout = "left center",
                    height = 70
                  ) ,
                  
                  plotOutput("age_le_port", height = 150)
                )
              ),
              
              nav_panel("Modalités d'embauche des bénéficiaires",
                card(
                  value_box(
                    title = "",
                    "Type d'embauche des bénéficiaires",
                    showcase = bsicons::bs_icon("arrow-90deg-right"),
                    showcase_layout = "left center",
                    height = 70
                  ),

                  plotOutput("contrat_le_port", height = 150)
                )
              )
              
            ),
          ),
          
          
          
          
    ),
),


#page Saint- Louis

nav_panel("Saint-Louis - Le Gol",
          
          p(layout_column_wrap(width = 200,
                               
                               value_box( 
                                 title = "", 
                                 "41 292 heures à réaliser à l'échelle du projet", 
                                 showcase = bsicons::bs_icon("clock"),
                                 showcase_layout = "left center",
                                 theme = "primary",
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "5829 heures réalisées", 
                                 showcase = bsicons::bs_icon("calendar2-check"),
                                 showcase_layout = "left center",
                                 theme = "primary",
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "24 bénéficiaires", 
                                 theme = "primary", 
                                 class = "border",
                                 showcase = bsicons::bs_icon("person-fill-check"),
                                 height = 100
                               ) ,
                               
                               value_box( 
                                 title = "", 
                                 "14 bénéficiaires issus d'un QPV (58%)", 
                                 showcase = bsicons::bs_icon("buildings"),
                                 height = 50, width = 500,
                               ) ,
                               
                               
          ),
          
          layout_column_wrap(
          
            navset_card_tab(
              
              nav_panel("Présentation",
                        layout_column_wrap(
                          plotOutput("map_saint_louis", width = 500, height = 500),
                          div(style = "max-width: 300px; font-size: 0.785 rem; margin-left: 110px; white-space: pre-line;", textOutput("info_saint_louis")))),
              
            
            ),
            
            
            
            
            
            card(
              
              
              navset_card_tab(
                
               
                nav_panel("Suivi des heures d'insertion à l'échelle du projet",
                          tableOutput("heure_saint_louis1")),
                
              ),
              
            
              
            ),
            
          ),
          
          
         
          
          ),
),





  ) # fin navset_card_tab
) # fin page_fillable
  


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$value <- renderText({ input$password })
  
  genre_pie <- function(df) {
    ggplot(df, aes(x="", y= nombre, fill= genre)) + geom_col(color="black") +
      coord_polar("y", start = 2.5) +
      theme_void() + 
      theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=17)) +
      geom_label(aes(label = pourcentage), position= position_stack(vjust = 0.7),size = 5.5,
                 show.legend = FALSE) + scale_fill_manual(values = c ("royalblue", "orange"))
  }
  
  age_bar <- function(df) {
    ggplot(df, aes(x= age, y = nombre, fill = age)) + geom_bar(stat = "identity", show.legend = FALSE) + theme_minimal() + 
      geom_text(aes(label = pourcentage), hjust= 0.67,vjust= 1.5, color="black", size = 7) + 
      theme(axis.title = element_blank(), axis.text.y  = element_blank(), axis.text.x = element_text(size = 17)) +
      scale_fill_manual(values = c ("grey", "orange","royalblue", "orange"))
  }

  # Graphes supprimés de Saint-Denis
  # output$genre_saint_denis <- renderPlot({ genre_pie(genre) }, height = 210, width = 500)
  # output$age_saint_denis   <- renderPlot({ age_bar(age) })
  
  output$genre_saint_benoit <- renderPlot({ genre_pie(genre_saint_benoit) }, height = 170, width = 700)
  output$age_saint_benoit   <- renderPlot({ age_bar(age_saint_benoit) }, height = 170, width = 700)
  
  # carte
  
  output$map_saint_denis <- renderPlot({ 
    ggplot() +
      geom_sf(data= iris2[82,31])+
      geom_sf(data= iris2[102,31])+
      geom_sf(data= iris2[108,31])+
      geom_sf(data= iris2[153,31])+
      geom_sf(data= iris2[208,31])+
      geom_sf(data= iris2[136,31])+
      geom_sf(data= shapefile2[16,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf(data= shapefile2[40,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf(data= shapefile2[8,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris2[102,31], label = "Maréchal Leclerc- Le Butor")+
      geom_sf_label(data= iris2[82,31], label = "Maréchal Leclerc-Le Petit Marché")+
      geom_sf_label(data= iris2[108,31], label = "Le Butor - Champ Fleuri")+
      geom_sf_label(data= iris2[153,31], label = "Vauban Bouvet")+
      geom_sf_label(data= iris2[208,31], label = "Bouvet-CGSS")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                              axis.title.x = element_blank(), axis.title.y = element_blank())
  }, height = 500, width = 500) 
  
  
  output$info_saint_denis <- renderText({"La convention pluriannuelle du projet de renouvellement urbain de la ville de Saint-Denis, PRU Nord Est Littoral (PRUNEL) couvrant les quartiers du Bas de M. Leclerc – Vauban – Butor a été signée le 6 novembre 2019. 
\nLa MDEN a signé 6 conventions de partenariat avec des donneurs d’ordre impliqués dans le projet PRUNEL, afin de contractualiser la mise en œuvre du volet insertion. 
\nLa CINOR, Saint-Denis, la SIDR, la SEDRE, la SODIAC et la SEMADER impliqués dans le projet PRUNEL bénéficient donc d’un accompagnement commun par la MDEN à l’achat socialement responsable ce qui facilite le suivi et l’élaboration des bilans.  
\n19 comités d’insertion, à raison d’un par trimestre, permettent un suivi régulier de la mise en œuvre du volet d’insertion du projet PRUNEL. 
\nSuperficie = 383 593 m²  "})
  
  output$heures_saint_denis <- renderTable(striped = TRUE,heures_saint_denis)
  output$echelle_saint_denis <- renderTable(striped = TRUE,echelle_saint_denis)
  output$MO_saint_denis <- renderTable(striped = TRUE,{head(MO_saint_denis)})
  
  output$map <- renderPlot({ 
      ggplot() +
      geom_sf(data = iris[1,31])+
      geom_sf_label(data= iris[1,31], label = "Beaulieu - Bras fusil - La Confiance")+
      geom_sf(data = iris[3,31])+
      geom_sf(data = iris[9,31])+
      geom_sf(data = iris[12,31])+
      geom_sf(data = iris[21,31])+
      geom_sf(data= shapefile2[36,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris[3,31], label = "Butor-Beaufond Le port")+
      geom_sf_label(data = iris[9,31], label = "Beaufond Distillerie")+
      geom_sf_label(data = iris[12,31], label = "Centre ville-Rive gauche")+
      geom_sf_label(data = iris[21,31], label = "Centre ville-Rive droite")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                                axis.title.x = element_blank(), axis.title.y = element_blank())
  
  },
  
  height = 500, width = 500)
  
  output$info_saint_benoit <- renderText({"La convention pluriannuelle du projet de renouvellement urbain de la ville de Saint-Benoît Rive droite a été signée le 10 mars 2020. Le 07 février 2025, un avenant à la convention a été signée.  
\nLa MDEN a signé 4 conventions de partenariat avec des donneurs d’ordre impliqués dans le projet Rive Droite, afin de contractualiser la mise en œuvre du volet insertion. 
La CIREST, la ville de Saint-Benoît, la SIDR et la SEMAC impliqués dans le projet Rive Droite bénéficie donc d’un accompagnement commun à l’achat socialement responsable par la MDEN ce qui facilite le suivi et l’élaboration des bilans. 
\nSuperficie : 1 656 391 m²"})
  
output$echelle_saint_benoit <- renderTable(striped = TRUE,{head(echelle_saint_benoit)})
output$echelle_LBU_saint_benoit <- renderTable(striped = TRUE,{head(echelle_LBU_saint_benoit)})
output$echelle_NPNRU_saint_benoit <- renderTable(striped = TRUE,{head(echelle_NPNRU_saint_benoit)})
output$heure_echelle_saint_benoit <- renderTable(striped = TRUE,{heure_echelle_saint_benoit})
output$heure_ANRU_saint_benoit <- renderTable(striped = TRUE,{heure_ANRU_saint_benoit})

# Graphiques qualitatifs pour Saint-Benoît
output$modalite_saint_benoit <- renderPlot({
  modalite_data <- modalite_saint_benoit
  modalite_data$modalite <- factor(modalite_data$modalite, 
                                   levels = c("EI", "AI", "Embauche directe", "ETTI"))
  
  ggplot(modalite_data, aes(y = modalite, x = nombre, fill = modalite)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    theme_minimal() +
    geom_text(aes(label = pourcentage), hjust = -0.1, vjust = 0.5, color = "black", size = 6) +
    theme(axis.title = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(size = 13)) +
    scale_fill_manual(values = c("coral", "steelblue", "orange", "royalblue")) +
    xlim(0, max(modalite_data$nombre) * 1.2)
}, height = 180, width = 700)

output$metier_saint_benoit <- renderPlot({
  metier_data <- metier_saint_benoit
  metier_data$metier <- factor(metier_data$metier, 
                               levels = c("Études et autres",
                                        "Engins de terrassement",
                                        "Menuiseries et fermetures",
                                        "Électricité et équipements",
                                        "Maçonnerie et béton",
                                        "Montage d'agencements",
                                        "Manoeuvre et conduite d'engins lourds",
                                        "Préparation du gros oeuvre et travaux publics"))
  
  ggplot(metier_data, aes(y = metier, x = nombre, fill = metier)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    theme_minimal() +
    geom_text(aes(label = pourcentage), hjust = 1, vjust = 0, color = "black", size = 6) +
    theme(axis.title = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12)) +
    scale_fill_manual(values = c("gold", "purple", "darkgreen", "coral", "steelblue", "grey", "orange", "royalblue"))
}, height = 280, width = 750)
  
  output$map_saint_andré <- renderPlot({ 
    ggplot() +
      geom_sf(data= iris2[162,31])+
      geom_sf(data= iris2[141,31])+
      geom_sf(data= iris2[342,31])+
      geom_sf(data= iris2[262,31])+
      geom_sf(data= shapefile2[15,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris2[162,31], label = "Centre Ville Mairie")+
      geom_sf_label(data= iris2[141,31], label = "Pont Minot")+
      geom_sf_label(data= iris2[342,31], label = "Gare - Lycée Sarda")+
      geom_sf_label(data= iris2[262,31], label = "Pont Auguste")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                              axis.title.x = element_blank(), axis.title.y = element_blank())
  },
  
  height = 500, width = 500)
  
output$info_saint_andré <- renderText({"La convention pluriannuelle du projet de renouvellement urbain de la ville de Saint-André centre-ville a été signée le 9 octobre 2019 (avenant n°1 signé le 1er février 2023). 
\nLa MDEN a signé 2 conventions de partenariat avec les donneurs d’ordre impliqués dans le projet Centre-Ville, afin de contractualiser la mise en œuvre du volet insertion.
La ville de Saint-André et la SIDR impliqués dans le projet Rive Droite bénéficie donc d’un accompagnement commun à l’achat socialement responsable par la MDEN ce qui facilite le suivi et l’élaboration des bilans. 
\nSuperficie = 758 114 m²"})

output$heure_echelle_saint_andré <- renderTable(striped = TRUE,{heure_echelle_saint_andré})
output$objectifs_saint_andre     <- renderTable(striped = TRUE,{objectifs_saint_andre})
output$heure_ANRU_saint_andre    <- renderTable(striped = TRUE,{heure_anru_saint_andré})

# Graphiques qualitatifs pour Saint-André
output$genre_saint_andre <- renderPlot({
  pie <- ggplot(genre_saint_andre, aes(x = "", y = nombre, fill = genre)) +
    geom_col(color = "black") +
    coord_polar("y", start = 2.5) +
    theme_void() +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.text = element_text(size = 17)) +
    geom_label(aes(label = pourcentage), position = position_stack(vjust = 0.7),
               size = 5.5, show.legend = FALSE) +
    scale_fill_manual(values = c("royalblue"))
  pie
}, height = 165, width = 500)

output$age_saint_andre <- renderPlot({
  ggplot(age_saint_andre, aes(x = age, y = nombre, fill = age)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    theme_minimal() +
    geom_text(aes(label = pourcentage), hjust = 0.67, vjust = 1.5, color = "black", size = 7) +
    theme(axis.title = element_blank(), axis.text.y = element_blank(), 
          axis.text.x = element_text(size = 20)) +
    scale_fill_manual(values = c("grey", "orange", "royalblue", "orange"))
}, height = 150, width = 750)

output$modalite_saint_andre <- renderPlot({
  ggplot(modalite_saint_andre, aes(x = modalite, y = nombre, fill = modalite)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    theme_minimal() +
    geom_text(aes(label = pourcentage), hjust = 0.5, vjust = 1.3, color = "black", size = 7) +
    theme(axis.title = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(size = 19)) +
    scale_fill_manual(values = c("royalblue", "orange"))
}, height = 150, width = 750)

output$metier_saint_andre <- renderPlot({
  metier_data <- metier_saint_andre
  metier_data$metier <- factor(metier_data$metier, levels = c("Autre / Métier non défini", "Entretien des espaces verts", "Manoeuvre et conduite d'engins lourds de manutention"))
  
  ggplot(metier_data, aes(y = metier, x = nombre, fill = metier)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    theme_minimal() +
    geom_text(aes(label = pourcentage), hjust = 1, vjust = 0.5, color = "black", size = 7) +
    theme(axis.title = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(size = 14)) +
    scale_fill_manual(values = c("grey", "orange", "royalblue"))
}, height = 250, width = 750)

output$map_saint_pierre <- renderPlot({ 
    ggplot() +
      geom_sf(data= iris2[336,31])+
      geom_sf(data= iris2[301,31])+
      geom_sf(data= shapefile2[4,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris2[336,31], label = "Bois d'Olives Est")+
      geom_sf_label(data= iris2[301,31], label = "Bois d'Olives Ouest")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                              axis.title.x = element_blank(), axis.title.y = element_blank())
  },
  height = 500, width = 500)

output$info_saint_pierre <- renderText({"La convention pluriannuelle du projet de renouvellement urbain de la ville de Saint-Pierre Bois d’Olive a été signée le 26 mars 2020. Des avenants mineurs ont été signé le 22 mars 2023 et le 23 novembre 2023. 
\nLa ville de Saint-Pierre travaille actuellement avec les donneurs d’ordre impliqués dans le projet a la mutualisation de la facilitation pour assurer la bonne mise en œuvre du volet social. 
\nSuperficie = 554 830 m²"})

output$heure_conventionne_saint_pierre     <- renderTable(striped = TRUE,{heure_conventionné_saint_pierre})
output$objectif_saint_pierre_anru          <- renderTable(striped = TRUE,{objectif_saint_pierre_anru})
output$heure_non_conventionne_saint_pierre <- renderTable(striped = TRUE,{heure_non_conventionné_saint_pierre})
output$objectif_saint_pierre_lbu           <- renderTable(striped = TRUE,{objectif_saint_pierre_lbu})


output$map_le_port <- renderPlot({ 
    ggplot() +
      geom_sf(data= iris2[263,31])+
      geom_sf(data= iris2[44,31])+
      geom_sf(data= iris2[175,31])+
      geom_sf(data= iris2[319,31])+
      geom_sf(data= iris2[193,31])+
      geom_sf(data= iris2[103,31])+
      geom_sf(data= iris2[95,31])+
      geom_sf(data= iris2[76,31])+
      geom_sf(data= iris2[335,31])+
      geom_sf(data= iris2[314,31])+
      geom_sf(data= shapefile2[37,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris2[263,31], label = "Haute cité - Ariste Bolon")+
      geom_sf_label(data= iris2[44,31], label = "Satec")+
      geom_sf_label(data= iris2[175,31], label = "Sidr Basse")+
      geom_sf_label(data= iris2[319,31], label = "ZAC Cité Cœur Saignant'")+
      geom_sf_label(data= iris2[193,31], label = "ZAC Cités Vergès/Lepervanche")+
      geom_sf_label(data= iris2[314,31], label = "Centre ville Est")+
      geom_sf_label(data= iris2[335,31], label = "zup3")+
      geom_sf_label(data= iris2[76,31], label = "zup 3")+
      geom_sf_label(data= iris2[95,31], label = "Cité maloya")+
      geom_sf_label(data= iris2[103,31], label = "Cité du Stade")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                              axis.title.x = element_blank(), axis.title.y = element_blank())
  },
  
  height = 500, width = 500)

output$info_le_port <- renderText({"La convention pluriannuelle du projet de renouvellement urbain de la ville du Port Ariste Bolon a été signée le 13 mars 2020 (avenant n°1 signé le 13 avril 2022).
  \nSuperficie = 1 884 242 m²"})

output$heures_le_port           <- renderTable(striped = TRUE,{heures_le_port})
output$objectif_echelle_le_port <- renderTable(striped = TRUE,{objectif_echelle_le_port})
output$objectif_anru_le_port   <- renderTable(striped = TRUE,{objectif_anru_le_port})
output$objectif_lbu_le_port    <- renderTable(striped = TRUE,{objectif_lbu_le_port})

output$genre_le_port <- renderPlot({
  pie <- ggplot(genre_le_port, aes(x = "", y = nombre, fill = genre)) +
    geom_col(color = "black") +
    coord_polar("y", start = 2.5) +
    theme_void() +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.text = element_text(size = 17)) +
    geom_label(aes(label = pourcentage), position = position_stack(vjust = 0.7),
               size = 5.5, show.legend = FALSE) +
    scale_fill_manual(values = c("royalblue", "orange"))
  pie
}, height = 165, width = 500)

output$age_le_port <- renderPlot({
  ggplot(age_le_port, aes(x = age, y = nombre, fill = age)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    theme_minimal() +
    geom_text(aes(label = pourcentage), hjust = 0.67, vjust = 1.25, color = "black", size = 7) +
    theme(axis.title = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(size = 17)) +
    scale_fill_manual(values = c("grey", "orange", "royalblue", "orange"))
})

output$contrat_le_port <- renderPlot({
  ggplot(contrat_le_port, aes(x = contrat, y = nombre, fill = contrat)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    theme_minimal() +
    geom_text(aes(label = pourcentage), hjust = 0.67, vjust = .7, color = "black", size = 6.2) +
    theme(axis.title = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20)) +
    scale_fill_manual(values = c("royalblue", "orange", "grey"))
})

  output$map_saint_louis <- renderPlot({ 
    ggplot() +
      geom_sf(data= iris2[210,31])+
      geom_sf(data= iris2[224,31])+
      geom_sf(data= iris2[317,31])+
      geom_sf(data= iris2[1,31])+
      geom_sf(data= shapefile2[10,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris2[210,31], label = "Le gol")+
      geom_sf_label(data= iris2[224,31], label = "Centre Ville")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                              axis.title.x = element_blank(), axis.title.y = element_blank())
  },
  
  height = 500, width = 500)
  
  output$info_saint_louis <- renderText({"La convention pluriannuelle du projet de renouvellement urbain de la ville de Saint-Louis Le Gol a été signée le 16 mars 2022. Un avenant a été signé le 23 mars 2025.
\nSuperficie : 413 875 m²
"})

output$heure_saint_louis1 <- renderTable(striped = TRUE,{heure_saint_louis1})
output$objectif_operation_saint_louis <- renderTable(striped = TRUE,{objectif_operation_saint_louis})
output$objectif_MO_saint_louis        <- renderTable(striped = TRUE,{objectif_MO_saint_louis})

}

# Run the app ----
shinyApp(ui = ui, server = server)



