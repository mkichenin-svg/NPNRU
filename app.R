
library(shiny)
library(bslib)
library(ggplot2)
library(sf)
library(tibble)
library(bsicons)
library(readxl)


shapefile <-read_sf("QPV/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")
shapefile1 <-read_sf("communes/communesPolygon.shp")
shapefile2 <- read_sf("QPV2/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")

iris <- read_sf("iris/georef-france-iris-millesime.shp")
iris2 <- read_sf("iris2/georef-france-iris-millesime.shp")

genre <- read.csv2("genre.csv")
age <- read.csv2("age.csv")


quali_le_port <- read.csv2("quali_le_port.csv", fileEncoding = "UTF-8-BOM")

# Données CSV manquantes
heures_saint_denis                  <- read.csv2("heures_saint_denis.csv")
heure_echelle_saint_benoit          <- read.csv2("heure_echelle_saint_benoit.csv")
heure_ANRU_saint_benoit             <- read.csv2("heure_ANRU_saint_benoit.csv")
heure_echelle_saint_andré           <- read.csv2("heure_echelle_saint_andré.csv")
heure_anru_saint_andré              <- read.csv2("heure_anru_saint_andré.csv")
heure_conventionné_saint_pierre     <- read.csv2("heure_conventionné_saint_pierre.csv")
heure_non_conventionné_saint_pierre <- read.csv2("heure_non_conventionné_saint_pierre.csv")
heures_le_port                      <- read.csv2("heures_le_port.csv")
heure_saint_louis1                  <- read.csv2("heure_saint_louis1.csv")

# Données Excel manquantes
echelle_saint_denis        <- read_excel("echelle_saint_denis.xlsx")
MO_saint_denis             <- read_excel("MO_saint_denis.xlsx")
echelle_saint_benoit       <- read_excel("echelle_saint_benoit.xlsx")
echelle_LBU_saint_benoit   <- read_excel("echelle_LBU_saint_benoit.xlsx")
echelle_NPNRU_saint_benoit <- read_excel("echelle_NPNRU_saint_benoit.xlsx")
objectifs_saint_andré      <- read_excel("objectifs_saint_andré.xlsx")
objectif_saint_pierre_anru <- read_excel("objectif_saint_pierre_anru.xlsx")
objectif_saint_pierre_lbu  <- read_excel("objectif_saint_pierre_lbu.xlsx")
objectif_echelle_le_port   <- read_excel("objectif_echelle_le_port.xlsx")
objectif_anru_le_port      <- read_excel("objectif_anru_le_port.xlsx")
objectif_lbu_le_port       <- read_excel("objectif_lbu_le_port.xlsx")
objectif_operation_saint_louis <- read_excel("objectif_opération_saint_louis.xlsx")
objectif_MO_saint_louis    <- read_excel("objectif_MO_saint_louis.xlsx")

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



ui <-page_navbar( title = div("POINT D'ÉTAPE ANRU ", class="custom title"), 
    # Custom CSS to change title size
    header = tagList(
    tags$style(HTML("
    .navbar .navbar-brand {
      font-size: 20px;
      font-weight: bold;
      color: #2c3e50;
    } ")),
    tags$script(HTML("
      var observer = new MutationObserver(function() {
        document.querySelectorAll('[title]').forEach(function(el) {
          if (el.title.toLowerCase().includes('arrow keys') ||
              el.title.toLowerCase().includes('resize')) {
            el.removeAttribute('title');
          }
        });
      });
      observer.observe(document.body, { childList: true, subtree: true });
    "))
    ),
                        
  theme = theme_bootswatch("minty"),
  
  sidebar = sidebar(width= 170,
                    div(style = "max-width: 400px; font-size: 0.785 rem; margin-left: 1px; margin-bottom: 4px;", textOutput("info_side")),
                    div(style = "display: flex; flex-direction: column; align-items: center; gap: 6px;",
                      tags$img(src = "img/anru.png",      style = "width: 100%; height: auto; max-height: 150px; object-fit: contain;"),
                      tags$img(src = "img/blanc.jpg",      style = "width: 100%; height: auto; max-height: 100px; object-fit: contain;"),
                      tags$img(src = "img/logo_pref.png", style = "width: 100%; height: auto; max-height: 150px; object-fit: contain;"),
                      tags$img(src = "img/blanc.jpg",      style = "width: 100%; height: auto; max-height: 100px; object-fit: contain;"),
                      tags$img(src = "img/logo_mden.png", style = "width: 100%; height: auto; max-height: 200px; object-fit: contain;")
                    )
                    ),
  
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
    "57 205 heures réalisées", 
    "% des objectifs conventionnés ANRU / LBU", 
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
                tableOutput("heures_saint_denis")),
      
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
                                              
),

layout_column_wrap(width = 550, height = 770,
  
  
navset_card_tab(
  
    nav_panel("Présentation",
      layout_column_wrap(
      plotOutput("map", width = 500, height = 500),
      div(style = "max-width: 300px; font-size: 0.785 rem; margin-left: 110px; white-space: pre-line;", textOutput("info_saint_benoit")))),
    
    nav_panel("Avancement des heures à l'échelle du projet",
              tableOutput("heure_echelle_saint_benoit")),
    
    nav_panel("État d'avancement des heures ANRU",
              tableOutput("heure_ANRU_saint_benoit")),

    
  ),
  
  
  card(
    
    
    value_box( 
      title = "", 
      "12 bénéficiaires issus d'un QPV (33,3%)", 
      "Dont 7 d'un QPV de Saint-Benoît",
      showcase = bsicons::bs_icon("buildings"),
      height = 50
    ) ,
    
    
    value_box( 
      title = "", 
      "Bénéficiaires majoritairement masculins",
      showcase = bsicons::bs_icon("arrow-90deg-right"),
      height = 10,
    ) ,
    
    plotOutput("genre_saint_benoit", height = 150, width = 550),
    
    value_box( 
      title = "",
      "64 % des bénéficaires ont moins de 41 ans",
      showcase = bsicons::bs_icon("arrow-90deg-right"),
      showcase_layout = "left center",
      height = 10
    ) ,
    
    plotOutput("age_saint_benoit", height = 70),
    
    
  ),
  
),

),
),

#page Saint-André - Centre ville

nav_panel("Saint-André - Centre Ville",
         
          p(layout_column_wrap(width = 200,
                              
                              value_box( 
                                title = "", 
                                "43 604 heures à réaliser à l'échelle du projet", 
                                showcase = bsicons::bs_icon("clock"),
                                showcase_layout = "left center",
                                theme = "primary",
                                height = 100
                              ) ,
                              
                              value_box( 
                                title = "", 
                                "49 772 heures réalisées", 
                                "% des objectifs conventionnés ANRU / LBU", 
                                showcase = bsicons::bs_icon("calendar2-check"),
                                showcase_layout = "left center",
                                theme = "primary",
                                height = 100
                              ) ,
                              
                              value_box( 
                                title = "", 
                                "108 bénéficiaires", 
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
      
             
    
           ),
           
           
           
           card(
             
             navset_card_tab(
             nav_panel("Suivi des heures d'insertion à l'échelle du projet",
                       tableOutput("heure_echelle_saint_andré")),
             
             nav_panel("Suivi des heures conventionnées",
                       tableOutput("heure_ANRU_saint_andre")),
           
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
                                 "% des objectifs conventionnés ANRU / LBU", 
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
                                 "5 bénéficiaires issus d'un QPV (%)", 
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
                                 "% des objectifs conventionnés ANRU / LBU", 
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
                                 "Tous des QPV de Le Port",
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
            
            
            card(
              
              value_box( 
                title = "", 
                "Bénéficiaires majoritairement masculins",
                showcase = bsicons::bs_icon("arrow-90deg-right"),
                height = 70,
              ) ,
              
              plotOutput("genre_le_port", height = 200),
              
              value_box( 
                title = "",
                "20% des bénéficiaires ont moins de 41 ans",
                showcase = bsicons::bs_icon("arrow-90deg-right"),
                showcase_layout = "left center",
                height = 70
              ) ,
              
              plotOutput("age_le_port", height = 150),

              value_box(
                title = "",
                "Type d'embauche des bénéficiaires",
                showcase = bsicons::bs_icon("arrow-90deg-right"),
                showcase_layout = "left center",
                height = 70
              ),

              plotOutput("contrat_le_port", height = 150),
              
              
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
                                 "% des objectifs conventionnés ANRU / LBU", 
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
                                 "Tous d'un QPV de Saint-Louis",
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





# fin corps
)
# fin corps
  


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  output$info_side <- renderText({"Cette note présente un point d’étape sur la mise en œuvre du volet insertion des NPNRU de La Réunion."})
  
  
  
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
  
  output$genre_saint_benoit <- renderPlot({ genre_pie(genre) }, height = 210, width = 500)
  output$age_saint_benoit   <- renderPlot({ age_bar(age) })
  
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
  
  output$heures_saint_denis <- renderTable(heures_saint_denis)
  output$echelle_saint_denis <- renderTable(echelle_saint_denis)
  output$MO_saint_denis <- renderTable({head(MO_saint_denis)})
  
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
  
output$echelle_saint_benoit <- renderTable({head(echelle_saint_benoit)})
output$echelle_LBU_saint_benoit <- renderTable({head(echelle_LBU_saint_benoit)})
output$echelle_NPNRU_saint_benoit <- renderTable({head(echelle_NPNRU_saint_benoit)})
output$heure_echelle_saint_benoit <- renderTable({heure_echelle_saint_benoit})
output$heure_ANRU_saint_benoit <- renderTable({heure_ANRU_saint_benoit})
  
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

output$heure_echelle_saint_andré <- renderTable({heure_echelle_saint_andré})
output$objectifs_saint_andre     <- renderTable({objectifs_saint_andre})
output$heure_ANRU_saint_andre    <- renderTable({heure_anru_saint_andré})


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

output$heure_conventionne_saint_pierre     <- renderTable({heure_conventionné_saint_pierre})
output$objectif_saint_pierre_anru          <- renderTable({objectif_saint_pierre_anru})
output$heure_non_conventionne_saint_pierre <- renderTable({heure_non_conventionné_saint_pierre})
output$objectif_saint_pierre_lbu           <- renderTable({objectif_saint_pierre_lbu})


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

output$heures_le_port           <- renderTable({heures_le_port})
output$objectif_echelle_le_port <- renderTable({objectif_echelle_le_port})
output$objectif_anru_le_port   <- renderTable({objectif_anru_le_port})
output$objectif_lbu_le_port    <- renderTable({objectif_lbu_le_port})

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
}, height = 200, width = 500)

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
    geom_text(aes(label = pourcentage), hjust = 0.67, vjust = 0.5, color = "black", size = 6.3) +
    theme(axis.title = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(size = 17)) +
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

output$heure_saint_louis1 <- renderTable({heure_saint_louis1})
output$objectif_operation_saint_louis <- renderTable({objectif_operation_saint_louis})
output$objectif_MO_saint_louis        <- renderTable({objectif_MO_saint_louis})

}

# Run the app ----
shinyApp(ui = ui, server = server)



