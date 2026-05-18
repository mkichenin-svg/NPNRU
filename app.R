

library(shiny)
library(bslib)
library(ggplot2)
library(sf)
library(tibble)
library(bsicons)

shapefile <-read_sf("QPV/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")
shapefile1 <-read_sf("communes/communesPolygon.shp")
iris <- read_sf("iris/georef-france-iris-millesime.shp")


ui <- fluidPage( 
  
      titlePanel(
        "NPNRU Rive-Droite - Saint-Benoît"
      ),
  
  theme = theme_bootswatch("superhero"),


  # Première page
    
    layout_column_wrap(
      
      card(
        plotOutput("map", width = 500, height = 500),
        
      ),
      
      card(
        card_header("LES DONNÉES À REMONTER SELON LA NOUVELLE CHARTE NATIONALE D'INSERTION ANRU   "),
        card_body(height = 30),
        p("- nombre d’heures travaillées pour les opérations liées aux travaux et dans le cadre de la gestion urbaine de proximité"),
        p("- modalités de réalisation des heures (embauche directe, intérim, alternance, formation…)"),
        p("- typologie des entreprises attributaires (nombre de salariés, secteur d’activité…)"),
        p("- typologie des bénéficiaires : sexe, âge, résidence dans un quartier prioritaire de la politique de la ville, …"),
        p(" - situation des bénéficiaires à 6 et 12 mois après leur entrée dans le dispositif ;"),
        p(" - embauches directes ou indirectes liées à l’ingénierie des projets, au fonctionnement des équipements et aux actions d’accompagnement")
  
      )
      
      
    ),
    
    
    layout_column_wrap(
      
      card(
        card_header(),
        tableOutput("echelle")
      ),
      
      card(
        card_header(),
        tableOutput("convent")
   ),
),
    
    
  
 
)
  
  # fin corps
  


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  
  output$image <- renderImage( 
    { 
      list(src = "logo.png", height = "30%") 
    }, 
    deleteFile = FALSE 
  ) 
  
  # carte
  
  output$map <- renderPlot({ 
      ggplot() +
      geom_sf(data = iris[1,31])+
      geom_sf_label(data= iris[1,31], label = "Bras fusil")+
      geom_sf(data = iris[3,31])+
      geom_sf(data = iris[9,31])+
      geom_sf(data = iris[12,31])+
      geom_sf(data = iris[21,31])+
      geom_sf(data= shapefile[36,], color = "black", fill="orange", alpha= 7/10
              ) +
      geom_sf_label(data= iris[3,31], label = "Beaufond_Le_Port")+
      geom_sf_label(data = iris[9,31], label = "Beaufond_Distillerie")+
      geom_sf_label(data = iris[12,31], label = "Centre ville-Rive gauche")+
      geom_sf_label(data = iris[21,31], label = "centre ville - Rive droite")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                                axis.title.x = element_blank(), axis.title.y = element_blank())
  
  }) 
  
  
  output$echelle <- renderTable({ echelle
    
    
  }) 
 
  output$convent <- renderTable({ convent
   
    
  }) 
  
  
  
  

  
  

  
}



# Run the app ----
shinyApp(ui = ui, server = server)

