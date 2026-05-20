
library(shiny)
library(bslib)
library(ggplot2)
library(sf)
library(tibble)
library(bsicons)

shapefile <-read_sf("QPV/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")
shapefile1 <-read_sf("communes/communesPolygon.shp")
iris <- read_sf("iris/georef-france-iris-millesime.shp")

genre <- read.csv2("genre.csv")
genre
age <- read.csv2("age.csv")
age

ui <-page_fluid( 
  
      titlePanel(
        "NPNRU Rive-Droite - Saint-Benoît"
      ),
  
  theme = theme_bootswatch("minty"),
  

  
  # Première page
  layout_column_wrap(
    
    value_box( 
      title = "", 
      "34 025 heures à réaliser", 
      "19 726 heures financées ANRU", 
      showcase = bsicons::bs_icon("clock"),
      showcase_layout = "left center",
      theme = "primary",
      height = 100
    ) ,
    
  value_box( 
    title = "", 
    "12 283 heures réalisées", 
    "62% des objectifs conventionnés ANRU", 
    showcase = bsicons::bs_icon("calendar2-check"),
    showcase_layout = "left center",
    theme = "primary",
    height = 100
  ) ,
  
  value_box( 
    title = "", 
    "38 bénéficiaires", 
    theme = "primary", 
    class = "border",
    showcase = bsicons::bs_icon("person-fill-check"),
    height = 100
  ) ,
  
  value_box( 
    title = "", 
    "34 bénéficiaires issus d'un QPV", 
    "89% du total", 
    theme = "primary",
    showcase = bsicons::bs_icon("buildings"),
    height = 100
  ) ,
  
  ),
    
    layout_column_wrap(
      
      
      card(
        plotOutput("map", width = 700, height = 700),
        
      ),
      
     
      card(
        value_box( 
          title = "", 
          "75 % des bénéficaires ont moins de 40 ans",
          class = "border",
          height = 10
        ) ,
        
        
        plotOutput("age", height = 70),
        
        value_box( 
          title = "", 
          "Bénéficiaires majoritairement masculins (32/34)",
          class = "border",
          height = 10
        ) ,
        plotOutput("genre", height = 70),
        
      ),
      
     
    ),
  

layout_column_wrap(  
  
imageOutput("anru", height = 500),
imageOutput("image", height = 400, width = 400),  
imageOutput("mden", height = 200, width = 100),
) 
 
)
  
  # fin corps
  


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  output$image <- renderImage( 
    { 
      list(src = "logo.jpg", height = "30%") 
    }
  ) 
  
  output$anru <- renderImage( 
    { 
      list(src = "anru.png", height = "30%") 
    }
  ) 
  
  output$mden <- renderImage( 
    { 
      list(src = "mden.png", height = "25%") 
    }
  ) 
  
  output$genre <- renderPlot(
    
    { 
      ggplot(genre,aes(x= genre, y = nombre, fill = genre)) + geom_bar(stat = "identity", show.legend = FALSE) + theme_minimal() +
        theme(axis.title = element_blank(), axis.text.y = element_blank()) 
      }
  )
  
  output$age <- renderPlot(
    
    { 
      ggplot(age,aes(x= âge, y = Nombre, fill = âge)) + geom_bar(stat = "identity", show.legend = FALSE) + theme_minimal()+
        theme(axis.title = element_blank(), axis.text.y  = element_blank()) 
    }
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
      geom_sf(data= shapefile[36,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris[3,31], label = "Beaufond_Le_Port")+
      geom_sf_label(data = iris[9,31], label = "Beaufond_Distillerie")+
      geom_sf_label(data = iris[12,31], label = "Centre ville-Rive gauche")+
      geom_sf_label(data = iris[21,31], label = "centre ville - Rive droite")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                                axis.title.x = element_blank(), axis.title.y = element_blank())
  
  }) 
  
  

 

  
}


# Run the app ----
shinyApp(ui = ui, server = server)



