
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
age <- read.csv2("age.csv")


ui <-page_fluid( 
  
      titlePanel(
        "NPNRU Rive droite - Ville de Saint-Benoît"
      ),
  
  theme = theme_bootswatch("minty"),
  

  
  # Première page
  layout_column_wrap(
    
    value_box( 
      title = "", 
      "19 726 heures conventionnées", 
      "34 025 heures à réaliser à l'échelle du projet", 
      showcase = bsicons::bs_icon("clock"),
      showcase_layout = "left center",
      theme = "primary",
      height = 100
    ) ,
    
  value_box( 
    title = "", 
    "14 863 heures réalisées", 
    "75% des objectifs conventionnés ANRU", 
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
    
    layout_column_wrap(
      
      
      card(
        plotOutput("map", width = 850, height = 700),
        
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
        
        plotOutput("genre", height = 100),
        
        value_box( 
          title = "",
          "64 % des bénéficaires ont moins de 41 ans",
          showcase = bsicons::bs_icon("arrow-90deg-right"),
          showcase_layout = "left center",
          height = 10
        ) ,
        
        plotOutput("age", height = 70),
      
        
      ),
     
    ),
  

layout_column_wrap(width = 100, height = 800,
  
imageOutput("anru", height = 450),
imageOutput("blanc", height = 400),
imageOutput("blanc", height = 400),
imageOutput("image", height = 400),
imageOutput("blanc", height = 400),
imageOutput("blanc", height = 400),
imageOutput("mden", height = 400),  
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
      list(src = "logo1.jpg", height = "35%") 
    }
  ) 
  
  
  output$blanc <- renderImage( 
    { 
      list(src = "blanc.jpg", height = "35%") 
    }
  ) 
  
  
  output$genre <- renderPlot(
    
    { 
      
      pie <- ggplot(genre, aes(x="", y= nombre, fill= genre)) + geom_col(color="black") +
        coord_polar("y", start = 2.5) +
        theme_void() + 
        theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=17)) +
        geom_label(aes(label = pourcentage), position= position_stack(vjust = 0.7),size = 5.5,
                   show.legend = FALSE) + scale_fill_manual(values = c ("royalblue", "orange")) 
      
      pie
      },
    height = 210, width = 500
  )
  
  output$age <- renderPlot(
    
    { 
      ggplot(age,aes(x= age, y = nombre, fill = age)) + geom_bar(stat = "identity", show.legend = FALSE) + theme_minimal() + 
         geom_text(aes(label = pourcentage), hjust= 0.67,vjust= 1.5, color="black", size = 7) + 
        theme(axis.title = element_blank(), axis.text.y  = element_blank(), axis.text.x = element_text(size = 17 )) + scale_fill_manual(values = c ("grey", "orange","royalblue", "orange")) 
    }
  )
  
  # carte
  
  output$map <- renderPlot({ 
      ggplot() +
      geom_sf(data = iris[1,31])+
      geom_sf_label(data= iris[1,31], label = "Beaulieu - Bras fusil - La Confiance")+
      geom_sf(data = iris[3,31])+
      geom_sf(data = iris[9,31])+
      geom_sf(data = iris[12,31])+
      geom_sf(data = iris[21,31])+
      geom_sf(data= shapefile[36,], color = "black", fill="orange", alpha= 7/10) +
      geom_sf_label(data= iris[3,31], label = "Beaufond Le port")+
      geom_sf_label(data = iris[9,31], label = "Beaufond Distillerie")+
      geom_sf_label(data = iris[12,31], label = "Centre ville-Rive gauche")+
      geom_sf_label(data = iris[21,31], label = "Centre ville-Rive droite")+
      theme_minimal() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                                axis.title.x = element_blank(), axis.title.y = element_blank())
  
  }) 
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)



