require(shiny)
require(ggplot2)

############ Préparation des jeux de données dont nous aurons besoin ############

#### Classification ascendante hiérarchique des variables ------------------------------#
load("export/classif_questions.RData")



#---------------------------------------------------------------------------------------------------------------------------------------#


############ Interface utilisateur ############
ui <- fluidPage(
  
  #Titre de l'application
  titlePanel("Application Shiny projet Cogmont"),
  
  #Définition du layout de l'appli
  sidebarLayout(
    
    #Composants de la région gauche de l'application
    sidebarPanel(
      
      #Menu déroulant pour choisir le jeu de données
      selectInput(
        inputId = "choix_cah",
        label = "Veuillez choisir une classification",
        choices = ls()[which(substr(ls(),1,3)=="cah")]
      ),
      
      #Choix du nombre de classes
      sliderInput(
        inputId = "nb_class",
        label = "Nombre de classes",
        min = 2,
        max = 20,
        value = 7
      )
    ),
    
    #Composants dans la fenêtre principale (au milieu)
    mainPanel(
      
      #Un graphique  
      plotOutput("cah")
    )
  )
)


#---------------------------------------------------------------------------------------------------------------------------------------#


############ Partie server ############
server <- function(input, output) {
  
  #l'élément displot que nous avons créé dans ui
  output$cah <- renderPlot({
    
    #Affichage du dendogramme
    plot(get(input$choix_cah))
    rect.hclust(get(input$choix_cah),input$nb_class)
  })
}


#---------------------------------------------------------------------------------------------------------------------------------------#

# Run the application 
shinyApp(ui = ui, server = server)
