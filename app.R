require(shiny)
require(ggplot2)

############ Préparation des jeux de données dont nous aurons besoin ############

#### Classification ascendante hiérarchique des variables ------------------------------#
load("export/classif_questions.RData")



#---------------------------------------------------------------------------------------------------------------------------------------#


############ Interface utilisateur ############
ui<-navbarPage(
  
  #Titre de la barre de navigation
  "Navigation",
  
  #### Onglet de la cah ####
  tabPanel(
    "Classifactions ascendantes hiérarchiques des variables",
    
    #Layout de l'onglet
    sidebarLayout(
      
      #Partie gauche de la page
      sidebarPanel(
        
        #Menu déroulant pour séléctionner le jeu de données
        selectInput(
          inputId = "choix_cah",
          label = "Séléction de la classification",
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
      
      #Partie centrale de la page
      mainPanel(
        
        #Graphique qui sera affiché
        plotOutput("cah")
      )
    )
  ),
  
  #### Onglet de la cah ####
  tabPanel(
    "Classifactions ascendantes hiérarchiques des variables",
    
    #Layout de l'onglet
    sidebarLayout(
      
      #Partie gauche de la page
      sidebarPanel(
        
        #Menu déroulant pour séléctionner le jeu de données
        selectInput(
          inputId = "choix_cah",
          label = "Séléction de la classification",
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
      
      #Partie centrale de la page
      mainPanel(
        
        #Graphique qui sera affiché
        #plotOutput("cah")
      )
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
