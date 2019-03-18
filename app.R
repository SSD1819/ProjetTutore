require(shiny)
require(ggplot2)

############ Préparation des jeux de données dont nous aurons besoin ############

#### Classification ascendante hiérarchique des variables ------------------------------
load("export/classif_questions.RData")



#---------------------------------------------------------------------------------------------------------------------------------------#


############ Interface utilisateur ############
ui <- fluidPage(
  
  #Titre de l'application
  titlePanel("Données clim'Actions"),
  
  #Définition du layout de l'appli
  sidebarLayout(
    
    #Composants de la région gauche de l'application
    sidebarPanel(
      
      #Menu déroulant pour choisir le jeu de données
      selectInput(
        inputId="choix_ville",
        label="Veuillez choisir une ville",
        choices=c()
      ),
      
      #Menu déroulant pour choisir la variable
      selectInput(
        inputId="choix_variable",
        label="Veuillez choisir une variable",
        choices=names(donnees[,c(8,10,12,13)])
      )
      
    ),
    
    
    
    #Composants dans la fenêtre principale (au milieu)
    mainPanel(
      
      #Un graphique  
      plotOutput("distPlot")
    )
  )
)


#---------------------------------------------------------------------------------------------------------------------------------------#


############ Partie server ############
server <- function(input, output) {
  
  #l'élément displot que nous avons créé dans ui
  output$distPlot <- renderPlot({
    
    #On récupère la ville et la variable séléctionnée
    ville<-input$choix_ville
    variable<-input$choix_variable
    
    #date début, date fin
    deb<-min(donnees[which(donnees$Ville==ville),4])
    fin<-max(donnees[which(donnees$Ville==ville),4])
    
    #Transformation de notre variable en objet time series
    variableTS<- ts(donnees[which(donnees$Ville==ville),which(names(donnees)==variable)], freq = 365, start = deb)
    
    #Aggrégation de notre jeu de données par année
    variableTSBY<-aggregate(variableTS,nfrequency=1,FUN=mean)
    
    #Préparation jeu de donnees pour ggplot
    dataplot<-as.data.frame(cbind(variableTSBY,c(deb:fin)))
    names(dataplot)<-c("valeur","date")
    
    #Création du graphique
    ggplot(dataplot,aes(date,valeur))+
      geom_line(col="chartreuse4")+
      geom_point(size=2,col="chartreuse3")+
      geom_smooth(col="red")
  })
}


#---------------------------------------------------------------------------------------------------------------------------------------#


# Run the application 
shinyApp(ui = ui, server = server)

#Supprssion des variables qui ne nous servent pas
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe")))
