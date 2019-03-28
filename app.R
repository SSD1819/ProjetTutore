require(shiny)
require(ggplot2)
require(corrplot)
require(FactoMineR)

############ Préparation des jeux de données dont nous aurons besoin ############

#### Classification ascendante hiérarchique des variables ------------------------------#
load("export/classif_questions.RData")

#### ACP ------------------------------#
load("export/ACP.RData")


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
  
  #### Onglet de l'ACP ####
  tabPanel(
    "Analyse en composantes principale",
    
    ###Ligne du dessus
    fluidRow(
      
      #Zone choix des axes à afficher
      column(4,
        numericInput("axe1", "Axe1", 1, min = 1, max = 100),
        numericInput("axe2", "Axe2", 2, min = 1, max = 100)
      ),
      
      #Zone inertie par axe
      column(4,
        plotOutput("inertie")
      ),
      
      #Zone graphique des corrélations
      column(4,
        plotOutput("correlations")
      )
    ),

    
    ###Ligne du dessous
    fluidRow(
      
      #Zone choix jeu de données (ACP avec ou sans outlier)
      column(4,
             
        #Menu déroulant pour séléctionner le jeu de données
        selectInput(
        inputId = "choix_acp",
        label = "Séléction des données sur lesquelles faire l'ACP",
        choices = ls()[which(substr(ls(),1,3)=="res")]
        )
      ),
             
      #Zone graphique des individus
      column(4,
        plotOutput("pcaInd")
      ),
             
      #Zone graphique des variables
      column(4,
        plotOutput("pcaVar")
      )
    )
  )
)

#---------------------------------------------------------------------------------------------------------------------------------------#


############ Partie server ############
server <- function(input, output) {
  
  #### CAH ####
  #l'élément displot que nous avons créé dans ui
  output$cah <- renderPlot({
    
    #Affichage du dendogramme
    plot(get(input$choix_cah))
    rect.hclust(get(input$choix_cah),input$nb_class)
  })
  
  #### ACP ####
  #le corrplot
  output$correlations <- renderPlot({
    corrplot(m.cor,method = "circle")
  })

  #le graphique des variables
  output$pcaVar <- renderPlot({
    plot.PCA(get(input$choix_acp),axes=c(input$axe1, input$axe2), choix = "var",select = "contrib 6")
  }) 
  
  #le graphique des individus
  output$pcaInd <- renderPlot({
    plot.PCA(get(input$choix_acp),axes=c(input$axe1, input$axe2),choix = "ind",cex=0.7,col.ind = as.numeric(valquanti$Pedagogie))
  }) 

  #le graphique des variances expliquées
  datagraph<-as.data.frame(cbind(names(res.pca$eig[,2]),round(res.pca$eig[,2],2)))
  datagraph$V2<-round(res.pca$eig[,2],2)
  output$inertie <- renderPlot({
    ggplot(data=datagraph, aes(x=reorder(V1, -V2),y=V2))+
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=V2), vjust=1.6, color="white", size=3.5)+
      xlab("Axe")+
      ylab("% Variance expliquée")
  })   
  
  
}


#---------------------------------------------------------------------------------------------------------------------------------------#

# Run the application 
shinyApp(ui = ui, server = server)
