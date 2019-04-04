require(shiny)
require(ggplot2)
require(corrplot)
require(FactoMineR)
require(arules)

############ Préparation des jeux de données dont nous aurons besoin ############

#### Classification ascendante hiérarchique des variables ------------------------------#
load("export/classif_questions.RData")

#### ACP ------------------------------#
load("export/ACP.RData")

#### Règles d'association ------------------------------#
load("export/ReglesAssociations.RData")

#---------------------------------------------------------------------------------------------------------------------------------------#


############ Interface utilisateur ############
ui<-navbarPage(
  
  #Titre de la barre de navigation
  "Navigation",
  
  #### Onglet de la cah ####-------------------------------------------------------#
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
  
  #### Onglet de l'ACP ####-------------------------------------------------------#
  tabPanel(
    "Analyse en composantes principale",
    
    ###Ligne du dessus
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

      #Zone choix des axes à afficher
      column(4,
             numericInput("axe1", "Axe1", 1, min = 1, max = 100),
             numericInput("axe2", "Axe2", 2, min = 1, max = 100)
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
  ),

  #### Onglet des règles d'association ####-------------------------------------------------------#
  tabPanel(
    "Règles d'association",
    
    #Layout de l'onglet
    sidebarLayout(
      
      #Partie gauche de la page
      sidebarPanel(
        
        #Paramètres des règles d'associations
        numericInput("supp", "Support", 0.7, min = 0, max = 0.99),
        numericInput("conf", "Confidence", 0.7, min = 0, max = 0.99),
        numericInput("lift", "Lift", 1.15, min = 1, max = 100),
        numericInput("minlen", "Longueur minimum règle", 1, min = 1, max = 5),
        numericInput("maxlen", "Longueur maximum règle", 2, min = 1, max = 5),
        
        #Variables à exclure
        checkboxGroupInput("inclus","Variables prises en compte",colnames(datAssos),selected=colnames(datAssos))
        
      ),
      
      #Partie centrale de la page
      mainPanel(
        
        #Graphique qui sera affiché
        dataTableOutput("rules")
      )
    )
  )
)

#---------------------------------------------------------------------------------------------------------------------------------------#


############ Partie server ############
server <- function(input, output) {
  
  #### CAH ####-------------------------------------------------------#
  #l'élément displot que nous avons créé dans ui
  output$cah <- renderPlot({
    
    #Affichage du dendogramme
    plot(get(input$choix_cah))
    rect.hclust(get(input$choix_cah),input$nb_class)
  })
  
  #### ACP ####-------------------------------------------------------#
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

  ##le graphique des variances expliquées
  
  
  #dataframe pour le graphique
  datagraph <- reactive({
    datagraph <- as.data.frame(cbind(names(get(input$choix_acp)$eig[,2]),round(get(input$choix_acp)$eig[,2],2)))
    datagraph$V2<-round(get(input$choix_acp)$eig[,2],2)
    datagraph
  })
  
  #graphique
  output$inertie <- renderPlot({
    ggplot(data=datagraph(), aes(x=reorder(V1, -V2),y=V2))+
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=V2), vjust=1.6, color="white", size=3.5)+
      xlab("Axe")+
      ylab("% Variance expliquée")
  })   
  
  #### Règles d'associations ####-------------------------------------------------------#
  
  #preparation des règles
  ruls <- reactive({
    ruls <- apriori(datAssos[,input$inclus], parameter = list(supp = as.numeric(input$supp), conf = as.numeric(input$conf), minlen = as.numeric(input$minlen), maxlen = as.numeric(input$maxlen), target = "rules"))
    ruls
  })
  
  #Affichage des règles
  output$rules <- renderDataTable({
    inspect(ruls())
  })
  
  
}


#---------------------------------------------------------------------------------------------------------------------------------------#

# Run the application 
shinyApp(ui = ui, server = server)
