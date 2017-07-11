library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Alberta Aerial Ungulate Survey Population Estimator"),
  sidebarPanel(
    fileInput('MegaDB', 'Step 1. Choose your Access database to commence distance sampling analysis',
              accept=c('.accdb', '.mdb')),
    fileInput('WMU_Shp', 'Step 2. Choose your WMU Polygon Shapefile File - Note include all shapefile components (i.e. *.shp, *.dbf, *.sbn, etc.)',
             accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
    #fileInput('Strata_Shp', 'Step 2. Choose your Strata Polygon Shapefile Files',
    #         accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
    fileInput('TransFlown_Shp', 'Step 3. Choose your Flown Transects Shapefile Files',
              accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
   sliderInput("truncation", "Step 4: Choose right truncation distance", min=0, max=1000, value=425, step = 25),
    tags$hr(),
    tags$b("Select your species:"),
    checkboxInput('moos', 'Moose', TRUE),
    checkboxInput('wtde', 'White-tailed deer', TRUE),
    checkboxInput('mude', 'Mule deer', TRUE),
    checkboxInput('wapt', 'Elk', TRUE),
    radioButtons('cov', 'Covariates', c('Crown Closure' ='CC', Activity='ACT')),
    radioButtons('quote', 'Quote', c(None='','Double Quote'='"', 'Single Quote'="'"))        
               ),
  mainPanel(
     tabsetPanel(
      tabPanel("Overview", plotOutput("myplot"),
                       plotOutput("myplot2")),
       tabPanel("Aircraft comparison", plotOutput("myplot"))
   
  ))
))
