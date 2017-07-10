library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Alberta Aerial Ungulate Survey Population Estimator"),
  sidebarPanel(
    fileInput('MegaDB', 'Step 1. Choose your Access database to commence distance sampling analysis',
              accept=c('.accdb', '.mdb')),
    fileInput(ns('WMU_Shp'), 'Step 2. Choose your WMU Polygon Shapefile',
              accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
  #  fileInput('Strata_Shp', 'Step 2. Choose your Strata Polygon Shapefile',
  #            accept=c('.shp')),
  #  fileInput('TransFlown_Shp', 'Step 2. Choose your Flown Transects Shapefile',
  #            accept=c('.shp')),
    tags$hr(),
    tags$b("Select your species:"),
    checkboxInput('moos', 'Moose', TRUE),
    checkboxInput('wtde', 'White-tailed deer', TRUE),
    checkboxInput('mude', 'Mule deer', TRUE),
    checkboxInput('wapt', 'Elk', TRUE),
    radioButtons('cov', 'Covariates',
                 c('Crown Closure' ='CC',
                   Activity='ACT')),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote')
  ),
  mainPanel(
    tableOutput('contents'),
    plotOutput("myplot"),
    plotOutput("myplot2")
  )
))
