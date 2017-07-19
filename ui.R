library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Alberta Aerial Ungulate Survey Population Estimator"),
  sidebarPanel(
    fileInput('MegaDB', 'Step 1. Choose your Access database to commence distance sampling analysis',
              accept=c('.accdb', '.mdb')),
    fileInput('WMU_Shp', 'Step 2. Choose your WMU Polygon Shapefile File (Alberta 10TM) - Note include all shapefile components (i.e. *.shp, *.dbf, *.sbn, etc.)',
             accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
    #fileInput('Strata_Shp', 'Step 2. Choose your Strata Polygon Shapefile Files',
    #         accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
    #fileInput('TransFlown_Shp', 'Step 3. Choose your Flown Transects Shapefile Files',
    #          accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
   sliderInput("truncation", "Step 3: Choose right truncation distance", min=0, max=1000, value=425, step = 25)
    
               ),
  mainPanel(
     tabsetPanel(
      tabPanel("Moose", plotOutput("myplot"),
                       plotOutput("myplot2")),
       tabPanel("Mule Deer", plotOutput("myplot3"),
                             plotOutput("MUDE_MAP")),
       tabPanel("White-tailed Deer", plotOutput("WTDE_DF"),
                                     plotOutput("WTDE_MAP")),
        tabPanel("Elk", plotOutput("WAPT_DF"),
                        plotOutput("WAPT_MAP"))

  ))
))
