#install and load required packages -----------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("shiny", "DT")
ipak(packages)


shinyUI(pageWithSidebar(
  headerPanel("Alberta Aerial Ungulate Survey Population Estimator"),
  sidebarPanel(
    fileInput('MegaDB', 'Step 1. Choose your Access database to commence distance sampling analysis',
              accept=c('.accdb', '.mdb')),
    fileInput('WMU_Shp', 'Step 2. Choose your WMU Polygon Shapefile File (Alberta 10TM) - Note include all shapefile components (i.e. *.shp, *.dbf, *.sbn, etc.)',
             accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
   sliderInput("truncation", "Step 3: Choose right truncation distance", min=0, max=1000, value=800, step = 25)
    
               ),
  mainPanel(
     tabsetPanel(
       tabPanel("Moose",textOutput("MOOS_TXT"), DT::dataTableOutput('MOOS_TAB'), plotOutput("myplot2"),
                     plotOutput("MOOS_QQ"), 
                     plotOutput("myplot")),
            tabPanel("Mule Deer", plotOutput("MUDE_MAP"),
                     plotOutput("myplot3"),
                     plotOutput("MUDE_QQ")),
            tabPanel("White-tailed Deer", plotOutput("WTDE_MAP"),
                     plotOutput("WTDE_DF"),
                     plotOutput("WTDE_QQ")),
            tabPanel("Elk", plotOutput("WAPT_MAP"),
                     plotOutput("WAPT_DF"),
                     plotOutput("WAPT_QQ")),
            tabPanel("Power analysis")

  ))
))
