#install and load required packages -----------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("shiny", "RODBC","dplyr","Distance","mrds", "ggplot2", "rgdal",
              "rgeos","dsm","knitr","maptools","gridExtra","sp", "DT", "kableExtra")
ipak(packages)

shinyUI(pageWithSidebar(
  headerPanel("Alberta Aerial Ungulate Survey Population Estimator"),
  sidebarPanel(
    fileInput('MegaDB', 'Step 1. Choose your Access database to commence distance sampling analysis',
              accept=c('.accdb', '.mdb')),
    fileInput('WMU_Shp', 'Step 2. Choose your WMU Polygon Shapefile File (Alberta 10TM) - Note include all shapefile components (i.e. *.shp, *.dbf, *.sbn, etc.)',
              accept=c('.shp','.dbf','.sbn','.sbx', '.shx','.prj','.cpg'), multiple=TRUE),
    sliderInput("truncation", "Step 3: Choose right truncation distance", min=0, max=1000, value=425, step = 25)

  ),
  mainPanel(
    tabsetPanel(id="inTabset",
      tabPanel("Moose",id="Moose",textOutput("MOOS_TXT"), plotOutput("MOOS_MAP"), DT::dataTableOutput('MOOS_TAB'), DT::dataTableOutput("MOOS_TAB1"), plotOutput("MOOS_DF"),plotOutput("MOOS_QQ")),
      tabPanel("Mule Deer", id="Mule Deer", textOutput("MUDE_TXT"), plotOutput("MUDE_MAP"), DT::dataTableOutput('MUDE_TAB'), plotOutput("MD_DF"),plotOutput("MUDE_QQ")),
      tabPanel("White-tailed Deer", id="White-tailed Deer", textOutput("WTDE_TXT"),  plotOutput("WTDE_MAP"), DT::dataTableOutput("WTDE_TAB"), plotOutput("WTDE_DF"), plotOutput("WTDE_QQ")),
      tabPanel("Elk", id="Elk", textOutput("WAPT_TXT"), plotOutput("WAPT_MAP"),DT::dataTableOutput("WAPT_TAB"), plotOutput("WAPT_DF"), plotOutput("WAPT_QQ")),
      tabPanel("Power analysis")

    ))
))