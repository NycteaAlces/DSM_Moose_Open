library(shiny)
library(datasets)


library(RODBC)
require(RODBC)
require(dplyr)
require(Distance)
require(mrds)
library(ggplot2)
library(rgdal)
library(rgeos)
library(dsm)
library(knitr)
library(maptools)
library(gridExtra)

ui <- fluidPage(
    verbatimTextOutput(("debug"))
)

shinyServer(function(input, output,session) {

  output$debug <- renderPrint({
    sessionInfo()
  })


DB <- reactive(input$MegaDB$datapath)
GIS <- reactive(input$WMU_Shp$datapath[1])

  output$myplot <- renderPlot({

    # input$file1 will be NULL initially. After the user selects and uploads a
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
    # columns. The 'datapath' column will contain the local filenames where the
    # data can be found.

    inFile <- input$MegaDB$datapath  #User input -- Get the Access database pathname

    if (is.null(inFile))
      return(NULL)
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")
    strat_num <- nrow(strat)
    results_num_index <- as.integer(strat_num) + 1

    datasheet <- as.data.frame(sqlQuery(myconn, "select * from datasheet"))

    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))



    transflown <- datasheet[!duplicated(datasheet[, c("Transect.ID", "Stratum")]), ]
    transflown <- transflown[!is.na(transflown$Stratum),]
    transflown$DistancePerp <- " "
    transflown$MOOS.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$MOOS.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.MOOS.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.MOOS.2 <- unique(DistancePreInput.MOOS.2)


    DistancePreInput.MOOS <- merge(datasheet.2, DistancePreInput.MOOS.2, all=T)
    DistancePreInput.MOOS <- unique(DistancePreInput.MOOS)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.MOOS$ID), Region.Label= DistancePreInput.MOOS$Stratum,Area = as.numeric(DistancePreInput.MOOS$Stratum.Area), TID = as.numeric(DistancePreInput.MOOS$Transect.ID), TLENGTH = as.numeric(DistancePreInput.MOOS$Transect.Length), Effort=as.numeric(DistancePreInput.MOOS$Length)/1000, distance= as.numeric(DistancePreInput.MOOS$DistancePerp), size=as.numeric(DistancePreInput.MOOS$MOOS.GroupSize),CC=as.factor(DistancePreInput.MOOS$Covariate.1), Activity=as.factor(DistancePreInput.MOOS$Covariate.2)))

    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]


    close(myconn)

    DistanceInput2 <- as.data.frame(cbind(object = as.numeric(DistancePreInput.MOOS$ID), Region.Label= DistancePreInput.MOOS$Stratum,Area = as.numeric(DistancePreInput.MOOS$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.MOOS$Transect.ID), Effort = as.numeric(DistancePreInput.MOOS$Transect.Length), distance= as.numeric(DistancePreInput.MOOS$DistancePerp), size=as.numeric(DistancePreInput.MOOS$MOOS.GroupSize),CC=as.factor(DistancePreInput.MOOS$Covariate.1), Activity=as.factor(DistancePreInput.MOOS$Covariate.2)))

    DistanceInput2 <- unique(DistanceInput2)


    model1 <- ddf(method="ds", data=DistanceInput2, dsmodel = ~cds(key="hn"), meta.data=list(width=425))
    ddf.1.moos <- ds(DistanceInput2, key="hn", adjustment = "cos", truncation = 425)




     plot(ddf.1.moos, main=("Global detection function for moose, HN-Cos, no truncation"))

   })





  output$myplot2 <- renderPlot({
####################################################################
      #Simply create the dataframe with distance data for mapping
#####################################################################      
      inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
     # print(inFile)
    if (is.null(inFile))
      return(NULL)
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")
    strat_num <- nrow(strat)
    results_num_index <- as.integer(strat_num) + 1

    datasheet <- as.data.frame(sqlQuery(myconn, "select * from datasheet"))

    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))



    transflown <- datasheet[!duplicated(datasheet[, c("Transect.ID", "Stratum")]), ]
    transflown <- transflown[!is.na(transflown$Stratum),]
    transflown$DistancePerp <- " "
    transflown$MOOS.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$MOOS.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.MOOS.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.MOOS.2 <- unique(DistancePreInput.MOOS.2)


    DistancePreInput.MOOS <- merge(datasheet.2, DistancePreInput.MOOS.2, all=T)
    DistancePreInput.MOOS <- unique(DistancePreInput.MOOS)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.MOOS$ID), Region.Label= DistancePreInput.MOOS$Stratum,Area = as.numeric(DistancePreInput.MOOS$Stratum.Area), TID = as.numeric(DistancePreInput.MOOS$Transect.ID), TLENGTH = as.numeric(DistancePreInput.MOOS$Transect.Length), Effort=as.numeric(DistancePreInput.MOOS$Length)/1000, distance= as.numeric(DistancePreInput.MOOS$DistancePerp), size=as.numeric(DistancePreInput.MOOS$MOOS.GroupSize),CC=as.factor(DistancePreInput.MOOS$Covariate.1), Activity=as.factor(DistancePreInput.MOOS$Covariate.2)))

    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]


    close(myconn)
      
####################################################################
      #Simply create the dataframe with distance data for mapping
#####################################################################   
    # WMUPolyLayerFile <- input$WMU_Shp$datapath
    # StrataPolyLayerFile <-  input$Strata_Shp$datapath #User input -- Get the Strata shapefile
    # PolyLineTransflown <- input$TransFlown_Shp$datapath #User input -- Get the transects shapefile

    GISInput <- GIS()
    print(paste("GIS Input=", GISInput)) #input$WMU_Shp$datapath[1])
    # GISInput <- "F:/GIS_Workspace/R_Files/A_359_Boundary_TTM.shp"

    survey.area359.TTM <- readOGR(input$WMU_Shp$datapath[1], substr(basename(GISInput),1,nchar(basename(GISInput))-4))
    # survey.areanon355 <- readOGR(dsn=StrataPolyLayerFile, layer=substr(basename(StrataPolyLayerFile),1,nchar(basename(StrataPolyLayerFile))-4))
    # survey.transects359.TTM <- readOGR(dsn=PolyLineTransflown, layer=substr(basename(PolyLineTransflown),1,nchar(basename(PolyLineTransflown))-4))


    obs.table.MOOS <- data.frame(cbind(object = DistanceInput$object.ID, Region.Label = DistanceInput$Region.Label, Sample.Label = DistanceInput$TID, distance = DistanceInput$distance, size = DistanceInput$size))

    m1 <- merge(obs.table.MOOS, DistancePreInput.MOOS, by.x = "object", by.y = "ID")

    grid_plot_obj <- function(fill, name, sp){


      names(fill) <- NULL
      row.names(fill) <- NULL
      data <- data.frame(fill)
      names(data) <- name

      spdf <- SpatialPolygonsDataFrame(sp, data)
      spdf@data$id <- rownames(spdf@data)
      spdf.points <- fortify(spdf, region="id")
      spdf.df <- join(spdf.points, spdf@data, by="id")


      spdf.df$x <- spdf.df$long
      spdf.df$y <- spdf.df$lat

      geom_polygon(aes_string(x="x",y="y",fill=name, group="group"), data=spdf.df)
    }


    p <- ggplot ()
    p <- p + geom_polygon(data = survey.area359.TTM, fill="light blue", aes(x=long, y=lat, group=group)) + coord_equal()
    #p <- p + geom_polygon(data = survey.areanon355, fill="khaki", aes(x=long, y=lat, group=group)) + coord_equal()

    # p <- p + geom_line(aes(x=long,y=lat,group=group), data = survey.transects359.TTM, colour = "gray" )
    p <- p + geom_point(data = m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
    p <- p + labs(fill = "MDSTRATA", x = "Easting (10TM AEP Forest)", y = "Northing (10TM AEP Forest)")
    p <- p + geom_point(aes(x=))


      plot(p)



 })
    })
