#install and load required packages -----------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("shiny", "RODBC","dplyr","Distance",
  "mrds", "ggplot2", "rgdal",
  "rgeos","dsm","knitr","maptools","gridExtra")

ipak(packages)

ui <- fluidPage(
    verbatimTextOutput(("debug"))
)

shinyServer(function(input, output,session) {

#Prepare the user-input slots -- dynamic/reactive
DB <- reactive(input$MegaDB$datapath)
GIS <- reactive(input$WMU_Shp)
truncvalue <- reactive(as.double(input$truncation[1]))
    
    ###########################################################
    ###########################################################
    ### Create the general moose detection function
    ###########################################################
    ###########################################################
  output$debug <- renderPrint({
    sessionInfo()
  })
  
  


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


    ddf.1.moos <- ds(DistanceInput2, key="hn", adjustment = "cos", truncation = list(left=0, right=truncvalue()))
    plot(ddf.1.moos, main=paste("Global detection function for moose, HN-Cos, truncation=",truncvalue()))
    # MOOS QQ-plot ----

    output$MOOS_QQ <- renderPlot({
      ddf.gof(ddf.1.moos$ddf)
    })
   })


    ###########################################################
    ###########################################################
    ### Plot a map of the moose observations
    ###########################################################
    ###########################################################


  output$myplot2 <- renderPlot({
    
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
#################################Create lines geometry for the transects
    trans.flown <- sqlFetch(myconn, "transects_flown")
    trans.all <- sqlFetch(myconn, "transects")
    close(myconn)    
    trans.flown <- merge(trans.all, trans.flown, by.x="UniqueID", by.y = "Transect ID")
  from.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
  #  to.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$TO_X, "y"=trans.flown$TO_Y))
    f <- as.data.frame(cbind(X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
    t <- as.data.frame(cbind(X =trans.flown$TO_X, "y"=trans.flown$TO_Y))   

    
  l <- vector("list", nrow(from.coords))
  library(sp)
  for (i in seq_along(l)){
    l[[i]] <- Lines(list(Line(rbind(f[i, ], t[i, ]))), as.character(i))
    }
   trans.flown.splat <- SpatialLines(l)
   trans.flown.splat.df <- SpatialLinesDataFrame(sl = trans.flown.splat,data = from.coords)
    
    

    
GetShapefile <- function(InShapefile, OutShapefile){
    if (is.null(InShapefile)) 
        return(NULL)  
    dir<-dirname(InShapefile[1,4])
      print(paste("Directory name:",dir))
    for ( i in 1:nrow(InShapefile)) {
    file.rename(InShapefile[i,4], paste0(dir,"/",InShapefile[i,1]))}
    OutShapefile <- grep(list.files(dir, pattern="*.shp", full.names=TRUE), pattern="*.xml", inv=T, value=T)
     }
      
      
     
      
    survey.area359.TTM <- readOGR(GetShapefile(input$WMU_Shp), substr(basename(GetShapefile(input$WMU_Shp)),1,nchar(basename(GetShapefile(input$WMU_Shp)))-4))
    #survey.area359.TTM <- readOGR(GetShapefile(input$WMU_Shp), substr(basename(GetShapefile(input$WMU_Shp)),1,nchar(basename(GetShapefile(input$WMU_Shp)))-4))
    #survey.areanon355 <- readOGR(dsn=StrataPolyLayerFile, layer=substr(basename(StrataPolyLayerFile),1,nchar(basename(StrataPolyLayerFile))-4))
    # transects <- reactive(input$TransFlown_Shp)
    #survey.transects359.TTM <- readOGR(GetShapefile2(input$TransFlown_Shp), substr(basename(GetShapefile2(input$TransFlown_Shp)),1,nchar(basename(GetShapefile2(input$TransFlown_Shp))-4)))


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
    p <- p + geom_path(aes(x=long,y=lat,group=group), data = trans.flown.splat.df, colour = "gray" )
    p <- p + geom_point(data = m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
    p <- p + labs(fill = "MDSTRATA", x = "Easting (10TM AEP Forest)", y = "Northing (10TM AEP Forest)")
    p <- p + geom_point(aes(x=))


      plot(p)



 })
    
      output$MOOS_TXT <- renderText({paste("The survey included", round(OL()$ddf.1.moos$dht$individuals$summary$Effort[1],1), "km of transects (n= ", nrow(OL()$transflown), " mean transect length = ",                  #---------------                                      round(OL()$ddf.1.moos$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),"km) that were sampled across a total of ", OL()$strat_num,                                      "strata. There were an estimated ", round(OL()$ddf.1.moos$dht$individuals$N$Estimate[1]*1000, 0),                                      " moose  (CV = ", round(OL()$ddf.1.moos$dht$individuals$N$cv[1], 2)," Confidence interval = ",                                      round(OL()$ddf.1.moos$dht$individuals$N$lcl[1]*1000, 0)," - ", round(OL()$ddf.1.moos$dht$individuals$N$ucl[1]*1000, 0),                                      ") within the study area. In total, ", OL()$MOOS_n," moose were observed in ",  OL()$ddf.1.moos$dht$clusters$summary$n[1],                                      " groups during the survey (sampling fraction = ", round(OL()$MOOS_n/(OL()$ddf.1.moos$dht$individuals$N$Estimate[1]*1000)*100,1),                                      "%). The unadjusted  observed calf ratio and bull ratio (i.e. not corrected for effort between strata) were ", round(OL()$Calf_ratio, 2), " and ",                                      round(OL()$Bull_ratio, 2), " , respectively. Of the bulls observed, ", round((sum(OL()$datasheet$MOOS.Bull.N)/OL()$Bull_n)*100,1),                                      "% had already shed their antlers. Of those bulls still with antlers, ",                                      round((sum(OL()$datasheet$MOOS.Bull.S)/(OL()$Bull_n - sum(OL()$datasheet$MOOS.Bull.N)))*100,1), "% were small,",                                      round((sum(OL()$datasheet$MOOS.Bull.M)/(OL()$Bull_n - sum(OL()$datasheet$MOOS.Bull.N)))*100,1), "% were medium, and ",                                      round((sum(OL()$datasheet$MOOS.Bull.L)/(OL()$Bull_n - sum(OL()$datasheet$MOOS.Bull.N)))*100,1), "% were large. In addition to moose, ",                                      OL()$WTD_n, " White-tailed deer, ", OL()$MUDE_n, " Mule Deer, and ", OL()$WAPT_n," elk were observed during the survey. The observed sample sizes for these species were not deemed sufficient to provide a reliable estimate of abundance.")})

    
    ###########################################################
    ###########################################################
    ### Plot mule deer detection function
    ###########################################################
    ###########################################################    
    output$myplot3 <- renderPlot({
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
    transflown$MUDE.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$MUDE.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.MUDE.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.MUDE.2 <- unique(DistancePreInput.MUDE.2)


    DistancePreInput.MUDE <- merge(datasheet.2, DistancePreInput.MUDE.2, all=T)
    DistancePreInput.MUDE <- unique(DistancePreInput.MUDE)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.MUDE$ID), Region.Label= DistancePreInput.MUDE$Stratum,Area = as.numeric(DistancePreInput.MUDE$Stratum.Area), TID = as.numeric(DistancePreInput.MUDE$Transect.ID), TLENGTH = as.numeric(DistancePreInput.MUDE$Transect.Length), Effort=as.numeric(DistancePreInput.MUDE$Length)/1000, distance= as.numeric(DistancePreInput.MUDE$DistancePerp), size=as.numeric(DistancePreInput.MUDE$MUDE.GroupSize),CC=as.factor(DistancePreInput.MUDE$Covariate.1), Activity=as.factor(DistancePreInput.MUDE$Covariate.2)))

    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]
    trans.flown <- sqlFetch(myconn, "transects_flown")
    trans.all <- sqlFetch(myconn, "transects")
    close(myconn)    
    trans.flown <- merge(trans.all, trans.flown, by.x="UniqueID", by.y = "Transect ID")
    from.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
  #  to.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$TO_X, "y"=trans.flown$TO_Y))
    f <- as.data.frame(cbind(X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
    t <- as.data.frame(cbind(X =trans.flown$TO_X, "y"=trans.flown$TO_Y))   
  

    DistanceInput2 <- as.data.frame(cbind(object = as.numeric(DistancePreInput.MUDE$ID), Region.Label= DistancePreInput.MUDE$Stratum,Area = as.numeric(DistancePreInput.MUDE$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.MUDE$Transect.ID), Effort = as.numeric(DistancePreInput.MUDE$Transect.Length), distance= as.numeric(DistancePreInput.MUDE$DistancePerp), size=as.numeric(DistancePreInput.MUDE$MUDE.GroupSize),CC=as.factor(DistancePreInput.MUDE$Covariate.1), Activity=as.factor(DistancePreInput.MUDE$Covariate.2)))
    DistanceInput2 <- unique(DistanceInput2)
    model2 <- ddf(method="ds", data=DistanceInput2, dsmodel = ~cds(key="hn"), meta.data=list(width=425))
    ddf.1.mude <- ds(DistanceInput2, key="hn", adjustment = "cos", truncation = 425)
    plot(ddf.1.mude, main=("Global detection function for mule deer, HN-Cos, no truncation"))
      # MUDE QQ-plots ----
    output$MUDE_QQ <- renderPlot({ ddf.gof(ddf.1.mude$ddf) })      
        
        })
 
#####################################################################
#MUDE Map
#####################################################################   
output$MUDE_MAP <- renderPlot({
    inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
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
    transflown$WTDE.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$MUDE.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.MUDE.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.MUDE.2 <- unique(DistancePreInput.MUDE.2)


    DistancePreInput.MUDE <- merge(datasheet.2, DistancePreInput.MUDE.2, all=T)
    DistancePreInput.MUDE <- unique(DistancePreInput.MUDE)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.MUDE$ID), Region.Label= DistancePreInput.MUDE$Stratum,Area = as.numeric(DistancePreInput.MUDE$Stratum.Area), TID = as.numeric(DistancePreInput.MUDE$Transect.ID), TLENGTH = as.numeric(DistancePreInput.MUDE$Transect.Length), Effort=as.numeric(DistancePreInput.MUDE$Length)/1000, distance= as.numeric(DistancePreInput.MUDE$DistancePerp), size=as.numeric(DistancePreInput.MUDE$MUDE.GroupSize),CC=as.factor(DistancePreInput.MUDE$Covariate.1), Activity=as.factor(DistancePreInput.MUDE$Covariate.2)))

    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]
   trans.flown <- sqlFetch(myconn, "transects_flown")
    trans.all <- sqlFetch(myconn, "transects")
    close(myconn)    
    trans.flown <- merge(trans.all, trans.flown, by.x="UniqueID", by.y = "Transect ID")
  from.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
  #  to.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$TO_X, "y"=trans.flown$TO_Y))
    f <- as.data.frame(cbind(X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
    t <- as.data.frame(cbind(X =trans.flown$TO_X, "y"=trans.flown$TO_Y))   
    
    
  l <- vector("list", nrow(from.coords))
  library(sp)
  for (i in seq_along(l)){
    l[[i]] <- Lines(list(Line(rbind(f[i, ], t[i, ]))), as.character(i))
    }
   trans.flown.splat <- SpatialLines(l)
   trans.flown.splat.df <- SpatialLinesDataFrame(sl = trans.flown.splat,data = from.coords)
    
    


GetShapefile <- function(InShapefile, OutShapefile){
    if (is.null(InShapefile)) 
        return(NULL)  
    dir<-dirname(InShapefile[1,4])
      print(paste("Directory name:",dir))
    for ( i in 1:nrow(InShapefile)) {
    file.rename(InShapefile[i,4], paste0(dir,"/",InShapefile[i,1]))}
    OutShapefile <- grep(list.files(dir, pattern="*.shp", full.names=TRUE), pattern="*.xml", inv=T, value=T)
     }
      
      
     
      
    survey.area359.TTM <- readOGR(GetShapefile(input$WMU_Shp), substr(basename(GetShapefile(input$WMU_Shp)),1,nchar(basename(GetShapefile(input$WMU_Shp)))-4))
    #survey.area359.TTM <- readOGR(GetShapefile(input$WMU_Shp), substr(basename(GetShapefile(input$WMU_Shp)),1,nchar(basename(GetShapefile(input$WMU_Shp)))-4))
    #survey.areanon355 <- readOGR(dsn=StrataPolyLayerFile, layer=substr(basename(StrataPolyLayerFile),1,nchar(basename(StrataPolyLayerFile))-4))
    # transects <- reactive(input$TransFlown_Shp)
    #survey.transects359.TTM <- readOGR(GetShapefile2(input$TransFlown_Shp), substr(basename(GetShapefile2(input$TransFlown_Shp)),1,nchar(basename(GetShapefile2(input$TransFlown_Shp))-4)))


    obs.table.MUDE <- data.frame(cbind(object = DistanceInput$object.ID, Region.Label = DistanceInput$Region.Label, Sample.Label = DistanceInput$TID, distance = DistanceInput$distance, size = DistanceInput$size))

    m1 <- merge(obs.table.MUDE, DistancePreInput.MUDE, by.x = "object", by.y = "ID")

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
    p <- p + geom_path(aes(x=long,y=lat,group=group), data = trans.flown.splat.df, colour = "gray" )
    #p <- p + geom_line(aes(x=long,y=lat,group=group), data = survey.transects359.TTM, colour = "gray" )
    p <- p + geom_point(data = m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
    p <- p + labs(fill = "MDSTRATA", x = "Easting (10TM AEP Forest)", y = "Northing (10TM AEP Forest)")
    p <- p + geom_point(aes(x=))


      plot(p)
    })
    
    
 ####################################################
 ###### WTDE Map
 ####################################################
    
    output$WTDE_MAP <- renderPlot({
    
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
    transflown$WTDE.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$WTDE.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.WTDE.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.WTDE.2 <- unique(DistancePreInput.WTDE.2)


    DistancePreInput.WTDE <- merge(datasheet.2, DistancePreInput.WTDE.2, all=T)
    DistancePreInput.WTDE <- unique(DistancePreInput.WTDE)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.WTDE$ID), Region.Label= DistancePreInput.WTDE$Stratum,Area = as.numeric(DistancePreInput.WTDE$Stratum.Area), TID = as.numeric(DistancePreInput.WTDE$Transect.ID), TLENGTH = as.numeric(DistancePreInput.WTDE$Transect.Length), Effort=as.numeric(DistancePreInput.WTDE$Length)/1000, distance= as.numeric(DistancePreInput.WTDE$DistancePerp), size=as.numeric(DistancePreInput.WTDE$WTDE.GroupSize),CC=as.factor(DistancePreInput.WTDE$Covariate.1), Activity=as.factor(DistancePreInput.WTDE$Covariate.2)))

    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]
   trans.flown <- sqlFetch(myconn, "transects_flown")
    trans.all <- sqlFetch(myconn, "transects")
    close(myconn)    
    trans.flown <- merge(trans.all, trans.flown, by.x="UniqueID", by.y = "Transect ID")
  from.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
  #  to.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$TO_X, "y"=trans.flown$TO_Y))
    f <- as.data.frame(cbind(X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
    t <- as.data.frame(cbind(X =trans.flown$TO_X, "y"=trans.flown$TO_Y))   
  
    
  l <- vector("list", nrow(from.coords))
  library(sp)
  for (i in seq_along(l)){
    l[[i]] <- Lines(list(Line(rbind(f[i, ], t[i, ]))), as.character(i))
    }
   trans.flown.splat <- SpatialLines(l)
   trans.flown.splat.df <- SpatialLinesDataFrame(sl = trans.flown.splat,data = from.coords)
    
    

GetShapefile <- function(InShapefile, OutShapefile){
    if (is.null(InShapefile)) 
        return(NULL)  
    dir<-dirname(InShapefile[1,4])
      print(paste("Directory name:",dir))
    for ( i in 1:nrow(InShapefile)) {
    file.rename(InShapefile[i,4], paste0(dir,"/",InShapefile[i,1]))}
    OutShapefile <- grep(list.files(dir, pattern="*.shp", full.names=TRUE), pattern="*.xml", inv=T, value=T)
     }
      
      
     
      
    survey.area359.TTM <- readOGR(GetShapefile(input$WMU_Shp), substr(basename(GetShapefile(input$WMU_Shp)),1,nchar(basename(GetShapefile(input$WMU_Shp)))-4))
    #survey.area359.TTM <- readOGR(GetShapefile(input$WMU_Shp), substr(basename(GetShapefile(input$WMU_Shp)),1,nchar(basename(GetShapefile(input$WMU_Shp)))-4))
    #survey.areanon355 <- readOGR(dsn=StrataPolyLayerFile, layer=substr(basename(StrataPolyLayerFile),1,nchar(basename(StrataPolyLayerFile))-4))
    # transects <- reactive(input$TransFlown_Shp)
    #survey.transects359.TTM <- readOGR(GetShapefile2(input$TransFlown_Shp), substr(basename(GetShapefile2(input$TransFlown_Shp)),1,nchar(basename(GetShapefile2(input$TransFlown_Shp))-4)))


    obs.table.WTDE <- data.frame(cbind(object = DistanceInput$object.ID, Region.Label = DistanceInput$Region.Label, Sample.Label = DistanceInput$TID, distance = DistanceInput$distance, size = DistanceInput$size))

    m1 <- merge(obs.table.WTDE, DistancePreInput.WTDE, by.x = "object", by.y = "ID")

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
    p <- p + geom_path(aes(x=long,y=lat,group=group), data = trans.flown.splat.df, colour = "gray" )
    #p <- p + geom_line(aes(x=long,y=lat,group=group), data = survey.transects359.TTM, colour = "gray" )
    p <- p + geom_point(data = m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
    p <- p + labs(fill = "MDSTRATA", x = "Easting (10TM AEP Forest)", y = "Northing (10TM AEP Forest)")
    p <- p + geom_point(aes(x=))


      plot(p)
    })
    
    
################################################
####WTDE DF
###################################################
  
    output$WTDE_DF <- renderPlot({
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
    transflown$WTDE.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$WTDE.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.WTDE.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.WTDE.2 <- unique(DistancePreInput.WTDE.2)


    DistancePreInput.WTDE <- merge(datasheet.2, DistancePreInput.WTDE.2, all=T)
    DistancePreInput.WTDE <- unique(DistancePreInput.WTDE)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.WTDE$ID), Region.Label= DistancePreInput.WTDE$Stratum,Area = as.numeric(DistancePreInput.WTDE$Stratum.Area), TID = as.numeric(DistancePreInput.WTDE$Transect.ID), TLENGTH = as.numeric(DistancePreInput.WTDE$Transect.Length), Effort=as.numeric(DistancePreInput.WTDE$Length)/1000, distance= as.numeric(DistancePreInput.WTDE$DistancePerp), size=as.numeric(DistancePreInput.WTDE$WTDE.GroupSize),CC=as.factor(DistancePreInput.WTDE$Covariate.1), Activity=as.factor(DistancePreInput.WTDE$Covariate.2)))

    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]


    close(myconn)
    

    DistanceInput2 <- as.data.frame(cbind(object = as.numeric(DistancePreInput.WTDE$ID), Region.Label= DistancePreInput.WTDE$Stratum,Area = as.numeric(DistancePreInput.WTDE$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.WTDE$Transect.ID), Effort = as.numeric(DistancePreInput.WTDE$Transect.Length), distance= as.numeric(DistancePreInput.WTDE$DistancePerp), size=as.numeric(DistancePreInput.WTDE$WTDE.GroupSize),CC=as.factor(DistancePreInput.WTDE$Covariate.1), Activity=as.factor(DistancePreInput.WTDE$Covariate.2)))

    DistanceInput2 <- unique(DistanceInput2)


    model3 <- ddf(method="ds", data=DistanceInput2, dsmodel = ~cds(key="hn"), meta.data=list(width=425))
    ddf.1.wtde <- ds(DistanceInput2, key="hn", adjustment = "cos", truncation = 425)

    plot(ddf.1.wtde, main=("Global detection function for white-tailed deer, HN-Cos, no truncation"))
     # WTDE QQ-plots ----
    output$WTDE_QQ <- renderPlot({
      ddf.gof(ddf.1.wtde$ddf)
    })
        
        })

    
###########################################################
###########################################################
### Plot elk detection function
###########################################################
###########################################################    
    output$WAPT_DF <- renderPlot({
   
        inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
        print(inFile)
        if (is.null(inFile))
          return(NULL)
        
        DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", inFile)
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
        transflown$WAPT.GroupSize <- " "
        transflown$Covariate.1 <- " "
        transflown$Covariate.2 <- " "
        transflown <- unique(transflown)

        datasheet.2 <- datasheet[ which(datasheet$WAPT.GroupSize >0),]
        datasheet.2 <- unique(datasheet.2)
        DistancePreInput.WAPT.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
        DistancePreInput.WAPT.2 <- unique(DistancePreInput.WAPT.2)

        DistancePreInput.WAPT <- merge(datasheet.2, DistancePreInput.WAPT.2, all=T)
        DistancePreInput.WAPT <- unique(DistancePreInput.WAPT)
        DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.WAPT$ID), Region.Label= DistancePreInput.WAPT$Stratum,Area = as.numeric(DistancePreInput.WAPT$Stratum.Area), TID = as.numeric(DistancePreInput.WAPT$Transect.ID), TLENGTH = as.numeric(DistancePreInput.WAPT$Transect.Length), Effort=as.numeric(DistancePreInput.WAPT$Length)/1000, distance= as.numeric(DistancePreInput.WAPT$DistancePerp), size=as.numeric(DistancePreInput.WAPT$WAPT.GroupSize),CC=as.factor(DistancePreInput.WAPT$Covariate.1), Activity=as.factor(DistancePreInput.WAPT$Covariate.2)))
        DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]
        close(myconn)

        DistanceInput2 <- as.data.frame(cbind(object = as.numeric(DistancePreInput.WAPT$ID), Region.Label= DistancePreInput.WAPT$Stratum,Area = as.numeric(DistancePreInput.WAPT$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.WAPT$Transect.ID), Effort = as.numeric(DistancePreInput.WAPT$Transect.Length), distance= as.numeric(DistancePreInput.WAPT$DistancePerp), size=as.numeric(DistancePreInput.WAPT$WAPT.GroupSize),CC=as.factor(DistancePreInput.WAPT$Covariate.1), Activity=as.factor(DistancePreInput.WAPT$Covariate.2)))
        DistanceInput2 <- unique(DistanceInput2)
        ddf.1.wapt <- ds(DistanceInput2, key="hn", adjustment = "cos", truncation = 425, debug.level = 3)
      
      plot(ddf.1.wapt, main=("Global detection function for elk, HN-Cos, no truncation"))             
       
      
      # WAPT QQ-plots ----
        output$WAPT_QQ <- renderPlot({
          ddf.gof(ddf.1.wapt$ddf)
           })
    })
 
#####################################################################
#WAPT Map
#####################################################################   
        
    output$WAPT_MAP <- renderPlot({
   
      inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
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
    transflown$WTDE.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$WAPT.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.WAPT.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.WAPT.2 <- unique(DistancePreInput.WAPT.2)


    DistancePreInput.WAPT <- merge(datasheet.2, DistancePreInput.WAPT.2, all=T)
    DistancePreInput.WAPT <- unique(DistancePreInput.WAPT)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.WAPT$ID), Region.Label= DistancePreInput.WAPT$Stratum,Area = as.numeric(DistancePreInput.WAPT$Stratum.Area), TID = as.numeric(DistancePreInput.WAPT$Transect.ID), TLENGTH = as.numeric(DistancePreInput.WAPT$Transect.Length), Effort=as.numeric(DistancePreInput.WAPT$Length)/1000, distance= as.numeric(DistancePreInput.WAPT$DistancePerp), size=as.numeric(DistancePreInput.WAPT$WAPT.GroupSize),CC=as.factor(DistancePreInput.WAPT$Covariate.1), Activity=as.factor(DistancePreInput.WAPT$Covariate.2)))

    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]

   trans.flown <- sqlFetch(myconn, "transects_flown")
    trans.all <- sqlFetch(myconn, "transects")
    close(myconn)    
    trans.flown <- merge(trans.all, trans.flown, by.x="UniqueID", by.y = "Transect ID")
  from.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
  #  to.coords <- as.data.frame(cbind(TID =trans.flown$UniqueID,X =trans.flown$TO_X, "y"=trans.flown$TO_Y))
    f <- as.data.frame(cbind(X =trans.flown$FROM_X, "y"=trans.flown$FROM_Y))
    t <- as.data.frame(cbind(X =trans.flown$TO_X, "y"=trans.flown$TO_Y))   
  
    
  l <- vector("list", nrow(from.coords))
  library(sp)
  for (i in seq_along(l)){
    l[[i]] <- Lines(list(Line(rbind(f[i, ], t[i, ]))), as.character(i))
    }
   trans.flown.splat <- SpatialLines(l)
   trans.flown.splat.df <- SpatialLinesDataFrame(sl = trans.flown.splat,data = from.coords)
    
    

    GetShapefile <- function(InShapefile, OutShapefile){
        if (is.null(InShapefile)) 
            return(NULL)  
        dir<-dirname(InShapefile[1,4])
          print(paste("Directory name:",dir))
        for ( i in 1:nrow(InShapefile)) {
        file.rename(InShapefile[i,4], paste0(dir,"/",InShapefile[i,1]))}
        OutShapefile <- grep(list.files(dir, pattern="*.shp", full.names=TRUE), pattern="*.xml", inv=T, value=T)
         }
      
      
     
      
    survey.area359.TTM <- readOGR(GetShapefile(input$WMU_Shp), substr(basename(GetShapefile(input$WMU_Shp)),1,nchar(basename(GetShapefile(input$WMU_Shp)))-4))
    obs.table.WAPT <- data.frame(cbind(object = DistanceInput$object.ID, Region.Label = DistanceInput$Region.Label, Sample.Label = DistanceInput$TID, distance = DistanceInput$distance, size = DistanceInput$size))

    m1 <- merge(obs.table.WAPT, DistancePreInput.WAPT, by.x = "object", by.y = "ID")

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
    p <- p + geom_path(aes(x=long,y=lat,group=group), data = trans.flown.splat.df, colour = "gray" )
    #p <- p + geom_line(aes(x=long,y=lat,group=group), data = survey.transects359.TTM, colour = "gray" )
    p <- p + geom_point(data = m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
    p <- p + labs(fill = "MDSTRATA", x = "Easting (10TM AEP Forest)", y = "Northing (10TM AEP Forest)")
    p <- p + geom_point(aes(x=))


      plot(p)
    })
    
  
    
    
    
    
    
    })
