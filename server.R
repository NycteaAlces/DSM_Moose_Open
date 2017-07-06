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

#Test passing variables

#DB <- input$file1
#channel <- odbcConnectAccess(input$MegaDB)
#print(head(channel))
#odbcCloseAll()

#########################################

shinyServer(function(input, output) {
    output$contents <- renderTable({

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



    output$myplot <- renderPlot({
     plot(ddf.1.moos, main=("Global detection function for moose, HN-Cos, no truncation"))

   })

  output$myplot2 <- renderPlot({
    plot(1:100, 1:100)
 })


 })
    })