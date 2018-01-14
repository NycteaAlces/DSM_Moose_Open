ui <- fluidPage(verbatimTextOutput(("debug")))

shinyServer(function(input, output,session) {#----

  #Prepare the user-input slots -- dynamic/reactive
  DB <- reactive(input$MegaDB$datapath)
 # GIS <- reactive(input$WMU_Shp)

  #truncvalue <- reactive(as.double(input$truncation[1]))

  truncvalue <- reactive({
    if(is.null(input$truncation))
      return(425)
    else
      return(as.double(input$truncation))
  })

  output$debug <- renderPrint({
    sessionInfo()
  })

  #Function to generate multiple DF plots for each aircraft and species
  get_plot_output_list <- function(U.list) {
    # Insert plot output objects the list
    plot_output_list <- lapply(1:length(U.list), function(i) {
      plotname <- paste("QQplot", i, sep="")
      plot_output_object <- plotOutput(plotname, height = 280, width = 250)
      plot_output_object <- renderPlot({plot(ds(U.list[[i]], key="hn", adjustment = "cos", truncation = 425), main= paste("Detection function for aircraft:", unique(U.list[[i]]$Aircraft)))})
    })

    do.call(tagList, plot_output_list) # needed to display properly.

    return(plot_output_list)
  }

  #Function to clean up model descriptions in summary tables
  fun.getModDescr <- function(model_description) {
    print(paste("DEBUG-in fun.getModeDescr"))
    x<-model_description
    key = substr(x, as.integer((gregexpr("key = \"", x)[[1]][1]))+7, as.integer(regexpr("\", form", x)[[1]][1])-1) #get key name
    adj = substr(x, as.integer((gregexpr("adj.series = \"", x)[[1]][1]))+14, as.integer(regexpr("\", adj.order", x)[[1]][1])-1) #get adjustement term
    adj.ord = substr(x, as.integer((gregexpr("adj.order = ", x)[[1]][1]))+12, as.integer(regexpr(", adj.scale = \"" , x)[[1]][1])-1) #get adjustment order
    modeldesc_2 <- gsub(" ", "", paste(key, "(", adj, ") - {",adj.ord,"}"), fixed=TRUE) #Make pretty words
    return(as.data.frame(modeldesc_2))
  }

    fun.fixDest <- function(ddf.1.D) {
      print(paste("DEBUG-in fun.fixDest"))
      table <- as.data.frame(ddf.1.D)
      table$Estimate <- table$Estimate * 1000
      table$se<-table$se*1000
      table$lcl<-table$lcl*1000
      table$ucl<-table$ucl*1000
      table <- round(table[,-1],3) #round everything except column 1
      return(table)
    }

  OL <- eventReactive(list(input$MegaDB$datapath, input$truncation), { ####----

    # input$file1 will be NULL initially. After the user selects and uploads a
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
    # columns. The 'datapath' column will contain the local filenames where the
    # data can be found.

    inFile <- input$MegaDB$datapath  #User input -- Get the Access database pathname

    if(is.null(inFile))
      return(NULL)

    #   myconn <- odbcDriverConnect('Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=//GOA/MyDocs/M/Mike.Russell/Analysis/R/Moose_R_Files/359_AUS Distance Survey Data Management Tool_20150223.accdb')
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")


    datasheet <- as.data.frame(sqlQuery(myconn, "select * from datasheet"))
    close(myconn)
    strat_num <- nrow(strat)
    if(strat_num==1){
      results_num_index <- as.integer(strat_num)
    } else {

      results_num_index <- as.integer(strat_num) + 1
    }



    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))
    transflown <- datasheet[!duplicated(datasheet[, c("Transect.ID", "Stratum")]), ]
    transflown <- transflown[!is.na(transflown$Stratum),]
    transflown$DistancePerp <- " "
    transflown$MOOS.GroupSize <- " "
    transflown$MUDE.GroupSize <- " "
    transflown$WTDE.GroupSize <- " "
    transflown$WAPT.GroupSize <- " "
    transflown$HORS.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)

    datasheet.md <- datasheet

    datasheet.2 <- unique(datasheet[ which(datasheet$MOOS.GroupSize >0),])
    DistancePreInput.MOOS.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.MOOS.2 <- unique(DistancePreInput.MOOS.2)
    DistancePreInput.MOOS <- merge(datasheet.2, DistancePreInput.MOOS.2, all=T)
    DistancePreInput.MOOS <- unique(DistancePreInput.MOOS)
    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.MOOS$ID), Region.Label= DistancePreInput.MOOS$Stratum,Area = as.numeric(DistancePreInput.MOOS$Stratum.Area), TID = as.numeric(DistancePreInput.MOOS$Transect.ID), TLENGTH = as.numeric(DistancePreInput.MOOS$Transect.Length), Effort=as.numeric(DistancePreInput.MOOS$Length)/1000, distance= as.numeric(DistancePreInput.MOOS$DistancePerp), size=as.numeric(DistancePreInput.MOOS$MOOS.GroupSize),CC=as.factor(DistancePreInput.MOOS$Covariate.1), Activity=as.factor(DistancePreInput.MOOS$Covariate.2)))
    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]

    datasheet.2.MD <- unique(datasheet.md[ which(datasheet.md$MUDE.GroupSize >0),])
    DistancePreInput.MUDE.2 <- anti_join(transflown, datasheet.2.MD, by=c("Transect.ID","Stratum"))
    DistancePreInput.MUDE.2 <- unique(DistancePreInput.MUDE.2)
    DistancePreInput.MUDE <- merge(datasheet.2.MD, DistancePreInput.MUDE.2, all=T)
    DistancePreInput.MUDE <- unique(DistancePreInput.MUDE)
    DistanceInput.MD<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.MUDE$ID), Region.Label= DistancePreInput.MUDE$Stratum,Area = as.numeric(DistancePreInput.MUDE$Stratum.Area), TID = as.numeric(DistancePreInput.MUDE$Transect.ID), TLENGTH = as.numeric(DistancePreInput.MUDE$Transect.Length), Effort=as.numeric(DistancePreInput.MUDE$Length)/1000, distance= as.numeric(DistancePreInput.MUDE$DistancePerp), size=as.numeric(DistancePreInput.MUDE$MUDE.GroupSize),CC=as.factor(DistancePreInput.MUDE$Covariate.1), Activity=as.factor(DistancePreInput.MUDE$Covariate.2)))
    DistanceInput.MD <- DistanceInput.MD[ order(DistanceInput.MD$Region.Label, DistanceInput.MD$TID, DistanceInput.MD$size), ]

    DistanceInput2 <- as.data.frame(cbind(object = as.numeric(DistancePreInput.MOOS$ID), Region.Label= DistancePreInput.MOOS$Stratum,Area = as.numeric(DistancePreInput.MOOS$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.MOOS$Transect.ID), Effort = as.numeric(DistancePreInput.MOOS$Transect.Length), distance= as.numeric(DistancePreInput.MOOS$DistancePerp), size=as.numeric(DistancePreInput.MOOS$MOOS.GroupSize),CC=as.factor(DistancePreInput.MOOS$Covariate.1), Activity=as.factor(DistancePreInput.MOOS$Covariate.2)))
    DistanceInput2 <- unique(DistanceInput2)
    DistanceInput2.MD <- as.data.frame(cbind(object = as.numeric(DistancePreInput.MUDE$ID), Region.Label= DistancePreInput.MUDE$Stratum,Area = as.numeric(DistancePreInput.MUDE$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.MUDE$Transect.ID), Effort = as.numeric(DistancePreInput.MUDE$Transect.Length), distance= as.numeric(DistancePreInput.MUDE$DistancePerp), size=as.numeric(DistancePreInput.MUDE$MUDE.GroupSize),CC=as.factor(DistancePreInput.MUDE$Covariate.1), Activity=as.factor(DistancePreInput.MUDE$Covariate.2)))
    DistanceInput2.MD <- unique(DistanceInput2.MD)


    datasheet.2.WT <- unique(datasheet.md[ which(datasheet.md$WTDE.GroupSize >0),])
    DistancePreInput.WTDE.2 <- anti_join(transflown, datasheet.2.WT, by=c("Transect.ID","Stratum"))
    DistancePreInput.WTDE.2 <- unique(DistancePreInput.WTDE.2)
    DistancePreInput.WTDE <- merge(datasheet.2.WT, DistancePreInput.WTDE.2, all=T)
    DistancePreInput.WTDE <- unique(DistancePreInput.WTDE)
    DistanceInput.WT<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.WTDE$ID), Region.Label= DistancePreInput.WTDE$Stratum,Area = as.numeric(DistancePreInput.WTDE$Stratum.Area), TID = as.numeric(DistancePreInput.WTDE$Transect.ID), TLENGTH = as.numeric(DistancePreInput.WTDE$Transect.Length), Effort=as.numeric(DistancePreInput.WTDE$Length)/1000, distance= as.numeric(DistancePreInput.WTDE$DistancePerp), size=as.numeric(DistancePreInput.WTDE$WTDE.GroupSize),CC=as.factor(DistancePreInput.WTDE$Covariate.1), Activity=as.factor(DistancePreInput.WTDE$Covariate.2)))
    DistanceInput.WT <- DistanceInput.MD[ order(DistanceInput.WT$Region.Label, DistanceInput.WT$TID, DistanceInput.WT$size), ]
    DistanceInput2.WT <- as.data.frame(cbind(object = as.numeric(DistancePreInput.WTDE$ID), Region.Label= DistancePreInput.WTDE$Stratum,Area = as.numeric(DistancePreInput.WTDE$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.WTDE$Transect.ID), Effort = as.numeric(DistancePreInput.WTDE$Transect.Length), distance= as.numeric(DistancePreInput.WTDE$DistancePerp), size=as.numeric(DistancePreInput.WTDE$WTDE.GroupSize),CC=as.factor(DistancePreInput.WTDE$Covariate.1), Activity=as.factor(DistancePreInput.WTDE$Covariate.2)))
    DistanceInput2.WT <- unique(DistanceInput2.WT)

    datasheet.2.WAPT <- unique(datasheet.md[ which(datasheet.md$WAPT.GroupSize >0),])
    DistancePreInput.WAPT.2 <- anti_join(transflown, datasheet.2.WAPT, by=c("Transect.ID","Stratum"))
    DistancePreInput.WAPT.2 <- unique(DistancePreInput.WAPT.2)
    DistancePreInput.WAPT <- merge(datasheet.2.WAPT, DistancePreInput.WAPT.2, all=T)
    DistancePreInput.WAPT <- unique(DistancePreInput.WAPT)
    DistanceInput.WAPT<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.WAPT$ID), Region.Label= DistancePreInput.WAPT$Stratum,Area = as.numeric(DistancePreInput.WAPT$Stratum.Area), TID = as.numeric(DistancePreInput.WAPT$Transect.ID), TLENGTH = as.numeric(DistancePreInput.WAPT$Transect.Length), Effort=as.numeric(DistancePreInput.WAPT$Length)/1000, distance= as.numeric(DistancePreInput.WAPT$DistancePerp), size=as.numeric(DistancePreInput.WAPT$WAPT.GroupSize),CC=as.factor(DistancePreInput.WAPT$Covariate.1), Activity=as.factor(DistancePreInput.WAPT$Covariate.2)))
    DistanceInput.WAPT <- DistanceInput.WAPT[ order(DistanceInput.WAPT$Region.Label, DistanceInput.WAPT$TID, DistanceInput.WAPT$size), ]
    DistanceInput2.WAPT <- as.data.frame(cbind(object = as.numeric(DistancePreInput.WAPT$ID), Region.Label= DistancePreInput.WAPT$Stratum,Area = as.numeric(DistancePreInput.WAPT$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.WAPT$Transect.ID), Effort = as.numeric(DistancePreInput.WAPT$Transect.Length), distance= as.numeric(DistancePreInput.WAPT$DistancePerp), size=as.numeric(DistancePreInput.WAPT$WAPT.GroupSize),CC=as.factor(DistancePreInput.WAPT$Covariate.1), Activity=as.factor(DistancePreInput.WAPT$Covariate.2)))
    DistanceInput2.WAPT <- unique(DistanceInput2.WAPT)

    datasheet.2.HORS <- unique(datasheet[ which(datasheet$HORS.GroupSize >0),])
    print(head(datasheet.2.HORS))
    DistancePreInput.HORS.2 <- anti_join(transflown, datasheet.2.HORS, by=c("Transect.ID","Stratum"))
    DistancePreInput.HORS.2 <- unique(DistancePreInput.HORS.2)
    DistancePreInput.HORS <- merge(datasheet.2.HORS, DistancePreInput.HORS.2, all=T)
    DistancePreInput.HORS <- unique(DistancePreInput.HORS)
    DistanceInput.HORS<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.HORS$ID), Region.Label= DistancePreInput.HORS$Stratum,Area = as.numeric(DistancePreInput.HORS$Stratum.Area), TID = as.numeric(DistancePreInput.HORS$Transect.ID), TLENGTH = as.numeric(DistancePreInput.HORS$Transect.Length), Effort=as.numeric(DistancePreInput.HORS$Length)/1000, distance= as.numeric(DistancePreInput.HORS$DistancePerp), size=as.numeric(DistancePreInput.HORS$HORS.GroupSize),CC=as.factor(DistancePreInput.HORS$Covariate.1), Activity=as.factor(DistancePreInput.HORS$Covariate.2)))
    DistanceInput.HORS <- DistanceInput.HORS[ order(DistanceInput.HORS$Region.Label, DistanceInput.HORS$TID, DistanceInput.HORS$size), ]
    DistanceInput2.HORS <- as.data.frame(cbind(object = as.numeric(DistancePreInput.HORS$ID), Region.Label= DistancePreInput.HORS$Stratum,Area = as.numeric(DistancePreInput.HORS$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.HORS$Transect.ID), Effort = as.numeric(DistancePreInput.HORS$Transect.Length), distance= as.numeric(DistancePreInput.HORS$DistancePerp), size=as.numeric(DistancePreInput.HORS$HORS.GroupSize),CC=as.factor(DistancePreInput.HORS$Covariate.1), Activity=as.factor(DistancePreInput.HORS$Covariate.2)))
    DistanceInput2.HORS <- unique(DistanceInput2.HORS)
    print(head(DistanceInput2.HORS))
    print(head(DistanceInput2.WAPT))

    #Create aircraft dataset for aircraft-specific plotting
    Copy.df <- DistancePreInput.MOOS
    Copy.df$DistancePerp <- as.numeric(Copy.df$DistancePerp)
    Copy.df$MOOS.GroupSize <- as.numeric(Copy.df$MOOS.GroupSize)
    CleanHistData <- Copy.df[!(Copy.df$DistancePerp==""),]
    CleanHistData <- Copy.df[!is.na(Copy.df$DistancePerp),]
    CleanHistData <- subset(CleanHistData, DistancePerp <= 600)
    CleanHistData["distance"] <- CleanHistData$DistancePerp
    CleanHistData["object"] <- seq.int(nrow(CleanHistData))
    U.list <- split(CleanHistData, CleanHistData$Aircraft)

    Calf_n <- sum(datasheet[!is.na(datasheet$MOOS.GroupSize),]$MOOS.Calf)
    Cow_n  <- sum(datasheet[!is.na(datasheet$MOOS.GroupSize),]$MOOS.Cow)
    Calf_ratio <- Calf_n / Cow_n
    Bull_n <- sum(sum(datasheet[!is.na(datasheet$MOOS.GroupSize),]$MOOS.Bull.N), sum(datasheet[!is.na(datasheet$MOOS.GroupSize),]$MOOS.Bull.S), sum(datasheet[!is.na(datasheet$MOOS.GroupSize),]$MOOS.Bull.M), sum(datasheet[!is.na(datasheet$MOOS.GroupSize),]$MOOS.Bull.L))
    Bull_ratio <- Bull_n / Cow_n
    MOOS_n <- sum(datasheet[!is.na(datasheet$MOOS.GroupSize),]$MOOS.GroupSize)
    MOOS_n2 <- sum(datasheet$MOOS.GroupSize)
    WTD_n  <- sum(datasheet[!is.na(datasheet$WTDE.GroupSize),]$WTDE.GroupSize)
    WTD_n2 <- sum(datasheet$WTDE.GroupSize)
    MUDE_n <- sum(datasheet[!is.na(datasheet$MUDE.GroupSize),]$MUDE.GroupSize)
    MD_n2 <- sum(DistanceInput2.MD$size[!is.na(DistanceInput2$size)]) 
    print(summary(DistanceInput2.MD))
    #MD_n2 <- sum(datasheet$MUDE.GroupSize)
    WAPT_n <- sum(datasheet[!is.na(datasheet$WAPT.GroupSize),]$WAPT.GroupSize)
    WAPT_n2 <- sum(datasheet$WAPT.GroupSize)
    HORS_n <- sum(datasheet[!is.na(datasheet$HORS.GroupSize),]$HORS.GroupSize)

    print(paste("MOOS ~ Calf_n =", Calf_n, ", Cows=",Cow_n,", Bulls=", Bull_n, ", Calf  ratio= ", Calf_ratio, ", Bull ratio=",Bull_ratio) )
    print(paste("Moose Total=", MOOS_n, "OR", MOOS_n2, "OR", sum(as.integer(as.character(DistancePreInput.MOOS$MOOS.GroupSize)))))
    print(paste("WTDE Total=", WTD_n, "OR", WTD_n2, "OR", sum(as.integer(as.character(DistancePreInput.WTDE$WTDE.GroupSize)))))
    print(paste("MUDE Total=", MUDE_n, "OR", MD_n2, "OR", sum(as.integer(as.character(DistancePreInput.MUDE$MUDE.GroupSize)))))
    print(paste("WAPT Total=", WAPT_n, "OR", WAPT_n2, "OR", sum(as.integer(as.character(DistancePreInput.WAPT$WAPT.GroupSize)))))
    print(paste("HORS Total=", HORS_n))

    print("Distance2_Totals")
    print(paste("Moose Total=", sum(DistanceInput2$size)))
    print(paste("WTDE Total=", sum(DistanceInput2.WT$size)))
    print(paste("MUDE Total=", sum(DistanceInput2.MD$size)))
    print(paste("WAPT Total=", sum(DistanceInput2.WAPT$size)))
    print(paste("HORS Total=", sum(DistanceInput2.HORS$size)))

    moos.hn_cos_2 <- NULL
    moos.hn_cos_3 <- NULL
    moos.hn_cos   <- NULL
    moos.hn_herm  <- NULL
    moos.hr_herm  <- NULL
    moos.hr_cos   <- NULL
    moos.hr_poly  <- NULL
    moos.unif_cos <- NULL
  #  moos.hn_cos_2 <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos", order = 2)
  #  moos.hn_cos_3 <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos", order = 3)
    moos.hn_cos   <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos")
    moos.hn_herm  <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "herm")
    moos.hr_herm  <- ds(DistanceInput2, truncation = truncvalue(), key="hr", adjustment = "herm")
    moos.hr_cos   <- ds(DistanceInput2, truncation = truncvalue(), key="hr", adjustment = "cos")
    moos.hr_poly  <- ds(DistanceInput2, truncation = truncvalue(), key="hr", adjustment = "poly")
    moos.unif_cos <- ds(DistanceInput2, truncation = truncvalue(), key="unif", adjustment = "cos")
   # mlist <- list(moos.hn_cos, moos.hr_cos, moos.hr_poly, moos.unif_cos,  moos.hn_cos_2,  moos.hn_cos_3, moos.hn_herm,  moos.hr_herm)
    mlist <- list(moos.hn_cos, moos.hr_cos, moos.hr_poly, moos.unif_cos,  moos.hn_herm,  moos.hr_herm)


    modelnum <- length(mlist)
    model_results <- list()
    for (i in 1:modelnum) { #return results for each candidate moose model such that they can simply be accessed for reporting in global env
      Vector <- numeric(9) #create vector
      Vector[1] <- fun.getModDescr(as.character(mlist[[i]]$ddf$dsmodel[2])) #model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mlist[[i]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      Vector[10] <- mlist[[i]]$ddf$dsmodel[2]
       model_results[[i]] <- Vector
    }
    model_result_df <- as.data.frame(do.call("rbind", model_results))
    print(model_result_df)
    colnames(model_result_df) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl","Full model description")
    best.moos <- model_result_df[which.min(model_result_df$AIC),]

    ddf.1.moos<-ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos")

    list(datasheet=datasheet,
         ddf.1.moos = moos.hn_cos ,
         model_result_df = model_result_df,
         Calf_n = Calf_n,
         Cow_n = Cow_n,
         datasheet=datasheet,
         transflown = transflown,
         strat_num=strat_num,
         MOOS_n=MOOS_n,
         Calf_ratio=Calf_ratio,
         Bull_ratio=Bull_ratio,
         Bull_n=Bull_n,
         WTD_n=WTD_n,
         MUDE_n=MUDE_n,
         WAPT_n=WAPT_n,
         truncvalue = truncvalue(),
         DistanceInput2.MD=DistanceInput2.MD,
         DistanceInput2.WT=DistanceInput2.WT,
         DistanceInput2.WAPT=DistanceInput2.WAPT,
         DistanceInput2.HORS=DistanceInput2.HORS,
         DistanceInput2 = DistanceInput2,
         results_num_index = results_num_index,
         HORS_n=HORS_n,
         strat=strat,
         U.list=U.list,
         DistancePreInput.WTDE=DistancePreInput.WTDE,
         DistancePreInput.MUDE=DistancePreInput.MUDE,
         DistancePreInput.HORS=DistancePreInput.HORS,
         DistancePreInput.WAPT=DistancePreInput.WAPT)
  }, ignoreNULL = FALSE)#######!!!!!!!!!!!!!!!

  OL.MD   <- eventReactive(OL(), {
    print("I'm into mulies NOW")
    print(paste("MULE DEER OL()$DISTANCEINPUT2"))
    print(OL()$DistanceInput2.MD)
    Calf_n.MD <-  sum(OL()$datasheet[!is.na(OL()$datasheet$MUDE.GroupSize),]$MUDE.Fawn)
    Cow_n.MD <- sum(OL()$datasheet[!is.na(OL()$datasheet$MUDE.GroupSize),]$MUDE.Doe)
    Calf_ratio.MD <- Calf_n.MD / Cow_n.MD
    Bull_n.MD <- sum(sum(OL()$datasheet[!is.na(OL()$datasheet$MUDE.GroupSize),]$MUDE.Buck.NA), sum(OL()$datasheet[!is.na(OL()$datasheet$MUDE.GroupSize),]$datasheet$MUDE.Buck.S), sum(OL()$datasheet[!is.na(OL()$datasheet$MUDE.GroupSize),]$MUDE.Buck.M), sum(OL()$datasheet[!is.na(OL()$datasheet$MUDE.GroupSize),]$MUDE.Buck.L))
    Bull_ratio.MD <- Bull_n.MD / Cow_n.MD
    #Create null objects to avoid erros when adding empty (non-converging) models to mdlist
    mude.hn_cos  <- NULL
    mude.hn_herm <- NULL
    mude.hr_herm <- NULL
    mude.hr_cos  <- NULL
    mude.hr_poly  <- NULL
    mude.unif_cos <- NULL

    mude.hn_cos   <- ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="hn", adjustment = "cos")
    mude.hn_herm  <- ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="hn", adjustment = "herm")
    mude.hr_herm  <- ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="hr", adjustment = "herm")
    mude.hr_cos   <- ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="hr", adjustment = "cos")
    mude.hr_poly  <- ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="hr", adjustment = "poly")
    mude.unif_cos <- ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="unif", adjustment = "cos")
    results_num_index <- as.integer(OL()$results_num_index)
    mdlist <-     list(mude.hn_cos,mude.hn_herm, mude.hr_herm, mude.hr_cos, mude.hr_poly, mude.unif_cos)
    modelnum.md <- length(mdlist)
    model_results.md <- list()

    for (j in 1:modelnum.md) {
      if(is.null(mdlist[[j]])){next}
      Vector <- numeric(9) #create vector
      Vector[1] <- fun.getModDescr(as.character(mdlist[[j]]$ddf$dsmodel[2])) #model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mdlist[[j]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      Vector[10] <- mdlist[[j]]$ddf$dsmodel[2]
       model_results.md[[j]] <- Vector
    }

    model_result_df.MUDE <- as.data.frame(do.call("rbind", model_results.md))
    colnames(model_result_df.MUDE) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl","Full model description")
    best.mude <- model_result_df.MUDE[which.min(model_result_df.MUDE$AIC),]
 #   ddf.1.mude<-ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="hn", adjustment = "cos")
    Copy.df.md <- OL()$DistancePreInput.MUDE
    Copy.df.md$DistancePerp <- as.numeric(Copy.df.md$DistancePerp)
    Copy.df.md$MUDE.GroupSize <- as.numeric(Copy.df.md$MUDE.GroupSize)
    CleanHistData.MD <- Copy.df.md[!(Copy.df.md$DistancePerp==""),]
    CleanHistData.MD <- Copy.df.md[!is.na(Copy.df.md$DistancePerp),]
    CleanHistData.MD <- subset(CleanHistData.MD, DistancePerp <= 600)
    CleanHistData.MD["distance"] <- CleanHistData.MD$DistancePerp
    CleanHistData.MD["object"] <- seq.int(nrow(CleanHistData.MD))
    U.list.MD <- split(CleanHistData.MD, CleanHistData.MD$Aircraft)
  print(paste("In MD line 321-before list"))
  print(paste("Mule deer Ulist table:"))
  print(U.list.MD)
      list(model_result_df.MUDE=model_result_df.MUDE, ddf.1.mude=mude.hn_cos, best.mude = best.mude, U.list.MD=U.list.MD, Calf_ratio.MD=Calf_ratio.MD, Bull_ratio.MD=Bull_ratio.MD, Bull_n.MD=Bull_n.MD)
  })
  OL.WT   <- eventReactive(OL(), {
    print("I'm into wtd NOW")
    print(OL()$DistanceInput2.WT)
    Calf_n.WT<-  sum(OL()$datasheet[!is.na(OL()$datasheet$WTDE.GroupSize),]$WTDE.Fawn)
    Cow_n.WT <- sum(OL()$datasheet[!is.na(OL()$datasheet$WTDE.GroupSize),]$WTDE.Doe)
    Calf_ratio.WT <- Calf_n.WT / Cow_n.WT
    Bull_n.WT <- sum(sum(OL()$datasheet[!is.na(OL()$datasheet$WTDE.GroupSize),]$WTDE.Buck.NA), sum(OL()$datasheet[!is.na(OL()$datasheet$WTDE.GroupSize),]$datasheet$WTDE.Buck.S), sum(OL()$datasheet[!is.na(OL()$datasheet$WTDE.GroupSize),]$WTDE.Buck.M), sum(OL()$datasheet[!is.na(OL()$datasheet$WTDE.GroupSize),]$WTDE.Buck.L))
    Bull_ratio.WT <- Bull_n.WT / Cow_n.WT
    #Create null objects to avoid erros when adding empty (non-converging) models to mdlist
    WTDE.hn_cos  <- NULL
    WTDE.hn_herm <- NULL
    WTDE.hr_herm <- NULL
    WTDE.hr_cos  <- NULL
    WTDE.hr_poly  <- NULL
    WTDE.unif_cos <- NULL

    WTDE.hn_cos   <- ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hn", adjustment = "cos")
    WTDE.hn_herm  <- ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hn", adjustment = "herm")
    WTDE.hr_herm  <- ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hr", adjustment = "herm")
    WTDE.hr_cos   <- ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hr", adjustment = "cos")
    WTDE.hr_poly  <- ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hr", adjustment = "poly")
    WTDE.unif_cos <- ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="unif", adjustment = "cos")
    results_num_index <- as.integer(OL()$results_num_index)
    mdlist <-     list(WTDE.hn_cos,WTDE.hn_herm, WTDE.hr_herm, WTDE.hr_cos, WTDE.hr_poly, WTDE.unif_cos)
    modelnum.md <- length(mdlist)
    model_results.md <- list()
    j=0
    for (j in 1:modelnum.md) {
      if(is.null(mdlist[[j]])){next}
      Vector <- numeric(9) #create vector
      Vector[1] <- fun.getModDescr(as.character(mdlist[[j]]$ddf$dsmodel[2])) #model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mdlist[[j]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      Vector[10] <- mdlist[[j]]$ddf$dsmodel[2]
        model_results.md[[j]] <- Vector
    }

    model_result_df.MUDE <- as.data.frame(do.call("rbind", model_results.md))
    print(paste("MUDE table - all mods"))
    print(model_result_df.MUDE)
    colnames(model_result_df.MUDE) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl","Full model description")
    best.mude <- model_result_df.MUDE[which.min(model_result_df.MUDE$AIC),]
    print(paste("best MUDE table"))
    print(best.mude)
  #ddf.1.wtde<-ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hn", adjustment = "cos")
    Copy.df.wt <- OL()$DistancePreInput.WTDE
    Copy.df.wt$DistancePerp <- as.numeric(Copy.df.wt$DistancePerp)
    Copy.df.wt$WTDE.GroupSize <- as.numeric(Copy.df.wt$WTDE.GroupSize)
    CleanHistData.WT <- Copy.df.wt[!(Copy.df.wt$DistancePerp==""),]
    CleanHistData.WT <- Copy.df.wt[!is.na(Copy.df.wt$DistancePerp),]
    CleanHistData.WT <- subset(CleanHistData.WT, DistancePerp <= 600)
    CleanHistData.WT["distance"] <- CleanHistData.WT$DistancePerp
    CleanHistData.WT["object"] <- seq.int(nrow(CleanHistData.WT))
    U.list.WT <- split(CleanHistData.WT, CleanHistData.WT$Aircraft)



    list(model_result_df.wtde=model_result_df.MUDE, ddf.1.wtde=WTDE.hn_cos, best.wtde = best.mude, U.list.WT=U.list.WT,Calf_ratio.WT=Calf_ratio.WT, Bull_ratio.WT=Bull_ratio.WT, Bull_n.WT=Bull_n.WT)
  })
  OL.WAPT <- eventReactive(OL(), {
    print("I'm into elk NOW")
    Calf_n.WAPT <-  sum(OL()$datasheet[!is.na(OL()$datasheet$WAPT.GroupSize),]$WAPT.Calf)
    Cow_n.WAPT <- sum(OL()$datasheet[!is.na(OL()$datasheet$WAPT.GroupSize),]$WAPT.Cow)
    Calf_ratio.WAPT <- Calf_n.WAPT / Cow_n.WAPT
    Bull_n.WAPT <- sum(sum(OL()$datasheet[!is.na(OL()$datasheet$WAPT.GroupSize),]$WAPT.Bull.NA), sum(OL()$datasheet[!is.na(OL()$datasheet$WAPT.GroupSize),]$WAPT.Bull.S), sum(OL()$datasheet[!is.na(OL()$datasheet$WAPT.GroupSize),]$WAPT.Bull.M), sum(OL()$datasheet[!is.na(OL()$datasheet$WAPT.GroupSize),]$WAPT.Bull.L))
    Bull_ratio.WAPT <- Bull_n.WAPT / Cow_n.WAPT
    #Create null objects to avoid erros when adding empty (non-converging) models to mdlist
    WAPT.hn_cos  <- NULL
    WAPT.hn_herm <- NULL
    WAPT.hr_herm <- NULL
    WAPT.hr_cos  <- NULL
    WAPT.hr_poly  <- NULL
    WAPT.unif_cos <- NULL
    WAPT.hn_cos   <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hn", adjustment = "cos")
    WAPT.hn_herm  <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hn", adjustment = "herm")
    WAPT.hr_herm  <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hr", adjustment = "herm")
    WAPT.hr_cos   <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hr", adjustment = "cos")
    WAPT.hr_poly  <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hr", adjustment = "poly")
    WAPT.unif_cos <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="unif", adjustment = "cos")
    # WAPT.hn_cos   <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hn", adjustment = "cos")
    # WAPT.hn_herm  <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hn", adjustment = "herm")
    # WAPT.hr_herm  <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hr", adjustment = "herm")
    # WAPT.hr_cos   <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hr", adjustment = "cos")
    # WAPT.hr_poly  <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="hr", adjustment = "poly")
    # WAPT.unif_cos <- ds(OL()$DistanceInput2.WAPT, truncation = truncvalue(), key="unif", adjustment = "cos")
    results_num_index <- as.integer(OL()$results_num_index)
    mdlist <-     list(WAPT.hn_cos,WAPT.hn_herm, WAPT.hr_herm, WAPT.hr_cos, WAPT.hr_poly, WAPT.unif_cos)
    print(paste("Elk - Made it through the models only"))
    modelnum.wapt <- length(mdlist)
    model_results.wapt <- list()
    Copy.df.wapt <- OL()$DistancePreInput.WAPT
    Copy.df.wapt$DistancePerp <- as.numeric(Copy.df.wapt$DistancePerp)
    Copy.df.wapt$WAPT.GroupSize <- as.numeric(Copy.df.wapt$WAPT.GroupSize)
    CleanHistData.WAPT <- Copy.df.wapt[!(Copy.df.wapt$DistancePerp==""),]
    CleanHistData.WAPT <- Copy.df.wapt[!is.na(Copy.df.wapt$DistancePerp),]
    CleanHistData.WAPT <- subset(CleanHistData.WAPT, DistancePerp <= 600)
    CleanHistData.WAPT["distance"] <- CleanHistData.WAPT$DistancePerp
    CleanHistData.WAPT["object"] <- seq.int(nrow(CleanHistData.WAPT))
    U.list.WAPT <- split(CleanHistData.WAPT, CleanHistData.WAPT$Aircraft)
    print(paste("Elk - Made it through the ulist section only"))
    j=0
    for (j in 1:modelnum.wapt) {
      if(is.null(mdlist[[j]])){next}
      Vector <- numeric(9) #create vector
      Vector[1] <- fun.getModDescr(as.character(mdlist[[j]]$ddf$dsmodel[2])) #model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mdlist[[j]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      Vector[10] <- mdlist[[j]]$ddf$dsmodel[2]
            model_results.wapt[[j]] <- Vector
    }
    print(paste("Elk - Made it through the models and loop"))
    model_result_df.WAPT <- as.data.frame(do.call("rbind", model_results.wapt))
    colnames(model_result_df.WAPT) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl")
    best.wapt <- model_result_df.WAPT[which.min(model_result_df.WAPT$AIC),]
    #  ddf.1.mude<-ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hn", adjustment = "cos")
    print(paste("Elk best model"))
    print(best.wapt)
    list(model_result_df.wapt=model_result_df.WAPT, ddf.1.wapt=WAPT.hn_cos, best.wapt = best.wapt, Calf_ratio.WAPT=Calf_ratio.WAPT, Bull_ratio.WAPT=Bull_ratio.WAPT, Bull_n.WAPT=Bull_n.WAPT, U.list.WAPT=U.list.WAPT)
  })
  OL.HORS <- eventReactive(OL(), {

    Calf_n.HORS <-  sum(OL()$datasheet[!is.na(OL()$datasheet$HORS.GroupSize),]$HORS.Foal)
    Cow_n.HORS <- sum(OL()$datasheet[!is.na(OL()$datasheet$HORS.GroupSize),]$HORS.Adult)
    Calf_ratio.HORS <- Calf_n.HORS / Cow_n.HORS
   # Bull_n.HO <- sum(sum(OL()$datasheet$HORS.Buck.NA), sum(OL()$datasheet$HORS.Buck.S), sum(OL()$datasheet$HORS.Buck.M), sum(OL()$datasheet$HORS.Buck.L))
  # Bull_ratio.HO <- Bull_n.HO / Cow_n.HO
    #Create null objects to avoid erros when adding empty (non-converging) models to mdlist
    HORS.hn_cos  <- NULL
    HORS.hn_herm <- NULL
    HORS.hr_herm <- NULL
    HORS.hr_cos  <- NULL
    HORS.hr_poly  <- NULL
    HORS.unif_cos <- NULL
    print("I'm into HORSE NOW")
    print(head(OL()$DistanceInput2.HORS))
    HORS.hn_cos   <- ds(OL()$DistanceInput2.HORS, truncation = truncvalue(), key="hn", adjustment = "cos")
    HORS.hn_herm  <- ds(OL()$DistanceInput2.HORS, truncation = truncvalue(), key="hn", adjustment = "herm")
    HORS.hr_herm  <- ds(OL()$DistanceInput2.HORS, truncation = truncvalue(), key="hr", adjustment = "herm")
    HORS.hr_cos   <- ds(OL()$DistanceInput2.HORS, truncation = truncvalue(), key="hr", adjustment = "cos")
    HORS.hr_poly  <- ds(OL()$DistanceInput2.HORS, truncation = truncvalue(), key="hr", adjustment = "poly")
    HORS.unif_cos <- ds(OL()$DistanceInput2.HORS, truncation = truncvalue(), key="unif", adjustment = "cos")
    results_num_index <- as.integer(OL()$results_num_index)
    mdlist <-     list(HORS.hn_cos,HORS.hn_herm, HORS.hr_herm, HORS.hr_cos, HORS.hr_poly, HORS.unif_cos)
    modelnum.md <- length(mdlist)
    model_results.md <- list()
      Copy.df.hors <- OL()$DistancePreInput.HORS
      Copy.df.hors$DistancePerp <- as.numeric(Copy.df.hors$DistancePerp)
      Copy.df.hors$HORS.GroupSize <- as.numeric(Copy.df.hors$HORS.GroupSize)
      CleanHistData.HORS <- Copy.df.hors[!(Copy.df.hors$DistancePerp==""),]
      CleanHistData.HORS <- Copy.df.hors[!is.na(Copy.df.hors$DistancePerp),]
      CleanHistData.HORS <- subset(CleanHistData.HORS, DistancePerp <= 600)
      CleanHistData.HORS["distance"] <- CleanHistData.HORS$DistancePerp
      CleanHistData.HORS["object"] <- seq.int(nrow(CleanHistData.HORS))
      U.list.HORS <- split(CleanHistData.HORS, CleanHistData.HORS$Aircraft)


    j=0
    for (j in 1:modelnum.md) {
      if(is.null(mdlist[[j]])){next}
      Vector <- numeric(9) #create vector
      Vector[1] <- fun.getModDescr(as.character(mdlist[[j]]$ddf$dsmodel[2]))#model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mdlist[[j]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      Vector[10] <- mdlist[[j]]$ddf$dsmodel[2]
            model_results.md[[j]] <- Vector
    }

    model_result_df.MUDE <- as.data.frame(do.call("rbind", model_results.md))
    colnames(model_result_df.MUDE) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl", "Full model description")
    print(model_result_df.MUDE)
    best.mude <- model_result_df.MUDE[which.min(model_result_df.MUDE$AIC),]
    print(best.mude)
    #  ddf.1.mude<-ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hn", adjustment = "cos")
    list(model_result_df.hors=model_result_df.MUDE, ddf.1.hors=HORS.hn_cos, best.hors = best.mude,  Calf_ratio.HORS=Calf_ratio.HORS, U.list.HORS=U.list.HORS)
  })
  OL.MAP <- eventReactive(input$WMU_Shp, {
    inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
    # print(inFile)
    if (is.null(inFile))
      return(NULL)
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")
    strat_num <- nrow(strat)
    #!!!!!!!!!!!!!!!# results_num_index <- OL()$results_num_index
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
    for (i in seq_along(l)){
      l[[i]] <- Lines(list(Line(rbind(f[i, ], t[i, ]))), as.character(i))
    }
    trans.flown.splat <- SpatialLines(l)
    trans.flown.splat.df <- SpatialLinesDataFrame(sl = trans.flown.splat,data = from.coords)
    print("HEAD transflownsplatdf")
    print(head(trans.flown.splat.df))


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
    m2 <- m1[!is.na(m1$GrpX),] #remove NAs from coordinates
    coordinates(m2)=~GrpX+GrpY
    proj4string(m2)<- CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    proj4string(survey.area359.TTM)<- CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    proj4string(trans.flown.splat.df)<- CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    wmu.shp <- spTransform(survey.area359.TTM, CRS("+proj=longlat +datum=WGS84"))
    pt.shp <- spTransform(m2, CRS("+proj=longlat +datum=WGS84"))
    transflown.shp <- spTransform(trans.flown.splat.df, CRS("+proj=longlat +datum=WGS84"))
    pt.df <- cbind(coordinates(pt.shp), pt.shp$size)
    pt.df <- pt.df[which(pt.shp$size>0),]
    colnames(pt.df) <- c("GrpX","GrpY","size")
    pt.shp.sp <- as(pt.shp, 'SpatialPoints')
    #line.shp <-spTransform(survey.transects359.TTM, CRS("+proj=longlat +datum=WGS84"))
    map <- ggmap::get_googlemap(center = c(lon=gCentroid(wmu.shp)$x, lat=gCentroid(wmu.shp)$y), maptype="hybrid",zoom=9, )#, markers =
    proj4string(trans.flown.splat.df)<- CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    transflown.shp <- spTransform(trans.flown.splat.df, CRS("+proj=longlat +datum=WGS84"))
    spat.list <- list(pt.shp, wmu.shp)
    #get the extent of the transects to compare the extent of the other layers - goal is to get the maximum extent for zooming
    minx <- extent(transflown.shp)[1]
    maxx <- extent(transflown.shp)[2]
    miny <- extent(transflown.shp)[3]
    maxy <- extent(transflown.shp)[4]

    #Get the maximum extents of the layers for zooming
    for(i in 1:length(spat.list)){
      if(extent(spat.list[[i]])[1] < minx) {minx <-extent(spat.list[[i]])[1] }
      if(extent(spat.list[[i]])[2] > maxx) {maxx <-extent(spat.list[[i]])[2] }
      if(extent(spat.list[[i]])[3] < miny) {miny <-extent(spat.list[[i]])[3] }
      if(extent(spat.list[[i]])[4] > maxy) {maxy <-extent(spat.list[[i]])[4] }
      extentofmap <- list(minx, maxx, miny, maxy)
    }
    #From: https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/download-raster.R
    #Handler to convert get_googlemap to raster
    # ggmap_rast <- function(map){
    #   map_bbox <- attr(map, 'bb')
    #   .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
    #   my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
    #   rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
    #   red <- my_map
    #   values(red) <- rgb_cols[['red']]
    #   green <- my_map
    #   values(green) <- rgb_cols[['green']]
    #   blue <- my_map
    #   values(blue) <- rgb_cols[['blue']]
    #   stack(red,green,blue)
    # }
    #
    # gmap.rast <- ggmap_rast(map=map) #convert googlemaps output to a raster format
    # googbackgrnd <- raster::mask(gmap.rast, as(raster::extent(minx, maxx, miny, maxy),'SpatialPolygons' ))#clip raster with max extent
    # googbackgrnd.df <- data.frame(rasterToPoints(googbackgrnd))
    list(
      map=map,
      wmu.shp=wmu.shp,
      transflown.shp = transflown.shp,
      pt.df = pt.df,
      survey.area359.TTM=survey.area359.TTM,
      trans.flown.splat.df=trans.flown.splat.df,
      m1=m1)

  })
  output$MOOS_DF <- renderPlot({ plot(OL()$ddf.1.moos, main=paste("Global detection function for moose, HN-Cos, truncation=",OL()$truncvalue[1]))})
  output$MOOS_QQ <- renderPlot({ ddf.gof(OL()$ddf.1.moos$ddf) })
  output$MOOS_TAB = DT::renderDataTable(OL()$model_result_df, options = list(lengthChange=FALSE), caption=paste("Table 1. Model results for candidate set of default models for moose. Truncation distance was ",OL()$ddf.1.moos$ddf$meta.data$width) )
  output$MOOS_MAP <- renderPlot({

 #   p <- ggplot(googbackgrnd.df)+geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + scale_color_identity()
 #   p <- p + geom_polygon(data = wmu.shp, fill="light blue", aes(x=long, y=lat, group=group, alpha=0.5))
 #  # p <- p + geom_path(aes(x=long,y=lat,group=group), data = trans.flown.splat, colour = "gray" )
 #   p <- p + geom_path(aes(x=long,y=lat,group=group), data = transflown.shp, colour = "gray" )
 #   # p <- p + geom_path(aes(x=long,y=lat,group=group), data = as.data.frame(trans.flown.splat.df), colour = "gray" )
 #   # p <- p+    geom_line(aes(x=long,y=lat,group=group), data = line.shp, colour = "gray" )
 #      #geom_point(data = m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
 #   p <- p + geom_point(data =as.data.frame(pt.df), aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) ) + coord_fixed(1.3)
 # #  p <- p + geom_point(aes(x=))
 #      plot(p)







      ggmap(OL.MAP()$map, extent="panel", maprange=FALSE) +
        geom_polygon(data = OL.MAP()$wmu.shp, fill="light blue", color="purple", aes(x=long, y=lat, group=group, alpha=0.1),size=0.7, alpha=0.1) +
        geom_path(aes(x=long,y=lat,group=group), data = OL.MAP()$transflown.shp, colour = "yellow", alpha=0.8 )+
        # p <- p + geom_path(aes(x=(coordinates(transflown.shp)[[1]][[1]][,1]),y=coordinates(transflown.shp)[[1]][[1]][,2],group=group), data = as.data.frame(transflown.shp), colour = "gray" )
        #geom_line(aes(x=long,y=lat,group=group), data = line.shp, colour = "gray" ) +
        #geom_point(data = m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
        geom_point(data =as.data.frame(OL.MAP()$pt.df), aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) ) + coord_fixed(1.3)


  })
  output$MOOS_MAP2 <- renderPlot({
     p <- ggplot ()
     p <- p + geom_polygon(data = OL.MAP()$survey.area359.TTM, fill="light blue", aes(x=long, y=lat, group=group)) + coord_equal()
     p <- p + geom_path(aes(x=long,y=lat,group=group), data = OL.MAP()$trans.flown.splat.df, colour = "gray" )
     p <- p + geom_point(data = OL.MAP()$m1, aes(x=GrpX, y=GrpY, size = size), colour = "red", alpha=I(0.5) )
     p <- p + labs(fill = "MDSTRATA", x = "Easting (10TM AEP Forest)", y = "Northing (10TM AEP Forest)")
     p <- p + geom_point(aes(x=))
     plot(p)
  })
  output$MOOS_TXT = renderText({
    paste("The survey included", round(OL()$ddf.1.moos$dht$individuals$summary$Effort[1],1), "km of transects (n= ", nrow(OL()$transflown), " mean transect length = ",
                                round(OL()$ddf.1.moos$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),"km) that were sampled across a total of ", OL()$strat_num,
                                "strata. There were an estimated ", round(OL()$ddf.1.moos$dht$individuals$N$Estimate[OL()$results_num_index]*1000, 0),
                                #     "strata. There were an estimated ", round(sum(OL()$#ddf.1.moos$dht$individuals$N$Estimate)*1000, 0),
                                #        "strata. There were an estimated ", round(OL()$model_result_df[1]$Nhat #ddf.1.moos$dht$individuals$N$Estimate[OL()$strat_num + 1]*1000, 0),
                                " moose  (CV = ", round(OL()$ddf.1.moos$dht$individuals$N$cv[1], 2)," Confidence interval = ",
                                round(OL()$ddf.1.moos$dht$individuals$N$lcl[1]*1000, 0)," - ", round(OL()$ddf.1.moos$dht$individuals$N$ucl[1]*1000, 0),
                                ") within the study area. In total, ", OL()$MOOS_n," moose were observed in ",  OL()$ddf.1.moos$dht$clusters$summary$n[1],
                                " groups during the survey (sampling fraction = ", round(OL()$MOOS_n/(OL()$ddf.1.moos$dht$individuals$N$Estimate[1]*1000)*100,1),
                                "%). The unadjusted  observed calf ratio and bull ratio (i.e. not corrected for effort between strata) were ", round(OL()$Calf_ratio, 2), " and ",
                                round(OL()$Bull_ratio, 2), " , respectively. Of the bulls observed, ", round((sum(OL()$datasheet$MOOS.Bull.N)/OL()$Bull_n)*100,1),
                                "% had already shed their antlers. Of those bulls still with antlers, ",
                                round((sum(OL()$datasheet$MOOS.Bull.S)/(OL()$Bull_n - sum(OL()$datasheet$MOOS.Bull.N)))*100,1), "% were small,",
                                round((sum(OL()$datasheet$MOOS.Bull.M)/(OL()$Bull_n - sum(OL()$datasheet$MOOS.Bull.N)))*100,1), "% were medium, and ",
                                round((sum(OL()$datasheet$MOOS.Bull.L)/(OL()$Bull_n - sum(OL()$datasheet$MOOS.Bull.N)))*100,1), "% were large. In addition to moose, ",
                                OL()$WTD_n, " White-tailed deer, ", OL()$MUDE_n, " Mule Deer, and ", OL()$WAPT_n," elk were observed during the survey. ")
  })
  output$MOOS_TAB2 = DT::renderDataTable(round(as.data.frame(OL()$ddf.1.moos$dht$clusters$summary)[,-1],3), options= list(lengthChange=F), caption ="Table 3. Results of distance sampling encounter rates, by stratum and combined. ") #%>% formatRound(c(2:7),1)%>% formatStyle(columns=c(2:7), 'text-align'='center')
  output$MOOS_TAB1 = DT::renderDataTable(fun.fixDest(OL()$ddf.1.moos$dht$clusters$D), options= list(lengthChange=F), caption ="Table 2. Results of distance sampling estimates, by stratum and combined. ") #%>% formatRound(c(2:4),1) %>% formatStyle(columns=c(2:4), 'text-align'='center')
  output$MOOS_AIRDF <- renderUI({ get_plot_output_list(OL()$U.list) })

  output$MUDE_QQ <- renderPlot({ddf.gof(OL.MD()$ddf.1.mude$ddf) })
  output$MUDE_MAP <- renderPlot({
    inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
    if (is.null(inFile))
      return(NULL)
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")
    strat_num <- nrow(strat)
    #!!!!!!!!!!!!!!#results_num_index <- OL()$results_num_index

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
  output$MUDE_TXT = renderText({


    paste("The survey included",
          round(OL.MD()$ddf.1.mude$dht$individuals$summary$Effort[1],1),
          "km of transects (n= ", nrow(OL()$transflown),
          " mean transect length = ",
          round(OL.MD()$ddf.1.mude$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),
          "km) that were sampled across a total of ", OL()$strat_num,
          "strata. There were an estimated ",
          round(OL.MD()$ddf.1.mude$dht$individuals$N$Estimate[OL()$results_num_index]*1000, 0),
          " mule deer  (CV = ",
          round(as.numeric(as.character(OL.MD()$best.mude$CV)), 2),
          " Confidence interval = ",
          round(as.numeric(as.character(OL.MD()$best.mude$Nlcl)), 0)," - ",
          round(as.numeric(as.character(OL.MD()$best.mude$Nucl)), 0),
          ") within the study area. In total, ",
          OL()$MUDE_n," mule deer were observed in ",
          OL.MD()$ddf.1.mude$dht$clusters$summary$n[1],
          " groups during the survey (sampling fraction = ",
          round((OL()$MUDE_n/((OL.MD()$ddf.1.mude$dht$individuals$N$Estimate[1]*1000)*100)),1),
          "%). The unadjusted  observed fawn ratio and buck ratio (i.e. not corrected for effort between strata) were ",
          round(OL.MD()$Calf_ratio.MD, 2), " and ",
          round(OL.MD()$Bull_ratio.MD, 2), " , respectively. Of the bucks observed, ",
          round((sum(OL()$datasheet$MUDE.Buck.NA)/OL.MD()$Bull_n.MD)*100,1),
          "% had already shed their antlers. Of those bulls still with antlers, ",
          round((sum(OL()$datasheet$MUDE.Bull.S)/(OL.MD()$Bull_n.MD - sum(OL()$datasheet$MUDE.Bull.NA)))*100,1), "% were small,",
          round((sum(OL()$datasheet$MUDE.Bull.M)/(OL.MD()$Bull_n.MD - sum(OL()$datasheet$MUDE.Bull.NA)))*100,1), "% were medium, and ",
          round((sum(OL()$datasheet$MUDE.Bull.L)/(OL.MD()$Bull_n.MD - sum(OL()$datasheet$MUDE.Bull.NA)))*100,1), "% were large.")


  })
  output$MD_DF <- renderPlot({plot(OL.MD()$ddf.1.mude, main=("Global detection function for mule deer, HN-Cos, no truncation"))})
  output$MUDE_TAB = DT::renderDataTable(OL.MD()$model_result_df.MUDE, options = list(lengthChange=FALSE), caption=paste("Table 1. Model results for candidate set of default models for mule deer. Truncation distance was ",OL()$truncvalue))
  output$MUDE_TAB2 = DT::renderDataTable(round(as.data.frame(OL.MD()$ddf.1.mude$dht$clusters$summary)[,-1],3), options= list(lengthChange=F), caption ="Table 3. Results of distance sampling encounter rates, by stratum and combined. ") #%>% formatRound(c(2:7),1)%>% formatStyle(columns=c(2:7), 'text-align'='center')
  output$MUDE_TAB1 = DT::renderDataTable(fun.fixDest(OL.MD()$ddf.1.mude$dht$clusters$D), options= list(lengthChange=F), caption ="Table 2. Results of distance sampling estimates, by stratum and combined. ") #%>% formatRound(c(2:4),1) %>% formatStyle(columns=c(2:4), 'text-align'='center')
  output$MUDE_AIRDF <- renderUI({get_plot_output_list(OL()$U.list.MD) })


  output$WTDE_TXT = renderText({
     paste("The survey included",
           round(OL.WT()$ddf.1.wtde$dht$individuals$summary$Effort[1],1),
           "km of transects (n= ", nrow(OL()$transflown),
           " mean transect length = ",
           round(OL.WT()$ddf.1.wtde$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),
           "km) that were sampled across a total of ", OL()$strat_num,
           "strata. There were an estimated ",
           round(OL.WT()$ddf.1.wtde$dht$individuals$N$Estimate[OL()$results_num_index]*1000, 0),
           " white-tailed deer  (CV = ",
           round(as.numeric(as.character(OL.WT()$best.wtde$CV)), 2),
           " Confidence interval = ",
           round(as.numeric(as.character(OL.WT()$best.wtde$Nlcl)), 0)," - ",
           round(as.numeric(as.character(OL.WT()$best.wtde$Nucl)), 0),
           ") within the study area. In total, ",
           OL()$WTDE_n," white-tailed deer were observed in ",
           OL.WT()$ddf.1.wtde$dht$clusters$summary$n[1],
           " groups during the survey (sampling fraction = ",
           round((OL()$WTDE_n/((OL.WT()$ddf.1.wtde$dht$individuals$N$Estimate[1]*1000)*100)),1),
           "%). The unadjusted  observed fawn ratio and buck ratio (i.e. not corrected for effort between strata) were ",
           round(OL.WT()$Calf_ratio.WT, 2), " and ",
           round(OL.WT()$Bull_ratio.WT, 2), " , respectively. Of the bucks observed, ",
           round((sum(OL()$datasheet$WTDE.Buck.NA)/OL.WT()$Bull_n.WT)*100,1),
           "% had already shed their antlers. Of those bulls still with antlers, ",
           round((sum(OL()$datasheet$WTDE.Bull.S)/(OL.WT()$Bull_n.WT - sum(OL()$datasheet$WTDE.Bull.NA)))*100,1), "% were small,",
           round((sum(OL()$datasheet$WTDE.Bull.M)/(OL.WT()$Bull_n.WT - sum(OL()$datasheet$WTDE.Bull.NA)))*100,1), "% were medium, and ",
           round((sum(OL()$datasheet$WTDE.Bull.L)/(OL.WT()$Bull_n.WT - sum(OL()$datasheet$WTDE.Bull.NA)))*100,1), "% were large. In addition to mule deer, ",
           OL()$WTD_n, " White-tailed deer, ", OL()$MOOS_n, " Mule Deer, and ", OL()$WAPT_n," elk were observed during the survey. ")
   })
  output$WTDE_TAB = DT::renderDataTable(OL.WT()$model_result_df.wtde, options = list(lengthChange=FALSE), caption=paste("Table 1. Model results for candidate set of default models for white-tailed deer. Truncation distance was ",OL()$truncvalue))
  output$WTDE_MAP <- renderPlot({

    inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
    # print(inFile)
    if (is.null(inFile))
      return(NULL)
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")
    strat_num <- nrow(strat)
   #!!!!!!!!!!!!# results_num_index <- OL()$results_num_index

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
  output$WTDE_DF <- renderPlot({plot(OL.WT()$ddf.1.wtde, main=("Global detection function for white-tailed deer, HN-Cos, no truncation"))})
  output$WTDE_QQ <- renderPlot({ddf.gof(OL.WT()$ddf.1.wtde$ddf) })
  output$WTDE_TAB2 = DT::renderDataTable(round(as.data.frame(OL.WT()$ddf.1.wtde$dht$clusters$summary)[,-1],3), options= list(lengthChange=F), caption ="Table 3. Results of distance sampling encounter rates, by stratum and combined. ") #%>% formatRound(c(2:7),1)%>% formatStyle(columns=c(2:7), 'text-align'='center')
  output$WTDE_TAB1 = DT::renderDataTable(fun.fixDest(OL.WT()$ddf.1.wtde$dht$clusters$D), options= list(lengthChange=F), caption ="Table 2. Results of distance sampling estimates, by stratum and combined. ") #%>% formatRound(c(2:4),1) %>% formatStyle(columns=c(2:4), 'text-align'='center')
  output$WTDE_AIRDF <- renderUI({ get_plot_output_list(OL()$U.list.WT) })


  output$WAPT_QQ <- renderPlot({ddf.gof(OL.WAPT()$ddf.1.wapt$ddf) })
  output$WAPT_DF <- renderPlot({ plot(OL()$ddf.1.wapt, main=paste("Global detection function for moose, HN-Cos, truncation=",OL()$truncvalue[1]))})
  output$WAPT_MAP <- renderPlot({

    inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
    if (is.null(inFile))
      return(NULL)
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")
    strat_num <- nrow(strat)
   #!!!!!!# results_num_index <- OL()$results_num_index

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
  output$WAPT_TXT = renderText({
    paste("The survey included",
          round(OL.WAPT()$ddf.1.wapt$dht$individuals$summary$Effort[1],1),
          "km of transects (n= ", nrow(OL()$transflown),
          " mean transect length = ",
          round(OL.WAPT()$ddf.1.wapt$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),
          "km) that were sampled across a total of ", OL()$strat_num,
          "strata. There were an estimated ",
          round(OL.WAPT()$ddf.1.wapt$dht$individuals$N$Estimate[OL()$results_num_index]*1000, 0),
          " elk  (CV = ",
          round(as.numeric(as.character(OL.WAPT()$best.wapt$CV)), 2),
          " Confidence interval = ",
          round(as.numeric(as.character(OL.WAPT()$best.wapt$Nlcl)), 0)," - ",
          round(as.numeric(as.character(OL.WAPT()$best.wapt$Nucl)), 0),
          ") within the study area. In total, ",
          OL()$WAPT_n," elk were observed in ",
          OL.WAPT()$ddf.1.wapt$dht$clusters$summary$n[1],
          " groups during the survey (sampling fraction = ",
          round((OL()$WAPT_n/((OL.WAPT()$ddf.1.wapt$dht$individuals$N$Estimate[1]*1000)*100)),1),
          "%). The unadjusted  observed calf ratio and bull ratio (i.e. not corrected for effort between strata) were ",
          round(OL.WAPT()$Calf_ratio.WAPT, 2), " and ",
          round(OL.WAPT()$Bull_ratio.WAPT, 2), " , respectively. Of the bulls observed, ",
          round((sum(OL()$datasheet$WAPT.Bull.NA)/OL.WAPT()$Bull_n.WAPT)*100,1),
          "% had already shed their antlers. Of those bulls still with antlers, ",
          round((sum(OL()$datasheet$WAPT.Bull.S)/(OL.WAPT()$Bull_n.WAPT - sum(OL()$datasheet$WAPT.Bull.NA)))*100,1), "% were small,",
          round((sum(OL()$datasheet$WAPT.Bull.M)/(OL.WAPT()$Bull_n.WAPT - sum(OL()$datasheet$WAPT.Bull.NA)))*100,1), "% were medium, and ",
          round((sum(OL()$datasheet$WAPT.Bull.L)/(OL.WAPT()$Bull_n.WAPT - sum(OL()$datasheet$WAPT.Bull.NA)))*100,1), "% were large. In addition to mule deer, ",
          OL()$WTD_n, " White-tailed deer, ", OL()$MOOS_n, " Mule Deer, and ", OL()$MUDE_n," mule deer were observed during the survey. ")
  })
  output$WAPT_TAB = DT::renderDataTable(OL.WAPT()$model_result_df.wapt, options = list(lengthChange=FALSE), caption=paste("Table 1. Model results for candidate set of default models for elk. Truncation distance was ",OL()$truncvalue))
  output$WAPT_TAB2 = DT::renderDataTable(round(as.data.frame(OL.WAPT()$ddf.1.wapt$dht$clusters$summary)[,-1],3), options= list(lengthChange=F), caption ="Table 3. Results of distance sampling encounter rates, by stratum and combined. ") #%>% formatRound(c(2:7),1)%>% formatStyle(columns=c(2:7), 'text-align'='center')
  output$WAPT_TAB1 = DT::renderDataTable(fun.fixDest(OL.WAPT()$ddf.1.wapt$dht$clusters$D), options= list(lengthChange=F), caption ="Table 2. Results of distance sampling estimates, by stratum and combined. ") #%>% formatRound(c(2:4),1) %>% formatStyle(columns=c(2:4), 'text-align'='center')
  output$WAPT_AIRDF <- renderUI({ get_plot_output_list(OL.WAPT()$U.list.WAPT) })


  output$HORS_QQ <- renderPlot({ddf.gof(OL.HORS()$ddf.1.hors$ddf) })
  output$HORS_DF <- renderPlot({ plot(OL.HORS()$ddf.1.hors, main=paste("Global detection function for moose, HN-Cos, truncation=",OL()$truncvalue[1]))})
  output$HORS_MAP <- renderPlot({

    inFile <- DB() #input$MegaDB$datapath  #User input -- Get the Access database pathname
    if (is.null(inFile))
      return(NULL)
    DB <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",inFile)
    myconn <- odbcDriverConnect(DB)
    strat <- sqlFetch(myconn, "strata")
    strat_num <- nrow(strat)
   #!!!!!!!!!!# results_num_index <- OL()$results_num_index

    datasheet <- as.data.frame(sqlQuery(myconn, "select * from datasheet"))

    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub(" ", ".", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))
    names(datasheet) <- sub("/", "", names(datasheet))



    transflown <- datasheet[!duplicated(datasheet[, c("Transect.ID", "Stratum")]), ]
    transflown <- transflown[!is.na(transflown$Stratum),]
    transflown$DistancePerp <- " "
    transflown$HORS.GroupSize <- " "
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)


    datasheet.2 <- datasheet[ which(datasheet$HORS.GroupSize >0),]
    datasheet.2 <- unique(datasheet.2)


    DistancePreInput.HORS.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.HORS.2 <- unique(DistancePreInput.HORS.2)


    DistancePreInput.HORS <- merge(datasheet.2, DistancePreInput.HORS.2, all=T)
    DistancePreInput.HORS <- unique(DistancePreInput.HORS)


    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.HORS$ID), Region.Label= DistancePreInput.HORS$Stratum,Area = as.numeric(DistancePreInput.HORS$Stratum.Area), TID = as.numeric(DistancePreInput.HORS$Transect.ID), TLENGTH = as.numeric(DistancePreInput.HORS$Transect.Length), Effort=as.numeric(DistancePreInput.HORS$Length)/1000, distance= as.numeric(DistancePreInput.HORS$DistancePerp), size=as.numeric(DistancePreInput.HORS$HORS.GroupSize),CC=as.factor(DistancePreInput.HORS$Covariate.1), Activity=as.factor(DistancePreInput.HORS$Covariate.2)))

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
    obs.table.HORS <- data.frame(cbind(object = DistanceInput$object.ID, Region.Label = DistanceInput$Region.Label, Sample.Label = DistanceInput$TID, distance = DistanceInput$distance, size = DistanceInput$size))

    m1 <- merge(obs.table.HORS, DistancePreInput.HORS, by.x = "object", by.y = "ID")

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
  output$HORS_TXT = renderText({
    paste("The survey included",
          round(OL.HORS()$ddf.1.hors$dht$individuals$summary$Effort[1],1),
          "km of transects (n= ", nrow(OL()$transflown),
          " mean transect length = ",
          round(OL.HORS()$ddf.1.hors$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),
          "km) that were sampled across a total of ", OL()$strat_num,
          "strata. There were an estimated ",
          round(OL.HORS()$ddf.1.hors$dht$individuals$N$Estimate[OL()$results_num_index]*1000, 0),
          " horses  (CV = ",
          round(as.numeric(as.character(OL.HORS()$best.hors$CV)), 2),
          " Confidence interval = ",
          round(as.numeric(as.character(OL.HORS()$best.hors$Nlcl)), 0)," - ",
          round(as.numeric(as.character(OL.HORS()$best.hors$Nucl)), 0),
          ") within the study area. In total, ",
          OL()$HORS_n," horses were observed in ",
          OL.HORS()$ddf.1.hors$dht$clusters$summary$n[1],
          " groups during the survey (sampling fraction = ",
          round((OL()$HORS_n/((OL.HORS()$ddf.1.hors$dht$individuals$N$Estimate[1]*1000)*100)),1),
          "%). The unadjusted  observed foal ratio (not corrected for effort between strata) was ",
          round(OL.HORS()$Calf_ratio.HORS, 2), ".")
  })
  output$HORS_TAB = DT::renderDataTable(OL.HORS()$model_result_df.hors, options = list(lengthChange=FALSE), caption=paste("Table 1. Model results for candidate set of default models for elk. Truncation distance was ",OL()$truncvalue))
  output$HORS_TAB2 = DT::renderDataTable(round(as.data.frame(OL.HORS()$ddf.1.hors$dht$clusters$summary)[,-1],3), options= list(lengthChange=F), caption ="Table 3. Results of distance sampling encounter rates, by stratum and combined. ") #%>% formatRound(c(2:7),1)%>% formatStyle(columns=c(2:7), 'text-align'='center')
  output$HORS_TAB1 = DT::renderDataTable(fun.fixDest(OL.HORS()$ddf.1.hors$dht$clusters$D), options= list(lengthChange=F), caption ="Table 2. Results of distance sampling estimates, by stratum and combined. ") #%>% formatRound(c(2:4),1) %>% formatStyle(columns=c(2:4), 'text-align'='center')
  output$HORS_AIRDF <- renderUI({ get_plot_output_list(OL.HORS()$U.list.HORS) })


  POW <- eventReactive(input$PowerAction, {

    # # Create a Progress object
    # progress <- shiny::Progress$new()
    # progress$set(message = "Computing data", value = 0)
    # # Close the progress when this reactive exits (even if there's an error)
    # on.exit(progress$close())

    bootstrap <- function(DistanceInput2, S, N){

     # DistanceInput2$Region.Label <- OL()$strat$'Stratum Name'[[1]]#`Stratum Name`[1] #In the instance of stratification, replace with 1 stratum
     # DistanceInput2$Area <- sum(OL()$strat$'Stratum Area')  #recalculate the area with the area of all strata
      #! Replace code to account for strata and minimum sample sizes to calculate density by strata, then collapse strata

      #Create dataframe with relevant fields
      cvsummary <- data.frame(Iteration=integer(),
                              Sample=integer(),
                              Region=integer(),
                              Area_Skm=numeric(),
                              Area_Cover=numeric(),
                              Transects=integer(),
                              Effort=numeric(),
                              ObsNum=numeric(),
                              Pop_est=numeric(),
                              Pop_se=numeric(),
                              Pop_cv=numeric(),
                              Pop_lcl=numeric(),
                              Pop_ucl=numeric(),
                              Pop_df=numeric(),
                              D_Est=numeric(),
                              D_se=numeric(),
                              D_cv=numeric(),
                              D_lcl=numeric(),
                              D_ucl=numeric(),
                              D_df=numeric(),
                              EncRate=numeric(),
                              EncRateSE=numeric(),
                              EncRateCV=numeric(),
                              DetP=numeric(),
                              DetP_SE=numeric(),
                              DetP_CV=numeric(),
                              ExpectedCS=numeric(),
                              ECS_SE=numeric())

      cvsummary_colnames <- names(cvsummary)
      # print(paste("starting loop"))

      for(i in 20:N) {
        # print(paste("starting loop 2"))


        for(j in 1:S) {
          print(paste("starting loop 3. N=",N, ",iteration = ",j, "of ",S,", and i=",i," of ",N,"transects." ))
          print(paste("Progress: ", round(((i-20)/(N-20))*100,1),"% completed."))
       #   progress$set(value = round(((i-20)/(N-20))*100,1), detail = detail)
          SubsetTable <- DistanceInput2[sample(1:nrow(DistanceInput2),i, replace=FALSE),]

          ds.model<-ds(SubsetTable, truncation = 425, monotonicity = FALSE,quiet=TRUE)

          results <- summary(ds.model)



          D_Stats <- data.frame(results$dht$clusters[4])
          A_Stats <-data.frame(results$dht$clusters[2])
          N_Stats <- data.frame(results$dht$clusters[3])


          Iteration <-   i
          Sample <-      j
          Region <-      1
          Area_Skm <-   (A_Stats[2])[1,1]
          Area_Cover <- (A_Stats[3])[1,1]
          Transects <-  (A_Stats[6])[1,1]
          Effort <-     (A_Stats[4])[1,1]
          ObsNum <-     (A_Stats[5])[1,1]
          Pop_est <-    (N_Stats[2])[1,1]
          Pop_se <-     (N_Stats[3])[1,1]
          Pop_cv <-     (N_Stats[4])[1,1]
          Pop_lcl <-    (N_Stats[5])[1,1]
          Pop_ucl <-    (N_Stats[6])[1,1]
          P_df <-       (N_Stats[7])[1,1]
          D_est <-      (D_Stats[2])[1,1]
          D_se <-       (D_Stats[3])[1,1]
          D_cv <-       (D_Stats[4])[1,1]
          D_lcl <-      (D_Stats[5])[1,1]
          D_ucl <-      (D_Stats[6])[1,1]
          D_df <-       (D_Stats[7])[1,1]
          EncRate <-    (A_Stats[7])[1,1]
          EncRateSE <-  (A_Stats[8])[1,1]
          EncRateCV <-  (A_Stats[9])[1,1]
          DetP <-       summary(ds.model)$ds$average.p[1]
          DetP_SE <-    summary(ds.model)$ds$average.p.se[1]
          DetP_CV <-    (DetP_SE/DetP)
          ExpectedCS <- summary(ds.model)$dht[3]$Expected.S$Expected.S[1]
          ECS_SE <-     summary(ds.model)$dht[3]$Expected.S$se.Expected.S[1]

          cvsummary <- rbind(c(Iteration,Sample,Region,Area_Skm,Area_Cover,Transects,Effort,ObsNum,Pop_est,Pop_se,Pop_cv,Pop_lcl,Pop_ucl,P_df,D_est,D_se,D_cv,D_lcl,D_ucl,D_df,EncRate,EncRateSE,EncRateCV,DetP,DetP_SE,DetP_CV,ExpectedCS,ECS_SE),cvsummary)

          names(cvsummary) <- cvsummary_colnames
          print(tail(cvsummary))

          #print(paste("The survey resulted in an estimate of ",Pop_est,"moose at a density of ",D_est,"moose per square kilometre, with a CV of", Pop_cv, "."))

        }

      } #add another 5 transects to the next iteration (instead of 1) to speed up processing
      return(cvsummary)
    }

    out <- bootstrap(OL()$DistanceInput2, 5, nrow(OL()$transflown))
    desired_cv <- input$DesiredCV
    out <- na.omit(out)
    out$cv <- rep(as.numeric(desired_cv),nrow(out)) # add desired cv to table such that it can be plotted in ggplot2 geomsmooth
    cvsummary2 <- subset(out, Pop_cv <= 1) #remove estimates with cv > 1
    cvsummary2 <- subset(out, Pop_cv >=0) # remove estimates with cv <0


    pow_plot.1 <- ggplot(cvsummary2)+ #Plot model and cv
      geom_jitter(aes(Effort, Pop_cv), colour = "red") + geom_smooth(aes(Effort, Pop_cv), method = lm, formula=y~log(x))  + ylim(low=0, high=1.0) +
      geom_smooth(aes(Effort, cv),colour="black", linetype="dashed",method = lm, se=FALSE)  +
      labs(x="Effort (km)", y= "Coefficient of variation")+
      scale_color_manual(name="Legend", values=c("blue", "black"))

    mod <- lm(Pop_cv ~ log(Effort+1), data = out) #create linear model with log fn
    pow_plot.2 <- plotFit(mod, interval ="prediction", shade=TRUE, col.pred = "lightblue") #plot the prediction intervals for the model
    req_effort <- exp((cal <- calibrate(mod, y0=desired_cv,interval="inversion"))$estimate) #Calculate the required effort to achieve desired_cv
    print(req_effort)
    list(pow_plot.1=pow_plot.1, mod, pow_plot.2=mod, pow_plot.2, req_effort=req_effort)
  })

  output$POW_TXT   <- renderText({paste("This analysis fits a model to the iterative resampling of your moose survey transects.The estimated additional effort to achieve the desired CV of ",input$DesiredCV, " is ", (round(as.numeric(as.character(POW()$req_effort,1))) - round(as.numeric(as.character(OL()$ddf.1.moos$dht$individuals$summary$Effort[1])),1)), "km.")})
  output$MOOS_POW1 <- renderPlot({plot(POW()$pow_plot.1) })
  output$MOOS_POW2 <- renderPlot({plot(POW()$pow_plot.2) })


  # # Insert the right number of plot output objects into the web page
  # output$MOOS_AIRDF <- renderUI({
  #   plot_output_list <- lapply(1:length(OL()$U.list), function(i) {
  #     plotname <- paste("QQplot", i, sep="")
  #     plotOutput(plotname, height = 280, width = 250)})
  #
  #   # Convert the list to a tagList - this is necessary for the list of items
  #   # to display properly.
  #   do.call(tagList, plot_output_list)
  #   # Call renderPlot for each one. Plots are only actually generated when they
  #   # are visible on the web page.
  #   for (i in 1:length(OL()$U.list)) {
  #     # Need local so that each item gets its own number. Without it, the value
  #     # of i in the renderPlot() will be the same across all instances, because
  #     # of when the expression is evaluated.
  #     local({
  #       my_i <- i
  #       plotname <- paste("plot", my_i, sep="")
  #
  #       output[[plotname]] <- renderPlot({plot(ds(OL()$U.list[[i]], key="hn", adjustment = "cos", truncation = 425), main= paste("Detection function for aircraft:", unique(U.list[[i]]$Aircraft)))})
  #     })
  #   }
  # })


})
