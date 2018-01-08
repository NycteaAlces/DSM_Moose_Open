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

  OL <- eventReactive(list(input$MegaDB$datapath,input$truncation), { ####----

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
    strat_num <- nrow(strat)
    results_num_index <- as.integer(strat_num) + 1
    datasheet <- as.data.frame(sqlQuery(myconn, "select * from datasheet"))
    close(myconn)
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
    transflown$Covariate.1 <- " "
    transflown$Covariate.2 <- " "
    transflown <- unique(transflown)

    datasheet.md <- datasheet
    datasheet.md$MUDE.GroupSize <- sum(datasheet$MUDE.Doe,  datasheet$MUDE.Fawn, datasheet$MUDE.Buck.NA, datasheet$MUDE.Buck.S, datasheet$MUDE.Buck.M, datasheet$MUDE.Buck.L, datasheet$MUDE.UC, datasheet$MUDE.UC.Adult)
    datasheet.md$WTDE.GroupSize <- sum(datasheet$WTDE.Doe,  datasheet$WTDE.Fawn, datasheet$WTDE.Buck.NA, datasheet$WTDE.Buck.S, datasheet$WTDE.Buck.M, datasheet$WTDE.Buck.L, datasheet$WTDE.UC, datasheet$WTDE.UC.Adult)
    datasheet.md$WAPT.GroupSize <- sum(datasheet$WAPT.Cow,  datasheet$WTDE.Fawn, datasheet$WTDE.Buck.NA, datasheet$WTDE.Buck.S, datasheet$WTDE.Buck.M, datasheet$WTDE.Buck.L, datasheet$WTDE.UC, datasheet$WTDE.UC.Adult)
    
    
    datasheet.2 <- unique(datasheet[ which(datasheet$MOOS.GroupSize >0),])
    DistancePreInput.MOOS.2 <- anti_join(transflown, datasheet.2, by=c("Transect.ID","Stratum"))
    DistancePreInput.MOOS.2 <- unique(DistancePreInput.MOOS.2)
    DistancePreInput.MOOS <- merge(datasheet.2, DistancePreInput.MOOS.2, all=T)
    DistancePreInput.MOOS <- unique(DistancePreInput.MOOS)
    DistanceInput<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.MOOS$ID), Region.Label= DistancePreInput.MOOS$Stratum,Area = as.numeric(DistancePreInput.MOOS$Stratum.Area), TID = as.numeric(DistancePreInput.MOOS$Transect.ID), TLENGTH = as.numeric(DistancePreInput.MOOS$Transect.Length), Effort=as.numeric(DistancePreInput.MOOS$Length)/1000, distance= as.numeric(DistancePreInput.MOOS$DistancePerp), size=as.numeric(DistancePreInput.MOOS$MOOS.GroupSize),CC=as.factor(DistancePreInput.MOOS$Covariate.1), Activity=as.factor(DistancePreInput.MOOS$Covariate.2)))
    DistanceInput <- DistanceInput[ order(DistanceInput$Region.Label, DistanceInput$TID, DistanceInput$size), ]

    datasheet.2.MD <- unique(datasheet.md[ which(datasheet$MUDE.GroupSize >0),])
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

    
    datasheet.2.WT <- unique(datasheet.md[ which(datasheet$WTDE.GroupSize >0),])
    DistancePreInput.WTDE.2 <- anti_join(transflown, datasheet.2.WT, by=c("Transect.ID","Stratum"))
    DistancePreInput.WTDE.2 <- unique(DistancePreInput.WTDE.2)
    DistancePreInput.WTDE <- merge(datasheet.2.WT, DistancePreInput.WTDE.2, all=T)
    DistancePreInput.WTDE <- unique(DistancePreInput.WTDE)
    DistanceInput.WT<- as.data.frame(cbind(object.ID = as.numeric(DistancePreInput.WTDE$ID), Region.Label= DistancePreInput.WTDE$Stratum,Area = as.numeric(DistancePreInput.WTDE$Stratum.Area), TID = as.numeric(DistancePreInput.WTDE$Transect.ID), TLENGTH = as.numeric(DistancePreInput.WTDE$Transect.Length), Effort=as.numeric(DistancePreInput.WTDE$Length)/1000, distance= as.numeric(DistancePreInput.WTDE$DistancePerp), size=as.numeric(DistancePreInput.WTDE$WTDE.GroupSize),CC=as.factor(DistancePreInput.WTDE$Covariate.1), Activity=as.factor(DistancePreInput.WTDE$Covariate.2)))
    DistanceInput.WT <- DistanceInput.MD[ order(DistanceInput.WT$Region.Label, DistanceInput.WT$TID, DistanceInput.WT$size), ]
    DistanceInput2.WT <- as.data.frame(cbind(object = as.numeric(DistancePreInput.WTDE$ID), Region.Label= DistancePreInput.WTDE$Stratum,Area = as.numeric(DistancePreInput.WTDE$Stratum.Area), Sample.Label = as.numeric(DistancePreInput.WTDE$Transect.ID), Effort = as.numeric(DistancePreInput.WTDE$Transect.Length), distance= as.numeric(DistancePreInput.WTDE$DistancePerp), size=as.numeric(DistancePreInput.WTDE$WTDE.GroupSize),CC=as.factor(DistancePreInput.WTDE$Covariate.1), Activity=as.factor(DistancePreInput.WTDE$Covariate.2)))
    DistanceInput2.WT <- unique(DistanceInput2.MD)
    # Copy.df <- DistancePreInput.MOOS
    # Copy.df$DistancePerp <- as.numeric(Copy.df$DistancePerp)
    # Copy.df$MOOS.GroupSize <- as.numeric(Copy.df$MOOS.GroupSize)
    # CleanHistData <- Copy.df[!(Copy.df$DistancePerp==""),]
    # CleanHistData <- Copy.df[!is.na(Copy.df$DistancePerp),]
    # CleanHistData <- subset(CleanHistData, DistancePerp <= 600)
    # CleanHistData["distance"] <- CleanHistData$DistancePerp
    # CleanHistData["object"] <- seq.int(nrow(CleanHistData))
    # U.list <- split(CleanHistData, CleanHistData$Aircraft)

    ddf.1.mude <- ds(DistanceInput2.MD, key="hn", adjustment = "cos", truncation = list(left=0, right=truncvalue()))
    Calf_n <-  sum(datasheet$MOOS.Calf)
    Cow_n <- sum(datasheet$MOOS.Cow)
    Calf_ratio <- Calf_n / Cow_n
    Bull_n <- sum(sum(datasheet$MOOS.Bull.N), sum(datasheet$MOOS.Bull.S), sum(datasheet$MOOS.Bull.M), sum(datasheet$MOOS.Bull.L))
    Bull_ratio <- Bull_n / Cow_n
    MOOS_n <- sum(datasheet$MOOS.Cow, datasheet$MOOS.Calf, datasheet$MOOS.Bull.NA, datasheet$MOOS.Bull.S, datasheet$MOOS.Bull.M, datasheet$MOOS.Bull.L, datasheet$MOOS.UC, datasheet$MOOS.UC.Adult)
    WTD_n  <- sum(datasheet$WTDE.Doe,  datasheet$WTDE.Fawn, datasheet$WTDE.Buck.NA, datasheet$WTDE.Buck.S, datasheet$WTDE.Buck.M, datasheet$WTDE.Buck.L, datasheet$WTDE.UC, datasheet$WTDE.UC.Adult)
    MUDE_n <- sum(datasheet$MUDE.Doe,  datasheet$MUDE.Fawn, datasheet$MUDE.Buck.NA, datasheet$MUDE.Buck.S, datasheet$MUDE.Buck.M, datasheet$MUDE.Buck.L, datasheet$MUDE.UC, datasheet$MUDE.UC.Adult)
    WAPT_n <-  sum(datasheet$WAPT.Cow, datasheet$WAPT.Calf, datasheet$WAPT.Bull.NA, datasheet$WAPT.Bull.S, datasheet$WAPT.Bull.M, datasheet$WAPT.Bull.L, datasheet$WAPT.UC, datasheet$WAPT.UC.Adult)
    WAPT_n <-  sum(is.numeric(datasheet$WAPT.Cow), is.numeric( datasheet$WAPT.Calf), is.numeric( datasheet$WAPT.Bull.NA), is.numeric( datasheet$WAPT.Bull.S), is.numeric( datasheet$WAPT.Bull.M), is.numeric( datasheet$WAPT.Bull.L), is.numeric( datasheet$WAPT.UC), is.numeric( datasheet$WAPT.UC.Adult))
    #k <- 425
    #    moos.hn_cos_2 <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos", order = 2)
    #    moos.hn_cos_3 <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos", order = 3)
    moos.hn_cos  <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos")
    #    moos.hn_herm <- ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "herm")
    #    moos.hr_herm <- ds(DistanceInput2, truncation = truncvalue(), key="hr", adjustment = "herm")
    moos.hr_cos  <- ds(DistanceInput2, truncation = truncvalue(), key="hr", adjustment = "cos")
    moos.hr_poly  <- ds(DistanceInput2, truncation = truncvalue(), key="hr", adjustment = "poly")
    moos.unif_cos <- ds(DistanceInput2, truncation = truncvalue(), key="unif", adjustment = "cos")
    mlist <- list(moos.hn_cos, moos.hr_cos, moos.hr_poly, moos.unif_cos)
    modelnum <- length(mlist)
    model_results <- list()
    for (i in 1:modelnum) { #return results for each candidate moose model such that they can simply be accessed for reporting in global env
      Vector <- numeric(9) #create vector
      Vector[1] <- mlist[[i]]$ddf$dsmodel[2] #model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mlist[[i]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mlist[[i]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      model_results[[i]] <- Vector
    }
    model_result_df <- as.data.frame(do.call("rbind", model_results))
    colnames(model_result_df) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl")
  #  best.moos <- model_result_df[which.min(model_result_df$AIC),]

    ddf.1.moos<-ds(DistanceInput2, truncation = truncvalue(), key="hn", adjustment = "cos")
    list(datasheet=datasheet,
         ddf.1.moos = ddf.1.moos ,
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
         ddf.1.mude=ddf.1.mude,
         DistanceInput2.WT=DistanceInput2.WT)
  })

  OL.MD <- eventReactive(OL(), {
    
    Calf_n.MD <-  sum(OL()$datasheet$MUDE.Fawn)
    Cow_n.MD <- sum(OL()$datasheet$MUDE.Doe)
    Calf_ratio.MD <- Calf_n.MD / Cow_n.MD
    Bull_n.MD <- sum(sum(OL()$datasheet$MUDE.Buck.NA), sum(OL()$datasheet$MUDE.Buck.S), sum(OL()$datasheet$MUDE.Buck.M), sum(OL()$datasheet$MUDE.Buck.L))
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
    results_num_index <- as.integer(OL()$strat_num) + 1
    mdlist <-     list(mude.hn_cos,mude.hn_herm, mude.hr_herm, mude.hr_cos, mude.hr_poly, mude.unif_cos)
    modelnum.md <- length(mdlist)
    model_results.md <- list()
    for (j in 1:modelnum.md) {
      if(is.null(mdlist[[j]])){next}
      Vector <- numeric(9) #create vector
      Vector[1] <- mdlist[[j]]$ddf$dsmodel[2] #model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mdlist[[j]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      model_results.md[[j]] <- Vector
    }
    
    model_result_df.MUDE <- as.data.frame(do.call("rbind", model_results.md))
    colnames(model_result_df.MUDE) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl")
    best.mude <- model_result_df.MUDE[which.min(model_result_df.MUDE$AIC),]
 #   ddf.1.mude<-ds(OL()$DistanceInput2.MD, truncation = truncvalue(), key="hn", adjustment = "cos")
    list(model_result_df.MUDE=model_result_df.MUDE, ddf.1.mude=mude.hn_cos, best.mude = best.mude, results_num_index=results_num_index, Calf_ratio.MD=Calf_ratio.MD, Bull_ratio.MD=Bull_ratio.MD, Bull_n.MD=Bull_n.MD)
  })
  
  OL.WT <- eventReactive(OL(), {
    
    Calf_n.WT <-  sum(OL()$datasheet$WTDE.Fawn)
    Cow_n.WT <- sum(OL()$datasheet$WTDE.Doe)
    Calf_ratio.WT <- Calf_n.WT / Cow_n.WT
    Bull_n.WT <- sum(sum(OL()$datasheet$WTDE.Buck.NA), sum(OL()$datasheet$WTDE.Buck.S), sum(OL()$datasheet$WTDE.Buck.M), sum(OL()$datasheet$WTDE.Buck.L))
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
    results_num_index <- as.integer(OL()$strat_num) + 1
    mdlist <-     list(WTDE.hn_cos,WTDE.hn_herm, WTDE.hr_herm, WTDE.hr_cos, WTDE.hr_poly, WTDE.unif_cos)
    modelnum.md <- length(mdlist)
    model_results.md <- list()
    j=0
    for (j in 1:modelnum.md) {
      if(is.null(mdlist[[j]])){next}
      Vector <- numeric(9) #create vector
      Vector[1] <- mdlist[[j]]$ddf$dsmodel[2] #model description (placeholder for more appropriate model names)
      Vector[2] <- as.vector(as.numeric(round(mdlist[[j]]$ddf$criterion,2))) #AIC value (uncorrected - currently, need to include small smaple size adjustment)
      Vector[3] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$Estimate[results_num_index]*1000,0))) #Population estimate for study area (all strata)
      Vector[4] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$cv[results_num_index],3))) #Coefficient of variation for the population estimate
      Vector[5] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$lcl[results_num_index]*1000,0))) #lower 95% confidence interval for the population estimate
      Vector[6] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$N$ucl[results_num_index]*1000,0))) #upper 95% confidence interval for the population estimate
      Vector[7] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$Estimate[results_num_index]*1000,2))) #Density estimate for the study area (all strata)
      Vector[8] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$lcl[results_num_index]*1000,2))) #lower 95% confidence interval for the density estimate
      Vector[9] <- as.vector(as.numeric(round(mdlist[[j]]$dht$individuals$D$ucl[results_num_index]*1000,2))) #upper 95% confidence interval for the density estimate
      model_results.md[[j]] <- Vector
    }
    
    model_result_df.MUDE <- as.data.frame(do.call("rbind", model_results.md))
    colnames(model_result_df.MUDE) <- c("Model description", "AIC", "Nhat", "CV", "Nlcl", "Nucl", "Dhat", "Dlcl", "Ducl")
    best.mude <- model_result_df.MUDE[which.min(model_result_df.MUDE$AIC),]
  #  ddf.1.mude<-ds(OL()$DistanceInput2.WT, truncation = truncvalue(), key="hn", adjustment = "cos")
    list(model_result_df.wtde=model_result_df.MUDE, ddf.1.wtde=WTDE.hn_cos, best.wtde = best.mude, results_num_index=results_num_index, Calf_ratio.WT=Calf_ratio.WT, Bull_ratio.WT=Bull_ratio.WT, Bull_n.WT=Bull_n.WT)
  })

  output$MOOS_DF <- renderPlot({ plot(OL()$ddf.1.moos, main=paste("Global detection function for moose, HN-Cos, truncation=",OL()$truncvalue[1]))})
  output$MOOS_QQ <- renderPlot({ ddf.gof(OL()$ddf.1.moos$ddf) })
  output$MUDE_QQ <- renderPlot({ ddf.gof(OL()$ddf.1.mude$ddf) })
  output$MOOS_TAB = DT::renderDataTable(OL()$model_result_df, options = list(lengthChange=FALSE), caption=paste("Table 1. Model results for candidate set of default models for moose. Truncation distance was ",OL()$ddf.1.moos$ddf$meta.data$width) )
  output$MOOS_MAP <- renderPlot({
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
  output$MOOS_TXT = renderText({paste("The survey included", round(OL()$ddf.1.moos$dht$individuals$summary$Effort[1],1), "km of transects (n= ", nrow(OL()$transflown), " mean transect length = ",
                                      round(OL()$ddf.1.moos$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),"km) that were sampled across a total of ", OL()$strat_num,
                                      "strata. There were an estimated ", round(OL()$ddf.1.moos$dht$individuals$N$Estimate[OL()$strat_num+1]*1000, 0),
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
  output$MD_DF <- renderPlot({plot(OL()$ddf.1.mude, main=("Global detection function for mule deer, HN-Cos, no truncation"))})
  output$MUDE_QQ <- renderPlot({ ddf.gof(OL()$ddf.1.mude$ddf) })
  output$WTDE_TXT = renderText({
    paste("The survey included",
          round(OL.WT()$ddf.1.wtde$dht$individuals$summary$Effort[1],1),
          "km of transects (n= ", nrow(OL()$transflown),
          " mean transect length = ",
          round(OL.WT()$ddf.1.wtde$dht$individuals$summary$Effort[1]/ nrow(OL()$transflown), 2),
          "km) that were sampled across a total of ", OL()$strat_num,
          "strata. There were an estimated ",
          round(OL.WT()$ddf.1.wtde$dht$individuals$N$Estimate[OL.WT()$results_num_index]*1000, 0),
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
          round((sum(OL()$datasheet$WTDE.Buck.NA)/OL()$Bull_n.WT)*100,1),
          "% had already shed their antlers. Of those bulls still with antlers, ",
          round((sum(OL()$datasheet$WTDE.Bull.S)/(OL.WT()$Bull_n.WT - sum(OL()$datasheet$WTDE.Bull.NA)))*100,1), "% were small,",
          round((sum(OL()$datasheet$WTDE.Bull.M)/(OL.WT()$Bull_n.WT - sum(OL()$datasheet$WTDE.Bull.NA)))*100,1), "% were medium, and ",
          round((sum(OL()$datasheet$WTDE.Bull.L)/(OL.WT()$Bull_n.WT - sum(OL()$datasheet$WTDE.Bull.NA)))*100,1), "% were large. In addition to mule deer, ",
          OL()$WTD_n, " White-tailed deer, ", OL()$MOOS_n, " Mule Deer, and ", OL()$WAPT_n," elk were observed during the survey. ")
  })
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
          round(OL.MD()$ddf.1.mude$dht$individuals$N$Estimate[OL.MD()$results_num_index]*1000, 0),
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
          round((sum(OL()$datasheet$MUDE.Buck.NA)/OL()$Bull_n.MD)*100,1),
          "% had already shed their antlers. Of those bulls still with antlers, ",
          round((sum(OL()$datasheet$MUDE.Bull.S)/(OL.MD()$Bull_n.MD - sum(OL()$datasheet$MUDE.Bull.NA)))*100,1), "% were small,",
          round((sum(OL()$datasheet$MUDE.Bull.M)/(OL.MD()$Bull_n.MD - sum(OL()$datasheet$MUDE.Bull.NA)))*100,1), "% were medium, and ",
          round((sum(OL()$datasheet$MUDE.Bull.L)/(OL.MD()$Bull_n.MD - sum(OL()$datasheet$MUDE.Bull.NA)))*100,1), "% were large. In addition to mule deer, ",
          OL()$WTD_n, " White-tailed deer, ", OL()$MOOS_n, " Mule Deer, and ", OL()$WAPT_n," elk were observed during the survey. ")

   
  })
  output$MD_DF <- renderPlot({plot(OL()$ddf.1.mude, main=("Global detection function for mule deer, HN-Cos, no truncation"))})
  output$MOOS_TAB1 = DT::renderDataTable(OL()$ddf.1.moos$dht$clusters$summary, options= list(lengthChange=F), caption ="Table 2. Results of distance sampling estimates, by strata and combined. ") #%>% formatRound(digits=1))
  output$MUDE_TAB = DT::renderDataTable(OL.MD()$model_result_df.MUDE, options = list(lengthChange=FALSE), caption=paste("Table 1. Model results for candidate set of default models for mule deer. Truncation distance was ",OL()$truncvalue))
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
  # knitr::kable(OL()$best.moos$dht$clusters$D)
  # knitr::kable(OL()$best.moos$dht$clusters$N)
  # knitr::kable(OL()$best.moos$dht$clusters$average.p)
  # knitr::kable(OL()$best.moos$dht$clusters$Expected.S)
  # knitr::kable(OL()$best.moos$dht$clusters$summary)
  
  })

