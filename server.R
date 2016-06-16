library(dygraphs)
library(datasets)
library(xts)
#library(rdrop2)

ph <- as.character(Sys.info()["nodename"])

shinyServer(function(input,output) {
  
  token <- readRDS("droptoken.rds")
  
  # uz <- c("modoek-data.251.RData","modoek-data.252.RData",
  #         "modoek-data.530.RData","modoek-data.temp251.RData",
  #         "modoek-data.temp252.RData","modoek-data.temp530.RData")
  # for(hh in uz) {
  #   drop_get(path=paste0("modoek/",hh),
  #            local_file=paste0("/tmp/",hh),
  #            dtoken=token,overwrite=TRUE)
  # }
  
  if(ph=="shiny05") {  
   # drop_get("modoek/main.RData","/tmp/main.RData",dtoken=token,overwrite=TRUE)
   # system("chmod 777 /tmp/main.RData")
    load("/tmp/main.RData")
  } else {
   # drop_get("modoek/main.RData",dtoken=token,overwrite=TRUE)
    load("main.RData")
  }
  
  ## RWC #######################################################################
  ##############################################################################
  
  # dat.251 <- read.csv("/tmp/modoek-data.251.csv",sep=";",
  #                     stringsAsFactors=FALSE)
  # load("/tmp/modoek-data.251.RData"); dat.251rwc <- d
  # head(dat.251rwc)
  dat.251rwc <- kk[[which(names(kk)=="modoek-data.251.RData")]]
  # dat.252 <- read.csv("/tmp/modoek-data.252.csv",sep=";",
  #                     stringsAsFactors=FALSE)
  # load("/tmp/modoek-data.252.RData"); dat.252rwc <- d
  dat.252rwc <- kk[[which(names(kk)=="modoek-data.252.RData")]]
  dat.25rwc <- cbind(dat.251rwc,dat.252rwc[,2:33])
  names(dat.25rwc)[1] <- "ts"
  
  dat.25rwc$ts <- as.POSIXct(strptime(dat.25rwc$ts,format="%d.%m.%Y %H:%M"),
                             tz="UTC")
  tod <- Sys.time(); ti <- format(tod,tz="UTC")
  ih <- round(as.numeric(difftime(ti,tod,units="hours"))*2,1)/2
  dat.25rwc$ts <- dat.25rwc$ts+ih
  
  names(dat.25rwc) <- 
    c("ts","1.1NW.25","1.1SE.25","1.2NW.25","1.2SE.25","1.3NW.25","1.3SE.25",
      "1.4NW.25","1.4SE.25","2.1NW.25","2.1SE.25","2.2NW.25","2.2SE.25",
      "2.3NW.25","2.3SE.25","2.4NW.25","2.4SE.25","3.1NW.25","3.1SE.25",
      "3.2NW.25","3.2SE.25","3.3NW.25","3.3SE.25","3.4NW.25","3.4SE.25",
      "4.1NW.25","4.1SE.25","4.2NW.25","4.2SE.25","4.3NW.25","4.3SE.25",
      "4.4NW.25","4.4SE.25","1.1NE.25","1.1SW.25","1.2NE.25","1.2SW.25",
      "1.3NE.25","1.3SW.25","1.4NE.25","1.4SW.25","2.1NE.25","2.1SW.25",
      "2.2NE.25","2.2SW.25","2.3NE.25","2.3SW.25","2.4NE.25","2.4SW.25",
      "3.1NE.25","3.1SW.25","3.2NE.25","3.2SW.25","3.3NE.25","3.3SW.25",
      "3.4NE.25","3.4SW.25","4.1NE.25","4.1SW.25","4.2NE.25","4.2SW.25",
      "4.3NE.25","4.3SW.25","4.4NE.25","4.4SW.25")
  
  P1 <-  c("1.1SW.25","2.4NE.25","3.3NE.25","4.2SE.25","1.1NE.25",
           "2.4SW.25","3.3SW.25","4.2NW.25")
  P2 <- c("1.3SE.25","2.1NW.25","3.2NE.25","4.4SW.25","1.3NW.25",
          "2.1SE.25","3.2SW.25","4.4NE.25")
  P3 <- c("1.4NW.25","2.2SW.25","3.1SE.25","4.3NE.25","1.4SE.25",
          "2.2NE.25","3.1NW.25","4.3SW.25")
  P4 <- c("1.2NE.25","2.3SE.25","3.4SW.25","4.1NW.25","1.2SW.25",
          "2.3NW.25","3.4NE.25","4.1SE.25")
  O1 <-  c("1.1SE.25","2.4NW.25","3.3NW.25","4.2SW.25","1.1NW.25",
           "2.4SE.25","3.3SE.25","4.2NE.25")
  O2 <- c("1.3SW.25","2.1NE.25","3.2NW.25","4.4SE.25","1.3NE.25",
          "2.1SW.25","3.2SE.25","4.4NW.25")
  O3 <- c("1.4NE.25","2.2SE.25","3.1SW.25","4.3NW.25","1.4SW.25",
          "2.2NW.25","3.1NE.25","4.3SE.25")
  O4 <- c("1.2NW.25","2.3SW.25","3.4SE.25","4.1NE.25","1.2SE.25",
          "2.3NE.25","3.4NW.25","4.1SW.25")
  
  meansrwc <- data.frame(Time=dat.25rwc$ts,P1=rowMeans(dat.25rwc[,P1]),
                         P2=rowMeans(dat.25rwc[,P2]),
                         P3=rowMeans(dat.25rwc[,P3]),
                         P4=rowMeans(dat.25rwc[,P4]),
                         O1=rowMeans(dat.25rwc[,O1]),
                         O2=rowMeans(dat.25rwc[,O2]),
                         O3=rowMeans(dat.25rwc[,O3]),
                         O4=rowMeans(dat.25rwc[,O4]))
  if(format(meansrwc$Time[1],"%S")==58) {meansrwc$Time <- meansrwc$Time+2}
  
  ## fig1 ######################################################################
  output$fig1 <- renderDygraph({
    P1 <- meansrwc[,c("Time","P1")]; P1 <- xts(P1$P1,P1$Time)
    P2 <- meansrwc[,c("Time","P2")]; P2 <- xts(P2$P2,P2$Time)
    P3 <- meansrwc[,c("Time","P3")]; P3 <- xts(P3$P3,P3$Time)
    P4 <- meansrwc[,c("Time","P4")]; P4 <- xts(P4$P4,P4$Time)
    xx <- cbind(P1,P2,P3,P4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Pine Soil RWC 25 cm",group="MODOEK",
            xlab="Time",ylab="Mean RWC [%]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,50)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLimit(6.1,color="grey") %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig2 ######################################################################
  output$fig2 <- renderDygraph({
    O1 <- meansrwc[,c("Time","O1")]; O1 <- xts(O1$O1,O1$Time)
    O2 <- meansrwc[,c("Time","O2")]; O2 <- xts(O2$O2,O2$Time)
    O3 <- meansrwc[,c("Time","O3")]; O3 <- xts(O3$O3,O3$Time)
    O4 <- meansrwc[,c("Time","O4")]; O4 <- xts(O4$O4,O4$Time)
    xx <- cbind(O1,O2,O3,O4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Oak Soil RWC 25 cm",group="MODOEK",
            xlab="Time",ylab="Mean RWC [%]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,50)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLimit(6.1,color="grey") %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  # dat.530rwc <- read.csv("/tmp/modoek-data.530.csv",sep=";",
  #                        stringsAsFactors=FALSE)
  # load("/tmp/modoek-data.530.RData"); dat.530rwc <- d
  dat.530rwc <- kk[[which(names(kk)=="modoek-data.530.RData")]]
  dat.530rwc <- dat.530rwc[,c(1,4,5,6,7,12,13,14,15,20,21,22,23,28,29,30,31)]
  names(dat.530rwc)[1] <- "ts"
  dat.530rwc$ts <- as.POSIXct(strptime(dat.530rwc$ts,
                                       format="%d.%m.%Y %H:%M"),tz="UTC")
  
  tod <- Sys.time(); ti <- format(tod,tz="UTC")
  ih <- round(as.numeric(difftime(ti,tod,units="hours"))*2,1)/2
  dat.530rwc$ts <- dat.530rwc$ts+ih
  
  names(dat.530rwc) <- c("Time","O4.5cm","O4.30cm","O2.5cm","O2.30cm","P3.5cm",
                         "P3.30cm","P4.5cm","P4.30cm","P2.5cm","P2.30cm",
                         "P1.5cm", "P1.30cm","O1.5cm","O1.30cm","O3.5cm",
                         "O3.30cm")
  if(format(dat.530rwc$Time[1],"%S")==58) {
    dat.530rwc$Time <- dat.530rwc$Time+2
  }
  
  ## fig3 ######################################################################
  output$fig3 <- renderDygraph({
    P1 <- dat.530rwc[,c("Time","P1.5cm")]; P1 <- xts(P1$P1.5cm,P1$Time)
    P2 <- dat.530rwc[,c("Time","P2.5cm")]; P2 <- xts(P2$P2.5cm,P2$Time)
    P3 <- dat.530rwc[,c("Time","P3.5cm")]; P3 <- xts(P3$P3.5cm,P3$Time)
    P4 <- dat.530rwc[,c("Time","P4.5cm")]; P4 <- xts(P4$P4.5cm,P4$Time)
    xx <- cbind(P1,P2,P3,P4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Pine Soil RWC 5 cm",group="MODOEK",
            xlab="Time",ylab="Mean RWC [%]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,50)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLimit(6.1,color="grey") %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig4 ######################################################################
  output$fig4 <- renderDygraph({
    P1 <- dat.530rwc[,c("Time","P1.30cm")]; P1 <- xts(P1$P1.30cm,P1$Time)
    P2 <- dat.530rwc[,c("Time","P2.30cm")]; P2 <- xts(P2$P2.30cm,P2$Time)
    P3 <- dat.530rwc[,c("Time","P3.30cm")]; P3 <- xts(P3$P3.30cm,P3$Time)
    P4 <- dat.530rwc[,c("Time","P4.30cm")]; P4 <- xts(P4$P4.30cm,P4$Time)
    xx <- cbind(P1,P2,P3,P4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Pine Soil RWC 30 cm",group="MODOEK",
            xlab="Time",ylab="Mean RWC [%]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,50)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLimit(6.1,color="grey") %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig5 ######################################################################
  output$fig5 <- renderDygraph({
    O1 <- dat.530rwc[,c("Time","O1.5cm")]; O1 <- xts(O1$O1.5cm,O1$Time)
    O2 <- dat.530rwc[,c("Time","O2.5cm")]; O2 <- xts(O2$O2.5cm,O2$Time)
    O3 <- dat.530rwc[,c("Time","O3.5cm")]; O3 <- xts(O3$O3.5cm,O3$Time)
    O4 <- dat.530rwc[,c("Time","O4.5cm")]; O4 <- xts(O4$O4.5cm,O4$Time)
    xx <- cbind(O1,O2,O3,O4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Oak Soil RWC 5 cm",group="MODOEK",
            xlab="Time",ylab="Mean RWC [%]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,50)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLimit(6.1,color="grey") %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig6 ######################################################################
  output$fig6 <- renderDygraph({
    O1 <- dat.530rwc[,c("Time","O1.30cm")]; O1 <- xts(O1$O1.30cm,O1$Time)
    O2 <- dat.530rwc[,c("Time","O2.30cm")]; O2 <- xts(O2$O2.30cm,O2$Time)
    O3 <- dat.530rwc[,c("Time","O3.30cm")]; O3 <- xts(O3$O3.30cm,O3$Time)
    O4 <- dat.530rwc[,c("Time","O4.30cm")]; O4 <- xts(O4$O4.30cm,O4$Time)
    xx <- cbind(O1,O2,O3,O4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Oak Soil RWC 30 cm",group="MODOEK",
            xlab="Time",ylab="Mean RWC [%]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,50)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLimit(6.1,color="grey") %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## Temp ######################################################################
  ## (excalty the same as above, just for temps! ###############################
  
  # dat.251temp <- read.csv("/tmp/modoek-data.temp251.csv",sep=";",
  #                         stringsAsFactors=FALSE)
  # dat.252temp <- read.csv("/tmp/modoek-data.temp252.csv",sep=";",
  #                         stringsAsFactors=FALSE)
  # load("/tmp/modoek-data.temp251.RData"); dat.251temp <- d
  dat.251temp <- kk[[which(names(kk)=="modoek-data.temp251.RData")]]
  # load("/tmp/modoek-data.temp252.RData"); dat.252temp <- d
  dat.252temp <- kk[[which(names(kk)=="modoek-data.temp252.RData")]]
  dat.25temp <- cbind(dat.251temp,dat.252temp[,2:33])
  names(dat.25temp)[1] <- "ts"
  
  dat.25temp$ts <- as.POSIXct(strptime(dat.25temp$ts,format="%d.%m.%Y %H:%M"),
                              tz="UTC")
  tod <- Sys.time(); ti <- format(tod,tz="UTC")
  ih <- round(as.numeric(difftime(ti,tod,units="hours"))*2,1)/2
  dat.25temp$ts <- dat.25temp$ts+ih
  
  names(dat.25temp) <- 
    c("ts","1.1NW.25","1.1SE.25","1.2NW.25","1.2SE.25","1.3NW.25","1.3SE.25",
      "1.4NW.25","1.4SE.25","2.1NW.25","2.1SE.25","2.2NW.25","2.2SE.25",
      "2.3NW.25","2.3SE.25","2.4NW.25","2.4SE.25","3.1NW.25","3.1SE.25",
      "3.2NW.25","3.2SE.25","3.3NW.25","3.3SE.25","3.4NW.25","3.4SE.25",
      "4.1NW.25","4.1SE.25","4.2NW.25","4.2SE.25","4.3NW.25","4.3SE.25",
      "4.4NW.25","4.4SE.25","1.1NE.25","1.1SW.25","1.2NE.25","1.2SW.25",
      "1.3NE.25","1.3SW.25","1.4NE.25","1.4SW.25","2.1NE.25","2.1SW.25",
      "2.2NE.25","2.2SW.25","2.3NE.25","2.3SW.25","2.4NE.25","2.4SW.25",
      "3.1NE.25","3.1SW.25","3.2NE.25","3.2SW.25","3.3NE.25","3.3SW.25",
      "3.4NE.25","3.4SW.25","4.1NE.25","4.1SW.25","4.2NE.25","4.2SW.25",
      "4.3NE.25","4.3SW.25","4.4NE.25","4.4SW.25")
  
  P1 <-  c("1.1SW.25","2.4NE.25","3.3NE.25","4.2SE.25","1.1NE.25",
           "2.4SW.25","3.3SW.25","4.2NW.25")
  P2 <- c("1.3SE.25","2.1NW.25","3.2NE.25","4.4SW.25","1.3NW.25",
          "2.1SE.25","3.2SW.25","4.4NE.25")
  P3 <- c("1.4NW.25","2.2SW.25","3.1SE.25","4.3NE.25","1.4SE.25",
          "2.2NE.25","3.1NW.25","4.3SW.25")
  P4 <- c("1.2NE.25","2.3SE.25","3.4SW.25","4.1NW.25","1.2SW.25",
          "2.3NW.25","3.4NE.25","4.1SE.25")
  O1 <-  c("1.1SE.25","2.4NW.25","3.3NW.25","4.2SW.25","1.1NW.25",
           "2.4SE.25","3.3SE.25","4.2NE.25")
  O2 <- c("1.3SW.25","2.1NE.25","3.2NW.25","4.4SE.25","1.3NE.25",
          "2.1SW.25","3.2SE.25","4.4NW.25")
  O3 <- c("1.4NE.25","2.2SE.25","3.1SW.25","4.3NW.25","1.4SW.25",
          "2.2NW.25","3.1NE.25","4.3SE.25")
  O4 <- c("1.2NW.25","2.3SW.25","3.4SE.25","4.1NE.25","1.2SE.25",
          "2.3NE.25","3.4NW.25","4.1SW.25")
  
  meanstemp <- data.frame(Time=dat.25temp$ts,P1=rowMeans(dat.25temp[,P1]),
                          P2=rowMeans(dat.25temp[,P2]),
                          P3=rowMeans(dat.25temp[,P3]),
                          P4=rowMeans(dat.25temp[,P4]),
                          O1=rowMeans(dat.25temp[,O1]),
                          O2=rowMeans(dat.25temp[,O2]),
                          O3=rowMeans(dat.25temp[,O3]),
                          O4=rowMeans(dat.25temp[,O4]))
  if(format(meanstemp$Time[1],"%S")==58) {meanstemp$ts <- meanstemp$Time+2}
  
  ## fig7 ######################################################################
  output$fig7 <- renderDygraph({
    P1 <- meanstemp[,c("Time","P1")]; P1 <- xts(P1$P1,P1$Time)
    P2 <- meanstemp[,c("Time","P2")]; P2 <- xts(P2$P2,P2$Time)
    P3 <- meanstemp[,c("Time","P3")]; P3 <- xts(P3$P3,P3$Time)
    P4 <- meanstemp[,c("Time","P4")]; P4 <- xts(P4$P4,P4$Time)
    xx <- cbind(P1,P2,P3,P4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Pine Soil Temperature 25 cm",group="MODOEK",
            xlab="Time",ylab="Mean Temperature [°C]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,35)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig8 ######################################################################
  output$fig8 <- renderDygraph({
    O1 <- meanstemp[,c("Time","O1")]; O1 <- xts(O1$O1,O1$Time)
    O2 <- meanstemp[,c("Time","O2")]; O2 <- xts(O2$O2,O2$Time)
    O3 <- meanstemp[,c("Time","O3")]; O3 <- xts(O3$O3,O3$Time)
    O4 <- meanstemp[,c("Time","O4")]; O4 <- xts(O4$O4,O4$Time)
    xx <- cbind(O1,O2,O3,O4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Oak Soil Temperature 25 cm",group="MODOEK",
            xlab="Time",ylab="Mean Temperature [°C]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,35)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  # dat.530temp <- read.csv("/tmp/modoek-data.temp530.csv",sep=";",
  #                         stringsAsFactors=FALSE)
  # load("/tmp/modoek-data.temp530.RData"); dat.530temp <- d
  dat.530temp <- kk[[which(names(kk)=="modoek-data.temp530.RData")]]
  dat.530temp <- dat.530temp[,c(1,4,5,6,7,12,13,14,15,20,21,22,23,28,29,30,31)]
  names(dat.530temp)[1] <- "ts"
  dat.530temp$ts <- as.POSIXct(strptime(dat.530temp$ts,
                                        format="%d.%m.%Y %H:%M"),tz="UTC")
  
  tod <- Sys.time(); ti <- format(tod,tz="UTC")
  ih <- round(as.numeric(difftime(ti,tod,units="hours"))*2,1)/2
  dat.530temp$ts <- dat.530temp$ts+ih
  
  names(dat.530temp) <- c("Time","O4.5cm","O4.30cm","O2.5cm","O2.30cm",
                          "P3.5cm","P3.30cm","P4.5cm","P4.30cm","P2.5cm",
                          "P2.30cm","P1.5cm", "P1.30cm","O1.5cm","O1.30cm",
                          "O3.5cm","O3.30cm")
  if(format(dat.530temp$Time[1],"%S")==58) {
    dat.530temp$Time <- dat.530temp$Time+2
  }
  
  ## fig9 ######################################################################
  output$fig9 <- renderDygraph({
    P1 <- dat.530temp[,c("Time","P1.5cm")]; P1 <- xts(P1$P1.5cm,P1$Time)
    P2 <- dat.530temp[,c("Time","P2.5cm")]; P2 <- xts(P2$P2.5cm,P2$Time)
    P3 <- dat.530temp[,c("Time","P3.5cm")]; P3 <- xts(P3$P3.5cm,P3$Time)
    P4 <- dat.530temp[,c("Time","P4.5cm")]; P4 <- xts(P4$P4.5cm,P4$Time)
    xx <- cbind(P1,P2,P3,P4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Pine Soil Temperature 5 cm",group="MODOEK",
            xlab="Time",ylab="Mean Temperature [°C]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,35)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig10 #####################################################################
  output$fig10 <- renderDygraph({
    P1 <- dat.530temp[,c("Time","P1.30cm")]; P1 <- xts(P1$P1.30cm,P1$Time)
    P2 <- dat.530temp[,c("Time","P2.30cm")]; P2 <- xts(P2$P2.30cm,P2$Time)
    P3 <- dat.530temp[,c("Time","P3.30cm")]; P3 <- xts(P3$P3.30cm,P3$Time)
    P4 <- dat.530temp[,c("Time","P4.30cm")]; P4 <- xts(P4$P4.30cm,P4$Time)
    xx <- cbind(P1,P2,P3,P4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Pine Soil Temperature 30 cm",group="MODOEK",
            xlab="Time",ylab="Mean Temperature [°C]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,35)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig11 #####################################################################
  output$fig11 <- renderDygraph({
    O1 <- dat.530temp[,c("Time","O1.5cm")]; O1 <- xts(O1$O1.5cm,O1$Time)
    O2 <- dat.530temp[,c("Time","O2.5cm")]; O2 <- xts(O2$O2.5cm,O2$Time)
    O3 <- dat.530temp[,c("Time","O3.5cm")]; O3 <- xts(O3$O3.5cm,O3$Time)
    O4 <- dat.530temp[,c("Time","O4.5cm")]; O4 <- xts(O4$O4.5cm,O4$Time)
    xx <- cbind(O1,O2,O3,O4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Oak Soil Temperature 5 cm",group="MODOEK",
            xlab="Time",ylab="Mean Temperature [°C]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,35)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
  ## fig12 #####################################################################
  output$fig12 <- renderDygraph({
    O1 <- dat.530temp[,c("Time","O1.30cm")]; O1 <- xts(O1$O1.30cm,O1$Time)
    O2 <- dat.530temp[,c("Time","O2.30cm")]; O2 <- xts(O2$O2.30cm,O2$Time)
    O3 <- dat.530temp[,c("Time","O3.30cm")]; O3 <- xts(O3$O3.30cm,O3$Time)
    O4 <- dat.530temp[,c("Time","O4.30cm")]; O4 <- xts(O4$O4.30cm,O4$Time)
    xx <- cbind(O1,O2,O3,O4)
    names(xx) <- c("Control","30% drought","60% drought","Dry")
    dygraph(xx,main="Oak Soil Temperature 30 cm",group="MODOEK",
            xlab="Time",ylab="Mean Temperature [°C]") %>%
      dyRangeSelector() %>%
      dyAxis("y",valueRange = c(0,35)) %>%
      dyOptions(colors=c("lightblue","lightgreen","orange","red"),
                useDataTimezone = TRUE,
                drawGrid=TRUE,
                drawPoints=TRUE,
                gridLineColor="gainsboro",
                rightGap=30) %>%
      dyHighlight(highlightCircleSize=4,
                  highlightSeriesBackgroundAlpha=0.6,
                  hideOnMouseOut=TRUE) %>%
      dyLegend(labelsSeparateLines=TRUE,width=300)
  })
  
})
