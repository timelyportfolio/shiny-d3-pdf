require(caTools)
require(latticeExtra)
require(directlabels)
require(reshape2)
require(xts)
require(PerformanceAnalytics)

shinyServer(function(input, output) {
  output$downloadPdf <- reactive(function() {
        #trying some new colors out
    mycolors=c(brewer.pal(9,"Blues")[c(7,5)],brewer.pal(9,"Greens")[6])
    #mycolors=c(brewer.pal(6,"Blues)[c(3,5)],"slategray4")

    #function to get numbers in percent format
    percent <- function(x, digits = 2, format = "f", ...) 
    { 
      paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "") 
    }
    
    
    #################################code to make graphs########################################
    
    data(managers)
    #get xts in df form so that we can melt with the reshape package
    #will use just manager 1, sp500, and 10y treasury
    managers <- managers[,c(1,8,9)]
    #add 0 at beginning so cumulative returns start at 1
    #also cumulative will match up datewise with returns
    managers <- as.xts(rbind(rep(0,NCOL(managers)),coredata(managers)),
                       order.by=c(as.Date(format(index(managers)[1],"%Y-%m-01"))-1,index(managers)))
    managers.df <- as.data.frame(cbind(index(managers),coredata(managers)),stringsAsFactors=FALSE)
    #melt data which puts in a form that lattice and ggplot enjoy
    managers.melt <- melt(managers.df,id.vars=1)
    colnames(managers.melt) <- c("date","account","return")
    managers.melt[,1] <- as.Date(managers.melt[,1])
    
    #get cumulative returns starting at 1
    managers.cumul <- as.xts(
      apply(managers+1,MARGIN=2,FUN=cumprod),
      #add end of first month to accommodate the 1 that we add
      order.by=index(managers))
    managers.cumul.df <- as.data.frame(cbind(index(managers.cumul),
                                             coredata(managers.cumul)),
                                       stringsAsFactors=FALSE)
    managers.cumul.melt <- melt(managers.cumul.df,id.vars=1)
    colnames(managers.cumul.melt) <- c("date","account","return")
    managers.cumul.melt[,1] <- as.Date(managers.cumul.melt[,1])
    #this is tricky but necessary
    #reorder accounts and indexes to preserve order with manager and then benchmarks
    managers.cumul.melt$account <- factor(as.character(managers.cumul.melt$account),colnames(managers)[c(2,3,1)],ordered=TRUE)
    
    #get rolling returns for 1y, 3y, 5y, since inception
    trailing <- table.TrailingPeriods(managers[,c(2,3,1)], periods=c(12,36,60,NROW(managers)),FUNCS=c("Return.annualized"),funcs.names=c("return"))
    trailing.df <- as.data.frame(cbind(c("1y","3y","5y",paste("Since Inception ",format(index(managers)[1],"%b %Y"),sep="")),
                                       c(rep("return",4)),  #will allow for multiple measures if we decide to include later
                                       coredata(trailing)),
                                 stringsAsFactors=TRUE)
    trailing.melt <- melt(trailing.df,id.vars=1:2)
    colnames(trailing.melt) <- c("period","measure","account","value")
    #this is tricky but necessary
    #reorder the period so that they will be in correct chronological order
    trailing.melt$period <- factor(as.character(trailing.melt$period),rev(c("1y","3y","5y",paste("Since Inception ",format(index(managers),"%b %Y"),sep=""))),ordered=TRUE)
    #reorder accounts and indexes to preserve order with manager and then benchmarks
    trailing.melt$account <- factor(as.character(trailing.melt$account),colnames(managers)[c(3,2,1)],ordered=TRUE)
    
    #get drawdown by date for drawdown graph
    drawdown <- Drawdowns(managers)
    drawdown.df <- as.data.frame(cbind(index(drawdown),coredata(drawdown)),
                                 stringsAsFactors=FALSE)
    drawdown.melt <- melt(drawdown.df,id.vars=1)
    colnames(drawdown.melt) <- c("date","account","drawdown")
    drawdown.melt[,1] <- as.Date(drawdown.melt[,1])
    #this is tricky but necessary
    #reorder accounts and indexes to preserve order with manager and then benchmarks
    drawdown.melt$account <- factor(as.character(drawdown.melt$account),colnames(managers)[c(2,3,1)],ordered=TRUE)

    #while latticeExtra theEconomist.theme is beautiful
    #I wanted to stretch my knowledge, so I will start from scratch
    
    #example given to left justify strip
    #http://maths.anu.edu.au/~johnm/r-book/xtras/boxcontrol.pdf
    stripfun <- function(which.given, which.panel,factor.levels, ...){
      grid.rect(name = trellis.grobname("bg", type = "strip"), 
        gp = gpar(fill = "seashell3", col = "seashell3"))
      panel.text(x=0.10, y=0.5,
                 lab = factor.levels[which.panel[which.given]],
                 adj=0, font=3, cex=1.3)
      
      }
    
    #heavily stripped and modified theEconomist.axis() from latticeExtra
    timely.axis <- function (side = c("top", "bottom", "left", "right"), scales, 
                                   components, ..., labels = c("default", "yes", "no"), ticks = c("default", 
                                                                                                  "yes", "no"), line.col, noleft=TRUE) 
    {
      side <- match.arg(side)
      if (side == "top") return()  
    
      labels <- match.arg(labels)
      ticks <- match.arg(ticks)
    
      if (side %in% c("left", "right")) {
        if (side == "right") {
          scales$draw=TRUE
          labels <- "no"
          ticks <- "no"
        }
        if (side == "left") {
            labels <- "yes"
            ticks <- "yes"
          }
        }
    
       axis.default(side, scales = scales, components = components, 
                   ..., labels = labels, ticks = ticks, line.col = "black")
       if (side == "right" ) {#& panel.number()==1) {
            comp.list <- components[["right"]]
            if (!is.list(comp.list)) 
                comp.list <- components[["left"]]
            panel.refline(h = comp.list$ticks$at)   
        lims <- current.panel.limits()
        panel.abline(h = lims$y[1], col = "black")
      }
    }
    
    
    #set up ylimits to use for the two scales
    ylimits<-c(pretty(c(min(managers.cumul.melt$return),
      max(managers.cumul.melt$return)),4),as.numeric(round(last(managers.cumul)[,order(last(managers.cumul))],2)))
    ylabels<-c(ylimits[1:(length(ylimits)-3)],colnames(managers)[order(last(managers.cumul))])
    
    
    returns <- list(
      bar = barchart(account~value|period,col=mycolors,data=trailing.melt,
             layout=c(1,4),
              box.ratio=0.10,
             origin=0,       
             reference=TRUE,
             border = NA,
             par.settings=
               list(
                    par.main.text = list(font = 1, cex=1.5, just = "left",x = grid::unit(5, "mm")),
                    axis.line = list(col = NA)),
             scales=list(x=list(
               limits=c(0,max(trailing.melt$value)+0.05),  #snug labels right up to bars by setting to 0
               at=pretty(trailing.melt$value),
               labels=paste(round(100*as.numeric(pretty(trailing.melt$value)), 2), "%", sep="")
               )),
              xlab=NULL,
           axis = timely.axis,
             strip=stripfun,
             strip.left=FALSE,
            panel=function(...) {
                 panel.barchart(...)  
                 tmp <- list(...) 
                 tmp <- data.frame(x=tmp$x, y=tmp$y) 
                 # add text labels                        
                 panel.text(x=tmp$x, y=tmp$y,                                 
                            label=percent(tmp$x , 2 ),                      
                            cex=1, col=mycolors, pos=4) 
            },
             main="Annualized Returns"),
      
      cumulgrowth = 
        xyplot(return~date,groups=account,data=managers.cumul.melt,
    #           col=mycolors,
               type="l",lwd=3,
               xlab=NULL,
               ylab=NULL,
            par.settings=
                 list(
                   par.main.text = list(font = 1, cex=1.5, just = "left",x = grid::unit(5, "mm")),
                   axis.line = list(col = "transparent"),
                   superpose.line=list(col=mycolors)),  #do this for direct.label
               
           scales=list(x=list(alternating=1,at=index(managers)[endpoints(managers,"years")],
            labels=format(index(managers)[endpoints(managers,"years")],"%Y")),
            y=list(alternating=3,at=ylimits,labels=ylabels)),
               
           axis=function (side = c("top", "bottom", "left", "right"), scales, 
                          components, ..., labels = c("default", "yes", "no"), ticks = c("default", 
                          "yes", "no"), line.col){
                             side <- match.arg(side)
                             labels <- match.arg(labels)
                             ticks <- match.arg(ticks)
                             axis.text <- trellis.par.get("axis.text")                         
                              if(side == "top") return()
                              if(side == "right") {
                                components[["right"]]<-components[["left"]]
                                components[["right"]]$ticks$at <- components[["right"]]$ticks$at[5:7]
                                components[["right"]]$labels$at <- components[["right"]]$labels$at[5:7]
                                components[["right"]]$labels$labels <- components[["right"]]$labels$labels[5:7]
                              }
                              if(side %in% c("bottom","right")){
                                axis.default(side, scales = scales, components = components, 
                                          ..., labels = labels, ticks = ticks, line.col = axis.text$col)
                                if (side == "right") {
                                  comp.list <- components[["left"]]
                                  panel.refline(h = comp.list$ticks$at[1:4])
                                  lims <- current.panel.limits()
                                  panel.abline(h = lims$y[1], col = axis.text$col)
    
                                comp.list.left<-components[["left"]]
                                comp.list.left$ticks$at <- components[["left"]]$ticks$at[1:4]
                                comp.list.left$labels$at <- components[["left"]]$labels$at[1:4]
                                comp.list.left$labels$labels <- components[["left"]]$labels$labels[1:4]
                            panel.axis(side="left",at=comp.list.left$ticks$at,outside=TRUE)
    
                                }                         
                              }
           },
               main=paste("Cumulative Growth Since Inception ",format(index(managers)[1],"%B %Y"),sep=""))
    )
    
    #set up ylimits to use for the two scales
    ylimits<-c(pretty(c(min(drawdown.melt$drawdown),
      max(drawdown.melt$drawdown)),4),as.numeric(round(last(drawdown)[,order(last(drawdown))],2)))
    ylabels<-c(percent(ylimits[1:(length(ylimits)-3)],digits=0),colnames(managers)[order(last(drawdown))])
    
    risk=list(
      drawdown=
        xyplot(drawdown~date,group=account,data=drawdown.melt,
                      type="l",lwd=3,
               xlab=NULL,
               ylab=NULL,
            par.settings=
                 list(
                   par.main.text = list(font = 1, cex=1.5, just = "left",x = grid::unit(5, "mm")),
                   axis.line = list(col = "transparent"),
                   superpose.line=list(col=mycolors)),  #do this for direct.label
               
           scales=list(x=list(alternating=1,at=index(managers)[endpoints(managers,"years")],
            labels=format(index(managers)[endpoints(managers,"years")],"%Y")),
            y=list(alternating=3,at=ylimits,labels=ylabels)),
               
           axis=function (side = c("top", "bottom", "left", "right"), scales, 
                          components, ..., labels = c("default", "yes", "no"), ticks = c("default", 
                          "yes", "no"), line.col){
                             side <- match.arg(side)
                             labels <- match.arg(labels)
                             ticks <- match.arg(ticks)
                             axis.text <- trellis.par.get("axis.text")                         
                              if(side == "top") return()
                              if(side == "right") {
                                components[["right"]]<-components[["left"]]
                                components[["right"]]$ticks$at <- components[["right"]]$ticks$at[6:8]
                                components[["right"]]$labels$at <- components[["right"]]$labels$at[6:8]
                                components[["right"]]$labels$labels <- #components[["right"]]$labels$labels[6:8]
                                  NULL
                              }
                              if(side %in% c("bottom","right")){
                                if(side=="bottom") {
                                axis.default(side, scales = scales, components = components, 
                                          ..., labels = labels, ticks = ticks, line.col = axis.text$col)
                              }
                                if (side == "right") {
                                  comp.list <- components[["left"]]
                                  panel.refline(h = comp.list$ticks$at[1:5])
                                  lims <- current.panel.limits()
                                  panel.abline(h = lims$y[1], col = axis.text$col)
    
                                comp.list.left<-components[["left"]]
                                comp.list.left$ticks$at <- components[["left"]]$ticks$at[1:5]
                                comp.list.left$labels$at <- components[["left"]]$labels$at[1:5]
                                comp.list.left$labels$labels <- components[["left"]]$labels$labels[1:5]
                            panel.axis(side="left",at=comp.list.left$ticks$at,labels=comp.list.left$labels$labels,outside=TRUE)
    
                                }                         
                              }
           },
               main=paste("Drawdown Since Inception ",format(index(managers)[1],"%B %Y"),sep=""))
      )
    
    risk$drawdown <- direct.label(risk$drawdown,list("smart.grid",cex=0.75))
    #####################################finished creating graphs###################################
    
    
    #####################################code to create pdf and return##############################
    
    pdffile <- tempfile(fileext=".pdf")
    pdf(file=pdffile, height=8, width=11)

    print(returns$cumulgrowth,position=c(0,0.6,0.6,1),more=TRUE)
    #print(returns$bar,position=c(0,0,0.6,0.6),more=TRUE)
    print(risk$drawdown,position=c(0,0,0.6,0.6),more=TRUE)
    #print(risk$drawdown,position=c(0.6,0,1,1))
    print(returns$bar,position=c(0.6,0,1,1))    
  
    dev.off()
    b64 <- base64encode(readBin(pdffile, what=raw(),n=100000))
    return(paste("data:application/pdf;base64,", b64, sep=''))
    #delete file now that we are done
    unlink(pdffile)
    }
  )
}
)
