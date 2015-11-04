#shinyServer
#setwd("U:/Scripts/R/Shiny")
library(gtools)
library(magrittr)
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
#library(GGally)
library(readxl)
#library(shinyBS)
library(lazyeval)
#library(rdrop2)
#source(file.path("U:","Scripts","R","common.r"))


read.nonmem <- function(file, n=-1) {
    ## auxiliary function to split text lines at blanks
    my.split <- function(line, numeric=FALSE) {
        pieces <- unlist(strsplit(line, split=" +"))[-1]

        if( numeric )
            return(as.numeric(pieces))
        else
            return(pieces)
    }

    cat(sprintf("Reading NONMEM data from '%s'\n", file))

    lines <- readLines(file, n) # read file as text
    cat(sprintf("- %d lines\n", length(lines)))

    idx <- substring(lines,1,1)!="T"
    cat(sprintf("- %d tables\n", sum(!idx)))

    lines <- lines[idx] # strip lines starting with T (TABLE NO ...)

    ## do we have header lines??
    if( length(grep("^ +[A-Za-z]", lines[1]))>0 ) { # yes!
        data <- sapply(lines[lines!= lines[1]], my.split, numeric=TRUE)
        header <-  my.split(lines[1])
        cat(sprintf("- file has column names (%s)\n", paste(header,collapse=", ")))
    } else {                                        # no
        data <- sapply(lines, my.split, numeric=TRUE)
        header <- sprintf("Column%02d", 1:nrow(data)) # make fake column names
        cat("- file has NO header names - creating names Column01, Column02, ...\n")
    }
    cat(sprintf("- %d columns\n", nrow(data)))

    ## transpose data and make a data.frame
    df <- data.frame(data[1,])
    for( i in 2:nrow(data))
        df <- cbind(df, data[i,])

    ## set column and row names
    rownames(df) <- NULL
    colnames(df) <- header

    cat("ok.\n")

    return(df)
}

server <- (function(input, output, session) { 
  inputdta <- reactive({inFile<-input$file;dta <- read.nonmem(inFile$datapath);return(dta)})
  
  ### mapping variables
  output$MAP <- renderText("Data Mapping:")
  output$CONVERSION <- renderText("Data Conversion:")
  output$ID <- renderUI({if(is.null(input$file)) return(NULL);selectInput("id","Subject",sort(names(inputdta())),selected="ID")}) 
  output$TIME <- renderUI({if(is.null(input$file)) return(NULL);selectInput("time","Time",sort(names(inputdta())),selected="TIME")})   
  output$DV <- renderUI({if(is.null(input$file)) return(NULL);selectInput("dv","Concentration",sort(names(inputdta())),selected="DV")}) 
  output$UNIQUE <- renderUI({if(is.null(input$file)) return(NULL);selectizeInput("unique","Unique Identifier",sort(names(dta_map())),multiple=T,selected="ID")}) 
  output$UNIQUEID <- renderPrint({if(is.null(input$file)) return(NULL);head(levels(dta_unique()$UNIQUE),20)})

  output$CONVERTCON <- renderUI({if(is.null(input$file)) return(NULL)
                              (selectizeInput('convertcon',"Continuous to Categorical Variable:",multiple=T,
                               sort(names(dta_map()[sapply(dta_map(),is.numeric)]))))
                            })
  output$CONVERTCAT <- renderUI({if(is.null(input$file)) return(NULL)
                              (selectizeInput('convertcat',"Categorical to Continuous Variable:",multiple=T,
                               sort(names(dta_map()[sapply(dta_map(),is.factor)]))))
                            })
  
  dta_map <-  reactive({dta <- inputdta() 
                        if("EVID" %in% names(dta)) dta %<>% filter(EVID==0)
                        else
                        if("AMT" %in% names(dta)) dta %<>% filter(AMT!=0|is.na(as.numeric(AMT)))
                        else
                        if("MDV" %in% names(dta)) dta %<>% filter(MDV==0)
  
                        dta$V1 <- dta[,sprintf(input$id)];dta$V2 <- dta[,sprintf(input$time)];dta$V3 <- dta[,sprintf(input$dv)] 
                        if(input$id=="ID") dta %<>% select(-V1) else dta %<>% rename(ID=V1)
                        if(input$time=="TIME") dta %<>% select(-V2) else dta %<>% rename(TIME=V2)             
                        if(input$dv=="DV") dta %<>% select(-V3) else dta %<>% rename(DV=V3)
                        return(dta)
                        })
  
  dta_unique <- reactive({dta <- dta_map() %>% mutate(ID=str_pad(ID,width=str_length(max(ID)),pad="0")) %>%
                                               unite_("UNIQUE",input$unique,remove=F) %>%
                                               mutate(UNIQUE=as.factor(UNIQUE),ID=as.numeric(ID),TIME=as.numeric(TIME),DV=as.numeric(as.character(DV)))                      
                          return(dta)
                          })
  dta_convert <- reactive({dta <- dta_unique()
                           if(!is.null(input$convertcat)) dta[,input$convertcat] <- as.numeric(as.matrix(dta[,c(input$convertcat)]))
                           if(!is.null(input$convertcon)) dta %<>% mutate_each_(funs(factor),input$convertcon)
                           return(dta)
                           })
  dta <- reactive ({dta <- dta_convert() %>% arrange(UNIQUE,ID,TIME);return(dta)})
  
  output$contents <-  renderDataTable({if(is.null(input$file)) return(NULL);                                                                                                                                           
                                       dta()},options=list(lengthMenu=c(10,25,50,100),pageLength=10)
                                       )
  
  ############################################################################################################################
  ############################################################################################################################
  
  output$colorvars <- renderUI({if(is.null(input$file)) return(NULL)
                              selectInput('colorvar',"Color by:",selected="NONE",
                                          sort(names(dta_strat()[sapply(dta_strat(),is.factor)])))
                              })

  output$xvars <- renderUI({if(is.null(input$file)) return(NULL)
                            selectInput('xvar',"X Variable:",selected="TIME",
                                        sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                                        names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                            })

  output$yvars <- renderUI({if(is.null(input$file)) return(NULL)
                            selectInput('yvar',"Y Variable:",selected="DV",
                                         sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                                         names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                            })
              
  output$stratas <- renderUI({if(is.null(input$file)) return(NULL)
                      selectInput("strata","Stratify by:",selected="NONE",
                                  sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                                  names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                      })  
  
  output$rowstratas <-  renderUI({if(is.null(input$file)) return(NULL) 
                          selectInput("rowstrata","Stratify by Row:",selected="NONE",
                                      sort(c(names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                        })  

  output$colstratas <-  renderUI({if(is.null(input$file)) return(NULL) 
                          selectInput("colstrata","Stratify by Column:",selected="NONE",
                                      sort(c(names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                        })

  output$colors <-  renderUI({if(is.null(input$file)) return(NULL) 
                      selectInput("color","Color:",selected="NONE",
                        sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                               names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                    })  
  output$shapes <-  renderUI({if(is.null(input$file)) return(NULL) 
                      selectInput("shape","Shape:",selected="NONE",
                        sort(c(names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                    })  
  
  output$sizes <- renderUI({if(is.null(input$file)) return(NULL) 
                    selectInput("size","Size:",selected="NONE",
                      sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                      names(dta_strat()[sapply(dta_strat(),is.factor)]))))
                  })  
  
  output$xlims <- renderUI({if(is.null(input$file)) return(NULL) 
                    sliderInput("xlim","X range:",min=min(dta_plot()$XVAR,na.rm=T),max=max(dta_plot()$XVAR,na.rm=T),
                                value=c(min(dta_plot()$XVAR,na.rm=T),max(dta_plot()$XVAR,na.rm=T)),round=T)
                  })  
  output$ylims <- renderUI({if(is.null(input$file)) return(NULL) 
                    sliderInput("ylim","Y range:",min=min(dta_plot()$YVAR,na.rm=T),max=max(dta_plot()$YVAR,na.rm=T),
                                value=c(min(dta_plot()$YVAR,na.rm=T),max(dta_plot()$YVAR,na.rm=T)),round=T)
                  })  
  
  dta_strat <- reactive({dta <- dta() %>% mutate(NONE=factor(0));return(dta)})
 
  dta_plot <- reactive({
    dta <- dta_strat()
    dta$XVAR <- dta[,sprintf(input$xvar)];dta$YVAR <- dta[,sprintf(input$yvar)]
    if(input$strata_type=="PANEL") dta$STRATVAR <- dta[,sprintf(input$strata)]  else dta$STRATVAR <- 0
    if(!is.null(input$color))  dta$COLORGRP <- dta[,sprintf(input$color)] else dta$COLORGRP <- 0
    if(!is.null(input$shape))  dta$SHAPEGRP <- dta[,sprintf(input$shape)] else dta$SHAPEGRP <- 0      
    if(!is.null(input$size))  dta$SIZEGRP <- dta[,sprintf(input$size)] else dta$SIZEGRP <- 0      
    #if(input$group_type=="SIZE")  plot.dta$SIZEGRP <- plot.dta[,sprintf(input$size)] else plot.dta$SIZEGRP <- rep(0,nrow(plot.dta))               
      
    #if(length(levels(as.factor(as.character(dta$COLORGRP))))<=8) dta$COLORGRP<- as.factor(dta$COLORGRP)
    #if(length(levels(as.factor(as.character(dta$SHAPEGRP))))<=6) dta$SHAPEGRP<- as.factor(dta$SHAPEGRP)
    #if(length(levels(as.factor(as.character(dta$SIZEGRP))))<=6) dta$SIZEGRP<- as.factor(dta$SIZEGRP)
    
    return(dta)  
  })
  
  dta_reg <- reactive ({dta <- dta_plot() %>% mutate_(X=input$xvar,Y=input$yvar)})  
  output$r2 <- renderText({
    linear.mod <- lm(data=dta_reg(),Y~X)
    r2 <- ifelse(summary(linear.mod)$r.squared<0.01,"r-squared < 0.01",paste("r-squared =",round(summary(linear.mod)$r.squared,digits=2)))
    return(r2)
  })
  
  ncol <- reactive({ dta <- ncol(plot_dta())
  })
  
  plot <- reactive({if(is.null(input$file)) return(NULL)
    
    if(!is.null(input$xvar) & !is.null(input$yvar)) 
      p <- ggplot(data=dta_plot(),
                  aes_string(x=input$xvar,y=input$yvar,color=input$color,fill=input$color,shape=input$shape)) +
                  xlab(input$xvar) + ylab(input$yvar) + coord_cartesian(xlim=ranges$x,ylim=ranges$y) + geom_blank() + theme_bw()
    
    if(is.numeric(dta_plot()$COLORGRP) & input$point_type!="NO") {
      p <- p + geom_point(position=input$jitter,size=input$point_size,alpha=input$point_alpha) + 
               scale_color_continuous(name=input$color) + scale_shape(name=input$shape)
      if(length(levels(dta_plot()$SHAPEGRP))==1) p <- p + guides(shape=FALSE)
      if(length(levels(dta_plot()$SHAPEGRP))>=2 & length(levels(as.factor(dta_plot()$COLORGRP)))<=1)  p <- p + guides(color=FALSE)
    }  
    
    if(is.factor(dta_plot()$COLORGRP) & input$point_type!="NO") {
      if(length(levels(dta_plot()$SHAPEGRP))==1) {
        p <- p + geom_point(position=input$jitter,shape=16,size=input$point_size,alpha=input$point_alpha) + 
                 scale_color_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) + 
                 scale_shape(guide=FALSE)
        if(length(levels(dta_plot()$COLORGRP))<=1) p <- p + theme(legend.position="none")
      }
      if(length(levels(dta_plot()$SHAPEGRP))>=2) {  
        p <- p + geom_point(position=input$jitter,size=input$point_size,alpha=input$point_alpha)  +
                 scale_color_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) + 
                 scale_shape(name=input$shape)
        if(length(levels(dta_plot()$COLORGRP))<=1) p <- p + guides(color=FALSE)
      }  
    }
    
    #if(input$plot_type=="SCATTERPLOT" & input$group_type=="NONE") 
    #  p <- p + geom_point(position=input$jitter,size=rel(3),color="black") + theme(legend.position="none")
        
    if(input$line_type=="MEAN" & length(levels(dta_plot()$COLORGRP))>=2) {
      p <- p + stat_summary(geom="line",size=input$line_size,alpha=input$line_alpha,fun.y=mean) +
               stat_summary(geom="ribbon",aes(color=NULL),alpha=0.1,
                            fun.ymin=function(x) quantile(x, 0.05),fun.ymax=function(x) quantile(x, 0.95)) + 
               scale_color_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) + 
               scale_fill_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) 
    }
    if(input$line_type=="MEAN" & length(levels(dta_plot()$COLORGRP))<2) { 
      p <- p + stat_summary(geom="line",color="black",size=input$line_size,alpha=input$line_alpha,fun.y=mean) +
               stat_summary(geom="ribbon",aes(color=NULL),fill="black",alpha=0.1,
                            fun.ymin=function(x) quantile(x, 0.05),fun.ymax=function(x) quantile(x, 0.95))                                  
    }
   
  
    if(input$line_type=="REGRESSION" & length(levels(dta_plot()$COLORGRP))>=2) {
      p <- p + stat_smooth(method="lm",size=input$line_size,alpha=input$line_alpha) + 
               scale_color_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) +
               scale_fill_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) 
    }
    if(input$line_type=="REGRESSION" & length(levels(dta_plot()$COLORGRP))<2) {
      p <- p + stat_smooth(color="black",fill="black",method="lm",)
    }
    if(input$line_type=="TRENDLINE" & length(levels(dta_plot()$COLORGRP))>=2) {
      p <- p + stat_smooth(method="loess",size=input$line_size,alpha=input$line_alpha) + 
               scale_color_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) +
               scale_fill_manual(name=input$color,values=c("black","red","blue","green","orange","purple","brown","gray")) 
    }
    if(input$line_type=="TRENDLINE" & length(levels(dta_plot()$COLORGRP))<2) {
      p <- p + stat_smooth(color="black",fill="black",method="loess",size=input$line_size,alpha=input$line_alpha)
    }
   
    if(input$line_type=="REFERENCE" & !is.null(input$xref)) {
      p <- p + geom_vline(x=as.numeric(unlist(str_split(input$xref,","))),size=input$line_size,alpha=input$line_alpha,linetype="dashed",color="darkblue")
      if(length(unlist(str_split(input$xref,",")))>1) {
        p <- p + annotate("rect",xmin=min(as.numeric(unlist(str_split(input$xref,","))),na.rm=T),
                          xmax=max(as.numeric(unlist(str_split(input$xref,","))),na.rm=T),
                          ymin=min(as.numeric((dta_plot()[,input$yvar])),na.rm=T),
                          ymax=max(as.numeric((dta_plot()[,input$yvar])),na.rm=T),
                          fill="blue",alpha=0.1)
      }
    }
    
    
    if(input$line_type=="REFERENCE" & !is.null(input$yref)) {
      p <- p + geom_hline(y=as.numeric(unlist(str_split(input$yref,","))),size=input$line_size,alpha=input$line_alpha,linetype="dashed",color="darkred")
      if(length(unlist(str_split(input$yref,",")))>1) {
        p <- p + annotate("rect",ymin=min(as.numeric(unlist(str_split(input$yref,","))),na.rm=T),
                          ymax=max(as.numeric(unlist(str_split(input$yref,","))),na.rm=T),
                          xmin=min(as.numeric((dta_plot()[,input$xvar])),na.rm=T),
                          xmax=max(as.numeric((dta_plot()[,input$xvar])),na.rm=T),
                          fill="red",alpha=0.1)
      }
    }
    
    if("X" %in% input$log) p <- p + scale_x_log10(); if("Y" %in% input$log) p <- p + scale_y_log10()
    if("X" %in% input$sqrt) p <- p + scale_x_sqrt(); if("Y" %in% input$sqrt) p <- p + scale_y_sqrt()
    if("X" %in% input$rev) p <- p + scale_x_reverse(); if("Y" %in% input$rev) p <- p + scale_y_reverse()
    if(input$flip) p <- p + coord_flip()

    facets <- paste(input$rowstrata,'~',input$colstrata)  
    if(input$strata_type=="PANEL") {p <- p + facet_wrap(~STRATVAR,scales=tolower(input$scale_type))}
    if(input$strata_type=="GRID") {p <- p + facet_grid(facets,scales=tolower(input$scale_type))}

    if(input$xlab!="Label") p <- p + xlab(input$xlab)
    if(input$ylab!="Label") p <- p + ylab(input$ylab)
    if(input$title != "") p <- p + ggtitle(input$title)
    #if(!is.null(input$color)) p <- p + aes_string(color=input$color)
    #if(!is.null(input$shape)) p <- p + aes_string(shape=input$shape)
    #if(!is.null(input$size)) p <- p + aes_string(size=input$size)
    #if(!is.null(input$color)) p <- p + geom_point(aes(color=COLORGRP))
    #if(!is.null(input$shape)) p <- p + geom_point(aes(shape=factor(SHAPEGRP)))
    #if(!is.null(input$size)) p <- p + geom_point(aes(size=SIZEGRP))
    #if (!is.null(input$plot_click)) p <- p + geom_point(data=clickid(),aes(x=TIME,y=DV),size=rel(5),col="red")
    return(p)
  })
  output$plot <- renderPlot(plot())
    
  output$test <- renderText({
    txt <- paste("color",length(levels(dta_plot()$COLORGRP)),"shape",length(levels(dta_plot()$SHAPEGRP)))
    return(txt)
  })
  
  
  
  
  clickid <-  reactive({
                click.id <- nearPoints(dta(),input$plot_click,maxpoints=1)
                dta() %>% filter(ID==click.id$ID)
              })
   
  output$brush_info <- renderPrint({if(is.null(input$file)) return(NULL)
    cat("Selected datapoints:\n")
    dta <- brushedPoints(dta_plot(), input$plot_brush)
    dta %<>% filter_(!is.na(input$xvar)) %>% select(-NONE,-STRATVAR,-COLORGRP,-SHAPEGRP) 
    return(dta)
  })
    
  output$brush_data <- renderPrint({if(is.null(input$file)) return(NULL)
    dta <- brushedPoints(dta_plot(), input$plot_brush)
    dta %<>% filter_(!is.na(input$xvar)) #%>% select(1:12) 
    return(dta)
  })

  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  output$exportplot <- downloadHandler(
      filename='output-figure.png',content=function(file) {
        device <- function(...,width,height) grDevices::png(...,width=width,height=height,res=300,units="in")
        ggsave(file,plot=plot(),device=device)
      })
  
})
