ui <- fluidPage(
      fluidRow(
        column(1,img(src="PMX.jpg",height=50,width=75),align="left"),       
        column(10,titlePanel("Pharmacometric Web Application:  Simple Simulation Showcase")),
        column(1,img(src="Takeda.jpg",height=50,width=125),align="right")
      ),
      fluidRow(column(12, tabsetPanel(
        tabPanel("Information",fluidRow(
          column(8,
            h3(HTML(paste(tags$b("Objective:"),("This web application provides an quick way to plot simulations.")))),
            br(),
            strong("Data:  The application handles only NONMEM output tables with column headers."),
            p("The user selects the simulated data file.",
              "User can map variables to Subject, Time, Concentration; these are automatically mapped to ID, TIME, and DV.",
              "Also, a unique identifier needs to mapped, if necessary; ID is mapped by default.",
              "Variables can be selected and toggled between continuous and categorical modes; this can be helpful for visualizing groups in plots",
              "The uploaded simulation dataset is presented in tabular format.  Number of viewed rows can be selected in upper left corner",
              "There is a global search in upper right corner and the table can be filtered by entering values under columns and sorted in ascending or descending order by selecting column header"
            ),
            strong("Plot:  The application enables user to generate plots without knowledge of using R."),
            p("The user can select X and Y variables; TIME and DV are mapped, respectvely, by default.",
              "User can map grouping variables to Color or Shape to improve visualization.",
              "For continuous variables, a spectrum of colors are displayed, based on corresponding values in legend; Shape is not applicable for continuous variables.",
              "For categorical variables, discrete colors and/or shapes are displayed as defined in the legend; Shape can allow up to 6 different categories.",
              "User can also stratify data into separate panels using one or two variables.",
              "Variables with fewer than 20 levels can string together plots in different frames based on the levels within the variable.",
              "Variables with fewer than 6 levels may be laid out plots in a 2-dimensional (rows and columns) in a grid format.",
              "Axis scales in panels can either be fixed and dynamic in the X or Y direction.",
              "The plot can be downloaded into a file called output-figure.png."
            ),
            p("User can choose to display or hide data points or display data points with a little noise to improve visualization.",
              "User can choose to add a line option to the plot:  ",
              "None (default) - no line is plotted;",
              "Mean - mean line and shaded region covering from 5th to 95th percentiles for each group (if applicable) are plotted;",
              "Regression: linear regression line and shaded region covering 95% CI for each group (if applicable) are plotted and provides overall coefficient of determination;",
              "Trendline: loess smoother line and shaded region covering 95% CI for each group (if applicable) are plotted;",
              "Reference: horizontal or vertical or both lines are plotted",
              "User can choose one of many axis options:  logarithm or square root transformed scales; reverse scales; switch axes; or rename axis labels",
              "User can highlight section of the plot to identify data point(s) of interest; Additional, user can double-click to zoom in to the selected region for closer examination; double-click again to zoom out",
              "Selected data point(s) are presented in a table below for further examination."
            ),
            br(),
            br(),
            h4(HTML(paste(tags$b("Author:"),("Max Tsai")))),
            h4(HTML(paste(em("Disclaimer:  This application has not been validated and should be used at the discretion of the user"))))
          ),
        column(4,
          h4(HTML(paste(tags$b("Version History")))),
          tags$ul(
            tags$li(h4(HTML(paste(tags$b("v1.0:"),("web application is launched"))))),
            tags$li(h4(HTML(paste(tags$b("v1.1:"),("added transparency and size options for points and lines;"),
                                                  ("enable multiple options for removing dosing records")))))
          )
        )  
      )),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data",
          fluidRow(
            column(3,
              wellPanel(fileInput("file", p('Data File:'),multiple=TRUE,accept=c(".tbl",".tab",".tbl",".dat"))),
              wellPanel(strong(textOutput('MAP')),
                fluidRow(
                  column(6,uiOutput('ID'),uiOutput('TIME')),
                  column(6,uiOutput('DV'),uiOutput('UNIQUE'))
                )#p(HTML(paste(tags$b("Example levels of unique identifier:")))),textOutput('UNIQUEID')
              ),
              wellPanel(strong(textOutput('CONVERSION')),uiOutput("CONVERTCON"),uiOutput("CONVERTCAT")
              )
            ),
            column(9,dataTableOutput('contents'))
          )  
        ),  
        
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Plot",fluidRow(
            column(4,
              fluidRow(
                column(6,
                  wellPanel(fluidRow(column(6,uiOutput('xvars')),column(6,uiOutput('yvars')))),         
                  wellPanel(strong("Grouping by:"),
                    fluidRow(column(6,uiOutput("colors")),column(6,uiOutput("shapes")))
                  ),
                  wellPanel(
                    selectInput('strata_type',"Stratification:",choices=c("NONE","PANEL","GRID"),selected="NONE"),
                    conditionalPanel("input.strata_type=='PANEL'",uiOutput("stratas")),
                    conditionalPanel("input.strata_type=='GRID'",uiOutput("rowstratas")),
                    conditionalPanel("input.strata_type=='GRID'",uiOutput("colstratas")),
                    conditionalPanel("input.strata_type!='NONE'",selectInput('scale_type',"Axis Scale:",c("FIXED","FREE_X","FREE_Y","FREE"),selected="fixed"))
                  ),
                  wellPanel(
                    fluidRow(column(12,
                      strong("Graph options:"),
                      fluidRow(
                        column(6,textInput('xlab',"X Axis","Label")),
                        column(6,textInput('ylab',"Y Axis","Label"))
                      ),
                      fluidRow(
                        column(12,textInput('title',"Plot Title",""))
                      )
                      #uiOutput('xlims'),uiOutput('ylims')
                    ))
                  ),
                  wellPanel(downloadButton("exportplot","Download plot"))
                  
                  #verbatimTextOutput('test')
                ),    
                column(6,
                  wellPanel(fluidRow(column(6,
                    selectInput('point_type',"Add points:",c("YES","NO"),selected="YES")),
                    conditionalPanel("input.point_type=='YES'",
                      radioButtons("jitter","Jitter Points",
                                   list("Yes"="jitter","No"="identity"),
                                   selected="identity",inline=T)
                      ),
                    fluidRow(column(12,
                      column(6,conditionalPanel("input.point_type!='NO'",sliderInput('point_alpha',"Transparency",0,1,0.6,step=0.05,round=T))),
                      column(6,conditionalPanel("input.point_type!='NO'",sliderInput('point_size',"Size",0,10,3,step=1,round=T)))
                    ))
                  )),  
                  wellPanel(
                    fluidRow(column(12,
                      selectInput('line_type',"Add line:",c("NONE","MEAN","REGRESSION","TRENDLINE","REFERENCE")),
                      conditionalPanel("input.line_type=='REGRESSION'",verbatimTextOutput('r2')),
                    fluidRow(
                      column(6,conditionalPanel("input.line_type=='REFERENCE'",textInput('yref',"Horizontal:","Enter Y"))),                  
                      column(6,conditionalPanel("input.line_type=='REFERENCE'",textInput('xref',"Vertical:","Enter X")))
                    )
                  )),
                    fluidRow(column(12,
                      column(6,conditionalPanel("input.line_type!='NONE'",sliderInput('line_alpha',"Transparency",0,1,0.6,step=0.05,round=T))),
                      column(6,conditionalPanel("input.line_type!='NONE'",sliderInput('line_size',"Size",0,10,3,step=1,round=T)))
                    ))
                  ),
                  wellPanel(
                    fluidRow(column(12,
                      strong("Axis options:"),
                      fluidRow(
                        column(6,checkboxGroupInput("log","Log Scale",c("X","Y"),inline=T)),    
                        column(6,checkboxGroupInput("sqrt","SquareRoot",c("X","Y"),inline=T))
                      ),
                      fluidRow(
                        column(6,checkboxGroupInput("rev","Reverse",c("X","Y"),inline=T)),
                        column(6,strong("Switch Axis"),checkboxInput("flip","Y/N"))
                      )
                    ))
                  )
                    
                )            
              )  
            ),
            column(8,
              fluidRow(column(12,  
              plotOutput('plot',click="plot_click",dblclick="plot_dblclick",hover="plot_hover",height="600px",
                brush=brushOpts(id="plot_brush",resetOnNew=TRUE)),
              verbatimTextOutput('brush_data')
              ))
            )
          )
        )
        ############################################################################################################################
        ############################################################################################################################
        
      )))        
    )
