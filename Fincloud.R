# rm(list=ls(all=TRUE)) # Delete all previous objects in the workspace
# CTRL+L to clean up the console screen

# Required packages
list.of.packages <- c("apt",
                      "astsa",
                      "betareg",
                      "BiasedUrn",
                      "bios2mds",
                      "broom.mixed",
                      "changepoint",
                      "compositions",
                      "CompQuadForm",
                      "dashboardthemes",
                      "devtools",
                      "DiagrammeR",
                      "dirmult",
                      "dplyr",
                      "DT",
                      "ecodist",
                      "entropart",
                      "erer",
                      "fBasics",
                      "forecast",
                      "forestplot",
                      "fossil",
                      "fUnitRoots",
                      "gee",
                      "geepack",
                      "generics",
                      "ggfortify",
                      "ggplot2",
                      "ggthemes",
                      "gitlabr",
                      "glmm",
                      "glmmTMB",
                      "googleVis",
                      "gridExtra",
                      "gridGraphics",
                      "GUniFrac",
                      "htmltools",
                      "kableExtra",
                      "lme4",
                      "lmerTest",
                      "lmtest",
                      "lubridate",
                      "magrittr",
                      "MiRKAT",
                      "modelr",
                      "nlme",
                      "patchwork",
                      "phangorn",
                      "picante",
                      "plotly",
                      "quantreg",
                      "remotes",
                      "reshape",
                      "reticulate",
                      # "rsconnect",
                      "Rmisc",
                      "robCompositions",
                      "robustbase",
                      # "sarbcurrent",
                      "seqinr",
                      "shiny",
                      "shinydashboard",
                      "shinyjs",
                      "shinyWidgets",
                      "stringr",
                      "strucchange",
                      "survMisc",
                      "tidyverse",
                      "TSA",
                      "xtable",
                      "xts",
                      "zCompositions",
                      "zip")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Deploy the app on the cloud
# rsconnect::deployApp(appFiles='fincloud_app.r', appPrimaryDoc='fincloud_app.r')

# Load packages
suppressPackageStartupMessages(library(apt))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(betareg))
suppressPackageStartupMessages(library(BiasedUrn))
suppressPackageStartupMessages(library(bios2mds))
suppressPackageStartupMessages(library(broom.mixed))
suppressPackageStartupMessages(library(changepoint))
suppressPackageStartupMessages(library(compositions))
suppressPackageStartupMessages(library(CompQuadForm))
suppressPackageStartupMessages(library(dashboardthemes))
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(DiagrammeR))
suppressPackageStartupMessages(library(dirmult))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ecodist))
suppressPackageStartupMessages(library(entropart))
suppressPackageStartupMessages(library(erer))
suppressPackageStartupMessages(library(fBasics))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(forestplot))
suppressPackageStartupMessages(library(fossil))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(gee))
suppressPackageStartupMessages(library(geepack))
suppressPackageStartupMessages(library(generics))
suppressPackageStartupMessages(library(ggfortify))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gitlabr))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(glmm))
suppressPackageStartupMessages(library(glmmTMB))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(gridGraphics))
suppressPackageStartupMessages(library(GUniFrac))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(lmerTest))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(MiRKAT))
suppressPackageStartupMessages(library(modelr))
suppressPackageStartupMessages(library(nlme))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(phangorn))
suppressPackageStartupMessages(library(picante))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(quantreg))
suppressPackageStartupMessages(library(remotes))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(reticulate))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(robCompositions))
suppressPackageStartupMessages(library(robustbase))
suppressPackageStartupMessages(library(seqinr))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(survMisc))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(TSA))
suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(zCompositions))
suppressPackageStartupMessages(library(zip))

if(!require(rsconnect)) devtools::install_github("rstudio/rsconnect")
suppressPackageStartupMessages(library(rsconnect))

# if(!require(sarbcurrent)) devtools::install_github("KevinKotze/sarbcurrent")
# suppressPackageStartupMessages(library(sarbcurrent))

# source("Source/FinDataProc.Chart.R")

##############
##############
## COMMENTS ##
##############
##############

{
  TITLE = p("FinCloud: Intervention Analysis and Forecasting for Pharmaceutical Stocks", style = "font-size:18pt")
  HOME_COMMENT = p(strong("FinCloud", style = "font-size:15pt"), "is an interactive platform for intervention anlaysis and forecasting for pharmaceutical stocks. More details are as follows:", style = "font-size:12pt")
  HOME_COMMENT1 = ("Interactive procedures for various data formats (.rdata, .csv, .xls, .xlsx, .txt).")
  HOME_COMMENT2 = ("Intervention analysis for break points, break dates, value changes, difference tests, etc.")
  HOME_COMMENT3 = ("Forecasting for the stock price per ARIMA (autoregression, integrated, and moving average) and ARNN (autoregressive neural network) models.")
  HOME_COMMENT4 = p("[1]",tags$a(href = "https://donorschain.strikingly.com",
                                 "Choi, D. M.",
                                 class = "bodylinks"),
                    ", &", 
                    tags$a(href = "https://fina.hkust.edu.hk/faculty/directory/sethhuang",
                           "Huang, S. H.",
                           class = "bodylinks"),
                    "(2024).",tags$em("Is innovation priced? Pharmaceutical evidence from the stock market."),
                    "Working Paper, IHS and HKUST.", style = "font-size:12pt", br(),
                    "[2] Kim, J., Jang, H., &", 
                    tags$a(href = "https://orcid.org/0000-0001-6893-7164",
                           "Koh, H.",
                           class = "bodylinks"),
                    "(2024). MiMultiCat: A unified cloud platform for the analysis of microbiome data with multi-categorical responses.", 
                    tags$em("Bioengineering"),", 11(1), 60."
  )
  
  DATA_COMMENT = p(strong("Data Processing:"), br(), "Xxx."
                   , style = "font-size:12pt")
  DATA_INPUT_COMMENT = p(strong("Data Input"), br(), "Currently, this should be a '.csv' file of stock prices in a matrix of time (rows) by symbol (columns) dimensions (", 
                         tags$a(href = "px1.csv", id = "downloadLink", target="_blank", download = "px1.csv", "sample 1"),",",
                         tags$a(href = "px2.csv", id = "downloadLink", target="_blank", download = "px2.csv", "sample 2"),",",
                         tags$a(href = "px3.csv", id = "downloadLink", target="_blank", download = "px3.csv", "sample 3"),",",
                         tags$a(href = "px4.csv", id = "downloadLink", target="_blank", download = "px4.csv", "sample 4"),
                         "). A forthcoming revision to this application will enable the uploading of '.rdata', '.xls', '.xlsx', and '.txt' formats."
                         , style = "font-size:12pt")
  PRICE_CHART_COMMENT = p(strong("Price Chart"), br(), "A first-shot price chart depicts the pattern of the uploaded time series. Choose the date range from the observation period in the previous",em("Data Input"),"tab."
                          , style = "font-size:12pt")
  DATA_CONVERSION_COMMENT = p(strong("Data Conversion"), br(), "The designated data from the previous",em("Data Input")," tab will be converted to an xts-format time series object.", style = "font-size:12pt")
  IA_COMMENT = p(strong("Intervention Analysis:"), br(), "Xxx."
                 , style = "font-size:12pt")
  BREAK_POINTS_COMMENT = p(strong("Break Points"), br(), "As part of the intervention analysis framework (Box & Tiao, 1975), the break (regime shift) points of a time series is determined (Zeileis et al., 2005).", style = "font-size:12pt")
  BREAK_DATES_COMMENT = p(strong("Break & Event Dates"), br(), "The dates corresponding to the statistical break points, calculated in the previous", em("Break Points")," tab, do not necessarily coincide with the announcement dates of the FDA's new drug approval (",
                          tags$a(href = "events.csv", id = "downloadLink", target="_blank", download = "events.csv", "sample"),
                          "). This may call for the event study (Eddy & Sauders, 1980; Fama et al., 1969) of the pharmaceutical company.", style = "font-size:12pt")
  BREAK_POINTS_REFERENCES = p("[1] Box, G. E., & Tiao, G. C. (1975). Intervention analysis with applications to economic and environmental problems.", 
                              tags$em("Journal of the American Statistical Association,"),"70(349), 70-79.", br(),
                              "[2] Zeileis, A., Leisch, F., Kleiber, C., Hornik, K., 2005. Monitoring structural change in dynamic econometric models.", 
                              tags$em("Journal of Applied Econometrics,"),"20(1), 99-121.")
  BREAK_DATES_REFERENCES = p("[1] Eddy, A. R., & Saunders, G. B. (1980). New product announcements and stock prices.", 
                             tags$em("Decision Sciences,"),"11(1), 90-97.", br(),
                             "[2] Fama, E., Fisher, L., Jensen, M., & Roll, R. (1969). The adjustment of stock prices to new information.", 
                             tags$em("International Economic Review,"),"10, 1-21.")
  FC_COMMENT = p(strong("Forecasting:"), br(), "Xxx."
                 , style = "font-size:12pt")
  DIAGNOSTICS_COMMENT = p(strong("Diagnostics"), br(), "The ARIMA model is preliminarily identified per residual diagnostics referring to standardized residuals, the autocorrelation fuction of residuals, the normal Q-Q plot of standardized residuals, and the p-values for Ljung-Box (1978) statistics (Garcia, n.d.; Hamilton, 2020).", style = "font-size:12pt")
  ARIMA_COMMENT = p(strong("ARIMA Forecasting"), br(), "The ARIMA model is identified and specified per standard and numerical procedures 
                      (Box & Jenkins, 1970; Hamilton, 2020). The chosen event date provides the threshold of forecasting prior to which the data is used for training. Subsequently, in-sample forecasting is performed with 80-percent and 95-percent confidence intervals.", style = "font-size:12pt")
  ARNN_COMMENT = p(strong("ARNN Forecasting"), br(), "An autoregressive neural network (ARNN) process (Leisch et al., 1998; Taskaya-Temizel & Casey, 2005) is 
                        an extension of the autoregressive model with a feed-forward processor. Likewise in the ARIMA case, forecasting is performed beyond the chosen event date which serves as the deadline for learning.", style = "font-size:12pt")
  HORSERACE_COMMENT = p(strong("Horserace: ARIMA vs. ARNN"), br(), "The forecasting performances of ARIMA and ARNN models are compared per in-sample acccuracy measures (Klimberg et al., 2010; Makridakis, 1993) as follows: mean error (ME, a.k.a. bias), root mean square error (RMSE), mean absolute error (MAE), mean percentage error (MPE), and mean absolute percentage error (MAPE).", style = "font-size:12pt")
  DIAGNOSTICS_REFERENCES = p("[1] Garcia, S. (n.d.). ARIMA models in R.", tags$em("RPubs by RStudio."),"Retrieved from", 
                             tags$a(href = "https://rpubs.com/Sergio_Garcia/arima_models_r",
                                    "https://rpubs.com/Sergio_Garcia/arima_models_r",
                                    class = "bodylinks"),".", br(),
                             "[2] Hamilton, J. D. (2020).",tags$em("Time Series Analysis."),"New Jersey: Princeton University Press.", br(),
                             "[3] Ljung, G. M., & Box, G. E. P. (1978). On a measure of a lack of fit in time series models.", tags$em("Biometrika"),", 65(2), 297–303.")    
  ARIMA_REFERENCES = p("[1] Box, G., & Jenkins, G. (1970).", 
                       tags$em("Time Series Analysis: Forecasting and Control."),"California: Holden-Day.", br(),
                       "[2] Hamilton, J. D. (2020).",tags$em("Time Series Analysis."),"New Jersey: Princeton University Press.")    
  ARNN_REFERENCES = p("[1] Leisch, F., Trapletti, A., & Hornik, K. (1998). Stationarity and stability of autoregressive neural network processes.", 
                      tags$em("Advances in Neural Information Processing Systems,"),"11.", br(),
                      "[2] Taskaya-Temizel, T., & Casey, M. C. (2005). A comparative study of autoregressive neural network hybrids.",
                      tags$em("Neural Networks,"),"18(5-6), 781-789.")    
  HORSERACE_REFERENCES = p("[1] Klimberg, R. K., Sillup, G. P., Boyle, K. J., & Tavva, V. (2010). Forecasting performance measures – what are their practical meaning? In K. D. Lawrence & R. K. Klimberg (Eds.),", 
                           tags$em("Advances in Business and Management Forecasting"),
                           ", 7, 137-147. Emerald Group Publishing Limited.",br(),
                           "[2] Makridakis, S. (1993). Accuracy measures: theoretical and practical concerns.", 
                           tags$em("International Journal of Forecasting,"),"9(4), 527-529.")
}

########
########
## UI ##
########
########

{
  ui = dashboardPage( # Beginning of the UI
    title = "FinCloud",
    dashboardHeader(title = span(TITLE, style = "float:left;font-size: 20px"), titleWidth = "100%"),
    dashboardSidebar(
      tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
      sidebarMenu(id = "side_menu",
                  menuItem("Home", tabName = "home", icon = icon("home")),
                  menuItem("Data Processing",  icon = icon("database"),
                           menuSubItem("Data Input", tabName = "Step1", icon = icon("mouse")),
                           menuSubItem("Price Chart", tabName = "Step2", icon = icon("chart-line")),
                           menuSubItem("Data Conversion", tabName = "Step3", icon = icon("diagram-next"))),
                  menuItem("Intervention Analysis",  icon = icon("magnifying-glass-chart"),
                           menuSubItem("Break Points", tabName = "Step4", icon = icon("stairs")),
                           menuSubItem("Break & Event Dates", tabName = "Step5", icon = icon("calendar-days"))),
                  menuItem("Forecasting",  icon = icon("chart-line"),
                           menuSubItem("Diagnostics", tabName = "Step6", icon = icon("stethoscope")),
                           menuSubItem("ARIMA", tabName = "Step7", icon = icon("chart-line")),
                           menuSubItem("ARNN", tabName = "Step8", icon = icon("chart-line")),
                           menuSubItem("Horserace", tabName = "Step9", icon = icon("horse"))))),
    dashboardBody(
      tags$head(tags$style(HTML(".content { padding-top: 2px;}"))),
      tags$script(src = "fileInput_text.js"),
      useShinyjs(),
      #     shinyDashboardThemes(theme = "onenote"),
      #     uiOutput("themes"),
      tabItems(
        
        ########        
        # HOME #
        ########        
        
        tabItem(tabName = "home",
                div(id = "homepage", br(), HOME_COMMENT,
                    tags$ol(
                      tags$li(HOME_COMMENT1), tags$li(HOME_COMMENT2), tags$li(HOME_COMMENT3),
                      style = "font-size:12pt"),
                    column(width = 12, style='padding-left:0px',
                           box(title = strong("References", style = "color:black"), width = NULL, status = "primary", solidHeader = TRUE,
                               HOME_COMMENT4)), br(),
                    # fluidRow()
                    
                    # Use the img tag to insert a picture
                    img(src = "FinCloud_home.png", 
                        height = "1050px", width = "700px")
                    # Other dashboard components go here
                )),
        
        ##############
        # DATA INPUT #
        ##############
        
        tabItem(tabName = "Step1", br(),DATA_INPUT_COMMENT,
                
                fluidRow(
                  ## File upload window
                  sidebarLayout(
                    sidebarPanel(
                      # Input: Select a file
                      fileInput("file", "Choose CSV File",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      
                      # Horizontal line
                      tags$hr(),
                      selectInput("x_var", "Select Date Series", ""),
                      selectInput("y_var", "Select Price Series", ""),
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # Checkbox to display the raw data
                      checkboxInput("ShowDesignatedData", "Show Designated Data"),
                      
                      # Output: Designated data
                      verbatimTextOutput("DesignatedData")
                      
                    ),
                    mainPanel(
                      # Output: Data table
                      tableOutput("DataSummary")
                      
                    )
                  )
                  ## File upload window
                )
        ),        
        
        ###############
        # PRICE CHART #
        ###############
        
        tabItem( # Tab item open
          tabName = "Step2", br(),
          PRICE_CHART_COMMENT,  
          
          fluidRow(
            sidebarLayout(
              sidebarPanel(
                
                # Output: Observation period
                verbatimTextOutput("ObsPeriod")
                
              ),
              mainPanel(
              )
            )
          ),
          
          # Date input
          # https://stackoverflow.com/questions/48633984/pass-a-dynamic-date-date-range-to-daterangeinput-in-r-shiny
          dateRangeInput('Date',
                         label = 'Date Range',
                         start = "1990-01-01",
                         end = Sys.Date(),
                         max = Sys.Date()
                         
                         ####################
                         # FIND THIS OUT!!! #        
                         ####################
                         # start = textOutput("PlotBegin"),
                         # end = textOutput("PlotEnd"),
                         # max = textOutput("PlotEnd")
          ),
          
          # Price chart      
          plotOutput("PriceChart")
          # verbatimTextOutput("PriceChart")
          
        ), # Tab item closure
        
        ###################        
        # DATA CONVERSION #
        ###################        
        
        tabItem(tabName = "Step3", br(),
                DATA_CONVERSION_COMMENT, 
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      
                      # Fancy action buttons: https://rdrr.io/cran/shinyWidgets/man/actionBttn.html
                      actionButton("ConvertButton", "Convert!"), 
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # Converted xts panel shown
                      verbatimTextOutput("xts")
                      
                      # Use the img tag to insert a picture
                      # img(src = "dc_example.png", width = "320px",height = "268.24px")
                      
                    ),
                    mainPanel(
                    )
                  )
                )
        ),
        
        ################                
        # BREAK POINTS #
        ################                
        
        tabItem(tabName = "Step4", br(),
                BREAK_POINTS_COMMENT, 
                
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      
                      # Calculate Break Points" button 
                      actionButton("calculateButton", "Calculate Break Points"), br(),
                      
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       # tags$div(em("Break point calculation for"),HTML(as.character(textOutput(Symbol))),em("under way..."),id="loadmessage"))
                                       tags$div(em("Please wait..."),id="loadmessage"))                      
                    ),
                    mainPanel(
                    )
                  )
                ), 
                
                # Break point chart      
                plotOutput("bpchart"), br(),
                
                # Break point chart - example
                # img(src = "bp_example.png", height = "350px", width = "700px"), br(),br(),
                
                # References    
                column(width = 12, style='padding-left:0px',
                       box(title = strong("References", style = "color:black"), width = NULL, status = "primary", solidHeader = TRUE,
                           BREAK_POINTS_REFERENCES)
                )
        ),
        
        #######################        
        # BREAK & EVENT DATES #
        #######################        
        
        tabItem(tabName = "Step5", br(),
                BREAK_DATES_COMMENT, 
                
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div(em("Please wait..."),id="loadmessage")),
                      
                      # verbatimTextOutput("execStatus"),
                      
                      # Output: break dates
                      verbatimTextOutput("bd"), # Paired with renderPrint()
                      # tableOutput("bd"), # Paired with renderTable()
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # Input: Upload the event file
                      fileInput("EventsIn", "Upload Event Dates",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      
                      # Checkbox to display the raw data
                      checkboxInput("ShowEventDates", "Show Event Dates"),
                      
                      # Output: Abridged event dates
                      verbatimTextOutput("EventsOut")
                      
                    ),
                    mainPanel()
                  )
                ), 
                
                # Break and event date chart      
                plotOutput("edchart"), br(),
                
                # Break dates vs. event dates - example 
                # img(src = "ed_example.png", height = "350px", width = "700px"), br(),br(),
                # Other dashboard components go here
                
                column(width = 12, style='padding-left:0px',
                       box(title = strong("References", style = "color:black"), width = NULL, status = "primary", solidHeader = TRUE,
                           BREAK_DATES_REFERENCES)
                )
        ),
        
        #################
        #################
        ## Forecasting ##
        #################
        #################
        
        ###############  
        # Diagnostics #
        ###############  
        
        tabItem(tabName = "Step6", br(),
                DIAGNOSTICS_COMMENT,
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      
                      ########################################################  
                      # MUST BE DEACTIVATED UNLESS PREVIOUS ACTIONS ARE DONE #
                      ########################################################  
                      
                      # Wrap the action button in a div with an id
                      # div(id = "DiagnoseButton_div",
                      #    actionButton("DiagnoseButton", "Diagnose!", disabled = TRUE)
                      # ),  
                      
                      actionButton("DiagnoseButton", "Diagnose!"), 
                      
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div(em("Please wait..."),id="loadmessage")),                      
                      
                      # Horizontal line
                      tags$hr(),
                      
                      verbatimTextOutput("diagnostics_out"), # Paired with renderPrint()
                      
                    ),
                    mainPanel()
                  )
                ),
                tags$head(
                  tags$style(HTML(".my-absolute-panel { border: none; /* Remove border */}")
                  )
                ),
                absolutePanel(
                  top = "330px",  # Adjust top position as needed
                  left = "245px",  # Adjust left position as needed
                  width = "8.1in",  # Adjust width as needed
                  height = "6.5in",  # Adjust height as needed
                  class = "my-absolute-panel",  # Add a custom class
                  # style = "overflow-y: auto; border: 1px solid black",  # Optional styling
                  imageOutput("residual_plots")
                ),  
                
                # column(width = 5, height=4, units="in",imageOutput("residual_plots")), 
                # Horizontal line
                tags$hr(),
                
                # Charts
                # plotOutput("residual_plots"),
                # verbatimTextOutput("residual_plots"),
                # imageOutput("residual_plots"), br(),
                
                # Use the img tag to insert a picture
                # img(src = "residuals_example.png", height = "581.5px", width = "700px"), br(),br(),
                
                absolutePanel(
                  top = "920px",  # Adjust top position as needed
                  left = "245px",  # Adjust left position as needed
                  width = "17.2in",  # Adjust width as needed
                  height = "2in",  # Adjust height as needed
                  class = "my-absolute-panel",  # Add a custom class
                  
                  column(width = 12, style='padding-left:0px',
                         box(title = strong("References", style = "color:black"), width = NULL, status = "primary", solidHeader = TRUE,
                             DIAGNOSTICS_REFERENCES)
                         
                  )
                  
                )
        ),
        
        
        #########  
        # ARIMA #
        #########  
        
        tabItem(tabName = "Step7", br(),
                ARIMA_COMMENT,
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("EventDate", "Select Event Date", ""),
                      
                      # Horizontal line
                      tags$hr(),
                      
                      actionButton("ARIMA_forecast", "Forecast!"), 
                      
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div(em("Please wait..."),id="loadmessage")),                      
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # Output: Chosen event date for the forecasting threshold
                      verbatimTextOutput("arima_process"), # Paired with renderPrint()
                      
                    ),
                    mainPanel(
                    )
                  )
                ),
                
                # ARIMA forecasting chart
                plotOutput("arimachart"), br(),
                
                # Use the img tag to insert a picture
                # img(src = "arima_example.png", height = "350px", width = "700px"), br(),br(),
                
                column(width = 12, style='padding-left:0px',
                       box(title = strong("References", style = "color:black"), width = NULL, status = "primary", solidHeader = TRUE,
                           ARIMA_REFERENCES)
                )
        ),
        
        ########        
        # ARNN #
        ########        
        
        tabItem(tabName = "Step8", br(),
                ARNN_COMMENT,
                
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      # Output: Chosen event date for the forecasting threshold
                      verbatimTextOutput("EventDate"), # Paired with renderPrint()
                      
                      # selectInput("EventDate", "Select Event Date", ""),
                      
                      # Horizontal line
                      tags$hr(),
                      
                      actionButton("ARNN_forecast", "Forecast!"), 
                      
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div(em("Please wait..."),id="loadmessage")),                      
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # Output: Chosen event date for the forecasting threshold
                      verbatimTextOutput("arnn_process"), # Paired with renderPrint()
                      
                    ),
                    mainPanel(
                    )
                  )
                ),
                
                # ARNN forecasting chart
                plotOutput("arnnchart"), br(),
                
                # Use the img tag to insert a picture
                # img(src = "arnn_example.png", height = "350px", width = "700px"), br(),br(),
                # Other dashboard components go here
                
                column(width = 12, style='padding-left:0px',
                       box(title = strong("References", style = "color:black"), width = NULL, status = "primary", solidHeader = TRUE,
                           ARNN_REFERENCES)
                )
        ),
        
        #############        
        # Horserace #
        #############        
        
        tabItem(tabName = "Step9", br(), HORSERACE_COMMENT, br(),
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      
                      # Calculate Break Points" button 
                      actionButton("HorseraceButton", "Horserace!"),
                      
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div(em("Please wait..."),id="loadmessage")),                      
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # DTOutput("ARIMA_accuracy"),
                      tableOutput("ARIMA_accuracy"),
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # DTOutput("ARNN_accuracy"),
                      tableOutput("ARNN_accuracy"),
                      
                      # Horizontal line
                      tags$hr(),
                      
                      # Output: Chosen event date for the forecasting threshold
                      verbatimTextOutput("horserace_report") # Paired with renderPrint()
                      
                    ),
                    mainPanel(
                    )
                  )
                ),
                
                column(width = 12, style='padding-left:0px',
                       box(title = strong("References", style = "color:black"), width = NULL, status = "primary", solidHeader = TRUE,
                           HORSERACE_REFERENCES)
                )
        )
        
        
      )
    )  
  ) # End of the UI
} 

############
############
## server ##
############
############

server <- function(input, output, session){  # Beginning of the server
  
  ##############
  # DATA INPUT #
  ##############    
  
  # Uploaded matrix of stock prices in a csv-format
  data0 <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    # df <- read.csv("e://DanielData//Research//Projects//FinCloud//Data//KFDA//px.csv")
    df
  })
  
  # Reactive expression to read the selected file
  output$DataSummary <- renderTable({
    df <- data0()
    # Return the abridged data frame
    df[c(1:5,(nrow(df)-4):nrow(df)),c(1:5,(ncol(df)-4):ncol(df))]
  })
  
  # observe function: https://appsilon.com/observe-function-r-shiny/  
  observe({
    # req(input$file)
    # Read the CSV file
    # df <- read.csv(input$file$datapath)
    df <- data0()
    
    # Update choices for the date (X) and price (Y) series
    updateSelectInput(session, "x_var", "Select Date Series", choices = names(df))
    updateSelectInput(session, "y_var", "Select Price Series", choices = names(df))
  })
  
  # Designated data with the date and chosen stock price series
  data1 <- reactive({
    results <- list()
    df <- data0()
    
    # Chosen X-variable's name ("D/date")
    # results$x_var_name <- x_var_name <- names(which(sapply(df, identical, y = df[[input$x_var]])))
    
    # Chosen Y-variable's name (stock code/ticker/symbol)
    results$Symbol <- names(which(sapply(df, identical, y = df[[input$y_var]])))
    
    df <- data.frame(cbind(df[[input$x_var]],df[[input$y_var]])) 
    names(df) <- c("Date","Price") 
    
    # Leave out non-trading days
    results$df <- df <- df[which(!is.na(df[,2]))[1]:tail(which(!is.na(df[,2])),1),]      
    results$Date <- df$Date <- date(df$Date)
    results$Price <- df$Price
    results
  })
  
  # Stock code/ticker/symbol
  output$Symbol <- renderText({data1()$Symbol})
  
  # Display the designated data with the date and chosen stock price series
  output$DesignatedData <- renderPrint({
    # Show only if the checkbox is checked
    if(input$ShowDesignatedData){
      df <- data1()$df; names(df) <- c("Date",data1()$Symbol)
      # First and last 5 rows
      df[c(1:5,(nrow(df)-4):nrow(df)),]
    }
  })  
  
  ###############
  # PRICE CHART #
  ###############
  
  # Observation period
  output$ObsPeriod <- renderPrint({
    # Show only if the previous "show designated data" checkbox is checked
    if(input$ShowDesignatedData){
      df <- data1()$df 
      # First through last observations
      df[c(1,nrow(df)),] 
    }
  })  
  
  # Plot-begin date
  output$PlotBegin <- renderText({df <- data1()$df;df[1,1]})
  
  # Plot-end date
  output$PlotEnd <- renderText({df <- data1()$df;df[nrow(df),1]})
  
  # A first-shot price chart
  output$PriceChart <- renderPlot({
    
    req(input$file); df <- read.csv(input$file$datapath)
    # df <- data1()$df
    Symbol <- data1()$Symbol # Chosen Y-variable's name
    # names(df) <- c("Date","Price") 
    # df$Date <- date(df$Date); 
    df[,1] <- date(df[,1])
    
    p <- ggplot(df, aes(x = df[[input$x_var]], y = df[[input$y_var]])) +
      # p <- ggplot(df, aes(x = Date, y = Price)) +
      geom_line() +
      labs(title = paste("Price Chart for",Symbol), x = "", y = "Price") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y",limits = input$Date) + 
      # scale_x_date(date_breaks = "1 year", date_labels = "%b %Y",limits = input$date) + 
      theme_minimal()
    p
    
  })
  
  ###################        
  # DATA CONVERSION #
  ###################        
  
  # Convert the uploaded data to an xts object
  # observe({
  data2 <- reactive({
    results <- list()
    df <- data1()$df      
    
    # xts 
    results$xts <- xts(as.numeric(df[,2]), order.by=date(df[,1]))
    results$Symbol <- colnames(results$xts) <- data1()$Symbol
    results
  })
  
  # An abridged output
  output$xts <- renderPrint({
    req(input$ConvertButton)
    xts <- data2()$xts    
    xts[c(1:5,(nrow(xts)-4):nrow(xts)),]
  })  
  
  ########################  
  # IA/Fc Chart Function #
  ########################
  
  # source("Source/FinDataProc.Chart.R")
  
  iafc_plot <- function(df,Symbol,
                        break_dates=NULL,
                        event_dates=NULL,
                        type=c("BP","ED","ARIMA","ARNN")){ # function open
    
    if(type=="ARIMA"){ # if open
      
      ###############
      # ARIMA chart #
      ###############
      
      forecast <- fc_process()$forecast
      p <- ggplot(df) +
        geom_line(aes(x=Date,y=Price,color="Stock Price",linetype="Stock Price"),lwd=0.5) +
        geom_line(aes(x=Date,y=Breaks,color="Break Steps",linetype="Break Steps"),lwd=1) +
        geom_line(aes(x=Date,y=Mean,color="Forecasts",linetype="Forecasts"),lwd=1.5) +
        geom_ribbon(aes(x=Date,ymin=lo80,ymax=up80,fill="80% CI"),alpha=0.2) +
        geom_ribbon(aes(x=Date,ymin=lo95,ymax=up95,fill="95% CI"),alpha=0.3) +
        scale_x_date(date_labels = "%Y",date_breaks = "1 years") +	
        labs(title = paste("Forecasts from",forecast$method,"for",Symbol),x = "",y = "Price") +
        scale_color_manual(name="",values = c("Stock Price"="black","Break Steps"="red","Forecasts"="blue")) +	
        scale_fill_manual(name="",values = c("80% CI"="blue","95% CI"="grey")) +
        scale_linetype_manual(name="",values = c("Stock Price"="solid","Break Steps"="dashed","Forecasts"="dotted")) +
        guides(fill = guide_legend(keywidth=1,keyheight=1),linetype=guide_legend(keywidth=3,keyheight=1),
               color=guide_legend(keywidth=3,keyheight=1)) +
        theme(
          axis.line = element_line(size = 0.5, color = "black", linetype=1),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      y.max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2] # Maximum of the y-axis range
      p <- p +
        # Structural changes
        annotate(geom = "vline",
                 x = break_dates,
                 xintercept = break_dates,
                 linetype = rep("dashed",length(break_dates)),
                 color = rep("darkgrey",length(break_dates))) +
        annotate(geom = "text",
                 label = format(break_dates,format="%b %d, %y"),
                 x = break_dates,
                 y = rep(y.max*0.9,length(break_dates)),
                 angle = 40, 
                 color = "darkgrey",
                 size = 3,  
                 vjust = 1) +
        # Drug approvals
        annotate(geom = "vline",
                 x = event_dates,
                 xintercept = event_dates,
                 linetype = rep("solid",length(event_dates)),
                 color = rep("darkgrey",length(event_dates)),
                 lwd=1) +
        annotate(geom = "text",
                 label = format(event_dates,format="%b %d, %y (New Drug)"),
                 x = event_dates,
                 y = rep(y.max*0.75,length(event_dates)),
                 angle = 90, 
                 color = "darkgrey",
                 size = 3,  
                 vjust = 1.5)
      p  
      
    } else if(type=="ARNN"){
      
      ##############
      # ARNN chart #
      ##############
      
      forecast <- fc_process()$forecast.nn
      p <- ggplot(df) +
        geom_line(aes(x=Date,y=Price,color="Stock Price",linetype="Stock Price"),lwd=0.5) +
        geom_line(aes(x=Date,y=Breaks,color="Break Steps",linetype="Break Steps"),lwd=1) +
        geom_line(aes(x=Date,y=NN,color="Forecasts",linetype="Forecasts"),lwd=1.5) +
        scale_x_date(date_labels = "%Y",date_breaks = "1 years") +	
        labs(title = paste("Forecasts from",forecast$method,"for",Symbol),x = "",y = "Price") +
        scale_color_manual(name="",values = c("Stock Price"="black","Break Steps"="red","Forecasts"="blue")) +	
        scale_fill_manual(name="",values = c("80% CI"="blue","95% CI"="grey")) +
        scale_linetype_manual(name="",values = c("Stock Price"="solid","Break Steps"="dashed","Forecasts"="dotted")) +
        guides(fill = guide_legend(keywidth=1,keyheight=1),linetype=guide_legend(keywidth=3,keyheight=1),
               color=guide_legend(keywidth=3,keyheight=1)) +
        theme(
          axis.line = element_line(size = 0.5, color = "black", linetype=1),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      y.max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2] # Maximum of the y-axis range
      p <- p +
        # Structural changes
        annotate(geom = "vline",
                 x = break_dates,
                 xintercept = break_dates,
                 linetype = rep("dashed",length(break_dates)),
                 color = rep("darkgrey",length(break_dates))) +
        annotate(geom = "text",
                 label = format(break_dates,format="%b %d, %y"),
                 x = break_dates,
                 y = rep(y.max*0.9,length(break_dates)),
                 angle = 40, 
                 color = "darkgrey",
                 size = 3,  
                 vjust = 1) +
        # Drug approvals
        annotate(geom = "vline",
                 x = event_dates,
                 xintercept = event_dates,
                 linetype = rep("solid",length(event_dates)),
                 color = rep("darkgrey",length(event_dates)),
                 lwd=1) +
        annotate(geom = "text",
                 label = format(event_dates,format="%b %d, %y (New Drug)"),
                 x = event_dates,
                 y = rep(y.max*0.75,length(event_dates)),
                 angle = 90, 
                 color = "darkgrey",
                 size = 3,  
                 vjust = 1.5)
      p
      
    } else if (type=="ED"){
      
      #############################  
      # Break & event dates chart #    
      #############################  
      
      p <- ggplot(df) +
        geom_line(aes(x=Date,y=Price,color="Stock Price",linetype="Stock Price"),lwd=0.5) +
        geom_line(aes(x=Date,y=Breaks,color="Break Steps",linetype="Break Steps"),lwd=1) +
        #	scale_fill_discrete(values = c("Price" = "Stock Price","Breaks" = "Break Steps"),breaks=c("Price","Breaks")) +	
        scale_color_manual(name="",values = c("Stock Price"="black","Break Steps"="red")) +	
        scale_x_date(date_labels = "%Y",date_breaks = "1 years") +	
        labs(title = paste("Break & Event Dates for",Symbol),x = "",y = "Price") +
        scale_linetype_manual(name="",values = c("Stock Price"="solid","Break Steps"="dashed","Forecasts"="dotted")) +
        guides(fill = guide_legend(keywidth=1,keyheight=1),linetype=guide_legend(keywidth=3,keyheight=1),
               color=guide_legend(keywidth=3,keyheight=1)) +
        theme(
          axis.line = element_line(size = 0.5, color = "black", linetype=1),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      y.max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2] # Maximum of the y-axis range
      p <- p +
        labs(title = paste("Break & Event Dates for",Symbol),x = "",y = "Price") +
        # Structural changes
        annotate(geom = "vline",
                 x = break_dates,
                 xintercept = break_dates,
                 linetype = rep("dashed",length(break_dates)),
                 color = rep("darkgrey",length(break_dates))) +
        annotate(geom = "text",
                 label = format(break_dates,format="%b %d, %y"),
                 x = break_dates,
                 y = rep(y.max*0.9,length(break_dates)),
                 angle = 40, 
                 color = "darkgrey",
                 size = 3,  
                 vjust = 1) +
        # Drug approvals
        annotate(geom = "vline",
                 x = event_dates,
                 xintercept = event_dates,
                 linetype = rep("solid",length(event_dates)),
                 color = rep("darkgrey",length(event_dates)),
                 lwd=1) +
        annotate(geom = "text",
                 label = format(event_dates,format="%b %d, %y (New Drug)"),
                 x = event_dates,
                 y = rep(y.max*0.75,length(event_dates)),
                 angle = 90, 
                 color = "darkgrey",
                 size = 3,  
                 vjust = 1.5)
      
      p  
      
    } else if (type=="BP"){
      
      #####################  
      # Break Point Chart #    
      #####################  
      
      p <- ggplot(df) +
        geom_line(aes(x=Date,y=Price,color="Stock Price",linetype="Stock Price"),lwd=0.5) +
        geom_line(aes(x=Date,y=Breaks,color="Break Steps",linetype="Break Steps"),lwd=1) +
        #	scale_fill_discrete(values = c("Price" = "Stock Price","Breaks" = "Break Steps"),breaks=c("Price","Breaks")) +	
        scale_color_manual(name="",values = c("Stock Price"="black","Break Steps"="red")) +	
        scale_x_date(date_labels = "%Y",date_breaks = "1 years") +	
        labs(title = paste("Break Points for",Symbol),x = "",y = "Price") +
        scale_linetype_manual(name="",values = c("Stock Price"="solid","Break Steps"="dashed","Forecasts"="dotted")) +
        guides(fill = guide_legend(keywidth=1,keyheight=1),linetype=guide_legend(keywidth=3,keyheight=1),
               color=guide_legend(keywidth=3,keyheight=1)) +
        theme(
          axis.line = element_line(size = 0.5, color = "black", linetype=1),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      p
      
    } 
    
  } # function closure  
  
  #########################        
  # Intervention Analysis #
  #########################        
  
  # Break point calculation and chart plotting process
  # ia_process <- reactive({ # IA process open
  ia_process <- function(){
    
    results <- list()
    
    # break points
    bpdata <- data2()$xts; colnames(bpdata) <- "Price"
    results$Symbol <- Symbol <- data2()$Symbol
    break_points <- breakpoints(bpdata ~ 1)
    
    # break dates
    results$break_dates <- break_dates <- date(bpdata)[break_points[[1]]]
    bic <- summary(break_points)[[3]][2,] # BIC box
    m <- as.numeric(names(which(bic==min(bic)))) # Optimal number of break points
    break_steps <- xts(fitted(break_points, breaks=m), order.by=date(bpdata))
    
    # Break (regime switching) dates
    output$bd <- renderPrint({
      cat("The break dates for",Symbol,"are",format(break_dates, "%Y-%m-%d"),"\n")
    })
    
    # Data frame for charts
    df <- data.frame(
      # Train data
      Date = c(date(bpdata)),
      Price = c(as.numeric(bpdata$Price)),
      # Break steps
      Breaks = c(as.numeric(break_steps[,1]))
    )
    results$df <- df
    
    results        
    # }) # IA process closure 
  }
  
  # Enable the action button once the data is converted to an xts format
  # data_converted <- reactive({!is.null(input$xts)})
  # observe({
  #  if(data_converted()){shinyjs::enable("calculateButton")} 
  #  else{shinyjs::disable("calculateButton")}
  #  })  
  
  # ia0 <- ia_process()  
  
  # Break point chart  
  observeEvent(input$calculateButton, {
    
    output$bpchart <- renderPlot({
      ia0 <- ia_process()
      iafc_plot(df = ia0$df,
                Symbol = ia0$Symbol,
                type="BP"
      )
    })
    
  })
  
  # The list of event dates for the chosen stock
  event_dates <- reactive({
    req(input$EventsIn)
    df <- read.csv(input$EventsIn$datapath)
    df[which(df[,1]==data2()$Symbol),]
  })
  
  # Display the list of event dates
  output$EventsOut <- renderPrint({
    
    # Show the event date list of the chosen stock only if the checkbox is checked
    if(input$ShowEventDates){
      event_dates() 
    }
    
  })  
  
  # Break & event dates chart
  output$edchart <- renderPlot({ # Break & event dates chart open 
    
    # Plot only if the event dates are uploaded
    req(input$EventsIn)
    ia0 <- ia_process()
    iafc_plot(df = ia0$df,
              Symbol = ia0$Symbol,
              break_dates = date(c(ia0$break_dates)),
              event_dates = date(c(event_dates()$Date)),
              type="ED"
    )
    
  }) # Break & event dates chart closure
  
  ###############
  # Forecasting # 
  ###############
  
  # observe function: https://appsilon.com/observe-function-r-shiny/  
  observeEvent(input$EventsIn, {
    # observe({
    df <- event_dates() # Event dates = forecasting thresholds
    # Update choice for 
    updateSelectInput(session, "EventDate", "Select Event Date", choices = c(df[,2]))
    
  })
  
  # Forecasting and chart plotting process
  # fc_process <- reactive({ 
  fc_process <- function(){  
    
    results <- list()
    
    # req(input$EventsIn)
    # In-sample forecasting
    deadline <- date(session$input$EventDate)-1 # Train until the previous day of the first drug approval announcement
    ia0 <- ia_process()
    df <- ia0$df # data frame from data2()
    results$Symbol <- Symbol <- ia0$Symbol
    results$break_dates <- break_dates <- date(c(ia0$break_dates))
    results$event_dates <- event_dates <- date(c(event_dates()$Date))
    results$train <- train <- df[df$Date <= deadline,]
    
    results$arima.fit <- arima.fit <- auto.arima(train$Price, stepwise = FALSE, trace = FALSE)
    nn.fit <- nnetar(train$Price)
    
    # In-sample forecasting period
    h <- sum(as.numeric(df$Date>deadline)) 
    
    results$forecast <- forecast <- forecast(arima.fit, h = h) # ARIMA forecasts
    forecast_dates <- seq(deadline+1, length = h, by = "day") # forecast period
    forecast_mean <- xts(forecast$mean, order.by=forecast_dates) # ARIMA forecast means
    results$forecast.nn <- forecast.nn <- forecast(nn.fit, h = h) # ARNN forecasts
    
    # In-sample forecasting
    results$df <- data.frame(
      # Train data
      Date = df$Date,
      Price = df$Price,
      Breaks = df$Breaks,
      # Forecast data with confidence intervals 
      Mean = c(rep(NA,length(train$Date)),forecast$mean),
      lo80 = c(rep(NA,length(train$Date)),forecast$lower[,1]),
      lo95 = c(rep(NA,length(train$Date)),forecast$lower[,2]),
      up80 = c(rep(NA,length(train$Date)),forecast$upper[,1]),
      up95 = c(rep(NA,length(train$Date)),forecast$upper[,2]),
      NN = c(rep(NA,length(train$Date)),forecast.nn$mean)
    )
    results
    # })
  }  
  
  ################  
  # Diagnostics #  
  ################  
  
  # Enable the action button once the plot is generated
  # edchart_generated <- reactive({!is.null(input$edchart)})
  # observe({
  #  if(edchart_generated()){shinyjs::enable("DiagnoseButton")} 
  #  else{shinyjs::disable("DiagnoseButton")}
  #  })  
  
  observeEvent(input$DiagnoseButton, {
    
    # req(input$EventsIn)
    
    fc0 <- fc_process()
    arima.par <- arimaorder(fc0$arima.fit)
    
    # output$residual_plots <- renderPlot({        
    output$residual_plots <- renderImage({
      
      temp_png <- tempfile(fileext = ".png")
      png(file=temp_png, width=5, height=4, units="in", res=145)
      sarima(data2()$xts, p=arima.par[1], d=arima.par[2], q=arima.par[3])
      dev.off()
      
      # Report "diagnostics.png"
      list(src = temp_png, contentType = "image/png")
      
    }, deleteFile = TRUE)
    
    # img(src = "residuals_example.png", height = "581.5px", width = "700px"), br(),br(),
    
    output$diagnostics_out <- renderPrint({
      
      cat("The identified model for the standardized residuals of",data2()$Symbol,"is ARIMA(",
          arima.par[1],arima.par[2],arima.par[3],")","\n")
      
    })
    
  })  
  
  
  #########
  # ARIMA #    
  #########
  
  observeEvent(input$ARIMA_forecast, {    
    
    fc0 <- fc_process()
    
    output$arima_process <- renderPrint({
      cat("The best model for",data2()$Symbol,"is",fc0$forecast$method,"\n")
    })
    
    output$arimachart <- renderPlot({ # Chart open 
      iafc_plot(df = fc0$df,
                Symbol = fc0$Symbol,
                break_dates = fc0$break_dates,
                event_dates = fc0$event_dates,
                type = "ARIMA"
      )
    }) # Chart closure
    
  })
  
  ########
  # ARNN #    
  ########
  
  
  # Conditional report
  output$EventDate <- renderPrint({
    req(input$EventsIn)
    cat("The chosen event date for forecasting is",session$input$EventDate,"\n")
  })
  
  observeEvent(input$ARNN_forecast, {    
    
    fc0 <- fc_process()
    
    output$arnn_process <- renderPrint({
      cat("The best model for",data2()$Symbol,"is",fc0$forecast.nn$method,"\n")
    })
    
    output$arnnchart <- renderPlot({ # Chart open 
      iafc_plot(df = fc0$df,
                Symbol = fc0$Symbol,
                break_dates = fc0$break_dates,
                event_dates = fc0$event_dates,
                type = "ARNN"
      )
    }) # Chart closure  
    
  })
  
  #############
  # Horserace #
  #############
  
  observeEvent(input$HorseraceButton, {
    
    fc0 <- fc_process()
    df <- fc_process()$df
    
    output$ARIMA_accuracy <- renderTable({
      
      box <- accuracy(df$Mean,df$Price) # ARIMA
      rownames(box) <- "ARIMA" 
      box
    }, rownames=TRUE)
    
    output$ARNN_accuracy <- renderTable({
      
      box <- accuracy(df$NN,df$Price) # ARNN
      rownames(box) <- "ARNN" 
      box
    }, rownames=TRUE)
    
    output$horserace_report <- renderPrint({
      arima <- accuracy(df$Mean,df$Price)
      arnn <- accuracy(df$NN,df$Price)
      
      if(mean(arima-arnn)>0){
        
        cat("ARNN wins over ARIMA for the forecasting horserace of",data2()$Symbol,
            "from",format(input$EventDate,format="%Y-%m-%d"),"until",
            format(df$Date[nrow(df)],format="%Y-%m-%d"),"\n")    
        
      } else {
        
        cat("ARIMA wins over ARNN for the forecasting horserace of",data2()$Symbol,
            "from",format(input$EventDate,format="%Y-%m-%d"),"until",
            format(df$Date[nrow(df)],format="%Y-%m-%d"),"\n")    
        
      }
      
    })
    
  })
  
} # End of the server

# Create Shiny app object
shinyApp(ui, server)