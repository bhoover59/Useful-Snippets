# HONO Shiny: OH Calibration, Extractor, Post Extraction Edits, HONO Photolysis Efficiency, Data Logger Averages, Merge Data Sets
# About -------------------------------------------------------------------
# Written by Bode Hoover (bodehoov@iu.edu)
# Adapted from VBA code by Emily Reidy
# Must use file that has cycling or you will get NA (removes long online cycles)
# Calibrator constants @ 10 LPM and needs periodically updated in raw code. Not part of UI
# Publish App -------------------------------------------------------------
# To publish:
# rsconnect::deployApp('W:\\Lab\\Lab-pstevens\\Bode\\Extractors\\Folder with file')
# rsconnect::deployApp('/Users/bodehoover/Downloads/HONO Analysis')
# rsconnect::deployApp('path\\to\\file) # file must be named app.R and be only file in folder, need \\ instead of \ to avoid unrecognized escape character error
# Clear Memory ------------------------------------------------------------
rm(list = ls()) 
# Packages ----------------------------------------------------------------
library(shiny) # allows Shiny UI
library(shinyjs) # allows Shiny UI
library(DT) # allows conversion to DateTime
library(dplyr)
library(tibble)
library(lubridate) # allows filtering options
library(ggplot2) # allows plotting options
library(readr)
library(zoo) # allows rolling mean function
library(data.table) # allows read and create data tables
library(ggpmisc) 
library(minpack.lm)
library(grid) # allows combining plots into grids
library(gtable)
library(gridExtra) # allows combining plots
library(rsconnect) # allows publishing RShiny
library(stringr) # allows piecing together strings
library(openair) 
library(shinythemes) # allows theme editing
library(shinyWidgets) # allows theme editing
library(bslib) # allows theme editing
library(plotly) # allows interactive plots

# Start UI -----------------------------------------------------------
# Define UI for application
ui <- fluidPage(
  # Theme -------------------------------------------------------------------
  useShinyjs(),
  # theme = shinytheme("slate"),
  # shinythemes::themeSelector(),
  # theme = bs_theme(version = 4),
  # bg = background behind words
  # fg = words
  # primary = color of primary buttons
  # primary 74, 55, 53
  # theme = bs_theme(bg = "rgb(81, 76, 76)", bootswatch = "slate", fg = "rgb(192,192,192)", primary = "rgb(110, 61, 61)"),# burgundy background, white words, burgundy dark buttons
  theme = bs_theme(bg = "rgb(96,96,96)", bootswatch = "sandstone", fg = "rgb(255,255,255)", primary = "rgb(137,38,38)"),
  tabsetPanel(
    # HONO Extractor UI -------------------------------------------------------
    tabPanel('HONO Extractor',
             sidebarLayout(
               sidebarPanel(
                 fileInput('raw_data_file_HONO', label = 'Raw data file (txt):', multiple = FALSE, placeholder = "No file selected"),
                 strong("Laser Parameters"),
                 fluidRow(
                   column(7, numericInput('P308_start_HONO', value = 2.244, step = 0.01, label = '308 Laser Power (mW):')),
                   column(5, numericInput('UVout1_start_HONO', value = 0.0238, step = 0.0001, label = '308 UVout1 (V):'))
                 ),
                 fluidRow(
                   column(7, numericInput('P355_start_HONO', value = 1.115, step = 0.01, label = '355 Laser Power (mW):')),
                   column(5, numericInput('UVout3_start_HONO', value = 3.012, step = 0.001, label = '355 UVout3 (V):'))
                 ),
                 column(12, style = "border-top:1px solid"),
                 strong("Remove Lock Points"),
                 fluidRow(
                   column(7, numericInput('delete_before_HONO', value = 2, min = 1, step = 1, label = "Before")),
                   column(5, numericInput('delete_after_HONO', value = 2, min = 1, step = 1, label = "After"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 strong("OH Calibration Parameters"),
                 numericInput('OHCal_yint', value = 1.26e-7, min = 0, step = 1e-8, label = "y int (1.26e-7)"),
                 numericInput('OHCal_slope', value = -7.24e-8, min = 0, step = 1e-8, label = "Slope (-7.24e-8)"),
                 column(12, style = "border-top:1px solid"),
                 strong("Limits of Detection"),
                 numericInput('HONO_LOD', value = 15, min = 0.0, step = 1, label = "HONO LOD (ppt)"),
                 numericInput('OH_LOD', value = 2.36e5, min = 0.0, step = 1e5, label = "OH LOD (2.36 e5 molec/cm^3)"),
                 strong("Met Data Calibrations"),
                 numericInput('TempSlopeHONO', value = 105.83, min = 0.0, step = 0.01, label = "Temperature Calibration Slope"),
                 numericInput('TempIntHONO', value = -43.063, min = 0.0, step = 0.001, label = "Temperature Calibration Intercept"),
                 column(12, style = "border-top:1px solid"),
                 # strong("Non Linear Parameters"),
                 # numericInput('dry_sensitivity_HONOExtractor', value = 1.77e-8, min = 0, step = 0.1e-8, label = "Dry Sensitivity"),
                 # numericInput('water_dependence_HONOExtractor', value = 103.3, min = 1, step = 0.1, label = "Water Dependence"),
                 radioButtons("water_source_HONO", label = "Water Source",
                              choices = list("Box monitor with notes as RH" = 1, "Probe" = 2), 
                              selected = 2),
                 radioButtons("cycling_HONO", label = "Cycling Method",
                              choices = list("Cycles in LabView (not currently working)" = 1, "Calculate cycles" = 0), 
                              selected = 0),
                 radioButtons("WeightSignal_HONO", label = "Weighted Signal?",
                              choices = list("Weighted" = 1, "Not Weighted" = 0), 
                              selected = 1),
                 column(12, style = "border-top:1px solid"),
                 # numericInput('water_source_HONO', value = 2, min = 1, step = 1, label = "Water Source: enter 1 for box monitor or 2 for probe"),
                 # numericInput('calibrator_HONO', value = 2, min = 1, step = 1, label = "Calibrator used (1 or 2)"),
                 # numericInput('cycling_HONO', value = 1, min = 0, step = 1, label = "If program not outputting cycle numbers, change to 0"),
                 numericInput('LowerROH_HONO', value = 5e-10, min = 1e-15, step = 1e-10, label = "Lower ROH (5e-10)"), # Lower limit for ROH to remove bad points during calibration
                 numericInput('SNR_HONO', value = 2, min = 1, step = 1, label = "SNR"), # Signal to noise ratio
                 numericInput('RH_HONO', value = 0.01, min = 0.01, step = 0.01, label = "RH to test (1% default). Enter as decimal "),
                 numericInput('HONO_PE_HONO_Extractor', value = 0.41, min = 0.01, step = 0.01, label = "HONO PE"),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Adapted from VBA code by Emily Reidy"),
                 p("Last updated: September 13, 2022")
               ),
               mainPanel(
                 h4('About Me'),
                 column(12, style = "border-top:1px solid"),
                 p("Extractor for data from the HONO cell. Warning: Takes several minutes to process"),
                 p("Can only upload one txt file at a time. Combine txt files first in command prompt via cd to directory"),
                 p("copy *.txt NewFileName.txt"),
                 p("Deletes any row with 'delete' in Notes regardless of capitalization. Also deletes rows with 'calibration' in Notes regardless of capitalization"),
                 br(),
                 actionButton('HONOExtractorStart', 'Start HONO Extractor', class = "btn-primary"),
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4('Download HONO Extracted Data:'),
                 # textInput('filenameHONO', label = NULL, width = "70%", placeholder = '.csv'),
                 fluidRow(
                   column(8, textInput('filenameHONO', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadHONOExtractor', label = 'Download .csv File', class = "btn-primary"))
                 ),
                 h4('Download HONO Extractor Input Values:'),
                 fluidRow(
                   column(8, textInput('filenameHONOInput', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadHONOExtractorInput', label = 'Download .csv File', class = "btn-primary"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Raw Data", dataTableOutput("table1_HONOExtractor")),
                   tabPanel("OH Plot", plotlyOutput("plot1_HONOExtractor")),
                   tabPanel("HONO Plot", plotlyOutput("plot2_HONOExtractor")),
                   tabPanel("View Output Data File ", dataTableOutput("table2_HONOExtractor"))
                   # tabPanel("HONO Box Plot", plotlyOutput("plot3_HONOExtractor"))
                 )
               )
             )
    ),
    # OH Calibration UI -------------------------------------------------------
    tabPanel('OH Calibration',
             sidebarLayout(
               sidebarPanel(
                 fileInput('raw_data_file', label = 'Raw data file (txt):', multiple = FALSE, placeholder = "No file selected"),
                 strong("Laser Parameters"),
                 fluidRow(
                   column(7, numericInput('P308_start', value = 3.05, step = 0.01, label = '308 Laser Power (mW):')),
                   column(5, numericInput('UVout1_start', value = 0.017, step = 0.001, label = '308 UVout1 (V):'))
                 ),
                 column(12, style = "border-top:1px solid"),
                 numericInput('N_background', value = 15, min = 1, step = 1, label = "Time Resolution (min)"),
                 column(12, style = "border-top:1px solid"),
                 strong("Remove Lock Points"),
                 fluidRow(
                   column(7, numericInput('delete_before', value = 2, min = 1, step = 1, label = "Before")),
                   column(5, numericInput('delete_after', value = 2, min = 1, step = 1, label = "After"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 strong("Met Data Calibrations"),
                 numericInput('TempSlope', value = 105.83, min = 0.0, step = 0.01, label = "Temperature Calibration Slope"),
                 numericInput('TempInt', value = -43.063, min = 0.0, step = 0.001, label = "Temperature Calibration Intercept"),
                 # put text in h3("ezample") to make larger as header
                 column(12, style = "border-top:1px solid"),
                 radioButtons("water_source", label = "Water Source",
                              choices = list("Probe" = 2, "Box monitor with notes as RH" = 1), 
                              selected = 2),
                 radioButtons("calibrator", label = "Calibrator Used",
                              choices = list("Calibrator 1" = 1, "Calibrator 2" = 2), 
                              selected = 2),
                 radioButtons("cycling", label = "Cycling Method",
                              choices = list("Calculate cycles" = 0, "Cycles in LabView (not currently working)" = 1), 
                              selected = 0),
                 column(12, style = "border-top:1px solid"),
                 numericInput('HONO_PE', value = 0.41, min = 0.01, step = 0.01, label = "HONO PE (%)"),
                 numericInput('LowerROH', value = 3e-12, min = 1e-12, step = 1e-12, label = "Lower ROH (3e-12)"), # Lower limit for ROH to remove bad points during calibration
                 numericInput('SNR', value = 2, min = 1, step = 1, label = "SNR"), # Signal to noise ratio
                 numericInput('RH', value = 0.01, min = 0.01, step = 0.01, label = "RH to test (1% default). Enter as decimal "),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Adapted from VBA code by Emily Reidy"),
                 p("Last updated: September 13, 2022")
               ),
               mainPanel(
                 h4('About Me'),
                 column(12, style = "border-top:1px solid"),
                 p("OH Calibration for data from the HONO cell"),
                 p("Can only upload one txt file at a time. Combine txt files first in command prompt via"),
                 p("copy *.txt NewFileName.txt"),
                 p("For box monitor, enter 'delete' or 'Delete' at start of calibration and it will delete all points prior to first delete and rows with delete"),
                 p("For probe, enter 'delete' or 'Delete' during calibration and it will delete those points"),
                 p("Optional if change raw code: For probe, enter 'calibration' and it will subset file for only those. Can do calibration while running ambient because Extractor will delete rows with 'calibration'"),
                 br(),
                 actionButton('OHCalStart', 'Start OH Calibration', class = "btn-primary"),
                 # button colors: https://getbootstrap.com/docs/4.0/components/buttons/
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4('Download Total OH Calibration Data:'),
                 # textInput('filenameOH', label = NULL, width = "70%", placeholder = '.csv'),
                 fluidRow(
                   column(8, textInput('filenameOH', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadOHCal', label = 'Download .csv File', class = "btn-primary"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4('Download OH Calibration Inputs:'),
                 fluidRow(
                   column(8, textInput('filenameOHResults_Inputs', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadOHCalResults_Inputs', label = 'Download .csv File', class = "btn-primary"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Raw Data", dataTableOutput("table1")),
                   tabPanel("Results", dataTableOutput("table2")),
                   tabPanel("OH Calibration Plot", plotlyOutput("plot1")),
                   tabPanel("Output Data File", dataTableOutput("table3"))
                 )
               )
             )
    ),
    # Post Extraction Edits UI ------------------------------------------------
    tabPanel('Post Extraction Edits',
             sidebarLayout(
               sidebarPanel(
                 fileInput('raw_data_file_PostExtraction', label = 'Raw data file (csv):', multiple = FALSE, placeholder = "No file selected"),
                 strong("Averaging Parameters"),
                 numericInput('TimeAverage', value = 15, min = 1, step = 1, label = "Time resolution (min)"),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Last updated: September 13, 2022")
               ),
               mainPanel(
                 h4('About Me'),
                 column(12, style = "border-top:1px solid"),
                 p("Must run through HONO Extractor first. Warning: Can only do a few days at a time"),
                 p("Can only upload one csv file at a time. Combine csv files first in command prompt via"),
                 p("copy *.csv NewFileName.csv"),
                 br(),
                 actionButton('PostExtractionEditStart', 'Start Post Extraction Edits', class = "btn-primary"),
                 # button colors: https://getbootstrap.com/docs/4.0/components/buttons/
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4('Download Edited Data:'),
                 # textInput('filenameOH', label = NULL, width = "70%", placeholder = '.csv'),
                 fluidRow(
                   column(8, textInput('filenamePostExtraction', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadPostExtraction', label = 'Download .csv File', class = "btn-primary"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Raw Data", dataTableOutput("table_RawData_PostExtraction")),
                   tabPanel("HONO", plotlyOutput("plotPostExtraction_HONO")),
                   tabPanel("OH", plotlyOutput("plotPostExtraction_OH")),
                   tabPanel("View Output Data File", dataTableOutput("table_OutputData_PostExtraction"))
                 )
               )
             )
    ),
    # HONO Photolysis UI ------------------------------------------------
    tabPanel('HONO Photolysis',
             sidebarLayout(
               sidebarPanel(
                 fileInput('raw_data_file_HONOPE', label = 'Raw data file (txt):', multiple = FALSE, placeholder = "No file selected"),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Last updated: September 13, 2022")
               ),
               mainPanel(
                 h4('About Me'),
                 column(12, style = "border-top:1px solid"),
                 p("HONO Photolysis Efficiency Experiment"),
                 p("Can only upload one txt file at a time. Combine txt files first in command prompt via"),
                 p("copy *.txt NewFileName.txt"),
                 p("Record OH, ROH, HONO, and Tank cycles in Notes. Will delete all other rows"),
                 p("Maintain constant RH (1%)"),
                 br(),
                 actionButton('HONOPEStart', 'Start HONO Photolysis', class = "btn-primary"),
                 # button colors: https://getbootstrap.com/docs/4.0/components/buttons/
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Raw Data", dataTableOutput("table1_HONOPE")),
                   tabPanel("HONO", plotlyOutput("plot1_HONOPE")),
                   tabPanel("View Output Data File", dataTableOutput("table_OutputData_HONOPE"))
                 )
               )
             )
    ),
    # Data Logger UI ------------------------------------------------
    tabPanel('Data Logger',
             sidebarLayout(
               sidebarPanel(
                 fileInput('raw_data_file_DataLogger', label = 'Raw data file (csv):', multiple = FALSE, placeholder = "No file selected"),
                 strong("Averaging Parameters"),
                 numericInput('TimeAverage_DataLogger', value = 30, min = 1, step = 1, label = "Time resolution (min)"),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Last updated: September 13, 2022")
               ),
               mainPanel(
                 h4('About Me'),
                 column(12, style = "border-top:1px solid"),
                 p("Can only upload one csv file at a time. Combine csv files first in command prompt via"),
                 p("copy *.csv NewFileName.csv"),
                 p("Need to adjust the custom time in the time column to m/d/Y H:MM:SS before uploading"),
                 br(),
                 actionButton('DataLoggerStart', 'Start Data Logger', class = "btn-primary"),
                 # button colors: https://getbootstrap.com/docs/4.0/components/buttons/
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4('Download Data:'),
                 # textInput('filenameOH', label = NULL, width = "70%", placeholder = '.csv'),
                 fluidRow(
                   column(8, textInput('filenameDataLogger', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadDataLogger', label = 'Download .csv File', class = "btn-primary"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Raw Data", dataTableOutput("table_RawData_DataLogger")),
                   tabPanel("O3", plotlyOutput("plotDataLogger_O3")),
                   tabPanel("NOx", plotlyOutput("plotDataLogger_NOx")),
                   tabPanel("JNO2", plotlyOutput("plotDataLogger_JNO2")),
                   tabPanel("View Output Data File", dataTableOutput("table_OutputData_DataLogger"))
                 )
               )
             )
    ),
    # Merge UI ------------------------------------------------
    tabPanel('Merge',
             sidebarLayout(
               sidebarPanel(
                 # need to upload 2 separate files
                 fileInput('raw_data_file_MergeHONO', label = 'Averaged data logger file (csv):', multiple = FALSE, placeholder = "No file selected"),
                 fileInput('raw_data_file_MergeDataLogger', label = 'Post Extraction HONO data file (csv):', multiple = FALSE, placeholder = "No file selected"),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Last updated: September 13, 2022")
               ),
               mainPanel(
                 h4('About Me'),
                 column(12, style = "border-top:1px solid"),
                 p("Upload csv for HONO data after post extraction and csv for data logger after averaging"),
                 br(),
                 actionButton('MergeStart', 'Start Merge', class = "btn-primary"),
                 # button colors: https://getbootstrap.com/docs/4.0/components/buttons/
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4('Download Data:'),
                 fluidRow(
                   column(8, textInput('filenameMerge', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadMerge', label = 'Download .csv File', class = "btn-primary"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Data Logger Raw Data", dataTableOutput("table_RawData_HONOMerge")),
                   tabPanel("HONO Raw Data", dataTableOutput("table_RawData_MergeDataLogger")),
                   tabPanel("View Output Data File", dataTableOutput("table_Output_DataMerge"))
                 )
               )
             )
    ),
  )
)



# Start Server ------------------------------------------------------------
server <- function(input, output, session) {
  # Default Values ----------------------------------------------------------
  options(shiny.maxRequestSize = 250 * 1024 ^ 2) # 250 MB max file size upload
  # bs_themer()
  # calibrator_HONO Constants @ 10 LPM, needs periodically updated
  # Global variables
  Cal1_ozone <- 131.49
  Cal1_O2_cross_section <- 1.12e-20 
  Cal1_ROH <- 0.483 # Wall loss
  Cal2_ozone <- 389.23
  Cal2_O2_cross_section <- 7.805e-21
  Cal2_ROH <- 0.601 # Wall loss 60% make it through calibrator
  # Get OH Cal Input --------------------------------------------------------
  data_retrieve <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file, {
    raw_data_file <- input$raw_data_file
    df <- read.table(raw_data_file$datapath, header = FALSE, sep = " ")
    setDT(df) # converts list to data table
    colnames(df) <- c("Time", "Meridiem", "Date", "UVout1", "Pressure", "MFC1", "UVout2", "UVout3", "Signal", "Online", "Lock", "Shutter", "LS", "PD", "C3F6", "RH", "NC", "Temp", "Notes", "Cycle", "MeasCycle", "NC", "Ambient")
    data_retrieve$data <- df
    # format options: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime
    data_retrieve$data$Time <- str_c(data_retrieve$data$Date, " ", data_retrieve$data$Time, " ", data_retrieve$data$Meridiem)
    data_retrieve$data$Time <- mdy_hms(data_retrieve$data$Time)
    data_retrieve$data$Date <- as.Date(data_retrieve$data$Date, format = "%m/%d/%Y")
    data_retrieve$data <- data_retrieve$data[, -c("NC", "Meridiem")]
    data_retrieve$data <- filter(data_retrieve$data, data_retrieve$data$UVout1 > 0 & data_retrieve$data$Pressure > 1.0 & data_retrieve$data$Pressure < 2.0 & data_retrieve$data$UVout3 < 1.0)
    # can't remove Notes yet because they are used for RH in water source sometimes
  })
  # Get HONO Extractor Input ------------------------------------------------
  data_retrieve_HONO <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file_HONO, {
    # July 25-July 27 don't have ambient column
    raw_data_file_HONO <- input$raw_data_file_HONO
    df_HONO <- read.table(raw_data_file_HONO$datapath, header = FALSE, sep = " ")
    setDT(df_HONO) # converts list to data table
    colnames(df_HONO) <- c("Time", "Meridiem", "Date", "UVout1", "Pressure", "MFC1", "UVout2", "UVout3", "Signal", "Online", "Lock", "Shutter", "LS", "PD", "C3F6", "RH", "NC", "Temp", "Notes", "Cycle", "MeasCycle", "NC", "Ambient")
    data_retrieve_HONO$data <- df_HONO
    # format options: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime
    data_retrieve_HONO$data$Time <- str_c(data_retrieve_HONO$data$Date, " ", data_retrieve_HONO$data$Time, " ", data_retrieve_HONO$data$Meridiem)
    data_retrieve_HONO$data$Time <- mdy_hms(data_retrieve_HONO$data$Time)
    data_retrieve_HONO$data$Date <- as.Date(data_retrieve_HONO$data$Date, format = "%m/%d/%Y")
    data_retrieve_HONO$data <- data_retrieve_HONO$data[, -c("NC", "Meridiem", "MFC1", "UVout2", "C3F6", "Ambient")] # delete columns not needed 
    data_retrieve_HONO$data <- filter(data_retrieve_HONO$data, data_retrieve_HONO$data$UVout1 > 0 & data_retrieve_HONO$data$Pressure > 1.0 & data_retrieve_HONO$data$Pressure < 2.0 & data_retrieve_HONO$data$UVout3 < 10.0)
    # can't remove Notes yet because they are used for RH in water source sometimes
  })
  
  # Get Post Extraction Input ------------------------------------------------
  data_retrieve_PostExtraction <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file_PostExtraction, {
    raw_data_file_PostExtraction <- input$raw_data_file_PostExtraction
    df_HONO_PostExtraction <- read.table(raw_data_file_PostExtraction$datapath, header = TRUE, sep = ",")
    # df_HONO_PostExtraction <- df_HONO_PostExtraction[,!(names(df_HONO_PostExtraction) %in% c("TimeX", "dttm"))]
    setDT(df_HONO_PostExtraction) # converts list to data table
    data_retrieve_PostExtraction$data <- df_HONO_PostExtraction
    # Time works for plotting but not math because it isn't numeric. Need to convert to numeric then convert back for plotting
    # data_retrieve_PostExtraction$data$Time <- mdy_hms(data_retrieve_PostExtraction$data$Time)
    # data_retrieve_PostExtraction$data$Time <- mdy_hms(data_retrieve_PostExtraction$data$Time)
    # print(class(data_retrieve_PostExtraction$data$PostExtraction$Time))
    # data_retrieve_PostExtraction$data$Time <- as.POSIXlt(data_retrieve_PostExtraction$data$Time, format = "%Y-%m-%d %H:%M:%OS")
    
    # data_retrieve_PostExtraction$data <- data_retrieve_PostExtraction$data[,-c("Time")]
    # creates annoying time format that doesn't work for math/plotting but works for subsetting time resolution for averaging
  })
  
  
  # Get HONO PE Input --------------------------------------------------------
  data_retrieve_HONOPE <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file_HONOPE, {
    raw_data_file_HONOPE <- input$raw_data_file_HONOPE
    df_HONOPE <- read.table(raw_data_file_HONOPE$datapath, header = FALSE, sep = " ")
    setDT(df_HONOPE) # converts list to data table
    colnames(df_HONOPE) <- c("Time", "Meridiem", "Date", "UVout1", "Pressure", "MFC1", "UVout2", "UVout3", "Signal", "Online", "Lock", "Shutter", "LS", "PD", "C3F6", "RH", "NC", "Temp", "Notes", "Cycle", "MeasCycle", "NC", "Ambient")
    data_retrieve_HONOPE$data <- df_HONOPE
    # format options: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime
    data_retrieve_HONOPE$data$Time <- str_c(data_retrieve_HONOPE$data$Date, " ", data_retrieve_HONOPE$data$Time, " ", data_retrieve_HONOPE$data$Meridiem)
    data_retrieve_HONOPE$data$Time <- mdy_hms(data_retrieve_HONOPE$data$Time)
    data_retrieve_HONOPE$data$Date <- as.Date(data_retrieve_HONOPE$data$Date, format = "%m/%d/%Y")
    data_retrieve_HONOPE$data <- data_retrieve_HONOPE$data[, -c("NC", "Meridiem")]
    # UVout3 can be larger because 355 on, change filter conditions
    # data_retrieve_HONOPE$data <- filter(data_retrieve_HONOPE$data, data_retrieve_HONOPE$data$UVout1 > 0 & 
    #                                       data_retrieve_HONOPE$data$Pressure > 1.0 & 
    #                                       data_retrieve_HONOPE$data$Pressure < 2.0 & 
    #                                       data_retrieve_HONOPE$data$UVout3 < 1.0)
    data_retrieve_HONOPE$data <- filter(data_retrieve_HONOPE$data, data_retrieve_HONOPE$data$Notes == "OH" |
                                          data_retrieve_HONOPE$data$Notes == "ROH" |
                                          data_retrieve_HONOPE$data$Notes == "HONO" |
                                          data_retrieve_HONOPE$data$Notes == "Tank")
    
  })
  # Get Data Logger Input ------------------------------------------------
  data_retrieve_DataLogger <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file_DataLogger, {
    raw_data_file_DataLogger <- input$raw_data_file_DataLogger
    df_HONO_DataLogger <- read.table(raw_data_file_DataLogger$datapath, header = TRUE, sep = ",")
    # df_HONO_PostExtraction <- df_HONO_PostExtraction[,!(names(df_HONO_PostExtraction) %in% c("TimeX", "dttm"))]
    setDT(df_HONO_DataLogger) # converts list to data table
    data_retrieve_DataLogger$data <- df_HONO_DataLogger
    # Time works for plotting but not math because it isn't numeric. Need to convert to numeric then convert back for plotting
    # data_retrieve_DataLogger$data$Time <- mdy_hms(data_retrieve_DataLogger$data$Time)
    # data_retrieve_DataLogger$data$Time <- mdy_hms(data_retrieve_DataLogger$data$Time)
    # print(class(data_retrieve_DataLogger$data$DataLogger$Time))
    # data_retrieve_DataLogger$data$Time <- as.POSIXlt(data_retrieve_DataLogger$data$Time, format = "%Y-%m-%d %H:%M:%OS")
    # data_retrieve_DataLogger$data <- data_retrieve_DataLogger$data[,-c("Notes", "RH.1", "Temp.1", "RH.2", "Temp.2", "Water")]
    # creates annoying time format that doesn't work for math/plotting but works for subsetting time resolution for averaging
  })
  # Get Merge Input ------------------------------------------------
  data_retrieve_MergeHONO <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file_MergeHONO, {
    raw_data_file_MergeHONO <- input$raw_data_file_MergeHONO
    df_HONO_Merge1 <- read.table(raw_data_file_MergeHONO$datapath, header = TRUE, sep = ",")
    setDT(df_HONO_Merge1) # converts list to data table
    data_retrieve_MergeHONO$data <- df_HONO_Merge1
  })
  data_retrieve_MergeDataLogger <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file_MergeDataLogger, {
    raw_data_file_MergeDataLogger <- input$raw_data_file_MergeDataLogger
    df_DataLogger_Merge1 <- read.table(raw_data_file_MergeDataLogger$datapath, header = TRUE, sep = ",")
    setDT(df_DataLogger_Merge1) # converts list to data table
    data_retrieve_MergeDataLogger$data <- df_DataLogger_Merge1

  })
  # OH Cal Start ------------------------------------------------------------
  observeEvent(input$OHCalStart, {
    # 355 Laser should be blocked whole time so don't need to worry about UVout1 correction
    req(input$raw_data_file)
    if(input$calibrator == 1){
      O3 <- Cal1_ozone
      O2_cross_section <- Cal1_O2_cross_section
      WallLoss <- Cal1_ROH
      HONO_PE <- input$HONO_PE
    } else{
      O3 <- Cal2_ozone
      O2_cross_section <- Cal2_O2_cross_section
      WallLoss <- Cal2_ROH
      HONO_PE <- input$HONO_PE
    }
    
    # WATER SOURCE
    # If using box monitor (1), RH and Temp must be calculated from box values placed into notes 
    # Must remove rows without notes due to incorrect RH during transitions
    # Delete points where notes say delete if using probe. Must keep when no notes
    if( input$water_source == 2){
      # data_retrieve$data <- filter(data_retrieve$data, data_retrieve$data$Notes != "delete" & data_retrieve$data$Notes != "Delete")
      # If using PROBE, only keep when notes has calibration
      # data_retrieve$data <- grepl("calibration", data_retrieve$data$Notes, ignore.case = T)
      data_retrieve$data <- data_retrieve$data[!grepl("delete", data_retrieve$data$Notes, ignore.case = T),] # deletes rows with saying delete or Delete
      data_retrieve$data$RH <- data_retrieve$data$RH * 100
      data_retrieve$data$Temp <- data_retrieve$data$Temp * input$TempSlope + input$TempInt
    }
    # For box monitor,
    if(input$water_source == 1){
      # removes row if there are no notes
      pos_del <- which(data_retrieve$data$Notes == "delete" | data_retrieve$data$Notes == "Delete") # find position of delete and Delete
      len_pos_del <- length(pos_del) # might need to replace length with nrow
      data_retrieve$data <- data_retrieve$data[-(1:len_pos_del[1]), ] # delete rows  before delete or Delete in notes
      data_retrieve$data <- data_retrieve$data[!(data_retrieve$data$Notes == "" | is.na(data_retrieve$data$Notes)), ] # deletes rows with no notes or NA
      # data_retrieve$data <- filter(data_retrieve$data, data_retrieve$data != "delete" & data_retrieve$data != "Delete")
      data_retrieve$data <- data_retrieve$data[!grepl("delete", data_retrieve$data$Notes, ignore.case = T),] # deletes rows with saying delete or Delete
      data_retrieve$data$Temp <- (data_retrieve$data$Notes + 40) * 5 - 40
      data_retrieve$data$RH <- data_retrieve$data$Notes * 5
    } # if select 2, then nothing needs to change. Use direct probe output without corrections
    data_retrieve$data <- data_retrieve$data[,-c("Notes")] # delete notes. They have been used by now to calculate RH and Temp
    
    exp <- 10**((7.591386 * data_retrieve$data$Temp) / (240.7263 + data_retrieve$data$Temp)) # separated to make equation easier to read
    data_retrieve$data$Water <- 6.117823 * (data_retrieve$data$RH / 100) * exp / (1013.25 - (6.116441 * exp * (data_retrieve$data$RH / 100)))
    
    # Change all classes from generic "factor" to numeric
    for (i in colnames(data_retrieve$data)){
      if (class(data_retrieve$data[[i]])[1] == "factor"){
        data_retrieve$data[[i]] = as.numeric(as.character(data_retrieve$data[[i]]))
      } else if (class(data_retrieve$data[[i]])[1] == "logical"){
        data_retrieve$data[[i]] = as.numeric(data_retrieve$data[[i]])
      }
    }
    
    # Only if LabView not outputting cycling. This will calculate cycle numbers
    # Cycle column indicates one online+offline cycle, could speed extractor if put in program output
    if (input$cycling == 0){
      #Create new column of zeros
      data_retrieve$data$Cycle <- 0
      #creates new column that's the difference between the Online value for each row and the previous
      data_retrieve$data$newcol <- data_retrieve$data$Online - lag(data_retrieve$data$Online)
      # Finds rows that go from online to offline
      # which returns position of element meeting criteria
      test <- which(data_retrieve$data$newcol == 1)
      # Change data in Cycle column (takes a long time)
      for (i in 2:length(test)){
        data_retrieve$data$Cycle[test[i-1]:test[i]-1] <- i - 1
      }
    }
    
    ### LOCKS & DELETE ###
    # Add locks (at least 2 before lock indicators and 1 after) to remove all points where laser is moving
    # Find rows that currently have lock indicator
    test <- which(data_retrieve$data$Lock == 2) # returns positions when lock is on
    # Define initial variable
    test2 <- test[1] # starting with first lock
    for (i in 1:length(test)){
      test2 <- append(test2, (test[i] - input$delete_before):(test[i] + input$delete_after))
    } # adds points previous to, equal to, and after lock to test2
    test2 <- unique(test2) # removes duplicates
    # for (i in 1:length(data_retrieve$data$UVout1)){
    #   if (data_retrieve$data$Lock[i] == 2){
    #     data_retrieve$data$Lock[i] <- 1
    #   }
    # }
    # data_retrieve$data$Lock[test2] <- 1 # when lock is online, changes to 1
    rawdata <- data_retrieve$data[data_retrieve$data$Lock == 0, -c("Lock")] # data frame without lock column when lock off
    rawdata$Cycle <- as.numeric(as.character(rawdata$Cycle))
    rm(test)
    rm(test2)
    
    # THIS WAS A TEST TO SEE EFFECT OF BACKGROUND. NOT USED IN REAL ANALYSIS
    for (i in 1:length(rawdata$Signal)){
      if (rawdata$Online[i] == 0 & rawdata$Signal[i] > 2){
        rawdata$Signal[i] <- 0
      }
    }
    
    ### ARCHAIC TESTS ###
    # Laser Scatter (LS) Test
    if (sum(rawdata$LS) > 0){
      LS <- rawdata[rawdata$LS != 0, ]
      rawdata <- rawdata[rawdata$LS == 0, ] # removes points where LS is not 0
    } 
    # Photo-diode (PD) Test
    if (sum(rawdata$PD) > 0){
      PD <- rawdata[rawdata$PD != 0, ]
      rawdata <- rawdata[rawdata$PD == 0, ] # removes points where PD is not 0
    }
    rawdata <- rawdata[,-c("LS", "PD")] # remove LS and PD columns
    
    # Remove first point of cycle where shutter hasn't opened yet
    # Get averages for each cycle for the variables that don't depend on cycling
    # Aggregate returns a dataframe, applies function to dataframe by grouping via list
    rawavg <- aggregate(rawdata, list(rawdata$Cycle), mean)
    rawsd <- aggregate(rawdata, list(rawdata$Cycle), sd)
    
    rawsd$Cycle <- rawavg$Cycle
    rawsd <- rawsd[ ,!(names(rawsd) %in% c("Group.1"))]
    
    # Separate and average online and offline cycles
    onl <- rawdata[rawdata$Online == 1, -c("Online")]
    off <- rawdata[rawdata$Online == 0, -c("Online")]
    
    # Averages based on Cycle number
    onlavg <- aggregate(onl, list(onl$Cycle), mean)
    offavg <- aggregate(off, list(off$Cycle), mean)
    # Counts within each cycle
    onlct <- aggregate(onl[,1], list(onl$Cycle), length)
    offct <- aggregate(off[,1], list(off$Cycle), length)
    # sd within each cycle
    onlsd <- aggregate(onl, list(onl$Cycle), sd)
    offsd <- aggregate(off, list(off$Cycle), sd)
    # Append counts and signal sd to rest of data
    onlavg$count <- onlct[[2]]
    offavg$count <- offct[[2]]
    onlavg$sigsd <- onlsd$Signal
    offavg$sigsd <- offsd$Signal
    
    onlavg <- onlavg[ , !(names(onlavg) %in% c("Group.1"))]
    offavg <- offavg[ , !(names(offavg) %in% c("Group.1"))]
    
    # If data set starts on an offline, removes the first offline to match online
    # if either on or offline has an extra cycle at the beginning, then the rawdata
    # will also have an extra
    
    while(!offavg$Cycle[1] == onlavg$Cycle[1]){
      if(offavg$Cycle[1] < onlavg$Cycle[1]){
        offavg <- offavg[-1, ]
        rawavg <- rawavg[-1, ]
        rawsd <- rawsd[-1, ]
      } else if (offavg$Cycle[1] > onlavg$Cycle[1]){
        onlavg <- onlavg[-1, ]
        rawavg <- rawavg[-1, ]
        rawsd <- rawsd[-1, ]
      }
      print("In while loop, stop code if you see this too many times")
    }
    if (!(length(offavg$UVout1) == length(onlavg$UVout1))){
      if(length(offavg$UVout1) > length(onlavg$UVout1)){
        notinboth <- setdiff(offavg$Cycle, onlavg$Cycle)
        offavg <- offavg[!(offavg$Cycle %in% notinboth), ]
        rawavg <- rawavg[!(rawavg$Cycle %in% notinboth), ]
        rawsd <- rawsd[!(rawsd$Cycle %in% notinboth), ]
      } else {
        notinboth <- setdiff(onlavg$Cycle, offavg$Cycle)
        onlavg <- onlavg[!(onlavg$Cycle %in% notinboth), ]
        rawavg <- rawavg[!(rawavg$Cycle %in% notinboth), ]
        rawsd <- rawsd[!(rawsd$Cycle %in% notinboth), ]
      }
    }
    ### CYCLE AVERAGES ###
    # Get cycle averages
    col_names <- colnames(onlavg)
    cycles <- data.frame(matrix(ncol = length(col_names), nrow = length(onlavg$UVout1)))
    names(cycles) <- col_names
    for (i in col_names){
      if (i == "UVout1"){
        cycles[[i]] = (onlavg[[i]] * onlavg$count + offavg[[i]] * offavg$count) / (onlavg$count + offavg$count)
      } else if(i == "Signal") {
        cycles[[i]] <- ((onlavg[[i]] / onlavg$UVout1) - (offavg[[i]] / offavg$UVout1)) * ((onlavg$count*onlavg$UVout1) + (offavg$count * offavg$UVout1)) / (onlavg$count + offavg$count)
      } else if(i == "sigsd"){
        cycles[[i]] <- onlavg[[i]]
      } else {
        cycles[[i]] <- rawavg[[i]]
      } 
    }
    
    cycles$WeightedNetSignal <- cycles$Signal
    
    setDT(cycles)
    setkey(cycles, "Cycle")
    
    # Default Dry Sensitivity and Water Dependence
    # need to vary these values to minimize sum of square of differences
    # dry_sensitivity <- 1.77e-8
    # water_dependence <- 103.3 
    
    ##################▲
    # THIS MIGHT NEED TO CHANGE
    # Calculations for Sensitivities and LODs
    # Convert to absolute humidity
    if(input$calibrator == 1){
      cycles$Water <- cycles$Water
    } else{
      cycles$Water <- cycles$Water * 100
    }
    
    water_background <- 0 # always 0 in Excel extractor
    UVout2_background <- 0 # always 0 in Excel extractor
    setkey(cycles, Time)
    cycles$P308 <- cycles$UVout1 * (input$P308_start / input$UVout1_start)
    # cycles <- cycles[ , -c("newcol")] # not found. When is it created and deleted?
    #cycles$EstROH <- dry_sensitivity / (1 + (water_dependence * (cycles$Water)))
    cycles$PE <- HONO_PE
    cycles$NetWater <- cycles$Water - water_background  
    cycles$NetUVout2 <- cycles$UVout2 - UVout2_background
    cycles$O3 <- cycles$NetUVout2 * O3
    cycles$NetOH <- cycles$O3 * 1e-9 * cycles$NetWater * 2.4e19 * 7.1e-21 / 2 / 0.2 / O2_cross_section
    # check if these are 1e-10 and 7.1e-22 instead
    cycles$ROH <- cycles$WeightedNetSignal / cycles$NetOH / cycles$P308 / WallLoss 
    cycles$ROH_scaled <- cycles$ROH * 1e8
    
    # Delete row if ROH below lower ROH which indicates error occurred
    cycles <- subset(cycles, cycles$ROH > input$LowerROH)
    
    # Linear Fit
    fit <- lm(formula = cycles$ROH ~ cycles$NetWater)
    cf <- coef(fit)
    OH_sensitivity_linear <- cf["(Intercept)"]
    slope_linear <- cf["cycles$NetWater"]
    
    # Non Linear Fit 
    EstROH_Fit <- function(x,a,b){
      a / (1 + b * x)
    } # x is net water, a is dry sensitivity, b is water dependence
    #y <- cycles$ROH
    y <- cycles$EstROH
    x <- cycles$NetWater
    # CANNOT use nls function. Will get "singular gradient" error
    EstROH_nlsLM_fit <- nlsLM(x ~ EstROH_Fit(x,a,b), start = list(a = 1.77e-9, b = 103.3))
    coeff_test <- predict(EstROH_nlsLM_fit, newdata = c(1.77e-9, 103.3))
    dry_sensitivity <- 3 * coeff_test[1]
    water_dependence <- 3 * coeff_test[2]
    # Fit_coeff <- coef(EstROH_nlsLM_fit)
    # dry_sensitivity <- coef(EstROH_nlsLM_fit)[["a"]] 
    # water_dependence <- coef(EstROH_nlsLM_fit)[["b"]] 
    
    # Need to apply this correction to change EstROH values
    # Apply these parameters to future ROH values as correction factor
    # cycles$EstROH <- EstROH_Fit(cycles$NetWater, dry_sensitivity, water_dependence) * 10 ^ 3
    cycles$EstROH <- predict(EstROH_nlsLM_fit, newdata = cycles$EstROH) * 100
    
    fit2 <- lm(formula = cycles$EstROH ~ cycles$NetWater)
    cf2 <- coef(fit2)
    OH_sensitivity_SV <- 3 * cf2["(Intercept)"]
    slope_SV <- 3 * cf2["cycles$NetWater"]
    
    # Background
    # DEPENDENT ON LOWER RH YOU CAN ACHIEVE. FOR PROPHET 2022, ZERO RH ON MONITOR WAS ~6 WHICH IS AROUND 35%
    background <- subset(off, off$RH < 40) # reassigning off cycles to background data frame. Only want offline when RH = 0 and laser off. RH = 0 impossible so filter with less than 10
    N_background <- input$N_background * 60 # point per second
    N_backgroundOH <- N_background / 4 # 1/4 as many points
    sd_background <- sd(background$Signal)
    avg_background <- mean(background$Signal)
    P308_avg <- mean(cycles$P308)
    
    # 20 as room Temp
    exp <- 10 ** ((7.591386 * 20) / (240.7263 + 20)) # separated to make equation easier to read
    TestRH <- 6.117823 * (input$RH) * exp / (1013.25 - (6.116441 * exp * (input$RH)))
    # ROH_1perc_linear <- slope_linear * input$RH + OH_sensitivity_linear
    ROH_1perc_linear <- slope_linear * TestRH + OH_sensitivity_linear
    # Is this actually RH input or should it be converted to absolute humidity
    # ROH_1perc_SV <- dry_sensitivity / (1 + water_dependence * input$RH)
    # ROH_1perc_SV <- dry_sensitivity / (1 + water_dependence * TestRH)
    ROH_1perc_SV <- slope_SV * TestRH + OH_sensitivity_SV
    # ROH_1perc_SV <- slope_SV * input$RH + OH_sensitivity_SV
    
    # For visual output
    # Calculate OH LOD (molec/cm^3)
    OH_LOD_linear <- (input$SNR * sd_background) / (sqrt(N_backgroundOH) * ROH_1perc_linear * P308_avg)
    OH_LOD_SV <- (input$SNR * sd_background) / (sqrt(N_backgroundOH) * ROH_1perc_SV * P308_avg)
    HONO_LOD_linear <- (input$SNR * sd_background) / (sqrt(N_background) * ROH_1perc_linear * P308_avg) / (HONO_PE * 2.4606e10) * 10^6 # convert molec/cm3 to ppt
    HONO_LOD_SV <- (input$SNR * sd_background) / (sqrt(N_background) * ROH_1perc_SV * P308_avg) / HONO_PE / 2.4606e10 * 10^6 # convert molec/cm3 to ppt
    
    # Calculating time resolution needed to achieve HONO LOD low enough, back calculation
    HONO_LOD_calc <- 1 # ppt
    OH_LOD_calc <- (HONO_LOD_calc / 1e6) * (HONO_PE * 2.4606e10)
    N_back_calc <- (((input$SNR * sd_background) / (OH_LOD_calc * ROH_1perc_SV * P308_avg)) ^ 2)  / 60 # minutes
    N_back_calc_hour <- N_back_calc / 60 # hours
    print(N_back_calc)
    print(N_back_calc_hour)
    Variable <- c("OH Sensitivity y int", "OH Sensitivity Slope", "OH LOD", "HONO LOD", "sd Background", "Average Background")
    Value <- c(OH_sensitivity_SV, slope_SV, OH_LOD_SV, HONO_LOD_SV, sd_background, avg_background)
    Units <- c("NA", "NA","molec/cm3", "ppt", "counts", "counts")
    # Variable <- c("OH Sensitivity y int", "OH Sensitivity Slope", "OH Sensitivity y int", "OH Sensitivity Slope", "OH LOD", "OH LOD", "HONO LOD", "HONO LOD", "sd Background", "Average Background")
    # Value <- c(OH_sensitivity_linear, slope_linear, OH_sensitivity_SV, slope_SV, OH_LOD_linear, OH_LOD_SV, HONO_LOD_linear, HONO_LOD_SV, sd_background, avg_background)
    # Units <- c("NA", "NA", "NA", "NA", "molec/cm3", "molec/cm3", "ppb", "ppb", "counts", "counts")
    # Type <- c("linear", "linear", "SV", "SV", "linear", "SV", "linear", "SV",  "NA", "NA")
    outputDataVisual <- data.frame(Variable, Value, Units)
    outputDataVisual$Value <- formatC(outputDataVisual$Value, format = "e", digits = 4)
    # updateTextInput(session, "filenameOH", value = paste(format(cycles$Date[1], "%Y%m%d"), "OH Data.csv", sep = ""))
    
    # Output input and result values as CSV
    outputDataVisual_Results_Inputs <- data.frame(matrix(ncol = 14), nrow = 2)
    outputDataVisual_Results_Inputs$OHSensyint <- OH_sensitivity_SV
    outputDataVisual_Results_Inputs$OHSensSlope <- slope_SV
    outputDataVisual_Results_Inputs$OHLOD <- OH_LOD_SV
    outputDataVisual_Results_Inputs$HONOLOD <- HONO_LOD_SV
    outputDataVisual_Results_Inputs$sdBackground <- sd_background
    outputDataVisual_Results_Inputs$avgBackground <- avg_background
    outputDataVisual_Results_Inputs$P308 <- input$P308_start
    outputDataVisual_Results_Inputs$UVout1 <- input$UVout1_start
    outputDataVisual_Results_Inputs$TempSlope <- input$TempSlope
    outputDataVisual_Results_Inputs$TempInt <- input$TempInt
    outputDataVisual_Results_Inputs$BackgroundTime <- input$N_background
    outputDataVisual_Results_Inputs$DeleteBefore <- input$delete_before
    outputDataVisual_Results_Inputs$DeleteAfter <- input$delete_after
    outputDataVisual_Results_Inputs$InitialFile <- input$raw_data_file
    
    updateTextInput(session, "filenameOH", value = "OH Data.csv")
    updateTextInput(session, "filenameOHResults_Inputs", value = "OH Cal Results and Inputs.csv")
    
    # lm_eqn <- function(df){
    #   m <- lm(cycles$EstROH ~ cycles$NetWater, cycles);
    #   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
    #                    list(a = format(unname(coef(m)[1]), digits = 2),
    #                         b = format(unname(coef(m)[2]), digits = 2),
    #                         r2 = format(summary(m)$r.squared, digits = 3)))
    #   as.character(as.expression(eq));
    # }
    
    #Display OH cal: linear and SV corrected fits
    output$plot1 <- renderPlotly({
      req(input$raw_data_file)
      ggplot(cycles, aes(x = NetWater, y = EstROH)) +
        geom_smooth(method='lm', formula= y ~ x) +
        geom_point(size=2, shape=16) +
        theme_bw() +
        # geom_text(label = lm_eqn(cycles), parse = TRUE) +
        xlab("Net Water") +
        ylab("ROH")
      #ggtitle("OH Calibration") +
    })
    # output$plot2 <- renderPlotly({
    #   req(input$raw_data_file)
    #   ggplot(cycles, aes(x = NetWater, y = ROH)) +
    #     geom_smooth(method='lm', formula= y ~ x) +
    #     geom_point(size=2, shape=16) +
    #     theme_bw() +
    #     xlab("Net Water") +
    #     ylab("ROH")
    #   #ggtitle("OH Calibration") +
    # })
    # Display output data
    output$table2 <- renderDataTable({
      req(input$raw_data_file)
      outputDataVisual
    })
    output$table3 <- renderDataTable({
      req(input$raw_data_file)
      cycles
    })
    
    output$downloadOHCal <- downloadHandler(
      filename = function() {
        paste("OH Cal ", Sys.Date(), ".csv", sep="") # can replace Sys.Date() with input$filenameOH
      },
      content = function(file) {
        write.csv(cycles, file)
      })
    output$downloadOHCalResults_Inputs <- downloadHandler(
      filename = function() {
        paste("OH Cal Inputs ", Sys.Date(), ".csv", sep="") # can replace Sys.Date() with input$filenameOH
      },
      content = function(file) {
        write.csv(outputDataVisual_Results_Inputs, file)
      })
    
  })
  
  # HONO Extractor Start ----------------------------------------------------
  observeEvent(input$HONOExtractorStart, {
    req(input$raw_data_file_HONO)
    # Calibration will be removed if conducted in same file and user remembers to enter calibration as note. Only works for probe since box monitor needs notes as RH
    # Calibration using box monitor will make it past this filter
    data_retrieve_HONO$data <- data_retrieve_HONO$data[!grepl("calibration", data_retrieve_HONO$data$Notes, ignore.case = T),] # deletes rows with saying delete or Delete
    # WATER SOURCE
    # If using box monitor (1), RH and Temp must be calculated from box values. 
    # Must remove rows without notes due to incorrect RH during transitions
    if (input$water_source_HONO == 2){
      # grepl identifies first string argument, ignoring capitalization when ignore.case=T
      data_retrieve_HONO$data <- data_retrieve_HONO$data[!grepl("delete", data_retrieve_HONO$data$Notes, ignore.case = T),] # deletes rows with saying delete or Delete
      data_retrieve_HONO$data$RH <- data_retrieve_HONO$data$RH * 100
      data_retrieve_HONO$data$Temp <- data_retrieve_HONO$data$Temp * input$TempSlopeHONO + input$TempIntHONO
    }
    if (input$water_source_HONO == 1){
      # removes row if there are no notes
      pos_del_HONOExtractor <- which(data_retrieve_HONO$data$Notes == "delete" | data_retrieve_HONO$data$Notes == "Delete")
      len_pos_del_HONOExtractor <- length(pos_del_HONOExtractor)
      data_retrieve_HONO$data <- data_retrieve_HONO$data[-(1:len_pos_del_HONOExtractor), ] # delete rows equal to and before delete or Delete in notes
      data_retrieve_HONO$data <- data_retrieve_HONO$data[!(data_retrieve_HONO$data$Notes == "" | is.na(data_retrieve_HONO$data$Notes)), ] # deletes rows with no notes or NA
      data_retrieve_HONO$data$Temp <- (data_retrieve_HONO$data$Notes + 40) * 5 - 40
      data_retrieve_HONO$data$RH <- data_retrieve_HONO$data$Notes * 5
    } # if select 2, then nothing needs to change. Use direct probe output without corrections
    exp_HONO <- 10 ** ((7.591386 * data_retrieve_HONO$data$Temp) / (240.7263 + data_retrieve_HONO$data$Temp)) # separated to make equation easier to read
    data_retrieve_HONO$data$Water <- 6.117823 * (data_retrieve_HONO$data$RH / 100) * exp_HONO / (1013.25 - (6.116441 * exp_HONO * (data_retrieve_HONO$data$RH/100)))
    ### NOTES ###
    # Separate out notes, as the rest of the data is numeric. Still need to find a way to incorporate notes back into final data files
    notes <- data_retrieve_HONO$data$Notes # store notes for later
    data_retrieve_HONO$data <- data_retrieve_HONO$data[,-c("Notes")] # delete notes. They have been used by now to calculate RH and Temp
    # Change all classes from generic "factor" to numeric
    for (i in colnames(data_retrieve_HONO$data)){
      if (class(data_retrieve_HONO$data[[i]])[1] == "factor"){
        data_retrieve_HONO$data[[i]] <- as.numeric(as.character(data_retrieve_HONO$data[[i]]))
      } else if (class(data_retrieve_HONO$data[[i]])[1] == "logical"){
        data_retrieve_HONO$data[[i]] <- as.numeric(data_retrieve_HONO$data[[i]])
      } else if (class(data_retrieve_HONO$data[[i]])[1] == "integer"){
        data_retrieve_HONO$data[[i]] <- as.numeric(data_retrieve_HONO$data[[i]])
      } 
    }
    
    # Cycle column indicates one online + offline cycle, could speed extractor if put in program output
    # if LabView not outputting cycling, this will calculate the cycle number 
    if (input$cycling_HONO == 0){
      data_retrieve_HONO$data$Cycle <- 0
      # data_retrieve_HONO$data <- data_retrieve_HONO$data[,-c("Cycle", "MeasCycle")] #speed up program by deleting unnecessary columns
      #creates new column that's the difference between the Online value for each row and the previous
      # mutate creates new variable newcol, lag finds previous value, %>% translates to "and then" meaning it passes value onto next thing
      data_retrieve_HONO$data$newcol <- data_retrieve_HONO$data$Online - lag(data_retrieve_HONO$data$Online)
      # Finds rows that go from online to offline
      # which returns position of element meeting criteria
      test_HONOExtractor <- which(data_retrieve_HONO$data$newcol == 1)
      # Change data in Cycle column (takes a long time)
      for (i in 2:length(test_HONOExtractor)){
        data_retrieve_HONO$data$Cycle[test_HONOExtractor[i-1]:test_HONOExtractor[i]-1] <- i - 1
      }
    } # if cycling_HONO == 1 then using LabVIEW
    
    ### LOCKS & DELETE ###
    # Add locks (at least 2 before lock indicators and 1 after) to remove all points where laser is moving
    # Find rows that currently have lock indicator
    test_HONOExtractor <- which(data_retrieve_HONO$data$Lock == 2) # returns positions when lock is on
    # Define initial variable
    test_HONOExtractor2 <- test_HONOExtractor[1] # starting with first lock
    for (i in 2:length(test_HONOExtractor)){
      test_HONOExtractor2 <- append(test_HONOExtractor2, (test_HONOExtractor[i] - input$delete_before_HONO):(test_HONOExtractor[i] + input$delete_after_HONO))
    } # adds points previous to, equal to, and after lock to test_HONOExtractor2
    test_HONOExtractor2 <- unique(test_HONOExtractor2) # removes duplicates
    # Don't need to change to 1. Not used again
    # data_retrieve_HONO$data$Lock[test_HONOExtractor2] <- 1 # when lock is online, changes to 1
    rawdata_HONOExtractor <- data_retrieve_HONO$data[data_retrieve_HONO$data$Lock == 0, -c("Lock")] # data frame without lock column when lock off
    rawdata_HONOExtractor$Cycle <- as.numeric(as.character(rawdata_HONOExtractor$Cycle))
    rm(test_HONOExtractor)
    rm(test_HONOExtractor2)
    
    # Filter out grounding issues
    rawdata_HONOExtractor <- filter(rawdata_HONOExtractor, rawdata_HONOExtractor$Signal < 30)
    
    # Not used in actual analysis. Testing effect of background and signal levels
    for (i in 2:length(rawdata_HONOExtractor$Signal)){
      if (rawdata_HONOExtractor$Online[i] == 0 & rawdata_HONOExtractor$Signal[i] > 2){
        rawdata_HONOExtractor$Signal[i] <- 0
      }
    }
    
    ### ARCHAIC tests ###
    # Laser Scatter (LS) Test
    if (sum(rawdata_HONOExtractor$LS) > 0){
      LS_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$LS != 0, ]
      rawdata_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$LS == 0, ] # removes points where LS is not 0
    } 
    # Photo-diode (PD) Test
    if (sum(rawdata_HONOExtractor$PD) > 0){
      PD_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$PD != 0, ]
      rawdata_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$PD == 0, ] # removes points where PD is not 0
    }
    rawdata_HONOExtractor <- rawdata_HONOExtractor[,-c("LS", "PD")] # remove LS and PD columns
    # Remove first point of cycle where shutter hasn't opened yet
    # Get new column that's the difference between shutter status for a row and the row before it
    rawdata_HONOExtractor$newcol <- rawdata_HONOExtractor$Shutter - lag(rawdata_HONOExtractor$Shutter)
    # Find first point of opened shutter (start of HONO cycles_HONOExtractor)
    test_HONOExtractor <- which(rawdata_HONOExtractor$newcol == -1)
    # Row before is last point of closed shutter
    test_HONOExtractor <- test_HONOExtractor - 1
    # Remove points from rawdata_HONOExtractor and newcol from dataframe
    rawdata_HONOExtractor <- rawdata_HONOExtractor[-test_HONOExtractor, -c("newcol")]
    rm(test_HONOExtractor)
    
    # Get averages for each cycle for the variables that don't depend on cycling
    # Aggregate returns a dataframe, applies function to dataframe by grouping via list
    rawavg_HONOExtractor <- aggregate(rawdata_HONOExtractor, list(rawdata_HONOExtractor$Cycle), mean)
    rawsd_HONOExtractor <- aggregate(rawdata_HONOExtractor, list(rawdata_HONOExtractor$Cycle), sd)
    rawsd_HONOExtractor$Cycle <- rawavg_HONOExtractor$Cycle
    rawsd_HONOExtractor <- rawsd_HONOExtractor[ ,!(names(rawsd_HONOExtractor) %in% c("Group.1"))]
    
    # Separate and average onlines and offlines
    onl_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$Online == 1, -c("Online")]
    off_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$Online == 0, -c("Online")]
    
    # Averages based on Cycle number
    onlavg_HONOExtractor <- aggregate(onl_HONOExtractor, list(onl_HONOExtractor$Cycle), mean)
    offavg_HONOExtractor <- aggregate(off_HONOExtractor, list(off_HONOExtractor$Cycle), mean)
    # Counts within each cycle
    onlct_HONOExtractor <- aggregate(onl_HONOExtractor[,1], list(onl_HONOExtractor$Cycle), length)
    offct_HONOExtractor <- aggregate(off_HONOExtractor[,1], list(off_HONOExtractor$Cycle), length)
    # sd within each cycle
    onlsd_HONOExtractor <- aggregate(onl_HONOExtractor, list(onl_HONOExtractor$Cycle), sd)
    offsd_HONOExtractor <- aggregate(off_HONOExtractor, list(off_HONOExtractor$Cycle), sd)
    # Append counts and signal sd to rest of data
    onlavg_HONOExtractor$count <- onlct_HONOExtractor[[2]]
    offavg_HONOExtractor$count <- offct_HONOExtractor[[2]]
    onlavg_HONOExtractor$sigsd <- onlsd_HONOExtractor$Signal
    offavg_HONOExtractor$sigsd <- offsd_HONOExtractor$Signal
    
    onlavg_HONOExtractor <- onlavg_HONOExtractor[ , !(names(onlavg_HONOExtractor) %in% c("Group.1"))]
    offavg_HONOExtractor <- offavg_HONOExtractor[ , !(names(offavg_HONOExtractor) %in% c("Group.1"))]
    # If data set starts on an offline, removes the first offline to match online
    # if either on or offline has an extra cycle at the beginning, then the rawdata_HONOExtractor will also have an extra
    while(!offavg_HONOExtractor$Cycle[1] == onlavg_HONOExtractor$Cycle[1]){
      if(offavg_HONOExtractor$Cycle[1] < onlavg_HONOExtractor$Cycle[1]){
        offavg_HONOExtractor <- offavg_HONOExtractor[-1, ]
        rawavg_HONOExtractor <- rawavg_HONOExtractor[-1, ]
        rawsd_HONOExtractor <- rawsd_HONOExtractor[-1, ]
      } else if (offavg_HONOExtractor$Cycle[1] > onlavg_HONOExtractor$Cycle[1]){
        onlavg_HONOExtractor <- onlavg_HONOExtractor[-1, ]
        rawavg_HONOExtractor <- rawavg_HONOExtractor[-1, ]
        rawsd_HONOExtractor <- rawsd_HONOExtractor[-1, ]
      }
      print("In while loop, stop code if you see this too many times")
    }
    if (!(length(offavg_HONOExtractor$UVout1) == length(onlavg_HONOExtractor$UVout1))){
      if(length(offavg_HONOExtractor$UVout1) > length(onlavg_HONOExtractor$UVout1)){
        notinboth <- setdiff(offavg_HONOExtractor$Cycle, onlavg_HONOExtractor$Cycle)
        offavg_HONOExtractor <- offavg_HONOExtractor[!(offavg_HONOExtractor$Cycle %in% notinboth), ]
        rawavg_HONOExtractor <- rawavg_HONOExtractor[!(rawavg_HONOExtractor$Cycle %in% notinboth), ]
        rawsd_HONOExtractor <- rawsd_HONOExtractor[!(rawsd_HONOExtractor$Cycle %in% notinboth), ]
      } else {
        notinboth <- setdiff(onlavg_HONOExtractor$Cycle, offavg_HONOExtractor$Cycle)
        onlavg_HONOExtractor <- onlavg_HONOExtractor[!(onlavg_HONOExtractor$Cycle %in% notinboth), ]
        rawavg_HONOExtractor <- rawavg_HONOExtractor[!(rawavg_HONOExtractor$Cycle %in% notinboth), ]
        rawsd_HONOExtractor <- rawsd_HONOExtractor[!(rawsd_HONOExtractor$Cycle %in% notinboth), ]
      }
    }
    
    ### CYCLE AVERAGES ###
    #Get cycle averages
    col_names_HONOExtractor <- colnames(onlavg_HONOExtractor)
    cycles_HONOExtractor <- data.frame(matrix(ncol = length(col_names_HONOExtractor), nrow = length(onlavg_HONOExtractor$UVout1)))
    names(cycles_HONOExtractor) <- col_names_HONOExtractor
    # Not used in real analysis. Testing effect of absolute value
    # if (input$WeightSignal_HONO == 1){
    #   for (i in col_names_HONOExtractor){
    #     if (i == "UVout1"){
    #       cycles_HONOExtractor[[i]] <- abs((onlavg_HONOExtractor[[i]] * onlavg_HONOExtractor$count + offavg_HONOExtractor[[i]] * offavg_HONOExtractor$count) / (onlavg_HONOExtractor$count + offavg_HONOExtractor$count))
    #     } else if(i == "Signal") {
    #       cycles_HONOExtractor[[i]] <- abs(((onlavg_HONOExtractor[[i]] / onlavg_HONOExtractor$UVout1) - (offavg_HONOExtractor[[i]] / offavg_HONOExtractor$UVout1)) * ((onlavg_HONOExtractor$count * onlavg_HONOExtractor$UVout1) + (offavg_HONOExtractor$count * offavg_HONOExtractor$UVout1)) / (onlavg_HONOExtractor$count + offavg_HONOExtractor$count))
    #     } else if(i == "sigsd"){
    #       cycles_HONOExtractor[[i]] <- abs(onlavg_HONOExtractor[[i]])
    #     } else {
    #       cycles_HONOExtractor[[i]] <- rawavg_HONOExtractor[[i]]
    #     }
    #   }} 
    print("checkpoint 896")
    if (input$WeightSignal_HONO == 1){
      for (i in col_names_HONOExtractor){
        if (i == "UVout1"){
          cycles_HONOExtractor[[i]] <- (onlavg_HONOExtractor[[i]] * onlavg_HONOExtractor$count + offavg_HONOExtractor[[i]] * offavg_HONOExtractor$count) / (onlavg_HONOExtractor$count + offavg_HONOExtractor$count)
        } else if(i == "Signal") {
          cycles_HONOExtractor[[i]] <- ((onlavg_HONOExtractor[[i]] / onlavg_HONOExtractor$UVout1) - (offavg_HONOExtractor[[i]] / offavg_HONOExtractor$UVout1)) * ((onlavg_HONOExtractor$count * onlavg_HONOExtractor$UVout1) + (offavg_HONOExtractor$count * offavg_HONOExtractor$UVout1)) / (onlavg_HONOExtractor$count + offavg_HONOExtractor$count)
        } else if(i == "sigsd"){
          cycles_HONOExtractor[[i]] <- onlavg_HONOExtractor[[i]]
        } else {
          cycles_HONOExtractor[[i]] <- rawavg_HONOExtractor[[i]]
        }
      }} 
    print("checkpoint 910")
    if (input$WeightSignal_HONO == 0){
      # unweighted signal
      for (i in col_names_HONOExtractor){
        if (i == "UVout1"){
          cycles_HONOExtractor[[i]] <- abs(onlavg_HONOExtractor[[i]])
        } else if(i == "Signal") {
          cycles_HONOExtractor[[i]] <- abs((onlavg_HONOExtractor[[i]]  - offavg_HONOExtractor[[i]]))
        } else if(i == "sigsd"){
          cycles_HONOExtractor[[i]] <- abs(onlavg_HONOExtractor[[i]])
        } else {
          cycles_HONOExtractor[[i]] <- rawavg_HONOExtractor[[i]]
        }
      }}
    # WeightedNetSignal from here onward may not actually be weighted. Depends on user input but changing code was too much work for now
    cycles_HONOExtractor$WeightedNetSignal <- cycles_HONOExtractor$Signal
    cycles_HONOExtractor$bkgsd <- offavg_HONOExtractor$sigsd
    cycles_HONOExtractor <- filter(cycles_HONOExtractor, Signal >= 0)
    print("checkpoint 927")
    # Correct UVout1 for 355 interference. If says shutter off but UVout3 is low, laser is not actually on. Correct to 1 so shutter blocks laser
    # IF 355 NM LASER NOT ON AT ALL, THEN THIS WILL DELETE ALL SHUTTER OFF POINTS BECAUSE UVOUT3 TOO LOW
    for (i in 1:length(cycles_HONOExtractor$UVout1)){
      if (cycles_HONOExtractor$Shutter[i] == 0 & cycles_HONOExtractor$UVout3[i] < 0.01){
        cycles_HONOExtractor$Shutter[i] <- 1
      }
    }
    print("checkpoint 935")
    # cycles_HONOExtractor$Shutter[cycles_HONOExtractor$Shutter == 0 & cycles_HONOExtractor$UVout3 < 0.01] <- 1 # return columns where shutter off and UVout3 less than 0.01
    cycles_HONOExtractor_shutter <- filter(cycles_HONOExtractor, cycles_HONOExtractor$Shutter >= 0.9)
    # cycles_HONOExtractor_shutter <- cycles_HONOExtractor[cycles_HONOExtractor$Shutter == 1, ] # returns data frame with only rows where shutter is on
    cycles_HONOExtractor_shutter$newcol <- 0
    print("checkpoint 940")
    cycles_HONOExtractor_shutter <- cycles_HONOExtractor_shutter %>%
      mutate(newcol = zoo::rollmean(UVout1, k = 10, fill = NA)) # average every 10, fill in between with NA
    test_HONOExtractor2 <- which(is.na(cycles_HONOExtractor_shutter$newcol)) # return positions with NA
    
    for (i in 1:length(test_HONOExtractor2)){
      if (i < (length(cycles_HONOExtractor_shutter$newcol) / 2)){
        cycles_HONOExtractor_shutter$newcol[test_HONOExtractor2[i]] <- mean(cycles_HONOExtractor_shutter$UVout1[1:10])
      } else {
        cycles_HONOExtractor_shutter$newcol[test_HONOExtractor2[i]] <- mean(cycles_HONOExtractor_shutter$UVout1[length((cycles_HONOExtractor_shutter$UVout1)-10):length(cycles_HONOExtractor_shutter$UVout1)])
      }
    }
    print("checkpoint 952")
    cycles_HONOExtractor_shutter <- cycles_HONOExtractor_shutter[,c("Cycle", "newcol")]
    setDT(cycles_HONOExtractor_shutter)
    setDT(cycles_HONOExtractor)
    setkey(cycles_HONOExtractor_shutter, "Cycle")
    setkey(cycles_HONOExtractor, "Cycle")
    cycles_HONOExtractor <- cycles_HONOExtractor_shutter[cycles_HONOExtractor, roll = TRUE]
    
    cycles_HONOExtractor$UVout1[cycles_HONOExtractor$Shutter == 0] <- cycles_HONOExtractor$newcol[cycles_HONOExtractor$Shutter == 0]
    test_HONOExtractor2 <- which(is.na(cycles_HONOExtractor$newcol))
    for (i in 1:length(test_HONOExtractor2)){
      if (i < length(cycles_HONOExtractor$newcol) / 2){
        cycles_HONOExtractor$newcol[test_HONOExtractor2[i]] <- mean(cycles_HONOExtractor$newcol[1:10], na.rm = TRUE)
      } else {
        cycles_HONOExtractor$newcol[test_HONOExtractor2[i]] <- mean(cycles_HONOExtractor$newcol[length((cycles_HONOExtractor$newcol)-10):length(cycles_HONOExtractor$newcol)], na.rm = TRUE)
      }
    }
    print("checkpoint 969")
    # converts to 0-3% scale as absolute humidity
    # cycles_HONOExtractor$Water <- cycles_HONOExtractor$Water * 100
    # only for display. Want to input decimal value
    
    water_background_HONOExtractor <- 0 # always 0 in Excel extractor
    UVout2_background_HONOExtractor <- 0 # always 0 in Excel extractor
    setkey(cycles_HONOExtractor, Time)
    cycles_HONOExtractor$P308 <- cycles_HONOExtractor$UVout1 * (input$P308_start_HONO / input$UVout1_start_HONO)
    cycles_HONOExtractor$P355 <- cycles_HONOExtractor$UVout3 * (input$P355_start_HONO / input$UVout3_start_HONO)
    cycles_HONOExtractor$PE <- input$HONO_PE_HONO_Extractor
    
    cycles_HONOExtractor$NetWater <- cycles_HONOExtractor$Water - water_background_HONOExtractor
    
    # calculate ROH from best fit parameters of OH calibration
    cycles_HONOExtractor$ROH <- input$OHCal_slope * cycles_HONOExtractor$NetWater + input$OHCal_yint # Applying OH calibration 
    for (i in length(cycles_HONOExtractor$Shutter)){
      if (cycles_HONOExtractor$Shutter[i] >= 0.9){
        cycles_HONOExtractor$OH[i] <- cycles_HONOExtractor$WeightedNetSignal[i] / (cycles_HONOExtractor$ROH[i] * cycles_HONOExtractor$P308[i])
      }
      else if (cycles_HONOExtractor$Shutter[i] <= 0.1){
        cycles_HONOExtractor$HONO[i] <- cycles_HONOExtractor$WeightedNetSignal[i] / (cycles_HONOExtractor$ROH[i] * cycles_HONOExtractor$P308[i] * input$HONO_PE_HONO_Extractor * 2.46e12)  * 10^6
      }
      else {
        print("Error in shutter")
      }
    }
    print("checkpoint 996")
    # Filter out unexpected extreme peaks likely caused by pen lamp or grounding issue
    cycles_HONOExtractor <- filter(cycles_HONOExtractor, cycles_HONOExtractor$Signal < 50)
    print("checkpoint 999")
    OH_sig_HONOExtractor <- which(cycles_HONOExtractor$Shutter >= 0.9) # find when shutter is on (No HONO photolysis)
    # 0.9 instead of 1 allows for slight overflow error
    cycles_HONOExtractor$OH <- NA
    cycles_HONOExtractor$OH[OH_sig_HONOExtractor] <- cycles_HONOExtractor$WeightedNetSignal[OH_sig_HONOExtractor] / (cycles_HONOExtractor$ROH[OH_sig_HONOExtractor] * cycles_HONOExtractor$P308[OH_sig_HONOExtractor])
    # HONO_sig_HONOExtractor <- which(is.na(cycles_HONOExtractor$OH)) # find when shutter is off (355 laser on, HONO being photolyzed)
    cycles_HONOExtractor$HONO <- NA
    HONO_sig_HONOExtractor <- which(cycles_HONOExtractor$Shutter <= 0.1) # find when shutter is off (355 laser on, HONO being photolyzed)
    # HONO in ppt
    # cycles_HONOExtractor <- subset(cycles_HONOExtractor, cycles_HONOExtractor$OH > 0 & cycles_HONOExtractor$HONO > 0)
    cycles_HONOExtractor$HONO[HONO_sig_HONOExtractor] <- cycles_HONOExtractor$WeightedNetSignal[HONO_sig_HONOExtractor] / (cycles_HONOExtractor$ROH[HONO_sig_HONOExtractor] * cycles_HONOExtractor$P308[HONO_sig_HONOExtractor] * cycles_HONOExtractor$PE[HONO_sig_HONOExtractor] * 2.46e12) * 10^6 # molec/cm3 to ppt
    
    # Other errors may result from OH and HONO being far beyond expected values. Check these
    # HighOH <- which(cycles_HONOExtractor$OH > 1e8)
    # LowOH <- which(cycles_HONOExtractor$OH < 3e5)
    # cycles_HONOExtractor$OH[HighOH] <- NA
    # cycles_HONOExtractor$OH[LowOH] <- NA
    print("checkpoint 1028")
    # Delete row if ROH below user entered level which indicates error
    cycles_HONOExtractor <- filter(cycles_HONOExtractor, cycles_HONOExtractor$ROH > input$LowerROH_HONO & cycles_HONOExtractor$UVout1 != is.na(cycles_HONOExtractor$UVout1))
    print("checkpoint 1031")
    for (i in 1:length(cycles_HONOExtractor$HONO)){
      if (!is.na(cycles_HONOExtractor$HONO[i]) & cycles_HONOExtractor$HONO[i] < 10){
        cycles_HONOExtractor$HONO[i] <- cycles_HONOExtractor$HONO[i] + 10
      }
    }
    # for (i in 1:length(cycles_HONOExtractor$HONO)){
    #   if (!is.na(cycles_HONOExtractor$HONO[i])){
    #     if (cycles_HONOExtractor$HONO[i] <= 5){
    #       cycles_HONOExtractor$HONO[i] <- cycles_HONOExtractor$HONO[i] * 4.5
    #     }
    #   }
    # }
    # for (i in 1:length(cycles_HONOExtractor$HONO)){
    #   if (!is.na(cycles_HONOExtractor$HONO[i])){
    #     cycles_HONOExtractor$HONO[i] <- cycles_HONOExtractor$HONO[i] * sqrt(2)
    #   }
    # }
    # Add LOD to data frame to display as horizontal line on plots
    cycles_HONOExtractor$HONO_LOD <- input$HONO_LOD
    cycles_HONOExtractor$OH_LOD <- input$OH_LOD
    
    # Output input values as CSV
    HONO_Extractor_Input <- data.frame(matrix(ncol = 8), nrow = 2)
    HONO_Extractor_Input$P308 <- input$P308_start_HONO
    HONO_Extractor_Input$UVout1 <- input$UVout1_start_HONO
    HONO_Extractor_Input$P355 <- input$P355_start_HONO
    HONO_Extractor_Input$UVout3 <- input$UVout3_start_HONO
    HONO_Extractor_Input$CalibrationSlope <- input$OHCal_slope
    HONO_Extractor_Input$CalibrationInt <- input$OHCal_yint
    HONO_Extractor_Input$InitialFile <- input$raw_data_file_HONO
    # HONO_Extractor_Input$StartTime <- cycles_HONOExtractor$Time[1]
    # HONO_Extractor_Input$EndTime <- cycles_HONOExtractor$Time[nrow(cycles_HONOExtractor$Time)]
    HONO_Extractor_Input <- HONO_Extractor_Input[,c("P308", "UVout1", "P355", "UVout3", "CalibrationSlope", "CalibrationInt", "InitialFile")]
    
    updateTextInput(session, "filenameHONO", value = "HONO Extracted Data.csv")
    updateTextInput(session, "filenameHONOInput", value = "HONO Extractor Inputs.csv")
    
    # Display Output HONO Extractor -------------------------------------------
    output$plot1_HONOExtractor <- renderPlotly({
      req(input$raw_data_file_HONO)
      ggplot(cycles_HONOExtractor, aes(x = Time, y = OH)) +
        geom_point(size=2, shape=16) +
        geom_hline(yintercept = input$OH_LOD, linetype = "dashed", color = "blue") + 
        theme_bw() +
        xlab("Time") +
        ylab("OH (molec/cm3)")
    })
    output$plot2_HONOExtractor <- renderPlotly({
      req(input$raw_data_file_HONO)
      ggplot(cycles_HONOExtractor, aes(x = Time, y = HONO)) +
        geom_point(size = 2, shape = 16) +
        geom_hline(yintercept = input$HONO_LOD, linetype = "dashed", color = "blue") + 
        theme_bw() +
        xlab("Time") +
        ylab("HONO (ppt)") 
      # geom_point(aes(x = Time, y = OH), size = 2, color = "blue") + 
      # scale_y_continuous(
      #   "[OH] (molec/cm^3", 
      #   sec.axis = sec_axis(~ . * 5e7, name = "[OH] (molec/cm^3)")
      # )
    })
    output$table2_HONOExtractor <- renderDataTable({
      req(input$raw_data_file_HONO)
      cycles_HONOExtractor
    })
    # output$plot3_HONOExtractor <- renderPlotly({
    #   ggplot(cycles_HONOExtractor, aes(x = Time, y = HONO)) +
    #   stat_boxplot(geom = "errorbar") +
    #   geom_boxplot() +
    #   theme(axis.title.x = element_blank()) +
    #   ylab("[HONO] (ppt)")
    # })
    # 
    output$downloadHONOExtractor <- downloadHandler(
      filename = function() {
        paste("HONO Extracted Data ", Sys.Date(), ".csv", sep = "") # can replace Sys.Date() with input$filenameHONO
      },
      content = function(file) {
        write.csv(cycles_HONOExtractor, file)
      })
    output$downloadHONOExtractorInput <- downloadHandler(
      filename = function() {
        paste("HONO Extractor Inputs ", Sys.Date(), ".csv", sep = "") # can replace Sys.Date() with input$filenameHONO
      },
      content = function(file) {
        write.csv(HONO_Extractor_Input, file)
      })
  })
  
  # Post Extraction Edit Start ----------------------------------------------
  observeEvent(input$PostExtractionEditStart, {
    req(input$raw_data_file_PostExtraction)
    # Averaging based on resolution entered by user
    # time break options: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cut.POSIXt
    col_names_PostExtraction <- colnames(data_retrieve_PostExtraction$data)
    PostExtraction <- data.frame(matrix(ncol = length(col_names_PostExtraction), nrow = length(data_retrieve_PostExtraction$data$UVout1)))
    names(PostExtraction) <- col_names_PostExtraction
    PostExtraction <- data_retrieve_PostExtraction$data
    # PostExtraction <- PostExtraction[, -c("newcol", "X", "Shutter", "PE", MeasCycle", "Cycle", "UVout1", "UVout3")]
    
    # If averaging already averaged file
    PostExtraction <- PostExtraction[, -c("Column1", "Column2", "X.1", "TimeBreak", "X", "PE", "Date", "i.TimeBreak", "Cycle", "UVout1", "UVout3")]
    
    # Average based on time resolution entered by user
    # Complains about averaging non numeric values
    PostExtraction$Cycle <- as.numeric(PostExtraction$Cycle)
    PostExtraction$Pressure <- as.numeric(PostExtraction$Pressure)
    PostExtraction$Signal <- as.numeric(PostExtraction$Signal)
    PostExtraction$RH <- as.numeric(PostExtraction$RH)
    PostExtraction$Temp <- as.numeric(PostExtraction$Temp)
    PostExtraction$Water <- as.numeric(PostExtraction$Water)
    PostExtraction$sigsd <- as.numeric(PostExtraction$sigsd)
    PostExtraction$WeightedNetSignal <- as.numeric(PostExtraction$WeightedNetSignal)
    PostExtraction$bkgsd <- as.numeric(PostExtraction$bkgsd)
    PostExtraction$P308 <- as.numeric(PostExtraction$P308)
    PostExtraction$P355 <- as.numeric(PostExtraction$P355)
    PostExtraction$NetWater <- as.numeric(PostExtraction$NetWater)
    PostExtraction$ROH <- as.numeric(PostExtraction$ROH)
    PostExtraction$HONO <- as.numeric(PostExtraction$HONO)
    PostExtraction$OH <- as.numeric(PostExtraction$OH)
    PostExtraction$HONO_LOD <- as.numeric(PostExtraction$HONO_LOD)
    PostExtraction$OH_LOD <- as.numeric(PostExtraction$OH_LOD)
    label <- "min"
    TimeBreak <- paste(input$TimeAverage, label)
    # Always check time format. Might need to change to %Y-%m-%d %H:%M:%S
    # If averaging a file that was already averaged for 30min and now doing 60min, change time format to %Y-%m-%d %H:%M:%S 
    # PostExtraction$TimeBreak <- cut.POSIXt(as.POSIXct(PostExtraction$Time, format = "%Y-%m-%d %H:%M:%S"), breaks = TimeBreak)
    print("checkpoint 1323")
    PostExtraction$TimeBreak <- cut.POSIXt(as.POSIXct(PostExtraction$Time, format = "%m/%d/%Y %H:%M"), breaks = TimeBreak)
    # Must have na.rm = TRUE or aggregate removes whole row when NA present
    print("checkpoint 1315")
    PostExtraction <- aggregate(PostExtraction, by = list(PostExtraction$TimeBreak), FUN = mean, na.rm = TRUE)
    # PostExtraction$Time <- as.POSIXct(as.character(PostExtraction$Group.1), format = "%Y-%m-%d %H:%M:%S")
    print("checkpoint 1318")
    PostExtraction$Time <- as.POSIXct(as.character(PostExtraction$Group.1), format = "%m/%d/%Y %H:%M")
    # for (i in 1:length(PostExtraction)){
    #   if (PostExtraction$HONO[i] > 100){
    #     PostExtraction$HONO[i] <- PostExtraction$HONO[i] / 1e6
    #   }
    # }
    # can't have NA in if statement
    # for (i in 1:length(PostExtraction)){
    #   if (!is.na(PostExtraction$HONO[i])){
    #     if (PostExtraction$HONO[i] > 100){
    #       PostExtraction$HONO[i] <- PostExtraction$HONO[i] / 1e5
    #     }
    #   }
    # }
    PostExtraction$HONO <- ifelse(PostExtraction$HONO > 100 & !is.na(PostExtraction$HONO), PostExtraction$HONO / 1e5, PostExtraction$HONO)

    # Filter out weird  temp, and RH probe
    PostExtraction <- subset(PostExtraction,
                               Temp < 50 & Temp > 0 &
                               RH > 0 & RH < 100)
    print(max(PostExtraction$HONO))
    print(min(PostExtraction$HONO))
    print(mean(PostExtraction$HONO))
    
    updateTextInput(session, "filenamePostExtraction", value = "HONO Edited Data.csv")
    
    output$plotPostExtraction_HONO <- renderPlotly({
      req(input$raw_data_file_PostExtraction)
      ggplot(PostExtraction, aes(x = Group.1, y = HONO)) +
        geom_point(size = 1, shape = 16) +
        theme_bw() + 
        xlab("Time") +
        ylab("[HONO] (ppt)")
    })
    output$plotPostExtraction_OH <- renderPlotly({
      req(input$raw_data_file_PostExtraction)
      ggplot(PostExtraction, aes(x = Group.1, y = OH)) +
        geom_point(size = 1, shape=16) +
        theme_bw() +
        xlab("Time") +
        ylab("[OH] (molec/cm^3)")
    })
    output$table_OutputData_PostExtraction <- renderDataTable({
      req(input$raw_data_file_PostExtraction)
      PostExtraction
    })
    # Download output data
    output$downloadPostExtraction <- downloadHandler(
      filename = function() {
        paste("HONO Edited Data ", Sys.Date(), ".csv", sep ="") # can replace Sys.Date() with input$filenameHONO
      },
      content = function(file) {
        write.csv(PostExtraction, file)
      })
  })
  # HONO Photolysis Start ----------------------------------------------------
  observeEvent(input$HONOPEStart, {
    req(input$raw_data_file_HONOPE)
    # ASSUMPTION: RH is constant
    # Cycles: reset, OH, ROH, HONO, Tank (1,3,4,6) but the 6 is 3 HONO and 3 tank. 
    # Lamp turned off during tank
    # NO flow off during OH cycles
    # Calculate mean by Notes 
    col_names_HONOPE <- colnames(data_retrieve_HONOPE$data)
    HONOPE_data <- data.frame(matrix(ncol = length(col_names_HONOPE), nrow = length(data_retrieve_HONOPE$data$UVout1)))
    names(HONOPE_data) <- col_names_HONOPE
    HONOPE_data <- data_retrieve_HONOPE$data
    # correct for grounding
    HONOPE_data <- filter(HONOPE_data, Signal < 100)
    
    # Filter cycles and average
    OH_cycles <- filter(HONOPE_data, Notes == "OH")
    HONOPE_OH <- mean(OH_cycles$Signal)
    ROH_cycles <- filter(HONOPE_data, Notes == "ROH")
    HONOPE_ROH <- mean(ROH_cycles$Signal)
    HONO_cycles <- filter(HONOPE_data, Notes == "HONO")
    HONOPE_HONO <- mean(HONO_cycles$Signal)
    Tank_cycles <- filter(HONOPE_data, Notes == "Tank")
    HONOPE_Tank <- mean(Tank_cycles$Signal)
    
    # Calculate HONO Photolysis Efficiency
    HONOPE_result <- (HONOPE_HONO - HONOPE_Tank) / (2 * HONOPE_OH - HONOPE_ROH) * 100 # %
    
    print(HONOPE_OH)
    print(HONOPE_ROH)
    print(HONOPE_HONO)
    print(HONOPE_Tank)
    print(HONOPE_result)
    Variable_HONOPE <- c("HONO Photolysis Efficiency")
    Value_HONOPE <- c(HONOPE_result)
    Units_HONOPE <- c("%")
    outputDataVisual_HONOPE <- data.frame(Variable_HONOPE, Value_HONOPE, Units_HONOPE)
    outputDataVisual_HONOPE$Value_HONOPE <- formatC(outputDataVisual_HONOPE$Value, format = "e", digits = 4)
    
    # Display Output HONO Photolysis -------------------------------------------
    output$plot1_HONOPE <- renderPlotly({
      req(input$raw_data_file_HONOPE)
      ggplot(data_retrieve_HONOPE$data, aes(x = Time, y = Signal)) +
        geom_point(size = 2, shape = 16) +
        theme_bw() +
        xlab("Time") +
        ylab("Raw Signal")
    })
    output$table_OutputData_HONOPE <- renderDataTable({
      req(input$raw_data_file_HONOPE)
      outputDataVisual_HONOPE
    })
  })
  # Data Logger Start ----------------------------------------------
  observeEvent(input$DataLoggerStart, {
    req(input$raw_data_file_DataLogger)
    # Averaging based on resolution entered by user
    # time break options: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cut.POSIXt
    col_names_DataLogger <- colnames(data_retrieve_DataLogger$data)
    DataLogger <- data.frame(matrix(ncol = length(col_names_DataLogger), nrow = length(data_retrieve_DataLogger$data$O3)))
    names(DataLogger) <- col_names_DataLogger
    DataLogger <- data_retrieve_DataLogger$data
    # Only run this if averaging an already averaged file, like averaging 30 min to get 60 min
    # DataLogger <- DataLogger[, -c("Column1", "Column2", "TimeBreak")]
    DataLogger$O3 <- as.numeric(DataLogger$O3)
    DataLogger$NO <- as.numeric(DataLogger$NO)
    DataLogger$NO2 <- as.numeric(DataLogger$NO2)
    DataLogger$NOx <- as.numeric(DataLogger$NOx)
    DataLogger$JNO2 <- as.numeric(DataLogger$JNO2)
    # Average based on time resolution entered by user
    # Complains about averaging non numeric values
    # Filter out bad NO2 data. NO will be estimated later and NOx = NO + NO2
    DataLogger <- filter(DataLogger, NO2 < 20)
    label_DataLogger <- "min"
    TimeBreak_DataLogger <- paste(input$TimeAverage_DataLogger, label_DataLogger)
    DataLogger$TimeBreak <- cut.POSIXt(as.POSIXct(DataLogger$Time, format = "%m/%d/%Y %H:%M"), breaks = TimeBreak_DataLogger)
    # Must have na.rm = TRUE or aggregate removes whole row when NA present
    DataLogger <- aggregate(DataLogger, by = list(DataLogger$TimeBreak), FUN = mean, na.rm = TRUE)
    DataLogger$Time <- as.POSIXct(as.character(DataLogger$Group.1), format = "%m/%d/%Y %H:%M")
    # can't have NA in if statement
    updateTextInput(session, "filenameDataLogger", value = "Data Logger.csv")
    
    output$plotDataLogger_O3 <- renderPlotly({
      req(input$raw_data_file_DataLogger)
      ggplot(DataLogger, aes(x = Time, y = O3)) +
        geom_point(size = 1, shape = 16) +
        theme_bw() + 
        xlab("Time") +
        ylab("[O3] (ppb)")
    })
    output$plotDataLogger_NOx <- renderPlotly({
      req(input$raw_data_file_DataLogger)
      ggplot(DataLogger, aes(x = Time, y = NOx)) +
        geom_point(size = 1, shape=16) +
        theme_bw() +
        xlab("Time") +
        ylab("[NOx] (ppb)")
    })
    output$plotDataLogger_JNO2 <- renderPlotly({
      req(input$raw_data_file_DataLogger)
      ggplot(DataLogger, aes(x = Time, y = JNO2)) +
        geom_point(size = 1, shape=16) +
        theme_bw() +
        xlab("Time") +
        ylab("JNO2")
    })
    output$table_OutputData_DataLogger <- renderDataTable({
      req(input$raw_data_file_DataLogger)
      DataLogger
    })
    # Download output data
    output$downloadDataLogger <- downloadHandler(
      filename = function() {
        paste("Data Logger ", Sys.Date(), ".csv", sep ="") # can replace Sys.Date() with input$filenameHONO
      },
      content = function(file) {
        write.csv(DataLogger, file)
      })
  })
  # Merge Start ----------------------------------------------
  observeEvent(input$MergeStart, {
    req(input$raw_data_file_MergeHONO)
    req(input$raw_data_file_MergeDataLogger)
    
    col_names_MergeHONO <- colnames(data_retrieve_MergeHONO$data)
    MergeHONO <- data.frame(matrix(ncol = length(col_names_MergeHONO), nrow = length(data_retrieve_MergeHONO$data$HONO)))
    names(MergeHONO) <- col_names_MergeHONO
    MergeHONO <- data_retrieve_MergeHONO$data
    MergeHONO$Time <- as.POSIXct(MergeHONO$Time, format = "%m/%d/%Y %H:%M")
    # MergeHONO$Time <- as.POSIXct(MergeHONO$Time, format = "%Y-%m-%d %H:%M:%S")
    
    col_names_MergeDataLogger <- colnames(data_retrieve_MergeDataLogger$data)
    MergeDataLogger <- data.frame(matrix(ncol = length(col_names_MergeDataLogger), nrow = length(data_retrieve_MergeDataLogger$data$O3)))
    names(MergeDataLogger) <- col_names_MergeDataLogger
    MergeDataLogger <- data_retrieve_MergeDataLogger$data
    MergeDataLogger$Time <- as.POSIXct(MergeDataLogger$Time, format = "%m/%d/%Y %H:%M")
    
    setDT(MergeHONO)
    setkey(MergeHONO, Time)
    setDT(MergeDataLogger)
    setkey(MergeDataLogger, Time)
    
    # Merge <- MergeHONO[MergeDataLogger, roll = 'nearest']
    Merge <- MergeDataLogger[MergeHONO, roll = 'nearest']

    ## Example
    # MergeHONO$Time2 <- MergeDataLogger$Time2[findclose(Mergehono$Time, MergeDataLogger$Time2)]
    # might need to do for each variable
    # Merge <- MergeDataLogger[findclose(MergeHONO$Time, MergeDataLogger$Time)]
    # match_times <- findclose(MergeHONO$Time, MergeDataLogger$Time)
    # print(match_times)
    # Merge$Time2 <- MergeDataLogger$Time[match_times]
    # Merge$Time3 <- MergeDataLogger$Time
    # test1$Time2 = test2$Time2[findclose(test1$Time1, test2$Time2)]
    # test1$data2 = test2$data2[findclose(test1$Time1, test2$Time2)]
    updateTextInput(session, "filenameMerge", value = "Merge.csv")
    
    output$table_Output_DataMerge <- renderDataTable({
      req(input$raw_data_file_MergeHONO)
      req(input$raw_data_file_MergeDataLogger)
      Merge
    })
    # Download output data
    output$downloadMerge <- downloadHandler(
      filename = function() {
        paste("Merge ", Sys.Date(), ".csv", sep ="") # can replace Sys.Date() with input$filenameHONO
      },
      content = function(file) {
        write.csv(Merge, file)
      })
  })
  # Display Raw Data --------------------------------------------------------
  # Automatically does this once file uploaded
  
  # Display raw OH data file
  output$table1 <- renderDataTable({
    req(input$raw_data_file)
    data_retrieve$data
  })
  # Display raw HONO data file
  output$table1_HONOExtractor <- renderDataTable({
    req(input$raw_data_file_HONO)
    data_retrieve_HONO$data
  })
  # Display raw Post Extraction data file
  output$table_RawData_PostExtraction <- renderDataTable({
    req(input$raw_data_file_PostExtraction)
    data_retrieve_PostExtraction$data
  })
  # Display raw HONO PE data file
  output$table1_HONOPE <- renderDataTable({
    req(input$raw_data_file_HONOPE)
    data_retrieve_HONOPE$data
  })
  # Display raw Data Logger data file
  output$table_RawData_DataLogger <- renderDataTable({
    req(input$raw_data_file_DataLogger)
    data_retrieve_DataLogger$data
  })
  # Display averaged HONO data file
  output$table_RawData_HONOMerge <- renderDataTable({
    req(input$raw_data_file_MergeHONO)
    data_retrieve_MergeHONO$data
  })
  # Display averaged Data Logger data file
  output$table_RawData_MergeDataLogger <- renderDataTable({
    req(input$raw_data_file_MergeDataLogger)
    data_retrieve_MergeDataLogger$data
  })
  
  
}
# run_with_themer(shinyApp(ui,server))
# End App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)


