##########################################################
### Universal Statistical Analysis Shiny App
### Works with Any Dataset - CRD Analysis
##########################################################

library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(moments)
library(corrplot)
library(RColorBrewer)
library(BSDA)
library(plotrix)
library(ggplot2)
library(car)
library(MASS)     # boxcox
library(lmtest)   # bptest, dwtest
library(bslib)

spn <- function(x) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(x, type = 4, color = "#2563eb")
  } else {
    x
  }
}

HAS_PLOTLY <- requireNamespace("plotly", quietly = TRUE)

plotly_output_safe <- function(outputId, height = "400px") {
  if (HAS_PLOTLY) {
    plotly::plotlyOutput(outputId, height = height)
  } else {
    tags$div(
      class = "alert alert-info",
      style = "margin-top:8px;",
      "Interactive plots require package 'plotly'. Install with install.packages('plotly')."
    )
  }
}

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Universal Statistical Analysis",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Data Preview", tabName = "preview", icon = icon("table")),
      menuItem("Variable Analysis", tabName = "varanalysis", icon = icon("sliders-h")),
      menuItem("Correlation Matrix", tabName = "correlation", icon = icon("th")),
      menuItem("Categorical Suite", tabName = "catsuite", icon = icon("layer-group")),
      menuItem("Hypothesis Tests", tabName = "tests", icon = icon("calculator")),
      menuItem("Simple Linear Regression", tabName = "slr", icon = icon("chart-line")),
      menuItem("Multiple Linear Regression", tabName = "mlr", icon = icon("project-diagram")),
      menuItem("Indicator Variable", tabName = "indicator", icon = icon("tags")),
      menuItem("Model Adequacy", tabName = "modeladequacy", icon = icon("stethoscope")),
      menuItem("Box-Cox & Box-Tidwell", tabName = "boxtrans", icon = icon("magic")),
      menuItem("Weighted Least Squares", tabName = "wls", icon = icon("balance-scale")),
      menuItem("Summary Report", tabName = "report", icon = icon("file-alt"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('setDarkMode', function(message) {
          if (message && message.enabled) {
            document.body.classList.add('dark-mode-pro');
          } else {
            document.body.classList.remove('dark-mode-pro');
          }
        });
      ")),
      tags$style(HTML("
        :root {
          --bg: #f5f7fb;
          --surface: #ffffff;
          --surface-soft: #f8fafc;
          --ink: #111827;
          --muted: #6b7280;
          --primary: #2563eb;
          --primary-dark: #1e40af;
          --border: #e5e7eb;
          --radius: 14px;
        }
        body, h1, h2, h3, h4, h5, h6, .content, .main-header .logo, .main-header .navbar, .sidebar-menu > li > a {
          font-family: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
          color: var(--ink);
        }
        .content-wrapper, .right-side {
          background: radial-gradient(circle at 10% 0%, #e9f2ff 0%, #f5f7fb 35%, #f5f7fb 100%);
        }
        .content { padding: 18px; }
        .main-header .logo {
          background: linear-gradient(135deg, #1d4ed8, #0f172a) !important;
          color: #fff !important;
          font-weight: 700;
          border-right: 1px solid rgba(255,255,255,.15);
        }
        .main-header .navbar {
          background: linear-gradient(135deg, #1e3a8a, #1d4ed8) !important;
          box-shadow: 0 6px 16px rgba(37, 99, 235, .25);
        }
        .main-sidebar {
          background: linear-gradient(180deg, #111827 0%, #0f172a 100%) !important;
          box-shadow: 6px 0 24px rgba(2, 6, 23, .22);
        }
        .sidebar-menu { margin-top: 10px; }
        .sidebar-menu > li > a {
          color: #d1d5db !important;
          border-radius: 10px;
          margin: 4px 10px;
          padding: 11px 14px;
          transition: background .2s ease, transform .2s ease, color .2s ease;
        }
        .sidebar-menu > li > a:hover {
          background: rgba(59, 130, 246, .18) !important;
          color: #fff !important;
          transform: translateX(2px);
        }
        .sidebar-menu > li.active > a {
          background: linear-gradient(135deg, #2563eb, #1d4ed8) !important;
          color: #fff !important;
          box-shadow: 0 6px 16px rgba(37,99,235,.35);
        }
        .box {
          background: var(--surface);
          border: 1px solid var(--border);
          border-top: 0 !important;
          border-radius: var(--radius);
          box-shadow: 0 8px 24px rgba(15, 23, 42, 0.07);
          overflow: visible;
        }
        .box-header {
          border-bottom: 1px solid var(--border);
          background: linear-gradient(180deg, #ffffff, #f8fbff);
        }
        .box.box-primary .box-header { border-left: 5px solid #2563eb; }
        .box.box-info .box-header { border-left: 5px solid #0891b2; }
        .box.box-success .box-header { border-left: 5px solid #16a34a; }
        .box.box-warning .box-header { border-left: 5px solid #ea580c; }
        .box.box-danger .box-header { border-left: 5px solid #dc2626; }
        .small-box, .info-box {
          border-radius: 12px;
          box-shadow: 0 6px 16px rgba(15, 23, 42, 0.08);
        }
        h2 { color: #1d4ed8; font-weight: 700; letter-spacing: -.02em; }
        .btn-analyze {
          background: linear-gradient(135deg, #0ea5e9, #2563eb);
          color: white;
          font-weight: 600;
          padding: 10px 30px;
          font-size: 15px;
          border: none;
          border-radius: 10px;
          box-shadow: 0 6px 16px rgba(37,99,235,.3);
        }
        .btn-analyze:hover { opacity: .95; color: #fff; }
        .form-control, .selectize-input {
          border-radius: 10px !important;
          border: 1px solid #d1d5db !important;
          min-height: 44px;
          background: #fff;
          box-shadow: none;
        }
        .selectize-dropdown {
          z-index: 10050 !important;
        }
        .selectize-input { font-size: 14px; padding-top: 10px; padding-bottom: 10px; }
        .control-label { font-weight: 600; color: #111827; }
        .tab-content { padding-top: 10px; }
        .nav-tabs-custom { border-radius: 12px; overflow: hidden; box-shadow: 0 4px 12px rgba(15,23,42,.06); }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #2563eb;
        }
        .nav-tabs-custom > .nav-tabs > li > a { color: #374151; font-weight: 600; }
        .help-block, .hint-text { color: var(--muted) !important; }
        .quick-actions .btn {
          border-radius: 10px;
          font-weight: 600;
          margin-right: 8px;
        }
        .kpi-row .info-box { min-height: 92px; }

        body.dark-mode-pro .content-wrapper,
        body.dark-mode-pro .right-side {
          background: #0b1220 !important;
        }
        body.dark-mode-pro .box,
        body.dark-mode-pro .small-box,
        body.dark-mode-pro .info-box,
        body.dark-mode-pro .nav-tabs-custom {
          background: #0f172a !important;
          color: #e5e7eb !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro .box-header {
          background: #111827 !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro .control-label,
        body.dark-mode-pro .section-label,
        body.dark-mode-pro h1, body.dark-mode-pro h2, body.dark-mode-pro h3,
        body.dark-mode-pro h4, body.dark-mode-pro h5 {
          color: #e5e7eb !important;
        }
        body.dark-mode-pro .form-control,
        body.dark-mode-pro .selectize-input {
          background: #111827 !important;
          color: #e5e7eb !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro pre {
          background: #0b1220 !important;
          color: #e5e7eb !important;
          border-color: #334155 !important;
        }

        /* ── Regression tab enhancements ── */
        .reg-step-banner {
          background: linear-gradient(135deg, #1a73e8 0%, #0d47a1 100%);
          color: #fff;
          border-radius: 8px;
          padding: 14px 20px;
          margin-bottom: 18px;
          display: flex;
          align-items: center;
          gap: 14px;
          box-shadow: 0 2px 8px rgba(0,0,0,.15);
        }
        .reg-step-banner.success { background: linear-gradient(135deg,#2e7d32,#1b5e20); }
        .reg-step-banner.warning { background: linear-gradient(135deg,#e65100,#bf360c); }
        .reg-step-banner.info    { background: linear-gradient(135deg,#00695c,#004d40); }
        .step-badge {
          background: rgba(255,255,255,.25);
          border-radius: 50%;
          width: 38px; height: 38px;
          display: flex; align-items: center; justify-content: center;
          font-size: 18px; font-weight: 700;
          flex-shrink: 0;
        }
        .reg-step-banner h4 { margin: 0; font-size: 16px; font-weight: 600; }
        .reg-step-banner p  { margin: 2px 0 0; font-size: 12px; opacity: .85; }
        .box.reg-card {
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,.08);
        }
        .reg-card .box-header { border-radius: 7px 7px 0 0; }
        .fit-btn {
          background: linear-gradient(135deg,#1565C0,#0D47A1);
          color: #fff !important;
          font-weight: 600;
          font-size: 15px;
          padding: 10px 28px;
          border: none;
          border-radius: 6px;
          box-shadow: 0 2px 6px rgba(0,0,0,.2);
          transition: opacity .2s;
        }
        .fit-btn:hover { opacity: .88; color: #fff !important; }
        .predict-btn {
          background: linear-gradient(135deg,#6a1b9a,#4a148c);
          color: #fff !important;
          font-weight: 600;
          font-size: 14px;
          padding: 9px 24px;
          border: none;
          border-radius: 6px;
          box-shadow: 0 2px 6px rgba(0,0,0,.2);
        }
        .predict-btn:hover { opacity: .88; color: #fff !important; }
        .hint-text { font-size: 12px; color: #666; margin-top: 4px; }
        .section-label {
          font-size: 11px;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: .8px;
          color: #888;
          margin-bottom: 6px;
        }
        pre { background:#f8f9fa; border:1px solid #e0e0e0;
              border-radius:5px; padding:12px; font-size:13px; }
        .nav-tabs > li > a { font-size: 13px; font-weight: 500; }
        .reg-help-alert {
          background:#e3f2fd; border-left:4px solid #1976d2;
          border-radius:5px; padding:10px 14px; margin-bottom:12px;
          font-size:13px; color:#0d47a1;
        }
      "))
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'upload' || input.tabs == 'preview'",
      fluidRow(
        box(
          width = 12, status = "primary", solidHeader = FALSE, class = "quick-actions",
          fluidRow(
            column(6,
              tags$div(style="display:flex; gap:8px; flex-wrap:wrap;",
                actionButton("qaSample", "Sample Data", icon = icon("database"), class = "btn btn-primary"),
                actionButton("qaReset", "Reset Data", icon = icon("undo"), class = "btn btn-default"),
                actionButton("presetPriceMileage", "Preset: Price vs Mileage", icon = icon("bolt"), class = "btn btn-info"),
                actionButton("presetBrandCompare", "Preset: Brand Comparison", icon = icon("tags"), class = "btn btn-info"),
                downloadButton("downloadReport", "Download Report", class = "btn btn-success")
              )
            ),
            column(3,
              uiOutput("globalVarSearchUI")
            ),
            column(3,
              tags$div(style="display:flex; gap:8px; align-items:center; justify-content:flex-end; flex-wrap:wrap;",
                checkboxInput("interactivePlots", "Interactive", value = HAS_PLOTLY),
                checkboxInput("darkModeToggle", "Dark mode", FALSE),
                downloadButton("downloadState", "Save Session", class = "btn btn-success"),
                fileInput("loadState", NULL, accept = ".rds", buttonLabel = "Load Session", placeholder = "No file")
              )
            )
          )
        )
      ),

      fluidRow(
        class = "kpi-row",
        infoBoxOutput("kpiRows", width = 3),
        infoBoxOutput("kpiCols", width = 3),
        infoBoxOutput("kpiMissing", width = 3),
        infoBoxOutput("kpiTypes", width = 3)
      )
    ),

    tabItems(
      # Upload Tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Your Dataset",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(6,
                fileInput("datafile", 
                          "Choose CSV or Excel File",
                          accept = c(".csv", ".xlsx", ".xls"),
                          buttonLabel = "Browse...",
                          placeholder = "No file selected")
              ),
              column(6,
                br(),
                actionButton("analyzeBtn", "Analyze Data", 
                           class = "btn-analyze",
                           icon = icon("chart-bar"))
              )
            ),
            
            hr(),
            
            h4("Instructions:"),
            tags$ul(
              tags$li("Upload any CSV or Excel file with your data (maximum 50 MB)"),
              tags$li("The app will automatically detect numerical and categorical variables"),
              tags$li("Preview your data in the 'Data Preview' tab"),
              tags$li("Select variables for analysis in each tab"),
              tags$li("Click 'Analyze Data' to confirm your upload")
            ),

            tags$div(
              style = "background-color: #dff0d8; padding: 10px; border-radius: 5px; margin-top: 10px;",
              tags$strong(icon("check-circle"), " Default Dataset Loaded:"),
              " The app starts with P2_DeliveryTime.xlsx, so you can analyze immediately without uploading."
            ),
            
            tags$div(
              style = "background-color: #d9edf7; padding: 10px; border-radius: 5px; margin-top: 10px;",
              tags$strong(icon("info-circle"), " File Size Limit:"),
              " Maximum upload size is 50 MB. For larger datasets, consider filtering or sampling your data before upload."
            ),
            
            hr(),
            
            h4("Variable Detection:"),
            fluidRow(
              column(6,
                h5("Numerical Variables:"),
                verbatimTextOutput("numericVars")
              ),
              column(6,
                h5("Categorical Variables:"),
                verbatimTextOutput("categoricalVars")
              )
            )
          )
        ),

        fluidRow(
          box(
            title = tagList(icon("sliders-h"), " Data Cleaning"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            fluidRow(
              column(3,
                selectInput("cleanMissing", "Missing Values:",
                            choices = c("Keep as is" = "keep",
                                        "Drop rows with missing" = "drop",
                                        "Impute (median/mode)" = "impute"),
                            selected = "keep")
              ),
              column(3,
                selectInput("cleanOutliers", "Outliers (IQR):",
                            choices = c("Remove" = "remove",
                                        "Keep" = "keep"),
                            selected = "remove")
              ),
              column(3,
                uiOutput("cleanLogVarsUI")
              ),
              column(3,
                br(),
                actionButton("applyCleaning", "Apply Cleaning", class = "btn btn-warning")
              )
            ),
            tags$p(class = "hint-text", "Tip: log transform applies log1p(x) to selected numeric variables.")
          )
        ),
        
        fluidRow(
          infoBoxOutput("rowCountBox", width = 4),
          infoBoxOutput("colCountBox", width = 4),
          infoBoxOutput("statusBox", width = 4)
        )
      ),
      
      # Data Preview Tab
      tabItem(
        tabName = "preview",
        fluidRow(
          box(
            title = "Data Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            spn(DTOutput("dataPreview")),
            
            hr(),
            
            h4("Data Summary"),
            fluidRow(
              column(7,
                h5("Numeric Variables"),
                spn(DT::DTOutput("dataSummaryNumeric"))
              ),
              column(5,
                h5("Categorical Variables"),
                spn(DT::DTOutput("dataSummaryCategorical"))
              )
            )
          )
        )
      ),
      
      # Variable Analysis (Single + Two Variables in one page)
      tabItem(
        tabName = "varanalysis",
        fluidRow(
          box(
            title = "Variable Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            fluidRow(
              column(4,
                selectInput("analysisMode", "Number of Variables to Analyze:",
                            choices = c("1 Variable" = "single",
                                        "Multiple Variables (2+)" = "multi"),
                            selected = "single")
              )
            ),

            conditionalPanel(
              condition = "input.analysisMode == 'single'",
              fluidRow(
                column(4,
                  selectInput("singleVarType", "Variable Type:",
                              choices = c("Numerical", "Categorical"))
                ),
                column(4,
                  uiOutput("singleVarSelect")
                ),
                column(4,
                  br(),
                  actionButton("runSingleAnalysis", "Run Analysis",
                               class = "btn btn-primary")
                )
              )
            ),

            conditionalPanel(
              condition = "input.analysisMode == 'multi'",
              fluidRow(
                column(4,
                  selectInput("multiVarCount", "How many variables?",
                              choices = as.character(2:6),
                              selected = "2")
                )
              ),
              uiOutput("multiVarRows"),
              fluidRow(
                column(12,
                  actionButton("runMultiAnalysis", "Run Analysis",
                               class = "btn btn-primary")
                )
              )
            )
          )
        ),

        conditionalPanel(
          condition = "input.analysisMode == 'single'",
          fluidRow(
            box(
              title = "Descriptive Statistics",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              verbatimTextOutput("singleStats")
            ),
            box(
              title = "Visualization",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              conditionalPanel(
                condition = "input.interactivePlots",
                spn(plotly_output_safe("singlePlotly", height = "400px"))
              ),
              conditionalPanel(
                condition = "!input.interactivePlots",
                spn(plotOutput("singlePlot", height = "400px"))
              )
            )
          )
        ),

        conditionalPanel(
          condition = "input.analysisMode == 'multi'",
          fluidRow(
            box(
              title = "Analysis Results",
              status = "success",
              solidHeader = TRUE,
              width = 6,
              verbatimTextOutput("twoVarStats")
            ),
            box(
              title = "Visualization",
              status = "success",
              solidHeader = TRUE,
              width = 6,
              conditionalPanel(
                condition = "input.interactivePlots",
                spn(plotly_output_safe("twoVarPlotly", height = "400px"))
              ),
              conditionalPanel(
                condition = "!input.interactivePlots",
                spn(plotOutput("twoVarPlot", height = "400px"))
              )
            )
          )
        )
      ),
      
      # Correlation Matrix
      tabItem(
        tabName = "correlation",
        fluidRow(
          box(
            title = "Correlation Matrix Analysis",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(8,
                uiOutput("corrVarsSelect")
              ),
              column(4,
                br(),
                actionButton("runCorrAnalysis", "Calculate Correlation", 
                           class = "btn btn-primary")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Correlation Matrix",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("corrMatrix")
          ),
          box(
            title = "Correlation Heatmap",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            spn(plotOutput("corrHeatmap", height = "400px"))
          )
        ),
        
        fluidRow(
          box(
            title = "Scatter Plot Matrix",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            spn(plotOutput("corrPairs", height = "500px"))
          )
        )
      ),
      
      # Categorical Suite (Single + Cross-Tab + Multi-Categorical)
      tabItem(
        tabName = "catsuite",
        fluidRow(
          box(
            title = "Categorical Suite",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(5,
                selectInput("catSuiteMode", "Analysis Type:",
                            choices = c("Single Categorical Variable" = "singlecat",
                                        "Cross-Tabulation (2 Variables)" = "crosstab",
                                        "Multi-Categorical (3+ Variables)" = "multicat"),
                            selected = "singlecat")
              )
            )
          )
        ),

        conditionalPanel(
          condition = "input.catSuiteMode == 'singlecat'",
          fluidRow(
            box(
              title = "Categorical Variable Analysis",
              status = "danger",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(4, uiOutput("catVarSelect")),
                column(4,
                  selectInput("catPlotType", "Plot Type:",
                              choices = c("Bar Plot", "Pie Chart", "Both"))
                ),
                column(4,
                  br(),
                  actionButton("runCatAnalysis", "Run Analysis", class = "btn btn-primary")
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Frequency Distribution",
              status = "danger",
              solidHeader = TRUE,
              width = 6,
              h5("Frequency Table:"),
              tableOutput("catFrequency"),
              hr(),
              h5("Proportion Table:"),
              tableOutput("catProportion")
            ),
            box(
              title = "Visualization",
              status = "danger",
              solidHeader = TRUE,
              width = 6,
              conditionalPanel(
                condition = "input.interactivePlots",
                spn(plotly_output_safe("catPlotly", height = "450px"))
              ),
              conditionalPanel(
                condition = "!input.interactivePlots",
                spn(plotOutput("catPlot", height = "450px"))
              )
            )
          )
        ),

        conditionalPanel(
          condition = "input.catSuiteMode == 'crosstab'",
          fluidRow(
            box(
              title = "Cross-Tabulation Analysis",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(4, uiOutput("crosstabVar1Select")),
                column(4, uiOutput("crosstabVar2Select")),
                column(4,
                  br(),
                  actionButton("runCrosstabAnalysis", "Run Analysis", class = "btn btn-primary")
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Contingency Table",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              tableOutput("crosstabTable")
            ),
            box(
              title = "Joint Proportions",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              tableOutput("crosstabProp")
            )
          ),
          fluidRow(
            box(
              title = "Chi-Square Test",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              verbatimTextOutput("chiSquareTest")
            ),
            box(
              title = "Visualization",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              selectInput("crosstabPlotType", "Plot Type:",
                         choices = c("Stacked Bar", "Grouped Bar", "Mosaic Plot")),
              spn(plotOutput("crosstabPlot", height = "350px"))
            )
          )
        ),

        conditionalPanel(
          condition = "input.catSuiteMode == 'multicat'",
          fluidRow(
            box(
              title = tagList(icon("cubes"), " Multi-Categorical Variable Analysis"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              h4("Analyze relationships among 3 or more categorical variables"),
              fluidRow(
                column(3, uiOutput("multiCatVar1Select")),
                column(3, uiOutput("multiCatVar2Select")),
                column(3, uiOutput("multiCatVar3Select")),
                column(3, uiOutput("multiCatVar4Select"))
              ),
              fluidRow(
                column(12,
                  br(),
                  actionButton("runMultiCatAnalysis", "Run Multi-Way Analysis",
                               class = "btn btn-warning btn-lg",
                               icon = icon("chart-bar"))
                )
              )
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("table"), " Multi-Way Contingency Table"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              h4("Frequency Table"),
              verbatimTextOutput("multiCatTable"),
              hr(),
              h4("Conditional Proportions"),
              verbatimTextOutput("multiCatProportions")
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("calculator"), " Multi-Way Chi-Square Test"),
              status = "success",
              solidHeader = TRUE,
              width = 6,
              verbatimTextOutput("multiCatChiSquare")
            ),
            box(
              title = tagList(icon("info-circle"), " Association Measures"),
              status = "success",
              solidHeader = TRUE,
              width = 6,
              verbatimTextOutput("multiCatAssociation")
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("chart-bar"), " Multi-Way Visualization"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(4,
                  selectInput("multiCatPlotType", "Visualization Type:",
                             choices = c("Grouped Bar Chart",
                                         "Faceted Bar Chart",
                                         "Mosaic Plot",
                                         "Heatmap"))
                ),
                column(8, uiOutput("multiCatPlotOptions"))
              ),
              spn(plotOutput("multiCatPlot", height = "500px"))
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("lightbulb"), " Analysis Insights"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              htmlOutput("multiCatInsights")
            )
          )
        )
      ),
      
      # Hypothesis Tests
      tabItem(
        tabName = "tests",
        fluidRow(
          box(
            title = "Hypothesis Testing",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            selectInput("testType", "Select Test Type:",
                       choices = c(
                         "One-Sample T-Test",
                         "Two-Sample T-Test",
                         "Paired T-Test",
                         "One-Way ANOVA (F-Test)",
                         "One-Sample Wilcoxon Test",
                         "Two-Sample Wilcoxon Test",
                         "Normality Test (Shapiro-Wilk)"
                       )),
            
            uiOutput("testVarInputs"),
            
            hr(),
            
            actionButton("runHypothesisTest", "Run Test", 
                       class = "btn btn-primary")
          )
        ),
        
        fluidRow(
          box(
            title = "Test Results",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            verbatimTextOutput("testResults")
          ),
          box(
            title = "Visualization",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            plotOutput("testPlot", height = "300px")
          )
        )
      ),
      
      # Summary Report - REDESIGNED FOR ENGAGEMENT
      tabItem(
        tabName = "report",
        
        # Header with download button
        fluidRow(
          box(
            title = tagList(icon("chart-line"), " Data Summary Dashboard"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(10,
                h4("Explore your dataset with interactive visualizations and insights")
              ),
              column(2,
                downloadButton("downloadReport", "Download Report", 
                             class = "btn-success btn-block",
                             icon = icon("download"))
              )
            )
          )
        ),
        
        # Key Metrics Info Boxes
        fluidRow(
          infoBoxOutput("summaryRows", width = 3),
          infoBoxOutput("summaryColumns", width = 3),
          infoBoxOutput("summaryNumeric", width = 3),
          infoBoxOutput("summaryCategorical", width = 3)
        ),
        
        # Dataset Overview Cards
        fluidRow(
          box(
            title = tagList(icon("table"), " Dataset Structure"),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            height = "400px",
            
            DTOutput("variableTable")
          ),
          
          box(
            title = tagList(icon("chart-pie"), " Variable Type Distribution"),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            height = "400px",
            
            plotOutput("variableTypePlot", height = "320px")
          )
        ),
        
        # Statistical Summary
        fluidRow(
          box(
            title = tagList(icon("calculator"), " Numerical Variables Summary"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            DTOutput("numericSummaryTable")
          )
        ),
        
        # Data Quality Section
        fluidRow(
          box(
            title = tagList(icon("exclamation-triangle"), " Data Quality Report"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            h4("Missing Values Analysis"),
            plotOutput("missingPlot", height = "300px"),
            hr(),
            verbatimTextOutput("missingStats")
          ),
          
          box(
            title = tagList(icon("chart-bar"), " Data Completeness"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput("completenessPlot", height = "300px"),
            hr(),
            uiOutput("dataQualityScore")
          )
        ),
        
        # Distribution Insights
        fluidRow(
          box(
            title = tagList(icon("chart-area"), " Distribution Overview"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            
            uiOutput("distributionSelect"),
            plotOutput("distributionPlot", height = "400px")
          )
        ),
        
        # Advanced Insights
        fluidRow(
          box(
            title = tagList(icon("lightbulb"), " Key Insights & Recommendations"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            htmlOutput("dataInsights")
          )
        )
      ),

      # ========================================================
      # SIMPLE LINEAR REGRESSION TAB  — Redesigned
      # ========================================================
      tabItem(
        tabName = "slr",

        # ── Step 1: Setup ──────────────────────────────────────
        tags$div(class = "reg-step-banner",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Model Setup"),
            tags$p("Select your response (Y) and predictor (X) variable, then click Fit Model.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",

            fluidRow(
              column(4,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("slrYSelect")
              ),
              column(4,
                tags$div(class = "section-label", "Predictor Variable (X)"),
                uiOutput("slrXSelect")
              ),
              column(4,
                tags$div(class = "section-label", "Options"),
                checkboxInput("slrNoIntercept",
                  tagList(icon("minus-circle"), " Force intercept through origin (β₀ = 0)"),
                  value = FALSE),
                br(),
                actionButton("runSLR", tagList(icon("play"), " Fit SLR Model"),
                             class = "fit-btn")
              )
            )
          )
        ),

        # ── Step 2: Exploratory Plot ───────────────────────────
        tags$div(class = "reg-step-banner info",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Exploratory Plot"),
            tags$p("Scatter plot with the fitted regression line.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = NULL, solidHeader = FALSE,
            status = "info", width = 12,
            plotOutput("slrScatter", height = "380px")
          )
        ),

        # ── Step 3: Model Results (tabbed) ────────────────────
        tags$div(class = "reg-step-banner success",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Model Results"),
            tags$p("Summary, ANOVA, confidence intervals, and correlation — use the tabs below.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = NULL, solidHeader = FALSE,
            status = "success", width = 12,

            tabsetPanel(
              type = "tabs",

              tabPanel(
                tagList(icon("table"), " Model Summary"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Shows fitted coefficients, standard errors, t-statistics, p-values, R², and adjusted R²."
                ),
                verbatimTextOutput("slrSummary")
              ),

              tabPanel(
                tagList(icon("calculator"), " ANOVA Table"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Tests overall model significance (F-test). A small p-value means the model explains\n                  a significant portion of the variation in Y."
                ),
                verbatimTextOutput("slrAnova")
              ),

              tabPanel(
                tagList(icon("arrows-alt-h"), " Confidence Intervals"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " 95% confidence intervals for the regression coefficients (β₀ and β₁)."
                ),
                verbatimTextOutput("slrConfint")
              ),

              tabPanel(
                tagList(icon("link"), " Correlation Tests"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Pearson (parametric), Spearman, and Kendall correlation tests between X and Y."
                ),
                verbatimTextOutput("slrCorrelation")
              )
            )
          )
        ),

        # ── Step 4: Prediction ────────────────────────────────
        tags$div(class = "reg-step-banner warning",
          tags$div(class = "step-badge", "4"),
          tags$div(
            tags$h4("Prediction"),
            tags$p("Enter new X value(s) to get predicted Y values with intervals.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = NULL, solidHeader = FALSE,
            status = "warning", width = 12,
            collapsible = TRUE, collapsed = FALSE,

            fluidRow(
              column(8,
                tags$div(class = "section-label", "New X Value(s)"),
                textInput("slrNewX", label = NULL,
                          placeholder = "Enter values separated by commas, e.g.  20, 30"),
                tags$div(class = "hint-text",
                  icon("info-circle"), " Each value produces one row of output."
                )
              ),
              column(4,
                br(),
                actionButton("runSLRPredict",
                             tagList(icon("magic"), " Predict"),
                             class = "predict-btn")
              )
            ),

            hr(style = "margin: 16px 0;"),

            tabsetPanel(
              type = "pills",

              tabPanel(
                tagList(icon("expand-arrows-alt"), " Prediction Interval"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Wider interval — covers where a single new observation is likely to fall."
                ),
                DT::DTOutput("slrPredInterval")
              ),

              tabPanel(
                tagList(icon("compress-arrows-alt"), " Confidence Interval"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Narrower interval — estimates the mean response at the given X value."
                ),
                DT::DTOutput("slrConfInterval")
              ),

              tabPanel(
                tagList(icon("search-location"), " Interpolation / Extrapolation"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Checks whether each new X falls inside (interpolation) or outside\n                  (EXTRAPOLATION) the observed data range. Extrapolation predictions are unreliable."
                ),
                verbatimTextOutput("slrInterpExtrap")
              )
            )
          )
        )
      ),

      # ========================================================
      # MULTIPLE LINEAR REGRESSION TAB  — Redesigned
      # ========================================================
      tabItem(
        tabName = "mlr",

        # ── Step 1: Setup ──────────────────────────────────────
        tags$div(class = "reg-step-banner",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Model Setup"),
            tags$p("Choose the response (Y) and two or more predictor variables (X), then click Fit Model.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",

            fluidRow(
              column(3,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("mlrYSelect")
              ),
              column(6,
                tags$div(class = "section-label", "Predictor Variables (X) — select 2 or more"),
                uiOutput("mlrXSelect"),
                tags$div(class = "hint-text",
                  icon("info-circle"), " Hold Ctrl / Cmd to select multiple variables."
                )
              ),
              column(3,
                tags$div(class = "section-label", "Options"),
                checkboxInput("mlrNoIntercept",
                  tagList(icon("minus-circle"), " Force intercept through origin"),
                  value = FALSE),
                br(),
                actionButton("runMLR", tagList(icon("play"), " Fit MLR Model"),
                             class = "fit-btn")
              )
            )
          )
        ),

        # ── Step 2: Exploratory Plots ──────────────────────────
        tags$div(class = "reg-step-banner info",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Exploratory Plots"),
            tags$p("Scatter plot matrix and mixed correlation matrix for all selected variables.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("th"), " Scatter Plot Matrix"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("mlrPairs", height = "370px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("border-all"), " Correlation Matrix"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("mlrCorrPlot", height = "370px")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Observed vs Predicted (Fit Line)"),
            solidHeader = TRUE, status = "info", width = 12,
            plotOutput("mlrFitLine", height = "320px")
          )
        ),

        # ── Step 3: Model Results (tabbed) ────────────────────
        tags$div(class = "reg-step-banner success",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Model Results"),
            tags$p("Summary, ANOVA, partial F-tests, and confidence intervals — use the tabs below.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = NULL, solidHeader = FALSE,
            status = "success", width = 12,

            tabsetPanel(
              type = "tabs",

              tabPanel(
                tagList(icon("table"), " Model Summary"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Displays fitted coefficients, standard errors, t-tests for each predictor,\n                  overall F-test, R², and adjusted R²."
                ),
                verbatimTextOutput("mlrSummary")
              ),

              tabPanel(
                tagList(icon("calculator"), " ANOVA Table"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Sequential (Type I) ANOVA — shows the contribution of each predictor\n                  added in order."
                ),
                verbatimTextOutput("mlrAnova")
              ),

              tabPanel(
                tagList(icon("vial"), " Partial F-Tests"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Tests H₀: βᵢ = 0 for each predictor by comparing the full model against\n                  a reduced model without that predictor."
                ),
                verbatimTextOutput("mlrPartialF")
              ),

              tabPanel(
                tagList(icon("arrows-alt-h"), " Confidence Intervals"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " 95% confidence intervals for all regression coefficients."
                ),
                verbatimTextOutput("mlrConfint")
              )
            )
          )
        ),

        # ── Step 4: Diagnostics — VIF ─────────────────────────
        tags$div(
          style = "background: linear-gradient(135deg,#c62828,#b71c1c); color:#fff;
                   border-radius:8px; padding:14px 20px; margin-bottom:18px;
                   display:flex; align-items:center; gap:14px;
                   box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class = "step-badge", "4"),
          tags$div(
            tags$h4(style="margin:0;font-size:16px;font-weight:600;",
                    "Multicollinearity Diagnostics — VIF"),
            tags$p(style="margin:2px 0 0;font-size:12px;opacity:.85;",
                   "VIF < 5: acceptable  |  5–10: moderate concern  |  > 10: severe — consider removing predictors.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("list-ol"), " VIF Values"),
            solidHeader = TRUE, status = "danger", width = 4,
            verbatimTextOutput("mlrVIF")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-bar"), " VIF Chart"),
            solidHeader = TRUE, status = "danger", width = 8,
            plotOutput("mlrVIFPlot", height = "280px")
          )
        ),

        # ── Step 5: Prediction ────────────────────────────────
        tags$div(class = "reg-step-banner warning",
          tags$div(class = "step-badge", "5"),
          tags$div(
            tags$h4("Prediction"),
            tags$p("Enter values for each predictor to obtain fitted values and prediction/confidence intervals.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = NULL, solidHeader = FALSE,
            status = "warning", width = 12,
            collapsible = TRUE, collapsed = FALSE,

            uiOutput("mlrNewXInputs"),

            fluidRow(
              column(12, style = "margin-top:8px;",
                tags$div(class = "hint-text",
                  icon("info-circle"),
                  " Enter one or more comma-separated values per predictor.
                    Each position across all fields corresponds to one new observation."
                )
              )
            ),

            br(),
            actionButton("runMLRPredict",
                         tagList(icon("magic"), " Predict"),
                         class = "predict-btn"),

            hr(style = "margin:16px 0;"),

            tabsetPanel(
              type = "pills",

              tabPanel(
                tagList(icon("expand-arrows-alt"), " Prediction Interval"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Wider interval — covers where a new individual observation is likely to fall."
                ),
                DT::DTOutput("mlrPredInterval")
              ),

              tabPanel(
                tagList(icon("compress-arrows-alt"), " Confidence Interval"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Narrower interval — estimates the mean response at the given predictor values."
                ),
                DT::DTOutput("mlrConfInterval")
              ),

              tabPanel(
                tagList(icon("search-location"), " Interpolation / Extrapolation"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Uses leverage values (hᵢᵢ) to detect whether new observations fall\n                  inside (interpolation) or outside (EXTRAPOLATION) the predictor space."
                ),
                verbatimTextOutput("mlrInterpExtrap")
              )
            )
          )
        )
      ),

      # ========================================================
      # INDICATOR VARIABLE TAB
      # ========================================================
      tabItem(
        tabName = "indicator",

        tags$div(class = "reg-step-banner",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Indicator (Dummy) Variable Setup"),
            tags$p("Choose Y, one or two qualitative factors, and optionally a numeric X. The app uses k-1 dummy variables per factor.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",

            fluidRow(
              column(4,
                tags$div(class = "section-label", "Number of Indicator Factors"),
                selectInput("indFactorCount", NULL,
                            choices = c("1 Factor" = "1", "2 Factors" = "2"),
                            selected = "1")
              ),
              column(4,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("indYSelect")
              ),
              column(4)
            ),

            fluidRow(
              column(6,
                tags$div(class = "section-label", "Categorical Variable 1"),
                uiOutput("indFactorSelect")
              ),
              column(6,
                tags$div(class = "section-label", "Reference Level 1"),
                uiOutput("indRefSelect")
              )
            ),

            conditionalPanel(
              condition = "input.indFactorCount == '2'",
              fluidRow(
                column(6,
                  tags$div(class = "section-label", "Categorical Variable 2"),
                  uiOutput("indFactor2Select")
                ),
                column(6,
                  tags$div(class = "section-label", "Reference Level 2"),
                  uiOutput("indRef2Select")
                )
              )
            ),

            fluidRow(
              column(6,
                tags$div(class = "section-label", "Numeric Predictor (Optional)"),
                uiOutput("indXSelect")
              ),
              column(6)
            ),

            fluidRow(
              column(8,
                checkboxInput("indInteraction",
                  tagList(icon("random"), " Include interaction with X (non-parallel slopes)"),
                  value = FALSE),
                tags$div(class = "hint-text",
                  icon("info-circle"),
                  " For 1 factor: X×Factor. For 2 factors: X×Factor1 and X×Factor2."
                )
              ),
              column(4,
                br(),
                actionButton("runIndicator",
                             tagList(icon("play"), " Fit Indicator Model"),
                             class = "fit-btn")
              )
            )
          )
        ),

        tags$div(class = "reg-step-banner info",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Model Outputs"),
            tags$p("See baseline coding, fitted model summary, ANOVA, and slide-style equation interpretation.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("info-circle"), " Model Setup"),
            solidHeader = TRUE, status = "info", width = 4,
            verbatimTextOutput("indModelInfo")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Model Summary"),
            solidHeader = TRUE, status = "info", width = 8,
            verbatimTextOutput("indSummary")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("calculator"), " ANOVA and Extra Sum-of-Squares Test"),
            solidHeader = TRUE, status = "success", width = 6,
            verbatimTextOutput("indAnova")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("superscript"), " Group-Specific Equations"),
            solidHeader = TRUE, status = "success", width = 6,
            verbatimTextOutput("indEquations")
          )
        ),

        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Visualization"),
            solidHeader = TRUE, status = "warning", width = 8,
            plotOutput("indPlot", height = "390px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("lightbulb"), " Interpretation"),
            solidHeader = TRUE, status = "warning", width = 4,
            htmlOutput("indInterpretation")
          )
        )
      ),

      # ============================================================
      # MODEL ADEQUACY TAB
      # ============================================================
      tabItem(
        tabName = "modeladequacy",

        tags$div(class = "reg-step-banner",
          tags$div(class = "step-badge", icon("stethoscope")),
          tags$div(
            tags$h4("Model Adequacy Checks"),
            tags$p("Select a fitted regression model (SLR or MLR) and click Run Diagnostics.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card", status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(4,
                tags$div(class = "section-label", "Model Type"),
                selectInput("maModelType", NULL,
                  choices = c("Simple Linear Regression (SLR)" = "slr",
                              "Multiple Linear Regression (MLR)" = "mlr"))
              ),
              column(4,
                tags$div(class = "section-label", "Predictor(s) hint"),
                uiOutput("maXHint")
              ),
              column(4,
                br(),
                actionButton("runMA", tagList(icon("play"), " Run Diagnostics"),
                             class = "fit-btn")
              )
            )
          )
        ),

        # Residuals table
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Residual Values"),
            solidHeader = TRUE, status = "info", width = 12,
            collapsible = TRUE, collapsed = TRUE,
            DT::DTOutput("maResidTable")
          )
        ),

        # Normality
        tags$div(class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Normality of Residuals"),
            tags$p("QQ-plot (informal) and Shapiro-Wilk test (formal).")
          )
        ),
        fluidRow(
          box(class="reg-card", title = tagList(icon("chart-line")," QQ Plots"),
              solidHeader=TRUE, status="info", width=8,
              plotOutput("maQQPlot", height="320px")),
          box(class="reg-card", title = tagList(icon("calculator")," Shapiro-Wilk Test"),
              solidHeader=TRUE, status="info", width=4,
              verbatimTextOutput("maShapiro"))
        ),

        # Homoscedasticity
        tags$div(class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Constant Variance (Homoscedasticity)"),
            tags$p("Residuals vs Fitted plot (informal) and Breusch-Pagan test (formal).")
          )
        ),
        fluidRow(
          box(class="reg-card", title = tagList(icon("chart-line")," Residuals vs Fitted"),
              solidHeader=TRUE, status="success", width=8,
              plotOutput("maResFitted", height="320px")),
          box(class="reg-card", title = tagList(icon("calculator")," Breusch-Pagan Test"),
              solidHeader=TRUE, status="success", width=4,
              verbatimTextOutput("maBPTest"))
        ),

        # Independence
        tags$div(class = "reg-step-banner warning",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Independence of Residuals"),
            tags$p("Residuals vs Observation Order (informal) and Durbin-Watson test (formal).")
          )
        ),
        fluidRow(
          box(class="reg-card", title = tagList(icon("chart-line")," Residuals vs Order"),
              solidHeader=TRUE, status="warning", width=8,
              plotOutput("maResOrder", height="320px")),
          box(class="reg-card", title = tagList(icon("calculator")," Durbin-Watson Test"),
              solidHeader=TRUE, status="warning", width=4,
              verbatimTextOutput("maDWTest"))
        ),

        # Added-variable plots
        tags$div(
          style="background:linear-gradient(135deg,#4a148c,#311b92);color:#fff;
                 border-radius:8px;padding:14px 20px;margin-bottom:18px;
                 display:flex;align-items:center;gap:14px;
                 box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class="step-badge", "4"),
          tags$div(
            tags$h4(style="margin:0;font-size:16px;font-weight:600;",
                    "Added-Variable (Partial Regression) Plots"),
            tags$p(style="margin:2px 0 0;font-size:12px;opacity:.85;",
                   "Shows unique contribution of each predictor after accounting for the others.")
          )
        ),
        fluidRow(
          box(class="reg-card", title = tagList(icon("chart-bar")," Added-Variable Plots"),
              solidHeader=TRUE, status="danger", width=12,
              plotOutput("maAVPlots", height="420px"))
        ),

        # Lack-of-fit (SLR only)
        tags$div(class="reg-step-banner",
          tags$div(class="step-badge", "5"),
          tags$div(
            tags$h4("Lack-of-Fit Test"),
            tags$p("ANOVA pure-error lack-of-fit test (requires replicate X values and EnvStats package).")
          )
        ),
        fluidRow(
          box(class="reg-card", title = tagList(icon("vial")," Lack-of-Fit Results"),
              solidHeader=TRUE, status="primary", width=12,
              verbatimTextOutput("maLackFit"))
        )
      ),

      # ============================================================
      # BOX-COX & BOX-TIDWELL TAB
      # ============================================================
      tabItem(
        tabName = "boxtrans",

        tags$div(class = "reg-step-banner",
          tags$div(class = "step-badge", icon("magic")),
          tags$div(
            tags$h4("Box-Cox & Box-Tidwell Transformations"),
            tags$p("Select Y and a single X variable, then choose the transformation to apply.")
          )
        ),

        fluidRow(
          box(
            class = "reg-card", status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(3,
                tags$div(class="section-label","Response Variable (Y)"),
                uiOutput("btYSelect")
              ),
              column(3,
                tags$div(class="section-label","Predictor Variable (X)"),
                uiOutput("btXSelect")
              ),
              column(3,
                tags$div(class="section-label","Transformation"),
                selectInput("btMethod", NULL,
                  choices = c("Box-Cox (transform Y)" = "boxcox",
                              "Box-Tidwell (transform X)" = "boxtidwell"))
              ),
              column(3,
                br(),
                actionButton("runBT", tagList(icon("play")," Run Transformation"),
                             class = "fit-btn")
              )
            )
          )
        ),

        # Step 1: Original model
        tags$div(class="reg-step-banner info",
          tags$div(class="step-badge","1"),
          tags$div(
            tags$h4("Original OLS Model"),
            tags$p("Scatter plot, summary, and residuals vs fitted for the untransformed data.")
          )
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," Scatter + Fit"),
              solidHeader=TRUE, status="info", width=6,
              plotOutput("btOrigScatter", height="280px")),
          box(class="reg-card", title=tagList(icon("calculator")," OLS Summary"),
              solidHeader=TRUE, status="info", width=6,
              verbatimTextOutput("btOrigSummary"))
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," Residuals vs Fitted (Original)"),
              solidHeader=TRUE, status="info", width=6,
              plotOutput("btOrigResid", height="250px")),
          box(class="reg-card", title=tagList(icon("calculator")," Breusch-Pagan (Original)"),
              solidHeader=TRUE, status="info", width=6,
              verbatimTextOutput("btOrigBP"))
        ),

        # Step 2: Transformation details
        tags$div(class="reg-step-banner success",
          tags$div(class="step-badge","2"),
          tags$div(
            tags$h4("Optimal Transformation"),
            tags$p("Box-Cox lambda / Box-Tidwell alpha profile and optimal value.")
          )
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("search")," Transformation Profile"),
              solidHeader=TRUE, status="success", width=7,
              plotOutput("btLambdaPlot", height="300px")),
          box(class="reg-card", title=tagList(icon("info-circle")," Optimal Parameter"),
              solidHeader=TRUE, status="success", width=5,
              verbatimTextOutput("btOptimal"))
        ),

        # Step 3: Transformed model
        tags$div(class="reg-step-banner warning",
          tags$div(class="step-badge","3"),
          tags$div(
            tags$h4("Transformed Model"),
            tags$p("Scatter plot and model results using the optimal transformation.")
          )
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," Scatter + Fit (Transformed)"),
              solidHeader=TRUE, status="warning", width=6,
              plotOutput("btTransScatter", height="280px")),
          box(class="reg-card", title=tagList(icon("calculator")," Transformed OLS Summary"),
              solidHeader=TRUE, status="warning", width=6,
              verbatimTextOutput("btTransSummary"))
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," Residuals vs Fitted (Transformed)"),
              solidHeader=TRUE, status="warning", width=6,
              plotOutput("btTransResid", height="250px")),
          box(class="reg-card", title=tagList(icon("calculator")," Breusch-Pagan (Transformed)"),
              solidHeader=TRUE, status="warning", width=6,
              verbatimTextOutput("btTransBP"))
        ),

        # Step 4: Side-by-side comparison
        tags$div(
          style="background:linear-gradient(135deg,#1a237e,#283593);color:#fff;
                 border-radius:8px;padding:14px 20px;margin-bottom:18px;
                 display:flex;align-items:center;gap:14px;
                 box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class="step-badge","4"),
          tags$div(
            tags$h4(style="margin:0;font-size:16px;font-weight:600;",
                    "Side-by-Side Residual Comparison"),
            tags$p(style="margin:2px 0 0;font-size:12px;opacity:.85;",
                   "Compare residual plots before and after transformation.")
          )
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("balance-scale")," Residual Comparison"),
              solidHeader=TRUE, status="danger", width=12,
              plotOutput("btComparison", height="320px"))
        )
      ),

      # ============================================================
      # WEIGHTED LEAST SQUARES TAB
      # ============================================================
      tabItem(
        tabName = "wls",

        tags$div(class = "reg-step-banner",
          tags$div(class = "step-badge", icon("balance-scale")),
          tags$div(
            tags$h4("Weighted Least Squares (WLS)"),
            tags$p("Select Y, X, and a weight variable (or let the app estimate weights automatically).")
          )
        ),

        fluidRow(
          box(
            class = "reg-card", status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(3,
                tags$div(class="section-label","Response Variable (Y)"),
                uiOutput("wlsYSelect")
              ),
              column(3,
                tags$div(class="section-label","Predictor Variable (X)"),
                uiOutput("wlsXSelect")
              ),
              column(3,
                tags$div(class="section-label","Weight Column (optional)"),
                uiOutput("wlsWSelect"),
                tags$div(class="hint-text",
                  icon("info-circle"),
                  " If no weight column, weights are estimated as 1/|residual| from OLS.")
              ),
              column(3,
                br(),
                actionButton("runWLS", tagList(icon("play")," Fit WLS Model"),
                             class = "fit-btn")
              )
            )
          )
        ),

        # Step 1 OLS
        tags$div(class="reg-step-banner info",
          tags$div(class="step-badge","1"),
          tags$div(
            tags$h4("OLS Baseline Model"),
            tags$p("Fit ordinary least squares first to diagnose heteroscedasticity.")
          )
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," OLS Scatter + Fit"),
              solidHeader=TRUE, status="info", width=6,
              plotOutput("wlsOLSScatter", height="280px")),
          box(class="reg-card", title=tagList(icon("calculator")," OLS Summary & BP Test"),
              solidHeader=TRUE, status="info", width=6,
              verbatimTextOutput("wlsOLSSummary"))
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," OLS Residuals vs Fitted"),
              solidHeader=TRUE, status="info", width=12,
              plotOutput("wlsOLSResid", height="280px"))
        ),

        # Step 2 WLS
        tags$div(class="reg-step-banner success",
          tags$div(class="step-badge","2"),
          tags$div(
            tags$h4("WLS Model"),
            tags$p("Weighted least squares results and standardised residual plot.")
          )
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," WLS Scatter + Fit"),
              solidHeader=TRUE, status="success", width=6,
              plotOutput("wlsWLSScatter", height="280px")),
          box(class="reg-card", title=tagList(icon("calculator")," WLS Summary & BP Test"),
              solidHeader=TRUE, status="success", width=6,
              verbatimTextOutput("wlsWLSSummary"))
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("chart-line")," Weighted Residuals vs Weighted Fitted"),
              solidHeader=TRUE, status="success", width=12,
              plotOutput("wlsWLSResid", height="280px"))
        ),

        # Step 3 Comparison
        tags$div(
          style="background:linear-gradient(135deg,#1b5e20,#2e7d32);color:#fff;
                 border-radius:8px;padding:14px 20px;margin-bottom:18px;
                 display:flex;align-items:center;gap:14px;
                 box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class="step-badge","3"),
          tags$div(
            tags$h4(style="margin:0;font-size:16px;font-weight:600;",
                    "OLS vs WLS Coefficient Comparison"),
            tags$p(style="margin:2px 0 0;font-size:12px;opacity:.85;",
                   "Side-by-side residual plots and coefficient table.")
          )
        ),
        fluidRow(
          box(class="reg-card", title=tagList(icon("balance-scale")," Residual Comparison"),
              solidHeader=TRUE, status="danger", width=8,
              plotOutput("wlsComparison", height="300px")),
          box(class="reg-card", title=tagList(icon("table")," Coefficient Comparison"),
              solidHeader=TRUE, status="danger", width=4,
              verbatimTextOutput("wlsCoefComp"))
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Increase maximum file upload size to 50MB (default is 5MB)
  options(shiny.maxRequestSize = 50 * 1024^2)

  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
  }

  # Remove rows containing outliers in any numeric variable (IQR rule)
  remove_outliers_iqr <- function(df, k = 1.5) {
    if (is.null(df) || nrow(df) == 0) return(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) return(df)

    keep <- rep(TRUE, nrow(df))
    for (col in num_cols) {
      x <- df[[col]]
      q1 <- suppressWarnings(quantile(x, 0.25, na.rm = TRUE))
      q3 <- suppressWarnings(quantile(x, 0.75, na.rm = TRUE))
      iqr <- q3 - q1
      if (!is.finite(iqr) || iqr == 0) next
      low <- q1 - k * iqr
      high <- q3 + k * iqr
      ok <- is.na(x) | (x >= low & x <= high)
      keep <- keep & ok
    }
    df[keep, , drop = FALSE]
  }

  impute_missing_median_mode <- function(df) {
    for (nm in names(df)) {
      x <- df[[nm]]
      if (is.numeric(x)) {
        med <- suppressWarnings(median(x, na.rm = TRUE))
        if (!is.finite(med)) med <- 0
        x[is.na(x)] <- med
      } else {
        x_chr <- as.character(x)
        tab <- sort(table(x_chr), decreasing = TRUE)
        mode_val <- if (length(tab) > 0) names(tab)[1] else "Unknown"
        x_chr[is.na(x_chr) | trimws(x_chr) == ""] <- mode_val
        x <- if (is.factor(x)) factor(x_chr) else x_chr
      }
      df[[nm]] <- x
    }
    df
  }

  preprocess_cars <- function(df) {
    if (is.null(df)) return(df)
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

    inr_to_sar <- 0.0404
    if ("selling_price" %in% names(df)) {
      price_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(df$selling_price))))
      df$selling_price_sar_1000 <- round((price_num * inr_to_sar) / 1000, 3)
      df$selling_price <- NULL
    }

    if ("name" %in% names(df)) {
      brand_name <- trimws(gsub("\\s+.*$", "", as.character(df$name)))
      brand_name[is.na(brand_name) | brand_name == ""] <- "Unknown"
      df$brand <- brand_name
      df$name <- NULL
    }
    df
  }

  apply_cleaning_pipeline <- function(df, missing_mode = "keep", outlier_mode = "remove", log_vars = NULL) {
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

    if (identical(missing_mode, "drop")) {
      df <- df[complete.cases(df), , drop = FALSE]
    } else if (identical(missing_mode, "impute")) {
      df <- impute_missing_median_mode(df)
    }

    if (identical(outlier_mode, "remove")) {
      df <- remove_outliers_iqr(df)
    }

    if (!is.null(log_vars) && length(log_vars) > 0) {
      log_vars <- intersect(log_vars, names(df)[sapply(df, is.numeric)])
      for (nm in log_vars) {
        df[[nm]] <- suppressWarnings(log1p(pmax(df[[nm]], 0)))
      }
    }
    df
  }

  refresh_var_types <- function(df) {
    numeric_cols <- sapply(df, is.numeric)
    data$numeric_vars <- names(df)[numeric_cols]
    data$categorical_vars <- names(df)[!numeric_cols]
  }

  # Default dataset (loaded automatically at startup)
  default_data_path <- "d:/KFUPM/T-252/Stat 413/Data/P2_DeliveryTime.xlsx"
  default_data <- tryCatch({
    ext0 <- tolower(tools::file_ext(default_data_path))
    df_default <- if (ext0 %in% c("xlsx", "xls")) {
      as.data.frame(read_excel(default_data_path), check.names = FALSE)
    } else {
      read.csv(default_data_path, stringsAsFactors = FALSE, check.names = FALSE)
    }
    preprocess_cars(df_default)
  }, error = function(e) {
    # Safe fallback if the default file is not accessible
    fallback <- data.frame(
      fuel_efficiency_mpg = mtcars$mpg,
      engine_displacement = mtcars$disp,
      gross_horsepower = mtcars$hp,
      rear_axle_ratio = mtcars$drat,
      vehicle_weight_1000lb = mtcars$wt,
      quarter_mile_time_sec = mtcars$qsec,
      engine_cylinders = factor(mtcars$cyl,
                                levels = c(4, 6, 8),
                                labels = c("4 cylinders", "6 cylinders", "8 cylinders")),
      engine_shape = factor(mtcars$vs,
                            levels = c(0, 1),
                            labels = c("V-shaped", "Straight")),
      transmission_type = factor(mtcars$am,
                                 levels = c(0, 1),
                                 labels = c("Automatic", "Manual")),
      forward_gears = factor(mtcars$gear),
      carburetors_count = factor(mtcars$carb),
      mpg_category = cut(mtcars$mpg,
                         breaks = quantile(mtcars$mpg, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                         include.lowest = TRUE,
                         labels = c("Low", "Medium", "High"))
    )
    fallback
  })

  default_data <- apply_cleaning_pipeline(default_data, missing_mode = "keep", outlier_mode = "remove")
  
  # Reactive data storage
  data <- reactiveValues(
    raw_base = default_data,
    raw = default_data,
    analyzed = TRUE,
    numeric_vars = names(default_data)[sapply(default_data, is.numeric)],
    categorical_vars = names(default_data)[!sapply(default_data, is.numeric)]
  )
  
  # File upload handler
  observeEvent(input$datafile, {
    req(input$datafile)
    
    tryCatch({
      ext <- tools::file_ext(input$datafile$name)
      
      if(ext == "csv") {
        uploaded <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if(ext %in% c("xlsx", "xls")) {
        uploaded <- as.data.frame(read_excel(input$datafile$datapath), check.names = FALSE)
      }

      uploaded <- preprocess_cars(uploaded)
      data$raw_base <- uploaded
      before_n <- nrow(uploaded)
      data$raw <- apply_cleaning_pipeline(uploaded,
                                          missing_mode = input$cleanMissing %||% "keep",
                                          outlier_mode = input$cleanOutliers %||% "remove",
                                          log_vars = input$cleanLogVars %||% NULL)
      removed_n <- before_n - nrow(data$raw)
      refresh_var_types(data$raw)
      
      data$analyzed <- FALSE
      
      showNotification(paste("Data uploaded successfully! Variable types detected. Outliers removed:", removed_n), 
                      type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), 
                      type = "error", duration = 5)
    })
  })
  
  # Display detected variables
  output$numericVars <- renderPrint({
    req(data$numeric_vars)
    cat(paste(data$numeric_vars, collapse = "\n"))
  })
  
  output$categoricalVars <- renderPrint({
    req(data$categorical_vars)
    if(length(data$categorical_vars) > 0) {
      cat(paste(data$categorical_vars, collapse = "\n"))
    } else {
      cat("No categorical variables detected")
    }
  })
  
  # Info boxes
  output$rowCountBox <- renderInfoBox({
    infoBox(
      "Total Records",
      ifelse(is.null(data$raw), 0, nrow(data$raw)),
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$colCountBox <- renderInfoBox({
    infoBox(
      "Total Variables",
      ifelse(is.null(data$raw), 0, ncol(data$raw)),
      icon = icon("columns"),
      color = "green"
    )
  })
  
  output$statusBox <- renderInfoBox({
    infoBox(
      "Status",
      ifelse(data$analyzed, "Ready", "Upload Data"),
      icon = icon(ifelse(data$analyzed, "check-circle", "upload")),
      color = ifelse(data$analyzed, "green", "yellow")
    )
  })

  output$kpiRows <- renderInfoBox({
    infoBox("Rows", ifelse(is.null(data$raw), 0, format(nrow(data$raw), big.mark=",")),
            icon = icon("table"), color = "blue", fill = TRUE)
  })
  output$kpiCols <- renderInfoBox({
    infoBox("Columns", ifelse(is.null(data$raw), 0, ncol(data$raw)),
            icon = icon("columns"), color = "teal", fill = TRUE)
  })
  output$kpiMissing <- renderInfoBox({
    miss <- if (is.null(data$raw)) 0 else sum(is.na(data$raw))
    miss_pct <- if (is.null(data$raw)) 0 else round(100 * miss / max(1, nrow(data$raw) * ncol(data$raw)), 2)
    infoBox("Missing %", paste0(miss_pct, "%"),
            subtitle = paste("Total:", format(miss, big.mark=",")),
            icon = icon("exclamation-triangle"), color = "yellow", fill = TRUE)
  })
  output$kpiTypes <- renderInfoBox({
    infoBox("Numeric | Categorical",
            paste(length(data$numeric_vars), "|", length(data$categorical_vars)),
            icon = icon("layer-group"), color = "green", fill = TRUE)
  })
  
  # Data preview
  output$dataPreview <- renderDT({
    req(data$raw)
    datatable(data$raw,
              filter = "top",
              extensions = c("Buttons", "FixedHeader"),
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                scrollY = "450px",
                fixedHeader = TRUE,
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel")
              ))
  })
  
  # Data summary (formatted tables instead of raw R output)
  output$dataSummaryNumeric <- DT::renderDT({
    req(data$raw)
    num_vars <- names(data$raw)[sapply(data$raw, is.numeric)]
    if (length(num_vars) == 0) {
      return(DT::datatable(data.frame(Message = "No numeric variables."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    dnum <- data$raw[, num_vars, drop = FALSE]
    tbl <- data.frame(
      Variable = num_vars,
      Mean = sapply(dnum, function(x) round(mean(x, na.rm = TRUE), 3)),
      Median = sapply(dnum, function(x) round(median(x, na.rm = TRUE), 3)),
      SD = sapply(dnum, function(x) round(sd(x, na.rm = TRUE), 3)),
      Min = sapply(dnum, function(x) round(min(x, na.rm = TRUE), 3)),
      Q1 = sapply(dnum, function(x) round(as.numeric(quantile(x, 0.25, na.rm = TRUE)), 3)),
      Q3 = sapply(dnum, function(x) round(as.numeric(quantile(x, 0.75, na.rm = TRUE)), 3)),
      Max = sapply(dnum, function(x) round(max(x, na.rm = TRUE), 3)),
      Missing = sapply(dnum, function(x) sum(is.na(x)))
    )
    DT::datatable(tbl,
                  options = list(pageLength = 8, scrollX = TRUE),
                  rownames = FALSE)
  })

  output$dataSummaryCategorical <- DT::renderDT({
    req(data$raw)
    cat_vars <- names(data$raw)[!sapply(data$raw, is.numeric)]
    if (length(cat_vars) == 0) {
      return(DT::datatable(data.frame(Message = "No categorical variables."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    tbl <- data.frame(
      Variable = cat_vars,
      Unique_Levels = sapply(cat_vars, function(v) length(unique(data$raw[[v]]))),
      Most_Frequent = sapply(cat_vars, function(v) {
        x <- as.character(data$raw[[v]])
        x <- x[!is.na(x) & trimws(x) != ""]
        if (length(x) == 0) return("N/A")
        names(sort(table(x), decreasing = TRUE))[1]
      }),
      Missing = sapply(cat_vars, function(v) sum(is.na(data$raw[[v]]) | trimws(as.character(data$raw[[v]])) == "")),
      stringsAsFactors = FALSE
    )
    DT::datatable(tbl,
                  options = list(pageLength = 8, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Analyze button
  observeEvent(input$analyzeBtn, {
    req(data$raw)
    data$analyzed <- TRUE
    showNotification("Data ready for analysis!", type = "message", duration = 3)
  })
  
  # ===== SINGLE VARIABLE ANALYSIS =====
  
  output$singleVarSelect <- renderUI({
    req(data$raw)
    
    if(input$singleVarType == "Numerical") {
      selectInput("singleVar", "Select Variable:", choices = data$numeric_vars)
    } else {
      selectInput("singleVar", "Select Variable:", choices = data$categorical_vars)
    }
  })
  
  observeEvent(input$runSingleAnalysis, {
    req(data$raw, input$singleVar)
    
    if(input$singleVarType == "Numerical") {
      var_data <- data$raw[[input$singleVar]]
      
      output$singleStats <- renderPrint({
        cat("Descriptive Statistics for", input$singleVar, "\n")
        cat(strrep("=", 50), "\n\n")
        cat("Central Tendency:\n")
        cat("  Mean   :", round(mean(var_data, na.rm = TRUE), 3), "\n")
        cat("  Median :", round(median(var_data, na.rm = TRUE), 3), "\n")
        cat("  Mode   :", names(sort(table(var_data), decreasing=TRUE)[1]), "\n\n")
        
        cat("Dispersion:\n")
        cat("  Range      :", round(max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE), 3), "\n")
        cat("  Variance   :", round(var(var_data, na.rm = TRUE), 3), "\n")
        cat("  Std Dev    :", round(sd(var_data, na.rm = TRUE), 3), "\n")
        cat("  IQR        :", round(IQR(var_data, na.rm = TRUE), 3), "\n\n")
        
        cat("Shape:\n")
        cat("  Skewness   :", round(skewness(var_data, na.rm = TRUE), 3), "\n")
        cat("  Kurtosis   :", round(kurtosis(var_data, na.rm = TRUE), 3), "\n")
      })
      
      output$singlePlot <- renderPlot({
        par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
        
        hist(var_data, 
             freq = TRUE, 
             main = paste("Histogram of", input$singleVar), 
             col = "lightblue",
             xlab = input$singleVar,
             border = "darkblue")
        
        boxplot(var_data,
                main = paste("Boxplot of", input$singleVar),
                xlab = input$singleVar,
                col = "lightgreen",
                horizontal = TRUE)
      })
      if (HAS_PLOTLY) {
        output$singlePlotly <- plotly::renderPlotly({
          p1 <- plotly::plot_ly(x = var_data, type = "histogram", marker = list(color = "#60a5fa")) |>
            plotly::layout(title = paste("Histogram of", input$singleVar))
          p2 <- plotly::plot_ly(y = var_data, type = "box", marker = list(color = "#34d399")) |>
            plotly::layout(title = paste("Boxplot of", input$singleVar))
          plotly::subplot(p1, p2, nrows = 2, shareX = FALSE, titleY = TRUE)
        })
      }
      
    } else {
      var_data <- data$raw[[input$singleVar]]
      freq_table <- table(var_data)
      prop_table <- prop.table(freq_table)
      
      output$singleStats <- renderPrint({
        cat("Frequency Distribution for", input$singleVar, "\n")
        cat(strrep("=", 50), "\n\n")
        cat("Frequency Table:\n")
        print(freq_table)
        cat("\n\nProportion Table:\n")
        print(round(prop_table, 4))
        cat("\n\nMode:", names(which.max(freq_table)), "\n")
        cat("Modal Frequency:", max(freq_table), "\n")
      })
      
      output$singlePlot <- renderPlot({
        par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
        
        barplot(freq_table,
                main = paste("Bar Plot of", input$singleVar),
                xlab = input$singleVar,
                ylab = "Frequency",
                col = brewer.pal(min(length(freq_table), 8), "Set2"),
                las = 2)
        
        pie(freq_table,
            main = paste("Pie Chart of", input$singleVar),
            col = brewer.pal(min(length(freq_table), 8), "Pastel1"))
      })
      if (HAS_PLOTLY) {
        output$singlePlotly <- plotly::renderPlotly({
          dfp <- data.frame(cat = names(freq_table), freq = as.numeric(freq_table))
          if (input$catPlotType == "Pie Chart") {
            plotly::plot_ly(dfp, labels = ~cat, values = ~freq, type = "pie")
          } else {
            plotly::plot_ly(dfp, x = ~cat, y = ~freq, type = "bar", marker = list(color = "#60a5fa"))
          }
        })
      }
    }
  })
  
  # ===== MULTI-VARIABLE ANALYSIS (2+) =====

  output$multiVarRows <- renderUI({
    req(data$raw)
    n_vars <- as.integer(input$multiVarCount %||% 2)
    n_vars <- max(2, min(6, n_vars))

    rows <- lapply(seq_len(n_vars), function(i) {
      type_id <- paste0("multiVarType", i)
      var_id  <- paste0("multiVar", i)

      type_val <- as.character(input[[type_id]] %||% "Numerical")
      choices <- if (identical(type_val, "Categorical")) data$categorical_vars else data$numeric_vars
      choices <- as.character(choices %||% character(0))
      selected_val <- input[[var_id]]
      if (is.null(selected_val) || length(selected_val) == 0 || !(selected_val %in% choices)) {
        selected_val <- if (length(choices) > 0) choices[1] else NULL
      }

      fluidRow(
        column(2, tags$strong(paste("Variable", i))),
        column(4,
          selectInput(type_id, NULL,
                      choices = c("Numerical", "Categorical"),
                      selected = type_val, width = "100%")
        ),
        column(6,
          selectInput(var_id, NULL,
                      choices = choices,
                      selected = selected_val, width = "100%")
        )
      )
    })

    do.call(tagList, rows)
  })

  observeEvent(input$runMultiAnalysis, {
    req(data$raw, input$multiVarCount)
    withProgress(message = "Running multi-variable analysis...", value = 0, {
      incProgress(0.25)

      n_vars <- as.integer(input$multiVarCount)
      n_vars <- max(2, min(6, n_vars))

      selected_types <- vapply(seq_len(n_vars), function(i) {
        as.character(input[[paste0("multiVarType", i)]] %||% "")
      }, character(1))
      selected_vars <- vapply(seq_len(n_vars), function(i) {
        as.character(input[[paste0("multiVar", i)]] %||% "")
      }, character(1))

      if (any(is.na(selected_vars)) || any(selected_vars == "")) {
        showNotification("Please choose all variables before running the analysis.", type = "error")
        return()
      }
      if (length(unique(selected_vars)) < length(selected_vars)) {
        showNotification("Please select distinct variables (no duplicates).", type = "error")
        return()
      }

      num_vars <- selected_vars[selected_types == "Numerical"]
      cat_vars <- selected_vars[selected_types == "Categorical"]
      df_sel   <- data$raw[, unique(selected_vars), drop = FALSE]
      incProgress(0.35)

    output$twoVarStats <- renderPrint({
      cat("Multi-Variable Analysis\n")
      cat(strrep("=", 55), "\n\n")
      cat("Selected variables:\n")
      for (i in seq_len(n_vars)) {
        cat(sprintf("  %d) %-30s [%s]\n", i, selected_vars[i], selected_types[i]))
      }

      if (length(num_vars) >= 2) {
        cat("\n\nNumerical Block: Correlation Matrix\n")
        cat(strrep("-", 55), "\n")
        corr_mat <- cor(df_sel[, num_vars, drop = FALSE], use = "complete.obs")
        print(round(corr_mat, 4))
      }

      if (length(cat_vars) >= 2) {
        cat("\n\nCategorical Block: Pairwise Chi-Square Tests\n")
        cat(strrep("-", 55), "\n")
        for (i in 1:(length(cat_vars) - 1)) {
          for (j in (i + 1):length(cat_vars)) {
            t_ij <- table(df_sel[[cat_vars[i]]], df_sel[[cat_vars[j]]])
            cat("\n", cat_vars[i], "vs", cat_vars[j], "\n")
            tryCatch({
              ch <- chisq.test(t_ij)
              cat("  Chi-square:", round(ch$statistic, 4),
                  " df:", ch$parameter,
                  " p-value:", format.pval(ch$p.value, digits = 4), "\n")
            }, error = function(e) {
              cat("  Test could not be computed:", e$message, "\n")
            })
          }
        }
      }

      if (length(num_vars) >= 1 && length(cat_vars) >= 1) {
        cat("\n\nMixed Block: Group Mean Comparisons (ANOVA)\n")
        cat(strrep("-", 55), "\n")
        for (nv in num_vars) {
          for (cv in cat_vars) {
            cat("\n", nv, "by", cv, "\n")
            tmp <- data.frame(y = df_sel[[nv]], g = as.factor(df_sel[[cv]]))
            tmp <- tmp[complete.cases(tmp), , drop = FALSE]
            if (nlevels(tmp$g) < 2 || nrow(tmp) < 3) {
              cat("  Not enough valid groups/data for ANOVA.\n")
            } else {
              aov_out <- summary(aov(y ~ g, data = tmp))
              print(aov_out)
            }
          }
        }
      }
    })

      output$twoVarPlot <- renderPlot({
      if (length(num_vars) >= 3) {
        pairs(df_sel[, num_vars, drop = FALSE],
              main = "Scatter Plot Matrix (Numerical Variables)",
              pch = 19, col = rgb(0.2, 0.5, 0.8, 0.5), cex = 0.7)
      } else if (length(num_vars) == 2) {
        x <- df_sel[[num_vars[1]]]
        y <- df_sel[[num_vars[2]]]
        plot(x, y,
             main = paste(num_vars[1], "vs", num_vars[2]),
             xlab = num_vars[1], ylab = num_vars[2],
             pch = 19, col = rgb(0.2, 0.5, 0.8, 0.5))
        abline(lm(y ~ x), col = "red", lwd = 2)
      } else if (length(num_vars) == 1 && length(cat_vars) >= 1) {
        g <- as.factor(df_sel[[cat_vars[1]]])
        boxplot(df_sel[[num_vars[1]]] ~ g,
                main = paste(num_vars[1], "by", cat_vars[1]),
                xlab = cat_vars[1], ylab = num_vars[1],
                col = brewer.pal(min(length(levels(g)), 8), "Set2"),
                las = 2)
      } else if (length(cat_vars) >= 2) {
        t12 <- table(df_sel[[cat_vars[1]]], df_sel[[cat_vars[2]]])
        barplot(t12,
                main = paste(cat_vars[1], "vs", cat_vars[2]),
                xlab = cat_vars[2], ylab = "Frequency",
                col = brewer.pal(min(nrow(t12), 8), "Set3"),
                beside = TRUE, legend.text = TRUE)
      } else {
        plot.new()
        text(0.5, 0.5, "Not enough valid variables selected for plotting.", cex = 1.1)
      }
    })
    if (HAS_PLOTLY) {
      output$twoVarPlotly <- plotly::renderPlotly({
        if (length(num_vars) >= 2) {
          x <- df_sel[[num_vars[1]]]
          y <- df_sel[[num_vars[2]]]
          plotly::plot_ly(x = x, y = y, type = "scatter", mode = "markers",
                          marker = list(color = "#3b82f6", opacity = 0.65)) |>
            plotly::layout(title = paste(num_vars[1], "vs", num_vars[2]),
                           xaxis = list(title = num_vars[1]),
                           yaxis = list(title = num_vars[2]))
        } else if (length(num_vars) == 1 && length(cat_vars) >= 1) {
          plotly::plot_ly(x = as.factor(df_sel[[cat_vars[1]]]),
                          y = df_sel[[num_vars[1]]], type = "box")
        } else if (length(cat_vars) >= 2) {
          t12 <- as.data.frame(table(df_sel[[cat_vars[1]]], df_sel[[cat_vars[2]]]))
          names(t12) <- c("Var1", "Var2", "Freq")
          plotly::plot_ly(t12, x = ~Var2, y = ~Freq, color = ~Var1, type = "bar")
        } else {
          plotly::plot_ly()
        }
      })
    }
      incProgress(0.40)
    })
  })
  
  # ===== CORRELATION MATRIX =====
  
  output$corrVarsSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("corrVars", "Select Variables (multiple):",
               choices = data$numeric_vars,
               multiple = TRUE,
               selected = data$numeric_vars[1:min(3, length(data$numeric_vars))])
  })
  
  observeEvent(input$runCorrAnalysis, {
    req(data$raw, input$corrVars)
    withProgress(message = "Computing correlation outputs...", value = 0, {
    
    selected_data <- data$raw[, input$corrVars, drop = FALSE]
    corr_matrix <- cor(selected_data, use = "complete.obs")
    incProgress(0.4)
    
    output$corrMatrix <- renderPrint({
      cat("Correlation Matrix\n")
      cat(strrep("=", 40), "\n\n")
      print(round(corr_matrix, 3))
    })
    
    output$corrHeatmap <- renderPlot({
      corrplot(corr_matrix, 
               method = "circle",
               addCoef.col = "black",
               type = "upper",
               diag = FALSE,
               col = COL2('RdYlBu', n=20),
               tl.cex = 0.9,
               tl.col = "black",
               number.cex = 0.8)
    })
    
    output$corrPairs <- renderPlot({
      pairs(selected_data,
            main = "Scatter Plot Matrix",
            pch = 19,
            col = rgb(0.2, 0.5, 0.8, 0.4),
            cex = 0.6)
    })
    incProgress(0.6)
    })
  })
  
  # ===== CATEGORICAL ANALYSIS =====
  
  output$catVarSelect <- renderUI({
    req(data$categorical_vars)
    selectInput("catVar", "Select Categorical Variable:",
               choices = data$categorical_vars)
  })
  
  observeEvent(input$runCatAnalysis, {
    req(data$raw, input$catVar)
    
    cat_data <- data$raw[[input$catVar]]
    freq_table <- table(cat_data)
    prop_table <- prop.table(freq_table)
    
    output$catFrequency <- renderTable({
      data.frame(
        Category = names(freq_table),
        Frequency = as.numeric(freq_table)
      )
    }, rownames = FALSE)
    
    output$catProportion <- renderTable({
      data.frame(
        Category = names(prop_table),
        Proportion = round(as.numeric(prop_table), 4),
        Percentage = paste0(round(as.numeric(prop_table) * 100, 2), "%")
      )
    }, rownames = FALSE)
    
    output$catPlot <- renderPlot({
      if(input$catPlotType == "Bar Plot") {
        par(mfrow = c(1, 1))
        barplot(freq_table,
                main = paste("Bar Plot of", input$catVar),
                xlab = input$catVar,
                ylab = "Frequency",
                col = brewer.pal(min(length(freq_table), 8), "Set2"),
                las = 2)
      } else if(input$catPlotType == "Pie Chart") {
        par(mfrow = c(1, 1))
        pie(freq_table,
            main = paste("Pie Chart of", input$catVar),
            col = brewer.pal(min(length(freq_table), 8), "Pastel1"),
            labels = paste(names(freq_table), "\n(", 
                          round(prop_table*100, 1), "%)", sep = ""))
      } else {
        par(mfrow = c(2, 1), mar = c(5, 4, 3, 2))
        barplot(freq_table,
                main = paste("Bar Plot of", input$catVar),
                xlab = input$catVar,
                ylab = "Frequency",
                col = brewer.pal(min(length(freq_table), 8), "Set2"),
                las = 2)
        pie(freq_table,
            main = paste("Pie Chart of", input$catVar),
            col = brewer.pal(min(length(freq_table), 8), "Pastel1"))
      }
    })
    if (HAS_PLOTLY) {
      output$catPlotly <- plotly::renderPlotly({
        dfp <- data.frame(Category = names(freq_table), Frequency = as.numeric(freq_table))
        if (input$catPlotType == "Pie Chart") {
          plotly::plot_ly(dfp, labels = ~Category, values = ~Frequency, type = "pie")
        } else {
          plotly::plot_ly(dfp, x = ~Category, y = ~Frequency, type = "bar",
                          marker = list(color = "#60a5fa"))
        }
      })
    }
  })
  
  # ===== CROSS-TABULATION =====
  
  output$crosstabVar1Select <- renderUI({
    req(data$categorical_vars)
    selectInput("crosstabVar1", "Row Variable:",
               choices = data$categorical_vars)
  })
  
  output$crosstabVar2Select <- renderUI({
    req(data$categorical_vars)
    selectInput("crosstabVar2", "Column Variable:",
               choices = data$categorical_vars)
  })
  
  observeEvent(input$runCrosstabAnalysis, {
    req(data$raw, input$crosstabVar1, input$crosstabVar2)
    
    var1_data <- data$raw[[input$crosstabVar1]]
    var2_data <- data$raw[[input$crosstabVar2]]
    
    cont_table <- table(var1_data, var2_data)
    joint_prop <- prop.table(cont_table)
    
    output$crosstabTable <- renderTable({
      as.data.frame.matrix(cont_table)
    }, rownames = TRUE)
    
    output$crosstabProp <- renderTable({
      as.data.frame.matrix(round(joint_prop, 4))
    }, rownames = TRUE)
    
    output$chiSquareTest <- renderPrint({
      chi_test <- chisq.test(cont_table)
      cat("Chi-Square Test of Independence\n")
      cat(strrep("=", 40), "\n\n")
      print(chi_test)
      
      cat("\n\nCramér's V:\n")
      cramers_v <- sqrt(chi_test$statistic / (sum(cont_table) * (min(dim(cont_table)) - 1)))
      cat(sprintf("%.4f\n", cramers_v))
    })
    
    output$crosstabPlot <- renderPlot({
      if(input$crosstabPlotType == "Stacked Bar") {
        barplot(cont_table,
                main = paste(input$crosstabVar1, "vs", input$crosstabVar2),
                xlab = input$crosstabVar2,
                col = brewer.pal(min(nrow(cont_table), 8), "Set3"),
                legend.text = TRUE)
      } else if(input$crosstabPlotType == "Grouped Bar") {
        barplot(cont_table,
                main = paste(input$crosstabVar1, "vs", input$crosstabVar2),
                xlab = input$crosstabVar2,
                col = brewer.pal(min(nrow(cont_table), 8), "Set3"),
                beside = TRUE,
                legend.text = TRUE)
      } else {
        mosaicplot(cont_table,
                   main = paste(input$crosstabVar1, "vs", input$crosstabVar2),
                   color = brewer.pal(min(ncol(cont_table), 8), "Set2"))
      }
    })
  })
  
  # ===== MULTI-CATEGORICAL ANALYSIS (3+ VARIABLES) =====
  
  # Variable selectors for multi-categorical
  output$multiCatVar1Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar1", "Variable 1:",
               choices = c("Select...", data$categorical_vars),
               selected = "Select...")
  })
  
  output$multiCatVar2Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar2", "Variable 2:",
               choices = c("Select...", data$categorical_vars),
               selected = "Select...")
  })
  
  output$multiCatVar3Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar3", "Variable 3:",
               choices = c("Select...", data$categorical_vars),
               selected = "Select...")
  })
  
  output$multiCatVar4Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar4", "Variable 4 (Optional):",
               choices = c("None", data$categorical_vars),
               selected = "None")
  })
  
  # Multi-categorical analysis
  observeEvent(input$runMultiCatAnalysis, {
    req(data$raw, input$multiCatVar1, input$multiCatVar2, input$multiCatVar3)
    
    # Collect selected variables (excluding "Select..." and "None")
    selected_vars <- c(input$multiCatVar1, input$multiCatVar2, input$multiCatVar3)
    if(!is.null(input$multiCatVar4) && input$multiCatVar4 != "None") {
      selected_vars <- c(selected_vars, input$multiCatVar4)
    }
    selected_vars <- selected_vars[selected_vars != "Select..."]
    
    if(length(selected_vars) < 3) {
      showNotification("Please select at least 3 categorical variables!", 
                      type = "error", duration = 5)
      return()
    }
    
    # Create multi-way table
    multi_data <- data$raw[, selected_vars, drop = FALSE]
    multi_table <- table(multi_data)
    
    # Multi-way contingency table output
    output$multiCatTable <- renderPrint({
      cat("Multi-Way Contingency Table\n")
      cat(strrep("=", 60), "\n\n")
      cat("Variables:", paste(selected_vars, collapse = ", "), "\n")
      cat("Dimensions:", paste(dim(multi_table), collapse = " x "), "\n\n")
      print(multi_table)
    })
    
    # Conditional proportions
    output$multiCatProportions <- renderPrint({
      cat("Proportions (relative to total)\n")
      cat(strrep("=", 60), "\n\n")
      prop_table <- prop.table(multi_table)
      print(round(prop_table, 4))
      
      cat("\n\nConditional Proportions (by", selected_vars[1], ")\n")
      cat(strrep("-", 60), "\n")
      cond_prop <- prop.table(multi_table, 1)
      print(round(cond_prop, 4))
    })
    
    # Chi-square test
    output$multiCatChiSquare <- renderPrint({
      cat("Multi-Way Chi-Square Test\n")
      cat(strrep("=", 40), "\n\n")
      
      # Flatten to 2D for chi-square test
      if(length(selected_vars) == 3) {
        # Create formula
        formula_str <- paste(selected_vars[3], "~", selected_vars[1], "+", selected_vars[2])
        test_table <- xtabs(as.formula(formula_str), data = multi_data)
        
        tryCatch({
          chi_test <- chisq.test(test_table)
          print(chi_test)
          
          cat("\n\nInterpretation:\n")
          if(chi_test$p.value < 0.05) {
            cat("Result: SIGNIFICANT association (p < 0.05)\n")
            cat("Conclusion: The variables are NOT independent.\n")
          } else {
            cat("Result: NO significant association (p >= 0.05)\n")
            cat("Conclusion: The variables appear to be independent.\n")
          }
        }, error = function(e) {
          cat("Chi-square test could not be computed.\n")
          cat("This may be due to small expected frequencies.\n")
          cat("Error:", e$message, "\n")
        })
      } else {
        cat("Note: For 4+ variables, pairwise testing is performed.\n\n")
        
        for(i in 1:(length(selected_vars)-1)) {
          for(j in (i+1):length(selected_vars)) {
            cat("\nTesting:", selected_vars[i], "vs", selected_vars[j], "\n")
            cat(strrep("-", 40), "\n")
            test_table <- table(multi_data[[selected_vars[i]]], 
                              multi_data[[selected_vars[j]]])
            tryCatch({
              chi_test <- chisq.test(test_table)
              cat("Chi-square =", round(chi_test$statistic, 3), 
                 ", df =", chi_test$parameter,
                 ", p-value =", format.pval(chi_test$p.value, digits = 4), "\n")
            }, error = function(e) {
              cat("Test failed: small expected frequencies\n")
            })
          }
        }
      }
    })
    
    # Association measures
    output$multiCatAssociation <- renderPrint({
      cat("Association Strength Measures\n")
      cat(strrep("=", 40), "\n\n")
      
      # Calculate Cramér's V for all pairs
      for(i in 1:(length(selected_vars)-1)) {
        for(j in (i+1):length(selected_vars)) {
          test_table <- table(multi_data[[selected_vars[i]]], 
                            multi_data[[selected_vars[j]]])
          
          tryCatch({
            chi_test <- chisq.test(test_table)
            n <- sum(test_table)
            min_dim <- min(dim(test_table)) - 1
            cramers_v <- sqrt(chi_test$statistic / (n * min_dim))
            
            cat(selected_vars[i], "vs", selected_vars[j], ":\n")
            cat("  Cramér's V =", round(cramers_v, 4))
            
            if(cramers_v < 0.1) {
              cat(" (Negligible)\n")
            } else if(cramers_v < 0.3) {
              cat(" (Weak)\n")
            } else if(cramers_v < 0.5) {
              cat(" (Moderate)\n")
            } else {
              cat(" (Strong)\n")
            }
          }, error = function(e) {
            cat(selected_vars[i], "vs", selected_vars[j], ": Unable to compute\n")
          })
        }
      }
    })
    
    # Visualization options
    output$multiCatPlotOptions <- renderUI({
      if(input$multiCatPlotType == "Heatmap") {
        selectInput("heatmapVars", "Select 2 variables for heatmap:",
                   choices = selected_vars,
                   multiple = TRUE,
                   selected = selected_vars[1:2])
      } else {
        NULL
      }
    })
    
    # Multi-way visualization
    output$multiCatPlot <- renderPlot({
      req(multi_table)
      
      if(input$multiCatPlotType == "Grouped Bar Chart") {
        # Grouped bar chart
        if(length(selected_vars) == 3) {
          counts <- as.data.frame(multi_table)
          names(counts)[length(names(counts))] <- "Freq"
          
          par(mar = c(8, 4, 3, 2))
          
          # Create interaction variable
          interaction_var <- interaction(counts[[selected_vars[2]]], 
                                        counts[[selected_vars[3]]])
          
          barplot(counts$Freq,
                  main = paste("Multi-Way Distribution"),
                  xlab = "",
                  ylab = "Frequency",
                  col = rep(brewer.pal(min(8, length(unique(counts[[selected_vars[2]]]))), "Set3"),
                           length.out = nrow(counts)),
                  las = 2,
                  names.arg = paste(counts[[selected_vars[1]]], 
                                  counts[[selected_vars[2]]],
                                  counts[[selected_vars[3]]], sep = "\n"))
        } else {
          plot.new()
          text(0.5, 0.5, "Grouped bar chart works best with 3 variables.\nTry another visualization type.", cex = 1.5)
        }
        
      } else if(input$multiCatPlotType == "Faceted Bar Chart") {
        # Faceted visualization
        if(length(selected_vars) >= 3) {
          par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
          
          # Create different facets
          unique_vals <- unique(multi_data[[selected_vars[1]]])
          
          for(val in unique_vals[1:min(4, length(unique_vals))]) {
            subset_data <- multi_data[multi_data[[selected_vars[1]]] == val, ]
            subset_table <- table(subset_data[[selected_vars[2]]], 
                                subset_data[[selected_vars[3]]])
            
            barplot(subset_table,
                    main = paste(selected_vars[1], "=", val),
                    xlab = selected_vars[3],
                    ylab = "Frequency",
                    col = brewer.pal(min(nrow(subset_table), 8), "Pastel1"),
                    beside = TRUE,
                    legend.text = TRUE,
                    args.legend = list(x = "topright", cex = 0.7))
          }
        }
        
      } else if(input$multiCatPlotType == "Mosaic Plot") {
        # Mosaic plot
        if(length(selected_vars) == 3) {
          formula_str <- paste("~", paste(selected_vars, collapse = " + "))
          mosaicplot(as.formula(formula_str), data = multi_data,
                    main = "Multi-Way Mosaic Plot",
                    color = brewer.pal(8, "Set2"))
        } else {
          # For 4 variables, show first 3
          formula_str <- paste("~", paste(selected_vars[1:3], collapse = " + "))
          mosaicplot(as.formula(formula_str), data = multi_data,
                    main = paste("Mosaic Plot:", paste(selected_vars[1:3], collapse = ", ")),
                    color = brewer.pal(8, "Set2"))
        }
        
      } else if(input$multiCatPlotType == "Heatmap") {
        # Heatmap for frequency counts
        if(!is.null(input$heatmapVars) && length(input$heatmapVars) == 2) {
          heat_table <- table(multi_data[[input$heatmapVars[1]]], 
                            multi_data[[input$heatmapVars[2]]])
          
          # Convert to matrix
          heat_matrix <- as.matrix(heat_table)
          
          # Create color palette
          colors <- colorRampPalette(c("white", "#FFF9C4", "#FF9800", "#F44336"))(20)
          
          # Plot heatmap
          image(1:ncol(heat_matrix), 1:nrow(heat_matrix), t(heat_matrix),
                col = colors,
                xlab = input$heatmapVars[2],
                ylab = input$heatmapVars[1],
                main = "Frequency Heatmap",
                axes = FALSE)
          
          axis(1, at = 1:ncol(heat_matrix), labels = colnames(heat_matrix), las = 2)
          axis(2, at = 1:nrow(heat_matrix), labels = rownames(heat_matrix), las = 2)
          
          # Add frequency values
          for(i in 1:nrow(heat_matrix)) {
            for(j in 1:ncol(heat_matrix)) {
              text(j, i, heat_matrix[i, j], cex = 1.2)
            }
          }
        } else {
          plot.new()
          text(0.5, 0.5, "Please select exactly 2 variables for heatmap", cex = 1.5)
        }
      }
    })
    
    # Insights
    output$multiCatInsights <- renderUI({
      insights <- list()
      
      # Total combinations
      total_combos <- prod(dim(multi_table))
      observed_combos <- sum(multi_table > 0)
      
      insights <- c(insights, paste0(
        "<li><strong>Sparsity:</strong> ", observed_combos, " out of ", total_combos,
        " possible combinations are observed (",
        round(100 * observed_combos / total_combos, 1), "%).</li>"
      ))
      
      # Most common pattern
      flat_table <- as.data.frame(multi_table)
      most_common_idx <- which.max(flat_table$Freq)
      most_common <- flat_table[most_common_idx, ]
      
      insights <- c(insights, paste0(
        "<li><strong>Most Common Pattern:</strong> ",
        paste(names(most_common)[1:(length(names(most_common))-1)], 
             "=", most_common[1:(length(most_common)-1)], collapse = ", "),
        " with ", most_common$Freq, " occurrences.</li>"
      ))
      
      # Recommendations
      recommendations <- paste0(
        "<h4><i class='fa fa-lightbulb'></i> Recommendations:</h4>",
        "<ul>",
        "<li>Use <strong>Mosaic Plot</strong> to visualize proportions across all variables</li>",
        "<li>Use <strong>Faceted Bar Chart</strong> to compare patterns within groups</li>",
        "<li>Check chi-square test results for statistical significance</li>",
        "</ul>"
      )
      
      HTML(paste0(
        '<div style="background-color: #FFF3E0; padding: 15px; border-radius: 5px; border-left: 4px solid #FF9800;">',
        '<h4><i class="fa fa-info-circle"></i> Analysis Insights:</h4>',
        '<ul>',
        paste(insights, collapse = "\n"),
        '</ul>',
        '</div>',
        '<br>',
        '<div style="background-color: #E8F5E9; padding: 15px; border-radius: 5px; border-left: 4px solid #4CAF50;">',
        recommendations,
        '</div>'
      ))
    })
  })
  
  # ===== HYPOTHESIS TESTS =====
  
  output$testVarInputs <- renderUI({
    req(data$raw)
    
    if(input$testType %in% c("One-Sample T-Test", "One-Sample Wilcoxon Test", "Normality Test (Shapiro-Wilk)")) {
      tagList(
        selectInput("testVar1", "Select Variable:", choices = data$numeric_vars),
        numericInput("testMu", "Hypothesized Mean (μ₀):", value = 0)
      )
    } else if(input$testType == "Two-Sample T-Test") {
      tagList(
        selectInput("testVar1", "Group Variable (Categorical):", 
                   choices = data$categorical_vars),
        selectInput("testVar2", "Measurement Variable (Numerical):", 
                   choices = data$numeric_vars)
      )
    } else if(input$testType == "Paired T-Test") {
      tagList(
        selectInput("testVar1", "Variable 1:", choices = data$numeric_vars),
        selectInput("testVar2", "Variable 2:", choices = data$numeric_vars)
      )
    } else if(input$testType == "One-Way ANOVA (F-Test)") {
      tagList(
        selectInput("testVar1", "Group Variable (Categorical):", choices = data$categorical_vars),
        selectInput("testVar2", "Measurement Variable (Numerical):", choices = data$numeric_vars)
      )
    } else {
      tagList(
        selectInput("testVar1", "Group Variable:", choices = data$categorical_vars),
        selectInput("testVar2", "Measurement Variable:", choices = data$numeric_vars)
      )
    }
  })
  
  observeEvent(input$runHypothesisTest, {
    req(data$raw, input$testVar1)
    
    if(input$testType == "One-Sample T-Test") {
      test_data <- data$raw[[input$testVar1]]
      
      output$testResults <- renderPrint({
        t_test <- t.test(test_data, mu = input$testMu)
        print(t_test)
      })
      
      output$testPlot <- renderPlot({
        boxplot(test_data,
                main = "Distribution",
                ylab = input$testVar1,
                col = "lightblue")
        abline(h = input$testMu, col = "red", lwd = 2, lty = 2)
      })
      
    } else if(input$testType == "Normality Test (Shapiro-Wilk)") {
      test_data <- data$raw[[input$testVar1]]
      sample_size <- min(5000, length(test_data))
      
      output$testResults <- renderPrint({
        shapiro_test <- shapiro.test(sample(test_data, sample_size))
        print(shapiro_test)
      })
      
      output$testPlot <- renderPlot({
        qqnorm(test_data, main = "Q-Q Plot")
        qqline(test_data, col = "red", lwd = 2)
      })
      
    } else if(input$testType == "Two-Sample T-Test") {
      req(input$testVar2)
      group_var <- data$raw[[input$testVar1]]
      measure_var <- data$raw[[input$testVar2]]
      
      output$testResults <- renderPrint({
        t_test <- t.test(measure_var ~ group_var)
        print(t_test)
      })
      
      output$testPlot <- renderPlot({
        boxplot(measure_var ~ group_var,
                main = "Group Comparison",
                xlab = input$testVar1,
                ylab = input$testVar2,
                col = c("lightblue", "lightgreen"))
      })
    } else if(input$testType == "One-Way ANOVA (F-Test)") {
      req(input$testVar2)
      group_var <- as.factor(data$raw[[input$testVar1]])
      measure_var <- data$raw[[input$testVar2]]
      df_test <- data.frame(g = group_var, y = measure_var)
      df_test <- df_test[complete.cases(df_test), , drop = FALSE]

      output$testResults <- renderPrint({
        if (nlevels(df_test$g) < 2) {
          cat("ANOVA requires at least 2 groups.\n")
          return()
        }
        aov_fit <- aov(y ~ g, data = df_test)
        cat("One-Way ANOVA (F-Test)\n")
        cat(strrep("=", 35), "\n\n")
        print(summary(aov_fit))
      })

      output$testPlot <- renderPlot({
        boxplot(y ~ g, data = df_test,
                main = "One-Way ANOVA: Group Comparison",
                xlab = input$testVar1,
                ylab = input$testVar2,
                col = brewer.pal(min(8, nlevels(df_test$g)), "Set2"),
                las = 2)
      })
    }
  })
  
  # ===== SUMMARY REPORT - REDESIGNED =====
  
  # Info Boxes at the top
  output$summaryRows <- renderInfoBox({
    req(data$raw)
    infoBox(
      "Total Rows",
      format(nrow(data$raw), big.mark = ","),
      icon = icon("database"),
      color = "blue",
      fill = TRUE
    )
  })
  
  output$summaryColumns <- renderInfoBox({
    req(data$raw)
    infoBox(
      "Total Variables",
      ncol(data$raw),
      icon = icon("columns"),
      color = "green",
      fill = TRUE
    )
  })
  
  output$summaryNumeric <- renderInfoBox({
    req(data$numeric_vars)
    infoBox(
      "Numerical",
      length(data$numeric_vars),
      icon = icon("hashtag"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$summaryCategorical <- renderInfoBox({
    req(data$categorical_vars)
    infoBox(
      "Categorical",
      length(data$categorical_vars),
      icon = icon("tags"),
      color = "orange",
      fill = TRUE
    )
  })
  
  # Variable Structure Table
  output$variableTable <- renderDT({
    req(data$raw)
    
    var_info <- data.frame(
      Variable = names(data$raw),
      Type = ifelse(names(data$raw) %in% data$numeric_vars, "Numerical", "Categorical"),
      Missing = colSums(is.na(data$raw)),
      `Missing %` = round(100 * colSums(is.na(data$raw)) / nrow(data$raw), 2),
      check.names = FALSE
    )
    
    datatable(var_info,
              options = list(
                pageLength = 10,
                scrollY = "250px",
                dom = 'ftp'
              ),
              rownames = FALSE) %>%
      formatStyle('Type',
                  backgroundColor = styleEqual(c('Numerical', 'Categorical'), 
                                              c('#e3f2fd', '#fff3e0')))
  })
  
  # Variable Type Distribution Plot
  output$variableTypePlot <- renderPlot({
    req(data$raw)
    
    type_counts <- c(
      Numerical = length(data$numeric_vars),
      Categorical = length(data$categorical_vars)
    )
    
    par(mar = c(3, 3, 3, 2))
    
    # Create a nice pie chart
    colors <- c("#42A5F5", "#FFA726")
    pie(type_counts,
        labels = paste(names(type_counts), "\n", type_counts, 
                      " (", round(100*type_counts/sum(type_counts), 1), "%)", sep=""),
        col = colors,
        main = "Variable Types",
        cex = 1.2,
        border = "white",
        lwd = 2)
  })
  
  # Numerical Variables Summary Table
  output$numericSummaryTable <- renderDT({
    req(data$raw, data$numeric_vars)
    
    if(length(data$numeric_vars) > 0) {
      numeric_data <- data$raw[, data$numeric_vars, drop = FALSE]
      
      summary_stats <- data.frame(
        Variable = data$numeric_vars,
        Mean = sapply(numeric_data, function(x) round(mean(x, na.rm = TRUE), 2)),
        Median = sapply(numeric_data, function(x) round(median(x, na.rm = TRUE), 2)),
        SD = sapply(numeric_data, function(x) round(sd(x, na.rm = TRUE), 2)),
        Min = sapply(numeric_data, function(x) round(min(x, na.rm = TRUE), 2)),
        Max = sapply(numeric_data, function(x) round(max(x, na.rm = TRUE), 2)),
        Missing = sapply(numeric_data, function(x) sum(is.na(x)))
      )
      
      datatable(summary_stats,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  dom = 'ftp'
                ),
                rownames = FALSE) %>%
        formatStyle(columns = 1:7, fontSize = '14px')
    }
  })
  
  # Missing Values Plot (enhanced)
  output$missingPlot <- renderPlot({
    req(data$raw)
    
    missing_counts <- colSums(is.na(data$raw))
    missing_pct <- 100 * missing_counts / nrow(data$raw)
    
    if(sum(missing_counts) > 0) {
      # Only show variables with missing values
      missing_vars <- missing_counts[missing_counts > 0]
      missing_pct_filtered <- missing_pct[missing_counts > 0]
      
      par(mar = c(8, 4, 3, 2))
      barplot(missing_pct_filtered,
              main = "Missing Values by Variable (%)",
              ylab = "Percentage Missing",
              col = colorRampPalette(c("#FFF9C4", "#FF5722"))(length(missing_vars)),
              las = 2,
              border = NA,
              ylim = c(0, max(missing_pct_filtered) * 1.2))
      
      # Add percentage labels on top
      text(x = seq_along(missing_vars) * 1.2 - 0.5,
           y = missing_pct_filtered + max(missing_pct_filtered) * 0.05,
           labels = paste0(round(missing_pct_filtered, 1), "%"),
           cex = 0.8)
    } else {
      plot.new()
      text(0.5, 0.5, "✓ No Missing Values!", cex = 2.5, col = "#4CAF50", font = 2)
      text(0.5, 0.35, "Your data is complete!", cex = 1.5, col = "#666")
    }
  })
  
  output$missingStats <- renderPrint({
    req(data$raw)
    
    missing_counts <- colSums(is.na(data$raw))
    total_cells <- nrow(data$raw) * ncol(data$raw)
    total_missing <- sum(missing_counts)
    
    cat("COMPLETENESS SUMMARY\n")
    cat(strrep("=", 40), "\n\n")
    cat(sprintf("Total cells: %s\n", format(total_cells, big.mark = ",")))
    cat(sprintf("Missing values: %s\n", format(total_missing, big.mark = ",")))
    cat(sprintf("Completeness: %.2f%%\n", 100 * (1 - total_missing/total_cells)))
    
    if(total_missing > 0) {
      cat("\nVariables with missing data:\n")
      cat(strrep("-", 40), "\n")
      missing_vars <- missing_counts[missing_counts > 0]
      for(var in names(missing_vars)) {
        pct <- 100 * missing_vars[var] / nrow(data$raw)
        cat(sprintf("  %-20s: %5d (%.1f%%)\n", var, missing_vars[var], pct))
      }
    }
  })
  
  # Data Completeness Visualization
  output$completenessPlot <- renderPlot({
    req(data$raw)
    
    missing_counts <- colSums(is.na(data$raw))
    completeness <- 100 * (1 - missing_counts / nrow(data$raw))
    
    par(mar = c(8, 4, 3, 2))
    
    # Color code: green for >95%, yellow for 90-95%, orange for 80-90%, red for <80%
    colors <- ifelse(completeness >= 95, "#4CAF50",
                    ifelse(completeness >= 90, "#FDD835",
                          ifelse(completeness >= 80, "#FF9800", "#F44336")))
    
    barplot(completeness,
            main = "Data Completeness by Variable",
            ylab = "Completeness (%)",
            col = colors,
            las = 2,
            border = NA,
            ylim = c(0, 105))
    
    abline(h = 95, col = "#4CAF50", lty = 2, lwd = 2)
    abline(h = 90, col = "#FDD835", lty = 2, lwd = 1)
    abline(h = 80, col = "#FF9800", lty = 2, lwd = 1)
  })
  
  # Data Quality Score
  output$dataQualityScore <- renderUI({
    req(data$raw)
    
    missing_counts <- colSums(is.na(data$raw))
    total_cells <- nrow(data$raw) * ncol(data$raw)
    completeness_pct <- 100 * (1 - sum(missing_counts) / total_cells)
    
    # Determine quality rating
    if(completeness_pct >= 99) {
      rating <- "Excellent"
      color <- "#4CAF50"
      icon_name <- "check-circle"
    } else if(completeness_pct >= 95) {
      rating <- "Very Good"
      color <- "#8BC34A"
      icon_name <- "check"
    } else if(completeness_pct >= 90) {
      rating <- "Good"
      color <- "#FDD835"
      icon_name <- "thumbs-up"
    } else if(completeness_pct >= 80) {
      rating <- "Fair"
      color <- "#FF9800"
      icon_name <- "exclamation-triangle"
    } else {
      rating <- "Needs Attention"
      color <- "#F44336"
      icon_name <- "exclamation-circle"
    }
    
    HTML(paste0(
      '<div style="text-align: center; padding: 20px;">',
      '<h2 style="color: ', color, '; margin: 0;">',
      '<i class="fa fa-', icon_name, '"></i> ', rating,
      '</h2>',
      '<h1 style="font-size: 48px; margin: 10px 0; color: ', color, ';">',
      round(completeness_pct, 1), '%',
      '</h1>',
      '<p style="font-size: 16px; color: #666;">Data Completeness Score</p>',
      '</div>'
    ))
  })
  
  # Distribution Select Input
  output$distributionSelect <- renderUI({
    req(data$numeric_vars)
    
    if(length(data$numeric_vars) > 0) {
      selectInput("distVar", "Select variable to visualize:",
                 choices = data$numeric_vars,
                 selected = data$numeric_vars[1])
    } else {
      p("No numerical variables available for distribution analysis.")
    }
  })
  
  # Distribution Plot
  output$distributionPlot <- renderPlot({
    req(data$raw, input$distVar)
    
    var_data <- data$raw[[input$distVar]]
    
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
    
    # Histogram with density
    hist(var_data,
         freq = FALSE,
         main = paste("Distribution of", input$distVar),
         xlab = input$distVar,
         col = rgb(0.3, 0.5, 0.8, 0.6),
         border = "white")
    
    lines(density(var_data, na.rm = TRUE), col = "#E91E63", lwd = 3)
    
    # Boxplot with statistics
    boxplot(var_data,
            main = paste("Boxplot of", input$distVar),
            ylab = input$distVar,
            col = rgb(0.5, 0.8, 0.5, 0.6),
            border = "#388E3C",
            horizontal = FALSE)
    
    # Add mean line
    abline(h = mean(var_data, na.rm = TRUE), col = "#F44336", lwd = 2, lty = 2)
    legend("topright", legend = "Mean", col = "#F44336", lty = 2, lwd = 2, cex = 0.8)
  })
  
  # Automated Insights
  output$dataInsights <- renderUI({
    req(data$raw)
    
    insights <- list()
    
    # Dataset size insight
    n_rows <- nrow(data$raw)
    if(n_rows < 30) {
      insights <- c(insights, paste0(
        "<li><strong>Small Sample:</strong> Your dataset has only ", n_rows, 
        " rows. Consider collecting more data for robust statistical analysis.</li>"
      ))
    } else if(n_rows > 10000) {
      insights <- c(insights, paste0(
        "<li><strong>Large Dataset:</strong> With ", format(n_rows, big.mark=","), 
        " rows, you have excellent statistical power for analysis.</li>"
      ))
    }
    
    # Missing values insight
    missing_counts <- colSums(is.na(data$raw))
    if(sum(missing_counts) == 0) {
      insights <- c(insights, 
        "<li><strong>Complete Data:</strong> Excellent! No missing values detected.</li>"
      )
    } else if(sum(missing_counts) > nrow(data$raw) * 0.1) {
      insights <- c(insights,
        "<li><strong>Missing Data:</strong> Significant missing values detected. Consider imputation methods.</li>"
      )
    }
    
    # Variable balance
    if(length(data$numeric_vars) > 0 && length(data$categorical_vars) > 0) {
      insights <- c(insights,
        "<li><strong>Mixed Data Types:</strong> Your dataset contains both numerical and categorical variables, enabling diverse analyses.</li>"
      )
    } else if(length(data$numeric_vars) == 0) {
      insights <- c(insights,
        "<li><strong>All Categorical:</strong> Your dataset contains only categorical variables. Focus on frequency analysis and chi-square tests.</li>"
      )
    } else if(length(data$categorical_vars) == 0) {
      insights <- c(insights,
        "<li><strong>All Numerical:</strong> Your dataset is entirely numerical. Perfect for correlation and regression analysis.</li>"
      )
    }
    
    # Recommended analyses
    recommendations <- "<h4><i class='fa fa-tasks'></i> Recommended Next Steps:</h4><ul>"
    
    if(length(data$numeric_vars) >= 2) {
      recommendations <- paste0(recommendations,
        "<li>Explore correlations in the <strong>Correlation Matrix</strong> tab</li>"
      )
    }
    
    if(length(data$categorical_vars) >= 2) {
      recommendations <- paste0(recommendations,
        "<li>Analyze relationships in the <strong>Cross-Tabulation</strong> tab</li>"
      )
    }
    
    if(length(data$numeric_vars) > 0 && length(data$categorical_vars) > 0) {
      recommendations <- paste0(recommendations,
        "<li>Compare groups in the <strong>Two Variables</strong> tab</li>"
      )
    }
    
    recommendations <- paste0(recommendations, "</ul>")
    
    HTML(paste0(
      '<div style="background-color: #E3F2FD; padding: 15px; border-radius: 5px; border-left: 4px solid #2196F3;">',
      '<h4><i class="fa fa-lightbulb"></i> Data Insights:</h4>',
      '<ul>',
      paste(insights, collapse = "\n"),
      '</ul>',
      '</div>',
      '<br>',
      '<div style="background-color: #F3E5F5; padding: 15px; border-radius: 5px; border-left: 4px solid #9C27B0;">',
      recommendations,
      '</div>'
    ))
  })
  
  # Download report (enhanced)
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("data_summary_report_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(data$raw)
      
      missing_counts <- colSums(is.na(data$raw))
      completeness <- 100 * (1 - sum(missing_counts) / (nrow(data$raw) * ncol(data$raw)))
      
      report_text <- paste(
        "COMPREHENSIVE DATA SUMMARY REPORT",
        strrep("=", 60),
        "",
        paste("Generated on:", Sys.time()),
        "",
        "DATASET OVERVIEW:",
        strrep("-", 60),
        paste("Total Records:", format(nrow(data$raw), big.mark = ",")),
        paste("Total Variables:", ncol(data$raw)),
        paste("Numerical Variables:", length(data$numeric_vars)),
        paste("Categorical Variables:", length(data$categorical_vars)),
        "",
        "DATA QUALITY:",
        strrep("-", 60),
        paste("Completeness:", sprintf("%.2f%%", completeness)),
        paste("Total Missing Values:", format(sum(missing_counts), big.mark = ",")),
        "",
        "VARIABLE LIST:",
        strrep("-", 60),
        paste(sapply(1:ncol(data$raw), function(i) {
          type <- ifelse(names(data$raw)[i] %in% data$numeric_vars, 
                        "[Numerical]", "[Categorical]")
          sprintf("%2d. %-30s %s", i, names(data$raw)[i], type)
        }), collapse = "\n"),
        "",
        "Please refer to the interactive dashboard for detailed visualizations.",
        "",
        strrep("=", 60),
        sep = "\n"
      )
      writeLines(report_text, file)
    }
  )

  # ===== SIMPLE LINEAR REGRESSION SERVER =====

  output$slrYSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("slrY", "Response Variable (y):", choices = data$numeric_vars)
  })

  output$slrXSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("slrX", "Predictor Variable (x):", choices = data$numeric_vars)
  })

  slr_model <- reactiveVal(NULL)

  observeEvent(input$runSLR, {
    req(data$raw, input$slrY, input$slrX)

    y <- data$raw[[input$slrY]]
    x <- data$raw[[input$slrX]]
    df_slr <- data.frame(y = y, x = x)

    formula_slr <- if (input$slrNoIntercept) y ~ 0 + x else y ~ x
    model <- lm(formula_slr, data = df_slr)
    slr_model(model)

    # Scatter plot
    output$slrScatter <- renderPlot({
      plot(x, y,
           xlab = input$slrX, ylab = input$slrY,
           main = paste("Scatter Plot:", input$slrY, "vs", input$slrX),
           pch = 19, col = rgb(0.2, 0.5, 0.8, 0.6), cex = 1.2)
      abline(model, col = "red", lwd = 2)
      legend("topleft",
             legend = paste0("y = ", round(coef(model)[1], 4),
                             ifelse(length(coef(model)) > 1,
                                    paste0(" + ", round(coef(model)[2], 4), " x"), "")),
             col = "red", lwd = 2, bty = "n")
    })

    # Model summary
    output$slrSummary <- renderPrint({
      cat("Fitted Model:\n")
      print(model)
      cat("\n")
      print(summary(model))
    })

    # ANOVA
    output$slrAnova <- renderPrint({
      cat("ANOVA Table (Overall Significance)\n")
      cat(strrep("=", 50), "\n\n")
      print(anova(model))
    })

    # Confidence intervals for coefficients
    output$slrConfint <- renderPrint({
      cat("95% Confidence Intervals for Coefficients\n")
      cat(strrep("=", 50), "\n\n")
      print(confint(model, level = 0.95))
    })

    # Correlation analysis
    output$slrCorrelation <- renderPrint({
      cat("Correlation Analysis\n")
      cat(strrep("=", 50), "\n\n")

      cat("Pearson (parametric):\n")
      print(cor.test(y, x, method = "pearson"))

      cat("\nSpearman (non-parametric):\n")
      tryCatch(print(cor.test(y, x, method = "spearman")),
               warning = function(w) cat(conditionMessage(w), "\n"))

      cat("\nKendall (non-parametric):\n")
      tryCatch(print(cor.test(y, x, method = "kendall")),
               warning = function(w) cat(conditionMessage(w), "\n"))
    })
  })

  # SLR Prediction
  observeEvent(input$runSLRPredict, {
    req(slr_model(), input$slrNewX)

    tryCatch({
      x0 <- as.numeric(trimws(strsplit(input$slrNewX, ",")[[1]]))
      new_data <- data.frame(x = x0)

      x_orig <- data$raw[[input$slrX]]

      output$slrPredInterval <- DT::renderDT({
        pred <- as.data.frame(
          predict(slr_model(), newdata = new_data, interval = "prediction", level = 0.95)
        )
        pred <- cbind(`X Value` = x0, round(pred, 4))
        DT::datatable(pred,
                      options = list(dom = "t", pageLength = nrow(pred), scrollX = TRUE),
                      rownames = FALSE)
      })
      output$slrConfInterval <- DT::renderDT({
        conf <- as.data.frame(
          predict(slr_model(), newdata = new_data, interval = "confidence", level = 0.95)
        )
        conf <- cbind(`X Value` = x0, round(conf, 4))
        DT::datatable(conf,
                      options = list(dom = "t", pageLength = nrow(conf), scrollX = TRUE),
                      rownames = FALSE)
      })
      output$slrInterpExtrap <- renderPrint({
        check <- ifelse(x0 > max(x_orig, na.rm = TRUE) | x0 < min(x_orig, na.rm = TRUE),
                        "EXTRAPOLATION", "interpolation")
        for (i in seq_along(x0)) {
          cat(sprintf("x = %-8.3f  =>  %s\n", x0[i], check[i]))
        }
      })
    }, error = function(e) {
      showNotification(paste("Prediction error:", e$message), type = "error")
    })
  })

  # ===== MULTIPLE LINEAR REGRESSION SERVER =====

  output$mlrYSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("mlrY", "Response Variable (y):", choices = data$numeric_vars)
  })

  output$mlrXSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("mlrX", "Predictor Variables (x1, x2, ...):",
                choices = data$numeric_vars, multiple = TRUE,
                selected = data$numeric_vars[1:min(2, length(data$numeric_vars))])
  })

  mlr_model <- reactiveVal(NULL)
  mlr_xvars <- reactiveVal(NULL)

  observeEvent(input$runMLR, {
    req(data$raw, input$mlrY, input$mlrX)

    if (length(input$mlrX) < 2) {
      showNotification("Please select at least 2 predictor variables for MLR.", type = "error")
      return()
    }

    y    <- data$raw[[input$mlrY]]
    xvars <- input$mlrX
    mlr_xvars(xvars)

    # Build a clean data frame with safe names to use in formulas
    df_mlr <- data$raw[, c(input$mlrY, xvars), drop = FALSE]
    # Backtick-quote names for formula construction
    bt <- function(v) paste0("`", v, "`")
    formula_rhs <- paste(sapply(xvars, bt), collapse = " + ")
    formula_mlr <- if (input$mlrNoIntercept) {
      as.formula(paste(bt(input$mlrY), "~ 0 +", formula_rhs))
    } else {
      as.formula(paste(bt(input$mlrY), "~", formula_rhs))
    }

    model <- lm(formula_mlr, data = df_mlr)
    mlr_model(model)

    # Scatter plot matrix
    output$mlrPairs <- renderPlot({
      all_vars <- c(input$mlrY, xvars)
      pairs(data$raw[, all_vars, drop = FALSE],
            main = "Scatter Plot Matrix",
            pch = 19, col = rgb(0.2, 0.5, 0.8, 0.4), cex = 0.7)
    })

    # Correlation matrix heatmap
    output$mlrCorrPlot <- renderPlot({
      all_vars <- c(input$mlrY, xvars)
      corr_mat <- cor(data$raw[, all_vars, drop = FALSE], use = "complete.obs")
      corrplot.mixed(corr_mat,
                     lower = "number", upper = "circle",
                     outline = TRUE, mar = c(1, 1, 0, 0),
                     tl.cex = 0.7, tl.col = "black",
                     cl.cex = 0.6, cl.ratio = 0.2,
                     number.cex = 0.85, number.digits = 3)
    })

    # Observed vs predicted fit line plot
    output$mlrFitLine <- renderPlot({
      y_obs <- model.response(model.frame(model))
      y_hat <- fitted(model)
      plot(y_hat, y_obs,
           xlab = "Predicted values",
           ylab = "Observed values",
           main = "Observed vs Predicted",
           pch = 19, col = rgb(0.2, 0.5, 0.8, 0.6))
      abline(0, 1, col = "red", lwd = 2, lty = 2)      # perfect fit line
      abline(lm(y_obs ~ y_hat), col = "darkgreen", lwd = 2)  # empirical fit line
      legend("topleft",
             legend = c("Perfect fit (y = x)", "Observed~Predicted fit"),
             col = c("red", "darkgreen"), lty = c(2, 1), lwd = 2, bty = "n")
    })

    # Model summary
    output$mlrSummary <- renderPrint({
      cat("Fitted Model:\n")
      print(model)
      cat("\n")
      print(summary(model))
    })

    # ANOVA table
    output$mlrAnova <- renderPrint({
      cat("ANOVA Table (Overall Significance)\n")
      cat(strrep("=", 50), "\n\n")
      print(anova(model))
    })

    # Partial F-tests: test each predictor (H0: beta_i = 0)
    output$mlrPartialF <- renderPrint({
      cat("Partial F-Tests (each predictor vs. full model)\n")
      cat(strrep("=", 55), "\n\n")
      for (xv in xvars) {
        reduced_vars <- setdiff(xvars, xv)
        if (length(reduced_vars) == 0) {
          red_formula <- if (input$mlrNoIntercept) {
            as.formula(paste(bt(input$mlrY), "~ 0 + 1"))
          } else {
            as.formula(paste(bt(input$mlrY), "~ 1"))
          }
        } else {
          red_rhs <- paste(sapply(reduced_vars, bt), collapse = " + ")
          red_formula <- if (input$mlrNoIntercept) {
            as.formula(paste(bt(input$mlrY), "~ 0 +", red_rhs))
          } else {
            as.formula(paste(bt(input$mlrY), "~", red_rhs))
          }
        }
        red_model <- lm(red_formula, data = df_mlr)
        cat(sprintf("H0: beta(%s) = 0\n", xv))
        print(anova(red_model, model))
        cat("\n")
      }
    })

    # Confidence intervals
    output$mlrConfint <- renderPrint({
      cat("95% Confidence Intervals for Coefficients\n")
      cat(strrep("=", 50), "\n\n")
      print(confint(model, level = 0.95))
    })

    # VIF
    output$mlrVIF <- renderPrint({
      if (length(xvars) >= 2) {
        cat("Variance Inflation Factors (VIF)\n")
        cat(strrep("=", 40), "\n\n")
        vif_vals <- vif(model)
        print(round(vif_vals, 4))
        cat("\nInterpretation:\n")
        cat("  VIF < 5  : No serious multicollinearity\n")
        cat("  5 <= VIF < 10 : Moderate multicollinearity\n")
        cat("  VIF >= 10 : Severe multicollinearity\n")
      } else {
        cat("VIF requires at least 2 predictors.\n")
      }
    })

    output$mlrVIFPlot <- renderPlot({
      if (length(xvars) >= 2) {
        vif_vals <- vif(model)
        bar_colors <- ifelse(vif_vals >= 10, "#F44336",
                             ifelse(vif_vals >= 5, "#FF9800", "steelblue"))
        barplot(vif_vals,
                main = "VIF Values",
                ylab = "VIF",
                col = bar_colors,
                horiz = FALSE,
                ylim = c(0, max(max(vif_vals) * 1.15, 11)),
                las = 2)
        abline(h = 5,  col = "orange", lwd = 2, lty = 2)
        abline(h = 10, col = "red",    lwd = 2, lty = 3)
        legend("topright",
               legend = c("VIF = 5 (moderate)", "VIF = 10 (severe)"),
               col = c("orange", "red"), lty = c(2, 3), lwd = 2, bty = "n", cex = 0.85)
      } else {
        plot.new()
        text(0.5, 0.5, "Need >= 2 predictors for VIF", cex = 1.4)
      }
    })

    # Dynamic new-x inputs for prediction
    # We sanitize input IDs: Shiny requires IDs without spaces/special chars
    make_input_id <- function(v) paste0("mlr_x_", gsub("[^a-zA-Z0-9]", "_", v))

    output$mlrNewXInputs <- renderUI({
      inputs <- lapply(xvars, function(xv) {
        column(max(2, floor(12 / length(xvars))),
               textInput(make_input_id(xv),
                         paste("Values for", xv, "(comma-sep):"),
                         placeholder = "e.g. 25, 10"))
      })
      do.call(fluidRow, inputs)
    })
  })

  # MLR Prediction
  observeEvent(input$runMLRPredict, {
    req(mlr_model(), mlr_xvars())
    xvars <- mlr_xvars()

    tryCatch({
      # Use the same sanitized ID helper to look up each input value
      make_input_id <- function(v) paste0("mlr_x_", gsub("[^a-zA-Z0-9]", "_", v))

      # Collect entered values; name by original column name for predict()
      new_vals <- lapply(xvars, function(xv) {
        raw_txt <- input[[make_input_id(xv)]]
        as.numeric(trimws(strsplit(raw_txt, ",")[[1]]))
      })
      names(new_vals) <- xvars
      new_data <- as.data.frame(new_vals, check.names = FALSE)

      output$mlrPredInterval <- DT::renderDT({
        pred <- as.data.frame(
          predict(mlr_model(), newdata = new_data, interval = "prediction", level = 0.95)
        )
        pred <- cbind(Observation = seq_len(nrow(pred)), round(pred, 4))
        DT::datatable(pred,
                      options = list(dom = "t", pageLength = nrow(pred), scrollX = TRUE),
                      rownames = FALSE)
      })
      output$mlrConfInterval <- DT::renderDT({
        conf <- as.data.frame(
          predict(mlr_model(), newdata = new_data, interval = "confidence", level = 0.95)
        )
        conf <- cbind(Observation = seq_len(nrow(conf)), round(conf, 4))
        DT::datatable(conf,
                      options = list(dom = "t", pageLength = nrow(conf), scrollX = TRUE),
                      rownames = FALSE)
      })

      # Leverage-based check
      output$mlrInterpExtrap <- renderPrint({
        hii      <- hatvalues(mlr_model())
        h_new    <- (predict(mlr_model(), newdata = new_data,
                             interval = "confidence", se.fit = TRUE)$se.fit /
                       sigma(mlr_model()))^2
        result   <- ifelse(h_new > max(hii), "EXTRAPOLATION", "interpolation")
        for (i in seq_along(result)) {
          cat(sprintf("Observation %d  =>  %s  (h_new=%.4f, max(hii)=%.4f)\n",
                      i, result[i], h_new[i], max(hii)))
        }
      })
    }, error = function(e) {
      showNotification(paste("Prediction error:", e$message), type = "error")
    })
  })

  # ===== INDICATOR VARIABLE SERVER =====

  output$indYSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("indY", "Response (Y):", choices = data$numeric_vars)
  })

  output$indFactorSelect <- renderUI({
    req(data$raw)
    if (length(data$categorical_vars) == 0) {
      selectInput("indFactor", "Categorical Variable:", choices = character(0))
    } else {
      selectInput("indFactor", "Categorical Variable:", choices = data$categorical_vars)
    }
  })

  output$indFactor2Select <- renderUI({
    req(data$raw)
    if (length(data$categorical_vars) <= 1) {
      selectInput("indFactor2", "Categorical Variable 2:", choices = character(0))
    } else {
      choices2 <- setdiff(data$categorical_vars, input$indFactor %||% "")
      selectInput("indFactor2", "Categorical Variable 2:", choices = choices2)
    }
  })

  output$cleanLogVarsUI <- renderUI({
    req(data$numeric_vars)
    selectizeInput("cleanLogVars", "Log Transform Numeric Vars:",
                   choices = data$numeric_vars, multiple = TRUE,
                   options = list(placeholder = "Optional"))
  })

  observeEvent(input$applyCleaning, {
    req(data$raw_base)
    withProgress(message = "Applying data cleaning...", value = 0, {
      incProgress(0.4)
      data$raw <- apply_cleaning_pipeline(data$raw_base,
                                          missing_mode = input$cleanMissing %||% "keep",
                                          outlier_mode = input$cleanOutliers %||% "remove",
                                          log_vars = input$cleanLogVars %||% NULL)
      incProgress(0.4)
      refresh_var_types(data$raw)
      data$analyzed <- TRUE
      incProgress(0.2)
    })
    showNotification("Cleaning pipeline applied.", type = "message")
  })

  output$globalVarSearchUI <- renderUI({
    all_vars <- c(data$numeric_vars, data$categorical_vars)
    selectizeInput("globalVarSearch", "Global Variable Search:",
                   choices = all_vars, multiple = FALSE,
                   options = list(placeholder = "Find variable"))
  })

  observeEvent(input$globalVarSearch, {
    req(input$globalVarSearch)
    var_selected <- input$globalVarSearch
    if (var_selected %in% data$numeric_vars) {
      updateTabItems(session, "tabs", selected = "varanalysis")
      updateSelectInput(session, "analysisMode", selected = "single")
      updateSelectInput(session, "singleVarType", selected = "Numerical")
      updateSelectInput(session, "singleVar", selected = var_selected)
    } else {
      updateTabItems(session, "tabs", selected = "catsuite")
      updateSelectInput(session, "catSuiteMode", selected = "singlecat")
      updateSelectInput(session, "catVar", selected = var_selected)
    }
    showNotification(paste("Jumped to variable:", var_selected), type = "message", duration = 2)
  })

  observeEvent(input$darkModeToggle, {
    session$sendCustomMessage("setDarkMode", list(enabled = isTRUE(input$darkModeToggle)))
  }, ignoreInit = TRUE)

  # Quick actions
  observeEvent(input$qaSample, {
    data$raw_base <- default_data
    data$raw <- apply_cleaning_pipeline(data$raw_base,
                                        missing_mode = input$cleanMissing %||% "keep",
                                        outlier_mode = input$cleanOutliers %||% "remove",
                                        log_vars = input$cleanLogVars %||% NULL)
    refresh_var_types(data$raw)
    data$analyzed <- TRUE
    updateTabItems(session, "tabs", selected = "preview")
    showNotification("Sample/default dataset loaded.", type = "message")
  })

  observeEvent(input$qaReset, {
    data$raw <- data$raw_base
    refresh_var_types(data$raw)
    data$analyzed <- TRUE
    showNotification("Reset to base dataset (without current cleaning transforms).", type = "message")
  })

  observeEvent(input$presetPriceMileage, {
    if (all(c("selling_price_sar_1000", "km_driven") %in% names(data$raw))) {
      updateTabItems(session, "tabs", selected = "varanalysis")
      updateSelectInput(session, "analysisMode", selected = "multi")
      updateSelectInput(session, "multiVarCount", selected = "2")
      updateSelectInput(session, "multiVarType1", selected = "Numerical")
      updateSelectInput(session, "multiVarType2", selected = "Numerical")
      updateSelectInput(session, "multiVar1", selected = "selling_price_sar_1000")
      updateSelectInput(session, "multiVar2", selected = "km_driven")
      showNotification("Preset loaded: Price vs Mileage", type = "message")
    } else {
      showNotification("Preset variables not found in current dataset.", type = "warning")
    }
  })

  observeEvent(input$presetBrandCompare, {
    if (all(c("brand", "selling_price_sar_1000") %in% names(data$raw))) {
      updateTabItems(session, "tabs", selected = "varanalysis")
      updateSelectInput(session, "analysisMode", selected = "multi")
      updateSelectInput(session, "multiVarCount", selected = "2")
      updateSelectInput(session, "multiVarType1", selected = "Categorical")
      updateSelectInput(session, "multiVarType2", selected = "Numerical")
      updateSelectInput(session, "multiVar1", selected = "brand")
      updateSelectInput(session, "multiVar2", selected = "selling_price_sar_1000")
      showNotification("Preset loaded: Brand comparison", type = "message")
    } else {
      showNotification("Preset variables not found in current dataset.", type = "warning")
    }
  })

  # Session save/load
  output$downloadState <- downloadHandler(
    filename = function() paste0("session_state_", Sys.Date(), ".rds"),
    content = function(file) {
      saveRDS(list(
        raw_base = data$raw_base,
        raw = data$raw,
        analyzed = data$analyzed,
        cleanMissing = input$cleanMissing,
        cleanOutliers = input$cleanOutliers,
        cleanLogVars = input$cleanLogVars
      ), file = file)
    }
  )

  observeEvent(input$loadState, {
    req(input$loadState$datapath)
    st <- tryCatch(readRDS(input$loadState$datapath), error = function(e) NULL)
    if (is.null(st)) {
      showNotification("Could not load session file.", type = "error")
      return()
    }
    if (!is.null(st$raw_base)) data$raw_base <- as.data.frame(st$raw_base, check.names = FALSE)
    if (!is.null(st$raw)) data$raw <- as.data.frame(st$raw, check.names = FALSE)
    data$analyzed <- isTRUE(st$analyzed)
    refresh_var_types(data$raw)
    if (!is.null(st$cleanMissing)) updateSelectInput(session, "cleanMissing", selected = st$cleanMissing)
    if (!is.null(st$cleanOutliers)) updateSelectInput(session, "cleanOutliers", selected = st$cleanOutliers)
    if (!is.null(st$cleanLogVars)) updateSelectizeInput(session, "cleanLogVars", selected = st$cleanLogVars)
    showNotification("Session loaded successfully.", type = "message")
  })

  # Non-blocking onboarding message (version-compatible one-time pattern)
  onboarding_shown <- reactiveVal(FALSE)
  observe({
    if (isTRUE(onboarding_shown())) return()
    onboarding_shown(TRUE)
    showNotification(
      "Welcome! Use Quick Actions, Global Search, and Data Cleaning from the top panel.",
      type = "message",
      duration = 6
    )
  })

  output$indXSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("indX", "Numeric X (optional):",
                choices = c("None (group means only)" = "__none__", data$numeric_vars),
                selected = "__none__")
  })

  output$indRefSelect <- renderUI({
    req(data$raw, input$indFactor)
    fac_vals <- as.factor(data$raw[[input$indFactor]])
    lvls <- levels(fac_vals)
    selectInput("indRef", "Reference level:", choices = lvls, selected = lvls[1])
  })

  output$indRef2Select <- renderUI({
    req(data$raw, input$indFactor2)
    fac_vals <- as.factor(data$raw[[input$indFactor2]])
    lvls <- levels(fac_vals)
    selectInput("indRef2", "Reference level 2:", choices = lvls, selected = lvls[1])
  })

  ind_model   <- reactiveVal(NULL)
  ind_reduced <- reactiveVal(NULL)
  ind_data    <- reactiveVal(NULL)

  observeEvent(input$runIndicator, {
    req(data$raw, input$indY, input$indFactor, input$indRef)

    if (length(data$categorical_vars) == 0) {
      showNotification("No categorical variables are available for indicator-variable analysis.", type = "error")
      return()
    }

    n_fac <- as.integer(input$indFactorCount %||% "1")
    use_f2 <- !is.na(n_fac) && n_fac == 2
    if (use_f2) req(input$indFactor2, input$indRef2)

    if (use_f2 && identical(input$indFactor, input$indFactor2)) {
      showNotification("Please choose two different categorical variables.", type = "error")
      return()
    }

    use_x <- !is.null(input$indX) && input$indX != "__none__"
    needed_cols <- c(input$indY, input$indFactor, if (use_f2) input$indFactor2 else NULL, if (use_x) input$indX else NULL)
    df <- data$raw[, needed_cols, drop = FALSE]
    df <- df[complete.cases(df), , drop = FALSE]

    if (nrow(df) < 4) {
      showNotification("Not enough complete rows to fit the indicator model.", type = "error")
      return()
    }

    names(df)[which(names(df) == input$indY)] <- "y"
    names(df)[which(names(df) == input$indFactor)] <- "f1"
    if (use_f2) names(df)[which(names(df) == input$indFactor2)] <- "f2"
    if (use_x) names(df)[which(names(df) == input$indX)] <- "x"

    df$f1 <- as.factor(df$f1)
    if (nlevels(df$f1) < 2) {
      showNotification("Factor 1 must have at least two levels.", type = "error")
      return()
    }
    if (!(input$indRef %in% levels(df$f1))) {
      showNotification("Reference level 1 not found in filtered data.", type = "error")
      return()
    }
    df$f1 <- relevel(df$f1, ref = input$indRef)

    if (use_f2) {
      df$f2 <- as.factor(df$f2)
      if (nlevels(df$f2) < 2) {
        showNotification("Factor 2 must have at least two levels.", type = "error")
        return()
      }
      if (!(input$indRef2 %in% levels(df$f2))) {
        showNotification("Reference level 2 not found in filtered data.", type = "error")
        return()
      }
      df$f2 <- relevel(df$f2, ref = input$indRef2)
    }

    f_main <- if (!use_f2) {
      if (use_x) {
        if (isTRUE(input$indInteraction)) y ~ x * f1 else y ~ x + f1
      } else {
        y ~ f1
      }
    } else {
      if (use_x) {
        if (isTRUE(input$indInteraction)) y ~ x * f1 + x * f2 else y ~ x + f1 + f2
      } else {
        y ~ f1 + f2
      }
    }

    fit <- lm(f_main, data = df)
    ind_model(fit)
    ind_data(df)

    fit_reduced <- if (!use_f2) {
      if (use_x) {
        if (isTRUE(input$indInteraction)) lm(y ~ x + f1, data = df) else lm(y ~ x, data = df)
      } else lm(y ~ 1, data = df)
    } else {
      if (use_x) {
        if (isTRUE(input$indInteraction)) lm(y ~ x + f1 + f2, data = df) else lm(y ~ x, data = df)
      } else lm(y ~ 1, data = df)
    }
    ind_reduced(fit_reduced)

    output$indModelInfo <- renderPrint({
      cat("Indicator Variable Model Setup\n")
      cat(strrep("=", 45), "\n\n")
      cat("Rows used                :", nrow(df), "\n")
      cat("Number of factors        :", ifelse(use_f2, 2, 1), "\n")
      cat("Factor 1 (f1)            :", input$indFactor, "\n")
      cat("Levels in f1             :", nlevels(df$f1), " => dummies:", nlevels(df$f1) - 1, "\n")
      cat("Reference level f1       :", input$indRef, "\n")
      if (use_f2) {
        cat("Factor 2 (f2)            :", input$indFactor2, "\n")
        cat("Levels in f2             :", nlevels(df$f2), " => dummies:", nlevels(df$f2) - 1, "\n")
        cat("Reference level f2       :", input$indRef2, "\n")
      }
      if (use_x) cat("Numeric predictor (x)    :", input$indX, "\n")
      cat("Interaction with X        :", ifelse(use_x && isTRUE(input$indInteraction), "Yes", "No"), "\n\n")
      cat("Fitted formula:\n")
      print(formula(fit))
    })

    output$indSummary <- renderPrint({
      print(summary(fit))
    })

    output$indAnova <- renderPrint({
      cat("ANOVA (Current Model)\n")
      cat(strrep("=", 45), "\n\n")
      print(anova(fit))
      cat("\nExtra Sum-of-Squares Comparison\n")
      cat(strrep("-", 45), "\n")
      if (use_x && isTRUE(input$indInteraction)) {
        cat("H0: All X×factor interaction terms = 0\n\n")
      } else if (use_x) {
        cat("H0: All indicator terms = 0 after controlling for X\n\n")
      } else {
        cat("H0: All factor effects = 0\n\n")
      }
      print(anova(fit_reduced, fit))
    })

    output$indEquations <- renderPrint({
      if (!use_f2) {
        lvls <- levels(df$f1)
        cat("Baseline level (f1):", lvls[1], "\n\n")
        for (lv in lvls) {
          if (use_x) {
            new0 <- data.frame(x = 0, f1 = factor(lv, levels = lvls))
            new1 <- data.frame(x = 1, f1 = factor(lv, levels = lvls))
            b0 <- as.numeric(predict(fit, newdata = new0))
            b1 <- as.numeric(predict(fit, newdata = new1)) - b0
            cat(sprintf("%s: y = %.4f + %.4f*x\n", lv, b0, b1))
          } else {
            new0 <- data.frame(f1 = factor(lv, levels = lvls))
            m <- as.numeric(predict(fit, newdata = new0))
            cat(sprintf("%s: y = %.4f\n", lv, m))
          }
        }
      } else {
        lv1 <- levels(df$f1)
        lv2 <- levels(df$f2)
        cat("Fitted equations by (Factor1, Factor2) combinations:\n")
        cat(strrep("=", 55), "\n\n")
        for (a in lv1) for (b in lv2) {
          if (use_x) {
            new0 <- data.frame(x = 0, f1 = factor(a, levels = lv1), f2 = factor(b, levels = lv2))
            new1 <- data.frame(x = 1, f1 = factor(a, levels = lv1), f2 = factor(b, levels = lv2))
            b0 <- as.numeric(predict(fit, newdata = new0))
            b1 <- as.numeric(predict(fit, newdata = new1)) - b0
            cat(sprintf("(%s, %s): y = %.4f + %.4f*x\n", a, b, b0, b1))
          } else {
            new0 <- data.frame(f1 = factor(a, levels = lv1), f2 = factor(b, levels = lv2))
            m <- as.numeric(predict(fit, newdata = new0))
            cat(sprintf("(%s, %s): y = %.4f\n", a, b, m))
          }
        }
      }
    })

    output$indPlot <- renderPlot({
      if (use_x) {
        grp <- if (use_f2) interaction(df$f1, df$f2, sep = " | ") else df$f1
        grp <- as.factor(grp)
        cols <- setNames(rainbow(nlevels(grp)), levels(grp))
        plot(df$x, df$y,
             xlab = input$indX, ylab = input$indY,
             main = "Indicator Variable Fit",
             pch = 19, col = cols[as.character(grp)])
        x_grid <- seq(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE), length.out = 120)
        for (g in levels(grp)) {
          idx <- as.character(grp) == g
          if (!any(idx)) next
          if (use_f2) {
            vals <- strsplit(g, " \\| ")[[1]]
            new_df <- data.frame(x = x_grid,
                                 f1 = factor(vals[1], levels = levels(df$f1)),
                                 f2 = factor(vals[2], levels = levels(df$f2)))
          } else {
            new_df <- data.frame(x = x_grid, f1 = factor(g, levels = levels(df$f1)))
          }
          y_hat <- predict(fit, newdata = new_df)
          lines(x_grid, y_hat, col = cols[g], lwd = 2)
        }
        legend("topright", legend = levels(grp), col = cols, pch = 19, lwd = 2, bty = "n", cex = 0.85)
      } else {
        grp <- if (use_f2) interaction(df$f1, df$f2, sep = " | ") else df$f1
        boxplot(df$y ~ grp,
                col = "lightblue", border = "steelblue",
                xlab = if (use_f2) "Factor 1 | Factor 2" else input$indFactor,
                ylab = input$indY,
                main = "Indicator Variable Model (Group Means)",
                las = 2)
      }
    })

    output$indInterpretation <- renderUI({
      p_base <- if (!use_f2) {
        paste0("One factor selected: ", nlevels(df$f1) - 1, " dummy variable(s) created for Factor 1.")
      } else {
        paste0("Two factors selected: ", nlevels(df$f1) - 1, " dummy variable(s) for Factor 1 and ",
               nlevels(df$f2) - 1, " for Factor 2.")
      }
      p_shape <- if (use_x && isTRUE(input$indInteraction)) {
        "Interaction with X is included, so slopes can vary by factor level(s)."
      } else if (use_x) {
        "No interaction with X, so slopes are common while intercepts shift by factor level(s)."
      } else {
        "No numeric predictor: model compares means across factor-defined groups."
      }
      p_ref <- if (!use_f2) {
        paste0("Baseline for Factor 1: ", input$indRef, ".")
      } else {
        paste0("Baselines: Factor 1 = ", input$indRef, ", Factor 2 = ", input$indRef2, ".")
      }
      tags$div(tags$p(p_base), tags$p(p_shape), tags$p(p_ref))
    })
  })

  # ===== MODEL ADEQUACY SERVER =====

  output$maXHint <- renderUI({
    req(data$numeric_vars)
    if (input$maModelType == "slr") {
      tags$p(class="hint-text",
             icon("info-circle"), " Uses the SLR model fitted in the SLR tab.")
    } else {
      tags$p(class="hint-text",
             icon("info-circle"), " Uses the MLR model fitted in the MLR tab.")
    }
  })

  observeEvent(input$runMA, {
    # Grab the currently fitted model
    mod <- if (input$maModelType == "slr") slr_model() else mlr_model()
    req(mod)

    ei <- resid(mod)
    ti <- MASS::studres(mod)
    n  <- length(ei)

    # Residual table
    output$maResidTable <- DT::renderDT({
      DT::datatable(
        data.frame(
          Obs      = seq_len(n),
          Residual = round(ei, 4),
          Std_Res  = round(rstandard(mod), 4),
          Stud_Res = round(ti, 4),
          Fitted   = round(fitted(mod), 4)
        ),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # --- Normality ---
    output$maQQPlot <- renderPlot({
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      car::qqPlot(ei, main = "QQ Plot (Raw Residuals)",
                  ylab = "Residuals", col = "steelblue", col.lines = "red")
      car::qqPlot(ti, main = "QQ Plot (Studentised Residuals)",
                  ylab = "Studentised Residuals", col = "darkorange", col.lines = "red")
    })

    output$maShapiro <- renderPrint({
      cat("Shapiro-Wilk Normality Test\n")
      cat(strrep("=", 40), "\n\n")
      sw <- shapiro.test(ei)
      print(sw)
      cat("\nInterpretation:\n")
      if (sw$p.value < 0.05) {
        cat("  p < 0.05  => Reject H0: residuals NOT normally distributed.\n")
      } else {
        cat("  p >= 0.05 => Fail to reject H0: residuals appear normal.\n")
      }
    })

    # --- Homoscedasticity ---
    output$maResFitted <- renderPlot({
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      plot(fitted(mod), ei,
           xlab = "Fitted values", ylab = "Residuals",
           main = "(a) Residuals vs Fitted",
           col = "steelblue", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      plot(fitted(mod), ti,
           xlab = "Fitted values", ylab = "Studentised Residuals",
           main = "(b) Studentised Residuals vs Fitted",
           col = "darkorange", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      abline(h = c(-2, 2), col = "grey50", lty = 2)
    })

    output$maBPTest <- renderPrint({
      cat("Breusch-Pagan Test (Homoscedasticity)\n")
      cat(strrep("=", 40), "\n\n")
      bp <- lmtest::bptest(mod)
      print(bp)
      cat("\nInterpretation:\n")
      if (bp$p.value < 0.05) {
        cat("  p < 0.05  => Reject H0: evidence of heteroscedasticity.\n")
      } else {
        cat("  p >= 0.05 => Fail to reject H0: no strong evidence of heteroscedasticity.\n")
      }
    })

    # --- Independence ---
    output$maResOrder <- renderPlot({
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      plot(seq_len(n), ei,
           type = "o", pch = 19, col = "steelblue",
           xlab = "Observation Order", ylab = "Residuals",
           main = "(a) Residuals vs Order")
      abline(h = 0, col = "red", lwd = 2)
      plot(seq_len(n), ti,
           type = "o", pch = 19, col = "darkorange",
           xlab = "Observation Order", ylab = "Studentised Residuals",
           main = "(b) Studentised vs Order")
      abline(h = 0, col = "red", lwd = 2)
    })

    output$maDWTest <- renderPrint({
      cat("Durbin-Watson Test (Independence)\n")
      cat(strrep("=", 40), "\n\n")
      dw <- lmtest::dwtest(mod)
      print(dw)
      cat("\nInterpretation:\n")
      if (dw$p.value < 0.05) {
        cat("  p < 0.05  => Reject H0: evidence of autocorrelation.\n")
      } else {
        cat("  p >= 0.05 => Fail to reject H0: residuals appear independent.\n")
      }
    })

    # --- Added-variable plots ---
    output$maAVPlots <- renderPlot({
      tryCatch(
        car::avPlots(mod, main = "Added-Variable (Partial Regression) Plots",
                     col = "steelblue", col.lines = "red", pch = 19),
        error = function(e) {
          plot.new()
          text(0.5, 0.5,
               paste("Added-variable plots require >= 2 predictors.\n", e$message),
               cex = 1.2)
        }
      )
    })

    # --- Lack-of-fit ---
    output$maLackFit <- renderPrint({
      cat("Lack-of-Fit Test (ANOVA Pure Error)\n")
      cat(strrep("=", 50), "\n\n")
      if (!requireNamespace("EnvStats", quietly = TRUE)) {
        cat("Package 'EnvStats' is required for this test.\n")
        cat("Install it with: install.packages('EnvStats')\n")
        return(invisible(NULL))
      }
      tryCatch({
        # Re-extract x and y from the model
        mf <- model.frame(mod)
        y_lof <- mf[[1]]
        x_lof <- mf[[2]]
        lof_mod <- lm(y_lof ~ x_lof)
        print(EnvStats::anovaPE(lof_mod))
        cat("\nNote: This test requires replicate X values.\n")
        cat("If X values are all unique, the test cannot be performed.\n")
      }, error = function(e) {
        cat("Lack-of-fit test could not be completed:\n")
        cat(e$message, "\n")
        cat("\nThis test requires replicate X values in your dataset.\n")
      })
    })
  })

  # ===== BOX-COX & BOX-TIDWELL SERVER =====

  output$btYSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("btY", "Response Variable (y):", choices = data$numeric_vars)
  })

  output$btXSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("btX", "Predictor Variable (x):", choices = data$numeric_vars)
  })

  bt_orig_model  <- reactiveVal(NULL)
  bt_trans_model <- reactiveVal(NULL)
  bt_info        <- reactiveVal(list())

  observeEvent(input$runBT, {
    req(data$raw, input$btY, input$btX)

    y_bt <- data$raw[[input$btY]]
    x_bt <- data$raw[[input$btX]]

    # Check Y > 0 for Box-Cox
    if (input$btMethod == "boxcox" && any(y_bt <= 0, na.rm = TRUE)) {
      showNotification(
        "Box-Cox requires all Y values to be strictly positive. Please choose a different Y.",
        type = "error", duration = 6)
      return()
    }
    # Check X > 0 for Box-Tidwell
    if (input$btMethod == "boxtidwell" && any(x_bt <= 0, na.rm = TRUE)) {
      showNotification(
        "Box-Tidwell requires all X values to be strictly positive. Please choose a different X.",
        type = "error", duration = 6)
      return()
    }

    df_bt    <- data.frame(y = y_bt, x = x_bt)
    mod_orig <- lm(y ~ x, data = df_bt)
    bt_orig_model(mod_orig)

    # --- Original model outputs ---
    output$btOrigScatter <- renderPlot({
      plot(x_bt, y_bt,
           pch = 21, cex = 1, col = "steelblue",
           xlab = input$btX, ylab = input$btY,
           main = paste("Original Data:", input$btY, "vs", input$btX))
      abline(mod_orig, col = "red", lwd = 2)
    })

    output$btOrigSummary <- renderPrint({
      cat("OLS Summary (Original Data)\n")
      cat(strrep("=", 45), "\n\n")
      print(summary(mod_orig))
    })

    output$btOrigResid <- renderPlot({
      plot(fitted(mod_orig), resid(mod_orig),
           xlab = "Fitted values", ylab = "Residuals",
           main = "Residuals vs Fitted (Original)",
           col = "steelblue", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      lines(lowess(fitted(mod_orig), resid(mod_orig)), col = "blue", lwd = 2, lty = 2)
    })

    output$btOrigBP <- renderPrint({
      cat("Breusch-Pagan Test (Original)\n")
      cat(strrep("=", 35), "\n\n")
      bp <- lmtest::bptest(mod_orig)
      print(bp)
    })

    # --- Transformation ---
    if (input$btMethod == "boxcox") {
      # Box-Cox: use formula + explicit data frame so MASS::boxcox never needs
      # to look up 'df_bt' by name in an outer environment (which fails in Shiny).
      bcox        <- MASS::boxcox(y_bt ~ x_bt, lambda = seq(-2, 2, 0.001), plotit = FALSE)
      lambda_opt  <- bcox$x[which.max(bcox$y)]
      bt_info(list(method = "boxcox", param = lambda_opt,
                   param_name = "lambda", x = bcox$x, y = bcox$y))

      output$btLambdaPlot <- renderPlot({
        plot(bcox$x, bcox$y, type = "l", lwd = 2, col = "steelblue",
             xlab = expression(lambda), ylab = "Log-Likelihood",
             main = "Box-Cox Log-Likelihood Profile")
        abline(v = lambda_opt, col = "red", lwd = 2, lty = 2)
        abline(h = max(bcox$y) - qchisq(0.95, 1) / 2,
               col = "grey50", lty = 3, lwd = 1.5)
        legend("bottomright",
               legend = c(paste("Optimal lambda =", round(lambda_opt, 3)),
                          "95% CI threshold"),
               col = c("red", "grey50"), lty = c(2, 3), lwd = 2, bty = "n")
      })

      output$btOptimal <- renderPrint({
        cat("Box-Cox Transformation\n")
        cat(strrep("=", 35), "\n\n")
        cat("Optimal lambda:", round(lambda_opt, 4), "\n\n")
        if (abs(lambda_opt) < 0.05) {
          cat("Suggested transformation: log(Y)\n")
        } else if (abs(lambda_opt - 0.5) < 0.1) {
          cat("Suggested transformation: sqrt(Y)\n")
        } else if (abs(lambda_opt - (-1)) < 0.1) {
          cat("Suggested transformation: 1/Y\n")
        } else {
          cat("Suggested transformation: Y^", round(lambda_opt, 3), "\n")
        }
        # 95% confidence interval for lambda
        ci_threshold <- max(bcox$y) - qchisq(0.95, 1) / 2
        ci_idx  <- bcox$y >= ci_threshold
        cat("\n95% CI for lambda: [",
            round(min(bcox$x[ci_idx]), 3), ",",
            round(max(bcox$x[ci_idx]), 3), "]\n")
      })

      # Transformed model (Y^lambda)
      y_trans  <- if (abs(lambda_opt) < 0.001) log(y_bt) else y_bt^lambda_opt
      df_trans <- data.frame(y_trans = y_trans, x = x_bt)
      mod_trans <- lm(y_trans ~ x, data = df_trans)
      bt_trans_model(mod_trans)

    } else {
      # Box-Tidwell: find optimal alpha
      bt_result   <- tryCatch(
        car::boxTidwell(y ~ x, data = df_bt),
        error = function(e) NULL
      )

      if (is.null(bt_result)) {
        showNotification("Box-Tidwell did not converge. Try different variables.",
                         type = "warning")
        return()
      }

      alpha_opt <- bt_result$result[1, "MLE of lambda"]
      bt_info(list(method = "boxtidwell", param = alpha_opt,
                   param_name = "alpha", bt_obj = bt_result))

      output$btLambdaPlot <- renderPlot({
        # Box-Tidwell doesn't produce a profile plot; show reciprocal transform
        # as illustration
        alpha_seq <- seq(-3, 3, length.out = 200)
        # Approximate residual log-lik proxy: SS from a grid of transformed models
        ss_vec <- sapply(alpha_seq, function(a) {
          xt <- tryCatch(x_bt^a, error = function(e) rep(NA, length(x_bt)))
          if (any(!is.finite(xt))) return(NA_real_)
          m  <- lm(y_bt ~ xt)
          sum(resid(m)^2)
        })
        ss_vec[!is.finite(ss_vec)] <- NA
        plot(alpha_seq, -ss_vec, type = "l", lwd = 2, col = "steelblue",
             xlab = expression(alpha),
             ylab = "-Residual SS (proxy for log-likelihood)",
             main = "Box-Tidwell Power Profile", na.action = na.omit)
        abline(v = alpha_opt, col = "red", lwd = 2, lty = 2)
        legend("bottomright",
               legend = paste("Optimal alpha =", round(alpha_opt, 3)),
               col = "red", lty = 2, lwd = 2, bty = "n")
      })

      output$btOptimal <- renderPrint({
        cat("Box-Tidwell Transformation\n")
        cat(strrep("=", 35), "\n\n")
        cat("Optimal alpha:", round(alpha_opt, 4), "\n\n")
        cat("Full Box-Tidwell output:\n")
        print(bt_result)
      })

      # Transformed model (X^alpha)
      x_trans  <- x_bt^alpha_opt
      df_trans <- data.frame(y = y_bt, x_trans = x_trans)
      mod_trans <- lm(y ~ x_trans, data = df_trans)
      bt_trans_model(mod_trans)
    }

    # --- Transformed model outputs ---
    mod_t <- bt_trans_model()

    output$btTransScatter <- renderPlot({
      mf_t <- model.frame(mod_t)
      x_t  <- mf_t[[2]]
      y_t  <- mf_t[[1]]
      info <- bt_info()
      x_lab <- if (info$method == "boxcox") input$btX else
                  paste0(input$btX, "^", round(info$param, 3))
      y_lab <- if (info$method == "boxcox") {
        if (abs(info$param) < 0.001) paste0("log(", input$btY, ")")
        else paste0(input$btY, "^", round(info$param, 3))
      } else input$btY
      plot(x_t, y_t,
           pch = 21, cex = 1, col = "darkorange",
           xlab = x_lab, ylab = y_lab,
           main = "Transformed Data with Fit")
      abline(mod_t, col = "red", lwd = 2)
    })

    output$btTransSummary <- renderPrint({
      cat("OLS Summary (Transformed Data)\n")
      cat(strrep("=", 45), "\n\n")
      print(summary(mod_t))
    })

    output$btTransResid <- renderPlot({
      plot(fitted(mod_t), resid(mod_t),
           xlab = "Fitted values", ylab = "Residuals",
           main = "Residuals vs Fitted (Transformed)",
           col = "darkorange", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      lines(lowess(fitted(mod_t), resid(mod_t)), col = "blue", lwd = 2, lty = 2)
    })

    output$btTransBP <- renderPrint({
      cat("Breusch-Pagan Test (Transformed)\n")
      cat(strrep("=", 35), "\n\n")
      bp <- tryCatch(lmtest::bptest(mod_t),
                     error = function(e) NULL)
      if (is.null(bp)) { cat("Could not compute BP test.\n"); return() }
      print(bp)
    })

    output$btComparison <- renderPlot({
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      # Original
      plot(fitted(mod_orig), resid(mod_orig),
           xlab = "Fitted", ylab = "Residuals",
           main = "Original Model", col = "steelblue", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      # Transformed
      plot(fitted(mod_t), resid(mod_t),
           xlab = "Fitted", ylab = "Residuals",
           main = "Transformed Model", col = "darkorange", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
    })
  })

  # ===== WEIGHTED LEAST SQUARES SERVER =====

  output$wlsYSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("wlsY", "Response Variable (y):", choices = data$numeric_vars)
  })

  output$wlsXSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("wlsX", "Predictor Variable (x):", choices = data$numeric_vars)
  })

  output$wlsWSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("wlsW", "Weight Variable:",
                choices = c("Auto-estimate (1/|residual|)" = "__auto__",
                            data$numeric_vars))
  })

  wls_ols_model <- reactiveVal(NULL)
  wls_wls_model <- reactiveVal(NULL)

  observeEvent(input$runWLS, {
    req(data$raw, input$wlsY, input$wlsX)

    y_wls <- data$raw[[input$wlsY]]
    x_wls <- data$raw[[input$wlsX]]
    df_wls <- data.frame(y = y_wls, x = x_wls)

    # OLS fit
    mod_ols <- lm(y ~ x, data = df_wls)
    wls_ols_model(mod_ols)
    bp_ols  <- lmtest::bptest(mod_ols)

    output$wlsOLSScatter <- renderPlot({
      plot(x_wls, y_wls,
           pch = 21, cex = 1, col = "steelblue",
           xlab = input$wlsX, ylab = input$wlsY,
           main = paste("OLS:", input$wlsY, "vs", input$wlsX))
      abline(mod_ols, col = "red", lwd = 2)
    })

    output$wlsOLSSummary <- renderPrint({
      cat("OLS Model Summary\n")
      cat(strrep("=", 40), "\n\n")
      print(summary(mod_ols))
      cat("\nBreusch-Pagan Test\n")
      cat(strrep("-", 40), "\n")
      print(bp_ols)
      if (bp_ols$p.value < 0.05) {
        cat("\n=> Heteroscedasticity detected. WLS is recommended.\n")
      } else {
        cat("\n=> No strong evidence of heteroscedasticity.\n")
      }
    })

    output$wlsOLSResid <- renderPlot({
      plot(fitted(mod_ols), resid(mod_ols),
           xlab = expression(hat(y)[i]), ylab = expression(e[i]),
           main = "OLS: Residuals vs Fitted",
           col = "steelblue", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      lines(lowess(fitted(mod_ols), resid(mod_ols)), col = "blue", lwd = 2, lty = 2)
    })

    # Determine weights
    if (!is.null(input$wlsW) && input$wlsW != "__auto__") {
      # User-provided weight column — treat as 1/variance style
      w_raw <- data$raw[[input$wlsW]]
      if (any(w_raw <= 0, na.rm = TRUE)) {
        showNotification(
          "Weight column contains non-positive values. Using auto-estimation instead.",
          type = "warning")
        w_vals <- 1 / (abs(resid(mod_ols)) + 1e-8)
      } else {
        w_vals <- 1 / w_raw
      }
    } else {
      # Auto: weights = 1 / |OLS residuals|
      w_vals <- 1 / (abs(resid(mod_ols)) + 1e-8)
    }

    w_vals <- w_vals / mean(w_vals)   # normalise
    df_wls$w <- w_vals

    # WLS fit
    mod_wls <- lm(y ~ x, data = df_wls, weights = w)
    wls_wls_model(mod_wls)
    bp_wls  <- lmtest::bptest(mod_wls)

    output$wlsWLSScatter <- renderPlot({
      plot(x_wls, y_wls,
           pch = 21, cex = sqrt(w_vals / max(w_vals)) * 2.5 + 0.3,
           col = "darkorange",
           xlab = input$wlsX, ylab = input$wlsY,
           main = paste("WLS:", input$wlsY, "vs", input$wlsX,
                        "(point size ~ weight)"))
      abline(mod_wls, col = "red", lwd = 2)
    })

    output$wlsWLSSummary <- renderPrint({
      cat("WLS Model Summary\n")
      cat(strrep("=", 40), "\n\n")
      print(summary(mod_wls))
      cat("\nBreusch-Pagan Test (on WLS model)\n")
      cat(strrep("-", 40), "\n")
      print(bp_wls)
      if (bp_wls$p.value < 0.05) {
        cat("\n=> Heteroscedasticity persists. Consider a different weight specification.\n")
      } else {
        cat("\n=> WLS has addressed the heteroscedasticity.\n")
      }
    })

    output$wlsWLSResid <- renderPlot({
      w_sqrt <- sqrt(w_vals)
      plot(w_sqrt * fitted(mod_wls),
           w_sqrt * resid(mod_wls),
           xlab = expression(sqrt(w[i]) * hat(y)[i]),
           ylab = expression(sqrt(w[i]) * e[i]),
           main = "WLS: Weighted Residuals vs Weighted Fitted",
           col = "darkorange", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      lines(lowess(w_sqrt * fitted(mod_wls), w_sqrt * resid(mod_wls)),
            col = "blue", lwd = 2, lty = 2)
    })

    output$wlsComparison <- renderPlot({
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      plot(fitted(mod_ols), resid(mod_ols),
           xlab = expression(hat(y)[i]), ylab = expression(e[i]),
           main = "OLS Residuals", col = "steelblue", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
      w_sqrt <- sqrt(w_vals)
      plot(w_sqrt * fitted(mod_wls), w_sqrt * resid(mod_wls),
           xlab = expression(sqrt(w[i]) * hat(y)[i]),
           ylab = expression(sqrt(w[i]) * e[i]),
           main = "WLS Weighted Residuals", col = "darkorange", pch = 19)
      abline(h = 0, col = "red", lwd = 2)
    })

    output$wlsCoefComp <- renderPrint({
      cat("Coefficient Comparison\n")
      cat(strrep("=", 38), "\n\n")
      ols_coef <- coef(mod_ols)
      wls_coef <- coef(mod_wls)
      comp <- data.frame(
        OLS  = round(ols_coef, 4),
        WLS  = round(wls_coef, 4),
        Diff = round(wls_coef - ols_coef, 4)
      )
      print(comp)
      cat("\nOLS R-squared:",  round(summary(mod_ols)$r.squared, 4), "\n")
      cat("WLS R-squared:",  round(summary(mod_wls)$r.squared, 4), "\n")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

