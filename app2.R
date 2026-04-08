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
      menuItem("Single Variable", tabName = "single", icon = icon("chart-bar")),
      menuItem("Two Variables", tabName = "two", icon = icon("chart-line")),
      menuItem("Correlation Matrix", tabName = "correlation", icon = icon("th")),
      menuItem("Categorical Analysis", tabName = "categorical", icon = icon("chart-pie")),
      menuItem("Cross-Tabulation (2 Vars)", tabName = "crosstab", icon = icon("table")),
      menuItem("Multi-Categorical (3+ Vars)", tabName = "multicategorical", icon = icon("cubes")),
      menuItem("Hypothesis Tests", tabName = "tests", icon = icon("calculator")),
      menuItem("Simple Linear Regression", tabName = "slr", icon = icon("chart-line")),
      menuItem("Multiple Linear Regression", tabName = "mlr", icon = icon("project-diagram")),
      menuItem("Summary Report", tabName = "report", icon = icon("file-alt"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #3c8dbc; }
        .small-box { border-radius: 5px; }
        .info-box { border-radius: 5px; }
        h2 { color: #3c8dbc; font-weight: bold; }
        .btn-analyze {
          background-color: #00a65a;
          color: white;
          font-weight: bold;
          padding: 10px 30px;
          font-size: 16px;
        }
        .selectize-input { font-size: 14px; }

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
            
            DTOutput("dataPreview"),
            
            hr(),
            
            h4("Data Summary"),
            verbatimTextOutput("dataSummary")
          )
        )
      ),
      
      # Single Variable Analysis
      tabItem(
        tabName = "single",
        fluidRow(
          box(
            title = "Single Variable Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
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
          )
        ),
        
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
            plotOutput("singlePlot", height = "400px")
          )
        )
      ),
      
      # Two Variables Analysis
      tabItem(
        tabName = "two",
        fluidRow(
          box(
            title = "Two Variables Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(3,
                selectInput("var1Type", "Variable 1 Type:",
                           choices = c("Numerical", "Categorical"))
              ),
              column(3,
                uiOutput("var1Select")
              ),
              column(3,
                selectInput("var2Type", "Variable 2 Type:",
                           choices = c("Numerical", "Categorical"))
              ),
              column(3,
                uiOutput("var2Select")
              )
            ),
            
            fluidRow(
              column(12,
                actionButton("runTwoAnalysis", "Run Analysis", 
                           class = "btn btn-primary")
              )
            )
          )
        ),
        
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
            plotOutput("twoVarPlot", height = "400px")
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
            plotOutput("corrHeatmap", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Scatter Plot Matrix",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("corrPairs", height = "500px")
          )
        )
      ),
      
      # Categorical Analysis
      tabItem(
        tabName = "categorical",
        fluidRow(
          box(
            title = "Categorical Variable Analysis",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                uiOutput("catVarSelect")
              ),
              column(4,
                selectInput("catPlotType", "Plot Type:",
                           choices = c("Bar Plot", "Pie Chart", "Both"))
              ),
              column(4,
                br(),
                actionButton("runCatAnalysis", "Run Analysis", 
                           class = "btn btn-primary")
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
            plotOutput("catPlot", height = "450px")
          )
        )
      ),
      
      # Cross-Tabulation
      tabItem(
        tabName = "crosstab",
        fluidRow(
          box(
            title = "Cross-Tabulation Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                uiOutput("crosstabVar1Select")
              ),
              column(4,
                uiOutput("crosstabVar2Select")
              ),
              column(4,
                br(),
                actionButton("runCrosstabAnalysis", "Run Analysis", 
                           class = "btn btn-primary")
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
            plotOutput("crosstabPlot", height = "350px")
          )
        )
      ),
      
      # Multi-Categorical Analysis (3+ Variables)
      tabItem(
        tabName = "multicategorical",
        
        fluidRow(
          box(
            title = tagList(icon("cubes"), " Multi-Categorical Variable Analysis"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            h4("Analyze relationships among 3 or more categorical variables"),
            
            fluidRow(
              column(3,
                uiOutput("multiCatVar1Select")
              ),
              column(3,
                uiOutput("multiCatVar2Select")
              ),
              column(3,
                uiOutput("multiCatVar3Select")
              ),
              column(3,
                uiOutput("multiCatVar4Select")
              )
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
        
        # Multi-way Contingency Table
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
        
        # Statistical Tests
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
        
        # Visualizations
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
              column(8,
                uiOutput("multiCatPlotOptions")
              )
            ),
            
            plotOutput("multiCatPlot", height = "500px")
          )
        ),
        
        # Insights
        fluidRow(
          box(
            title = tagList(icon("lightbulb"), " Analysis Insights"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            htmlOutput("multiCatInsights")
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
                verbatimTextOutput("slrPredInterval")
              ),

              tabPanel(
                tagList(icon("compress-arrows-alt"), " Confidence Interval"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Narrower interval — estimates the mean response at the given X value."
                ),
                verbatimTextOutput("slrConfInterval")
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
                verbatimTextOutput("mlrPredInterval")
              ),

              tabPanel(
                tagList(icon("compress-arrows-alt"), " Confidence Interval"),
                br(),
                tags$div(class = "reg-help-alert",
                  icon("info-circle"),
                  " Narrower interval — estimates the mean response at the given predictor values."
                ),
                verbatimTextOutput("mlrConfInterval")
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
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Increase maximum file upload size to 50MB (default is 5MB)
  options(shiny.maxRequestSize = 50 * 1024^2)
  
  # Reactive data storage
  data <- reactiveValues(
    raw = NULL,
    analyzed = FALSE,
    numeric_vars = NULL,
    categorical_vars = NULL
  )
  
  # File upload handler
  observeEvent(input$datafile, {
    req(input$datafile)
    
    tryCatch({
      ext <- tools::file_ext(input$datafile$name)
      
      if(ext == "csv") {
        data$raw <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
      } else if(ext %in% c("xlsx", "xls")) {
        data$raw <- read_excel(input$datafile$datapath)
      }
      
      # Automatically detect variable types
      numeric_cols <- sapply(data$raw, is.numeric)
      data$numeric_vars <- names(data$raw)[numeric_cols]
      data$categorical_vars <- names(data$raw)[!numeric_cols]
      
      data$analyzed <- FALSE
      
      showNotification("Data uploaded successfully! Variable types detected.", 
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
  
  # Data preview
  output$dataPreview <- renderDT({
    req(data$raw)
    datatable(data$raw,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                scrollY = "450px"
              ))
  })
  
  # Data summary
  output$dataSummary <- renderPrint({
    req(data$raw)
    summary(data$raw)
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
    }
  })
  
  # ===== TWO VARIABLES ANALYSIS =====
  
  output$var1Select <- renderUI({
    req(data$raw)
    if(input$var1Type == "Numerical") {
      selectInput("var1", "Select Variable 1:", choices = data$numeric_vars)
    } else {
      selectInput("var1", "Select Variable 1:", choices = data$categorical_vars)
    }
  })
  
  output$var2Select <- renderUI({
    req(data$raw)
    if(input$var2Type == "Numerical") {
      selectInput("var2", "Select Variable 2:", choices = data$numeric_vars)
    } else {
      selectInput("var2", "Select Variable 2:", choices = data$categorical_vars)
    }
  })
  
  observeEvent(input$runTwoAnalysis, {
    req(data$raw, input$var1, input$var2)
    
    var1_data <- data$raw[[input$var1]]
    var2_data <- data$raw[[input$var2]]
    
    if(input$var1Type == "Numerical" && input$var2Type == "Numerical") {
      # Both numerical - correlation and scatter
      output$twoVarStats <- renderPrint({
        cor_p <- cor(var1_data, var2_data, method = "pearson", use = "complete.obs")
        cor_s <- cor(var2_data, var2_data, method = "spearman", use = "complete.obs")
        
        cat("Correlation Analysis\n")
        cat(strrep("=", 40), "\n\n")
        cat("Pearson Correlation  :", round(cor_p, 4), "\n")
        cat("Spearman Correlation :", round(cor_s, 4), "\n\n")
        
        lm_model <- lm(var2_data ~ var1_data)
        cat("Linear Regression:\n")
        print(summary(lm_model))
      })
      
      output$twoVarPlot <- renderPlot({
        plot(var1_data, var2_data,
             main = paste(input$var1, "vs", input$var2),
             xlab = input$var1,
             ylab = input$var2,
             pch = 19,
             col = rgb(0.2, 0.5, 0.8, 0.5))
        abline(lm(var2_data ~ var1_data), col = "red", lwd = 2)
      })
      
    } else if(input$var1Type == "Categorical" && input$var2Type == "Categorical") {
      # Both categorical - contingency table
      cont_table <- table(var1_data, var2_data)
      
      output$twoVarStats <- renderPrint({
        cat("Contingency Table Analysis\n")
        cat(strrep("=", 40), "\n\n")
        cat("Contingency Table:\n")
        print(cont_table)
        cat("\n\nJoint Proportions:\n")
        print(round(prop.table(cont_table), 4))
        
        chi_test <- chisq.test(cont_table)
        cat("\n\nChi-Square Test:\n")
        print(chi_test)
      })
      
      output$twoVarPlot <- renderPlot({
        barplot(cont_table,
                main = paste(input$var1, "vs", input$var2),
                xlab = input$var2,
                ylab = "Frequency",
                col = brewer.pal(min(nrow(cont_table), 8), "Set3"),
                beside = TRUE,
                legend.text = TRUE)
      })
      
    } else {
      # One numerical, one categorical - group comparison
      if(input$var1Type == "Numerical") {
        num_var <- var1_data
        cat_var <- var2_data
        num_name <- input$var1
        cat_name <- input$var2
      } else {
        num_var <- var2_data
        cat_var <- var1_data
        num_name <- input$var2
        cat_name <- input$var1
      }
      
      output$twoVarStats <- renderPrint({
        cat("Group Comparison Analysis\n")
        cat(strrep("=", 40), "\n\n")
        
        agg_stats <- aggregate(num_var ~ cat_var, FUN = function(x) {
          c(Mean = mean(x, na.rm = TRUE),
            SD = sd(x, na.rm = TRUE),
            N = length(x))
        })
        
        cat("Descriptive Statistics by Group:\n")
        print(agg_stats)
        
        cat("\n\nANOVA Test:\n")
        anova_result <- aov(num_var ~ cat_var)
        print(summary(anova_result))
      })
      
      output$twoVarPlot <- renderPlot({
        boxplot(num_var ~ cat_var,
                main = paste(num_name, "by", cat_name),
                xlab = cat_name,
                ylab = num_name,
                col = brewer.pal(min(length(unique(cat_var)), 8), "Set1"),
                las = 2)
      })
    }
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
    
    selected_data <- data$raw[, input$corrVars, drop = FALSE]
    corr_matrix <- cor(selected_data, use = "complete.obs")
    
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

      output$slrPredInterval <- renderPrint({
        print(predict(slr_model(), newdata = new_data,
                      interval = "prediction", level = 0.95))
      })
      output$slrConfInterval <- renderPrint({
        print(predict(slr_model(), newdata = new_data,
                      interval = "confidence", level = 0.95))
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

      output$mlrPredInterval <- renderPrint({
        print(predict(mlr_model(), newdata = new_data,
                      interval = "prediction", level = 0.95))
      })
      output$mlrConfInterval <- renderPrint({
        print(predict(mlr_model(), newdata = new_data,
                      interval = "confidence", level = 0.95))
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
}

# Run the application
shinyApp(ui = ui, server = server)

