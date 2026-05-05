##########################################################
### ModelCraft: Statistical Learning in R
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
library(MASS) # boxcox
library(lmtest) # bptest, dwtest
library(boot) # cv.glm
library(bslib)

spn <- function(x) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(x, type = 4, color = "#2563eb")
  } else {
    x
  }
}

HAS_PLOTLY <- requireNamespace("plotly", quietly = TRUE)
has_glmnet <- function() requireNamespace("glmnet", quietly = TRUE)

detox_asset_dir <- file.path(
  ".",
  "detox-data-science-analytics-html-template-2023-11-27-05-25-38-utc",
  "Detox Pack",
  "Detox",
  "assets"
)

if (dir.exists(detox_asset_dir) && !("detox-assets" %in% names(shiny::resourcePaths()))) {
  addResourcePath("detox-assets", normalizePath(detox_asset_dir, winslash = "/", mustWork = TRUE))
}

app_sections <- list(
  list(tab = "upload", label = "Upload Data", icon = "upload"),
  list(tab = "preview", label = "Data Preview", icon = "table"),
  list(tab = "varanalysis", label = "Variable Analysis", icon = "sliders-h"),
  list(tab = "correlation", label = "Correlation Matrix", icon = "th"),
  list(tab = "catsuite", label = "Categorical Suite", icon = "layer-group"),
  list(tab = "tests", label = "Hypothesis Tests", icon = "calculator"),
  list(tab = "slr", label = "Simple Linear Regression", icon = "chart-line"),
  list(tab = "mlr", label = "Multiple Linear Regression", icon = "project-diagram"),
  list(tab = "polyreg", label = "Polynomial Regression", icon = "chart-area"),
  list(tab = "influence", label = "Leverage & Influence", icon = "crosshairs"),
  list(tab = "ch9multi", label = "Multicollinearity", icon = "bezier-curve"),
  list(tab = "modelbuilding", label = "Model Building", icon = "sitemap"),
  list(tab = "glm", label = "Generalized Linear Models", icon = "wave-square"),
  list(tab = "indicator", label = "Indicator Variable", icon = "tags"),
  list(tab = "modeladequacy", label = "Model Adequacy", icon = "stethoscope"),
  list(tab = "boxtrans", label = "Box-Cox & Box-Tidwell", icon = "magic"),
  list(tab = "wls", label = "Weighted Least Squares", icon = "balance-scale"),
  list(tab = "report", label = "Summary Report", icon = "file-alt")
)

grouped_sections <- list(
  data_analysis = c("upload", "preview", "varanalysis", "correlation", "catsuite", "tests"),
  linear_reg = c("slr", "mlr", "polyreg"),
  correcting_models = c("boxtrans", "wls")
)

main_nav_sections <- list(
  list(tab = "upload", tabs = grouped_sections$data_analysis, label = "Data & Analysis", icon = "table"),
  list(tab = "slr", tabs = grouped_sections$linear_reg, label = "Regression", icon = "chart-line"),
  list(tab = "influence", tabs = "influence", label = "Leverage & Influence", icon = "crosshairs"),
  list(tab = "boxtrans", tabs = grouped_sections$correcting_models, label = "Correcting Models", icon = "magic"),
  list(tab = "ch9multi", tabs = "ch9multi", label = "Multicollinearity", icon = "bezier-curve"),
  list(tab = "modelbuilding", tabs = "modelbuilding", label = "Model Building", icon = "sitemap"),
  list(tab = "glm", tabs = "glm", label = "Generalized Linear Models", icon = "wave-square"),
  list(tab = "indicator", tabs = "indicator", label = "Indicator Variable", icon = "tags"),
  list(tab = "modeladequacy", tabs = "modeladequacy", label = "Model Adequacy", icon = "stethoscope"),
  list(tab = "report", tabs = "report", label = "Summary Report", icon = "file-alt")
)

build_proxy_sidebar <- function(items) {
  sidebarMenu(
    id = "tabs",
    .list = lapply(items, function(item) {
      menuItem(item$label, tabName = item$tab, icon = icon(item$icon))
    })
  )
}

build_detox_nav <- function(items) {
  tags$ul(
    class = "detox-nav-list",
    lapply(seq_along(items), function(i) {
      item <- items[[i]]
      tags$li(
        tags$a(
          href = paste0("#", item$tab),
          class = paste("detox-nav-link js-detox-nav", if (i == 1) "is-active"),
          `data-tab` = item$tab,
          `data-tabs` = paste(item$tabs %||% item$tab, collapse = "|"),
          `data-label` = item$label,
          icon(item$icon),
          tags$span(item$label)
        )
      )
    })
  )
}

build_subpage_nav <- function(title, items) {
  tags$div(
    class = "detox-subnav-wrap",
    tags$div(class = "detox-subnav-title", title),
    tags$div(
      class = "detox-subnav-list",
      lapply(seq_along(items), function(i) {
        item <- items[[i]]
        tags$a(
          href = paste0("#", item$tab),
          class = paste("detox-subnav-link js-detox-subnav", if (i == 1) "is-active"),
          `data-tab` = item$tab,
          `data-label` = item$label,
          icon(item$icon),
          tags$span(item$label)
        )
      })
    )
  )
}

sample_button_row <- function(label, buttons, hint = NULL) {
  box(
    class = "reg-card",
    title = tagList(icon("database"), paste("Sample Data -", label)),
    solidHeader = TRUE,
    status = "info",
    width = 12,
    tags$div(
      style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
      buttons
    ),
    if (!is.null(hint)) tags$div(class = "hint-text", style = "margin-top:10px;", hint)
  )
}

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
    title = "ModelCraft: Statistical Learning in R",
    titleWidth = 350
  ),

  # Sidebar
  dashboardSidebar(
    width = 250,
    div(class = "detox-tab-proxy", build_proxy_sidebar(app_sections))
  ),

  # Body
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700;800&family=Roboto:wght@400;500;700&display=swap"
      ),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('setDarkMode', function(message) {
          if (message && message.enabled) {
            document.body.classList.add('dark-mode-pro');
          } else {
            document.body.classList.remove('dark-mode-pro');
          }
        });

        function detoxSyncNav(tabName) {
          var links = document.querySelectorAll('.js-detox-nav');
          links.forEach(function(link) {
            var tabs = (link.getAttribute('data-tabs') || link.getAttribute('data-tab') || '').split('|');
            var isActive = tabs.indexOf(tabName) !== -1;
            link.classList.toggle('is-active', isActive);
          });

          var subLinks = document.querySelectorAll('.js-detox-subnav');
          subLinks.forEach(function(link) {
            var isActive = link.getAttribute('data-tab') === tabName;
            link.classList.toggle('is-active', isActive);
          });

          var currentTab = document.getElementById('detox-current-tab');
          if (currentTab) {
            var activeSub = document.querySelector('.js-detox-subnav[data-tab=\"' + tabName + '\"]');
            var activeMain = document.querySelector('.js-detox-nav.is-active');
            var labelSource = activeSub || activeMain;
            currentTab.textContent = labelSource ? (labelSource.getAttribute('data-label') || tabName) : tabName;
          }
        }

        document.addEventListener('click', function(event) {
          var navLink = event.target.closest('.js-detox-nav, .js-detox-subnav');
          if (!navLink) return;

          event.preventDefault();
          var tabName = navLink.getAttribute('data-tab');
          var proxyLink = document.querySelector('.detox-tab-proxy a[data-value=\"' + tabName + '\"]');

          if (proxyLink && window.jQuery) {
            window.jQuery(proxyLink).tab('show');
            proxyLink.click();
          } else if (window.Shiny) {
            window.Shiny.setInputValue('tabs', tabName, {priority: 'event'});
          }

          document.body.classList.remove('detox-mobile-open');
          detoxSyncNav(tabName);
        });

        document.addEventListener('shown.bs.tab', function(event) {
          var target = event.target;
          if (target && target.matches('.detox-tab-proxy a[data-value]')) {
            detoxSyncNav(target.getAttribute('data-value'));
          }
        });

        document.addEventListener('DOMContentLoaded', function() {
          var darkToggle = document.getElementById('darkModeToggle');
          if (darkToggle && darkToggle.checked) {
            document.body.classList.add('dark-mode-pro');
          }

          var activeProxy = document.querySelector('.detox-tab-proxy li.active a[data-value]');
          detoxSyncNav(activeProxy ? activeProxy.getAttribute('data-value') : 'upload');

          var toggle = document.querySelector('.js-detox-menu-toggle');
          if (toggle) {
            toggle.addEventListener('click', function() {
              document.body.classList.toggle('detox-mobile-open');
            });
          }

          var backdrop = document.querySelector('.js-detox-backdrop');
          if (backdrop) {
            backdrop.addEventListener('click', function() {
              document.body.classList.remove('detox-mobile-open');
            });
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
        body.dark-mode-pro .small-box h3,
        body.dark-mode-pro .small-box p,
        body.dark-mode-pro .small-box .icon,
        body.dark-mode-pro .small-box .small-box-footer {
          color: #e5e7eb !important;
        }
        body.dark-mode-pro .small-box .icon {
          color: rgba(148, 163, 184, 0.22) !important;
          opacity: 1 !important;
          text-shadow: 0 0 18px rgba(15, 23, 42, 0.55);
        }
        body.dark-mode-pro .small-box .small-box-footer {
          background: rgba(255, 255, 255, 0.04) !important;
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
        .mb-rule-row {
          display:flex;
          gap:8px;
          flex-wrap:wrap;
          margin-bottom:12px;
        }
        .mb-pill {
          display:inline-flex;
          align-items:center;
          gap:6px;
          border-radius:999px;
          padding:6px 10px;
          font-size:12px;
          font-weight:700;
          background:#fff7ed;
          color:#9a3412;
          border:1px solid #fed7aa;
        }
        .mb-step-list {
          display:flex;
          flex-direction:column;
          gap:8px;
        }
        .mb-step-row {
          display:grid;
          grid-template-columns:34px 88px 1fr auto;
          align-items:center;
          gap:8px;
          padding:10px 12px;
          border:1px solid #e5e7eb;
          border-radius:8px;
          background:#fff;
        }
        .mb-step-num {
          width:26px;
          height:26px;
          border-radius:50%;
          display:flex;
          align-items:center;
          justify-content:center;
          background:#eff6ff;
          color:#1d4ed8;
          font-weight:800;
          font-size:12px;
        }
        .mb-action {
          border-radius:999px;
          padding:4px 8px;
          font-size:11px;
          font-weight:800;
          text-align:center;
        }
        .mb-action-add { background:#dcfce7; color:#166534; }
        .mb-action-remove { background:#fee2e2; color:#991b1b; }
        .mb-action-stop { background:#e5e7eb; color:#374151; }
        .mb-step-predictor {
          font-weight:700;
          color:#111827;
          min-width:0;
          word-break:break-word;
        }
        .mb-step-p {
          font-variant-numeric:tabular-nums;
          font-weight:700;
          color:#4b5563;
        }
        .mb-final-chip {
          margin-top:12px;
          padding:10px 12px;
          border-radius:8px;
          background:#f0fdf4;
          border:1px solid #bbf7d0;
          color:#14532d;
          font-weight:800;
          word-break:break-word;
        }
        .mb-metric-grid {
          display:grid;
          grid-template-columns:repeat(4, minmax(120px, 1fr));
          gap:10px;
          margin-bottom:12px;
        }
        .mb-metric {
          border:1px solid #e5e7eb;
          border-radius:8px;
          padding:12px;
          background:#ffffff;
        }
        .mb-metric span {
          display:block;
          color:#6b7280;
          font-size:11px;
          font-weight:800;
          text-transform:uppercase;
        }
        .mb-metric strong {
          display:block;
          margin-top:4px;
          color:#111827;
          font-size:20px;
        }
        .mb-equation {
          padding:11px 12px;
          border-radius:8px;
          background:#eff6ff;
          border:1px solid #bfdbfe;
          color:#1e3a8a;
          font-weight:800;
          word-break:break-word;
        }
        body.dark-mode-pro .mb-step-row,
        body.dark-mode-pro .mb-metric {
          background:#111827;
          border-color:#334155;
        }
        body.dark-mode-pro .mb-step-predictor,
        body.dark-mode-pro .mb-metric strong {
          color:#e5e7eb;
        }

        .wrapper,
        .content-wrapper,
        .right-side {
          min-height: 100vh;
        }
        .main-header,
        .main-sidebar,
        .left-side {
          display: none !important;
        }
        .content-wrapper,
        .right-side,
        .main-footer {
          margin-left: 0 !important;
        }
        .wrapper {
          background: linear-gradient(180deg, #dfe9f8 0%, #eef4fb 18%, #eef3f9 18.1%, #eef3f9 100%);
        }
        .content {
          padding: 0 0 24px 0;
        }
        .detox-tab-proxy {
          display: none;
        }
        .detox-app-shell {
          min-height: 100vh;
          position: relative;
        }
        .detox-mobile-backdrop {
          display: none;
          position: fixed;
          inset: 0;
          background: rgba(3, 11, 24, 0.55);
          z-index: 998;
        }
        .detox-header {
          position: relative;
          z-index: 999;
          padding: 24px 28px 18px;
          color: #0f172a;
        }
        .detox-header-inner {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 18px;
          flex-wrap: wrap;
        }
        .detox-brand-wrap {
          display: flex;
          align-items: center;
          gap: 12px;
        }
        .detox-brand-title {
          font-family: 'Poppins', 'Plus Jakarta Sans', sans-serif;
          font-size: 28px;
          font-weight: 700;
          line-height: 1.1;
          margin: 0;
          color: #0f172a;
          text-shadow: none;
        }
        .detox-brand-copy p {
          margin: 6px 0 0;
          color: #475569;
          font-size: 14px;
          max-width: 680px;
          text-shadow: none;
        }
        .detox-header-badges {
          display: flex;
          align-items: center;
          gap: 12px;
          flex-wrap: wrap;
        }
        .detox-chip {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          padding: 10px 14px;
          border-radius: 999px;
          background: rgba(255, 255, 255, 0.92);
          border: 1px solid #dbe4f0;
          color: #334155;
          font-weight: 600;
          box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06);
        }
        .detox-chip strong {
          color: #ffb07c;
          font-weight: 700;
        }
        .detox-chip.current-module {
          background: rgba(255, 255, 255, 0.96);
          border-color: #d7e0ec;
        }
        .upload-note {
          color: #1f2937;
        }
        .upload-note strong {
          color: inherit;
        }
        .detox-menu-toggle {
          display: none;
          width: 48px;
          height: 48px;
          border: 0;
          border-radius: 14px;
          background: rgba(255, 255, 255, 0.12);
          color: #fff;
          font-size: 20px;
        }
        .detox-layout {
          display: grid;
          grid-template-columns: 320px minmax(0, 1fr);
          gap: 24px;
          padding: 0 28px 28px;
          align-items: start;
        }
        .detox-sidebar-panel {
          position: sticky;
          top: 24px;
          background: linear-gradient(180deg, #0d1b2f 0%, #102540 100%);
          border-radius: 26px;
          padding: 22px 18px;
          box-shadow: 0 20px 45px rgba(4, 13, 27, 0.2);
          color: #fff;
        }
        .detox-sidebar-panel h4 {
          color: #fff;
          font-family: 'Poppins', 'Plus Jakarta Sans', sans-serif;
          font-size: 18px;
          font-weight: 700;
          margin: 0 0 8px;
        }
        .detox-sidebar-panel p {
          color: rgba(255, 255, 255, 0.7);
          font-size: 13px;
          margin: 0 0 18px;
        }
        .detox-nav-list {
          list-style: none;
          margin: 0;
          padding: 0;
          display: flex;
          flex-direction: column;
          gap: 10px;
        }
        .detox-nav-link {
          display: flex;
          align-items: center;
          gap: 12px;
          width: 100%;
          padding: 13px 16px;
          border-radius: 16px;
          color: rgba(255, 255, 255, 0.78);
          background: rgba(255, 255, 255, 0.04);
          border: 1px solid rgba(255, 255, 255, 0.05);
          transition: all .2s ease;
          font-weight: 600;
        }
        .detox-nav-link:hover,
        .detox-nav-link:focus {
          color: #fff;
          text-decoration: none;
          transform: translateX(3px);
          background: rgba(255, 255, 255, 0.08);
        }
        .detox-nav-link.is-active {
          background: linear-gradient(135deg, #ff7f50 0%, #ff5e57 100%);
          color: #fff;
          border-color: transparent;
          box-shadow: 0 16px 24px rgba(255, 94, 87, 0.28);
        }
        .detox-main {
          min-width: 0;
        }
        .detox-content-card {
          background: rgba(255, 255, 255, 0.68);
          backdrop-filter: blur(10px);
          border: 1px solid rgba(255, 255, 255, 0.7);
          border-radius: 28px;
          padding: 22px;
          box-shadow: 0 20px 40px rgba(30, 41, 59, 0.08);
        }
        .detox-content-intro {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 16px;
          margin-bottom: 18px;
          flex-wrap: wrap;
        }
        .detox-content-intro h2 {
          margin: 0;
          color: #0f172a;
          font-family: 'Poppins', 'Plus Jakarta Sans', sans-serif;
          font-size: 30px;
          font-weight: 700;
        }
        .detox-content-intro p {
          margin: 8px 0 0;
          color: #64748b;
          max-width: 680px;
        }
        .detox-note {
          padding: 10px 14px;
          border-radius: 16px;
          background: #fff7ed;
          color: #9a3412;
          font-weight: 700;
          border: 1px solid #fed7aa;
        }
        .detox-subnav-wrap {
          margin-bottom: 18px;
          padding: 16px 18px;
          border-radius: 22px;
          background: linear-gradient(180deg, #f8fbff 0%, #eef5ff 100%);
          border: 1px solid #dbe8f6;
        }
        .detox-subnav-title {
          font-size: 13px;
          font-weight: 800;
          text-transform: uppercase;
          letter-spacing: .08em;
          color: #47607c;
          margin-bottom: 12px;
        }
        .detox-subnav-list {
          display: flex;
          gap: 10px;
          flex-wrap: wrap;
        }
        .detox-subnav-link {
          display: inline-flex;
          align-items: center;
          gap: 9px;
          padding: 11px 16px;
          border-radius: 999px;
          background: #ffffff;
          border: 1px solid #d6e2f0;
          color: #334155;
          font-weight: 700;
          box-shadow: 0 8px 20px rgba(148, 163, 184, 0.12);
          transition: transform .18s ease, box-shadow .18s ease, background .18s ease;
        }
        .detox-subnav-link:hover,
        .detox-subnav-link:focus {
          text-decoration: none;
          color: #0f172a;
          transform: translateY(-1px);
          box-shadow: 0 10px 22px rgba(148, 163, 184, 0.18);
        }
        .detox-subnav-link.is-active {
          background: linear-gradient(135deg, #2563eb 0%, #1d4ed8 100%);
          border-color: transparent;
          color: #ffffff;
          box-shadow: 0 12px 24px rgba(37, 99, 235, 0.28);
        }
        .tab-content > .tab-pane {
          margin-top: 0;
        }
        .tab-content > .tab-pane:not(.active) {
          display: none;
        }
        .quick-actions .box-body {
          padding-bottom: 6px;
        }
        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          color: #111827 !important;
          background: #ffffff !important;
        }
        .dataTables_wrapper table.dataTable thead th,
        .dataTables_wrapper table.dataTable tbody td,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate .paginate_button,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter label,
        .dataTables_wrapper .dataTables_length label {
          color: #1f2937 !important;
        }
        body.dark-mode-pro .wrapper {
          background: linear-gradient(180deg, #030712 0%, #07111f 18%, #020617 18.1%, #020617 100%);
        }
        body.dark-mode-pro .detox-content-card {
          background: rgba(15, 23, 42, 0.82);
          border-color: rgba(51, 65, 85, 0.9);
        }
        body.dark-mode-pro .detox-subnav-wrap {
          background: linear-gradient(180deg, #0f1b31 0%, #0b1629 100%);
          border-color: #23344b;
        }
        body.dark-mode-pro .detox-subnav-title {
          color: #93c5fd;
        }
        body.dark-mode-pro .detox-subnav-link {
          background: #111c30;
          border-color: #2c415f;
          color: #dbeafe;
          box-shadow: none;
        }
        body.dark-mode-pro .detox-subnav-link:hover,
        body.dark-mode-pro .detox-subnav-link:focus {
          color: #ffffff;
          background: #16233b;
        }
        body.dark-mode-pro .detox-subnav-link.is-active {
          background: linear-gradient(135deg, #fb7185 0%, #f97316 100%);
          color: #ffffff;
          border-color: transparent;
          box-shadow: 0 12px 24px rgba(249, 115, 22, 0.22);
        }
        body.dark-mode-pro .detox-content-intro h2,
        body.dark-mode-pro .detox-chip,
        body.dark-mode-pro .detox-sidebar-panel h4 {
          color: #f8fafc;
        }
        body.dark-mode-pro .detox-content-intro p,
        body.dark-mode-pro .detox-sidebar-panel p {
          color: #cbd5e1;
        }
        body.dark-mode-pro .detox-brand-title,
        body.dark-mode-pro .detox-brand-copy p,
        body.dark-mode-pro .detox-chip,
        body.dark-mode-pro .detox-chip strong {
          color: #f8fafc !important;
        }
        body.dark-mode-pro .detox-chip {
          background: rgba(15, 23, 42, 0.9) !important;
          border-color: #334155 !important;
          box-shadow: 0 8px 20px rgba(2, 6, 23, 0.28);
        }
        body.dark-mode-pro .detox-chip.current-module {
          background: rgba(15, 23, 42, 0.96) !important;
          border-color: #475569 !important;
        }
        body.dark-mode-pro .detox-chip.current-module strong {
          color: #fdba74 !important;
        }
        body.dark-mode-pro .upload-note {
          color: #e5e7eb !important;
          border: 1px solid #334155;
        }
        body.dark-mode-pro .upload-note.upload-note-success {
          background: #1f3a28 !important;
        }
        body.dark-mode-pro .upload-note.upload-note-info {
          background: #173247 !important;
        }
        body.dark-mode-pro .upload-note strong,
        body.dark-mode-pro .upload-note .fa,
        body.dark-mode-pro .upload-note .fas {
          color: #f8fafc !important;
        }
        body.dark-mode-pro .detox-nav-link {
          color: rgba(248, 250, 252, 0.82);
          background: rgba(255, 255, 255, 0.05);
        }
        body.dark-mode-pro .detox-nav-link.is-active {
          background: linear-gradient(135deg, #fb7185 0%, #f97316 100%);
        }
        body.dark-mode-pro .dataTables_wrapper table.dataTable,
        body.dark-mode-pro .dataTables_wrapper table.dataTable thead th,
        body.dark-mode-pro .dataTables_wrapper table.dataTable tbody td,
        body.dark-mode-pro .dataTables_wrapper .dataTables_info,
        body.dark-mode-pro .dataTables_wrapper .dataTables_paginate .paginate_button,
        body.dark-mode-pro .dataTables_wrapper .dataTables_filter,
        body.dark-mode-pro .dataTables_wrapper .dataTables_length,
        body.dark-mode-pro .dataTables_wrapper .dataTables_filter label,
        body.dark-mode-pro .dataTables_wrapper .dataTables_length label {
          color: #e5e7eb !important;
        }
        body.dark-mode-pro .dataTables_wrapper table.dataTable thead th,
        body.dark-mode-pro .dataTables_wrapper table.dataTable tbody td {
          border-color: #243244 !important;
          background-color: transparent !important;
        }
        body.dark-mode-pro .dataTables_wrapper .dataTables_filter input,
        body.dark-mode-pro .dataTables_wrapper .dataTables_length select,
        body.dark-mode-pro .dataTables_wrapper .dataTables_paginate .paginate_button,
        body.dark-mode-pro .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        body.dark-mode-pro .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          color: #e5e7eb !important;
          background: #111827 !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro .selectize-dropdown,
        body.dark-mode-pro .selectize-dropdown .option,
        body.dark-mode-pro .selectize-dropdown .optgroup-header {
          background: #111827 !important;
          color: #e5e7eb !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro .checkbox,
        body.dark-mode-pro .radio,
        body.dark-mode-pro .help-block {
          color: #e5e7eb !important;
        }
        @media (max-width: 1200px) {
          .detox-layout {
            grid-template-columns: 280px minmax(0, 1fr);
          }
        }
        @media (max-width: 992px) {
          .detox-menu-toggle {
            display: inline-flex;
            align-items: center;
            justify-content: center;
          }
          .detox-layout {
            grid-template-columns: 1fr;
            padding: 0 16px 20px;
          }
          .detox-sidebar-panel {
            position: fixed;
            top: 0;
            left: 0;
            width: min(86vw, 320px);
            height: 100vh;
            z-index: 999;
            border-radius: 0 28px 28px 0;
            transform: translateX(-104%);
            transition: transform .22s ease;
            overflow-y: auto;
          }
          .detox-header {
            padding: 18px 16px 16px;
          }
          .detox-content-card {
            padding: 16px;
            border-radius: 20px;
          }
          body.detox-mobile-open .detox-sidebar-panel {
            transform: translateX(0);
          }
          body.detox-mobile-open .detox-mobile-backdrop {
            display: block;
          }
        }
      "))
    ),
    div(
      class = "boxed_wrapper ltr detox-app-shell",
      div(class = "detox-mobile-backdrop js-detox-backdrop"),
      tags$header(
        class = "detox-header",
        div(
          class = "detox-header-inner",
          div(
            class = "detox-brand-wrap",
            tags$button(
              class = "detox-menu-toggle js-detox-menu-toggle",
              type = "button",
              icon("bars")
            ),
            div(
              class = "detox-brand-copy",
              tags$h1(class = "detox-brand-title", "ModelCraft: Statistical Learning in R"),
              tags$p("Upload any dataset, explore it visually, and run your full CRD and regression workflow in one place.")
            )
          ),
          div(
            class = "detox-header-badges",
            div(class = "detox-chip current-module", icon("compass"), "Current Module:", tags$strong(id = "detox-current-tab", "Upload Data"))
          )
        )
      ),
      div(
        class = "detox-layout",
          tags$aside(
          class = "detox-sidebar-panel",
          tags$h4("Analysis Modules"),
          tags$p("Switch between data preparation, inference, diagnostics, and reporting without leaving the same interface."),
          build_detox_nav(main_nav_sections)
        ),
        tags$main(
          class = "detox-main",
          div(
            class = "detox-content-card",
            conditionalPanel(
              condition = "['upload','preview','varanalysis','correlation','catsuite','tests'].includes(input.tabs)",
              build_subpage_nav(
                "Data & Analysis",
                Filter(function(x) x$tab %in% grouped_sections$data_analysis, app_sections)
              )
            ),
            conditionalPanel(
              condition = "['slr','mlr','polyreg'].includes(input.tabs)",
              build_subpage_nav(
                "Regression",
                Filter(function(x) x$tab %in% grouped_sections$linear_reg, app_sections)
              )
            ),
            conditionalPanel(
              condition = "['boxtrans','wls'].includes(input.tabs)",
              build_subpage_nav(
                "Correcting Inadequate Models",
                Filter(function(x) x$tab %in% grouped_sections$correcting_models, app_sections)
              )
            ),
    conditionalPanel(
      condition = "input.tabs == 'upload' || input.tabs == 'preview'",
      fluidRow(
        box(
          width = 12, status = "primary", solidHeader = FALSE, class = "quick-actions",
          fluidRow(
            column(
              6,
              tags$div(
                style = "display:flex; gap:8px; flex-wrap:wrap;",
                actionButton("qaSample", "Sample Data", icon = icon("database"), class = "btn btn-primary"),
                actionButton("qaReset", "Reset Data", icon = icon("undo"), class = "btn btn-default"),
                actionButton("presetPriceMileage", "Preset: Price vs Mileage", icon = icon("bolt"), class = "btn btn-info"),
                actionButton("presetBrandCompare", "Preset: Brand Comparison", icon = icon("tags"), class = "btn btn-info"),
                downloadButton("downloadReport", "Download Report", class = "btn btn-success")
              )
            ),
            column(
              3,
              uiOutput("globalVarSearchUI")
            ),
            column(
              3,
              tags$div(
                style = "display:flex; gap:8px; align-items:center; justify-content:flex-end; flex-wrap:wrap;",
                checkboxInput("interactivePlots", "Interactive", value = HAS_PLOTLY),
                checkboxInput("darkModeToggle", "Dark mode", TRUE),
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
              column(
                6,
                fileInput("datafile",
                  "Choose CSV or Excel File",
                  accept = c(".csv", ".xlsx", ".xls"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                )
              ),
              column(
                6,
                br(),
                actionButton("analyzeBtn", "Analyze Data",
                  class = "btn-analyze",
                  icon = icon("chart-bar")
                )
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
              class = "upload-note upload-note-success",
              style = "background-color: #dff0d8; padding: 10px; border-radius: 5px; margin-top: 10px;",
              tags$strong(icon("check-circle"), " Default Dataset Loaded:"),
              " The app starts with P2_DeliveryTime.xlsx, so you can analyze immediately without uploading."
            ),
            tags$div(
              class = "upload-note upload-note-info",
              style = "background-color: #d9edf7; padding: 10px; border-radius: 5px; margin-top: 10px;",
              tags$strong(icon("info-circle"), " File Size Limit:"),
              " Maximum upload size is 50 MB. For larger datasets, consider filtering or sampling your data before upload."
            ),
            hr(),
            h4("Variable Detection:"),
            fluidRow(
              column(
                6,
                h5("Numerical Variables:"),
                verbatimTextOutput("numericVars")
              ),
              column(
                6,
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
              column(
                3,
                selectInput("cleanMissing", "Missing Values:",
                  choices = c(
                    "Keep as is" = "keep",
                    "Drop rows with missing" = "drop",
                    "Impute (median/mode)" = "impute"
                  ),
                  selected = "keep"
                )
              ),
              column(
                3,
                selectInput("cleanOutliers", "Outliers (IQR):",
                  choices = c(
                    "Remove" = "remove",
                    "Keep" = "keep"
                  ),
                  selected = "remove"
                )
              ),
              column(
                3,
                uiOutput("cleanLogVarsUI")
              ),
              column(
                3,
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
              column(
                7,
                h5("Numeric Variables"),
                spn(DT::DTOutput("dataSummaryNumeric"))
              ),
              column(
                5,
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
              column(
                4,
                selectInput("analysisMode", "Number of Variables to Analyze:",
                  choices = c(
                    "1 Variable" = "single",
                    "Multiple Variables (2+)" = "multi"
                  ),
                  selected = "single"
                )
              )
            ),
            conditionalPanel(
              condition = "input.analysisMode == 'single'",
              fluidRow(
                column(
                  4,
                  selectInput("singleVarType", "Variable Type:",
                    choices = c("Numerical", "Categorical")
                  )
                ),
                column(
                  4,
                  uiOutput("singleVarSelect")
                ),
                column(
                  4,
                  br(),
                  actionButton("runSingleAnalysis", "Run Analysis",
                    class = "btn btn-primary"
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.analysisMode == 'multi'",
              fluidRow(
                column(
                  4,
                  selectInput("multiVarCount", "How many variables?",
                    choices = as.character(2:6),
                    selected = "2"
                  )
                )
              ),
              uiOutput("multiVarRows"),
              fluidRow(
                column(
                  12,
                  actionButton("runMultiAnalysis", "Run Analysis",
                    class = "btn btn-primary"
                  )
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
              column(
                8,
                uiOutput("corrVarsSelect")
              ),
              column(
                4,
                br(),
                actionButton("runCorrAnalysis", "Calculate Correlation",
                  class = "btn btn-primary"
                )
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
              column(
                5,
                selectInput("catSuiteMode", "Analysis Type:",
                  choices = c(
                    "Single Categorical Variable" = "singlecat",
                    "Cross-Tabulation (2 Variables)" = "crosstab",
                    "Multi-Categorical (3+ Variables)" = "multicat"
                  ),
                  selected = "singlecat"
                )
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
                column(
                  4,
                  selectInput("catPlotType", "Plot Type:",
                    choices = c("Bar Plot", "Pie Chart", "Both")
                  )
                ),
                column(
                  4,
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
                column(
                  4,
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
                choices = c("Stacked Bar", "Grouped Bar", "Mosaic Plot")
              ),
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
                column(
                  12,
                  br(),
                  actionButton("runMultiCatAnalysis", "Run Multi-Way Analysis",
                    class = "btn btn-warning btn-lg",
                    icon = icon("chart-bar")
                  )
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
                column(
                  4,
                  selectInput("multiCatPlotType", "Visualization Type:",
                    choices = c(
                      "Grouped Bar Chart",
                      "Faceted Bar Chart",
                      "Mosaic Plot",
                      "Heatmap"
                    )
                  )
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
              )
            ),
            uiOutput("testVarInputs"),
            hr(),
            actionButton("runHypothesisTest", "Run Test",
              class = "btn btn-primary"
            )
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
              column(
                10,
                h4("Explore your dataset with interactive visualizations and insights")
              ),
              column(
                2,
                downloadButton("downloadReport", "Download Report",
                  class = "btn-success btn-block",
                  icon = icon("download")
                )
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
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Model Setup"),
            tags$p("Select your response (Y) and predictor (X) variable, then click Fit Model.")
          )
        ),
        fluidRow(
          sample_button_row(
            "SLR",
            actionButton("loadSampleSLR", tagList(icon("database"), "Load P1_RocketPropellant"), class = "btn btn-info"),
            "Loads the chapter sample dataset for Simple Linear Regression."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",
            fluidRow(
              column(
                4,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("slrYSelect")
              ),
              column(
                4,
                tags$div(class = "section-label", "Predictor Variable (X)"),
                uiOutput("slrXSelect")
              ),
              column(
                4,
                tags$div(class = "section-label", "Options"),
                checkboxInput("slrNoIntercept",
                  tagList(icon("minus-circle"), " Force intercept through origin (β₀ = 0)"),
                  value = FALSE
                ),
                br(),
                actionButton("runSLR", tagList(icon("play"), " Fit SLR Model"),
                  class = "fit-btn"
                )
              )
            )
          )
        ),

        # ── Step 2: Exploratory Plot ───────────────────────────
        tags$div(
          class = "reg-step-banner info",
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
        tags$div(
          class = "reg-step-banner success",
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
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Shows fitted coefficients, standard errors, t-statistics, p-values, R², and adjusted R²."
                ),
                verbatimTextOutput("slrSummary")
              ),
              tabPanel(
                tagList(icon("calculator"), " ANOVA Table"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Tests overall model significance (F-test). A small p-value means the model explains\n                  a significant portion of the variation in Y."
                ),
                verbatimTextOutput("slrAnova")
              ),
              tabPanel(
                tagList(icon("arrows-alt-h"), " Confidence Intervals"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " 95% confidence intervals for the regression coefficients (β₀ and β₁)."
                ),
                verbatimTextOutput("slrConfint")
              ),
              tabPanel(
                tagList(icon("link"), " Correlation Tests"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Pearson (parametric), Spearman, and Kendall correlation tests between X and Y."
                ),
                verbatimTextOutput("slrCorrelation")
              )
            )
          )
        ),

        # ── Step 4: Prediction ────────────────────────────────
        tags$div(
          class = "reg-step-banner warning",
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
              column(
                8,
                tags$div(class = "section-label", "New X Value(s)"),
                textInput("slrNewX",
                  label = NULL,
                  placeholder = "Enter values separated by commas, e.g.  20, 30"
                ),
                tags$div(
                  class = "hint-text",
                  icon("info-circle"), " Each value produces one row of output."
                )
              ),
              column(
                4,
                br(),
                actionButton("runSLRPredict",
                  tagList(icon("magic"), " Predict"),
                  class = "predict-btn"
                )
              )
            ),
            hr(style = "margin: 16px 0;"),
            tabsetPanel(
              type = "pills",
              tabPanel(
                tagList(icon("expand-arrows-alt"), " Prediction Interval"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Wider interval — covers where a single new observation is likely to fall."
                ),
                DT::DTOutput("slrPredInterval")
              ),
              tabPanel(
                tagList(icon("compress-arrows-alt"), " Confidence Interval"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Narrower interval — estimates the mean response at the given X value."
                ),
                DT::DTOutput("slrConfInterval")
              ),
              tabPanel(
                tagList(icon("search-location"), " Interpolation / Extrapolation"),
                br(),
                tags$div(
                  class = "reg-help-alert",
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
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Model Setup"),
            tags$p("Choose the response (Y) and two or more predictor variables (X), then click Fit Model.")
          )
        ),
        fluidRow(
          sample_button_row(
            "MLR",
            actionButton("loadSampleMLR", tagList(icon("database"), "Load P2_DeliveryTime"), class = "btn btn-info"),
            "Loads the chapter sample dataset for Multiple Linear Regression."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",
            fluidRow(
              column(
                3,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("mlrYSelect")
              ),
              column(
                6,
                tags$div(class = "section-label", "Predictor Variables (X) — select 2 or more"),
                uiOutput("mlrXSelect"),
                tags$div(
                  class = "hint-text",
                  icon("info-circle"), " Hold Ctrl / Cmd to select multiple variables."
                )
              ),
              column(
                3,
                tags$div(class = "section-label", "Options"),
                checkboxInput("mlrNoIntercept",
                  tagList(icon("minus-circle"), " Force intercept through origin"),
                  value = FALSE
                ),
                br(),
                actionButton("runMLR", tagList(icon("play"), " Fit MLR Model"),
                  class = "fit-btn"
                )
              )
            )
          )
        ),

        # ── Step 2: Exploratory Plots ──────────────────────────
        tags$div(
          class = "reg-step-banner info",
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
        tags$div(
          class = "reg-step-banner success",
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
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Displays fitted coefficients, standard errors, t-tests for each predictor,\n                  overall F-test, R², and adjusted R²."
                ),
                verbatimTextOutput("mlrSummary")
              ),
              tabPanel(
                tagList(icon("calculator"), " ANOVA Table"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Sequential (Type I) ANOVA — shows the contribution of each predictor\n                  added in order."
                ),
                verbatimTextOutput("mlrAnova")
              ),
              tabPanel(
                tagList(icon("vial"), " Partial F-Tests"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Tests H₀: βᵢ = 0 for each predictor by comparing the full model against\n                  a reduced model without that predictor."
                ),
                verbatimTextOutput("mlrPartialF")
              ),
              tabPanel(
                tagList(icon("arrows-alt-h"), " Confidence Intervals"),
                br(),
                tags$div(
                  class = "reg-help-alert",
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
            tags$h4(
              style = "margin:0;font-size:16px;font-weight:600;",
              "Multicollinearity Diagnostics — VIF"
            ),
            tags$p(
              style = "margin:2px 0 0;font-size:12px;opacity:.85;",
              "VIF < 5: acceptable  |  5–10: moderate concern  |  > 10: severe — consider removing predictors."
            )
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
        tags$div(
          class = "reg-step-banner warning",
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
              column(12,
                style = "margin-top:8px;",
                tags$div(
                  class = "hint-text",
                  icon("info-circle"),
                  " Enter one or more comma-separated values per predictor.
                    Each position across all fields corresponds to one new observation."
                )
              )
            ),
            br(),
            actionButton("runMLRPredict",
              tagList(icon("magic"), " Predict"),
              class = "predict-btn"
            ),
            hr(style = "margin:16px 0;"),
            tabsetPanel(
              type = "pills",
              tabPanel(
                tagList(icon("expand-arrows-alt"), " Prediction Interval"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Wider interval — covers where a new individual observation is likely to fall."
                ),
                DT::DTOutput("mlrPredInterval")
              ),
              tabPanel(
                tagList(icon("compress-arrows-alt"), " Confidence Interval"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Narrower interval — estimates the mean response at the given predictor values."
                ),
                DT::DTOutput("mlrConfInterval")
              ),
              tabPanel(
                tagList(icon("search-location"), " Interpolation / Extrapolation"),
                br(),
                tags$div(
                  class = "reg-help-alert",
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
      # CHAPTER 7: POLYNOMIAL REGRESSION MODELS TAB
      # ========================================================
      tabItem(
        tabName = "polyreg",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", icon("chart-area")),
          tags$div(
            tags$h4("Polynomial Regression and Splines"),
            tags$p("Compare polynomial orders, inspect centering effects on multicollinearity, and fit spline models for piecewise behavior.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Polynomial Regression",
            tagList(
              actionButton("loadSamplePolyP5", tagList(icon("database"), "Load P5_HardWood"), class = "btn btn-info"),
              actionButton("loadSamplePolyVoltage", tagList(icon("database"), "Load VoltageDrop"), class = "btn btn-info")
            ),
            "Use P5_HardWood for polynomial/centering examples and VoltageDrop for spline examples."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(
                2,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("polyYSelect")
              ),
              column(
                2,
                tags$div(class = "section-label", "Predictor Variable (X)"),
                uiOutput("polyXSelect")
              ),
              column(
                2,
                tags$div(class = "section-label", "Polynomial Degree"),
                selectInput("polyDegree", NULL, choices = c("1 (Linear)" = 1, "2 (Quadratic)" = 2, "3 (Cubic)" = 3, "4 (Quartic)" = 4), selected = 3)
              ),
              column(
                2,
                tags$div(class = "section-label", "Spline Breakpoint 1"),
                numericInput("polyKnot1", NULL, value = 6.5, step = 0.5)
              ),
              column(
                2,
                tags$div(class = "section-label", "Spline Breakpoint 2"),
                numericInput("polyKnot2", NULL, value = 13, step = 0.5)
              ),
              column(
                2,
                tags$div(class = "section-label", "Single Breakpoint"),
                numericInput("polySingleKnot", NULL, value = 10, step = 0.5)
              )
            ),
            fluidRow(
              column(
                12,
                style = "margin-top:10px;",
                tags$div(
                  class = "hint-text",
                  icon("info-circle"),
                  "For Topic 7, P5_HardWood is the default polynomial example and VoltageDrop is the default spline example."
                )
              )
            ),
            br(),
            actionButton("runPoly", tagList(icon("play"), " Run Chapter 7 Analysis"), class = "fit-btn")
          )
        ),

        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Polynomial Order Comparison"),
            tags$p("Fit linear through quartic models to see how model order affects fit quality and residual behavior.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Polynomial Model Comparison"),
            solidHeader = TRUE, status = "info", width = 6,
            DTOutput("polyCompareTable")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Polynomial Fits on Scatterplot"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("polyFitPlot", height = "340px")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("calculator"), " Selected Polynomial Summary"),
            solidHeader = TRUE, status = "info", width = 6,
            verbatimTextOutput("polySummary")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-area"), " Residual Diagnostics by Order"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("polyResidualPlot", height = "340px")
          )
        ),

        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Centering and Multicollinearity"),
            tags$p("Centering keeps the same fitted curve while usually reducing VIF inflation in quadratic and cubic models.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("balance-scale"), " Raw vs Centered Comparison"),
            solidHeader = TRUE, status = "success", width = 6,
            DTOutput("polyCenterCompare")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-bar"), " VIF: Raw vs Centered"),
            solidHeader = TRUE, status = "success", width = 6,
            plotOutput("polyVifPlot", height = "320px")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("list-ol"), " Centering Interpretation"),
            solidHeader = TRUE, status = "success", width = 12,
            verbatimTextOutput("polyCenterText")
          )
        ),

        tags$div(
          class = "reg-step-banner warning",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Spline Models"),
            tags$p("Use one-knot and two-knot splines to model piecewise changes in the response, especially for the VoltageDrop data.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Spline Model Comparison"),
            solidHeader = TRUE, status = "warning", width = 6,
            DTOutput("polySplineCompare")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("project-diagram"), " Polynomial vs Cubic Spline Fit"),
            solidHeader = TRUE, status = "warning", width = 6,
            plotOutput("polySplinePlot", height = "340px")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("calculator"), " Cubic Spline Summary"),
            solidHeader = TRUE, status = "warning", width = 6,
            verbatimTextOutput("polySplineSummary")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Cubic Spline Residual Plot"),
            solidHeader = TRUE, status = "warning", width = 6,
            plotOutput("polySplineResidualPlot", height = "340px")
          )
        )
      ),

      # ========================================================
      # CHAPTER 6: LEVERAGE & INFLUENCE DIAGNOSTICS TAB
      # ========================================================
      tabItem(
        tabName = "influence",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", icon("crosshairs")),
          tags$div(
            tags$h4("Diagnostics for Leverage and Influential Points"),
            tags$p("Fit a multiple regression, inspect leverage and influence measures, then compare OLS with robust alternatives.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Leverage & Influence",
            actionButton("loadSampleInfluence", tagList(icon("database"), "Load P4_RealEstate"), class = "btn btn-info"),
            "Loads P4_RealEstate, the default dataset for Topic 6 diagnostics."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(
                3,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("inflYSelect")
              ),
              column(
                6,
                tags$div(class = "section-label", "Predictors (X)"),
                uiOutput("inflXSelect")
              ),
              column(
                3,
                br(),
                actionButton("runInfluence", tagList(icon("play"), " Run Diagnostics"), class = "fit-btn")
              )
            ),
            fluidRow(
              column(
                12,
                style = "margin-top:10px;",
                tags$div(
                  class = "hint-text",
                  icon("info-circle"),
                  "The lecture defaults for P4_RealEstate are y ~ x1 + x2 + x3 + x4 + x5. Any numeric upload can also be analyzed here."
                )
              )
            )
          )
        ),

        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Influence Measures"),
            tags$p("Review leverage, Cook's D, DFFITS, DFBETAS, and COVRATIO using standard cutoff rules.")
          )
        ),
        fluidRow(
          valueBoxOutput("inflNBox", width = 3),
          valueBoxOutput("inflPBox", width = 3),
          valueBoxOutput("inflLeverageBox", width = 3),
          valueBoxOutput("inflFlaggedBox", width = 3)
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Observation Diagnostics Table"),
            solidHeader = TRUE, status = "info", width = 12,
            DTOutput("inflTable")
          )
        ),

        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Visual Influence Diagnostics"),
            tags$p("Use the plots to identify cases that combine unusual predictor values with large residual impact.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("dot-circle"), " Leverage vs Studentized Residuals"),
            solidHeader = TRUE, status = "success", width = 6,
            plotOutput("inflBubblePlot", height = "340px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Cook's Distance by Observation"),
            solidHeader = TRUE, status = "success", width = 6,
            plotOutput("inflCookPlot", height = "340px")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-area"), " DFFITS by Observation"),
            solidHeader = TRUE, status = "success", width = 6,
            plotOutput("inflDffitsPlot", height = "300px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("exclamation-triangle"), " Flagged Observations"),
            solidHeader = TRUE, status = "success", width = 6,
            verbatimTextOutput("inflFlagSummary")
          )
        ),

        tags$div(
          class = "reg-step-banner warning",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Refit Without Selected Cases"),
            tags$p("Compare the original fit against a trimmed OLS model after removing influential observations.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("filter"), " Removal List"),
            solidHeader = TRUE, status = "warning", width = 12,
            textInput("inflRemoveRows", "Observation numbers to remove (comma-separated):", value = ""),
            tags$div(
              class = "hint-text",
              "The input is pre-filled from the flagged cases after running diagnostics, and you can edit it before refitting."
            ),
            br(),
            actionButton("runInfluenceTrim", tagList(icon("eraser"), " Refit Without These Cases"), class = "btn btn-warning")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("balance-scale"), " Coefficient Comparison"),
            solidHeader = TRUE, status = "warning", width = 7,
            DTOutput("inflCoefCompare")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("list-alt"), " Model Metrics"),
            solidHeader = TRUE, status = "warning", width = 5,
            verbatimTextOutput("inflMetricCompare")
          )
        ),

        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", "4"),
          tags$div(
            tags$h4("Robust Regression"),
            tags$p("Compare classical OLS with Huber and Tukey bisquare M-estimation instead of deleting valid but influential observations.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("shield-alt"), " Robust Model Summaries"),
            solidHeader = TRUE, status = "primary", width = 6,
            verbatimTextOutput("inflRobustSummary")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("table"), " OLS vs Robust Coefficients"),
            solidHeader = TRUE, status = "primary", width = 6,
            DTOutput("inflRobustCompare")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Huber Residuals vs Weights"),
            solidHeader = TRUE, status = "primary", width = 6,
            plotOutput("inflHuberPlot", height = "300px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Bisquare Residuals vs Weights"),
            solidHeader = TRUE, status = "primary", width = 6,
            plotOutput("inflBisquarePlot", height = "300px")
          )
        )
      ),

      # ========================================================
      # CHAPTER 9: MULTICOLLINEARITY TAB
      # ========================================================
      tabItem(
        tabName = "ch9multi",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Quadratic Surface Setup"),
            tags$p("Select one response and exactly 3 predictors, then fit the full second-order model.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Multicollinearity",
            actionButton("loadSampleCh9", tagList(icon("database"), "Load Acetylene Data"), class = "btn btn-info"),
            "Loads the Acetylene dataset used for the multicollinearity chapter."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",
            fluidRow(
              column(
                3,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("ch9YSelect")
              ),
              column(
                6,
                tags$div(class = "section-label", "Predictors (Select Exactly 3)"),
                uiOutput("ch9XSelect")
              ),
              column(
                3,
                tags$div(class = "section-label", "Run"),
                actionButton("runCh9", tagList(icon("play"), " Run Analysis"),
                  class = "fit-btn"
                ),
                br(), br(),
                if (!has_glmnet()) {
                  tags$span(
                    style = "color:#b91c1c;font-weight:600;",
                    "Optional: install 'glmnet' to enable ridge/lasso."
                  )
                }
              )
            )
          )
        ),
        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Original Data Model and VIF"),
            tags$p("Full quadratic model using original predictors with multicollinearity diagnostics.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Full Model Summary (Original Data)"),
            solidHeader = TRUE, status = "info", width = 8,
            verbatimTextOutput("ch9ModelOrig")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("list-ol"), " VIF Values (Original Data)"),
            solidHeader = TRUE, status = "info", width = 4,
            DT::DTOutput("ch9VIFOrigTbl")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-bar"), " VIF Chart (Original Data)"),
            solidHeader = TRUE, status = "info", width = 12,
            plotOutput("ch9VIFOrigPlot", height = "300px")
          )
        ),
        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Centered Predictors Model"),
            tags$p("Apply centering/scaling, refit the same second-order model, and compare VIF values.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("info-circle"), " Centering Summary"),
            solidHeader = TRUE, status = "success", width = 4,
            verbatimTextOutput("ch9CenterInfo")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Full Model Summary (Centered Data)"),
            solidHeader = TRUE, status = "success", width = 8,
            verbatimTextOutput("ch9ModelCent")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("list-ol"), " VIF Values (Centered Data)"),
            solidHeader = TRUE, status = "success", width = 4,
            DT::DTOutput("ch9VIFCentTbl")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-bar"), " VIF Chart (Centered Data)"),
            solidHeader = TRUE, status = "success", width = 8,
            plotOutput("ch9VIFCentPlot", height = "300px")
          )
        ),
        tags$div(
          class = "reg-step-banner warning",
          tags$div(class = "step-badge", "4"),
          tags$div(
            tags$h4("Ridge and LASSO via Cross-Validation"),
            tags$p("Cross-validation selects lambda, inspects coefficients, then compares predictions.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Ridge CV Curve"),
            solidHeader = TRUE, status = "warning", width = 6,
            plotOutput("ch9RidgeCVPlot", height = "300px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("calculator"), " Ridge Results"),
            solidHeader = TRUE, status = "warning", width = 6,
            DT::DTOutput("ch9RidgeTbl"),
            br(),
            htmlOutput("ch9RidgeEq"),
            br(),
            tags$h4("Ridge-Adjusted VIF Table", style = "margin:6px 0 10px 0; font-weight:700;"),
            DT::DTOutput("ch9RidgeVifTbl")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " LASSO CV Curve"),
            solidHeader = TRUE, status = "danger", width = 6,
            plotOutput("ch9LassoCVPlot", height = "300px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("calculator"), " LASSO Results"),
            solidHeader = TRUE, status = "danger", width = 6,
            DT::DTOutput("ch9LassoTbl"),
            br(),
            htmlOutput("ch9LassoEq"),
            br(),
            tags$h4("Post-LASSO VIF Table", style = "margin:6px 0 10px 0; font-weight:700;"),
            DT::DTOutput("ch9LassoVifTbl")
          )
        ),
        tags$div(
          style = "background: linear-gradient(135deg,#1f2937,#111827); color:#fff;
                   border-radius:8px; padding:14px 20px; margin-bottom:18px;
                   display:flex; align-items:center; gap:14px;
                   box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class = "step-badge", "5"),
          tags$div(
            tags$h4(
              style = "margin:0;font-size:16px;font-weight:600;",
              "Predictions and Model Comparison"
            ),
            tags$p(
              style = "margin:2px 0 0;font-size:12px;opacity:.85;",
              "Enter new X values to compare OLS, Ridge, and LASSO predictions, then inspect CV RMSE."
            )
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("edit"), " New Predictor Values"),
            solidHeader = TRUE, status = "primary", width = 12,
            uiOutput("ch9NewXInputs"),
            br(),
            actionButton("runCh9Predict", tagList(icon("magic"), " Predict Models"),
              class = "predict-btn"
            )
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Prediction Table"),
            solidHeader = TRUE, status = "primary", width = 7,
            DT::DTOutput("ch9PredTable")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-bar"), " CV RMSE Comparison"),
            solidHeader = TRUE, status = "primary", width = 5,
            plotOutput("ch9ComparePlot", height = "290px")
          )
        )
      ),

      # ========================================================
      # CHAPTER 10: MODEL BUILDING TAB
      # ========================================================
      tabItem(
        tabName = "modelbuilding",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", "10"),
          tags$div(
            tags$h4("Model Building and Variable Selection"),
            tags$p("Compare the full model, all possible regressions, and stepwise procedures.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Model Building",
            actionButton("loadSampleMB", tagList(icon("database"), "Load P3_HaldsCement"), class = "btn btn-info"),
            "Loads the chapter sample dataset for Topic 10 model building."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",
            fluidRow(
              column(
                3,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("mbYSelect")
              ),
              column(
                6,
                tags$div(class = "section-label", "Candidate Predictors"),
                uiOutput("mbXSelect")
              ),
              column(
                3,
                tags$div(class = "section-label", "Run"),
                actionButton("runMB", tagList(icon("play"), " Build Models"),
                  class = "fit-btn"
                )
              )
            ),
            hr(),
            fluidRow(
              column(
                3,
                numericInput("mbPEnter", "Forward p-enter:", value = 0.10, min = 0.001, max = 0.99, step = 0.01)
              ),
              column(
                3,
                numericInput("mbPRemove", "Backward p-remove:", value = 0.15, min = 0.001, max = 0.99, step = 0.01)
              ),
              column(
                4,
                uiOutput("mbFinalXSelect")
              ),
              column(
                2,
                br(),
                actionButton("runMBFinal", tagList(icon("check"), " Fit Selected"),
                  class = "predict-btn"
                )
              )
            )
          )
        ),

        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Full Model and Screening"),
            tags$p("Start from the full model, inspect correlations, and check VIF values.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Full Model Summary"),
            solidHeader = TRUE, status = "info", width = 7,
            htmlOutput("mbFullStats"),
            br(),
            DT::DTOutput("mbFullCoefTbl")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("calculator"), " ANOVA and VIF"),
            solidHeader = TRUE, status = "info", width = 5,
            tabsetPanel(
              type = "tabs",
              tabPanel(tagList(icon("calculator"), " ANOVA"), br(), DT::DTOutput("mbFullAnovaTbl")),
              tabPanel(tagList(icon("shield-alt"), " VIF"), br(), DT::DTOutput("mbFullVifTbl"))
            )
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("border-all"), " Correlation Matrix"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("mbCorrPlot", height = "330px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-bar"), " VIF Chart"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("mbVifPlot", height = "330px")
          )
        ),

        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("All Possible Regressions"),
            tags$p("Evaluate every candidate subset using adjusted R-squared, MSE, predicted R-squared, Cp, AIC, and BIC.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("table"), " All Candidate Models"),
            solidHeader = TRUE, status = "success", width = 12,
            DT::DTOutput("mbAllModels")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("award"), " Best Model by Criterion"),
            solidHeader = TRUE, status = "success", width = 5,
            DT::DTOutput("mbBestCriteria")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("layer-group"), " Best Model of Each Size"),
            solidHeader = TRUE, status = "success", width = 7,
            DT::DTOutput("mbBestBySize")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Mallows Cp Plot"),
            solidHeader = TRUE, status = "success", width = 6,
            plotOutput("mbCpPlot", height = "320px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("chart-area"), " Selection Criteria by Model Size"),
            solidHeader = TRUE, status = "success", width = 6,
            plotOutput("mbCriteriaPlot", height = "320px")
          )
        ),

        tags$div(
          class = "reg-step-banner warning",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Forward, Backward, and Stepwise Selection"),
            tags$p("Run p-value based selection using the same p-enter and p-remove ideas from the chapter.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("arrow-right"), " Forward Selection"),
            solidHeader = TRUE, status = "warning", width = 4,
            htmlOutput("mbForwardTrace")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("arrow-left"), " Backward Elimination"),
            solidHeader = TRUE, status = "warning", width = 4,
            htmlOutput("mbBackwardTrace")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("exchange-alt"), " Stepwise Selection"),
            solidHeader = TRUE, status = "warning", width = 4,
            htmlOutput("mbStepwiseTrace")
          )
        ),

        tags$div(
          style = "background: linear-gradient(135deg,#1f2937,#111827); color:#fff;
                   border-radius:8px; padding:14px 20px; margin-bottom:18px;
                   display:flex; align-items:center; gap:14px;
                   box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class = "step-badge", "4"),
          tags$div(
            tags$h4(
              style = "margin:0;font-size:16px;font-weight:600;",
              "Final Candidate Model"
            ),
            tags$p(
              style = "margin:2px 0 0;font-size:12px;opacity:.85;",
              "Fit the selected predictors and inspect the model before using it."
            )
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("check-circle"), " Selected Model Summary"),
            solidHeader = TRUE, status = "primary", width = 7,
            htmlOutput("mbFinalStats"),
            tabsetPanel(
              type = "tabs",
              tabPanel(tagList(icon("list-ol"), " Coefficients"), br(), DT::DTOutput("mbFinalCoefTbl")),
              tabPanel(tagList(icon("calculator"), " ANOVA"), br(), DT::DTOutput("mbFinalAnovaTbl")),
              tabPanel(tagList(icon("shield-alt"), " VIF"), br(), DT::DTOutput("mbFinalVifTbl"))
            )
          ),
          box(
            class = "reg-card",
            title = tagList(icon("lightbulb"), " Chapter Interpretation"),
            solidHeader = TRUE, status = "primary", width = 5,
            htmlOutput("mbInterpretation")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Residual Diagnostics"),
            solidHeader = TRUE, status = "primary", width = 12,
            plotOutput("mbFinalDiagnostics", height = "320px")
          )
        )
      ),

      # ========================================================
      # CHAPTER 11: GENERALIZED LINEAR MODELS TAB
      # ========================================================
      tabItem(
        tabName = "glm",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", icon("wave-square")),
          tags$div(
            tags$h4("Generalized Linear Models"),
            tags$p("Fit logit, probit, and Poisson models for binary and count responses using the Topic 11 datasets.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Generalized Linear Models",
            tagList(
              actionButton("loadSampleGLMWellness", tagList(icon("database"), "Load Wellness"), class = "btn btn-info"),
              actionButton("loadSampleGLMAdmit", tagList(icon("database"), "Load Admit"), class = "btn btn-info"),
              actionButton("loadSampleGLMAwards", tagList(icon("database"), "Load Awards"), class = "btn btn-info")
            ),
            "Use Wellness and Admit for binary models, and Awards for Poisson count regression."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(
                3,
                tags$div(class = "section-label", "GLM Family"),
                selectInput("glmFamily", NULL,
                  choices = c(
                    "Logit (Binary)" = "logit",
                    "Probit (Binary)" = "probit",
                    "Poisson (Count)" = "poisson"
                  ),
                  selected = "logit"
                )
              ),
              column(
                3,
                tags$div(class = "section-label", "Response Variable"),
                uiOutput("glmYSelect")
              ),
              column(
                4,
                tags$div(class = "section-label", "Predictors"),
                uiOutput("glmXSelect")
              ),
              column(
                2,
                br(),
                actionButton("runGLM", tagList(icon("play"), " Run GLM"), class = "fit-btn")
              )
            ),
            fluidRow(
              column(
                12,
                style = "margin-top:10px;",
                tags$div(
                  class = "hint-text",
                  icon("info-circle"),
                  "Recommended defaults: Wellness -> logit/probit with work, Admit -> logit/probit with gre + gpa + rank, Awards -> Poisson with prog + math."
                )
              )
            )
          )
        ),

        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Model Fit and Interpretation"),
            tags$p("Review the fitted GLM, coefficient significance, and transformed interpretation as odds ratios or incidence-rate ratios.")
          )
        ),
        fluidRow(
          valueBoxOutput("glmObsBox", width = 3),
          valueBoxOutput("glmAICBox", width = 3),
          valueBoxOutput("glmDevBox", width = 3),
          valueBoxOutput("glmPerfBox", width = 3)
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("calculator"), " GLM Summary"),
            solidHeader = TRUE, status = "info", width = 6,
            verbatimTextOutput("glmSummary")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Coefficients and Effect Sizes"),
            solidHeader = TRUE, status = "info", width = 6,
            DTOutput("glmCoefTable")
          )
        ),

        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Observed vs Fitted Response"),
            tags$p("Visualize fitted probabilities or expected counts alongside the original data.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("chart-line"), " Response Curve / Fitted Counts"),
            solidHeader = TRUE, status = "success", width = 7,
            plotOutput("glmFitPlot", height = "360px")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("tasks"), " Performance / Model Notes"),
            solidHeader = TRUE, status = "success", width = 5,
            verbatimTextOutput("glmPerformanceText")
          )
        ),

        tags$div(
          class = "reg-step-banner warning",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Family Comparison"),
            tags$p("For binary outcomes, compare logit and probit. For counts, compare observed and fitted rates under the Poisson model.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            title = tagList(icon("balance-scale"), " Companion Model Comparison"),
            solidHeader = TRUE, status = "warning", width = 6,
            DTOutput("glmCompareTable")
          ),
          box(
            class = "reg-card",
            title = tagList(icon("table"), " Prediction Table"),
            solidHeader = TRUE, status = "warning", width = 6,
            DTOutput("glmPredTable")
          )
        )
      ),

      # ========================================================
      # INDICATOR VARIABLE TAB
      # ========================================================
      tabItem(
        tabName = "indicator",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Indicator (Dummy) Variable Setup"),
            tags$p("Choose Y, one or two qualitative factors, and optionally a numeric X. The app uses k-1 dummy variables per factor.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Indicator Variables",
            actionButton("loadSampleIndicator", tagList(icon("database"), "Load P11_ToolLife"), class = "btn btn-info"),
            "Loads the Tool Life dataset used for dummy-variable examples."
          )
        ),
        fluidRow(
          box(
            class = "reg-card",
            status = "primary", solidHeader = FALSE,
            width = 12, style = "padding: 18px;",
            fluidRow(
              column(
                4,
                tags$div(class = "section-label", "Number of Indicator Factors"),
                selectInput("indFactorCount", NULL,
                  choices = c("1 Factor" = "1", "2 Factors" = "2"),
                  selected = "1"
                )
              ),
              column(
                4,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("indYSelect")
              ),
              column(4)
            ),
            fluidRow(
              column(
                6,
                tags$div(class = "section-label", "Categorical Variable 1"),
                uiOutput("indFactorSelect")
              ),
              column(
                6,
                tags$div(class = "section-label", "Reference Level 1"),
                uiOutput("indRefSelect")
              )
            ),
            conditionalPanel(
              condition = "input.indFactorCount == '2'",
              fluidRow(
                column(
                  6,
                  tags$div(class = "section-label", "Categorical Variable 2"),
                  uiOutput("indFactor2Select")
                ),
                column(
                  6,
                  tags$div(class = "section-label", "Reference Level 2"),
                  uiOutput("indRef2Select")
                )
              )
            ),
            fluidRow(
              column(
                6,
                tags$div(class = "section-label", "Numeric Predictor (Optional)"),
                uiOutput("indXSelect")
              ),
              column(6)
            ),
            fluidRow(
              column(
                8,
                checkboxInput("indInteraction",
                  tagList(icon("random"), " Include interaction with X (non-parallel slopes)"),
                  value = FALSE
                ),
                tags$div(
                  class = "hint-text",
                  icon("info-circle"),
                  " For 1 factor: X×Factor. For 2 factors: X×Factor1 and X×Factor2."
                )
              ),
              column(
                4,
                br(),
                actionButton("runIndicator",
                  tagList(icon("play"), " Fit Indicator Model"),
                  class = "fit-btn"
                )
              )
            )
          )
        ),
        tags$div(
          class = "reg-step-banner info",
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
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", icon("stethoscope")),
          tags$div(
            tags$h4("Model Adequacy Checks"),
            tags$p("Select a fitted regression model (SLR or MLR) and click Run Diagnostics.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Model Adequacy",
            actionButton("loadSampleMA", tagList(icon("database"), "Load P2_DeliveryTime"), class = "btn btn-info"),
            "Loads the Delivery Time dataset commonly used with the MLR adequacy checks."
          )
        ),
        fluidRow(
          box(
            class = "reg-card", status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(
                4,
                tags$div(class = "section-label", "Model Type"),
                selectInput("maModelType", NULL,
                  choices = c(
                    "Simple Linear Regression (SLR)" = "slr",
                    "Multiple Linear Regression (MLR)" = "mlr"
                  )
                )
              ),
              column(
                4,
                tags$div(class = "section-label", "Predictor(s) hint"),
                uiOutput("maXHint")
              ),
              column(
                4,
                br(),
                actionButton("runMA", tagList(icon("play"), " Run Diagnostics"),
                  class = "fit-btn"
                )
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
        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Normality of Residuals"),
            tags$p("QQ-plot (informal) and Shapiro-Wilk test (formal).")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " QQ Plots"),
            solidHeader = TRUE, status = "info", width = 8,
            plotOutput("maQQPlot", height = "320px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " Shapiro-Wilk Test"),
            solidHeader = TRUE, status = "info", width = 4,
            verbatimTextOutput("maShapiro")
          )
        ),

        # Homoscedasticity
        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Constant Variance (Homoscedasticity)"),
            tags$p("Residuals vs Fitted plot (informal) and Breusch-Pagan test (formal).")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " Residuals vs Fitted"),
            solidHeader = TRUE, status = "success", width = 8,
            plotOutput("maResFitted", height = "320px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " Breusch-Pagan Test"),
            solidHeader = TRUE, status = "success", width = 4,
            verbatimTextOutput("maBPTest")
          )
        ),

        # Independence
        tags$div(
          class = "reg-step-banner warning",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Independence of Residuals"),
            tags$p("Residuals vs Observation Order (informal) and Durbin-Watson test (formal).")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " Residuals vs Order"),
            solidHeader = TRUE, status = "warning", width = 8,
            plotOutput("maResOrder", height = "320px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " Durbin-Watson Test"),
            solidHeader = TRUE, status = "warning", width = 4,
            verbatimTextOutput("maDWTest")
          )
        ),

        # Added-variable plots
        tags$div(
          style = "background:linear-gradient(135deg,#4a148c,#311b92);color:#fff;
                 border-radius:8px;padding:14px 20px;margin-bottom:18px;
                 display:flex;align-items:center;gap:14px;
                 box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class = "step-badge", "4"),
          tags$div(
            tags$h4(
              style = "margin:0;font-size:16px;font-weight:600;",
              "Added-Variable (Partial Regression) Plots"
            ),
            tags$p(
              style = "margin:2px 0 0;font-size:12px;opacity:.85;",
              "Shows unique contribution of each predictor after accounting for the others."
            )
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-bar"), " Added-Variable Plots"),
            solidHeader = TRUE, status = "danger", width = 12,
            plotOutput("maAVPlots", height = "420px")
          )
        ),

        # Lack-of-fit (SLR only)
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", "5"),
          tags$div(
            tags$h4("Lack-of-Fit Test"),
            tags$p("ANOVA pure-error lack-of-fit test (requires replicate X values and EnvStats package).")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("vial"), " Lack-of-Fit Results"),
            solidHeader = TRUE, status = "primary", width = 12,
            verbatimTextOutput("maLackFit")
          )
        )
      ),

      # ============================================================
      # BOX-COX & BOX-TIDWELL TAB
      # ============================================================
      tabItem(
        tabName = "boxtrans",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", icon("magic")),
          tags$div(
            tags$h4("Box-Cox & Box-Tidwell Transformations"),
            tags$p("Select Y and a single X variable, then choose the transformation to apply.")
          )
        ),
        fluidRow(
          sample_button_row(
            "Correcting Model Inadequacy",
            tagList(
              actionButton("loadSampleBTUtility", tagList(icon("database"), "Electric Utility"), class = "btn btn-info"),
              actionButton("loadSampleBTWind", tagList(icon("database"), "Wind Mill"), class = "btn btn-info"),
              actionButton("loadSampleBTIncome", tagList(icon("database"), "Income"), class = "btn btn-info")
            ),
            "Loads course datasets typically used with Box-Cox, Box-Tidwell, and related corrective methods."
          )
        ),
        fluidRow(
          box(
            class = "reg-card", status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(
                3,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("btYSelect")
              ),
              column(
                3,
                tags$div(class = "section-label", "Predictor Variable (X)"),
                uiOutput("btXSelect")
              ),
              column(
                3,
                tags$div(class = "section-label", "Transformation"),
                selectInput("btMethod", NULL,
                  choices = c(
                    "Box-Cox (transform Y)" = "boxcox",
                    "Box-Tidwell (transform X)" = "boxtidwell"
                  )
                )
              ),
              column(
                3,
                br(),
                actionButton("runBT", tagList(icon("play"), " Run Transformation"),
                  class = "fit-btn"
                )
              )
            )
          )
        ),

        # Step 1: Original model
        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("Original OLS Model"),
            tags$p("Scatter plot, summary, and residuals vs fitted for the untransformed data.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " Scatter + Fit"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("btOrigScatter", height = "280px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " OLS Summary"),
            solidHeader = TRUE, status = "info", width = 6,
            verbatimTextOutput("btOrigSummary")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " Residuals vs Fitted (Original)"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("btOrigResid", height = "250px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " Breusch-Pagan (Original)"),
            solidHeader = TRUE, status = "info", width = 6,
            verbatimTextOutput("btOrigBP")
          )
        ),

        # Step 2: Transformation details
        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("Optimal Transformation"),
            tags$p("Box-Cox lambda / Box-Tidwell alpha profile and optimal value.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("search"), " Transformation Profile"),
            solidHeader = TRUE, status = "success", width = 7,
            plotOutput("btLambdaPlot", height = "300px")
          ),
          box(
            class = "reg-card", title = tagList(icon("info-circle"), " Optimal Parameter"),
            solidHeader = TRUE, status = "success", width = 5,
            verbatimTextOutput("btOptimal")
          )
        ),

        # Step 3: Transformed model
        tags$div(
          class = "reg-step-banner warning",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4("Transformed Model"),
            tags$p("Scatter plot and model results using the optimal transformation.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " Scatter + Fit (Transformed)"),
            solidHeader = TRUE, status = "warning", width = 6,
            plotOutput("btTransScatter", height = "280px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " Transformed OLS Summary"),
            solidHeader = TRUE, status = "warning", width = 6,
            verbatimTextOutput("btTransSummary")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " Residuals vs Fitted (Transformed)"),
            solidHeader = TRUE, status = "warning", width = 6,
            plotOutput("btTransResid", height = "250px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " Breusch-Pagan (Transformed)"),
            solidHeader = TRUE, status = "warning", width = 6,
            verbatimTextOutput("btTransBP")
          )
        ),

        # Step 4: Side-by-side comparison
        tags$div(
          style = "background:linear-gradient(135deg,#1a237e,#283593);color:#fff;
                 border-radius:8px;padding:14px 20px;margin-bottom:18px;
                 display:flex;align-items:center;gap:14px;
                 box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class = "step-badge", "4"),
          tags$div(
            tags$h4(
              style = "margin:0;font-size:16px;font-weight:600;",
              "Side-by-Side Residual Comparison"
            ),
            tags$p(
              style = "margin:2px 0 0;font-size:12px;opacity:.85;",
              "Compare residual plots before and after transformation."
            )
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("balance-scale"), " Residual Comparison"),
            solidHeader = TRUE, status = "danger", width = 12,
            plotOutput("btComparison", height = "320px")
          )
        )
      ),

      # ============================================================
      # WEIGHTED LEAST SQUARES TAB
      # ============================================================
      tabItem(
        tabName = "wls",
        tags$div(
          class = "reg-step-banner",
          tags$div(class = "step-badge", icon("balance-scale")),
          tags$div(
            tags$h4("Weighted Least Squares (WLS)"),
            tags$p("Select Y, X, and a weight variable (or let the app estimate weights automatically).")
          )
        ),
        fluidRow(
          sample_button_row(
            "Weighted Least Squares",
            actionButton("loadSampleWLS", tagList(icon("database"), "Load Income"), class = "btn btn-info"),
            "Loads the Income dataset commonly used to demonstrate WLS."
          )
        ),
        fluidRow(
          box(
            class = "reg-card", status = "primary", solidHeader = FALSE,
            width = 12, style = "padding:18px;",
            fluidRow(
              column(
                3,
                tags$div(class = "section-label", "Response Variable (Y)"),
                uiOutput("wlsYSelect")
              ),
              column(
                3,
                tags$div(class = "section-label", "Predictor Variable (X)"),
                uiOutput("wlsXSelect")
              ),
              column(
                3,
                tags$div(class = "section-label", "Weight Column (optional)"),
                uiOutput("wlsWSelect"),
                tags$div(
                  class = "hint-text",
                  icon("info-circle"),
                  " If no weight column, weights are estimated as 1/|residual| from OLS."
                )
              ),
              column(
                3,
                br(),
                actionButton("runWLS", tagList(icon("play"), " Fit WLS Model"),
                  class = "fit-btn"
                )
              )
            )
          )
        ),

        # Step 1 OLS
        tags$div(
          class = "reg-step-banner info",
          tags$div(class = "step-badge", "1"),
          tags$div(
            tags$h4("OLS Baseline Model"),
            tags$p("Fit ordinary least squares first to diagnose heteroscedasticity.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " OLS Scatter + Fit"),
            solidHeader = TRUE, status = "info", width = 6,
            plotOutput("wlsOLSScatter", height = "280px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " OLS Summary & BP Test"),
            solidHeader = TRUE, status = "info", width = 6,
            verbatimTextOutput("wlsOLSSummary")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " OLS Residuals vs Fitted"),
            solidHeader = TRUE, status = "info", width = 12,
            plotOutput("wlsOLSResid", height = "280px")
          )
        ),

        # Step 2 WLS
        tags$div(
          class = "reg-step-banner success",
          tags$div(class = "step-badge", "2"),
          tags$div(
            tags$h4("WLS Model"),
            tags$p("Weighted least squares results and standardised residual plot.")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " WLS Scatter + Fit"),
            solidHeader = TRUE, status = "success", width = 6,
            plotOutput("wlsWLSScatter", height = "280px")
          ),
          box(
            class = "reg-card", title = tagList(icon("calculator"), " WLS Summary & BP Test"),
            solidHeader = TRUE, status = "success", width = 6,
            verbatimTextOutput("wlsWLSSummary")
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("chart-line"), " Weighted Residuals vs Weighted Fitted"),
            solidHeader = TRUE, status = "success", width = 12,
            plotOutput("wlsWLSResid", height = "280px")
          )
        ),

        # Step 3 Comparison
        tags$div(
          style = "background:linear-gradient(135deg,#1b5e20,#2e7d32);color:#fff;
                 border-radius:8px;padding:14px 20px;margin-bottom:18px;
                 display:flex;align-items:center;gap:14px;
                 box-shadow:0 2px 8px rgba(0,0,0,.15);",
          tags$div(class = "step-badge", "3"),
          tags$div(
            tags$h4(
              style = "margin:0;font-size:16px;font-weight:600;",
              "OLS vs WLS Coefficient Comparison"
            ),
            tags$p(
              style = "margin:2px 0 0;font-size:12px;opacity:.85;",
              "Side-by-side residual plots and coefficient table."
            )
          )
        ),
        fluidRow(
          box(
            class = "reg-card", title = tagList(icon("balance-scale"), " Residual Comparison"),
            solidHeader = TRUE, status = "danger", width = 8,
            plotOutput("wlsComparison", height = "300px")
          ),
          box(
            class = "reg-card", title = tagList(icon("table"), " Coefficient Comparison"),
            solidHeader = TRUE, status = "danger", width = 4,
            verbatimTextOutput("wlsCoefComp")
          )
        )
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

  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
  }

  # Remove rows containing outliers in any numeric variable (IQR rule)
  remove_outliers_iqr <- function(df, k = 1.5) {
    if (is.null(df) || nrow(df) == 0) {
      return(df)
    }
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
      return(df)
    }

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
    if (is.null(df)) {
      return(df)
    }
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
  default_data_path <- "data/P2_DeliveryTime.xlsx"
  default_data <- tryCatch(
    {
      ext0 <- tolower(tools::file_ext(default_data_path))
      df_default <- if (ext0 %in% c("xlsx", "xls")) {
        as.data.frame(read_excel(default_data_path), check.names = FALSE)
      } else {
        read.csv(default_data_path, stringsAsFactors = FALSE, check.names = FALSE)
      }
      preprocess_cars(df_default)
    },
    error = function(e) {
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
          labels = c("4 cylinders", "6 cylinders", "8 cylinders")
        ),
        engine_shape = factor(mtcars$vs,
          levels = c(0, 1),
          labels = c("V-shaped", "Straight")
        ),
        transmission_type = factor(mtcars$am,
          levels = c(0, 1),
          labels = c("Automatic", "Manual")
        ),
        forward_gears = factor(mtcars$gear),
        carburetors_count = factor(mtcars$carb),
        mpg_category = cut(mtcars$mpg,
          breaks = quantile(mtcars$mpg, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
          include.lowest = TRUE,
          labels = c("Low", "Medium", "High")
        )
      )
      fallback
    }
  )

  default_data <- apply_cleaning_pipeline(default_data, missing_mode = "keep", outlier_mode = "remove")

  # Reactive data storage
  data <- reactiveValues(
    raw_base = default_data,
    raw = default_data,
    analyzed = TRUE,
    numeric_vars = names(default_data)[sapply(default_data, is.numeric)],
    categorical_vars = names(default_data)[!sapply(default_data, is.numeric)]
  )

  course_dataset_path <- function(filename) {
    candidates <- c(
      file.path("data", filename),
      file.path("Codes&Data", filename),
      file.path("..", "Codes&Data", filename)
    )
    found <- candidates[file.exists(candidates)]
    if (length(found) == 0) NULL else found[1]
  }

  read_course_dataset <- function(filename) {
    dataset_path <- course_dataset_path(filename)
    if (is.null(dataset_path)) {
      stop(sprintf("Could not find %s in the expected course folders.", filename), call. = FALSE)
    }
    ext <- tolower(tools::file_ext(dataset_path))
    if (ext %in% c("xlsx", "xls")) {
      as.data.frame(readxl::read_excel(dataset_path), check.names = FALSE)
    } else if (ext == "csv") {
      read.csv(dataset_path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      stop(sprintf("Unsupported dataset format for %s.", filename), call. = FALSE)
    }
  }

  load_course_dataset <- function(filename, selected_tab, success_message, post_load = NULL) {
    dataset <- read_course_dataset(filename)
    data$raw_base <- dataset
    data$raw <- apply_cleaning_pipeline(dataset,
      missing_mode = input$cleanMissing %||% "keep",
      outlier_mode = input$cleanOutliers %||% "remove",
      log_vars = input$cleanLogVars %||% NULL
    )
    refresh_var_types(data$raw)
    data$analyzed <- TRUE
    if (!is.null(post_load)) post_load()
    updateTabItems(session, "tabs", selected = selected_tab)
    showNotification(success_message, type = "message")
  }

  # File upload handler
  observeEvent(input$datafile, {
    req(input$datafile)

    tryCatch(
      {
        ext <- tools::file_ext(input$datafile$name)

        if (ext == "csv") {
          uploaded <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          uploaded <- as.data.frame(read_excel(input$datafile$datapath), check.names = FALSE)
        }

        uploaded <- preprocess_cars(uploaded)
        data$raw_base <- uploaded
        before_n <- nrow(uploaded)
        data$raw <- apply_cleaning_pipeline(uploaded,
          missing_mode = input$cleanMissing %||% "keep",
          outlier_mode = input$cleanOutliers %||% "remove",
          log_vars = input$cleanLogVars %||% NULL
        )
        removed_n <- before_n - nrow(data$raw)
        refresh_var_types(data$raw)

        data$analyzed <- TRUE
        updateTabItems(session, "tabs", selected = "upload")

        showNotification(paste("Data uploaded successfully! Variable types detected. Outliers removed:", removed_n),
          type = "message", duration = 3
        )
      },
      error = function(e) {
        showNotification(paste("Error reading file:", e$message),
          type = "error", duration = 5
        )
      }
    )
  })

  # Display detected variables
  output$numericVars <- renderPrint({
    req(data$numeric_vars)
    cat(paste(data$numeric_vars, collapse = "\n"))
  })

  output$categoricalVars <- renderPrint({
    req(data$categorical_vars)
    if (length(data$categorical_vars) > 0) {
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
    infoBox("Rows", ifelse(is.null(data$raw), 0, format(nrow(data$raw), big.mark = ",")),
      icon = icon("table"), color = "blue", fill = TRUE
    )
  })
  output$kpiCols <- renderInfoBox({
    infoBox("Columns", ifelse(is.null(data$raw), 0, ncol(data$raw)),
      icon = icon("columns"), color = "teal", fill = TRUE
    )
  })
  output$kpiMissing <- renderInfoBox({
    miss <- if (is.null(data$raw)) 0 else sum(is.na(data$raw))
    miss_pct <- if (is.null(data$raw)) 0 else round(100 * miss / max(1, nrow(data$raw) * ncol(data$raw)), 2)
    infoBox("Missing %", paste0(miss_pct, "%"),
      subtitle = paste("Total:", format(miss, big.mark = ",")),
      icon = icon("exclamation-triangle"), color = "yellow", fill = TRUE
    )
  })
  output$kpiTypes <- renderInfoBox({
    infoBox("Numeric | Categorical",
      paste(length(data$numeric_vars), "|", length(data$categorical_vars)),
      icon = icon("layer-group"), color = "green", fill = TRUE
    )
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
      )
    )
  })

  # Data summary (formatted tables instead of raw R output)
  output$dataSummaryNumeric <- DT::renderDT({
    req(data$raw)
    num_vars <- names(data$raw)[sapply(data$raw, is.numeric)]
    if (length(num_vars) == 0) {
      return(DT::datatable(data.frame(Message = "No numeric variables."),
        options = list(dom = "t"), rownames = FALSE
      ))
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
      rownames = FALSE
    )
  })

  output$dataSummaryCategorical <- DT::renderDT({
    req(data$raw)
    cat_vars <- names(data$raw)[!sapply(data$raw, is.numeric)]
    if (length(cat_vars) == 0) {
      return(DT::datatable(data.frame(Message = "No categorical variables."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    tbl <- data.frame(
      Variable = cat_vars,
      Unique_Levels = sapply(cat_vars, function(v) length(unique(data$raw[[v]]))),
      Most_Frequent = sapply(cat_vars, function(v) {
        x <- as.character(data$raw[[v]])
        x <- x[!is.na(x) & trimws(x) != ""]
        if (length(x) == 0) {
          return("N/A")
        }
        names(sort(table(x), decreasing = TRUE))[1]
      }),
      Missing = sapply(cat_vars, function(v) sum(is.na(data$raw[[v]]) | trimws(as.character(data$raw[[v]])) == "")),
      stringsAsFactors = FALSE
    )
    DT::datatable(tbl,
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Analyze button
  observeEvent(input$analyzeBtn, {
    req(data$raw)
    data$analyzed <- TRUE
    updateTabItems(session, "tabs", selected = "upload")
    showNotification("Data ready for analysis!", type = "message", duration = 3)
  })

  # ===== SINGLE VARIABLE ANALYSIS =====

  output$singleVarSelect <- renderUI({
    req(data$raw)

    if (input$singleVarType == "Numerical") {
      selectInput("singleVar", "Select Variable:", choices = data$numeric_vars)
    } else {
      selectInput("singleVar", "Select Variable:", choices = data$categorical_vars)
    }
  })

  observeEvent(input$runSingleAnalysis, {
    req(data$raw, input$singleVar)

    if (input$singleVarType == "Numerical") {
      var_data <- data$raw[[input$singleVar]]

      output$singleStats <- renderPrint({
        cat("Descriptive Statistics for", input$singleVar, "\n")
        cat(strrep("=", 50), "\n\n")
        cat("Central Tendency:\n")
        cat("  Mean   :", round(mean(var_data, na.rm = TRUE), 3), "\n")
        cat("  Median :", round(median(var_data, na.rm = TRUE), 3), "\n")
        cat("  Mode   :", names(sort(table(var_data), decreasing = TRUE)[1]), "\n\n")

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
          border = "darkblue"
        )

        boxplot(var_data,
          main = paste("Boxplot of", input$singleVar),
          xlab = input$singleVar,
          col = "lightgreen",
          horizontal = TRUE
        )
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
          las = 2
        )

        pie(freq_table,
          main = paste("Pie Chart of", input$singleVar),
          col = brewer.pal(min(length(freq_table), 8), "Pastel1")
        )
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
      var_id <- paste0("multiVar", i)

      type_val <- as.character(input[[type_id]] %||% "Numerical")
      choices <- if (identical(type_val, "Categorical")) data$categorical_vars else data$numeric_vars
      choices <- as.character(choices %||% character(0))
      selected_val <- input[[var_id]]
      if (is.null(selected_val) || length(selected_val) == 0 || !(selected_val %in% choices)) {
        selected_val <- if (length(choices) > 0) choices[1] else NULL
      }

      fluidRow(
        column(2, tags$strong(paste("Variable", i))),
        column(
          4,
          selectInput(type_id, NULL,
            choices = c("Numerical", "Categorical"),
            selected = type_val, width = "100%"
          )
        ),
        column(
          6,
          selectInput(var_id, NULL,
            choices = choices,
            selected = selected_val, width = "100%"
          )
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
      df_sel <- data$raw[, unique(selected_vars), drop = FALSE]
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
              tryCatch(
                {
                  ch <- chisq.test(t_ij)
                  cat(
                    "  Chi-square:", round(ch$statistic, 4),
                    " df:", ch$parameter,
                    " p-value:", format.pval(ch$p.value, digits = 4), "\n"
                  )
                },
                error = function(e) {
                  cat("  Test could not be computed:", e$message, "\n")
                }
              )
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
            pch = 19, col = rgb(0.2, 0.5, 0.8, 0.5), cex = 0.7
          )
        } else if (length(num_vars) == 2) {
          x <- df_sel[[num_vars[1]]]
          y <- df_sel[[num_vars[2]]]
          plot(x, y,
            main = paste(num_vars[1], "vs", num_vars[2]),
            xlab = num_vars[1], ylab = num_vars[2],
            pch = 19, col = rgb(0.2, 0.5, 0.8, 0.5)
          )
          abline(lm(y ~ x), col = "red", lwd = 2)
        } else if (length(num_vars) == 1 && length(cat_vars) >= 1) {
          g <- as.factor(df_sel[[cat_vars[1]]])
          boxplot(df_sel[[num_vars[1]]] ~ g,
            main = paste(num_vars[1], "by", cat_vars[1]),
            xlab = cat_vars[1], ylab = num_vars[1],
            col = brewer.pal(min(length(levels(g)), 8), "Set2"),
            las = 2
          )
        } else if (length(cat_vars) >= 2) {
          t12 <- table(df_sel[[cat_vars[1]]], df_sel[[cat_vars[2]]])
          barplot(t12,
            main = paste(cat_vars[1], "vs", cat_vars[2]),
            xlab = cat_vars[2], ylab = "Frequency",
            col = brewer.pal(min(nrow(t12), 8), "Set3"),
            beside = TRUE, legend.text = TRUE
          )
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
            plotly::plot_ly(
              x = x, y = y, type = "scatter", mode = "markers",
              marker = list(color = "#3b82f6", opacity = 0.65)
            ) |>
              plotly::layout(
                title = paste(num_vars[1], "vs", num_vars[2]),
                xaxis = list(title = num_vars[1]),
                yaxis = list(title = num_vars[2])
              )
          } else if (length(num_vars) == 1 && length(cat_vars) >= 1) {
            plotly::plot_ly(
              x = as.factor(df_sel[[cat_vars[1]]]),
              y = df_sel[[num_vars[1]]], type = "box"
            )
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
      selected = data$numeric_vars[1:min(3, length(data$numeric_vars))]
    )
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
          col = COL2("RdYlBu", n = 20),
          tl.cex = 0.9,
          tl.col = "black",
          number.cex = 0.8
        )
      })

      output$corrPairs <- renderPlot({
        pairs(selected_data,
          main = "Scatter Plot Matrix",
          pch = 19,
          col = rgb(0.2, 0.5, 0.8, 0.4),
          cex = 0.6
        )
      })
      incProgress(0.6)
    })
  })

  # ===== CATEGORICAL ANALYSIS =====

  output$catVarSelect <- renderUI({
    req(data$categorical_vars)
    selectInput("catVar", "Select Categorical Variable:",
      choices = data$categorical_vars
    )
  })

  observeEvent(input$runCatAnalysis, {
    req(data$raw, input$catVar)

    cat_data <- data$raw[[input$catVar]]
    freq_table <- table(cat_data)
    prop_table <- prop.table(freq_table)

    output$catFrequency <- renderTable(
      {
        data.frame(
          Category = names(freq_table),
          Frequency = as.numeric(freq_table)
        )
      },
      rownames = FALSE
    )

    output$catProportion <- renderTable(
      {
        data.frame(
          Category = names(prop_table),
          Proportion = round(as.numeric(prop_table), 4),
          Percentage = paste0(round(as.numeric(prop_table) * 100, 2), "%")
        )
      },
      rownames = FALSE
    )

    output$catPlot <- renderPlot({
      if (input$catPlotType == "Bar Plot") {
        par(mfrow = c(1, 1))
        barplot(freq_table,
          main = paste("Bar Plot of", input$catVar),
          xlab = input$catVar,
          ylab = "Frequency",
          col = brewer.pal(min(length(freq_table), 8), "Set2"),
          las = 2
        )
      } else if (input$catPlotType == "Pie Chart") {
        par(mfrow = c(1, 1))
        pie(freq_table,
          main = paste("Pie Chart of", input$catVar),
          col = brewer.pal(min(length(freq_table), 8), "Pastel1"),
          labels = paste(names(freq_table), "\n(",
            round(prop_table * 100, 1), "%)",
            sep = ""
          )
        )
      } else {
        par(mfrow = c(2, 1), mar = c(5, 4, 3, 2))
        barplot(freq_table,
          main = paste("Bar Plot of", input$catVar),
          xlab = input$catVar,
          ylab = "Frequency",
          col = brewer.pal(min(length(freq_table), 8), "Set2"),
          las = 2
        )
        pie(freq_table,
          main = paste("Pie Chart of", input$catVar),
          col = brewer.pal(min(length(freq_table), 8), "Pastel1")
        )
      }
    })
    if (HAS_PLOTLY) {
      output$catPlotly <- plotly::renderPlotly({
        dfp <- data.frame(Category = names(freq_table), Frequency = as.numeric(freq_table))
        if (input$catPlotType == "Pie Chart") {
          plotly::plot_ly(dfp, labels = ~Category, values = ~Frequency, type = "pie")
        } else {
          plotly::plot_ly(dfp,
            x = ~Category, y = ~Frequency, type = "bar",
            marker = list(color = "#60a5fa")
          )
        }
      })
    }
  })

  # ===== CROSS-TABULATION =====

  output$crosstabVar1Select <- renderUI({
    req(data$categorical_vars)
    selectInput("crosstabVar1", "Row Variable:",
      choices = data$categorical_vars
    )
  })

  output$crosstabVar2Select <- renderUI({
    req(data$categorical_vars)
    selectInput("crosstabVar2", "Column Variable:",
      choices = data$categorical_vars
    )
  })

  observeEvent(input$runCrosstabAnalysis, {
    req(data$raw, input$crosstabVar1, input$crosstabVar2)

    var1_data <- data$raw[[input$crosstabVar1]]
    var2_data <- data$raw[[input$crosstabVar2]]

    cont_table <- table(var1_data, var2_data)
    joint_prop <- prop.table(cont_table)

    output$crosstabTable <- renderTable(
      {
        as.data.frame.matrix(cont_table)
      },
      rownames = TRUE
    )

    output$crosstabProp <- renderTable(
      {
        as.data.frame.matrix(round(joint_prop, 4))
      },
      rownames = TRUE
    )

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
      if (input$crosstabPlotType == "Stacked Bar") {
        barplot(cont_table,
          main = paste(input$crosstabVar1, "vs", input$crosstabVar2),
          xlab = input$crosstabVar2,
          col = brewer.pal(min(nrow(cont_table), 8), "Set3"),
          legend.text = TRUE
        )
      } else if (input$crosstabPlotType == "Grouped Bar") {
        barplot(cont_table,
          main = paste(input$crosstabVar1, "vs", input$crosstabVar2),
          xlab = input$crosstabVar2,
          col = brewer.pal(min(nrow(cont_table), 8), "Set3"),
          beside = TRUE,
          legend.text = TRUE
        )
      } else {
        mosaicplot(cont_table,
          main = paste(input$crosstabVar1, "vs", input$crosstabVar2),
          color = brewer.pal(min(ncol(cont_table), 8), "Set2")
        )
      }
    })
  })

  # ===== MULTI-CATEGORICAL ANALYSIS (3+ VARIABLES) =====

  # Variable selectors for multi-categorical
  output$multiCatVar1Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar1", "Variable 1:",
      choices = c("Select...", data$categorical_vars),
      selected = "Select..."
    )
  })

  output$multiCatVar2Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar2", "Variable 2:",
      choices = c("Select...", data$categorical_vars),
      selected = "Select..."
    )
  })

  output$multiCatVar3Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar3", "Variable 3:",
      choices = c("Select...", data$categorical_vars),
      selected = "Select..."
    )
  })

  output$multiCatVar4Select <- renderUI({
    req(data$categorical_vars)
    selectInput("multiCatVar4", "Variable 4 (Optional):",
      choices = c("None", data$categorical_vars),
      selected = "None"
    )
  })

  # Multi-categorical analysis
  observeEvent(input$runMultiCatAnalysis, {
    req(data$raw, input$multiCatVar1, input$multiCatVar2, input$multiCatVar3)

    # Collect selected variables (excluding "Select..." and "None")
    selected_vars <- c(input$multiCatVar1, input$multiCatVar2, input$multiCatVar3)
    if (!is.null(input$multiCatVar4) && input$multiCatVar4 != "None") {
      selected_vars <- c(selected_vars, input$multiCatVar4)
    }
    selected_vars <- selected_vars[selected_vars != "Select..."]

    if (length(selected_vars) < 3) {
      showNotification("Please select at least 3 categorical variables!",
        type = "error", duration = 5
      )
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
      if (length(selected_vars) == 3) {
        # Create formula
        formula_str <- paste(selected_vars[3], "~", selected_vars[1], "+", selected_vars[2])
        test_table <- xtabs(as.formula(formula_str), data = multi_data)

        tryCatch(
          {
            chi_test <- chisq.test(test_table)
            print(chi_test)

            cat("\n\nInterpretation:\n")
            if (chi_test$p.value < 0.05) {
              cat("Result: SIGNIFICANT association (p < 0.05)\n")
              cat("Conclusion: The variables are NOT independent.\n")
            } else {
              cat("Result: NO significant association (p >= 0.05)\n")
              cat("Conclusion: The variables appear to be independent.\n")
            }
          },
          error = function(e) {
            cat("Chi-square test could not be computed.\n")
            cat("This may be due to small expected frequencies.\n")
            cat("Error:", e$message, "\n")
          }
        )
      } else {
        cat("Note: For 4+ variables, pairwise testing is performed.\n\n")

        for (i in 1:(length(selected_vars) - 1)) {
          for (j in (i + 1):length(selected_vars)) {
            cat("\nTesting:", selected_vars[i], "vs", selected_vars[j], "\n")
            cat(strrep("-", 40), "\n")
            test_table <- table(
              multi_data[[selected_vars[i]]],
              multi_data[[selected_vars[j]]]
            )
            tryCatch(
              {
                chi_test <- chisq.test(test_table)
                cat(
                  "Chi-square =", round(chi_test$statistic, 3),
                  ", df =", chi_test$parameter,
                  ", p-value =", format.pval(chi_test$p.value, digits = 4), "\n"
                )
              },
              error = function(e) {
                cat("Test failed: small expected frequencies\n")
              }
            )
          }
        }
      }
    })

    # Association measures
    output$multiCatAssociation <- renderPrint({
      cat("Association Strength Measures\n")
      cat(strrep("=", 40), "\n\n")

      # Calculate Cramér's V for all pairs
      for (i in 1:(length(selected_vars) - 1)) {
        for (j in (i + 1):length(selected_vars)) {
          test_table <- table(
            multi_data[[selected_vars[i]]],
            multi_data[[selected_vars[j]]]
          )

          tryCatch(
            {
              chi_test <- chisq.test(test_table)
              n <- sum(test_table)
              min_dim <- min(dim(test_table)) - 1
              cramers_v <- sqrt(chi_test$statistic / (n * min_dim))

              cat(selected_vars[i], "vs", selected_vars[j], ":\n")
              cat("  Cramér's V =", round(cramers_v, 4))

              if (cramers_v < 0.1) {
                cat(" (Negligible)\n")
              } else if (cramers_v < 0.3) {
                cat(" (Weak)\n")
              } else if (cramers_v < 0.5) {
                cat(" (Moderate)\n")
              } else {
                cat(" (Strong)\n")
              }
            },
            error = function(e) {
              cat(selected_vars[i], "vs", selected_vars[j], ": Unable to compute\n")
            }
          )
        }
      }
    })

    # Visualization options
    output$multiCatPlotOptions <- renderUI({
      if (input$multiCatPlotType == "Heatmap") {
        selectInput("heatmapVars", "Select 2 variables for heatmap:",
          choices = selected_vars,
          multiple = TRUE,
          selected = selected_vars[1:2]
        )
      } else {
        NULL
      }
    })

    # Multi-way visualization
    output$multiCatPlot <- renderPlot({
      req(multi_table)

      if (input$multiCatPlotType == "Grouped Bar Chart") {
        # Grouped bar chart
        if (length(selected_vars) == 3) {
          counts <- as.data.frame(multi_table)
          names(counts)[length(names(counts))] <- "Freq"

          par(mar = c(8, 4, 3, 2))

          # Create interaction variable
          interaction_var <- interaction(
            counts[[selected_vars[2]]],
            counts[[selected_vars[3]]]
          )

          barplot(counts$Freq,
            main = paste("Multi-Way Distribution"),
            xlab = "",
            ylab = "Frequency",
            col = rep(brewer.pal(min(8, length(unique(counts[[selected_vars[2]]]))), "Set3"),
              length.out = nrow(counts)
            ),
            las = 2,
            names.arg = paste(counts[[selected_vars[1]]],
              counts[[selected_vars[2]]],
              counts[[selected_vars[3]]],
              sep = "\n"
            )
          )
        } else {
          plot.new()
          text(0.5, 0.5, "Grouped bar chart works best with 3 variables.\nTry another visualization type.", cex = 1.5)
        }
      } else if (input$multiCatPlotType == "Faceted Bar Chart") {
        # Faceted visualization
        if (length(selected_vars) >= 3) {
          par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))

          # Create different facets
          unique_vals <- unique(multi_data[[selected_vars[1]]])

          for (val in unique_vals[1:min(4, length(unique_vals))]) {
            subset_data <- multi_data[multi_data[[selected_vars[1]]] == val, ]
            subset_table <- table(
              subset_data[[selected_vars[2]]],
              subset_data[[selected_vars[3]]]
            )

            barplot(subset_table,
              main = paste(selected_vars[1], "=", val),
              xlab = selected_vars[3],
              ylab = "Frequency",
              col = brewer.pal(min(nrow(subset_table), 8), "Pastel1"),
              beside = TRUE,
              legend.text = TRUE,
              args.legend = list(x = "topright", cex = 0.7)
            )
          }
        }
      } else if (input$multiCatPlotType == "Mosaic Plot") {
        # Mosaic plot
        if (length(selected_vars) == 3) {
          formula_str <- paste("~", paste(selected_vars, collapse = " + "))
          mosaicplot(as.formula(formula_str),
            data = multi_data,
            main = "Multi-Way Mosaic Plot",
            color = brewer.pal(8, "Set2")
          )
        } else {
          # For 4 variables, show first 3
          formula_str <- paste("~", paste(selected_vars[1:3], collapse = " + "))
          mosaicplot(as.formula(formula_str),
            data = multi_data,
            main = paste("Mosaic Plot:", paste(selected_vars[1:3], collapse = ", ")),
            color = brewer.pal(8, "Set2")
          )
        }
      } else if (input$multiCatPlotType == "Heatmap") {
        # Heatmap for frequency counts
        if (!is.null(input$heatmapVars) && length(input$heatmapVars) == 2) {
          heat_table <- table(
            multi_data[[input$heatmapVars[1]]],
            multi_data[[input$heatmapVars[2]]]
          )

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
            axes = FALSE
          )

          axis(1, at = 1:ncol(heat_matrix), labels = colnames(heat_matrix), las = 2)
          axis(2, at = 1:nrow(heat_matrix), labels = rownames(heat_matrix), las = 2)

          # Add frequency values
          for (i in 1:nrow(heat_matrix)) {
            for (j in 1:ncol(heat_matrix)) {
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
        paste(names(most_common)[1:(length(names(most_common)) - 1)],
          "=", most_common[1:(length(most_common) - 1)],
          collapse = ", "
        ),
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
        "<ul>",
        paste(insights, collapse = "\n"),
        "</ul>",
        "</div>",
        "<br>",
        '<div style="background-color: #E8F5E9; padding: 15px; border-radius: 5px; border-left: 4px solid #4CAF50;">',
        recommendations,
        "</div>"
      ))
    })
  })

  # ===== HYPOTHESIS TESTS =====

  output$testVarInputs <- renderUI({
    req(data$raw)

    if (input$testType %in% c("One-Sample T-Test", "One-Sample Wilcoxon Test", "Normality Test (Shapiro-Wilk)")) {
      tagList(
        selectInput("testVar1", "Select Variable:", choices = data$numeric_vars),
        numericInput("testMu", "Hypothesized Mean (μ₀):", value = 0)
      )
    } else if (input$testType == "Two-Sample T-Test") {
      tagList(
        selectInput("testVar1", "Group Variable (Categorical):",
          choices = data$categorical_vars
        ),
        selectInput("testVar2", "Measurement Variable (Numerical):",
          choices = data$numeric_vars
        )
      )
    } else if (input$testType == "Paired T-Test") {
      tagList(
        selectInput("testVar1", "Variable 1:", choices = data$numeric_vars),
        selectInput("testVar2", "Variable 2:", choices = data$numeric_vars)
      )
    } else if (input$testType == "One-Way ANOVA (F-Test)") {
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

    fmt_p <- function(p) {
      if (is.na(p)) return("NA")
      if (p < 0.001) return("< 0.001")
      sprintf("%.4f", p)
    }

    decision_text <- function(p, alpha = 0.05) {
      if (is.na(p)) return("Decision: Not available")
      if (p < alpha) {
        "Decision: Reject H0 at alpha = 0.05"
      } else {
        "Decision: Fail to reject H0 at alpha = 0.05"
      }
    }

    if (input$testType == "One-Sample T-Test") {
      test_data <- data$raw[[input$testVar1]]

      output$testResults <- renderText({
        t_test <- t.test(test_data, mu = input$testMu)
        paste(
          "One-Sample T-Test",
          "",
          sprintf("Variable: %s", input$testVar1),
          sprintf("Sample mean: %.4f", mean(test_data, na.rm = TRUE)),
          sprintf("Hypothesized mean (mu0): %.4f", input$testMu),
          sprintf("t-statistic: %.4f", unname(t_test$statistic)),
          sprintf("Degrees of freedom: %.2f", unname(t_test$parameter)),
          sprintf("P-value: %s", fmt_p(t_test$p.value)),
          sprintf(
            "95%% CI: [%.4f, %.4f]",
            t_test$conf.int[1],
            t_test$conf.int[2]
          ),
          decision_text(t_test$p.value),
          sep = "\n"
        )
      })

      output$testPlot <- renderPlot({
        boxplot(test_data,
          main = "Distribution",
          ylab = input$testVar1,
          col = "lightblue"
        )
        abline(h = input$testMu, col = "red", lwd = 2, lty = 2)
      })
    } else if (input$testType == "Normality Test (Shapiro-Wilk)") {
      test_data <- data$raw[[input$testVar1]]
      sample_size <- min(5000, length(test_data))

      output$testResults <- renderText({
        shapiro_test <- shapiro.test(sample(test_data, sample_size))
        paste(
          "Shapiro-Wilk Normality Test",
          "",
          sprintf("Variable: %s", input$testVar1),
          sprintf("Sample size used: %d", sample_size),
          sprintf("W-statistic: %.4f", unname(shapiro_test$statistic)),
          sprintf("P-value: %s", fmt_p(shapiro_test$p.value)),
          if (shapiro_test$p.value < 0.05) {
            "Conclusion: Evidence suggests the data are not normally distributed."
          } else {
            "Conclusion: No strong evidence against normality."
          },
          sep = "\n"
        )
      })

      output$testPlot <- renderPlot({
        qqnorm(test_data, main = "Q-Q Plot")
        qqline(test_data, col = "red", lwd = 2)
      })
    } else if (input$testType == "Two-Sample T-Test") {
      req(input$testVar2)
      group_var <- data$raw[[input$testVar1]]
      measure_var <- data$raw[[input$testVar2]]

      output$testResults <- renderText({
        t_test <- t.test(measure_var ~ group_var)
        grp_levels <- unique(stats::na.omit(as.character(group_var)))
        paste(
          "Two-Sample T-Test",
          "",
          sprintf("Grouping variable: %s", input$testVar1),
          sprintf("Measurement variable: %s", input$testVar2),
          sprintf("Groups compared: %s", paste(grp_levels, collapse = " vs ")),
          sprintf("t-statistic: %.4f", unname(t_test$statistic)),
          sprintf("Degrees of freedom: %.2f", unname(t_test$parameter)),
          sprintf("P-value: %s", fmt_p(t_test$p.value)),
          sprintf(
            "95%% CI of mean difference: [%.4f, %.4f]",
            t_test$conf.int[1],
            t_test$conf.int[2]
          ),
          decision_text(t_test$p.value),
          sep = "\n"
        )
      })

      output$testPlot <- renderPlot({
        boxplot(measure_var ~ group_var,
          main = "Group Comparison",
          xlab = input$testVar1,
          ylab = input$testVar2,
          col = c("lightblue", "lightgreen")
        )
      })
    } else if (input$testType == "One-Way ANOVA (F-Test)") {
      req(input$testVar2)
      group_var <- as.factor(data$raw[[input$testVar1]])
      measure_var <- data$raw[[input$testVar2]]
      df_test <- data.frame(g = group_var, y = measure_var)
      df_test <- df_test[complete.cases(df_test), , drop = FALSE]

      output$testResults <- renderText({
        if (nlevels(df_test$g) < 2) {
          return("ANOVA requires at least 2 groups.")
        }
        aov_fit <- aov(y ~ g, data = df_test)
        anova_tbl <- summary(aov_fit)[[1]]
        f_stat <- anova_tbl$`F value`[1]
        p_val <- anova_tbl$`Pr(>F)`[1]
        df_between <- anova_tbl$Df[1]
        df_within <- anova_tbl$Df[2]
        paste(
          "One-Way ANOVA (F-Test)",
          "",
          sprintf("Grouping variable: %s", input$testVar1),
          sprintf("Measurement variable: %s", input$testVar2),
          sprintf("Number of groups: %d", nlevels(df_test$g)),
          sprintf("F-statistic: %.4f", f_stat),
          sprintf("Degrees of freedom: %d, %d", df_between, df_within),
          sprintf("P-value: %s", fmt_p(p_val)),
          decision_text(p_val),
          sep = "\n"
        )
      })

      output$testPlot <- renderPlot({
        boxplot(y ~ g,
          data = df_test,
          main = "One-Way ANOVA: Group Comparison",
          xlab = input$testVar1,
          ylab = input$testVar2,
          col = brewer.pal(min(8, nlevels(df_test$g)), "Set2"),
          las = 2
        )
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
        dom = "ftp"
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Type",
        backgroundColor = styleEqual(
          c("Numerical", "Categorical"),
          c("#e3f2fd", "#fff3e0")
        )
      )
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
        " (", round(100 * type_counts / sum(type_counts), 1), "%)",
        sep = ""
      ),
      col = colors,
      main = "Variable Types",
      cex = 1.2,
      border = "white",
      lwd = 2
    )
  })

  # Numerical Variables Summary Table
  output$numericSummaryTable <- renderDT({
    req(data$raw, data$numeric_vars)

    if (length(data$numeric_vars) > 0) {
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
          dom = "ftp"
        ),
        rownames = FALSE
      ) %>%
        formatStyle(columns = 1:7, fontSize = "14px")
    }
  })

  # Missing Values Plot (enhanced)
  output$missingPlot <- renderPlot({
    req(data$raw)

    missing_counts <- colSums(is.na(data$raw))
    missing_pct <- 100 * missing_counts / nrow(data$raw)

    if (sum(missing_counts) > 0) {
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
        ylim = c(0, max(missing_pct_filtered) * 1.2)
      )

      # Add percentage labels on top
      text(
        x = seq_along(missing_vars) * 1.2 - 0.5,
        y = missing_pct_filtered + max(missing_pct_filtered) * 0.05,
        labels = paste0(round(missing_pct_filtered, 1), "%"),
        cex = 0.8
      )
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
    cat(sprintf("Completeness: %.2f%%\n", 100 * (1 - total_missing / total_cells)))

    if (total_missing > 0) {
      cat("\nVariables with missing data:\n")
      cat(strrep("-", 40), "\n")
      missing_vars <- missing_counts[missing_counts > 0]
      for (var in names(missing_vars)) {
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
        ifelse(completeness >= 80, "#FF9800", "#F44336")
      )
    )

    barplot(completeness,
      main = "Data Completeness by Variable",
      ylab = "Completeness (%)",
      col = colors,
      las = 2,
      border = NA,
      ylim = c(0, 105)
    )

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
    if (completeness_pct >= 99) {
      rating <- "Excellent"
      color <- "#4CAF50"
      icon_name <- "check-circle"
    } else if (completeness_pct >= 95) {
      rating <- "Very Good"
      color <- "#8BC34A"
      icon_name <- "check"
    } else if (completeness_pct >= 90) {
      rating <- "Good"
      color <- "#FDD835"
      icon_name <- "thumbs-up"
    } else if (completeness_pct >= 80) {
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
      "</h2>",
      '<h1 style="font-size: 48px; margin: 10px 0; color: ', color, ';">',
      round(completeness_pct, 1), "%",
      "</h1>",
      '<p style="font-size: 16px; color: #666;">Data Completeness Score</p>',
      "</div>"
    ))
  })

  # Distribution Select Input
  output$distributionSelect <- renderUI({
    req(data$numeric_vars)

    if (length(data$numeric_vars) > 0) {
      selectInput("distVar", "Select variable to visualize:",
        choices = data$numeric_vars,
        selected = data$numeric_vars[1]
      )
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
      border = "white"
    )

    lines(density(var_data, na.rm = TRUE), col = "#E91E63", lwd = 3)

    # Boxplot with statistics
    boxplot(var_data,
      main = paste("Boxplot of", input$distVar),
      ylab = input$distVar,
      col = rgb(0.5, 0.8, 0.5, 0.6),
      border = "#388E3C",
      horizontal = FALSE
    )

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
    if (n_rows < 30) {
      insights <- c(insights, paste0(
        "<li><strong>Small Sample:</strong> Your dataset has only ", n_rows,
        " rows. Consider collecting more data for robust statistical analysis.</li>"
      ))
    } else if (n_rows > 10000) {
      insights <- c(insights, paste0(
        "<li><strong>Large Dataset:</strong> With ", format(n_rows, big.mark = ","),
        " rows, you have excellent statistical power for analysis.</li>"
      ))
    }

    # Missing values insight
    missing_counts <- colSums(is.na(data$raw))
    if (sum(missing_counts) == 0) {
      insights <- c(
        insights,
        "<li><strong>Complete Data:</strong> Excellent! No missing values detected.</li>"
      )
    } else if (sum(missing_counts) > nrow(data$raw) * 0.1) {
      insights <- c(
        insights,
        "<li><strong>Missing Data:</strong> Significant missing values detected. Consider imputation methods.</li>"
      )
    }

    # Variable balance
    if (length(data$numeric_vars) > 0 && length(data$categorical_vars) > 0) {
      insights <- c(
        insights,
        "<li><strong>Mixed Data Types:</strong> Your dataset contains both numerical and categorical variables, enabling diverse analyses.</li>"
      )
    } else if (length(data$numeric_vars) == 0) {
      insights <- c(
        insights,
        "<li><strong>All Categorical:</strong> Your dataset contains only categorical variables. Focus on frequency analysis and chi-square tests.</li>"
      )
    } else if (length(data$categorical_vars) == 0) {
      insights <- c(
        insights,
        "<li><strong>All Numerical:</strong> Your dataset is entirely numerical. Perfect for correlation and regression analysis.</li>"
      )
    }

    # Recommended analyses
    recommendations <- "<h4><i class='fa fa-tasks'></i> Recommended Next Steps:</h4><ul>"

    if (length(data$numeric_vars) >= 2) {
      recommendations <- paste0(
        recommendations,
        "<li>Explore correlations in the <strong>Correlation Matrix</strong> tab</li>"
      )
    }

    if (length(data$categorical_vars) >= 2) {
      recommendations <- paste0(
        recommendations,
        "<li>Analyze relationships in the <strong>Cross-Tabulation</strong> tab</li>"
      )
    }

    if (length(data$numeric_vars) > 0 && length(data$categorical_vars) > 0) {
      recommendations <- paste0(
        recommendations,
        "<li>Compare groups in the <strong>Two Variables</strong> tab</li>"
      )
    }

    recommendations <- paste0(recommendations, "</ul>")

    HTML(paste0(
      '<div style="background-color: #E3F2FD; padding: 15px; border-radius: 5px; border-left: 4px solid #2196F3;">',
      '<h4><i class="fa fa-lightbulb"></i> Data Insights:</h4>',
      "<ul>",
      paste(insights, collapse = "\n"),
      "</ul>",
      "</div>",
      "<br>",
      '<div style="background-color: #F3E5F5; padding: 15px; border-radius: 5px; border-left: 4px solid #9C27B0;">',
      recommendations,
      "</div>"
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
            "[Numerical]", "[Categorical]"
          )
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
        pch = 19, col = rgb(0.2, 0.5, 0.8, 0.6), cex = 1.2
      )
      abline(model, col = "red", lwd = 2)
      legend("topleft",
        legend = paste0(
          "y = ", round(coef(model)[1], 4),
          ifelse(length(coef(model)) > 1,
            paste0(" + ", round(coef(model)[2], 4), " x"), ""
          )
        ),
        col = "red", lwd = 2, bty = "n"
      )
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
        warning = function(w) cat(conditionMessage(w), "\n")
      )

      cat("\nKendall (non-parametric):\n")
      tryCatch(print(cor.test(y, x, method = "kendall")),
        warning = function(w) cat(conditionMessage(w), "\n")
      )
    })
  })

  # SLR Prediction
  observeEvent(input$runSLRPredict, {
    req(slr_model(), input$slrNewX)

    tryCatch(
      {
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
            rownames = FALSE
          )
        })
        output$slrConfInterval <- DT::renderDT({
          conf <- as.data.frame(
            predict(slr_model(), newdata = new_data, interval = "confidence", level = 0.95)
          )
          conf <- cbind(`X Value` = x0, round(conf, 4))
          DT::datatable(conf,
            options = list(dom = "t", pageLength = nrow(conf), scrollX = TRUE),
            rownames = FALSE
          )
        })
        output$slrInterpExtrap <- renderPrint({
          check <- ifelse(x0 > max(x_orig, na.rm = TRUE) | x0 < min(x_orig, na.rm = TRUE),
            "EXTRAPOLATION", "interpolation"
          )
          for (i in seq_along(x0)) {
            cat(sprintf("x = %-8.3f  =>  %s\n", x0[i], check[i]))
          }
        })
      },
      error = function(e) {
        showNotification(paste("Prediction error:", e$message), type = "error")
      }
    )
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
      selected = data$numeric_vars[1:min(2, length(data$numeric_vars))]
    )
  })

  mlr_model <- reactiveVal(NULL)
  mlr_xvars <- reactiveVal(NULL)

  observeEvent(input$runMLR, {
    req(data$raw, input$mlrY, input$mlrX)

    if (length(input$mlrX) < 2) {
      showNotification("Please select at least 2 predictor variables for MLR.", type = "error")
      return()
    }

    y <- data$raw[[input$mlrY]]
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
        pch = 19, col = rgb(0.2, 0.5, 0.8, 0.4), cex = 0.7
      )
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
        number.cex = 0.85, number.digits = 3
      )
    })

    # Observed vs predicted fit line plot
    output$mlrFitLine <- renderPlot({
      y_obs <- model.response(model.frame(model))
      y_hat <- fitted(model)
      plot(y_hat, y_obs,
        xlab = "Predicted values",
        ylab = "Observed values",
        main = "Observed vs Predicted",
        pch = 19, col = rgb(0.2, 0.5, 0.8, 0.6)
      )
      abline(0, 1, col = "red", lwd = 2, lty = 2) # perfect fit line
      abline(lm(y_obs ~ y_hat), col = "darkgreen", lwd = 2) # empirical fit line
      legend("topleft",
        legend = c("Perfect fit (y = x)", "Observed~Predicted fit"),
        col = c("red", "darkgreen"), lty = c(2, 1), lwd = 2, bty = "n"
      )
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
          ifelse(vif_vals >= 5, "#FF9800", "steelblue")
        )
        barplot(vif_vals,
          main = "VIF Values",
          ylab = "VIF",
          col = bar_colors,
          horiz = FALSE,
          ylim = c(0, max(max(vif_vals) * 1.15, 11)),
          las = 2
        )
        abline(h = 5, col = "orange", lwd = 2, lty = 2)
        abline(h = 10, col = "red", lwd = 2, lty = 3)
        legend("topright",
          legend = c("VIF = 5 (moderate)", "VIF = 10 (severe)"),
          col = c("orange", "red"), lty = c(2, 3), lwd = 2, bty = "n", cex = 0.85
        )
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
        column(
          max(2, floor(12 / length(xvars))),
          textInput(make_input_id(xv),
            paste("Values for", xv, "(comma-sep):"),
            placeholder = "e.g. 25, 10"
          )
        )
      })
      do.call(fluidRow, inputs)
    })
  })

  # MLR Prediction
  observeEvent(input$runMLRPredict, {
    req(mlr_model(), mlr_xvars())
    xvars <- mlr_xvars()

    tryCatch(
      {
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
            rownames = FALSE
          )
        })
        output$mlrConfInterval <- DT::renderDT({
          conf <- as.data.frame(
            predict(mlr_model(), newdata = new_data, interval = "confidence", level = 0.95)
          )
          conf <- cbind(Observation = seq_len(nrow(conf)), round(conf, 4))
          DT::datatable(conf,
            options = list(dom = "t", pageLength = nrow(conf), scrollX = TRUE),
            rownames = FALSE
          )
        })

        # Leverage-based check
        output$mlrInterpExtrap <- renderPrint({
          hii <- hatvalues(mlr_model())
          h_new <- (predict(mlr_model(),
            newdata = new_data,
            interval = "confidence", se.fit = TRUE
          )$se.fit /
            sigma(mlr_model()))^2
          result <- ifelse(h_new > max(hii), "EXTRAPOLATION", "interpolation")
          for (i in seq_along(result)) {
            cat(sprintf(
              "Observation %d  =>  %s  (h_new=%.4f, max(hii)=%.4f)\n",
              i, result[i], h_new[i], max(hii)
            ))
          }
        })
      },
      error = function(e) {
        showNotification(paste("Prediction error:", e$message), type = "error")
      }
    )
  })

  # ===== CHAPTER 7: POLYNOMIAL REGRESSION SERVER =====

  poly_results <- reactiveVal(NULL)

  output$polyYSelect <- renderUI({
    req(data$numeric_vars)
    nums <- data$numeric_vars
    default_y <- if ("y" %in% nums) "y" else tail(nums, 1)
    selectInput("polyY", NULL, choices = nums, selected = default_y)
  })

  output$polyXSelect <- renderUI({
    req(data$numeric_vars)
    nums <- data$numeric_vars
    default_y <- if ("y" %in% nums) "y" else tail(nums, 1)
    choices <- setdiff(nums, default_y)
    default_x <- if ("x" %in% choices) "x" else head(choices, 1)
    selectInput("polyX", NULL, choices = choices, selected = default_x)
  })

  poly_formula <- function(var_name = "x", degree = 1) {
    terms <- var_name
    if (degree >= 2) {
      terms <- c(terms, sprintf("I(%s^%d)", var_name, 2:degree))
    }
    as.formula(paste("y ~", paste(terms, collapse = " + ")))
  }

  poly_safe_vif <- function(fit) {
    tryCatch({
      vif_obj <- car::vif(fit)
      if (is.matrix(vif_obj)) {
        out <- vif_obj[, 1]
      } else {
        out <- vif_obj
      }
      out <- as.numeric(out)
      names(out) <- names(vif_obj)
      out
    }, error = function(e) NULL)
  }

  poly_metric_row <- function(label, fit, vif_vals = NULL) {
    sm <- summary(fit)
    data.frame(
      Model = label,
      R_Squared = sm$r.squared,
      Adj_R_Squared = sm$adj.r.squared,
      Residual_SE = sm$sigma,
      AIC = AIC(fit),
      Max_VIF = if (is.null(vif_vals) || length(vif_vals) == 0) NA_real_ else max(vif_vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  observeEvent(input$runPoly, {
    req(data$raw, input$polyX, input$polyY)

    df <- data.frame(
      x = data$raw[[input$polyX]],
      y = data$raw[[input$polyY]]
    )
    df <- df[complete.cases(df), , drop = FALSE]

    if (nrow(df) < 8) {
      showNotification("Need at least 8 complete observations for the polynomial chapter.", type = "error")
      return()
    }
    if (stats::sd(df$x) == 0) {
      showNotification("The selected X variable must vary.", type = "error")
      return()
    }

    sel_degree <- max(1L, min(4L, as.integer(input$polyDegree %||% 3L)))
    knot1 <- as.numeric(input$polyKnot1)
    knot2 <- as.numeric(input$polyKnot2)
    knot_single <- as.numeric(input$polySingleKnot)
    knot_pair <- sort(c(knot1, knot2))
    x_rng <- range(df$x)

    if (any(!is.finite(c(knot_single, knot_pair))) || knot_pair[1] == knot_pair[2]) {
      showNotification("Enter valid and distinct spline knots.", type = "error")
      return()
    }
    if (knot_single <= x_rng[1] || knot_single >= x_rng[2] || any(knot_pair <= x_rng[1]) || any(knot_pair >= x_rng[2])) {
      showNotification("Spline knots must lie strictly inside the range of X.", type = "error")
      return()
    }

    raw_models <- lapply(1:4, function(d) lm(poly_formula("x", d), data = df))
    names(raw_models) <- c("Linear", "Quadratic", "Cubic", "Quartic")

    df_center <- df
    df_center$xc <- df_center$x - mean(df_center$x)
    centered_models <- lapply(1:4, function(d) lm(poly_formula("xc", d), data = df_center))
    names(centered_models) <- names(raw_models)

    raw_vifs <- lapply(raw_models, poly_safe_vif)
    centered_vifs <- lapply(centered_models, poly_safe_vif)

    poly_compare <- do.call(rbind, lapply(seq_along(raw_models), function(i) {
      poly_metric_row(names(raw_models)[i], raw_models[[i]], raw_vifs[[i]])
    }))

    sel_raw <- raw_models[[sel_degree]]
    sel_center <- centered_models[[sel_degree]]
    sel_raw_vif <- raw_vifs[[sel_degree]]
    sel_center_vif <- centered_vifs[[sel_degree]]

    center_compare <- rbind(
      poly_metric_row(paste0(names(raw_models)[sel_degree], " (Raw)"), sel_raw, sel_raw_vif),
      poly_metric_row(paste0(names(raw_models)[sel_degree], " (Centered)"), sel_center, sel_center_vif)
    )

    spline_models <- list(
      `Linear spline (1 knot)` = lm(y ~ splines::bs(x, knots = knot_single, degree = 1, intercept = TRUE), data = df),
      `Linear spline (2 knots)` = lm(y ~ splines::bs(x, knots = knot_pair, degree = 1), data = df),
      `Quadratic spline (2 knots)` = lm(y ~ splines::bs(x, knots = knot_pair, degree = 2), data = df),
      `Cubic spline (2 knots)` = lm(y ~ splines::bs(x, knots = knot_pair, degree = 3), data = df)
    )

    spline_compare <- rbind(
      poly_metric_row(paste0(names(raw_models)[sel_degree], " polynomial"), sel_raw, sel_raw_vif),
      do.call(rbind, lapply(names(spline_models), function(label) poly_metric_row(label, spline_models[[label]])))
    )

    poly_results(list(
      df = df,
      df_center = df_center,
      raw_models = raw_models,
      centered_models = centered_models,
      raw_vifs = raw_vifs,
      centered_vifs = centered_vifs,
      poly_compare = poly_compare,
      center_compare = center_compare,
      selected_degree = sel_degree,
      spline_models = spline_models,
      spline_compare = spline_compare,
      knot_single = knot_single,
      knot_pair = knot_pair,
      x_label = input$polyX,
      y_label = input$polyY
    ))

    showNotification("Polynomial regression chapter updated.", type = "message")
  })

  output$polyCompareTable <- renderDT({
    req(poly_results())
    tbl <- poly_results()$poly_compare
    num_cols <- setdiff(names(tbl), "Model")
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) round(x, 4))
    datatable(tbl, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$polyFitPlot <- renderPlot({
    req(poly_results())
    r <- poly_results()
    plot(r$df$x, r$df$y,
      xlab = r$x_label, ylab = r$y_label,
      main = "Polynomial Fits by Order",
      pch = 21, bg = "darkblue", col = "white", cex = 0.9
    )
    xgrid <- seq(min(r$df$x), max(r$df$x), length.out = 300)
    cols <- c("#ef4444", "#f59e0b", "#10b981", "#2563eb")
    for (i in seq_along(r$raw_models)) {
      preds <- predict(r$raw_models[[i]], newdata = data.frame(x = xgrid))
      lines(xgrid, preds, col = cols[i], lwd = 2)
    }
    legend("topleft", legend = names(r$raw_models), col = cols, lwd = 2, bty = "n")
  })

  output$polySummary <- renderPrint({
    req(poly_results())
    r <- poly_results()
    degree_label <- names(r$raw_models)[r$selected_degree]
    cat(degree_label, "model (raw x)\n")
    cat(strrep("=", 45), "\n\n")
    print(summary(r$raw_models[[r$selected_degree]]))
    cat("\n", degree_label, "model after centering x\n", sep = "")
    cat(strrep("=", 45), "\n\n")
    print(summary(r$centered_models[[r$selected_degree]]))
  })

  output$polyResidualPlot <- renderPlot({
    req(poly_results())
    r <- poly_results()
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    for (i in seq_along(r$raw_models)) {
      fit <- r$raw_models[[i]]
      plot(
        fitted(fit), resid(fit),
        pch = 19, col = "#2563eb",
        xlab = "Fitted", ylab = "Residuals",
        main = names(r$raw_models)[i]
      )
      abline(h = 0, col = "red", lwd = 2)
      lines(lowess(fitted(fit), resid(fit)), col = "darkorange", lwd = 2)
    }
  })

  output$polyCenterCompare <- renderDT({
    req(poly_results())
    tbl <- poly_results()$center_compare
    num_cols <- setdiff(names(tbl), "Model")
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) round(x, 4))
    datatable(tbl, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$polyVifPlot <- renderPlot({
    req(poly_results())
    r <- poly_results()
    if (r$selected_degree == 1) {
      plot.new()
      text(0.5, 0.5, "VIF is not informative for a linear model with one predictor.", cex = 1.1)
      return()
    }

    raw_vif <- r$raw_vifs[[r$selected_degree]]
    cent_vif <- r$centered_vifs[[r$selected_degree]]
    terms <- union(names(raw_vif), names(cent_vif))
    mat <- rbind(
      Raw = setNames(rep(NA_real_, length(terms)), terms),
      Centered = setNames(rep(NA_real_, length(terms)), terms)
    )
    if (!is.null(raw_vif)) mat["Raw", names(raw_vif)] <- raw_vif
    if (!is.null(cent_vif)) mat["Centered", names(cent_vif)] <- cent_vif
    mat[is.na(mat)] <- 0

    barplot(mat,
      beside = TRUE, col = c("#ef4444", "#10b981"),
      las = 2, ylab = "VIF", main = "Variance Inflation Factors"
    )
    abline(h = 5, col = "darkorange", lty = 2, lwd = 2)
    legend("topright", legend = rownames(mat), fill = c("#ef4444", "#10b981"), bty = "n")
  })

  output$polyCenterText <- renderPrint({
    req(poly_results())
    r <- poly_results()
    raw_vif <- r$raw_vifs[[r$selected_degree]]
    cent_vif <- r$centered_vifs[[r$selected_degree]]

    cat("Centering interpretation\n")
    cat(strrep("=", 35), "\n\n")
    cat("Selected order:", names(r$raw_models)[r$selected_degree], "\n")
    if (r$selected_degree == 1) {
      cat("Centering does not change the shape of a first-order fit, so there is no multicollinearity issue to solve.\n")
      return()
    }
    raw_max <- if (is.null(raw_vif)) NA_real_ else max(raw_vif, na.rm = TRUE)
    cent_max <- if (is.null(cent_vif)) NA_real_ else max(cent_vif, na.rm = TRUE)
    cat(sprintf("Raw max VIF: %.4f\n", raw_max))
    cat(sprintf("Centered max VIF: %.4f\n\n", cent_max))
    cat("Centering keeps the fitted curve and R-squared essentially unchanged, but usually reduces the VIF values by removing part of the ill-conditioning in x, x^2, x^3, ...\n")
  })

  output$polySplineCompare <- renderDT({
    req(poly_results())
    tbl <- poly_results()$spline_compare
    num_cols <- setdiff(names(tbl), "Model")
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) round(x, 4))
    datatable(tbl, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$polySplinePlot <- renderPlot({
    req(poly_results())
    r <- poly_results()
    cubic_spline <- r$spline_models[["Cubic spline (2 knots)"]]
    poly_fit <- r$raw_models[[r$selected_degree]]
    xgrid <- seq(min(r$df$x), max(r$df$x), length.out = 300)
    plot(r$df$x, r$df$y,
      xlab = r$x_label, ylab = r$y_label,
      main = "Selected Polynomial vs Cubic Spline",
      pch = 21, bg = "darkblue", col = "white", cex = 0.9
    )
    lines(xgrid, predict(poly_fit, newdata = data.frame(x = xgrid)), col = "#ef4444", lwd = 2, lty = 2)
    lines(xgrid, predict(cubic_spline, newdata = data.frame(x = xgrid)), col = "#10b981", lwd = 2)
    abline(v = r$knot_pair, col = "grey50", lty = 3)
    legend("topleft",
      legend = c(paste(names(r$raw_models)[r$selected_degree], "polynomial"), "Cubic spline", "Knots"),
      col = c("#ef4444", "#10b981", "grey50"),
      lty = c(2, 1, 3), lwd = c(2, 2, 1), bty = "n"
    )
  })

  output$polySplineSummary <- renderPrint({
    req(poly_results())
    r <- poly_results()
    cat("Cubic spline with two knots\n")
    cat(strrep("=", 35), "\n")
    cat("Knots:", paste(r$knot_pair, collapse = ", "), "\n\n")
    print(summary(r$spline_models[["Cubic spline (2 knots)"]]))
  })

  output$polySplineResidualPlot <- renderPlot({
    req(poly_results())
    fit <- poly_results()$spline_models[["Cubic spline (2 knots)"]]
    plot(
      fitted(fit), resid(fit),
      xlab = "Fitted", ylab = "Residuals",
      main = "Cubic Spline Residuals vs Fitted",
      pch = 19, col = "#10b981"
    )
    abline(h = 0, col = "red", lwd = 2)
    lines(lowess(fitted(fit), resid(fit)), col = "darkorange", lwd = 2)
  })

  # ===== CHAPTER 6: LEVERAGE & INFLUENCE SERVER =====

  influence_results <- reactiveVal(NULL)
  influence_trim_fit <- reactiveVal(NULL)

  infl_bt <- function(x) paste0("`", gsub("`", "``", x), "`")

  output$inflYSelect <- renderUI({
    req(data$numeric_vars)
    nums <- data$numeric_vars
    if (length(nums) < 2) {
      return(helpText("Need at least two numeric variables to run influence diagnostics."))
    }
    default_y <- if ("y" %in% nums) "y" else tail(nums, 1)
    selectInput("inflY", NULL, choices = nums, selected = default_y)
  })

  output$inflXSelect <- renderUI({
    req(data$numeric_vars)
    nums <- data$numeric_vars
    default_y <- if ("y" %in% nums) "y" else tail(nums, 1)
    choices <- setdiff(nums, default_y)
    default_x <- intersect(c("x1", "x2", "x3", "x4", "x5"), choices)
    if (length(default_x) == 0) {
      default_x <- head(choices, min(5, length(choices)))
    }
    selectizeInput("inflX", NULL,
      choices = choices,
      selected = default_x,
      multiple = TRUE,
      options = list(placeholder = "Choose one or more predictors")
    )
  })

  observeEvent(input$runInfluence, {
    req(data$raw, input$inflY, input$inflX)

    if (length(input$inflX) == 0) {
      showNotification("Select at least one predictor for the influence model.", type = "error")
      return()
    }

    selected <- unique(c(input$inflY, input$inflX))
    raw_df <- as.data.frame(data$raw[, selected, drop = FALSE], check.names = FALSE)
    keep_rows <- complete.cases(raw_df)
    model_df <- raw_df[keep_rows, , drop = FALSE]
    row_id <- which(keep_rows)

    if (nrow(model_df) <= length(input$inflX) + 2) {
      showNotification("Not enough complete observations for the selected regression model.", type = "error")
      return()
    }

    model_formula <- as.formula(paste(infl_bt(input$inflY), "~", paste(infl_bt(input$inflX), collapse = " + ")))
    fit <- lm(model_formula, data = model_df)

    stud <- stats::rstudent(fit)
    hat <- stats::hatvalues(fit)
    cook <- stats::cooks.distance(fit)
    dff <- stats::dffits(fit)
    covr <- stats::covratio(fit)
    dfb <- stats::dfbetas(fit)

    n_obs <- nrow(model_df)
    p_params <- length(coef(fit))
    leverage_cutoff <- 2 * p_params / n_obs
    cook_cutoff <- 4 / n_obs
    dffits_cutoff <- 2 * sqrt(p_params / n_obs)
    dfbetas_cutoff <- 2 / sqrt(n_obs)
    covratio_low <- 1 - 3 * p_params / n_obs
    covratio_high <- 1 + 3 * p_params / n_obs

    max_abs_dfb <- if (is.null(dim(dfb))) abs(dfb) else apply(abs(dfb), 1, max, na.rm = TRUE)

    diag_tbl <- data.frame(
      Observation = row_id,
      Studentized_Residual = as.numeric(stud),
      Leverage = as.numeric(hat),
      Cooks_D = as.numeric(cook),
      DFFITS = as.numeric(dff),
      Max_abs_DFBETAS = as.numeric(max_abs_dfb),
      COVRATIO = as.numeric(covr),
      stringsAsFactors = FALSE
    )

    diag_tbl$High_Leverage <- diag_tbl$Leverage > leverage_cutoff
    diag_tbl$Cook_Flag <- diag_tbl$Cooks_D > cook_cutoff
    diag_tbl$DFFITS_Flag <- abs(diag_tbl$DFFITS) > dffits_cutoff
    diag_tbl$DFBETAS_Flag <- diag_tbl$Max_abs_DFBETAS > dfbetas_cutoff
    diag_tbl$COVRATIO_Flag <- diag_tbl$COVRATIO < covratio_low | diag_tbl$COVRATIO > covratio_high
    diag_tbl$Flag_Count <- rowSums(diag_tbl[, c("High_Leverage", "Cook_Flag", "DFFITS_Flag", "DFBETAS_Flag", "COVRATIO_Flag")])
    diag_tbl$Flagged <- ifelse(diag_tbl$Flag_Count > 0, "Yes", "")

    flagged_ids <- diag_tbl$Observation[diag_tbl$Flag_Count > 0]
    updateTextInput(session, "inflRemoveRows", value = paste(flagged_ids, collapse = ", "))

    robust_huber <- tryCatch(MASS::rlm(model_formula, data = model_df, psi = MASS::psi.huber), error = function(e) NULL)
    robust_bisquare <- tryCatch(MASS::rlm(model_formula, data = model_df, psi = MASS::psi.bisquare), error = function(e) NULL)

    influence_results(list(
      fit = fit,
      formula = model_formula,
      model_df = model_df,
      row_id = row_id,
      xvars = input$inflX,
      yvar = input$inflY,
      diag_tbl = diag_tbl,
      n_obs = n_obs,
      p_params = p_params,
      cutoffs = list(
        leverage = leverage_cutoff,
        cook = cook_cutoff,
        dffits = dffits_cutoff,
        dfbetas = dfbetas_cutoff,
        covratio_low = covratio_low,
        covratio_high = covratio_high
      ),
      robust_huber = robust_huber,
      robust_bisquare = robust_bisquare
    ))
    influence_trim_fit(NULL)

    showNotification("Influence diagnostics updated.", type = "message")
  })

  output$inflNBox <- renderValueBox({
    req(influence_results())
    valueBox(influence_results()$n_obs, "Usable Observations", icon = icon("table"), color = "blue")
  })

  output$inflPBox <- renderValueBox({
    req(influence_results())
    valueBox(influence_results()$p_params - 1, "Predictors in Model", icon = icon("sliders-h"), color = "teal")
  })

  output$inflLeverageBox <- renderValueBox({
    req(influence_results())
    valueBox(sprintf("%.3f", influence_results()$cutoffs$leverage), "Leverage Cutoff", icon = icon("crosshairs"), color = "yellow")
  })

  output$inflFlaggedBox <- renderValueBox({
    req(influence_results())
    flagged_n <- sum(influence_results()$diag_tbl$Flag_Count > 0)
    valueBox(flagged_n, "Flagged Observations", icon = icon("exclamation-triangle"), color = if (flagged_n > 0) "red" else "green")
  })

  output$inflTable <- renderDT({
    req(influence_results())
    tbl <- influence_results()$diag_tbl
    show_tbl <- tbl
    logical_cols <- c("High_Leverage", "Cook_Flag", "DFFITS_Flag", "DFBETAS_Flag", "COVRATIO_Flag")
    show_tbl[logical_cols] <- lapply(show_tbl[logical_cols], function(x) ifelse(x, "Yes", ""))
    numeric_cols <- setdiff(names(show_tbl), c("Observation", "Flagged", logical_cols))
    show_tbl[numeric_cols] <- lapply(show_tbl[numeric_cols], function(x) round(x, 4))
    datatable(
      show_tbl,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$inflBubblePlot <- renderPlot({
    req(influence_results())
    d <- influence_results()
    tbl <- d$diag_tbl
    cook_vals <- tbl$Cooks_D
    cook_range <- range(cook_vals, finite = TRUE)
    if (!all(is.finite(cook_range)) || diff(cook_range) < 1e-12) {
      bubble_cex <- rep(1.8, length(cook_vals))
    } else {
      bubble_cex <- 0.8 + (cook_vals - cook_range[1]) / diff(cook_range) * (3.4 - 0.8)
    }
    plot(
      tbl$Leverage, tbl$Studentized_Residual,
      pch = 21, bg = ifelse(tbl$Flag_Count > 0, "#f97316", "#2563eb"),
      col = "#0f172a", cex = bubble_cex,
      xlab = "Leverage (hii)", ylab = "Studentized Residual",
      main = "Leverage vs Studentized Residuals"
    )
    abline(v = d$cutoffs$leverage, col = "darkorange", lty = 2, lwd = 2)
    abline(h = c(-2, 2), col = "red", lty = 3, lwd = 1.5)
    flagged <- tbl$Flag_Count > 0
    if (any(flagged)) {
      text(tbl$Leverage[flagged], tbl$Studentized_Residual[flagged], labels = tbl$Observation[flagged], pos = 3, cex = 0.8)
    }
    legend(
      "topright",
      legend = c("Flagged case", "Other case"),
      pt.bg = c("#f97316", "#2563eb"),
      pch = 21, bty = "n"
    )
  })

  output$inflCookPlot <- renderPlot({
    req(influence_results())
    d <- influence_results()
    tbl <- d$diag_tbl
    plot(
      tbl$Observation, tbl$Cooks_D,
      type = "h", lwd = 2,
      col = ifelse(tbl$Cooks_D > d$cutoffs$cook, "#dc2626", "#2563eb"),
      xlab = "Observation", ylab = "Cook's D",
      main = "Cook's Distance"
    )
    points(tbl$Observation, tbl$Cooks_D, pch = 19, col = ifelse(tbl$Cooks_D > d$cutoffs$cook, "#dc2626", "#2563eb"))
    abline(h = d$cutoffs$cook, col = "darkorange", lty = 2, lwd = 2)
  })

  output$inflDffitsPlot <- renderPlot({
    req(influence_results())
    d <- influence_results()
    tbl <- d$diag_tbl
    plot(
      tbl$Observation, tbl$DFFITS,
      type = "h", lwd = 2,
      col = ifelse(abs(tbl$DFFITS) > d$cutoffs$dffits, "#dc2626", "#2563eb"),
      xlab = "Observation", ylab = "DFFITS",
      main = "DFFITS"
    )
    points(tbl$Observation, tbl$DFFITS, pch = 19, col = ifelse(abs(tbl$DFFITS) > d$cutoffs$dffits, "#dc2626", "#2563eb"))
    abline(h = c(-d$cutoffs$dffits, d$cutoffs$dffits), col = "darkorange", lty = 2, lwd = 2)
  })

  output$inflFlagSummary <- renderPrint({
    req(influence_results())
    d <- influence_results()
    tbl <- d$diag_tbl
    flagged <- tbl[tbl$Flag_Count > 0, , drop = FALSE]

    cat("Cutoff rules used\n")
    cat(strrep("=", 35), "\n")
    cat(sprintf("Leverage > %.4f\n", d$cutoffs$leverage))
    cat(sprintf("Cook's D > %.4f\n", d$cutoffs$cook))
    cat(sprintf("|DFFITS| > %.4f\n", d$cutoffs$dffits))
    cat(sprintf("Max |DFBETAS| > %.4f\n", d$cutoffs$dfbetas))
    cat(sprintf("COVRATIO outside [%.4f, %.4f]\n\n", d$cutoffs$covratio_low, d$cutoffs$covratio_high))

    if (nrow(flagged) == 0) {
      cat("No observations were flagged by these rules.\n")
    } else {
      cat("Flagged observations:\n")
      cat(paste(flagged$Observation, collapse = ", "), "\n\n")
      top_tbl <- flagged[order(-flagged$Flag_Count, -flagged$Cooks_D), c("Observation", "Flag_Count", "Leverage", "Cooks_D", "DFFITS", "Max_abs_DFBETAS", "COVRATIO")]
      print(round(top_tbl, 4), row.names = FALSE)
    }
  })

  observeEvent(input$runInfluenceTrim, {
    req(influence_results())
    d <- influence_results()
    raw_ids <- trimws(input$inflRemoveRows %||% "")
    if (!nzchar(raw_ids)) {
      showNotification("Enter at least one observation number to remove.", type = "warning")
      return()
    }

    ids <- suppressWarnings(as.integer(unlist(strsplit(gsub("[^0-9,]+", "", raw_ids), ","))))
    ids <- unique(ids[!is.na(ids)])
    ids <- ids[ids %in% d$row_id]

    if (length(ids) == 0) {
      showNotification("None of the entered observation numbers matched the fitted model rows.", type = "error")
      return()
    }

    keep <- !(d$row_id %in% ids)
    trimmed_df <- d$model_df[keep, , drop = FALSE]
    if (nrow(trimmed_df) <= length(d$xvars) + 2) {
      showNotification("Too few observations would remain after deletion.", type = "error")
      return()
    }

    trim_fit <- lm(d$formula, data = trimmed_df)
    influence_trim_fit(list(
      fit = trim_fit,
      removed = ids,
      kept_rows = d$row_id[keep],
      n_obs = nrow(trimmed_df)
    ))
    showNotification("Trimmed OLS model refit successfully.", type = "message")
  })

  output$inflCoefCompare <- renderDT({
    req(influence_results(), influence_trim_fit())
    base_fit <- influence_results()$fit
    trim_fit <- influence_trim_fit()$fit
    coef_names <- union(names(coef(base_fit)), names(coef(trim_fit)))
    comp <- data.frame(
      Term = coef_names,
      `OLS (All Data)` = coef(base_fit)[coef_names],
      `OLS (Trimmed)` = coef(trim_fit)[coef_names],
      Change = coef(trim_fit)[coef_names] - coef(base_fit)[coef_names],
      check.names = FALSE
    )
    comp[is.na(comp)] <- NA_real_
    comp[-1] <- lapply(comp[-1], round, 4)
    datatable(comp, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$inflMetricCompare <- renderPrint({
    req(influence_results(), influence_trim_fit())
    base_fit <- influence_results()$fit
    trim_fit <- influence_trim_fit()$fit
    removed <- influence_trim_fit()$removed

    cat("Original vs Trimmed OLS\n")
    cat(strrep("=", 35), "\n")
    cat("Removed observations:", paste(removed, collapse = ", "), "\n\n")

    metric_tbl <- data.frame(
      Model = c("OLS (All Data)", "OLS (Trimmed)"),
      Observations = c(length(resid(base_fit)), length(resid(trim_fit))),
      R_Squared = c(summary(base_fit)$r.squared, summary(trim_fit)$r.squared),
      Adj_R_Squared = c(summary(base_fit)$adj.r.squared, summary(trim_fit)$adj.r.squared),
      Residual_SE = c(summary(base_fit)$sigma, summary(trim_fit)$sigma),
      AIC = c(AIC(base_fit), AIC(trim_fit)),
      check.names = FALSE
    )
    print(round(metric_tbl, 4), row.names = FALSE)
  })

  output$inflRobustSummary <- renderPrint({
    req(influence_results())
    d <- influence_results()

    cat("Robust regression summaries\n")
    cat(strrep("=", 40), "\n\n")

    cat("OLS residual standard error:", round(summary(d$fit)$sigma, 4), "\n\n")

    if (!is.null(d$robust_huber)) {
      sh <- summary(d$robust_huber)
      cat("Huber M-estimation\n")
      cat(strrep("-", 28), "\n")
      print(round(sh$coefficients, 4))
      cat("Scale estimate:", round(sh$sigma, 4), "\n\n")
    } else {
      cat("Huber model could not be fit.\n\n")
    }

    if (!is.null(d$robust_bisquare)) {
      sb <- summary(d$robust_bisquare)
      cat("Tukey bisquare M-estimation\n")
      cat(strrep("-", 28), "\n")
      print(round(sb$coefficients, 4))
      cat("Scale estimate:", round(sb$sigma, 4), "\n")
    } else {
      cat("Bisquare model could not be fit.\n")
    }
  })

  output$inflRobustCompare <- renderDT({
    req(influence_results())
    d <- influence_results()
    model_list <- list(`OLS` = d$fit)
    if (!is.null(d$robust_huber)) model_list$Huber <- d$robust_huber
    if (!is.null(d$robust_bisquare)) model_list$Bisquare <- d$robust_bisquare

    coef_names <- Reduce(union, lapply(model_list, function(m) names(coef(m))))
    rows <- lapply(names(model_list), function(label) {
      fit_obj <- model_list[[label]]
      vals <- setNames(rep(NA_real_, length(coef_names)), coef_names)
      vals[names(coef(fit_obj))] <- coef(fit_obj)
      data.frame(
        Model = label,
        t(vals),
        Residual_SE = if (inherits(fit_obj, "rlm")) summary(fit_obj)$sigma else summary(fit_obj)$sigma,
        check.names = FALSE
      )
    })
    tbl <- do.call(rbind, rows)
    num_cols <- setdiff(names(tbl), "Model")
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) round(as.numeric(x), 4))
    datatable(tbl, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$inflHuberPlot <- renderPlot({
    req(influence_results())
    fit_obj <- influence_results()$robust_huber
    if (is.null(fit_obj)) {
      plot.new()
      text(0.5, 0.5, "Huber robust model unavailable.")
      return()
    }
    plot(
      resid(fit_obj), fit_obj$w,
      pch = 19, col = "darkgreen",
      xlab = "Residuals", ylab = "Weights",
      main = "Huber's Residuals vs Weights"
    )
    abline(v = 0, col = "red", lty = 2)
  })

  output$inflBisquarePlot <- renderPlot({
    req(influence_results())
    fit_obj <- influence_results()$robust_bisquare
    if (is.null(fit_obj)) {
      plot.new()
      text(0.5, 0.5, "Bisquare robust model unavailable.")
      return()
    }
    plot(
      resid(fit_obj), fit_obj$w,
      pch = 19, col = "darkorange",
      xlab = "Residuals", ylab = "Weights",
      main = "Bisquare Residuals vs Weights"
    )
    abline(v = 0, col = "red", lty = 2)
  })

  # ===== CHAPTER 9: MULTICOLLINEARITY SERVER =====

  ch9_results <- reactiveVal(NULL)
  ch9_xvars <- reactiveVal(character(0))

  calc_vif_from_matrix <- function(M) {
    M <- as.matrix(M)
    if (ncol(M) < 2) {
      return(setNames(rep(NA_real_, ncol(M)), colnames(M)))
    }
    out <- rep(NA_real_, ncol(M))
    names(out) <- colnames(M)
    for (j in seq_len(ncol(M))) {
      yj <- M[, j]
      Xj <- M[, -j, drop = FALSE]
      fit_j <- tryCatch(stats::lm(yj ~ Xj), error = function(e) NULL)
      if (!is.null(fit_j)) {
        r2 <- summary(fit_j)$r.squared
        if (is.finite(r2) && r2 < 1) out[j] <- 1 / (1 - r2)
      }
    }
    out
  }

  calc_ridge_vif_from_matrix <- function(M, lambda) {
    M <- as.matrix(M)
    if (ncol(M) < 2 || !is.finite(lambda)) {
      return(setNames(rep(NA_real_, ncol(M)), colnames(M)))
    }

    keep <- apply(M, 2, function(x) stats::sd(x, na.rm = TRUE) > 0)
    M <- M[, keep, drop = FALSE]
    if (ncol(M) < 2) {
      return(setNames(rep(NA_real_, ncol(M)), colnames(M)))
    }

    C <- stats::cor(scale(M), use = "pairwise.complete.obs")
    C[!is.finite(C)] <- 0

    p <- ncol(C)
    A <- tryCatch(solve(C + lambda * diag(p)), error = function(e) NULL)
    if (is.null(A)) {
      return(setNames(rep(NA_real_, p), colnames(C)))
    }

    ridge_vif <- diag(A %*% C %*% A)
    names(ridge_vif) <- colnames(C)
    ridge_vif
  }

  output$ch9YSelect <- renderUI({
    req(data$numeric_vars)
    selectInput("ch9Y", "Response (Y):", choices = data$numeric_vars)
  })

  output$ch9XSelect <- renderUI({
    req(data$numeric_vars)
    selectInput(
      "ch9X",
      "Predictors:",
      choices = data$numeric_vars,
      multiple = TRUE,
      selected = data$numeric_vars[1:min(3, length(data$numeric_vars))]
    )
  })

  output$ch9NewXInputs <- renderUI({
    xv <- ch9_xvars()
    if (length(xv) != 3) {
      return(tags$div(
        class = "hint-text",
        icon("info-circle"),
        "Run the multicollinearity model first (with exactly 3 predictors)."
      ))
    }
    inputs <- lapply(xv, function(v) {
      textInput(
        inputId = paste0("ch9new_", gsub("[^a-zA-Z0-9]", "_", v)),
        label = paste("Values for", v, "(comma-separated):"),
        value = ""
      )
    })
    do.call(tagList, inputs)
  })

  observeEvent(input$runCh9, {
    req(data$raw, input$ch9Y, input$ch9X)

    if (length(input$ch9X) != 3) {
      showNotification("Please select exactly 3 predictors.", type = "error")
      return()
    }

    y_name <- input$ch9Y
    x_names <- input$ch9X
    if (y_name %in% x_names) {
      showNotification("Response Y cannot be included among the 3 predictors.", type = "error")
      return()
    }
    ch9_xvars(x_names)

    sel <- c(y_name, x_names)
    df_raw <- data$raw[, sel, drop = FALSE]
    df_raw <- df_raw[complete.cases(df_raw), , drop = FALSE]

    if (nrow(df_raw) < 12) {
      showNotification("Need at least 12 complete rows to run this chapter reliably.", type = "error")
      return()
    }

    bt <- function(v) paste0("`", v, "`")
    i_terms <- c(
      paste0("I(", bt(x_names[1]), "^2)"),
      paste0("I(", bt(x_names[2]), "^2)"),
      paste0("I(", bt(x_names[3]), "^2)"),
      paste0("I(", bt(x_names[1]), "*", bt(x_names[2]), ")"),
      paste0("I(", bt(x_names[1]), "*", bt(x_names[3]), ")"),
      paste0("I(", bt(x_names[2]), "*", bt(x_names[3]), ")")
    )
    rhs <- c(sapply(x_names, bt), i_terms)
    form_orig <- as.formula(paste(bt(y_name), "~", paste(rhs, collapse = " + ")))

    fit_orig <- tryCatch(lm(form_orig, data = df_raw), error = function(e) NULL)
    if (is.null(fit_orig)) {
      showNotification("Original quadratic model failed to fit.", type = "error")
      return()
    }

    vif_orig <- tryCatch(car::vif(fit_orig), error = function(e) NULL)

    n <- nrow(df_raw)
    center_scale <- function(x) {
      denom <- sqrt((n - 1) * stats::var(x))
      if (!is.finite(denom) || denom == 0) denom <- stats::sd(x)
      if (!is.finite(denom) || denom == 0) denom <- 1
      (x - mean(x)) / denom
    }

    df_cent <- data.frame(
      y = df_raw[[y_name]],
      x1 = center_scale(df_raw[[x_names[1]]]),
      x2 = center_scale(df_raw[[x_names[2]]]),
      x3 = center_scale(df_raw[[x_names[3]]])
    )

    form_cent <- y ~ x1 + x2 + x3 + I(x1^2) + I(x2^2) + I(x3^2) +
      I(x1 * x2) + I(x1 * x3) + I(x2 * x3)
    fit_cent <- tryCatch(lm(form_cent, data = df_cent), error = function(e) NULL)
    if (is.null(fit_cent)) {
      showNotification("Centered model failed to fit.", type = "error")
      return()
    }

    vif_cent <- tryCatch(car::vif(fit_cent), error = function(e) NULL)

    X <- model.matrix(form_orig, data = df_raw)[, -1, drop = FALSE]
    y_vec <- df_raw[[y_name]]
    cv_folds <- max(3, min(10, floor(nrow(df_raw) / 3)))

    cv_ridge <- NULL
    cv_lasso <- NULL
    ridge_model <- NULL
    lasso_model <- NULL
    best_lambda_ridge <- NA_real_
    best_lambda_lasso <- NA_real_
    ridge_coef <- NULL
    lasso_coef <- NULL

    if (has_glmnet()) {
      set.seed(523132)
      ridge_lambda <- exp(seq(log(0.001), log(10), length.out = 100))
      cv_ridge <- suppressWarnings(glmnet::cv.glmnet(
        X, y_vec,
        alpha = 0, standardize = TRUE, nfolds = cv_folds, lambda = ridge_lambda
      ))
      best_lambda_ridge <- cv_ridge$lambda.min
      ridge_model <- glmnet::glmnet(X, y_vec, alpha = 0, lambda = best_lambda_ridge)
      ridge_coef <- as.matrix(stats::coef(ridge_model))

      cv_lasso <- suppressWarnings(glmnet::cv.glmnet(
        X, y_vec,
        alpha = 1, standardize = TRUE, nfolds = cv_folds
      ))
      # Use the 1-SE rule for LASSO so the displayed model reflects variable selection,
      # not just the smallest CV error with nearly all terms retained.
      best_lambda_lasso <- cv_lasso$lambda.1se
      lasso_model <- cv_lasso$glmnet.fit
      lasso_coef <- as.matrix(stats::coef(cv_lasso, s = "lambda.1se"))
    }

    vif_ridge <- if (!is.null(ridge_coef)) {
      tryCatch(calc_ridge_vif_from_matrix(X, best_lambda_ridge), error = function(e) NULL)
    } else {
      NULL
    }

    vif_lasso <- NULL
    if (!is.null(lasso_coef)) {
      lasso_vec <- as.numeric(lasso_coef[, 1])
      names(lasso_vec) <- rownames(lasso_coef)
      active_terms <- setdiff(names(lasso_vec[abs(lasso_vec) > 1e-8]), "(Intercept)")
      if (length(active_terms) >= 2) {
        vif_lasso <- tryCatch(calc_vif_from_matrix(X[, active_terms, drop = FALSE]), error = function(e) NULL)
      } else {
        vif_lasso <- setNames(rep(NA_real_, length(active_terms)), active_terms)
      }
    }

    cv_rmse_orig <- tryCatch(
      {
        suppressWarnings(sqrt(boot::cv.glm(df_raw, glm(form_orig, data = df_raw), K = cv_folds)$delta[1]))
      },
      error = function(e) NA_real_
    )

    cv_rmse_cent <- tryCatch(
      {
        suppressWarnings(sqrt(boot::cv.glm(df_cent, glm(form_cent, data = df_cent), K = cv_folds)$delta[1]))
      },
      error = function(e) NA_real_
    )

    cv_rmse_ridge <- if (!is.null(cv_ridge)) sqrt(min(cv_ridge$cvm)) else NA_real_
    cv_rmse_lasso <- if (!is.null(cv_lasso)) {
      lasso_idx <- which.min(abs(cv_lasso$lambda - best_lambda_lasso))
      sqrt(cv_lasso$cvm[lasso_idx])
    } else {
      NA_real_
    }

    ch9_results(list(
      y_name = y_name,
      x_names = x_names,
      form_orig = form_orig,
      fit_orig = fit_orig,
      vif_orig = vif_orig,
      fit_cent = fit_cent,
      vif_cent = vif_cent,
      center_n = n,
      X_cols = colnames(X),
      cv_folds = cv_folds,
      cv_ridge = cv_ridge,
      cv_lasso = cv_lasso,
      ridge_model = ridge_model,
      lasso_model = lasso_model,
      best_lambda_ridge = best_lambda_ridge,
      best_lambda_lasso = best_lambda_lasso,
      ridge_coef = ridge_coef,
      lasso_coef = lasso_coef,
      vif_ridge = vif_ridge,
      vif_lasso = vif_lasso,
      rmse = c(
        OLS = cv_rmse_orig,
        `OLS (Centered)` = cv_rmse_cent,
        Ridge = cv_rmse_ridge,
        LASSO = cv_rmse_lasso
      )
    ))

    if (!has_glmnet()) {
      showNotification("Model completed. Ridge/LASSO skipped because 'glmnet' is not installed.", type = "warning")
    } else {
      showNotification("Multicollinearity model completed successfully.", type = "message")
    }
  })

  output$ch9ModelOrig <- renderPrint({
    req(ch9_results())
    r <- ch9_results()
    cat("Quadratic model with original predictors:\n")
    print(r$form_orig)
    cat("\n")
    print(summary(r$fit_orig))
  })

  make_vif_table <- function(vif_vals) {
    if (is.null(vif_vals)) {
      return(NULL)
    }
    data.frame(
      Term = names(vif_vals),
      VIF = as.numeric(vif_vals),
      Level = ifelse(vif_vals > 10, "Severe",
        ifelse(vif_vals >= 5, "Moderate", "Acceptable")
      ),
      stringsAsFactors = FALSE
    )
  }

  output$ch9VIFOrigTbl <- DT::renderDT({
    req(ch9_results())
    tbl <- make_vif_table(ch9_results()$vif_orig)
    req(tbl)
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 12, scrollX = TRUE, autoWidth = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = "VIF", digits = 4)
    DT::formatStyle(
      dt, "Level",
      color = "white",
      fontWeight = "700",
      backgroundColor = DT::styleEqual(
        c("Acceptable", "Moderate", "Severe"),
        c("#2563eb", "#f59e0b", "#ef4444")
      )
    )
  })

  output$ch9VIFOrigPlot <- renderPlot({
    req(ch9_results())
    vif_vals <- ch9_results()$vif_orig
    if (is.null(vif_vals)) {
      plot.new()
      text(0.5, 0.5, "VIF unavailable for original model")
      return()
    }
    vif_cap <- pmin(vif_vals, 10)
    cols <- ifelse(vif_vals > 10, "#ef4444", ifelse(vif_vals >= 5, "#f59e0b", "steelblue"))
    mids <- barplot(
      vif_cap,
      col = cols,
      ylab = "VIF (Capped at 10)",
      main = "VIF Values (Original Data)",
      las = 2,
      ylim = c(0, 10)
    )
    text(
      x = mids,
      y = pmin(vif_cap + 0.35, 9.8),
      labels = ifelse(vif_vals > 10, paste0(">", "10 (", round(vif_vals, 1), ")"), round(vif_vals, 1)),
      cex = 0.75,
      col = "#111827"
    )
    abline(h = 5, col = "#f59e0b", lty = 2, lwd = 2)
    abline(h = 10, col = "#ef4444", lty = 2, lwd = 2)
  })

  output$ch9CenterInfo <- renderPrint({
    req(ch9_results())
    r <- ch9_results()
    cat("Centered/scaled variables used:\n")
    cat("x1 =", r$x_names[1], ", x2 =", r$x_names[2], ", x3 =", r$x_names[3], "\n")
    cat("Formula applied: x = (x - mean(x)) / sqrt((n - 1) * var(x))\n")
    cat("n =", r$center_n, "\n")
  })

  output$ch9ModelCent <- renderPrint({
    req(ch9_results())
    print(summary(ch9_results()$fit_cent))
  })

  output$ch9VIFCentTbl <- DT::renderDT({
    req(ch9_results())
    tbl <- make_vif_table(ch9_results()$vif_cent)
    req(tbl)
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 12, scrollX = TRUE, autoWidth = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = "VIF", digits = 4)
    DT::formatStyle(
      dt, "Level",
      color = "white",
      fontWeight = "700",
      backgroundColor = DT::styleEqual(
        c("Acceptable", "Moderate", "Severe"),
        c("#2563eb", "#f59e0b", "#ef4444")
      )
    )
  })

  output$ch9VIFCentPlot <- renderPlot({
    req(ch9_results())
    vif_vals <- ch9_results()$vif_cent
    if (is.null(vif_vals)) {
      plot.new()
      text(0.5, 0.5, "VIF unavailable for centered model")
      return()
    }
    vif_cap <- pmin(vif_vals, 10)
    cols <- ifelse(vif_vals > 10, "#ef4444", ifelse(vif_vals >= 5, "#f59e0b", "steelblue"))
    mids <- barplot(
      vif_cap,
      col = cols,
      ylab = "VIF (Capped at 10)",
      main = "VIF Values (Centered Data)",
      las = 2,
      ylim = c(0, 10)
    )
    text(
      x = mids,
      y = pmin(vif_cap + 0.35, 9.8),
      labels = ifelse(vif_vals > 10, paste0(">", "10 (", round(vif_vals, 1), ")"), round(vif_vals, 1)),
      cex = 0.75,
      col = "#111827"
    )
    abline(h = 5, col = "#f59e0b", lty = 2, lwd = 2)
    abline(h = 10, col = "#ef4444", lty = 2, lwd = 2)
  })

  output$ch9RidgeCVPlot <- renderPlot({
    req(ch9_results())
    r <- ch9_results()
    if (is.null(r$cv_ridge)) {
      plot.new()
      text(0.5, 0.5, "Ridge unavailable. Install package 'glmnet'.")
      return()
    }
    plot(r$cv_ridge, main = paste0("Ridge Regression (", r$cv_folds, "-fold CV)"))
  })

  make_regularization_table <- function(best_lambda, coef_mat, vif_vals = NULL, model_name = "Model",
                                        lambda_label = "Best lambda", show_selection = FALSE,
                                        zero_tol = 1e-8) {
    if (is.null(coef_mat) || !is.matrix(coef_mat)) {
      out <- data.frame(
        Item = paste(model_name, "Status"),
        Value = "Unavailable. Install package 'glmnet'.",
        VIF = NA_real_,
        stringsAsFactors = FALSE
      )
      if (show_selection) out$Status <- "Unavailable"
      return(out)
    }
    coef_df <- data.frame(
      Item = rownames(coef_mat),
      Value = as.numeric(coef_mat[, 1]),
      VIF = NA_real_,
      stringsAsFactors = FALSE
    )
    if (show_selection) {
      coef_df$Status <- ifelse(
        coef_df$Item == "(Intercept)", "Intercept",
        ifelse(abs(coef_df$Value) > zero_tol, "Selected", "Dropped")
      )
    }
    if (!is.null(vif_vals) && length(vif_vals) > 0) {
      idx <- match(coef_df$Item, names(vif_vals))
      coef_df$VIF[!is.na(idx)] <- as.numeric(vif_vals[idx[!is.na(idx)]])
    }
    lambda_row <- data.frame(Item = lambda_label, Value = as.numeric(best_lambda), VIF = NA_real_, stringsAsFactors = FALSE)
    if (show_selection) lambda_row$Status <- "CV choice"
    rbind(
      lambda_row,
      coef_df
    )
  }

  build_model_equation <- function(response_name, coef_mat, nonzero_only = FALSE, digits = 4) {
    if (is.null(coef_mat) || !is.matrix(coef_mat)) return(NULL)
    vals <- as.numeric(coef_mat[, 1])
    names(vals) <- rownames(coef_mat)
    b0 <- vals["(Intercept)"]
    terms <- vals[setdiff(names(vals), "(Intercept)")]
    if (nonzero_only) terms <- terms[abs(terms) > 1e-8]
    if (length(terms) == 0) {
      return(sprintf("%s = %.4f", response_name, b0))
    }
    pieces <- vapply(names(terms), function(nm) {
      coef <- terms[[nm]]
      sign <- if (coef >= 0) "+" else "-"
      sprintf(" %s %.4f*%s", sign, abs(coef), nm)
    }, character(1))
    paste0(response_name, " = ", sprintf("%.4f", b0), paste0(pieces, collapse = ""))
  }

  output$ch9RidgeTbl <- DT::renderDT({
    req(ch9_results())
    r <- ch9_results()
    tbl <- make_regularization_table(r$best_lambda_ridge, r$ridge_coef, r$vif_ridge, model_name = "Ridge")
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 12, scrollX = TRUE, autoWidth = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = c("Value", "VIF"), digits = 6)
    DT::formatStyle(
      dt, "Item",
      target = "row",
      backgroundColor = DT::styleEqual("Best lambda", "#fff7e6"),
      fontWeight = DT::styleEqual("Best lambda", "700")
    )
  })

  output$ch9RidgeEq <- renderUI({
    req(ch9_results())
    r <- ch9_results()
    eq <- build_model_equation(r$y_name, r$ridge_coef, nonzero_only = FALSE, digits = 4)
    if (is.null(eq)) {
      return(tags$div(class = "hint-text", "Ridge model equation unavailable."))
    }
    tags$div(
      style = "background:#fff7e6;border:1px solid #f59e0b;border-radius:8px;padding:10px 12px;",
      tags$b("Ridge fitted model:"),
      tags$div(style = "margin-top:6px;word-break:break-word;font-family:monospace;",
               htmltools::htmlEscape(eq))
    )
  })

  render_post_vif_table <- function(vif_vals, empty_label) {
    if (is.null(vif_vals) || length(vif_vals) == 0) {
      tbl <- data.frame(
        Term = empty_label,
        VIF = NA_real_,
        Level = "N/A",
        stringsAsFactors = FALSE
      )
    } else {
      tbl <- data.frame(
        Term = names(vif_vals),
        VIF = as.numeric(vif_vals),
        Level = ifelse(!is.finite(vif_vals), "N/A",
                       ifelse(vif_vals > 10, "Severe",
                              ifelse(vif_vals >= 5, "Moderate", "Acceptable"))),
        stringsAsFactors = FALSE
      )
    }

    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(tbl), scrollX = TRUE, autoWidth = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = "VIF", digits = 6)
    DT::formatStyle(
      dt, "Level",
      color = "white",
      fontWeight = "700",
      backgroundColor = DT::styleEqual(
        c("Acceptable", "Moderate", "Severe", "N/A"),
        c("#2563eb", "#f59e0b", "#ef4444", "#6b7280")
      )
    )
  }

  output$ch9RidgeVifTbl <- DT::renderDT({
    req(ch9_results())
    render_post_vif_table(ch9_results()$vif_ridge, "Ridge VIF unavailable")
  })

  output$ch9LassoCVPlot <- renderPlot({
    req(ch9_results())
    r <- ch9_results()
    if (is.null(r$cv_lasso)) {
      plot.new()
      text(0.5, 0.5, "LASSO unavailable. Install package 'glmnet'.")
      return()
    }
    plot(r$cv_lasso, main = paste0("LASSO Regression (", r$cv_folds, "-fold CV)"))
  })

  output$ch9LassoTbl <- DT::renderDT({
    req(ch9_results())
    r <- ch9_results()
    tbl <- make_regularization_table(
      r$best_lambda_lasso, r$lasso_coef, r$vif_lasso,
      model_name = "LASSO",
      lambda_label = "Selected lambda (1-SE)",
      show_selection = TRUE
    )
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 12, scrollX = TRUE, autoWidth = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = c("Value", "VIF"), digits = 6)
    dt <- DT::formatStyle(
      dt, "Item",
      target = "row",
      backgroundColor = DT::styleEqual("Selected lambda (1-SE)", "#fff7e6"),
      fontWeight = DT::styleEqual("Selected lambda (1-SE)", "700")
    )
    DT::formatStyle(
      dt, "Status",
      color = DT::styleEqual(c("Selected", "Dropped", "Intercept", "CV choice"), c("#047857", "#991b1b", "#374151", "#92400e")),
      fontWeight = DT::styleEqual(c("Selected", "Dropped", "CV choice"), c("700", "700", "700"))
    )
  })

  output$ch9LassoEq <- renderUI({
    req(ch9_results())
    r <- ch9_results()
    eq <- build_model_equation(r$y_name, r$lasso_coef, nonzero_only = TRUE, digits = 4)
    if (is.null(eq)) {
      return(tags$div(class = "hint-text", "LASSO model equation unavailable."))
    }
    tags$div(
      style = "background:#fff1f2;border:1px solid #ef4444;border-radius:8px;padding:10px 12px;",
      tags$b("LASSO fitted model (non-zero terms):"),
      tags$div(style = "margin-top:6px;word-break:break-word;font-family:monospace;",
               htmltools::htmlEscape(eq))
    )
  })

  output$ch9LassoVifTbl <- DT::renderDT({
    req(ch9_results())
    render_post_vif_table(ch9_results()$vif_lasso, "No active terms for VIF")
  })

  observeEvent(input$runCh9Predict, {
    req(ch9_results())
    r <- ch9_results()
    x_names <- r$x_names

    parsed <- lapply(x_names, function(v) {
      raw_txt <- input[[paste0("ch9new_", gsub("[^a-zA-Z0-9]", "_", v))]]
      if (is.null(raw_txt) || trimws(raw_txt) == "") {
        return(numeric(0))
      }
      as.numeric(trimws(strsplit(raw_txt, ",")[[1]]))
    })
    names(parsed) <- x_names

    lens <- lengths(parsed)
    if (any(lens == 0)) {
      showNotification("Enter at least one numeric value for each predictor.", type = "error")
      return()
    }
    if (any(vapply(parsed, function(x) any(is.na(x)), logical(1)))) {
      showNotification("Please enter only numeric values in the prediction inputs.", type = "error")
      return()
    }
    if (length(unique(lens)) != 1) {
      showNotification("Each predictor must have the same number of values.", type = "error")
      return()
    }

    new_data <- as.data.frame(parsed, check.names = FALSE)
    form_no_y <- stats::delete.response(stats::terms(r$form_orig))
    X_new <- model.matrix(form_no_y, data = new_data)
    if ("(Intercept)" %in% colnames(X_new)) {
      X_new <- X_new[, setdiff(colnames(X_new), "(Intercept)"), drop = FALSE]
    }
    X_new <- X_new[, r$X_cols, drop = FALSE]

    pred_ols <- as.numeric(predict(r$fit_orig, newdata = new_data))
    pred_ridge <- if (!is.null(r$cv_ridge)) {
      as.numeric(predict(r$cv_ridge, newx = X_new, s = r$best_lambda_ridge))
    } else {
      rep(NA_real_, nrow(new_data))
    }
    pred_lasso <- if (!is.null(r$cv_lasso)) {
      as.numeric(predict(r$cv_lasso, newx = X_new, s = r$best_lambda_lasso))
    } else {
      rep(NA_real_, nrow(new_data))
    }

    out <- cbind(new_data, `OLS Pred` = pred_ols, `Ridge Pred` = pred_ridge, `LASSO Pred` = pred_lasso)

    output$ch9PredTable <- DT::renderDT({
      DT::datatable(
        round(out, 6),
        rownames = FALSE,
        options = list(pageLength = min(10, nrow(out)), scrollX = TRUE)
      )
    })
  })

  output$ch9ComparePlot <- renderPlot({
    req(ch9_results())
    rmse_vals <- ch9_results()$rmse
    keep <- is.finite(rmse_vals)
    rmse_plot <- rmse_vals[keep]
    if (length(rmse_plot) == 0) {
      plot.new()
      text(0.5, 0.5, "No CV RMSE values available.")
      return()
    }
    palette <- c("#2563eb", "#1d4ed8", "#0ea5e9", "#16a34a")
    cols <- palette[seq_along(rmse_plot)]
    folds <- ch9_results()$cv_folds %||% 10
    barplot(rmse_plot, col = cols, ylab = paste0(folds, "-fold CV RMSE"), las = 2, main = "Model Comparison")
  })

  # ===== MODEL BUILDING SERVER =====

  mb_results <- reactiveVal(NULL)
  mb_final_model <- reactiveVal(NULL)

  mb_bt <- function(x) paste0("`", gsub("`", "``", x), "`")

  mb_formula <- function(y_name, x_names) {
    rhs <- if (length(x_names) == 0) "1" else paste(mb_bt(x_names), collapse = " + ")
    as.formula(paste(mb_bt(y_name), "~", rhs))
  }

  mb_complete_df <- function(df, y_name, x_names) {
    sel <- c(y_name, x_names)
    out <- df[, sel, drop = FALSE]
    out <- out[complete.cases(out), , drop = FALSE]
    as.data.frame(out, check.names = FALSE)
  }

  mb_model_metrics <- function(fit, full_mse, total_ss, n) {
    p <- length(stats::coef(fit))
    sse <- sum(stats::resid(fit)^2)
    h <- stats::hatvalues(fit)
    press <- sum((stats::resid(fit) / pmax(1 - h, 1e-8))^2)
    sm <- summary(fit)
    data.frame(
      p = p,
      R_Squared = sm$r.squared,
      Adj_R_Squared = sm$adj.r.squared,
      MSE = sse / stats::df.residual(fit),
      Pred_MSE = press / n,
      Pred_R_Squared = 1 - press / total_ss,
      Cp = sse / full_mse - (n - 2 * p),
      AIC = stats::AIC(fit),
      BIC = stats::BIC(fit),
      stringsAsFactors = FALSE
    )
  }

  mb_all_possible <- function(df, y_name, x_names) {
    full_fit <- lm(mb_formula(y_name, x_names), data = df)
    full_sse <- sum(resid(full_fit)^2)
    full_mse <- full_sse / df.residual(full_fit)
    y_vec <- df[[y_name]]
    total_ss <- sum((y_vec - mean(y_vec))^2)
    n <- nrow(df)

    rows <- list()
    idx <- 1
    for (k in seq_along(x_names)) {
      combos <- combn(x_names, k, simplify = FALSE)
      for (vars in combos) {
        fit <- lm(mb_formula(y_name, vars), data = df)
        met <- mb_model_metrics(fit, full_mse, total_ss, n)
        rows[[idx]] <- data.frame(
          Predictor_Count = k,
          Predictors = paste(vars, collapse = " + "),
          Formula = paste(y_name, "~", paste(vars, collapse = " + ")),
          met,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
    out <- do.call(rbind, rows)
    out$Cp_minus_p_abs <- abs(out$Cp - out$p)
    out[order(out$Predictor_Count, out$AIC), , drop = FALSE]
  }

  mb_pick_best <- function(tbl) {
    picks <- list(
      "Minimum MSE" = list(idx = which.min(tbl$MSE), value = "MSE", goal = "Minimize"),
      "Minimum prediction MSE" = list(idx = which.min(tbl$Pred_MSE), value = "Pred_MSE", goal = "Minimize"),
      "Maximum adjusted R-squared" = list(idx = which.max(tbl$Adj_R_Squared), value = "Adj_R_Squared", goal = "Maximize"),
      "Maximum predicted R-squared" = list(idx = which.max(tbl$Pred_R_Squared), value = "Pred_R_Squared", goal = "Maximize"),
      "Cp closest to p" = list(idx = which.min(tbl$Cp_minus_p_abs), value = "Cp", goal = "Cp near p"),
      "Minimum AIC" = list(idx = which.min(tbl$AIC), value = "AIC", goal = "Minimize"),
      "Minimum BIC" = list(idx = which.min(tbl$BIC), value = "BIC", goal = "Minimize")
    )
    do.call(rbind, lapply(names(picks), function(nm) {
      pick <- picks[[nm]]
      row <- tbl[pick$idx, , drop = FALSE]
      data.frame(
        Criterion = nm,
        Goal = pick$goal,
        Predictors = row$Predictors,
        Value = row[[pick$value]],
        stringsAsFactors = FALSE
      )
    }))
  }

  mb_best_by_size <- function(tbl) {
    pieces <- split(tbl, tbl$Predictor_Count)
    do.call(rbind, lapply(pieces, function(x) {
      x[which.min(x$Cp_minus_p_abs), c(
        "Predictor_Count", "Predictors", "Adj_R_Squared",
        "Pred_R_Squared", "MSE", "Cp", "AIC", "BIC"
      ), drop = FALSE]
    }))
  }

  mb_forward_select <- function(df, y_name, x_names, p_enter = 0.10) {
    selected <- character(0)
    remaining <- x_names
    trace <- character(0)
    steps <- list()
    add_step <- function(action, predictor, p_value, decision) {
      steps[[length(steps) + 1]] <<- data.frame(
        Step = length(steps) + 1,
        Action = action,
        Predictor = predictor,
        P_Value = p_value,
        Decision = decision,
        stringsAsFactors = FALSE
      )
    }
    repeat {
      if (length(remaining) == 0) break
      pvals <- sapply(remaining, function(v) {
        reduced <- lm(mb_formula(y_name, selected), data = df)
        full <- lm(mb_formula(y_name, c(selected, v)), data = df)
        an <- anova(reduced, full)
        as.numeric(an$`Pr(>F)`[2])
      })
      best <- names(which.min(pvals))
      best_p <- min(pvals, na.rm = TRUE)
      if (is.finite(best_p) && best_p <= p_enter) {
        selected <- c(selected, best)
        remaining <- setdiff(remaining, best)
        trace <- c(trace, sprintf("Add %-20s p = %.4f", best, best_p))
        add_step("Add", best, best_p, "Entered the model")
      } else {
        trace <- c(trace, sprintf("Stop: best remaining p = %.4f > p-enter %.4f", best_p, p_enter))
        add_step("Stop", "No additional predictor", best_p, "Best p-value is above p-enter")
        break
      }
    }
    step_tbl <- if (length(steps) == 0) {
      data.frame(Step = integer(0), Action = character(0), Predictor = character(0), P_Value = numeric(0), Decision = character(0))
    } else {
      do.call(rbind, steps)
    }
    list(selected = selected, trace = trace, steps = step_tbl)
  }

  mb_backward_select <- function(df, y_name, x_names, p_remove = 0.15) {
    selected <- x_names
    trace <- character(0)
    steps <- list()
    add_step <- function(action, predictor, p_value, decision) {
      steps[[length(steps) + 1]] <<- data.frame(
        Step = length(steps) + 1,
        Action = action,
        Predictor = predictor,
        P_Value = p_value,
        Decision = decision,
        stringsAsFactors = FALSE
      )
    }
    repeat {
      if (length(selected) == 0) break
      fit <- lm(mb_formula(y_name, selected), data = df)
      drops <- tryCatch(drop1(fit, test = "F"), error = function(e) NULL)
      if (is.null(drops) || !("Pr(>F)" %in% names(drops))) break
      pvals <- drops$`Pr(>F)`
      names(pvals) <- rownames(drops)
      pvals <- pvals[setdiff(names(pvals), "<none>")]
      if (length(pvals) == 0 || all(is.na(pvals))) break
      worst <- names(which.max(pvals))
      worst_p <- max(pvals, na.rm = TRUE)
      if (is.finite(worst_p) && worst_p > p_remove) {
        selected <- setdiff(selected, worst)
        trace <- c(trace, sprintf("Remove %-17s p = %.4f", worst, worst_p))
        add_step("Remove", worst, worst_p, "Removed from the model")
      } else {
        trace <- c(trace, sprintf("Stop: largest p = %.4f <= p-remove %.4f", worst_p, p_remove))
        add_step("Stop", worst, worst_p, "All remaining predictors pass the rule")
        break
      }
    }
    step_tbl <- if (length(steps) == 0) {
      data.frame(Step = integer(0), Action = character(0), Predictor = character(0), P_Value = numeric(0), Decision = character(0))
    } else {
      do.call(rbind, steps)
    }
    list(selected = selected, trace = trace, steps = step_tbl)
  }

  mb_stepwise_select <- function(df, y_name, x_names, p_enter = 0.10, p_remove = 0.15) {
    selected <- character(0)
    trace <- character(0)
    steps <- list()
    add_step <- function(action, predictor, p_value, decision) {
      steps[[length(steps) + 1]] <<- data.frame(
        Step = length(steps) + 1,
        Action = action,
        Predictor = predictor,
        P_Value = p_value,
        Decision = decision,
        stringsAsFactors = FALSE
      )
    }
    repeat {
      remaining <- setdiff(x_names, selected)
      changed <- FALSE
      if (length(remaining) > 0) {
        pvals <- sapply(remaining, function(v) {
          reduced <- lm(mb_formula(y_name, selected), data = df)
          full <- lm(mb_formula(y_name, c(selected, v)), data = df)
          an <- anova(reduced, full)
          as.numeric(an$`Pr(>F)`[2])
        })
        best <- names(which.min(pvals))
        best_p <- min(pvals, na.rm = TRUE)
        if (is.finite(best_p) && best_p <= p_enter) {
          selected <- c(selected, best)
          trace <- c(trace, sprintf("Add %-20s p = %.4f", best, best_p))
          add_step("Add", best, best_p, "Entered the model")
          changed <- TRUE
        }
      }
      repeat {
        if (length(selected) == 0) break
        fit <- lm(mb_formula(y_name, selected), data = df)
        drops <- tryCatch(drop1(fit, test = "F"), error = function(e) NULL)
        if (is.null(drops) || !("Pr(>F)" %in% names(drops))) break
        pvals <- drops$`Pr(>F)`
        names(pvals) <- rownames(drops)
        pvals <- pvals[setdiff(names(pvals), "<none>")]
        if (length(pvals) == 0 || all(is.na(pvals))) break
        worst <- names(which.max(pvals))
        worst_p <- max(pvals, na.rm = TRUE)
        if (is.finite(worst_p) && worst_p > p_remove) {
          selected <- setdiff(selected, worst)
          trace <- c(trace, sprintf("Remove %-17s p = %.4f", worst, worst_p))
          add_step("Remove", worst, worst_p, "Removed after reassessment")
          changed <- TRUE
        } else {
          break
        }
      }
      if (!changed) {
        trace <- c(trace, "Stop: no variable met the entry/removal rule.")
        add_step("Stop", "No change", NA_real_, "No variable met the entry/removal rule")
        break
      }
    }
    step_tbl <- if (length(steps) == 0) {
      data.frame(Step = integer(0), Action = character(0), Predictor = character(0), P_Value = numeric(0), Decision = character(0))
    } else {
      do.call(rbind, steps)
    }
    list(selected = selected, trace = trace, steps = step_tbl)
  }

  output$mbYSelect <- renderUI({
    req(data$numeric_vars)
    selected <- if ("y" %in% data$numeric_vars) "y" else data$numeric_vars[1]
    selectInput("mbY", "Response Variable (Y):", choices = data$numeric_vars, selected = selected)
  })

  output$mbXSelect <- renderUI({
    req(data$numeric_vars)
    choices <- setdiff(data$numeric_vars, input$mbY %||% "")
    selected <- if (all(c("x1", "x2", "x3", "x4") %in% choices)) {
      c("x1", "x2", "x3", "x4")
    } else {
      choices[seq_len(min(4, length(choices)))]
    }
    selectizeInput("mbX", "Candidate Predictors:", choices = choices, selected = selected, multiple = TRUE)
  })

  output$mbFinalXSelect <- renderUI({
    choices <- input$mbX %||% setdiff(data$numeric_vars, input$mbY %||% "")
    res <- mb_results()
    selected <- choices
    if (!is.null(res) && length(res$stepwise$selected) > 0) selected <- res$stepwise$selected
    selectizeInput("mbFinalX", "Final model predictors:", choices = choices, selected = selected, multiple = TRUE)
  })

  observeEvent(input$runMB, {
    req(data$raw, input$mbY, input$mbX)
    if (length(input$mbX) < 1) {
      showNotification("Select at least one candidate predictor.", type = "error")
      return()
    }
    df_mb <- mb_complete_df(data$raw, input$mbY, input$mbX)
    if (nrow(df_mb) <= length(input$mbX) + 2) {
      showNotification("Not enough complete rows for this number of predictors.", type = "error")
      return()
    }
    full_fit <- lm(mb_formula(input$mbY, input$mbX), data = df_mb)
    all_tbl <- mb_all_possible(df_mb, input$mbY, input$mbX)
    forward <- mb_forward_select(df_mb, input$mbY, input$mbX, input$mbPEnter %||% 0.10)
    backward <- mb_backward_select(df_mb, input$mbY, input$mbX, input$mbPRemove %||% 0.15)
    stepwise <- mb_stepwise_select(df_mb, input$mbY, input$mbX, input$mbPEnter %||% 0.10, input$mbPRemove %||% 0.15)

    final_vars <- if (length(stepwise$selected) > 0) stepwise$selected else input$mbX
    final_fit <- lm(mb_formula(input$mbY, final_vars), data = df_mb)

    mb_results(list(
      df = df_mb,
      y_name = input$mbY,
      x_names = input$mbX,
      full_fit = full_fit,
      all_tbl = all_tbl,
      best_criteria = mb_pick_best(all_tbl),
      best_by_size = mb_best_by_size(all_tbl),
      forward = forward,
      backward = backward,
      stepwise = stepwise
    ))
    mb_final_model(final_fit)
    showNotification("Model building analysis completed.", type = "message")
  })

  observeEvent(input$runMBFinal, {
    req(data$raw, input$mbY, input$mbFinalX)
    df_mb <- mb_complete_df(data$raw, input$mbY, input$mbFinalX)
    fit <- lm(mb_formula(input$mbY, input$mbFinalX), data = df_mb)
    mb_final_model(fit)
    showNotification("Selected final model fitted.", type = "message")
  })

  mb_model_stats_ui <- function(fit) {
    sm <- summary(fit)
    tags$div(
      tags$div(
        class = "mb-metric-grid",
        tags$div(class = "mb-metric", tags$span("R-squared"), tags$strong(round(sm$r.squared, 4))),
        tags$div(class = "mb-metric", tags$span("Adjusted R-squared"), tags$strong(round(sm$adj.r.squared, 4))),
        tags$div(class = "mb-metric", tags$span("Residual MSE"), tags$strong(round(sm$sigma^2, 4))),
        tags$div(class = "mb-metric", tags$span("Predictors"), tags$strong(length(attr(terms(fit), "term.labels"))))
      ),
      tags$div(class = "mb-equation", mb_equation_text(fit))
    )
  }

  mb_coef_dt <- function(fit) {
    coef_tbl <- as.data.frame(coef(summary(fit)))
    coef_tbl$Term <- rownames(coef_tbl)
    rownames(coef_tbl) <- NULL
    names(coef_tbl) <- c("Estimate", "Std_Error", "t_Value", "P_Value", "Term")
    coef_tbl <- coef_tbl[, c("Term", "Estimate", "Std_Error", "t_Value", "P_Value")]
    dt <- DT::datatable(
      coef_tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(coef_tbl), scrollX = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = c("Estimate", "Std_Error", "t_Value", "P_Value"), digits = 5)
    DT::formatStyle(
      dt, "P_Value",
      color = DT::styleInterval(c(0.05), c("#047857", "#991b1b")),
      fontWeight = "700"
    )
  }

  mb_anova_dt <- function(fit) {
    an <- as.data.frame(anova(fit))
    an$Term <- rownames(an)
    rownames(an) <- NULL
    an <- an[, c("Term", setdiff(names(an), "Term"))]
    dt <- DT::datatable(
      an,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(an), scrollX = TRUE),
      class = "compact stripe hover"
    )
    DT::formatRound(dt, columns = names(an)[sapply(an, is.numeric)], digits = 5)
  }

  mb_vif_dt <- function(fit) {
    vif_vals <- tryCatch(car::vif(fit), error = function(e) NULL)
    if (is.null(vif_vals)) {
      tbl <- data.frame(Term = "VIF unavailable", VIF = NA_real_, Level = "N/A")
    } else {
      tbl <- data.frame(
        Term = names(vif_vals),
        VIF = as.numeric(vif_vals),
        Level = ifelse(vif_vals > 10, "Severe", ifelse(vif_vals >= 5, "Moderate", "Acceptable")),
        stringsAsFactors = FALSE
      )
    }
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(tbl), scrollX = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = "VIF", digits = 4)
    DT::formatStyle(
      dt, "Level",
      color = "white",
      fontWeight = "700",
      backgroundColor = DT::styleEqual(
        c("Acceptable", "Moderate", "Severe", "N/A"),
        c("#2563eb", "#f59e0b", "#ef4444", "#6b7280")
      )
    )
  }

  output$mbFullStats <- renderUI({
    req(mb_results())
    mb_model_stats_ui(mb_results()$full_fit)
  })

  output$mbFullCoefTbl <- DT::renderDT({
    req(mb_results())
    mb_coef_dt(mb_results()$full_fit)
  })

  output$mbFullAnovaTbl <- DT::renderDT({
    req(mb_results())
    mb_anova_dt(mb_results()$full_fit)
  })

  output$mbFullVifTbl <- DT::renderDT({
    req(mb_results())
    mb_vif_dt(mb_results()$full_fit)
  })

  output$mbCorrPlot <- renderPlot({
    req(mb_results())
    r <- mb_results()
    M <- stats::cor(r$df[, c(r$y_name, r$x_names), drop = FALSE], use = "pairwise.complete.obs")
    corrplot::corrplot.mixed(M,
      lower = "number", upper = "circle",
      tl.col = "black", tl.cex = 0.8, number.cex = 0.8,
      mar = c(1, 1, 1, 1)
    )
  })

  output$mbVifPlot <- renderPlot({
    req(mb_results())
    vif_vals <- tryCatch(car::vif(mb_results()$full_fit), error = function(e) NULL)
    if (is.null(vif_vals)) {
      plot.new()
      text(0.5, 0.5, "VIF unavailable for this model.")
      return()
    }
    vals <- as.numeric(vif_vals)
    names(vals) <- names(vif_vals)
    cols <- ifelse(vals > 10, "#ef4444", ifelse(vals >= 5, "#f59e0b", "steelblue"))
    barplot(vals,
      col = cols, ylab = "VIF", las = 2,
      main = "Variance Inflation Factors",
      ylim = c(0, max(10, vals, na.rm = TRUE) * 1.1)
    )
    abline(h = 5, col = "#f59e0b", lty = 2, lwd = 2)
    abline(h = 10, col = "#ef4444", lty = 2, lwd = 2)
  })

  output$mbAllModels <- DT::renderDT({
    req(mb_results())
    tbl <- mb_results()$all_tbl
    show_cols <- c(
      "Predictor_Count", "Predictors", "R_Squared", "Adj_R_Squared",
      "MSE", "Pred_MSE", "Pred_R_Squared", "Cp", "AIC", "BIC"
    )
    dt <- DT::datatable(
      tbl[, show_cols, drop = FALSE],
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      class = "compact stripe hover"
    )
    DT::formatRound(dt,
      columns = c("R_Squared", "Adj_R_Squared", "MSE", "Pred_MSE", "Pred_R_Squared", "Cp", "AIC", "BIC"),
      digits = 4
    )
  })

  output$mbBestCriteria <- DT::renderDT({
    req(mb_results())
    dt <- DT::datatable(
      mb_results()$best_criteria,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 8, scrollX = TRUE),
      class = "compact stripe hover"
    )
    DT::formatRound(dt, columns = "Value", digits = 4)
  })

  output$mbBestBySize <- DT::renderDT({
    req(mb_results())
    dt <- DT::datatable(
      mb_results()$best_by_size,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 8, scrollX = TRUE),
      class = "compact stripe hover"
    )
    DT::formatRound(dt,
      columns = c("Adj_R_Squared", "Pred_R_Squared", "MSE", "Cp", "AIC", "BIC"),
      digits = 4
    )
  })

  output$mbCpPlot <- renderPlot({
    req(mb_results())
    tbl <- mb_results()$all_tbl
    plot(tbl$p, tbl$Cp,
      pch = 19, col = "steelblue",
      xlab = "p = number of parameters including intercept",
      ylab = "Mallows Cp",
      main = "Cp against p"
    )
    text(tbl$p, tbl$Cp, labels = tbl$Predictors, pos = 4, cex = 0.7)
    abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
  })

  output$mbCriteriaPlot <- renderPlot({
    req(mb_results())
    tbl <- mb_results()$best_by_size
    op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
    on.exit(par(op))
    plot(tbl$Predictor_Count, tbl$Adj_R_Squared,
      type = "b", pch = 19, col = "#2563eb",
      xlab = "Number of predictors", ylab = "Adjusted R-squared",
      main = "Best adjusted R-squared by size"
    )
    plot(tbl$Predictor_Count, tbl$MSE,
      type = "b", pch = 19, col = "#16a34a",
      xlab = "Number of predictors", ylab = "MSE",
      main = "Best MSE by size"
    )
  })

  mb_selection_ui <- function(selection, rule_text) {
    steps <- selection$steps
    selected <- selection$selected
    final_text <- if (length(selected) == 0) "Intercept only" else paste(selected, collapse = " + ")

    if (is.null(steps) || nrow(steps) == 0) {
      step_tags <- tags$div(class = "hint-text", "No steps were recorded for this procedure.")
    } else {
      step_tags <- lapply(seq_len(nrow(steps)), function(i) {
        row <- steps[i, ]
        action_class <- switch(tolower(row$Action),
          add = "mb-action-add",
          remove = "mb-action-remove",
          stop = "mb-action-stop",
          "mb-action-stop"
        )
        p_txt <- if (is.na(row$P_Value)) "" else paste0("p = ", formatC(row$P_Value, format = "f", digits = 4))
        tags$div(
          class = "mb-step-row",
          tags$div(class = "mb-step-num", row$Step),
          tags$div(class = paste("mb-action", action_class), row$Action),
          tags$div(
            tags$div(class = "mb-step-predictor", htmltools::htmlEscape(row$Predictor)),
            tags$div(class = "hint-text", htmltools::htmlEscape(row$Decision))
          ),
          tags$div(class = "mb-step-p", p_txt)
        )
      })
    }

    tags$div(
      tags$div(class = "mb-rule-row", tags$span(class = "mb-pill", icon("filter"), rule_text)),
      tags$div(class = "mb-step-list", step_tags),
      tags$div(class = "mb-final-chip", tagList(icon("check"), " Final predictors: ", final_text))
    )
  }

  output$mbForwardTrace <- renderUI({
    req(mb_results())
    mb_selection_ui(mb_results()$forward, paste0("p-enter = ", input$mbPEnter %||% 0.10))
  })

  output$mbBackwardTrace <- renderUI({
    req(mb_results())
    mb_selection_ui(mb_results()$backward, paste0("p-remove = ", input$mbPRemove %||% 0.15))
  })

  output$mbStepwiseTrace <- renderUI({
    req(mb_results())
    mb_selection_ui(
      mb_results()$stepwise,
      paste0("p-enter = ", input$mbPEnter %||% 0.10, " | p-remove = ", input$mbPRemove %||% 0.15)
    )
  })

  mb_equation_text <- function(fit, digits = 4) {
    coefs <- stats::coef(fit)
    response <- as.character(formula(fit))[2]
    pieces <- sprintf("%.*f", digits, coefs[[1]])
    if (length(coefs) > 1) {
      term_names <- names(coefs)[-1]
      term_vals <- coefs[-1]
      term_pieces <- vapply(seq_along(term_vals), function(i) {
        sign <- if (term_vals[[i]] >= 0) "+" else "-"
        sprintf(" %s %.*f(%s)", sign, digits, abs(term_vals[[i]]), term_names[[i]])
      }, character(1))
      pieces <- paste0(pieces, paste(term_pieces, collapse = ""))
    }
    paste(response, "=", pieces)
  }

  output$mbFinalStats <- renderUI({
    req(mb_final_model())
    fit <- mb_final_model()
    sm <- summary(fit)
    tags$div(
      tags$div(
        class = "mb-metric-grid",
        tags$div(class = "mb-metric", tags$span("R-squared"), tags$strong(round(sm$r.squared, 4))),
        tags$div(class = "mb-metric", tags$span("Adjusted R-squared"), tags$strong(round(sm$adj.r.squared, 4))),
        tags$div(class = "mb-metric", tags$span("Residual MSE"), tags$strong(round(sm$sigma^2, 4))),
        tags$div(class = "mb-metric", tags$span("Predictors"), tags$strong(length(attr(terms(fit), "term.labels"))))
      ),
      tags$div(class = "mb-equation", mb_equation_text(fit))
    )
  })

  output$mbFinalCoefTbl <- DT::renderDT({
    req(mb_final_model())
    fit <- mb_final_model()
    coef_tbl <- as.data.frame(coef(summary(fit)))
    coef_tbl$Term <- rownames(coef_tbl)
    rownames(coef_tbl) <- NULL
    names(coef_tbl) <- c("Estimate", "Std_Error", "t_Value", "P_Value", "Term")
    coef_tbl <- coef_tbl[, c("Term", "Estimate", "Std_Error", "t_Value", "P_Value")]
    dt <- DT::datatable(
      coef_tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(coef_tbl), scrollX = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = c("Estimate", "Std_Error", "t_Value", "P_Value"), digits = 5)
    DT::formatStyle(
      dt, "P_Value",
      color = DT::styleInterval(c(0.05), c("#047857", "#991b1b")),
      fontWeight = "700"
    )
  })

  output$mbFinalAnovaTbl <- DT::renderDT({
    req(mb_final_model())
    an <- as.data.frame(anova(mb_final_model()))
    an$Term <- rownames(an)
    rownames(an) <- NULL
    an <- an[, c("Term", setdiff(names(an), "Term"))]
    dt <- DT::datatable(
      an,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(an), scrollX = TRUE),
      class = "compact stripe hover"
    )
    DT::formatRound(dt, columns = names(an)[sapply(an, is.numeric)], digits = 5)
  })

  output$mbFinalVifTbl <- DT::renderDT({
    req(mb_final_model())
    fit <- mb_final_model()
    vif_vals <- tryCatch(car::vif(fit), error = function(e) NULL)
    if (is.null(vif_vals)) {
      tbl <- data.frame(Term = "VIF unavailable", VIF = NA_real_, Level = "N/A")
    } else {
      tbl <- data.frame(
        Term = names(vif_vals),
        VIF = as.numeric(vif_vals),
        Level = ifelse(vif_vals > 10, "Severe", ifelse(vif_vals >= 5, "Moderate", "Acceptable")),
        stringsAsFactors = FALSE
      )
    }
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(tbl), scrollX = TRUE),
      class = "compact stripe hover"
    )
    dt <- DT::formatRound(dt, columns = "VIF", digits = 4)
    DT::formatStyle(
      dt, "Level",
      color = "white",
      fontWeight = "700",
      backgroundColor = DT::styleEqual(
        c("Acceptable", "Moderate", "Severe", "N/A"),
        c("#2563eb", "#f59e0b", "#ef4444", "#6b7280")
      )
    )
  })

  output$mbInterpretation <- renderUI({
    req(mb_results(), mb_final_model())
    r <- mb_results()
    fit <- mb_final_model()
    sm <- summary(fit)
    final_terms <- attr(terms(fit), "term.labels")
    rec <- r$best_criteria
    common <- sort(table(rec$Predictors), decreasing = TRUE)
    consensus <- names(common)[1]
    tags$div(
      style = "line-height:1.65;",
      tags$p(tags$b("Selected model: "), paste(final_terms, collapse = " + ")),
      tags$p(tags$b("Adjusted R-squared: "), round(sm$adj.r.squared, 4)),
      tags$p(tags$b("Residual MSE: "), round(sm$sigma^2, 4)),
      tags$p(tags$b("Most common criterion winner: "), consensus),
      tags$hr(),
      tags$p("Chapter caution: use the algorithm as a guide, then check subject knowledge, coefficient stability, multicollinearity, and residual diagnostics before declaring a final model.")
    )
  })

  output$mbFinalDiagnostics <- renderPlot({
    req(mb_final_model())
    fit <- mb_final_model()
    op <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 2))
    on.exit(par(op))
    plot(fitted(fit), resid(fit),
      pch = 19, col = "steelblue",
      xlab = "Fitted values", ylab = "Residuals",
      main = "Residuals vs Fitted"
    )
    abline(h = 0, col = "red", lwd = 2)
    qqnorm(resid(fit), pch = 19, col = "steelblue", main = "Normal QQ Plot")
    qqline(resid(fit), col = "red", lwd = 2)
    plot(seq_along(resid(fit)), resid(fit),
      type = "b", pch = 19, col = "steelblue",
      xlab = "Observation order", ylab = "Residuals",
      main = "Residuals vs Order"
    )
    abline(h = 0, col = "red", lwd = 2)
  })

  # ===== CHAPTER 11: GENERALIZED LINEAR MODELS SERVER =====

  glm_results <- reactiveVal(NULL)

  glm_bt <- function(x) paste0("`", gsub("`", "``", x), "`")

  glm_default_x <- function(y_name, choices) {
    if (identical(y_name, "wellness") && "work" %in% choices) return("work")
    if (identical(y_name, "admit")) {
      wanted <- intersect(c("gre", "gpa", "rank"), choices)
      if (length(wanted) > 0) return(wanted)
    }
    if (identical(y_name, "num_awards")) {
      wanted <- intersect(c("prog", "math"), choices)
      if (length(wanted) > 0) return(wanted)
    }
    head(choices, min(2, length(choices)))
  }

  output$glmYSelect <- renderUI({
    req(names(data$raw))
    fam <- input$glmFamily %||% "logit"
    candidates <- names(data$raw)
    selected <- switch(fam,
      logit = if ("wellness" %in% candidates) "wellness" else if ("admit" %in% candidates) "admit" else candidates[1],
      probit = if ("wellness" %in% candidates) "wellness" else if ("admit" %in% candidates) "admit" else candidates[1],
      poisson = if ("num_awards" %in% candidates) "num_awards" else candidates[1]
    )
    selectInput("glmY", NULL, choices = candidates, selected = selected)
  })

  output$glmXSelect <- renderUI({
    req(names(data$raw), input$glmY)
    choices <- setdiff(names(data$raw), input$glmY)
    selected <- glm_default_x(input$glmY, choices)
    selectizeInput("glmX", NULL, choices = choices, selected = selected, multiple = TRUE)
  })

  observeEvent(input$glmFamily, {
    req(names(data$raw))
    all_names <- names(data$raw)
    if (isTRUE(input$glmFamily %in% c("logit", "probit"))) {
      y_sel <- if ("wellness" %in% all_names) "wellness" else if ("admit" %in% all_names) "admit" else all_names[1]
    } else {
      y_sel <- if ("num_awards" %in% all_names) "num_awards" else all_names[1]
    }
    updateSelectInput(session, "glmY", choices = all_names, selected = y_sel)
  }, ignoreInit = TRUE)

  observeEvent(input$glmY, {
    req(names(data$raw), input$glmY)
    x_choices <- setdiff(names(data$raw), input$glmY)
    updateSelectizeInput(
      session, "glmX",
      choices = x_choices,
      selected = glm_default_x(input$glmY, x_choices),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$runGLM, {
    req(data$raw, input$glmY, input$glmX)
    if (length(input$glmX) == 0) {
      showNotification("Select at least one predictor for the GLM.", type = "error")
      return()
    }

    fam <- input$glmFamily %||% "logit"
    df <- as.data.frame(data$raw[, c(input$glmY, input$glmX), drop = FALSE], check.names = FALSE)
    df <- df[complete.cases(df), , drop = FALSE]
    if (nrow(df) < 10) {
      showNotification("Need at least 10 complete observations for this GLM.", type = "error")
      return()
    }

    y_name <- input$glmY
    x_names <- input$glmX

    for (nm in x_names) {
      if (nm %in% c("rank", "prog")) {
        df[[nm]] <- factor(df[[nm]])
      } else if (!is.numeric(df[[nm]])) {
        df[[nm]] <- factor(df[[nm]])
      }
    }

    if (fam %in% c("logit", "probit")) {
      y_vals <- df[[y_name]]
      uniq <- sort(unique(y_vals))
      if (!all(uniq %in% c(0, 1))) {
        if (length(uniq) == 2) {
          df[[y_name]] <- as.numeric(factor(y_vals)) - 1
        } else {
          showNotification("Binary GLMs require a response coded with two levels.", type = "error")
          return()
        }
      }
    } else {
      y_vals <- df[[y_name]]
      if (any(y_vals < 0, na.rm = TRUE) || any(abs(y_vals - round(y_vals)) > 1e-8, na.rm = TRUE)) {
        showNotification("Poisson regression requires a non-negative integer count response.", type = "error")
        return()
      }
    }

    glm_formula <- as.formula(paste(glm_bt(y_name), "~", paste(vapply(x_names, glm_bt, character(1)), collapse = " + ")))
    fam_obj <- switch(fam,
      logit = binomial(link = "logit"),
      probit = binomial(link = "probit"),
      poisson = poisson(link = "log")
    )
    fit <- glm(glm_formula, family = fam_obj, data = df)

    companion <- NULL
    if (fam %in% c("logit", "probit")) {
      other_fam <- if (fam == "logit") binomial(link = "probit") else binomial(link = "logit")
      companion <- glm(glm_formula, family = other_fam, data = df)
    }

    fitted_vals <- fitted(fit)
    perf_value <- if (fam %in% c("logit", "probit")) {
      mean((fitted_vals >= 0.5) == df[[y_name]])
    } else {
      sqrt(mean((df[[y_name]] - fitted_vals)^2))
    }
    perf_label <- if (fam %in% c("logit", "probit")) "Accuracy" else "RMSE"

    coef_mat <- summary(fit)$coefficients
    coef_tbl <- data.frame(
      Term = rownames(coef_mat),
      Estimate = coef_mat[, "Estimate"],
      Std_Error = coef_mat[, "Std. Error"],
      Z_Value = coef_mat[, "z value"],
      P_Value = coef_mat[, "Pr(>|z|)"],
      Effect = exp(coef_mat[, "Estimate"]),
      check.names = FALSE
    )
    names(coef_tbl)[6] <- if (fam %in% c("logit", "probit")) "Odds Ratio" else "Incidence Rate Ratio"

    pred_tbl <- cbind(df[, c(y_name, x_names), drop = FALSE], Fitted = fitted_vals)
    pred_tbl <- pred_tbl[seq_len(min(12, nrow(pred_tbl))), , drop = FALSE]

    compare_tbl <- if (is.null(companion)) {
      data.frame(
        Model = "Poisson",
        AIC = AIC(fit),
        Null_Deviance = fit$null.deviance,
        Residual_Deviance = fit$deviance,
        Pseudo_R2 = 1 - fit$deviance / fit$null.deviance,
        check.names = FALSE
      )
    } else {
      data.frame(
        Model = c(if (fam == "logit") "Logit" else "Probit", if (fam == "logit") "Probit" else "Logit"),
        AIC = c(AIC(fit), AIC(companion)),
        Null_Deviance = c(fit$null.deviance, companion$null.deviance),
        Residual_Deviance = c(fit$deviance, companion$deviance),
        Pseudo_R2 = c(1 - fit$deviance / fit$null.deviance, 1 - companion$deviance / companion$null.deviance),
        check.names = FALSE
      )
    }

    glm_results(list(
      family = fam,
      fit = fit,
      companion = companion,
      data = df,
      y_name = y_name,
      x_names = x_names,
      coef_tbl = coef_tbl,
      pred_tbl = pred_tbl,
      compare_tbl = compare_tbl,
      perf_value = perf_value,
      perf_label = perf_label
    ))

    showNotification("GLM analysis completed.", type = "message")
  })

  output$glmObsBox <- renderValueBox({
    req(glm_results())
    valueBox(nrow(glm_results()$data), "Usable Observations", icon = icon("table"), color = "blue")
  })

  output$glmAICBox <- renderValueBox({
    req(glm_results())
    valueBox(sprintf("%.2f", AIC(glm_results()$fit)), "AIC", icon = icon("balance-scale"), color = "teal")
  })

  output$glmDevBox <- renderValueBox({
    req(glm_results())
    valueBox(sprintf("%.2f", glm_results()$fit$deviance), "Residual Deviance", icon = icon("chart-line"), color = "yellow")
  })

  output$glmPerfBox <- renderValueBox({
    req(glm_results())
    valueBox(sprintf("%.3f", glm_results()$perf_value), glm_results()$perf_label, icon = icon("bullseye"), color = "green")
  })

  output$glmSummary <- renderPrint({
    req(glm_results())
    print(summary(glm_results()$fit))
  })

  output$glmCoefTable <- renderDT({
    req(glm_results())
    tbl <- glm_results()$coef_tbl
    num_cols <- setdiff(names(tbl), "Term")
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) round(x, 4))
    datatable(tbl, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$glmFitPlot <- renderPlot({
    req(glm_results())
    r <- glm_results()
    df <- r$data
    x_num <- r$x_names[vapply(df[r$x_names], is.numeric, logical(1))]

    if (length(r$x_names) == 1 && length(x_num) == 1) {
      xnm <- x_num[1]
      xgrid <- seq(min(df[[xnm]]), max(df[[xnm]]), length.out = 200)
      newdata <- setNames(data.frame(xgrid), xnm)
      preds <- predict(r$fit, newdata = newdata, type = "response")
      plot(
        df[[xnm]], df[[r$y_name]],
        xlab = xnm, ylab = r$y_name,
        pch = 19, col = "darkblue",
        main = "Observed Response and Fitted Curve"
      )
      lines(xgrid, preds, col = "darkgreen", lwd = 2)
      if (!is.null(r$companion)) {
        preds2 <- predict(r$companion, newdata = newdata, type = "response")
        lines(xgrid, preds2, col = "darkorange", lwd = 2, lty = 2)
        legend("topleft", legend = c("Primary fit", "Companion fit"), col = c("darkgreen", "darkorange"), lwd = 2, lty = c(1, 2), bty = "n")
      }
    } else {
      plot(
        seq_len(nrow(df)), df[[r$y_name]],
        xlab = "Observation", ylab = r$y_name,
        pch = 19, col = "darkblue",
        main = "Observed vs Fitted"
      )
      points(seq_len(nrow(df)), fitted(r$fit), pch = 17, col = "darkgreen")
      legend("topleft", legend = c("Observed", "Fitted"), col = c("darkblue", "darkgreen"), pch = c(19, 17), bty = "n")
    }
  })

  output$glmPerformanceText <- renderPrint({
    req(glm_results())
    r <- glm_results()
    cat("Model notes\n")
    cat(strrep("=", 28), "\n")
    cat("Family:", switch(r$family, logit = "Logit", probit = "Probit", poisson = "Poisson"), "\n")
    cat("Formula:", deparse(formula(r$fit)), "\n")
    cat(sprintf("AIC: %.4f\n", AIC(r$fit)))
    cat(sprintf("Null deviance: %.4f\n", r$fit$null.deviance))
    cat(sprintf("Residual deviance: %.4f\n", r$fit$deviance))
    if (r$family %in% c("logit", "probit")) {
      cat(sprintf("Classification accuracy at 0.5 cutoff: %.4f\n", r$perf_value))
      cat("Binary coefficients are interpreted on the log-odds or latent-normal scale, and exponentiating them gives multiplicative changes in odds.\n")
    } else {
      cat(sprintf("Root mean squared error: %.4f\n", r$perf_value))
      cat("Poisson coefficients are interpreted on the log-count scale, and exponentiating them gives multiplicative changes in expected count.\n")
    }
  })

  output$glmCompareTable <- renderDT({
    req(glm_results())
    tbl <- glm_results()$compare_tbl
    num_cols <- setdiff(names(tbl), "Model")
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) round(x, 4))
    datatable(tbl, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$glmPredTable <- renderDT({
    req(glm_results())
    tbl <- glm_results()$pred_tbl
    for (nm in names(tbl)) {
      if (is.numeric(tbl[[nm]])) tbl[[nm]] <- round(tbl[[nm]], 4)
    }
    datatable(tbl, rownames = FALSE, options = list(pageLength = min(10, nrow(tbl)), scrollX = TRUE))
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
      options = list(placeholder = "Optional")
    )
  })

  observeEvent(input$applyCleaning, {
    req(data$raw_base)
    withProgress(message = "Applying data cleaning...", value = 0, {
      incProgress(0.4)
      data$raw <- apply_cleaning_pipeline(data$raw_base,
        missing_mode = input$cleanMissing %||% "keep",
        outlier_mode = input$cleanOutliers %||% "remove",
        log_vars = input$cleanLogVars %||% NULL
      )
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
      choices = c(stats::setNames("", ""), all_vars), selected = "",
      multiple = FALSE,
      options = list(
        placeholder = "Find variable",
        allowEmptyOption = TRUE
      )
    )
  })

  observeEvent(input$globalVarSearch, ignoreInit = TRUE, {
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

  observeEvent(input$darkModeToggle,
    {
      session$sendCustomMessage("setDarkMode", list(enabled = isTRUE(input$darkModeToggle)))
    }
  )

  # Quick actions
  observeEvent(input$qaSample, {
    data$raw_base <- default_data
    data$raw <- apply_cleaning_pipeline(data$raw_base,
      missing_mode = input$cleanMissing %||% "keep",
      outlier_mode = input$cleanOutliers %||% "remove",
      log_vars = input$cleanLogVars %||% NULL
    )
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

  observeEvent(input$loadSampleSLR, {
    tryCatch(
      load_course_dataset("P1_RocketPropellant.xlsx", "slr", "P1 Rocket Propellant sample data loaded for SLR."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleMLR, {
    tryCatch(
      load_course_dataset("P2_DeliveryTime.xlsx", "mlr", "P2 Delivery Time sample data loaded for MLR."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSamplePolyP5, {
    tryCatch(
      load_course_dataset("P5_HardWood.xlsx", "polyreg", "P5 HardWood sample data loaded for polynomial regression."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSamplePolyVoltage, {
    tryCatch(
      load_course_dataset("VoltageDrop.xlsx", "polyreg", "Voltage Drop sample data loaded for spline regression."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleInfluence, {
    tryCatch(
      load_course_dataset("P4_RealEstate.xlsx", "influence", "P4 Real Estate sample data loaded for leverage and influence diagnostics."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleGLMWellness, {
    tryCatch(
      load_course_dataset(
        "Wellness.xlsx", "glm", "Wellness sample data loaded for binary GLM analysis.",
        post_load = function() updateSelectInput(session, "glmFamily", selected = "logit")
      ),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleGLMAdmit, {
    tryCatch(
      load_course_dataset(
        "Admit.xlsx", "glm", "Admit sample data loaded for binary GLM analysis.",
        post_load = function() updateSelectInput(session, "glmFamily", selected = "logit")
      ),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleGLMAwards, {
    tryCatch(
      load_course_dataset(
        "Awards.xlsx", "glm", "Awards sample data loaded for Poisson GLM analysis.",
        post_load = function() updateSelectInput(session, "glmFamily", selected = "poisson")
      ),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleMA, {
    tryCatch(
      load_course_dataset("P2_DeliveryTime.xlsx", "modeladequacy", "P2 Delivery Time sample data loaded for model adequacy checks."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleIndicator, {
    tryCatch(
      load_course_dataset("P11_ToolLife.xlsx", "indicator", "P11 Tool Life sample data loaded for indicator-variable analysis."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleCh9, {
    tryCatch(
      load_course_dataset("Aacetylene.xlsx", "ch9multi", "Acetylene sample data loaded for multicollinearity analysis."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleMB, {
    tryCatch(
      load_course_dataset(
        "P3_HaldsCement.xlsx",
        "modelbuilding",
        "P3 Hald's Cement sample data loaded for model building.",
        post_load = function() {
          mb_results(NULL)
          mb_final_model(NULL)
        }
      ),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleBTUtility, {
    tryCatch(
      load_course_dataset("P8_ElectricUtility.xlsx", "boxtrans", "Electric Utility sample data loaded."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleBTWind, {
    tryCatch(
      load_course_dataset("P9_WindMill.xlsx", "boxtrans", "Wind Mill sample data loaded."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleBTIncome, {
    tryCatch(
      load_course_dataset("Income.xlsx", "boxtrans", "Income sample data loaded for corrective-model checks."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  observeEvent(input$loadSampleWLS, {
    tryCatch(
      load_course_dataset("Income.xlsx", "wls", "Income sample data loaded for WLS."),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
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
    if (isTRUE(onboarding_shown())) {
      return()
    }
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
      selected = "__none__"
    )
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

  ind_model <- reactiveVal(NULL)
  ind_reduced <- reactiveVal(NULL)
  ind_data <- reactiveVal(NULL)

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
      } else {
        lm(y ~ 1, data = df)
      }
    } else {
      if (use_x) {
        if (isTRUE(input$indInteraction)) lm(y ~ x + f1 + f2, data = df) else lm(y ~ x, data = df)
      } else {
        lm(y ~ 1, data = df)
      }
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
        for (a in lv1) {
          for (b in lv2) {
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
          pch = 19, col = cols[as.character(grp)]
        )
        x_grid <- seq(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE), length.out = 120)
        for (g in levels(grp)) {
          idx <- as.character(grp) == g
          if (!any(idx)) next
          if (use_f2) {
            vals <- strsplit(g, " \\| ")[[1]]
            new_df <- data.frame(
              x = x_grid,
              f1 = factor(vals[1], levels = levels(df$f1)),
              f2 = factor(vals[2], levels = levels(df$f2))
            )
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
          las = 2
        )
      }
    })

    output$indInterpretation <- renderUI({
      p_base <- if (!use_f2) {
        paste0("One factor selected: ", nlevels(df$f1) - 1, " dummy variable(s) created for Factor 1.")
      } else {
        paste0(
          "Two factors selected: ", nlevels(df$f1) - 1, " dummy variable(s) for Factor 1 and ",
          nlevels(df$f2) - 1, " for Factor 2."
        )
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
      tags$p(
        class = "hint-text",
        icon("info-circle"), " Uses the SLR model fitted in the SLR tab."
      )
    } else {
      tags$p(
        class = "hint-text",
        icon("info-circle"), " Uses the MLR model fitted in the MLR tab."
      )
    }
  })

  observeEvent(input$runMA, {
    # Grab the currently fitted model
    mod <- if (input$maModelType == "slr") slr_model() else mlr_model()
    req(mod)

    ei <- resid(mod)
    ti <- MASS::studres(mod)
    n <- length(ei)

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
      car::qqPlot(ei,
        main = "QQ Plot (Raw Residuals)",
        ylab = "Residuals", col = "steelblue", col.lines = "red"
      )
      car::qqPlot(ti,
        main = "QQ Plot (Studentised Residuals)",
        ylab = "Studentised Residuals", col = "darkorange", col.lines = "red"
      )
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
        col = "steelblue", pch = 19
      )
      abline(h = 0, col = "red", lwd = 2)
      plot(fitted(mod), ti,
        xlab = "Fitted values", ylab = "Studentised Residuals",
        main = "(b) Studentised Residuals vs Fitted",
        col = "darkorange", pch = 19
      )
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
        main = "(a) Residuals vs Order"
      )
      abline(h = 0, col = "red", lwd = 2)
      plot(seq_len(n), ti,
        type = "o", pch = 19, col = "darkorange",
        xlab = "Observation Order", ylab = "Studentised Residuals",
        main = "(b) Studentised vs Order"
      )
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
        car::avPlots(mod,
          main = "Added-Variable (Partial Regression) Plots",
          col = "steelblue", col.lines = "red", pch = 19
        ),
        error = function(e) {
          plot.new()
          text(0.5, 0.5,
            paste("Added-variable plots require >= 2 predictors.\n", e$message),
            cex = 1.2
          )
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
      tryCatch(
        {
          # Re-extract x and y from the model
          mf <- model.frame(mod)
          y_lof <- mf[[1]]
          x_lof <- mf[[2]]
          lof_mod <- lm(y_lof ~ x_lof)
          print(EnvStats::anovaPE(lof_mod))
          cat("\nNote: This test requires replicate X values.\n")
          cat("If X values are all unique, the test cannot be performed.\n")
        },
        error = function(e) {
          cat("Lack-of-fit test could not be completed:\n")
          cat(e$message, "\n")
          cat("\nThis test requires replicate X values in your dataset.\n")
        }
      )
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

  bt_orig_model <- reactiveVal(NULL)
  bt_trans_model <- reactiveVal(NULL)
  bt_info <- reactiveVal(list())

  observeEvent(input$runBT, {
    req(data$raw, input$btY, input$btX)

    y_bt <- data$raw[[input$btY]]
    x_bt <- data$raw[[input$btX]]

    # Check Y > 0 for Box-Cox
    if (input$btMethod == "boxcox" && any(y_bt <= 0, na.rm = TRUE)) {
      showNotification(
        "Box-Cox requires all Y values to be strictly positive. Please choose a different Y.",
        type = "error", duration = 6
      )
      return()
    }
    # Check X > 0 for Box-Tidwell
    if (input$btMethod == "boxtidwell" && any(x_bt <= 0, na.rm = TRUE)) {
      showNotification(
        "Box-Tidwell requires all X values to be strictly positive. Please choose a different X.",
        type = "error", duration = 6
      )
      return()
    }

    df_bt <- data.frame(y = y_bt, x = x_bt)
    mod_orig <- lm(y ~ x, data = df_bt)
    bt_orig_model(mod_orig)

    # --- Original model outputs ---
    output$btOrigScatter <- renderPlot({
      plot(x_bt, y_bt,
        pch = 21, cex = 1, col = "steelblue",
        xlab = input$btX, ylab = input$btY,
        main = paste("Original Data:", input$btY, "vs", input$btX)
      )
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
        col = "steelblue", pch = 19
      )
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
      bcox <- MASS::boxcox(y_bt ~ x_bt, lambda = seq(-2, 2, 0.001), plotit = FALSE)
      lambda_opt <- bcox$x[which.max(bcox$y)]
      bt_info(list(
        method = "boxcox", param = lambda_opt,
        param_name = "lambda", x = bcox$x, y = bcox$y
      ))

      output$btLambdaPlot <- renderPlot({
        plot(bcox$x, bcox$y,
          type = "l", lwd = 2, col = "steelblue",
          xlab = expression(lambda), ylab = "Log-Likelihood",
          main = "Box-Cox Log-Likelihood Profile"
        )
        abline(v = lambda_opt, col = "red", lwd = 2, lty = 2)
        abline(
          h = max(bcox$y) - qchisq(0.95, 1) / 2,
          col = "grey50", lty = 3, lwd = 1.5
        )
        legend("bottomright",
          legend = c(
            paste("Optimal lambda =", round(lambda_opt, 3)),
            "95% CI threshold"
          ),
          col = c("red", "grey50"), lty = c(2, 3), lwd = 2, bty = "n"
        )
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
        ci_idx <- bcox$y >= ci_threshold
        cat(
          "\n95% CI for lambda: [",
          round(min(bcox$x[ci_idx]), 3), ",",
          round(max(bcox$x[ci_idx]), 3), "]\n"
        )
      })

      # Transformed model (Y^lambda)
      y_trans <- if (abs(lambda_opt) < 0.001) log(y_bt) else y_bt^lambda_opt
      df_trans <- data.frame(y_trans = y_trans, x = x_bt)
      mod_trans <- lm(y_trans ~ x, data = df_trans)
      bt_trans_model(mod_trans)
    } else {
      # Box-Tidwell: find optimal alpha
      bt_result <- tryCatch(
        car::boxTidwell(y ~ x, data = df_bt),
        error = function(e) NULL
      )

      if (is.null(bt_result)) {
        showNotification("Box-Tidwell did not converge. Try different variables.",
          type = "warning"
        )
        return()
      }

      alpha_opt <- bt_result$result[1, "MLE of lambda"]
      bt_info(list(
        method = "boxtidwell", param = alpha_opt,
        param_name = "alpha", bt_obj = bt_result
      ))

      output$btLambdaPlot <- renderPlot({
        # Box-Tidwell doesn't produce a profile plot; show reciprocal transform
        # as illustration
        alpha_seq <- seq(-3, 3, length.out = 200)
        # Approximate residual log-lik proxy: SS from a grid of transformed models
        ss_vec <- sapply(alpha_seq, function(a) {
          xt <- tryCatch(x_bt^a, error = function(e) rep(NA, length(x_bt)))
          if (any(!is.finite(xt))) {
            return(NA_real_)
          }
          m <- lm(y_bt ~ xt)
          sum(resid(m)^2)
        })
        ss_vec[!is.finite(ss_vec)] <- NA
        plot(alpha_seq, -ss_vec,
          type = "l", lwd = 2, col = "steelblue",
          xlab = expression(alpha),
          ylab = "-Residual SS (proxy for log-likelihood)",
          main = "Box-Tidwell Power Profile", na.action = na.omit
        )
        abline(v = alpha_opt, col = "red", lwd = 2, lty = 2)
        legend("bottomright",
          legend = paste("Optimal alpha =", round(alpha_opt, 3)),
          col = "red", lty = 2, lwd = 2, bty = "n"
        )
      })

      output$btOptimal <- renderPrint({
        cat("Box-Tidwell Transformation\n")
        cat(strrep("=", 35), "\n\n")
        cat("Optimal alpha:", round(alpha_opt, 4), "\n\n")
        cat("Full Box-Tidwell output:\n")
        print(bt_result)
      })

      # Transformed model (X^alpha)
      x_trans <- x_bt^alpha_opt
      df_trans <- data.frame(y = y_bt, x_trans = x_trans)
      mod_trans <- lm(y ~ x_trans, data = df_trans)
      bt_trans_model(mod_trans)
    }

    # --- Transformed model outputs ---
    mod_t <- bt_trans_model()

    output$btTransScatter <- renderPlot({
      mf_t <- model.frame(mod_t)
      x_t <- mf_t[[2]]
      y_t <- mf_t[[1]]
      info <- bt_info()
      x_lab <- if (info$method == "boxcox") {
        input$btX
      } else {
        paste0(input$btX, "^", round(info$param, 3))
      }
      y_lab <- if (info$method == "boxcox") {
        if (abs(info$param) < 0.001) {
          paste0("log(", input$btY, ")")
        } else {
          paste0(input$btY, "^", round(info$param, 3))
        }
      } else {
        input$btY
      }
      plot(x_t, y_t,
        pch = 21, cex = 1, col = "darkorange",
        xlab = x_lab, ylab = y_lab,
        main = "Transformed Data with Fit"
      )
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
        col = "darkorange", pch = 19
      )
      abline(h = 0, col = "red", lwd = 2)
      lines(lowess(fitted(mod_t), resid(mod_t)), col = "blue", lwd = 2, lty = 2)
    })

    output$btTransBP <- renderPrint({
      cat("Breusch-Pagan Test (Transformed)\n")
      cat(strrep("=", 35), "\n\n")
      bp <- tryCatch(lmtest::bptest(mod_t),
        error = function(e) NULL
      )
      if (is.null(bp)) {
        cat("Could not compute BP test.\n")
        return()
      }
      print(bp)
    })

    output$btComparison <- renderPlot({
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      # Original
      plot(fitted(mod_orig), resid(mod_orig),
        xlab = "Fitted", ylab = "Residuals",
        main = "Original Model", col = "steelblue", pch = 19
      )
      abline(h = 0, col = "red", lwd = 2)
      # Transformed
      plot(fitted(mod_t), resid(mod_t),
        xlab = "Fitted", ylab = "Residuals",
        main = "Transformed Model", col = "darkorange", pch = 19
      )
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
      choices = c(
        "Auto-estimate (1/|residual|)" = "__auto__",
        data$numeric_vars
      )
    )
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
    bp_ols <- lmtest::bptest(mod_ols)

    output$wlsOLSScatter <- renderPlot({
      plot(x_wls, y_wls,
        pch = 21, cex = 1, col = "steelblue",
        xlab = input$wlsX, ylab = input$wlsY,
        main = paste("OLS:", input$wlsY, "vs", input$wlsX)
      )
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
        col = "steelblue", pch = 19
      )
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
          type = "warning"
        )
        w_vals <- 1 / (abs(resid(mod_ols)) + 1e-8)
      } else {
        w_vals <- 1 / w_raw
      }
    } else {
      # Auto: weights = 1 / |OLS residuals|
      w_vals <- 1 / (abs(resid(mod_ols)) + 1e-8)
    }

    w_vals <- w_vals / mean(w_vals) # normalise
    df_wls$w <- w_vals

    # WLS fit
    mod_wls <- lm(y ~ x, data = df_wls, weights = w)
    wls_wls_model(mod_wls)
    bp_wls <- lmtest::bptest(mod_wls)

    output$wlsWLSScatter <- renderPlot({
      plot(x_wls, y_wls,
        pch = 21, cex = sqrt(w_vals / max(w_vals)) * 2.5 + 0.3,
        col = "darkorange",
        xlab = input$wlsX, ylab = input$wlsY,
        main = paste(
          "WLS:", input$wlsY, "vs", input$wlsX,
          "(point size ~ weight)"
        )
      )
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
        col = "darkorange", pch = 19
      )
      abline(h = 0, col = "red", lwd = 2)
      lines(lowess(w_sqrt * fitted(mod_wls), w_sqrt * resid(mod_wls)),
        col = "blue", lwd = 2, lty = 2
      )
    })

    output$wlsComparison <- renderPlot({
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      plot(fitted(mod_ols), resid(mod_ols),
        xlab = expression(hat(y)[i]), ylab = expression(e[i]),
        main = "OLS Residuals", col = "steelblue", pch = 19
      )
      abline(h = 0, col = "red", lwd = 2)
      w_sqrt <- sqrt(w_vals)
      plot(w_sqrt * fitted(mod_wls), w_sqrt * resid(mod_wls),
        xlab = expression(sqrt(w[i]) * hat(y)[i]),
        ylab = expression(sqrt(w[i]) * e[i]),
        main = "WLS Weighted Residuals", col = "darkorange", pch = 19
      )
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
      cat("\nOLS R-squared:", round(summary(mod_ols)$r.squared, 4), "\n")
      cat("WLS R-squared:", round(summary(mod_wls)$r.squared, 4), "\n")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
