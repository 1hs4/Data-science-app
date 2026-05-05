##########################################################
### ModelCraft: Statistical Learning in R
### Works with Any Dataset - CRD Analysis
##########################################################

library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(moments)
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
  data_analysis = c("upload", "preview", "varanalysis", "catsuite", "tests"),
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
          document.body.classList.add('dark-mode-pro');
          var darkToggle = document.getElementById('darkModeToggle');
          if (darkToggle) {
            darkToggle.checked = true;
          }

          var activeProxy = document.querySelector('.detox-tab-proxy li.active a[data-value]');
          detoxSyncNav(activeProxy ? activeProxy.getAttribute('data-value') : 'upload');

          var toggle = document.querySelector('.js-detox-menu-toggle');
          if (toggle) {
            toggle.addEventListener('click', function() {
              if (window.matchMedia('(max-width: 992px)').matches) {
                document.body.classList.toggle('detox-mobile-open');
              } else {
                document.body.classList.toggle('detox-nav-collapsed');
              }
            });
          }

          window.addEventListener('resize', function() {
            if (!window.matchMedia('(max-width: 992px)').matches) {
              document.body.classList.remove('detox-mobile-open');
            }
          });

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
          --bg: #020617;
          --surface: #0f172a;
          --surface-soft: #111c30;
          --ink: #e5e7eb;
          --muted: #94a3b8;
          --primary: #38bdf8;
          --primary-dark: #2563eb;
          --border: #26364d;
          --radius: 14px;
        }
        body, h1, h2, h3, h4, h5, h6, .content, .main-header .logo, .main-header .navbar, .sidebar-menu > li > a {
          font-family: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
          color: var(--ink);
        }
        .content-wrapper, .right-side {
          background: radial-gradient(circle at 12% 0%, #10233d 0%, #07111f 34%, #020617 100%);
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
          color: var(--ink);
          box-shadow: 0 18px 38px rgba(2, 6, 23, 0.34);
          overflow: visible;
        }
        .box-header {
          border-bottom: 1px solid var(--border);
          background: linear-gradient(180deg, #111c30, #0f172a);
        }
        .box.box-primary .box-header { border-left: 5px solid #2563eb; }
        .box.box-info .box-header { border-left: 5px solid #0891b2; }
        .box.box-success .box-header { border-left: 5px solid #16a34a; }
        .box.box-warning .box-header { border-left: 5px solid #ea580c; }
        .box.box-danger .box-header { border-left: 5px solid #dc2626; }
        .small-box, .info-box {
          border-radius: 12px;
          box-shadow: 0 14px 28px rgba(2, 6, 23, 0.28);
        }
        h2 { color: #93c5fd; font-weight: 700; letter-spacing: -.02em; }
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
          border: 1px solid #334155 !important;
          min-height: 44px;
          background: #111827;
          color: #e5e7eb;
          box-shadow: none;
        }
        .selectize-dropdown {
          z-index: 10050 !important;
        }
        .selectize-input { font-size: 14px; padding-top: 10px; padding-bottom: 10px; }
        .control-label { font-weight: 600; color: #e5e7eb; }
        .tab-content { padding-top: 10px; }
        .nav-tabs-custom { border-radius: 12px; overflow: hidden; box-shadow: 0 4px 12px rgba(15,23,42,.06); }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #2563eb;
        }
        .nav-tabs-custom > .nav-tabs > li > a { color: #cbd5e1; font-weight: 600; }
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
        body.dark-mode-pro input,
        body.dark-mode-pro textarea,
        body.dark-mode-pro select,
        body.dark-mode-pro .input-group-addon,
        body.dark-mode-pro .selectize-control.single .selectize-input,
        body.dark-mode-pro .selectize-control.multi .selectize-input,
        body.dark-mode-pro .selectize-control.multi .selectize-input > div {
          background: #111827 !important;
          color: #e5e7eb !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro .box-title,
        body.dark-mode-pro .box-body,
        body.dark-mode-pro .box-footer,
        body.dark-mode-pro label,
        body.dark-mode-pro .checkbox label,
        body.dark-mode-pro .radio label,
        body.dark-mode-pro .shiny-input-container,
        body.dark-mode-pro .well,
        body.dark-mode-pro .modal-content,
        body.dark-mode-pro .dropdown-menu {
          color: #e5e7eb !important;
        }
        body.dark-mode-pro .well,
        body.dark-mode-pro .modal-content,
        body.dark-mode-pro .dropdown-menu {
          background: #0f172a !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro .btn-default,
        body.dark-mode-pro .btn-file,
        body.dark-mode-pro .fileinput-button {
          background: #1e293b !important;
          color: #e5e7eb !important;
          border-color: #475569 !important;
        }
        body.dark-mode-pro .btn-primary,
        body.dark-mode-pro .btn-info {
          background: linear-gradient(135deg, #0ea5e9, #2563eb) !important;
          border-color: transparent !important;
          color: #ffffff !important;
        }
        body.dark-mode-pro .btn-success {
          background: linear-gradient(135deg, #059669, #047857) !important;
          border-color: transparent !important;
          color: #ffffff !important;
        }
        body.dark-mode-pro .btn-warning {
          background: linear-gradient(135deg, #d97706, #b45309) !important;
          border-color: transparent !important;
          color: #ffffff !important;
        }
        body.dark-mode-pro table,
        body.dark-mode-pro .table,
        body.dark-mode-pro .table-striped > tbody > tr:nth-of-type(odd) {
          background: transparent !important;
          color: #e5e7eb !important;
        }
        body.dark-mode-pro .table > thead > tr > th,
        body.dark-mode-pro .table > tbody > tr > th,
        body.dark-mode-pro .table > tfoot > tr > th,
        body.dark-mode-pro .table > thead > tr > td,
        body.dark-mode-pro .table > tbody > tr > td,
        body.dark-mode-pro .table > tfoot > tr > td {
          border-color: #243244 !important;
        }
        body.dark-mode-pro .nav-tabs,
        body.dark-mode-pro .nav-tabs-custom > .nav-tabs {
          background: #0b1220 !important;
          border-color: #334155 !important;
        }
        body.dark-mode-pro .nav-tabs > li > a,
        body.dark-mode-pro .nav-tabs-custom > .nav-tabs > li > a {
          color: #cbd5e1 !important;
          background: transparent !important;
          border-color: transparent !important;
        }
        body.dark-mode-pro .nav-tabs > li.active > a,
        body.dark-mode-pro .nav-tabs > li.active > a:hover,
        body.dark-mode-pro .nav-tabs > li.active > a:focus {
          background: #111c30 !important;
          color: #f8fafc !important;
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
        .va-summary {
          display:flex;
          flex-direction:column;
          gap:12px;
        }
        .va-metric-grid {
          display:grid;
          grid-template-columns:repeat(2, minmax(120px, 1fr));
          gap:10px;
        }
        .va-metric-card {
          border:1px solid #334155;
          border-radius:10px;
          padding:12px;
          background:#111827;
        }
        .va-metric-card span {
          display:block;
          color:#94a3b8;
          font-size:11px;
          font-weight:800;
          text-transform:uppercase;
        }
        .va-metric-card strong {
          display:block;
          margin-top:5px;
          color:#f8fafc;
          font-size:20px;
          line-height:1.15;
          word-break:break-word;
        }
        .va-metric-card small {
          display:block;
          margin-top:5px;
          color:#94a3b8;
          line-height:1.35;
        }
        .va-status-ok { border-left:4px solid #22c55e; }
        .va-status-warn { border-left:4px solid #f59e0b; }
        .va-status-bad { border-left:4px solid #ef4444; }
        .va-status-neutral { border-left:4px solid #38bdf8; }
        .va-interpretation {
          border:1px solid #334155;
          border-radius:10px;
          padding:12px 14px;
          background:#0f1b31;
          color:#dbeafe;
          line-height:1.45;
        }
        .va-interpretation h5 {
          margin:0 0 6px;
          font-weight:800;
          color:#f8fafc;
        }
        .va-pill-row {
          display:flex;
          flex-wrap:wrap;
          gap:8px;
          margin-bottom:2px;
        }
        .va-pill {
          display:inline-flex;
          align-items:center;
          gap:6px;
          border-radius:999px;
          padding:6px 10px;
          font-size:12px;
          font-weight:800;
          border:1px solid #334155;
          background:#111827;
          color:#e5e7eb;
        }
        .va-pill.ok { border-color:#166534; color:#bbf7d0; background:#052e1d; }
        .va-pill.warn { border-color:#92400e; color:#fde68a; background:#27180f; }
        .va-pill.bad { border-color:#991b1b; color:#fecaca; background:#3b0a12; }
        .va-pill.neutral { border-color:#1d4ed8; color:#bfdbfe; background:#0f1b31; }
        .va-detail {
          border:1px solid #334155;
          border-radius:10px;
          padding:10px 12px;
          background:#0b1220;
        }
        .va-detail summary {
          cursor:pointer;
          color:#bfdbfe;
          font-weight:800;
        }
        .va-detail pre {
          margin-top:10px;
          max-height:260px;
          overflow:auto;
          white-space:pre-wrap;
        }
        .va-table {
          width:100%;
          border-collapse:collapse;
          font-size:13px;
        }
        .va-table th,
        .va-table td {
          border-bottom:1px solid #243244;
          padding:8px 6px;
          vertical-align:top;
        }
        .va-table th {
          color:#bfdbfe;
          font-weight:800;
        }
        .va-empty {
          border:1px dashed #475569;
          border-radius:10px;
          padding:14px;
          color:#cbd5e1;
          background:#0b1220;
        }
        .va-visual-card .box-body {
          padding:18px;
        }
        .va-visual-card .shiny-plot-output,
        .va-visual-card .plotly {
          min-height:540px;
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
        body.dark-mode-pro .mb-pill {
          background:#27180f;
          color:#fdba74;
          border-color:#7c2d12;
        }
        body.dark-mode-pro .mb-step-num,
        body.dark-mode-pro .mb-equation {
          background:#0f1b31;
          color:#93c5fd;
          border-color:#1d4ed8;
        }
        body.dark-mode-pro .mb-action-add {
          background:#052e1d;
          color:#86efac;
        }
        body.dark-mode-pro .mb-action-remove {
          background:#3b0a12;
          color:#fda4af;
        }
        body.dark-mode-pro .mb-action-stop {
          background:#1f2937;
          color:#cbd5e1;
        }
        body.dark-mode-pro .mb-final-chip {
          background:#052e1d;
          border-color:#166534;
          color:#bbf7d0;
        }
        body.dark-mode-pro .mb-step-p,
        body.dark-mode-pro .mb-metric span,
        body.dark-mode-pro .hint-text {
          color:#94a3b8 !important;
        }
        body.dark-mode-pro .reg-help-alert {
          background:#0f1b31;
          border-left-color:#38bdf8;
          color:#bfdbfe;
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
          background: linear-gradient(180deg, #030712 0%, #07111f 18%, #020617 18.1%, #020617 100%);
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
          color: #f8fafc;
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
          color: #f8fafc;
          text-shadow: none;
        }
        .detox-brand-copy p {
          margin: 6px 0 0;
          color: #cbd5e1;
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
          background: rgba(15, 23, 42, 0.92);
          border: 1px solid #334155;
          color: #f8fafc;
          font-weight: 600;
          box-shadow: 0 8px 20px rgba(2, 6, 23, 0.28);
        }
        .detox-chip strong {
          color: #fdba74;
          font-weight: 700;
        }
        .detox-chip.current-module {
          background: rgba(15, 23, 42, 0.96);
          border-color: #475569;
        }
        .upload-note {
          color: #e5e7eb;
          border: 1px solid #334155;
        }
        .upload-note strong {
          color: inherit;
        }
        .detox-menu-toggle {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 48px;
          height: 48px;
          border: 0;
          border-radius: 14px;
          background: #1e293b;
          color: #f8fafc;
          font-size: 20px;
          box-shadow: 0 10px 22px rgba(2, 6, 23, 0.35);
        }
        .detox-layout {
          display: grid;
          grid-template-columns: 320px minmax(0, 1fr);
          gap: 24px;
          padding: 0 28px 28px;
          align-items: start;
          transition: grid-template-columns .22s ease;
        }
        .detox-sidebar-panel {
          position: sticky;
          top: 24px;
          background: linear-gradient(180deg, #07111f 0%, #0d1b2f 100%);
          border-radius: 26px;
          padding: 22px 18px;
          border: 1px solid #1f3148;
          box-shadow: 0 20px 45px rgba(2, 6, 23, 0.38);
          color: #fff;
          overflow: hidden;
          transition: padding .22s ease, border-radius .22s ease;
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
        .detox-nav-link .fa,
        .detox-nav-link .fas {
          flex: 0 0 18px;
          text-align: center;
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
        body.detox-nav-collapsed .detox-layout {
          grid-template-columns: 88px minmax(0, 1fr);
        }
        body.detox-nav-collapsed .detox-sidebar-panel {
          padding: 18px 12px;
        }
        body.detox-nav-collapsed .detox-sidebar-panel h4,
        body.detox-nav-collapsed .detox-sidebar-panel p,
        body.detox-nav-collapsed .detox-nav-link span {
          display: none;
        }
        body.detox-nav-collapsed .detox-nav-list {
          gap: 8px;
        }
        body.detox-nav-collapsed .detox-nav-link {
          justify-content: center;
          padding: 14px 0;
          border-radius: 18px;
        }
        body.detox-nav-collapsed .detox-nav-link:hover,
        body.detox-nav-collapsed .detox-nav-link:focus {
          transform: none;
        }
        .detox-content-card {
          background: rgba(15, 23, 42, 0.82);
          backdrop-filter: blur(10px);
          border: 1px solid rgba(51, 65, 85, 0.9);
          border-radius: 28px;
          padding: 22px;
          box-shadow: 0 22px 45px rgba(2, 6, 23, 0.34);
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
          color: #f8fafc;
          font-family: 'Poppins', 'Plus Jakarta Sans', sans-serif;
          font-size: 30px;
          font-weight: 700;
        }
        .detox-content-intro p {
          margin: 8px 0 0;
          color: #cbd5e1;
          max-width: 680px;
        }
        .detox-note {
          padding: 10px 14px;
          border-radius: 16px;
          background: #27180f;
          color: #fdba74;
          font-weight: 700;
          border: 1px solid #7c2d12;
        }
        .detox-subnav-wrap {
          margin-bottom: 18px;
          padding: 16px 18px;
          border-radius: 22px;
          background: linear-gradient(180deg, #0f1b31 0%, #0b1629 100%);
          border: 1px solid #23344b;
        }
        .detox-subnav-title {
          font-size: 13px;
          font-weight: 800;
          text-transform: uppercase;
          letter-spacing: .08em;
          color: #93c5fd;
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
          background: #111c30;
          border: 1px solid #2c415f;
          color: #dbeafe;
          font-weight: 700;
          box-shadow: none;
          transition: transform .18s ease, box-shadow .18s ease, background .18s ease;
        }
        .detox-subnav-link:hover,
        .detox-subnav-link:focus {
          text-decoration: none;
          color: #ffffff;
          transform: translateY(-1px);
          background: #16233b;
          box-shadow: none;
        }
        .detox-subnav-link.is-active {
          background: linear-gradient(135deg, #0ea5e9 0%, #2563eb 100%);
          border-color: transparent;
          color: #ffffff;
          box-shadow: 0 12px 24px rgba(14, 165, 233, 0.22);
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
        .preview-page .box {
          overflow: hidden;
        }
        .preview-table-card .box-body {
          padding: 18px;
        }
        .preview-table-card .dataTables_wrapper {
          min-height: 560px;
        }
        .preview-summary-card .box-body {
          min-height: 320px;
          padding: 16px;
        }
        .preview-summary-card h5 {
          margin-top: 0;
          margin-bottom: 12px;
          font-weight: 700;
          color: #e5e7eb;
        }
        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          color: #e5e7eb !important;
          background: #111827 !important;
          border-color: #334155 !important;
        }
        .dataTables_wrapper table.dataTable thead th,
        .dataTables_wrapper table.dataTable tbody td,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate .paginate_button,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter label,
        .dataTables_wrapper .dataTables_length label {
          color: #e5e7eb !important;
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
        body.dark-mode-pro .detox-menu-toggle {
          background: #1e293b;
          color: #f8fafc;
          box-shadow: 0 10px 22px rgba(2, 6, 23, 0.35);
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
          .detox-layout {
            grid-template-columns: 1fr;
            padding: 0 16px 20px;
          }
          body.detox-nav-collapsed .detox-layout {
            grid-template-columns: 1fr;
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
              title = "Toggle navigation",
              `aria-label` = "Toggle navigation",
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
              condition = "['upload','preview','varanalysis','catsuite','tests'].includes(input.tabs)",
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
            tags$div(
              class = "upload-note upload-note-success",
              style = "background-color: #dff0d8; padding: 10px; border-radius: 5px; margin-top: 10px;",
              tags$strong(icon("check-circle"), " Ready to analyze:"),
              " Use the default P2_DeliveryTime.xlsx dataset or upload a CSV/Excel file, then click Analyze Data."
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
        )
      ),

      # Data Preview Tab
      tabItem(
        tabName = "preview",
        tags$div(
          class = "preview-page",
          fluidRow(
            box(
              title = tagList(icon("table"), " Data Preview"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              class = "preview-table-card",
              spn(DTOutput("dataPreview"))
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("calculator"), " Numeric Summary"),
              status = "info",
              solidHeader = TRUE,
              width = 8,
              class = "preview-summary-card",
              h5("Numeric Variables"),
              spn(DT::DTOutput("dataSummaryNumeric"))
            ),
            box(
              title = tagList(icon("tags"), " Categorical Summary"),
              status = "success",
              solidHeader = TRUE,
              width = 4,
              class = "preview-summary-card",
              h5("Categorical Variables"),
              spn(DT::DTOutput("dataSummaryCategorical"))
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
              width = 12,
              uiOutput("singleStats")
            )
          ),
          fluidRow(
            box(
              title = "Visualization",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              class = "va-visual-card",
              conditionalPanel(
                condition = "input.interactivePlots",
                spn(plotly_output_safe("singlePlotly", height = "560px"))
              ),
              conditionalPanel(
                condition = "!input.interactivePlots",
                spn(plotOutput("singlePlot", height = "560px"))
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.analysisMode == 'multi'",
          uiOutput("multiAnalysisOutputUI")
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
              status = "info",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(4, uiOutput("catVarSelect")),
                column(
                  4,
                  selectInput("catPlotType", "Plot Type:",
                    choices = c("Frequency Bar", "Share Bar")
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
              status = "info",
              solidHeader = TRUE,
              width = 12,
              uiOutput("catFrequency"),
              uiOutput("catProportion")
            )
          ),
          fluidRow(
            box(
              title = "Visualization",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              class = "va-visual-card",
              conditionalPanel(
                condition = "input.interactivePlots",
                spn(plotly_output_safe("catPlotly", height = "560px"))
              ),
              conditionalPanel(
                condition = "!input.interactivePlots",
                spn(plotOutput("catPlot", height = "560px"))
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
              width = 12,
              uiOutput("crosstabTable")
            )
          ),
          fluidRow(
            box(
              title = "Chi-Square Test",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              uiOutput("chiSquareTest")
            )
          ),
          fluidRow(
            box(
              title = "Joint Proportions",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              uiOutput("crosstabProp")
            )
          ),
          fluidRow(
            box(
              title = "Visualization",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              class = "va-visual-card",
              selectInput("crosstabPlotType", "Plot Type:",
                choices = c("Count Heatmap", "Stacked Share Bar", "Grouped Count Bar")
              ),
              spn(plotOutput("crosstabPlot", height = "600px"))
            ),
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
              uiOutput("multiCatTable"),
              hr(),
              h4("Conditional Proportions"),
              uiOutput("multiCatProportions")
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("calculator"), " Multi-Way Chi-Square Test"),
              status = "success",
              solidHeader = TRUE,
              width = 6,
              uiOutput("multiCatChiSquare")
            ),
            box(
              title = tagList(icon("info-circle"), " Association Measures"),
              status = "success",
              solidHeader = TRUE,
              width = 6,
              uiOutput("multiCatAssociation")
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("chart-bar"), " Multi-Way Visualization"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              class = "va-visual-card",
              fluidRow(
                column(
                  4,
                  selectInput("multiCatPlotType", "Visualization Type:",
                    choices = c(
                      "Heatmap",
                      "Faceted Heatmap",
                      "Stacked Share Bar",
                      "Grouped Count Bar"
                    )
                  )
                ),
                column(8, uiOutput("multiCatPlotOptions"))
              ),
              spn(plotOutput("multiCatPlot", height = "620px"))
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("lightbulb"), " Analysis Insights"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              uiOutput("multiCatInsights")
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
            width = 12,
            uiOutput("testResults")
          )
        ),
        fluidRow(
          box(
            title = "Visualization",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            class = "va-visual-card",
            plotOutput("testPlot", height = "560px")
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
                uiOutput("slrSummary")
              ),
              tabPanel(
                tagList(icon("calculator"), " ANOVA Table"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Tests overall model significance (F-test). A small p-value means the model explains a significant portion of the variation in Y."
                ),
                uiOutput("slrAnova")
              ),
              tabPanel(
                tagList(icon("arrows-alt-h"), " Confidence Intervals"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " 95% confidence intervals for the regression coefficients (β₀ and β₁)."
                ),
                uiOutput("slrConfint")
              ),
              tabPanel(
                tagList(icon("link"), " Correlation Tests"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Pearson (parametric), Spearman, and Kendall correlation tests between X and Y."
                ),
                uiOutput("slrCorrelation")
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
                  " Checks whether each new X falls inside (interpolation) or outside (EXTRAPOLATION) the observed data range. Extrapolation predictions are unreliable."
                ),
                uiOutput("slrInterpExtrap")
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
                  " Displays fitted coefficients, standard errors, t-tests for each predictor, overall F-test, R², and adjusted R²."
                ),
                uiOutput("mlrSummary")
              ),
              tabPanel(
                tagList(icon("calculator"), " ANOVA Table"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Sequential (Type I) ANOVA — shows the contribution of each predictor added in order."
                ),
                uiOutput("mlrAnova")
              ),
              tabPanel(
                tagList(icon("vial"), " Partial F-Tests"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " Tests H₀: βᵢ = 0 for each predictor by comparing the full model against a reduced model without that predictor."
                ),
                uiOutput("mlrPartialF")
              ),
              tabPanel(
                tagList(icon("arrows-alt-h"), " Confidence Intervals"),
                br(),
                tags$div(
                  class = "reg-help-alert",
                  icon("info-circle"),
                  " 95% confidence intervals for all regression coefficients."
                ),
                uiOutput("mlrConfint")
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
            uiOutput("mlrVIF")
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
                  " Uses leverage values (hᵢᵢ) to detect whether new observations fall inside (interpolation) or outside (EXTRAPOLATION) the predictor space."
                ),
                uiOutput("mlrInterpExtrap")
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
            uiOutput("polySummary")
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
            uiOutput("polyCenterText")
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
            uiOutput("polySplineSummary")
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
        scrollY = "520px",
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
      options = list(pageLength = 8, scrollX = TRUE, scrollY = "260px"),
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
      options = list(pageLength = 8, scrollX = TRUE, scrollY = "260px"),
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

  fmt_value <- function(x, digits = 3) {
    if (length(x) == 0 || is.null(x) || all(is.na(x))) {
      return("N/A")
    }
    x <- x[1]
    if (is.numeric(x)) {
      if (!is.finite(x)) return("N/A")
      return(format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE))
    }
    as.character(x)
  }

  p_status <- function(p) {
    if (length(p) == 0 || is.na(p)) return("bad")
    if (p < 0.05) "ok" else if (p < 0.10) "warn" else "neutral"
  }

  p_label <- function(p) {
    if (length(p) == 0 || is.na(p)) return("Could not compute")
    if (p < 0.05) "Statistically significant" else if (p < 0.10) "Borderline evidence" else "No clear evidence"
  }

  va_pill <- function(text, status = "neutral", icon_name = NULL) {
    tags$span(
      class = paste("va-pill", status),
      if (!is.null(icon_name)) icon(icon_name),
      text
    )
  }

  va_metric <- function(label, value, note = NULL, status = "neutral") {
    tags$div(
      class = paste("va-metric-card", paste0("va-status-", status)),
      tags$span(label),
      tags$strong(value),
      if (!is.null(note)) tags$small(note)
    )
  }

  va_interpretation <- function(title, body, pills = NULL) {
    tags$div(
      class = "va-interpretation",
      if (!is.null(pills)) tags$div(class = "va-pill-row", pills),
      tags$h5(title),
      tags$p(style = "margin:0;", body)
    )
  }

  va_details <- function(title, lines) {
    tags$details(
      class = "va-detail",
      tags$summary(title),
      tags$pre(paste(lines, collapse = "\n"))
    )
  }

  va_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(class = "va-empty", "No rows to display."))
    }
    tags$table(
      class = "va-table",
      tags$thead(tags$tr(lapply(names(df), tags$th))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(lapply(df[i, , drop = FALSE], function(x) tags$td(as.character(x[[1]]))))
      }))
    )
  }

  skew_status <- function(sk) {
    if (is.na(sk)) return("bad")
    if (abs(sk) < 0.5) "ok" else if (abs(sk) < 1) "warn" else "bad"
  }

  corr_status <- function(r) {
    if (is.na(r)) return("bad")
    ar <- abs(r)
    if (ar < 0.3) "ok" else if (ar < 0.7) "warn" else "bad"
  }

  cramers_status <- function(v) {
    if (length(v) == 0 || is.na(v)) return("bad")
    if (v < 0.1) "ok" else if (v < 0.3) "neutral" else if (v < 0.5) "warn" else "bad"
  }

  cramers_label <- function(v) {
    if (length(v) == 0 || is.na(v)) return("Could not compute")
    if (v < 0.1) "Negligible association" else if (v < 0.3) "Weak association" else if (v < 0.5) "Moderate association" else "Strong association"
  }

  clean_cat_vector <- function(x) {
    x <- as.character(x)
    x[is.na(x) | trimws(x) == ""] <- NA_character_
    factor(x)
  }

  cat_count_df <- function(x, max_levels = Inf) {
    x <- clean_cat_vector(x)
    freq <- table(x, useNA = "no")
    prop <- prop.table(freq)
    df <- data.frame(
      Category = names(freq),
      Count = as.integer(freq),
      Percent = paste0(round(100 * as.numeric(prop), 1), "%"),
      Share = as.numeric(prop),
      check.names = FALSE
    )
    df <- df[order(df$Count, decreasing = TRUE), , drop = FALSE]
    if (is.finite(max_levels) && nrow(df) > max_levels) {
      top <- df[seq_len(max_levels - 1), , drop = FALSE]
      other <- data.frame(
        Category = "Other levels",
        Count = sum(df$Count[-seq_len(max_levels - 1)]),
        Percent = paste0(round(100 * sum(df$Share[-seq_len(max_levels - 1)]), 1), "%"),
        Share = sum(df$Share[-seq_len(max_levels - 1)]),
        check.names = FALSE
      )
      df <- rbind(top, other)
    }
    df
  }

  va_palette <- function(n, palette = "Set2") {
    n <- max(1, n)
    base_n <- min(max(3, n), 8)
    if (n <= 8) {
      RColorBrewer::brewer.pal(base_n, palette)[seq_len(n)]
    } else {
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, palette))(n)
    }
  }

  va_plot_theme <- function() {
    ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#0f172a", color = NA),
        panel.background = ggplot2::element_rect(fill = "#0f172a", color = NA),
        panel.grid.major = ggplot2::element_line(color = "#26364d", linewidth = 0.25),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(color = "#f8fafc", face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "#cbd5e1"),
        axis.title = ggplot2::element_text(color = "#dbeafe", face = "bold"),
        axis.text = ggplot2::element_text(color = "#cbd5e1"),
        legend.title = ggplot2::element_text(color = "#dbeafe", face = "bold"),
        legend.text = ggplot2::element_text(color = "#cbd5e1"),
        legend.background = ggplot2::element_rect(fill = "#0f172a", color = NA),
        strip.background = ggplot2::element_rect(fill = "#111c30", color = "#334155"),
        strip.text = ggplot2::element_text(color = "#f8fafc", face = "bold")
      )
  }

  capture_lines <- function(expr) {
    tryCatch(capture.output(expr), error = function(e) paste("Could not compute technical output:", e$message))
  }

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
      valid_data <- var_data[is.finite(var_data)]
      n_total <- length(var_data)
      n_valid <- length(valid_data)
      n_missing <- sum(is.na(var_data) | !is.finite(var_data))

      output$singleStats <- renderUI({
        if (n_valid == 0) {
          return(tags$div(class = "va-empty", "No finite numeric values are available for this variable."))
        }

        mean_v <- mean(valid_data)
        median_v <- median(valid_data)
        sd_v <- if (n_valid > 1) sd(valid_data) else NA_real_
        iqr_v <- IQR(valid_data)
        mode_v <- names(sort(table(valid_data), decreasing = TRUE)[1])
        skew_v <- if (n_valid > 2) moments::skewness(valid_data, na.rm = TRUE) else NA_real_
        kurt_v <- if (n_valid > 3) moments::kurtosis(valid_data, na.rm = TRUE) else NA_real_
        mean_median_gap <- abs(mean_v - median_v)
        gap_note <- if (!is.na(sd_v) && sd_v > 0) {
          paste0("Mean-median gap is ", fmt_value(mean_median_gap / sd_v, 2), " SD.")
        } else {
          "Mean and median comparison is limited because spread is zero or unavailable."
        }
        skew_msg <- if (is.na(skew_v)) {
          "Skewness needs at least three finite values."
        } else if (abs(skew_v) < 0.5) {
          "The distribution is roughly symmetric."
        } else if (skew_v > 0) {
          "The distribution is right-skewed; high values pull the mean upward."
        } else {
          "The distribution is left-skewed; low values pull the mean downward."
        }
        missing_status <- if (n_missing == 0) "ok" else "warn"

        tags$div(
          class = "va-summary",
          tags$div(
            class = "va-metric-grid",
            va_metric("Valid values", format(n_valid, big.mark = ","), paste("Out of", format(n_total, big.mark = ",")), "ok"),
            va_metric("Missing / non-finite", format(n_missing, big.mark = ","), "Ignored in descriptive statistics", missing_status),
            va_metric("Mean", fmt_value(mean_v), "Average value", "neutral"),
            va_metric("Median", fmt_value(median_v), "Middle value", "neutral"),
            va_metric("Std. deviation", fmt_value(sd_v), "Typical spread around the mean", ifelse(is.na(sd_v), "bad", "neutral")),
            va_metric("IQR", fmt_value(iqr_v), "Middle 50% spread", "neutral")
          ),
          va_interpretation(
            paste("What this says about", input$singleVar),
            paste(skew_msg, gap_note, "Outliers are best checked with the boxplot."),
            pills = tagList(
              va_pill(paste("Skewness:", fmt_value(skew_v)), skew_status(skew_v), ifelse(skew_status(skew_v) == "ok", "check-circle", ifelse(skew_status(skew_v) == "warn", "exclamation-triangle", "times-circle"))),
              va_pill(paste("Kurtosis:", fmt_value(kurt_v)), "neutral", "chart-area")
            )
          ),
          va_details(
            "Technical details: descriptive statistics",
            c(
              paste("Variable:", input$singleVar),
              paste("Mean:", fmt_value(mean_v)),
              paste("Median:", fmt_value(median_v)),
              paste("Mode:", mode_v %||% "N/A"),
              paste("Range:", fmt_value(max(valid_data) - min(valid_data))),
              paste("Variance:", fmt_value(if (n_valid > 1) var(valid_data) else NA_real_)),
              paste("Std Dev:", fmt_value(sd_v)),
              paste("IQR:", fmt_value(iqr_v)),
              paste("Skewness:", fmt_value(skew_v)),
              paste("Kurtosis:", fmt_value(kurt_v)),
              "Rules of thumb: |skewness| < 0.5 is roughly symmetric; 0.5-1 is moderate skew; > 1 is strong skew."
            )
          )
        )
      })

      output$singlePlot <- renderPlot({
        req(n_valid > 0)
        plot_df <- data.frame(value = valid_data)
        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = value)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            bins = min(35, max(8, ceiling(sqrt(n_valid)))),
            fill = "#38bdf8", color = "#0f172a", alpha = 0.78
          ) +
          ggplot2::geom_vline(xintercept = mean(valid_data), color = "#22c55e", linewidth = 1.1) +
          ggplot2::geom_vline(xintercept = median(valid_data), color = "#f59e0b", linewidth = 1.1, linetype = "dashed") +
          ggplot2::geom_rug(color = "#93c5fd", alpha = 0.35) +
          ggplot2::labs(
            title = paste("Distribution of", input$singleVar),
            subtitle = "Histogram scaled to density; green line = mean, amber dashed line = median",
            x = input$singleVar,
            y = "Density"
          ) +
          va_plot_theme()
        if (length(unique(valid_data)) > 1) {
          p <- p + ggplot2::geom_density(color = "#f8fafc", linewidth = 1.1, adjust = 1.1)
        }
        print(p)
      })
      if (HAS_PLOTLY) {
        output$singlePlotly <- plotly::renderPlotly({
          req(n_valid > 0)
          p1 <- plotly::plot_ly(x = valid_data, type = "histogram", histnorm = "probability density", marker = list(color = "#38bdf8")) |>
            plotly::layout(
              title = paste("Distribution of", input$singleVar),
              xaxis = list(title = input$singleVar),
              yaxis = list(title = "Density"),
              shapes = list(
                list(type = "line", x0 = mean(valid_data), x1 = mean(valid_data), y0 = 0, y1 = 1, yref = "paper", line = list(color = "#22c55e", width = 2)),
                list(type = "line", x0 = median(valid_data), x1 = median(valid_data), y0 = 0, y1 = 1, yref = "paper", line = list(color = "#f59e0b", width = 2, dash = "dash"))
              )
            )
          p2 <- plotly::plot_ly(x = valid_data, type = "box", marker = list(color = "#34d399"), boxpoints = "outliers") |>
            plotly::layout(title = paste("Outlier check for", input$singleVar), xaxis = list(title = input$singleVar))
          plotly::subplot(p1, p2, nrows = 2, shareX = FALSE, titleY = TRUE)
        })
      }
    } else {
      var_data <- data$raw[[input$singleVar]]
      missing_cat <- is.na(var_data) | trimws(as.character(var_data)) == ""
      freq_table <- table(var_data[!missing_cat])
      prop_table <- prop.table(freq_table)

      output$singleStats <- renderUI({
        if (length(freq_table) == 0) {
          return(tags$div(class = "va-empty", "No non-missing categories are available for this variable."))
        }
        mode_name <- names(which.max(freq_table))
        mode_freq <- max(freq_table)
        mode_pct <- mode_freq / sum(freq_table)
        missing_n <- sum(missing_cat)
        top_df <- data.frame(
          Level = names(freq_table),
          Count = as.integer(freq_table),
          Percent = paste0(round(100 * as.numeric(prop_table), 1), "%"),
          check.names = FALSE
        )
        top_df <- top_df[order(top_df$Count, decreasing = TRUE), , drop = FALSE]

        concentration_status <- if (mode_pct >= 0.75) "warn" else "ok"
        concentration_msg <- if (mode_pct >= 0.75) {
          "One category dominates the variable, so comparisons involving smaller groups may be unstable."
        } else {
          "The categories are not dominated by a single level."
        }

        tags$div(
          class = "va-summary",
          tags$div(
            class = "va-metric-grid",
            va_metric("Levels", length(freq_table), "Distinct non-missing categories", "neutral"),
            va_metric("Most common", mode_name, paste0(mode_freq, " rows (", round(100 * mode_pct, 1), "%)"), concentration_status),
            va_metric("Non-missing rows", format(sum(freq_table), big.mark = ","), "Used in frequency table", "ok"),
            va_metric("Missing / blank", format(missing_n, big.mark = ","), "Excluded from proportions", ifelse(missing_n == 0, "ok", "warn"))
          ),
          va_interpretation(
            paste("What this says about", input$singleVar),
            concentration_msg,
            pills = tagList(
              va_pill(paste("Mode:", mode_name), concentration_status, ifelse(concentration_status == "ok", "check-circle", "exclamation-triangle")),
              va_pill(paste("Top share:", paste0(round(100 * mode_pct, 1), "%")), "neutral", "chart-pie")
            )
          ),
          va_table(head(top_df, 12)),
          va_details(
            "Technical details: full frequency table",
            c(
              paste("Variable:", input$singleVar),
              "Frequency table:",
              capture.output(print(freq_table)),
              "",
              "Proportion table:",
              capture.output(print(round(prop_table, 4))),
              "",
              paste("Mode:", mode_name),
              paste("Modal frequency:", mode_freq)
            )
          )
        )
      })

      output$singlePlot <- renderPlot({
        req(length(freq_table) > 0)
        dfp <- data.frame(
          level = names(freq_table),
          count = as.numeric(freq_table),
          percent = as.numeric(prop_table)
        )
        dfp <- dfp[order(dfp$count, decreasing = TRUE), , drop = FALSE]
        if (nrow(dfp) > 18) {
          top <- dfp[seq_len(17), , drop = FALSE]
          other <- data.frame(
            level = "Other levels",
            count = sum(dfp$count[-seq_len(17)]),
            percent = sum(dfp$percent[-seq_len(17)])
          )
          dfp <- rbind(top, other)
        }
        dfp$level <- factor(dfp$level, levels = rev(dfp$level))
        ggplot2::ggplot(dfp, ggplot2::aes(x = level, y = count, fill = percent)) +
          ggplot2::geom_col(width = 0.72) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(round(percent * 100, 1), "%")),
            hjust = -0.1, color = "#e5e7eb", size = 3.6
          ) +
          ggplot2::coord_flip(clip = "off") +
          ggplot2::scale_fill_gradient(low = "#2563eb", high = "#22c55e", labels = function(x) paste0(round(100 * x), "%")) +
          ggplot2::labs(
            title = paste("Category profile of", input$singleVar),
            subtitle = "Bars are sorted by frequency; labels show share of non-missing rows",
            x = NULL,
            y = "Count",
            fill = "Share"
          ) +
          va_plot_theme() +
          ggplot2::theme(plot.margin = ggplot2::margin(10, 42, 10, 10))
      })
      if (HAS_PLOTLY) {
        output$singlePlotly <- plotly::renderPlotly({
          req(length(freq_table) > 0)
          dfp <- data.frame(
            cat = names(freq_table),
            freq = as.numeric(freq_table),
            pct = round(100 * as.numeric(prop_table), 1)
          )
          dfp <- dfp[order(dfp$freq, decreasing = TRUE), , drop = FALSE]
          plotly::plot_ly(
            dfp,
            x = ~freq, y = ~reorder(cat, freq),
            type = "bar", orientation = "h",
            text = ~paste0(pct, "%"),
            textposition = "outside",
            marker = list(color = "#38bdf8")
          ) |>
            plotly::layout(
              title = paste("Category profile of", input$singleVar),
              xaxis = list(title = "Count"),
              yaxis = list(title = "")
            )
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

      visual_supported <- nrow(df_sel) > 0 && (length(num_vars) + length(cat_vars) >= 2)
      output$multiAnalysisOutputUI <- renderUI({
        tagList(
          fluidRow(
            box(
              title = "Analysis Results",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              uiOutput("twoVarStats")
            )
          ),
          if (visual_supported) {
            fluidRow(
              box(
                title = "Visualization",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                class = "va-visual-card",
                conditionalPanel(
                  condition = "input.interactivePlots",
                  spn(plotly_output_safe("twoVarPlotly", height = "600px"))
                ),
                conditionalPanel(
                  condition = "!input.interactivePlots",
                  spn(plotOutput("twoVarPlot", height = "600px"))
                )
              )
            )
          }
        )
      })

      selected_tbl <- data.frame(
        Variable = selected_vars,
        Type = selected_types,
        Missing = vapply(selected_vars, function(v) sum(is.na(df_sel[[v]])), integer(1)),
        check.names = FALSE
      )

      corr_mat <- NULL
      corr_summary <- NULL
      if (length(num_vars) >= 2) {
        corr_mat <- tryCatch(cor(df_sel[, num_vars, drop = FALSE], use = "complete.obs"), error = function(e) NULL)
        if (!is.null(corr_mat)) {
          upper_idx <- upper.tri(corr_mat)
          upper_vals <- abs(corr_mat[upper_idx])
          if (length(upper_vals) > 0 && any(is.finite(upper_vals))) {
            max_pos <- which(upper_idx, arr.ind = TRUE)[which.max(upper_vals), , drop = FALSE]
            r_val <- corr_mat[max_pos[1, 1], max_pos[1, 2]]
            corr_summary <- list(
              r = r_val,
              var1 = rownames(corr_mat)[max_pos[1, 1]],
              var2 = colnames(corr_mat)[max_pos[1, 2]],
              status = corr_status(r_val)
            )
          }
        }
      }

      chi_results <- list()
      if (length(cat_vars) >= 2) {
        for (i in 1:(length(cat_vars) - 1)) {
          for (j in (i + 1):length(cat_vars)) {
            t_ij <- table(df_sel[[cat_vars[i]]], df_sel[[cat_vars[j]]])
            res <- tryCatch(
              {
                ch <- suppressWarnings(chisq.test(t_ij))
                list(
                  var1 = cat_vars[i],
                  var2 = cat_vars[j],
                  statistic = unname(ch$statistic),
                  df = unname(ch$parameter),
                  p = ch$p.value,
                  low_expected = any(ch$expected < 5),
                  error = NULL
                )
              },
              error = function(e) {
                list(var1 = cat_vars[i], var2 = cat_vars[j], error = e$message)
              }
            )
            chi_results[[length(chi_results) + 1]] <- res
          }
        }
      }

      anova_results <- list()
      if (length(num_vars) >= 1 && length(cat_vars) >= 1) {
        for (nv in num_vars) {
          for (cv in cat_vars) {
            tmp <- data.frame(y = df_sel[[nv]], g = as.factor(df_sel[[cv]]))
            tmp <- tmp[complete.cases(tmp), , drop = FALSE]
            res <- if (nlevels(tmp$g) < 2 || nrow(tmp) < 3) {
              list(num = nv, cat = cv, error = "Not enough valid groups/data for ANOVA.")
            } else {
              tryCatch(
                {
                  aov_out <- summary(aov(y ~ g, data = tmp))
                  tbl <- aov_out[[1]]
                  list(
                    num = nv,
                    cat = cv,
                    f_value = unname(tbl[1, "F value"]),
                    p = unname(tbl[1, "Pr(>F)"]),
                    groups = nlevels(tmp$g),
                    n = nrow(tmp),
                    technical = capture.output(print(aov_out)),
                    error = NULL
                  )
                },
                error = function(e) list(num = nv, cat = cv, error = e$message)
              )
            }
            anova_results[[length(anova_results) + 1]] <- res
          }
        }
      }

      output$twoVarStats <- renderUI({
        sections <- list(
          tags$div(
            class = "va-summary",
            va_interpretation(
              "Selected variables",
              "The analysis below is grouped by variable type: numeric-numeric relationships use correlations, categorical-categorical relationships use chi-square tests, and numeric-categorical relationships use one-way ANOVA.",
              pills = tagList(
                va_pill(paste(length(num_vars), "numeric"), "neutral", "hashtag"),
                va_pill(paste(length(cat_vars), "categorical"), "neutral", "tags")
              )
            ),
            va_table(selected_tbl)
          )
        )

        if (length(num_vars) >= 2) {
          corr_body <- if (is.null(corr_summary)) {
            tags$div(class = "va-empty", "The correlation matrix could not be computed. Check for too few complete numeric rows or constant variables.")
          } else {
            tagList(
              tags$div(
                class = "va-metric-grid",
                va_metric("Strongest absolute r", fmt_value(abs(corr_summary$r), 3), paste(corr_summary$var1, "vs", corr_summary$var2), corr_summary$status),
                va_metric("Direction", ifelse(corr_summary$r >= 0, "Positive", "Negative"), "Sign shows whether variables move together or in opposite directions.", "neutral")
              ),
              va_interpretation(
                "Numeric relationship",
                "Correlation ranges from -1 to 1. Values near 0 suggest weak linear association; values near -1 or 1 suggest stronger linear association. Correlation does not prove causation.",
                pills = tagList(
                  va_pill(ifelse(abs(corr_summary$r) >= 0.7, "Strong linear association", ifelse(abs(corr_summary$r) >= 0.3, "Moderate linear association", "Weak linear association")), corr_summary$status, ifelse(corr_summary$status == "ok", "check-circle", ifelse(corr_summary$status == "warn", "exclamation-triangle", "times-circle")))
                )
              ),
              va_details(
                "Technical details: correlation matrix",
                capture.output(print(round(corr_mat, 4)))
              )
            )
          }
          sections[[length(sections) + 1]] <- tags$div(class = "va-summary", corr_body)
        }

        if (length(chi_results) > 0) {
          chi_df <- do.call(rbind, lapply(chi_results, function(x) {
            if (!is.null(x$error)) {
              data.frame(Comparison = paste(x$var1, "vs", x$var2), Chi_square = "N/A", df = "N/A", P_value = "N/A", Result = x$error, check.names = FALSE)
            } else {
              data.frame(
                Comparison = paste(x$var1, "vs", x$var2),
                Chi_square = fmt_value(x$statistic),
                df = fmt_value(x$df, 0),
                P_value = format.pval(x$p, digits = 4),
                Result = paste(p_label(x$p), if (isTRUE(x$low_expected)) "(expected counts warning)" else ""),
                check.names = FALSE
              )
            }
          }))
          sections[[length(sections) + 1]] <- tags$div(
            class = "va-summary",
            va_interpretation(
              "Categorical relationships",
              "Chi-square tests compare observed counts to the counts expected if the variables were independent. H0: the variables are independent. Small p-values suggest an association, but sparse tables can make the approximation unreliable.",
              pills = tagList(
                va_pill(paste(length(chi_results), "pairwise test(s)"), "neutral", "calculator"),
                va_pill("p < 0.05: evidence of association", "ok", "check-circle")
              )
            ),
            va_table(chi_df),
            va_details(
              "Technical details: chi-square tests",
              unlist(lapply(chi_results, function(x) {
                if (!is.null(x$error)) {
                  return(c(paste(x$var1, "vs", x$var2), paste("Error:", x$error), ""))
                }
                c(
                  paste(x$var1, "vs", x$var2),
                  paste("Chi-square:", fmt_value(x$statistic), "df:", fmt_value(x$df, 0), "p-value:", format.pval(x$p, digits = 4)),
                  paste("Expected counts warning:", ifelse(isTRUE(x$low_expected), "Yes; some expected counts are below 5.", "No.")),
                  ""
                )
              }))
            )
          )
        }

        if (length(anova_results) > 0) {
          anova_df <- do.call(rbind, lapply(anova_results, function(x) {
            if (!is.null(x$error)) {
              data.frame(Comparison = paste(x$num, "by", x$cat), Groups = "N/A", F_value = "N/A", P_value = "N/A", Result = x$error, check.names = FALSE)
            } else {
              data.frame(
                Comparison = paste(x$num, "by", x$cat),
                Groups = x$groups,
                F_value = fmt_value(x$f_value),
                P_value = format.pval(x$p, digits = 4),
                Result = p_label(x$p),
                check.names = FALSE
              )
            }
          }))
          sections[[length(sections) + 1]] <- tags$div(
            class = "va-summary",
            va_interpretation(
              "Numeric outcome by category",
              "One-way ANOVA compares group means. H0: all group means are equal. A small p-value suggests at least one group mean differs; it does not identify which groups differ or prove assumptions such as normal residuals and equal variances.",
              pills = tagList(
                va_pill(paste(length(anova_results), "ANOVA comparison(s)"), "neutral", "chart-bar"),
                va_pill("p < 0.05: group means differ", "ok", "check-circle")
              )
            ),
            va_table(anova_df),
            va_details(
              "Technical details: ANOVA outputs",
              unlist(lapply(anova_results, function(x) {
                if (!is.null(x$error)) {
                  return(c(paste(x$num, "by", x$cat), paste("Error:", x$error), ""))
                }
                c(paste(x$num, "by", x$cat), x$technical, "")
              }))
            )
          )
        }

        if (length(sections) == 1) {
          sections[[length(sections) + 1]] <- tags$div(
            class = "va-empty",
            "No statistical block was available for the selected combination. Choose at least two numeric variables, two categorical variables, or a mix of numeric and categorical variables."
          )
        }

        tags$div(class = "va-summary", sections)
      })

      output$twoVarPlot <- renderPlot({
        if (length(num_vars) >= 2 && length(cat_vars) >= 1) {
          complete_rows <- complete.cases(df_sel[, c(num_vars[1:2], cat_vars[1]), drop = FALSE])
          if (sum(complete_rows) < 2) {
            plot.new()
            text(0.5, 0.5, "Not enough complete rows for this scatter plot.", cex = 1.1)
            return()
          }
          plot_df <- data.frame(
            x = df_sel[[num_vars[1]]][complete_rows],
            y = df_sel[[num_vars[2]]][complete_rows],
            group = as.factor(df_sel[[cat_vars[1]]][complete_rows])
          )
          ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y, color = group)) +
            ggplot2::geom_point(alpha = 0.72, size = 2.5) +
            ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, alpha = 0.6) +
            ggplot2::scale_color_manual(values = va_palette(length(levels(plot_df$group)), "Set2")) +
            ggplot2::labs(
              title = paste(num_vars[1], "vs", num_vars[2]),
              subtitle = paste("Colored by", cat_vars[1], "- lines show within-group linear trends"),
              x = num_vars[1],
              y = num_vars[2],
              color = cat_vars[1]
            ) +
            va_plot_theme()
        } else if (length(num_vars) >= 3) {
          complete_numeric <- df_sel[complete.cases(df_sel[, num_vars, drop = FALSE]), num_vars, drop = FALSE]
          if (nrow(complete_numeric) < 2) {
            plot.new()
            text(0.5, 0.5, "Not enough complete numeric rows for a correlation heatmap.", cex = 1.1)
            return()
          }
          cm <- cor(complete_numeric, use = "complete.obs")
          heat_df <- as.data.frame(as.table(cm))
          names(heat_df) <- c("Variable_1", "Variable_2", "Correlation")
          ggplot2::ggplot(heat_df, ggplot2::aes(x = Variable_2, y = Variable_1, fill = Correlation)) +
            ggplot2::geom_tile(color = "#0f172a", linewidth = 0.5) +
            ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), color = "#f8fafc", size = 3.5) +
            ggplot2::scale_fill_gradient2(low = "#ef4444", mid = "#1e293b", high = "#22c55e", midpoint = 0, limits = c(-1, 1)) +
            ggplot2::labs(
              title = "Correlation heatmap",
              subtitle = "Green = positive linear association, red = negative linear association",
              x = NULL,
              y = NULL,
              fill = "r"
            ) +
            va_plot_theme() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
        } else if (length(num_vars) == 2) {
          x <- df_sel[[num_vars[1]]]
          y <- df_sel[[num_vars[2]]]
          complete_rows <- complete.cases(x, y)
          if (sum(complete_rows) < 2) {
            plot.new()
            text(0.5, 0.5, "Not enough complete rows for this scatter plot.", cex = 1.1)
            return()
          }
          plot_df <- data.frame(x = x[complete_rows], y = y[complete_rows])
          r_val <- suppressWarnings(cor(plot_df$x, plot_df$y))
          ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point(color = "#38bdf8", alpha = 0.72, size = 2.6) +
            ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#22c55e", fill = "#22c55e", alpha = 0.16, linewidth = 1) +
            ggplot2::labs(
              title = paste(num_vars[1], "vs", num_vars[2]),
              subtitle = paste("Linear trend with 95% confidence band; correlation r =", fmt_value(r_val, 3)),
              x = num_vars[1],
              y = num_vars[2]
            ) +
            va_plot_theme()
        } else if (length(num_vars) == 1 && length(cat_vars) >= 2) {
          plot_df <- data.frame(y = df_sel[[num_vars[1]]], df_sel[, cat_vars[1:2], drop = FALSE])
          plot_df <- plot_df[complete.cases(plot_df), , drop = FALSE]
          if (nrow(plot_df) < 2) {
            plot.new()
            text(0.5, 0.5, "Not enough complete rows for this grouped boxplot.", cex = 1.1)
            return()
          }
          g <- interaction(plot_df[, cat_vars[1:2], drop = FALSE], drop = TRUE)
          plot_df$group <- stats::reorder(g, plot_df$y, median, na.rm = TRUE)
          ggplot2::ggplot(plot_df, ggplot2::aes(x = group, y = y)) +
            ggplot2::geom_boxplot(fill = "#2563eb", color = "#dbeafe", alpha = 0.65, outlier.color = "#f97316") +
            ggplot2::geom_jitter(width = 0.15, alpha = 0.35, color = "#93c5fd", size = 1.7) +
            ggplot2::coord_flip() +
            ggplot2::labs(
              title = paste(num_vars[1], "by", paste(cat_vars[1:2], collapse = " + ")),
              subtitle = "Groups are ordered by median; points show individual observations",
              x = paste(cat_vars[1:2], collapse = " + "),
              y = num_vars[1]
            ) +
            va_plot_theme()
        } else if (length(num_vars) == 1 && length(cat_vars) == 1) {
          plot_df <- data.frame(y = df_sel[[num_vars[1]]], g = as.factor(df_sel[[cat_vars[1]]]))
          plot_df <- plot_df[complete.cases(plot_df), , drop = FALSE]
          if (nrow(plot_df) < 2) {
            plot.new()
            text(0.5, 0.5, "Not enough complete rows for this grouped boxplot.", cex = 1.1)
            return()
          }
          plot_df$g <- stats::reorder(plot_df$g, plot_df$y, median, na.rm = TRUE)
          ggplot2::ggplot(plot_df, ggplot2::aes(x = g, y = y, fill = g)) +
            ggplot2::geom_boxplot(alpha = 0.62, outlier.color = "#f97316") +
            ggplot2::geom_jitter(width = 0.16, alpha = 0.35, color = "#dbeafe", size = 1.8) +
            ggplot2::scale_fill_manual(values = va_palette(length(levels(plot_df$g)), "Set2")) +
            ggplot2::coord_flip() +
            ggplot2::labs(
              title = paste(num_vars[1], "by", cat_vars[1]),
              subtitle = "Groups are ordered by median; points reveal sample size and spread",
              x = cat_vars[1],
              y = num_vars[1],
              fill = cat_vars[1]
            ) +
            va_plot_theme() +
            ggplot2::theme(legend.position = "none")
        } else if (length(cat_vars) >= 3) {
          plot_df <- df_sel[complete.cases(df_sel[, cat_vars[1:3], drop = FALSE]), cat_vars[1:3], drop = FALSE]
          if (nrow(plot_df) < 2) {
            plot.new()
            text(0.5, 0.5, "Not enough complete rows for this mosaic plot.", cex = 1.1)
            return()
          }
          count_df <- as.data.frame(table(plot_df))
          names(count_df) <- c("Var1", "Var2", "Var3", "Freq")
          ggplot2::ggplot(count_df, ggplot2::aes(x = Var2, y = Var1, fill = Freq)) +
            ggplot2::geom_tile(color = "#0f172a", linewidth = 0.45) +
            ggplot2::geom_text(ggplot2::aes(label = ifelse(Freq > 0, Freq, "")), color = "#f8fafc", size = 3) +
            ggplot2::facet_wrap(~Var3) +
            ggplot2::scale_fill_gradient(low = "#111c30", high = "#38bdf8") +
            ggplot2::labs(
              title = paste("Count heatmap:", cat_vars[1], "by", cat_vars[2]),
              subtitle = paste("Faceted by", cat_vars[3]),
              x = cat_vars[2],
              y = cat_vars[1],
              fill = "Count"
            ) +
            va_plot_theme() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
        } else if (length(cat_vars) == 2) {
          plot_df <- df_sel[complete.cases(df_sel[, cat_vars[1:2], drop = FALSE]), cat_vars[1:2], drop = FALSE]
          if (nrow(plot_df) < 2) {
            plot.new()
            text(0.5, 0.5, "Not enough complete rows for this mosaic plot.", cex = 1.1)
            return()
          }
          count_df <- as.data.frame(table(plot_df[[cat_vars[1]]], plot_df[[cat_vars[2]]]))
          names(count_df) <- c("Var1", "Var2", "Freq")
          ggplot2::ggplot(count_df, ggplot2::aes(x = Var2, y = Var1, fill = Freq)) +
            ggplot2::geom_tile(color = "#0f172a", linewidth = 0.5) +
            ggplot2::geom_text(ggplot2::aes(label = ifelse(Freq > 0, Freq, "")), color = "#f8fafc", size = 3.5) +
            ggplot2::scale_fill_gradient(low = "#111c30", high = "#38bdf8") +
            ggplot2::labs(
              title = paste("Count heatmap:", cat_vars[1], "vs", cat_vars[2]),
              subtitle = "Darker cells have fewer observations; brighter cells have more",
              x = cat_vars[2],
              y = cat_vars[1],
              fill = "Count"
            ) +
            va_plot_theme() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
        } else {
          plot.new()
          text(0.5, 0.5, "Not enough valid variables selected for plotting.", cex = 1.1)
        }
      })
      if (HAS_PLOTLY) {
        output$twoVarPlotly <- plotly::renderPlotly({
          if (length(num_vars) >= 2 && length(cat_vars) >= 1) {
            complete_rows <- complete.cases(df_sel[, c(num_vars[1:2], cat_vars[1]), drop = FALSE])
            req(sum(complete_rows) >= 2)
            plotly::plot_ly(
              x = df_sel[[num_vars[1]]][complete_rows],
              y = df_sel[[num_vars[2]]][complete_rows],
              color = as.factor(df_sel[[cat_vars[1]]][complete_rows]),
              type = "scatter", mode = "markers",
              marker = list(opacity = 0.7)
            ) |>
              plotly::layout(
                title = paste(num_vars[1], "vs", num_vars[2], "colored by", cat_vars[1]),
                xaxis = list(title = num_vars[1]),
                yaxis = list(title = num_vars[2])
              )
          } else if (length(num_vars) >= 3) {
            complete_numeric <- df_sel[complete.cases(df_sel[, num_vars, drop = FALSE]), num_vars, drop = FALSE]
            req(nrow(complete_numeric) >= 2)
            cm <- cor(complete_numeric, use = "complete.obs")
            plotly::plot_ly(
              x = colnames(cm), y = rownames(cm), z = cm,
              type = "heatmap",
              colors = c("#ef4444", "#1e293b", "#22c55e"),
              zmin = -1, zmax = 1,
              text = round(cm, 3),
              hovertemplate = "x: %{x}<br>y: %{y}<br>r: %{z:.3f}<extra></extra>"
            ) |>
              plotly::layout(
                title = "Correlation heatmap",
                xaxis = list(title = ""),
                yaxis = list(title = "")
              )
          } else if (length(num_vars) >= 2) {
            x <- df_sel[[num_vars[1]]]
            y <- df_sel[[num_vars[2]]]
            complete_rows <- complete.cases(x, y)
            req(sum(complete_rows) >= 2)
            plotly::plot_ly(
              x = x[complete_rows], y = y[complete_rows], type = "scatter", mode = "markers",
              marker = list(color = "#3b82f6", opacity = 0.65)
            ) |>
              plotly::layout(
                title = paste(num_vars[1], "vs", num_vars[2]),
                xaxis = list(title = num_vars[1]),
                yaxis = list(title = num_vars[2])
              )
          } else if (length(num_vars) == 1 && length(cat_vars) >= 2) {
            plot_df <- data.frame(y = df_sel[[num_vars[1]]], df_sel[, cat_vars[1:2], drop = FALSE])
            plot_df <- plot_df[complete.cases(plot_df), , drop = FALSE]
            req(nrow(plot_df) >= 2)
            plotly::plot_ly(
              x = interaction(plot_df[, cat_vars[1:2], drop = FALSE], drop = TRUE),
              y = plot_df$y, type = "box", boxpoints = "all", jitter = 0.25,
              marker = list(color = "#38bdf8", opacity = 0.45),
              line = list(color = "#dbeafe")
            ) |>
              plotly::layout(
                title = paste(num_vars[1], "by", paste(cat_vars[1:2], collapse = " + ")),
                xaxis = list(title = paste(cat_vars[1:2], collapse = " + ")),
                yaxis = list(title = num_vars[1])
              )
          } else if (length(num_vars) == 1 && length(cat_vars) == 1) {
            plot_df <- data.frame(y = df_sel[[num_vars[1]]], g = as.factor(df_sel[[cat_vars[1]]]))
            plot_df <- plot_df[complete.cases(plot_df), , drop = FALSE]
            req(nrow(plot_df) >= 2)
            plotly::plot_ly(
              x = plot_df$g,
              y = plot_df$y, type = "box", boxpoints = "all", jitter = 0.25,
              marker = list(color = "#38bdf8", opacity = 0.45),
              line = list(color = "#dbeafe")
            )
          } else if (length(cat_vars) >= 2) {
            plot_df <- df_sel[complete.cases(df_sel[, cat_vars[1:2], drop = FALSE]), cat_vars[1:2], drop = FALSE]
            req(nrow(plot_df) >= 2)
            t12 <- table(plot_df[[cat_vars[1]]], plot_df[[cat_vars[2]]])
            plotly::plot_ly(
              x = colnames(t12), y = rownames(t12), z = unclass(t12),
              type = "heatmap",
              colors = c("#111c30", "#38bdf8"),
              hovertemplate = paste(cat_vars[2], ": %{x}<br>", cat_vars[1], ": %{y}<br>Count: %{z}<extra></extra>")
            ) |>
              plotly::layout(
                title = paste("Count heatmap:", cat_vars[1], "vs", cat_vars[2]),
                xaxis = list(title = cat_vars[2]),
                yaxis = list(title = cat_vars[1])
              )
          } else {
            plotly::plot_ly()
          }
        })
      }
      incProgress(0.40)
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

    cat_data <- clean_cat_vector(data$raw[[input$catVar]])
    freq_table <- table(cat_data, useNA = "no")
    prop_table <- prop.table(freq_table)
    missing_n <- sum(is.na(cat_data))
    total_n <- length(cat_data)
    freq_df <- cat_count_df(cat_data)

    output$catFrequency <- renderUI({
      if (length(freq_table) == 0) {
        return(tags$div(class = "va-empty", "No non-missing categories are available for this variable."))
      }
      mode_name <- names(which.max(freq_table))
      mode_count <- max(freq_table)
      mode_share <- mode_count / sum(freq_table)
      concentration_status <- if (mode_share >= 0.75) "warn" else "ok"

      return(tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Non-missing rows", format(sum(freq_table), big.mark = ","), paste("Out of", format(total_n, big.mark = ",")), "ok"),
          va_metric("Missing / blank", format(missing_n, big.mark = ","), "Excluded from percentages", ifelse(missing_n == 0, "ok", "warn")),
          va_metric("Distinct levels", length(freq_table), "Categories observed", "neutral"),
          va_metric("Most common level", mode_name, paste0(mode_count, " rows (", round(100 * mode_share, 1), "%)"), concentration_status)
        ),
        va_interpretation(
          paste("Categorical profile for", input$catVar),
          if (mode_share >= 0.75) {
            "One category dominates this variable. Analyses involving rare categories may be unstable or hard to interpret."
          } else {
            "The variable has a usable spread across categories. Review the bar chart for small groups before comparing categories."
          },
          pills = tagList(
            va_pill(paste("Top share:", paste0(round(100 * mode_share, 1), "%")), concentration_status, ifelse(concentration_status == "ok", "check-circle", "exclamation-triangle")),
            va_pill(paste("Levels:", length(freq_table)), "neutral", "tags")
          )
        )
      ))
    })

    output$catProportion <- renderUI({
      if (length(freq_table) == 0) {
        return(NULL)
      }
      display_df <- freq_df[, c("Category", "Count", "Percent"), drop = FALSE]
      tagList(
        tags$h5("Frequency and Share"),
        va_table(display_df),
        va_details(
          "Technical details: full frequency/proportion table",
          c(
            paste("Variable:", input$catVar),
            "Frequency table:",
            capture.output(print(freq_table)),
            "",
            "Proportion table:",
            capture.output(print(round(prop_table, 4)))
          )
        )
      )
    })

    output$catPlot <- renderPlot({
      req(nrow(freq_df) > 0)
      plot_df <- cat_count_df(cat_data, max_levels = 18)
      plot_df$Category <- factor(plot_df$Category, levels = rev(plot_df$Category))
      y_var <- if (identical(input$catPlotType, "Share Bar")) "Share" else "Count"
      y_lab <- if (identical(input$catPlotType, "Share Bar")) "Share of non-missing rows" else "Count"
      ggplot2::ggplot(plot_df, ggplot2::aes(x = Category, y = .data[[y_var]], fill = Share)) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::geom_text(
          ggplot2::aes(label = if (identical(input$catPlotType, "Share Bar")) Percent else Count),
          hjust = -0.1, color = "#e5e7eb", size = 3.6
        ) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_fill_gradient(low = "#2563eb", high = "#22c55e", labels = function(x) paste0(round(100 * x), "%")) +
        ggplot2::labs(
          title = paste("Category distribution:", input$catVar),
          subtitle = "Sorted by frequency; low-frequency levels may be combined into 'Other levels'",
          x = NULL,
          y = y_lab,
          fill = "Share"
        ) +
        va_plot_theme() +
        ggplot2::theme(plot.margin = ggplot2::margin(10, 50, 10, 10))
    })
    if (HAS_PLOTLY) {
      output$catPlotly <- plotly::renderPlotly({
        req(nrow(freq_df) > 0)
        dfp <- cat_count_df(cat_data, max_levels = 18)
        y <- if (identical(input$catPlotType, "Share Bar")) dfp$Share else dfp$Count
        plotly::plot_ly(
          dfp,
          x = y,
          y = ~reorder(Category, Count),
          type = "bar",
          orientation = "h",
          text = if (identical(input$catPlotType, "Share Bar")) ~Percent else ~Count,
          textposition = "outside",
          marker = list(color = "#38bdf8")
        ) |>
          plotly::layout(
            title = paste("Category distribution:", input$catVar),
            xaxis = list(title = if (identical(input$catPlotType, "Share Bar")) "Share" else "Count"),
            yaxis = list(title = "")
          )
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

    var1_data <- clean_cat_vector(data$raw[[input$crosstabVar1]])
    var2_data <- clean_cat_vector(data$raw[[input$crosstabVar2]])
    complete_idx <- complete.cases(var1_data, var2_data)

    cont_table <- table(var1_data[complete_idx], var2_data[complete_idx])
    joint_prop <- prop.table(cont_table)
    row_prop <- prop.table(cont_table, 1)
    chi_test <- tryCatch(suppressWarnings(chisq.test(cont_table)), error = function(e) NULL)
    cramers_v <- if (!is.null(chi_test) && min(dim(cont_table)) > 1) {
      sqrt(unname(chi_test$statistic) / (sum(cont_table) * (min(dim(cont_table)) - 1)))
    } else {
      NA_real_
    }

    output$crosstabTable <- renderUI({
      if (sum(cont_table) == 0) {
        return(tags$div(class = "va-empty", "No complete rows are available for this cross-tabulation."))
      }
      return(tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Complete rows", format(sum(cont_table), big.mark = ","), paste("Missing in either variable:", format(sum(!complete_idx), big.mark = ",")), ifelse(sum(!complete_idx) == 0, "ok", "warn")),
          va_metric("Row levels", nrow(cont_table), input$crosstabVar1, "neutral"),
          va_metric("Column levels", ncol(cont_table), input$crosstabVar2, "neutral"),
          va_metric("Largest cell", max(cont_table), "Highest observed combination", "neutral")
        ),
        va_interpretation(
          paste(input$crosstabVar1, "by", input$crosstabVar2),
          "The contingency table shows how often each combination of the two categories occurs. Use the heatmap below to quickly spot concentrations or sparse cells.",
          pills = tagList(
            va_pill("Counts", "neutral", "table"),
            va_pill("Missing rows excluded", ifelse(sum(!complete_idx) == 0, "ok", "warn"), ifelse(sum(!complete_idx) == 0, "check-circle", "exclamation-triangle"))
          )
        ),
        va_table(cbind(data.frame(Level = rownames(cont_table), check.names = FALSE), as.data.frame.matrix(cont_table)))
      ))
    })

    output$crosstabProp <- renderUI({
      if (sum(cont_table) == 0) {
        return(NULL)
      }
      prop_df <- cbind(data.frame(Level = rownames(cont_table), check.names = FALSE), as.data.frame.matrix(round(joint_prop, 4)))
      row_df <- cbind(data.frame(Level = rownames(cont_table), check.names = FALSE), as.data.frame.matrix(round(row_prop, 4)))
      tagList(
        va_interpretation(
          "Proportions",
          "Joint proportions show each cell's share of all complete rows. Row proportions show the distribution within each row level, which is often easier to interpret for comparisons.",
          pills = tagList(
            va_pill("Joint proportions", "neutral", "percentage"),
            va_pill("Row proportions", "neutral", "layer-group")
          )
        ),
        tags$h5("Joint Proportions"),
        va_table(prop_df),
        tags$br(),
        tags$h5("Row Proportions"),
        va_table(row_df)
      )
    })

    output$chiSquareTest <- renderUI({
      if (is.null(chi_test)) {
        return(tags$div(class = "va-empty", "Chi-square test could not be computed. This usually happens when a variable has too few observed levels."))
      }
      expected_warning <- any(chi_test$expected < 5)
      status <- p_status(chi_test$p.value)
      return(tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("p-value", format.pval(chi_test$p.value, digits = 4), "H0: variables are independent", status),
          va_metric("Chi-square", fmt_value(unname(chi_test$statistic)), paste("df =", fmt_value(unname(chi_test$parameter), 0)), "neutral"),
          va_metric("Cramer's V", fmt_value(cramers_v, 3), cramers_label(cramers_v), cramers_status(cramers_v)),
          va_metric("Expected counts", ifelse(expected_warning, "Sparse cells", "Adequate"), "Rule of thumb: expected counts should mostly be >= 5", ifelse(expected_warning, "warn", "ok"))
        ),
        va_interpretation(
          "Chi-square test of independence",
          paste(
            p_label(chi_test$p.value),
            "A small p-value suggests the two categorical variables are associated. Cramer's V describes practical association strength; it is not a causal measure."
          ),
          pills = tagList(
            va_pill("p < 0.05: evidence of association", "ok", "check-circle"),
            va_pill(ifelse(expected_warning, "Expected-count warning", "Expected counts OK"), ifelse(expected_warning, "warn", "ok"), ifelse(expected_warning, "exclamation-triangle", "check-circle"))
          )
        ),
        va_details(
          "Technical details: chi-square output",
          c(
            capture.output(print(chi_test)),
            "",
            paste("Cramer's V:", fmt_value(cramers_v, 4)),
            paste("Any expected count < 5:", ifelse(expected_warning, "Yes", "No"))
          )
        )
      ))

      cat("\n\nCramér's V:\n")
    })

    output$crosstabPlot <- renderPlot({
      req(sum(cont_table) > 0)
      plot_df <- as.data.frame(cont_table)
      names(plot_df) <- c("Row", "Column", "Count")
      plot_df$ColumnShare <- ave(plot_df$Count, plot_df$Column, FUN = function(x) x / sum(x))

      if (input$crosstabPlotType == "Stacked Share Bar") {
        ggplot2::ggplot(plot_df, ggplot2::aes(x = Column, y = ColumnShare, fill = Row)) +
          ggplot2::geom_col(position = "stack", width = 0.72) +
          ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
          ggplot2::scale_fill_manual(values = va_palette(length(unique(plot_df$Row)), "Set2")) +
          ggplot2::labs(
            title = paste("Composition of", input$crosstabVar1, "within", input$crosstabVar2),
            subtitle = "Each bar sums to 100%; useful for comparing category mix",
            x = input$crosstabVar2,
            y = "Share within column",
            fill = input$crosstabVar1
          ) +
          va_plot_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
      } else if (input$crosstabPlotType == "Grouped Count Bar") {
        ggplot2::ggplot(plot_df, ggplot2::aes(x = Column, y = Count, fill = Row)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.68) +
          ggplot2::scale_fill_manual(values = va_palette(length(unique(plot_df$Row)), "Set2")) +
          ggplot2::labs(
            title = paste("Grouped counts:", input$crosstabVar1, "vs", input$crosstabVar2),
            subtitle = "Side-by-side bars emphasize absolute frequency differences",
            x = input$crosstabVar2,
            y = "Count",
            fill = input$crosstabVar1
          ) +
          va_plot_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
      } else {
        ggplot2::ggplot(plot_df, ggplot2::aes(x = Column, y = Row, fill = Count)) +
          ggplot2::geom_tile(color = "#0f172a", linewidth = 0.5) +
          ggplot2::geom_text(ggplot2::aes(label = ifelse(Count > 0, Count, "")), color = "#f8fafc", size = 3.8) +
          ggplot2::scale_fill_gradient(low = "#111c30", high = "#38bdf8") +
          ggplot2::labs(
            title = paste("Count heatmap:", input$crosstabVar1, "vs", input$crosstabVar2),
            subtitle = "Brighter cells have more observations",
            x = input$crosstabVar2,
            y = input$crosstabVar1,
            fill = "Count"
          ) +
          va_plot_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
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
    output$multiCatTable <- renderUI({
      flat_table <- as.data.frame(multi_table)
      flat_table <- flat_table[order(flat_table$Freq, decreasing = TRUE), , drop = FALSE]
      observed_combos <- sum(flat_table$Freq > 0)
      total_combos <- nrow(flat_table)
      top_table <- head(flat_table[flat_table$Freq > 0, , drop = FALSE], 15)
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Variables", length(selected_vars), paste(selected_vars, collapse = ", "), "neutral"),
          va_metric("Possible combinations", format(total_combos, big.mark = ","), paste("Dimensions:", paste(dim(multi_table), collapse = " x ")), "neutral"),
          va_metric("Observed combinations", format(observed_combos, big.mark = ","), paste0(round(100 * observed_combos / max(1, total_combos), 1), "% non-empty"), ifelse(observed_combos / max(1, total_combos) < 0.35, "warn", "ok")),
          va_metric("Total complete rows", format(sum(multi_table), big.mark = ","), "Rows used in the multi-way table", "ok")
        ),
        va_interpretation(
          "Multi-way contingency table",
          "This table summarizes the most common observed category combinations. Sparse combinations are common with 3+ categorical variables, so interpret rare patterns cautiously.",
          pills = tagList(
            va_pill("Top observed combinations shown", "neutral", "table"),
            va_pill(ifelse(observed_combos / max(1, total_combos) < 0.35, "Sparse table", "Coverage OK"), ifelse(observed_combos / max(1, total_combos) < 0.35, "warn", "ok"), ifelse(observed_combos / max(1, total_combos) < 0.35, "exclamation-triangle", "check-circle"))
          )
        ),
        va_table(top_table),
        va_details(
          "Technical details: full multi-way table",
          c(
            paste("Variables:", paste(selected_vars, collapse = ", ")),
            paste("Dimensions:", paste(dim(multi_table), collapse = " x ")),
            capture.output(print(multi_table))
          )
        )
      )
    })

    # Conditional proportions
    output$multiCatProportions <- renderUI({
      prop_table <- prop.table(multi_table)
      cond_prop <- prop.table(multi_table, 1)
      flat_prop <- as.data.frame(prop_table)
      names(flat_prop)[ncol(flat_prop)] <- "Proportion"
      flat_prop <- flat_prop[order(flat_prop$Proportion, decreasing = TRUE), , drop = FALSE]
      flat_prop$Percent <- paste0(round(100 * flat_prop$Proportion, 2), "%")
      flat_prop$Proportion <- round(flat_prop$Proportion, 4)
      tagList(
        va_interpretation(
          "Proportions",
          paste("Overall proportions are relative to all complete rows. Conditional proportions are calculated within", selected_vars[1], "and are available in technical details."),
          pills = tagList(
            va_pill("Overall proportions", "neutral", "percentage"),
            va_pill(paste("Conditioned by", selected_vars[1]), "neutral", "layer-group")
          )
        ),
        va_table(head(flat_prop, 15)),
        va_details(
          "Technical details: conditional proportions",
          c(
            "Proportions relative to total:",
            capture.output(print(round(prop_table, 4))),
            "",
            paste("Conditional proportions by", selected_vars[1], ":"),
            capture.output(print(round(cond_prop, 4)))
          )
        )
      )
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

    pairwise_results <- list()
    for (i in 1:(length(selected_vars) - 1)) {
      for (j in (i + 1):length(selected_vars)) {
        test_table <- table(multi_data[[selected_vars[i]]], multi_data[[selected_vars[j]]])
        res <- tryCatch(
          {
            chi_test <- suppressWarnings(chisq.test(test_table))
            v <- if (min(dim(test_table)) > 1) {
              sqrt(unname(chi_test$statistic) / (sum(test_table) * (min(dim(test_table)) - 1)))
            } else {
              NA_real_
            }
            list(
              var1 = selected_vars[i], var2 = selected_vars[j],
              statistic = unname(chi_test$statistic), df = unname(chi_test$parameter),
              p = chi_test$p.value, v = v, low_expected = any(chi_test$expected < 5),
              technical = capture.output(print(chi_test)), error = NULL
            )
          },
          error = function(e) {
            list(var1 = selected_vars[i], var2 = selected_vars[j], error = e$message)
          }
        )
        pairwise_results[[length(pairwise_results) + 1]] <- res
      }
    }

    output$multiCatChiSquare <- renderUI({
      result_df <- do.call(rbind, lapply(pairwise_results, function(x) {
        if (!is.null(x$error)) {
          data.frame(Comparison = paste(x$var1, "vs", x$var2), P_value = "N/A", Result = x$error, Expected_Counts = "N/A", check.names = FALSE)
        } else {
          data.frame(
            Comparison = paste(x$var1, "vs", x$var2),
            P_value = format.pval(x$p, digits = 4),
            Result = p_label(x$p),
            Expected_Counts = ifelse(isTRUE(x$low_expected), "Warning", "OK"),
            check.names = FALSE
          )
        }
      }))
      significant_n <- sum(vapply(pairwise_results, function(x) is.null(x$error) && !is.na(x$p) && x$p < 0.05, logical(1)))
      warning_n <- sum(vapply(pairwise_results, function(x) is.null(x$error) && isTRUE(x$low_expected), logical(1)))
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Pairwise tests", length(pairwise_results), "All selected variable pairs", "neutral"),
          va_metric("Significant pairs", significant_n, "p < 0.05", ifelse(significant_n > 0, "ok", "neutral")),
          va_metric("Expected-count warnings", warning_n, "Sparse cells can weaken chi-square reliability", ifelse(warning_n > 0, "warn", "ok")),
          va_metric("Decision rule", "p < 0.05", "Evidence against independence", "neutral")
        ),
        va_interpretation(
          "Pairwise chi-square tests",
          "For 3+ categorical variables, the app summarizes pairwise independence tests. A significant result means the two variables show evidence of association, but it does not describe all higher-order interactions.",
          pills = tagList(
            va_pill("H0: pair is independent", "neutral", "calculator"),
            va_pill("Sparse tables need caution", ifelse(warning_n > 0, "warn", "ok"), ifelse(warning_n > 0, "exclamation-triangle", "check-circle"))
          )
        ),
        va_table(result_df),
        va_details(
          "Technical details: chi-square outputs",
          unlist(lapply(pairwise_results, function(x) {
            if (!is.null(x$error)) {
              return(c(paste(x$var1, "vs", x$var2), paste("Error:", x$error), ""))
            }
            c(
              paste(x$var1, "vs", x$var2),
              x$technical,
              paste("Expected-count warning:", ifelse(isTRUE(x$low_expected), "Yes", "No")),
              ""
            )
          }))
        )
      )
    })

    output$multiCatAssociation <- renderUI({
      assoc_df <- do.call(rbind, lapply(pairwise_results, function(x) {
        if (!is.null(x$error)) {
          data.frame(Comparison = paste(x$var1, "vs", x$var2), Cramers_V = "N/A", Strength = "Unable to compute", check.names = FALSE)
        } else {
          data.frame(
            Comparison = paste(x$var1, "vs", x$var2),
            Cramers_V = fmt_value(x$v, 4),
            Strength = cramers_label(x$v),
            check.names = FALSE
          )
        }
      }))
      max_v <- suppressWarnings(max(vapply(pairwise_results, function(x) if (is.null(x$error)) x$v else NA_real_, numeric(1)), na.rm = TRUE))
      if (!is.finite(max_v)) max_v <- NA_real_
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Strongest Cramer's V", fmt_value(max_v, 3), cramers_label(max_v), cramers_status(max_v)),
          va_metric("Pairs evaluated", nrow(assoc_df), "Pairwise practical association", "neutral")
        ),
        va_interpretation(
          "Association strength",
          "Cramer's V ranges from 0 to 1. Larger values indicate stronger categorical association, but the scale is context-dependent and should be interpreted with sample size and table sparsity.",
          pills = tagList(
            va_pill("< 0.10 negligible", "ok", "check-circle"),
            va_pill("0.30+ moderate/strong", "warn", "exclamation-triangle")
          )
        ),
        va_table(assoc_df)
      )
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

    output$multiCatPlot <- renderPlot({
      req(multi_table)
      plot_data <- multi_data[complete.cases(multi_data[, selected_vars, drop = FALSE]), selected_vars, drop = FALSE]
      if (nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No complete rows are available for the selected categorical variables.", cex = 1.1)
        return()
      }

      if (input$multiCatPlotType == "Stacked Share Bar") {
        count_df <- as.data.frame(table(plot_data[[selected_vars[1]]], plot_data[[selected_vars[2]]]))
        names(count_df) <- c("Group", "Category", "Count")
        count_df$Share <- ave(count_df$Count, count_df$Category, FUN = function(x) x / sum(x))
        ggplot2::ggplot(count_df, ggplot2::aes(x = Category, y = Share, fill = Group)) +
          ggplot2::geom_col(width = 0.72) +
          ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
          ggplot2::scale_fill_manual(values = va_palette(length(unique(count_df$Group)), "Set2")) +
          ggplot2::labs(
            title = paste("Composition of", selected_vars[1], "within", selected_vars[2]),
            subtitle = "Each bar sums to 100%",
            x = selected_vars[2],
            y = "Share",
            fill = selected_vars[1]
          ) +
          va_plot_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
      } else if (input$multiCatPlotType == "Grouped Count Bar") {
        count_df <- as.data.frame(table(plot_data[[selected_vars[1]]], plot_data[[selected_vars[2]]]))
        names(count_df) <- c("Group", "Category", "Count")
        ggplot2::ggplot(count_df, ggplot2::aes(x = Category, y = Count, fill = Group)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.68) +
          ggplot2::scale_fill_manual(values = va_palette(length(unique(count_df$Group)), "Set2")) +
          ggplot2::labs(
            title = paste("Grouped counts:", selected_vars[1], "vs", selected_vars[2]),
            subtitle = "First two selected variables are displayed",
            x = selected_vars[2],
            y = "Count",
            fill = selected_vars[1]
          ) +
          va_plot_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
      } else if (input$multiCatPlotType == "Faceted Heatmap" && length(selected_vars) >= 3) {
        count_df <- as.data.frame(table(plot_data[, selected_vars[1:3], drop = FALSE]))
        names(count_df) <- c("Var1", "Var2", "Var3", "Count")
        ggplot2::ggplot(count_df, ggplot2::aes(x = Var2, y = Var1, fill = Count)) +
          ggplot2::geom_tile(color = "#0f172a", linewidth = 0.45) +
          ggplot2::geom_text(ggplot2::aes(label = ifelse(Count > 0, Count, "")), color = "#f8fafc", size = 3) +
          ggplot2::facet_wrap(~Var3) +
          ggplot2::scale_fill_gradient(low = "#111c30", high = "#38bdf8") +
          ggplot2::labs(
            title = paste("Count heatmap:", selected_vars[1], "by", selected_vars[2]),
            subtitle = paste("Faceted by", selected_vars[3]),
            x = selected_vars[2],
            y = selected_vars[1],
            fill = "Count"
          ) +
          va_plot_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
      } else {
        heat_vars <- input$heatmapVars
        if (is.null(heat_vars) || length(heat_vars) != 2) {
          heat_vars <- selected_vars[1:2]
        }
        count_df <- as.data.frame(table(plot_data[[heat_vars[1]]], plot_data[[heat_vars[2]]]))
        names(count_df) <- c("Var1", "Var2", "Count")
        ggplot2::ggplot(count_df, ggplot2::aes(x = Var2, y = Var1, fill = Count)) +
          ggplot2::geom_tile(color = "#0f172a", linewidth = 0.5) +
          ggplot2::geom_text(ggplot2::aes(label = ifelse(Count > 0, Count, "")), color = "#f8fafc", size = 3.6) +
          ggplot2::scale_fill_gradient(low = "#111c30", high = "#38bdf8") +
          ggplot2::labs(
            title = paste("Count heatmap:", heat_vars[1], "vs", heat_vars[2]),
            subtitle = "Brighter cells have more observations",
            x = heat_vars[2],
            y = heat_vars[1],
            fill = "Count"
          ) +
          va_plot_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
      }
    })

    # Insights
    output$multiCatInsights <- renderUI({
      total_combos <- prod(dim(multi_table))
      observed_combos <- sum(multi_table > 0)
      coverage <- observed_combos / max(1, total_combos)
      flat_table <- as.data.frame(multi_table)
      most_common_idx <- which.max(flat_table$Freq)
      most_common <- flat_table[most_common_idx, ]
      pattern <- paste(
        names(most_common)[1:(length(names(most_common)) - 1)],
        "=",
        unlist(most_common[1:(length(most_common) - 1)]),
        collapse = ", "
      )
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Observed coverage", paste0(round(100 * coverage, 1), "%"), paste(observed_combos, "of", total_combos, "combinations"), ifelse(coverage < 0.35, "warn", "ok")),
          va_metric("Most common pattern", most_common$Freq, pattern, "neutral")
        ),
        va_interpretation(
          "How to read this page",
          "Use the heatmap to identify dense and sparse combinations, then compare the pairwise chi-square and Cramer's V summaries to separate statistical evidence from practical association strength.",
          pills = tagList(
            va_pill("Heatmap: where counts concentrate", "neutral", "th"),
            va_pill("Chi-square: evidence", "neutral", "calculator"),
            va_pill("Cramer's V: strength", "neutral", "signal")
          )
        )
      )
    })
  })

  # ===== HYPOTHESIS TESTS =====

  output$testVarInputs <- renderUI({
    req(data$raw)

    if (input$testType %in% c("One-Sample T-Test", "One-Sample Wilcoxon Test")) {
      tagList(
        selectInput("testVar1", "Select Variable:", choices = data$numeric_vars),
        numericInput("testMu", "Hypothesized Mean (μ₀):", value = 0)
      )
    } else if (input$testType == "Normality Test (Shapiro-Wilk)") {
      tagList(
        selectInput("testVar1", "Select Variable:", choices = data$numeric_vars)
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

    alpha <- 0.05

    decision_sentence <- function(p, reject_label = "Reject H0", keep_label = "Fail to reject H0") {
      if (is.na(p)) return("The test did not return a usable p-value.")
      if (p < alpha) {
        paste0(reject_label, " at alpha = ", alpha, ". The result is statistically significant.")
      } else {
        paste0(keep_label, " at alpha = ", alpha, ". The result is not statistically significant.")
      }
    }

    htest_summary_ui <- function(title, subtitle, test_obj, metric_cards, interpretation, assumptions, status = p_status(test_obj$p.value), extra_lines = NULL) {
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("p-value", format.pval(test_obj$p.value, digits = 4), paste("Decision rule: p <", alpha), status),
          metric_cards
        ),
        va_interpretation(
          title,
          interpretation,
          pills = tagList(
            va_pill(ifelse(test_obj$p.value < alpha, "Reject H0", "Fail to reject H0"), status, ifelse(test_obj$p.value < alpha, "check-circle", "info-circle")),
            va_pill(subtitle, "neutral", "calculator")
          )
        ),
        va_interpretation("Assumptions and limitations", assumptions),
        va_details(
          "Technical details: R test output",
          c(capture.output(print(test_obj)), extra_lines)
        )
      )
    }

    empty_test_ui <- function(message) {
      tags$div(class = "va-empty", message)
    }

    plot_distribution <- function(x, var_name, mu = NULL) {
      x <- x[is.finite(x)]
      plot_df <- data.frame(value = x)
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ggplot2::after_stat(density)),
          bins = min(35, max(8, ceiling(sqrt(length(x))))),
          fill = "#38bdf8", color = "#0f172a", alpha = 0.78
        ) +
        ggplot2::geom_rug(color = "#93c5fd", alpha = 0.35) +
        ggplot2::labs(
          title = paste("Distribution of", var_name),
          subtitle = "Histogram scaled to density; reference line shown when applicable",
          x = var_name,
          y = "Density"
        ) +
        va_plot_theme()
      if (length(unique(x)) > 1) {
        p <- p + ggplot2::geom_density(color = "#f8fafc", linewidth = 1.1)
      }
      if (!is.null(mu) && is.finite(mu)) {
        p <- p + ggplot2::geom_vline(xintercept = mu, color = "#f97316", linewidth = 1.1, linetype = "dashed")
      }
      print(p)
    }

    plot_group_box <- function(df, group_label, measure_label, title) {
      ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(g, y, median, na.rm = TRUE), y = y, fill = g)) +
        ggplot2::geom_boxplot(alpha = 0.62, outlier.color = "#f97316") +
        ggplot2::geom_jitter(width = 0.16, alpha = 0.35, color = "#dbeafe", size = 1.8) +
        ggplot2::scale_fill_manual(values = va_palette(nlevels(df$g), "Set2")) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = title,
          subtitle = "Groups are ordered by median; points show individual observations",
          x = group_label,
          y = measure_label,
          fill = group_label
        ) +
        va_plot_theme() +
        ggplot2::theme(legend.position = "none")
    }

    if (input$testType == "One-Sample T-Test") {
      test_data <- data$raw[[input$testVar1]]
      x <- test_data[is.finite(test_data)]

      output$testResults <- renderUI({
        if (length(x) < 2) {
          return(empty_test_ui("One-sample t-test requires at least 2 finite numeric values."))
        }
        t_test <- t.test(x, mu = input$testMu)
        htest_summary_ui(
          "One-sample t-test",
          "Tests whether the mean differs from the hypothesized mean",
          t_test,
          tagList(
            va_metric("Sample mean", fmt_value(mean(x)), paste("Hypothesized mean:", fmt_value(input$testMu)), "neutral"),
            va_metric("t-statistic", fmt_value(unname(t_test$statistic)), paste("df =", fmt_value(unname(t_test$parameter), 2)), "neutral"),
            va_metric("95% CI", paste0("[", fmt_value(t_test$conf.int[1]), ", ", fmt_value(t_test$conf.int[2]), "]"), "For the population mean", "neutral")
          ),
          decision_sentence(t_test$p.value),
          "Assumes observations are independent and the sampling distribution of the mean is approximately normal. For small samples, strong skew or outliers can affect reliability.",
          p_status(t_test$p.value),
          paste("Variable:", input$testVar1)
        )
      })

      output$testPlot <- renderPlot({
        req(length(x) >= 2)
        plot_distribution(x, input$testVar1, input$testMu)
      })
    } else if (input$testType == "Normality Test (Shapiro-Wilk)") {
      test_data <- data$raw[[input$testVar1]]
      x <- test_data[is.finite(test_data)]
      sample_size <- min(5000, length(x))
      sampled_x <- if (sample_size > 0) sample(x, sample_size) else numeric(0)

      output$testResults <- renderUI({
        if (length(sampled_x) < 3) {
          return(empty_test_ui("Shapiro-Wilk normality test requires at least 3 finite numeric values."))
        }
        shapiro_test <- shapiro.test(sampled_x)
        norm_status <- if (shapiro_test$p.value < alpha) "bad" else "ok"
        htest_summary_ui(
          "Shapiro-Wilk normality test",
          "Tests whether the distribution departs from normality",
          shapiro_test,
          tagList(
            va_metric("W-statistic", fmt_value(unname(shapiro_test$statistic)), "Closer to 1 is more normal-looking", "neutral"),
            va_metric("Sample used", format(sample_size, big.mark = ","), "Shapiro-Wilk uses at most 5,000 observations", "neutral"),
            va_metric("Practical result", ifelse(shapiro_test$p.value < alpha, "Non-normal signal", "No strong departure"), "Use the Q-Q plot too", norm_status)
          ),
          if (shapiro_test$p.value < alpha) {
            "Reject H0: the data show evidence of non-normality. Check the Q-Q plot to see whether the departure is severe or driven by outliers."
          } else {
            "Fail to reject H0: there is no strong statistical evidence against normality. This does not prove the data are perfectly normal."
          },
          "The test is sensitive: with large samples, tiny departures can become significant; with small samples, clear non-normality may be missed.",
          norm_status,
          paste("Variable:", input$testVar1)
        )
      })

      output$testPlot <- renderPlot({
        req(length(x) >= 3)
        qq_df <- data.frame(value = x)
        ggplot2::ggplot(qq_df, ggplot2::aes(sample = value)) +
          ggplot2::stat_qq(color = "#38bdf8", alpha = 0.75, size = 2) +
          ggplot2::stat_qq_line(color = "#f97316", linewidth = 1.1) +
          ggplot2::labs(
            title = paste("Q-Q plot:", input$testVar1),
            subtitle = "Points close to the line are more consistent with normality",
            x = "Theoretical quantiles",
            y = "Sample quantiles"
          ) +
          va_plot_theme()
      })
    } else if (input$testType == "Two-Sample T-Test") {
      req(input$testVar2)
      df_test <- data.frame(
        g = clean_cat_vector(data$raw[[input$testVar1]]),
        y = data$raw[[input$testVar2]]
      )
      df_test <- df_test[complete.cases(df_test) & is.finite(df_test$y), , drop = FALSE]
      df_test$g <- droplevels(df_test$g)

      output$testResults <- renderUI({
        if (nlevels(df_test$g) != 2 || nrow(df_test) < 3) {
          return(empty_test_ui("Two-sample t-test requires exactly 2 groups and enough complete numeric observations."))
        }
        t_test <- t.test(y ~ g, data = df_test)
        group_means <- tapply(df_test$y, df_test$g, mean)
        htest_summary_ui(
          "Two-sample t-test",
          "Tests whether two group means differ",
          t_test,
          tagList(
            va_metric("Groups", paste(levels(df_test$g), collapse = " vs "), paste("n =", paste(table(df_test$g), collapse = " / ")), "neutral"),
            va_metric("Mean difference", fmt_value(diff(group_means)), "Second group mean minus first group mean", "neutral"),
            va_metric("t-statistic", fmt_value(unname(t_test$statistic)), paste("df =", fmt_value(unname(t_test$parameter), 2)), "neutral"),
            va_metric("95% CI", paste0("[", fmt_value(t_test$conf.int[1]), ", ", fmt_value(t_test$conf.int[2]), "]"), "For mean difference", "neutral")
          ),
          decision_sentence(t_test$p.value),
          "Welch's t-test does not assume equal variances, but it still assumes independent observations and roughly normal group distributions or enough sample size.",
          p_status(t_test$p.value),
          c(paste("Grouping variable:", input$testVar1), paste("Measurement variable:", input$testVar2))
        )
      })

      output$testPlot <- renderPlot({
        req(nlevels(df_test$g) == 2)
        print(plot_group_box(df_test, input$testVar1, input$testVar2, "Two-group comparison"))
      })
    } else if (input$testType == "Paired T-Test") {
      req(input$testVar2)
      df_test <- data.frame(x = data$raw[[input$testVar1]], y = data$raw[[input$testVar2]])
      df_test <- df_test[complete.cases(df_test) & is.finite(df_test$x) & is.finite(df_test$y), , drop = FALSE]
      diff_vals <- df_test$x - df_test$y

      output$testResults <- renderUI({
        if (nrow(df_test) < 2) {
          return(empty_test_ui("Paired t-test requires at least 2 complete numeric pairs."))
        }
        t_test <- t.test(df_test$x, df_test$y, paired = TRUE)
        htest_summary_ui(
          "Paired t-test",
          "Tests whether the mean paired difference differs from zero",
          t_test,
          tagList(
            va_metric("Complete pairs", nrow(df_test), "Rows with both variables present", "ok"),
            va_metric("Mean difference", fmt_value(mean(diff_vals)), paste(input$testVar1, "-", input$testVar2), "neutral"),
            va_metric("t-statistic", fmt_value(unname(t_test$statistic)), paste("df =", fmt_value(unname(t_test$parameter), 2)), "neutral"),
            va_metric("95% CI", paste0("[", fmt_value(t_test$conf.int[1]), ", ", fmt_value(t_test$conf.int[2]), "]"), "For mean paired difference", "neutral")
          ),
          decision_sentence(t_test$p.value),
          "Requires paired observations from the same units or matched units. The paired differences should be approximately normal for small samples.",
          p_status(t_test$p.value),
          c(paste("Variable 1:", input$testVar1), paste("Variable 2:", input$testVar2))
        )
      })

      output$testPlot <- renderPlot({
        req(nrow(df_test) >= 2)
        plot_distribution(diff_vals, paste(input$testVar1, "-", input$testVar2), 0)
      })
    } else if (input$testType == "One-Way ANOVA (F-Test)") {
      req(input$testVar2)
      df_test <- data.frame(
        g = clean_cat_vector(data$raw[[input$testVar1]]),
        y = data$raw[[input$testVar2]]
      )
      df_test <- df_test[complete.cases(df_test) & is.finite(df_test$y), , drop = FALSE]
      df_test$g <- droplevels(df_test$g)

      output$testResults <- renderUI({
        if (nlevels(df_test$g) < 2) {
          return(empty_test_ui("ANOVA requires at least 2 groups with complete numeric observations."))
        }
        aov_fit <- aov(y ~ g, data = df_test)
        anova_tbl <- summary(aov_fit)[[1]]
        f_stat <- anova_tbl$`F value`[1]
        p_val <- anova_tbl$`Pr(>F)`[1]
        fake_test <- list(
          statistic = c(F = f_stat),
          parameter = c(df1 = anova_tbl$Df[1], df2 = anova_tbl$Df[2]),
          p.value = p_val,
          method = "One-way ANOVA",
          data.name = paste(input$testVar2, "by", input$testVar1)
        )
        class(fake_test) <- "htest"
        htest_summary_ui(
          "One-way ANOVA",
          "Tests whether at least one group mean differs",
          fake_test,
          tagList(
            va_metric("Groups", nlevels(df_test$g), paste("n =", paste(table(df_test$g), collapse = " / ")), "neutral"),
            va_metric("F-statistic", fmt_value(f_stat), paste("df =", anova_tbl$Df[1], ",", anova_tbl$Df[2]), "neutral"),
            va_metric("Between-group MS", fmt_value(anova_tbl$`Mean Sq`[1]), "Variation among group means", "neutral")
          ),
          decision_sentence(p_val),
          "ANOVA assumes independent observations, roughly normal residuals within groups, and similar variances. A significant result says at least one mean differs, not which groups differ.",
          p_status(p_val),
          capture.output(print(summary(aov_fit)))
        )
      })

      output$testPlot <- renderPlot({
        req(nlevels(df_test$g) >= 2)
        print(plot_group_box(df_test, input$testVar1, input$testVar2, "ANOVA group comparison"))
      })
    } else if (input$testType == "One-Sample Wilcoxon Test") {
      x <- data$raw[[input$testVar1]]
      x <- x[is.finite(x)]

      output$testResults <- renderUI({
        if (length(x) < 1) {
          return(empty_test_ui("One-sample Wilcoxon test requires finite numeric values."))
        }
        w_test <- wilcox.test(x, mu = input$testMu, conf.int = TRUE, exact = FALSE)
        htest_summary_ui(
          "One-sample Wilcoxon signed-rank test",
          "Tests whether the distribution center differs from the hypothesized value",
          w_test,
          tagList(
            va_metric("Sample median", fmt_value(median(x)), paste("Hypothesized center:", fmt_value(input$testMu)), "neutral"),
            va_metric("V statistic", fmt_value(unname(w_test$statistic)), "Rank-based statistic", "neutral"),
            va_metric("Complete values", length(x), "Finite numeric values used", "ok")
          ),
          decision_sentence(w_test$p.value),
          "This nonparametric test is less sensitive to non-normality than a t-test, but it assumes independent observations and is best interpreted as a test of location under roughly symmetric differences.",
          p_status(w_test$p.value),
          paste("Variable:", input$testVar1)
        )
      })

      output$testPlot <- renderPlot({
        req(length(x) >= 1)
        plot_distribution(x, input$testVar1, input$testMu)
      })
    } else if (input$testType == "Two-Sample Wilcoxon Test") {
      req(input$testVar2)
      df_test <- data.frame(
        g = clean_cat_vector(data$raw[[input$testVar1]]),
        y = data$raw[[input$testVar2]]
      )
      df_test <- df_test[complete.cases(df_test) & is.finite(df_test$y), , drop = FALSE]
      df_test$g <- droplevels(df_test$g)

      output$testResults <- renderUI({
        if (nlevels(df_test$g) != 2 || nrow(df_test) < 2) {
          return(empty_test_ui("Two-sample Wilcoxon test requires exactly 2 groups and complete numeric observations."))
        }
        w_test <- wilcox.test(y ~ g, data = df_test, exact = FALSE)
        medians <- tapply(df_test$y, df_test$g, median)
        htest_summary_ui(
          "Two-sample Wilcoxon rank-sum test",
          "Tests whether two groups tend to have different values",
          w_test,
          tagList(
            va_metric("Groups", paste(levels(df_test$g), collapse = " vs "), paste("n =", paste(table(df_test$g), collapse = " / ")), "neutral"),
            va_metric("Median difference", fmt_value(diff(medians)), "Second group median minus first", "neutral"),
            va_metric("W statistic", fmt_value(unname(w_test$statistic)), "Rank-based statistic", "neutral")
          ),
          decision_sentence(w_test$p.value),
          "This nonparametric test compares ranks rather than means. It is robust to skew, but different distribution shapes can complicate median-only interpretations.",
          p_status(w_test$p.value),
          c(paste("Grouping variable:", input$testVar1), paste("Measurement variable:", input$testVar2))
        )
      })

      output$testPlot <- renderPlot({
        req(nlevels(df_test$g) == 2)
        print(plot_group_box(df_test, input$testVar1, input$testVar2, "Two-group rank comparison"))
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
        "<li>Explore numeric relationships in the <strong>Variable Analysis</strong> tab</li>"
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

    sm   <- summary(model)
    cf   <- coef(sm)
    an   <- anova(model)
    ci   <- confint(model, level = 0.95)
    r2   <- sm$r.squared
    ar2  <- sm$adj.r.squared
    sigma_v <- sm$sigma
    fstat   <- sm$fstatistic
    f_p     <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_

    # ── Scatter plot (ggplot2) ───────────────────────────────
    output$slrScatter <- renderPlot({
      df_plot  <- data.frame(x = x, y = y)
      b0 <- coef(model)[1]; b1 <- if (length(coef(model)) > 1) coef(model)[2] else 0
      eq_label <- if (input$slrNoIntercept) {
        sprintf("y = %.4f x", b1)
      } else {
        sprintf("y = %.4f %s %.4f x", b0, ifelse(b1 >= 0, "+", "-"), abs(b1))
      }
      ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(color = "#38bdf8", alpha = 0.75, size = 2.6) +
        ggplot2::geom_smooth(method = "lm",
          formula = if (input$slrNoIntercept) y ~ 0 + x else y ~ x,
          se = TRUE, color = "#f97316", fill = "#f97316", alpha = 0.14, linewidth = 1.1) +
        ggplot2::annotate("text", x = -Inf, y = Inf,
          label = eq_label, hjust = -0.08, vjust = 1.5,
          color = "#fdba74", size = 4.5, fontface = "bold") +
        ggplot2::labs(
          title = paste("Scatter Plot:", input$slrY, "vs", input$slrX),
          subtitle = sprintf("R² = %.4f  |  Adj R² = %.4f  |  Residual SE = %.4f", r2, ar2, sigma_v),
          x = input$slrX, y = input$slrY
        ) +
        va_plot_theme()
    })

    # ── Model Summary (styled card) ──────────────────────────
    output$slrSummary <- renderUI({
      coef_df <- as.data.frame(cf)
      coef_df <- cbind(Term = rownames(coef_df), round(coef_df, 4))
      rownames(coef_df) <- NULL
      names(coef_df) <- c("Term", "Estimate", "Std.Error", "t value", "Pr(>|t|)")
      sig_badge <- function(p) {
        if (is.na(p)) return(tags$span("N/A"))
        cls <- if (p < 0.001) "ok" else if (p < 0.01) "ok" else if (p < 0.05) "ok" else if (p < 0.1) "warn" else "bad"
        lbl <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else "ns"
        tags$span(class = paste("va-pill", cls), lbl, " ", format.pval(p, digits = 4, eps = 0.0001))
      }
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("R²", fmt_value(r2, 4), "Variation in Y explained by X",
            if (r2 >= 0.7) "ok" else if (r2 >= 0.4) "warn" else "bad"),
          va_metric("Adjusted R²", fmt_value(ar2, 4), "Penalised for number of predictors",
            if (ar2 >= 0.7) "ok" else if (ar2 >= 0.4) "warn" else "bad"),
          va_metric("Residual SE", fmt_value(sigma_v, 4), "Average prediction error (in Y units)", "neutral"),
          va_metric("F-test p-value", format.pval(f_p, digits = 4), "Overall model significance", p_status(f_p))
        ),
        va_interpretation(
          "What does the model say?",
          paste0(
            "The model explains ", round(100 * r2, 1), "% of the variation in ", input$slrY, ". ",
            if (!is.na(f_p) && f_p < 0.05) {
              paste0("The overall F-test is significant (p = ", format.pval(f_p, digits = 4), "), indicating that ",
                input$slrX, " is a useful linear predictor of ", input$slrY, ".")
            } else {
              paste0("The overall F-test is not significant (p = ", format.pval(f_p, digits = 4), "), suggesting ",
                input$slrX, " may not be a reliable linear predictor.")
            }
          ),
          pills = tagList(
            va_pill(paste("R² =", fmt_value(r2, 4)), if (r2 >= 0.7) "ok" else if (r2 >= 0.4) "warn" else "bad", "chart-line"),
            va_pill(if (!is.na(f_p) && f_p < 0.05) "Significant F-test" else "Non-significant F",
              p_status(f_p), if (!is.na(f_p) && f_p < 0.05) "check-circle" else "info-circle")
          )
        ),
        tags$h5(style = "margin:12px 0 6px; color:#93c5fd; font-weight:700;", "Coefficients"),
        tags$table(
          class = "va-table",
          tags$thead(tags$tr(
            tags$th("Term"), tags$th("Estimate"), tags$th("Std. Error"),
            tags$th("t value"), tags$th("p-value")
          )),
          tags$tbody(lapply(seq_len(nrow(coef_df)), function(i) {
            p_val <- suppressWarnings(as.numeric(coef_df[["Pr(>|t|)"]][i]))
            tags$tr(
              tags$td(tags$strong(coef_df$Term[i])),
              tags$td(coef_df$Estimate[i]),
              tags$td(coef_df$`Std.Error`[i]),
              tags$td(coef_df$`t value`[i]),
              tags$td(sig_badge(p_val))
            )
          }))
        ),
        va_details("Technical details: full R summary",
          c(capture.output(print(model)), "", capture.output(print(sm))))
      )
    })

    # ── ANOVA (styled card) ──────────────────────────────────
    output$slrAnova <- renderUI({
      anova_df <- as.data.frame(an)
      anova_df <- cbind(Source = rownames(anova_df), round(anova_df, 4))
      rownames(anova_df) <- NULL
      f_row <- an[1, ]
      f_val  <- f_row[["F value"]]
      p_val  <- f_row[["Pr(>F)"]]
      ssr <- f_row[["Sum Sq"]]
      sse <- if (nrow(an) > 1) an[nrow(an), "Sum Sq"] else NA_real_
      sst <- ssr + if (!is.na(sse)) sse else 0
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("SS Regression", fmt_value(ssr, 4), "Variation explained by the model", "neutral"),
          va_metric("SS Residual", fmt_value(sse, 4), "Unexplained variation (error)", "neutral"),
          va_metric("F-statistic", fmt_value(f_val, 4), paste0("df = ", an$Df[1], ", ", if (nrow(an) > 1) an$Df[nrow(an)] else "N/A"), "neutral"),
          va_metric("p-value", format.pval(p_val, digits = 4), "H₀: slope = 0", p_status(p_val))
        ),
        va_interpretation(
          "ANOVA: is the linear relationship significant?",
          paste0(
            if (!is.na(p_val) && p_val < 0.05) {
              paste0("Reject H₀ (p = ", format.pval(p_val, digits = 4), "). ",
                "The regression explains significantly more variation than a model with just the mean. ",
                "Conclusion: the linear relationship between ", input$slrX, " and ", input$slrY, " is statistically significant.")
            } else {
              paste0("Fail to reject H₀ (p = ", format.pval(p_val, digits = 4), "). ",
                "The regression does not explain significantly more variation than a mean-only model.")
            }
          ),
          pills = tagList(
            va_pill(if (!is.na(p_val) && p_val < 0.05) "Reject H₀" else "Fail to reject H₀",
              p_status(p_val), if (!is.na(p_val) && p_val < 0.05) "check-circle" else "info-circle"),
            va_pill(paste("F =", fmt_value(f_val, 3)), "neutral", "calculator")
          )
        ),
        va_details("Technical details: ANOVA table", capture.output(print(an)))
      )
    })

    # ── Confidence Intervals (styled card) ───────────────────
    output$slrConfint <- renderUI({
      ci_df <- as.data.frame(round(ci, 4))
      ci_df <- cbind(Term = rownames(ci_df), ci_df)
      rownames(ci_df) <- NULL
      names(ci_df) <- c("Term", "Lower 2.5%", "Upper 97.5%")
      tags$div(
        class = "va-summary",
        va_interpretation(
          "95% Confidence intervals for regression coefficients",
          paste0(
            "Each interval gives a plausible range for the true population coefficient at 95% confidence. ",
            "If an interval for a slope does not contain zero, that predictor is statistically significant at the 5% level."
          ),
          pills = tagList(
            va_pill("95% confidence level", "neutral", "arrows-alt-h"),
            va_pill("Excludes 0 → significant", "ok", "check-circle")
          )
        ),
        va_table(ci_df),
        va_details("Technical details: confint output", capture.output(print(confint(model, level = 0.95))))
      )
    })

    # ── Correlation Tests (styled card) ──────────────────────
    output$slrCorrelation <- renderUI({
      pear  <- tryCatch(cor.test(y, x, method = "pearson"),  error = function(e) NULL)
      spear <- tryCatch(suppressWarnings(cor.test(y, x, method = "spearman")), error = function(e) NULL)
      kend  <- tryCatch(suppressWarnings(cor.test(y, x, method = "kendall")),  error = function(e) NULL)
      r_val <- if (!is.null(pear)) unname(pear$estimate) else NA_real_
      strength_label <- function(r) {
        if (is.na(r)) return("N/A")
        ar <- abs(r)
        if (ar >= 0.9) "Very strong" else if (ar >= 0.7) "Strong" else if (ar >= 0.5) "Moderate" else if (ar >= 0.3) "Weak" else "Very weak"
      }
      corr_row <- function(label, test_obj) {
        if (is.null(test_obj)) return(tags$tr(tags$td(label), tags$td("N/A"), tags$td("N/A"), tags$td("N/A")))
        r_e <- unname(test_obj$estimate)
        p_e <- test_obj$p.value
        tags$tr(
          tags$td(tags$strong(label)),
          tags$td(fmt_value(r_e, 4)),
          tags$td(format.pval(p_e, digits = 4, eps = 0.0001)),
          tags$td(tags$span(class = paste("va-pill", p_status(p_e)),
            if (p_e < 0.05) icon("check-circle") else icon("info-circle"),
            if (p_e < 0.05) "Significant" else "Not significant"))
        )
      }
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Pearson r", fmt_value(r_val, 4),
            paste0(strength_label(r_val), " ", if (!is.na(r_val) && r_val >= 0) "positive" else "negative", " linear association"),
            if (abs(r_val) >= 0.7) "ok" else if (abs(r_val) >= 0.4) "warn" else "neutral"),
          va_metric("Pearson r²", fmt_value(r_val^2, 4), "Proportion of shared variance", "neutral"),
          va_metric("Pearson p-value", if (!is.null(pear)) format.pval(pear$p.value, digits = 4) else "N/A",
            "H₀: true correlation = 0", if (!is.null(pear)) p_status(pear$p.value) else "bad")
        ),
        va_interpretation(
          "Interpreting correlation",
          paste0(
            "Pearson r measures linear association (assumes normality and linearity). ",
            "Spearman and Kendall are rank-based (non-parametric) alternatives robust to outliers and non-normality. ",
            "A large r does not prove causation."
          ),
          pills = tagList(
            va_pill(paste0("r = ", fmt_value(r_val, 3), " (", strength_label(r_val), ")"),
              if (abs(r_val) >= 0.7) "ok" else if (abs(r_val) >= 0.4) "warn" else "neutral", "link")
          )
        ),
        tags$table(
          class = "va-table",
          tags$thead(tags$tr(tags$th("Method"), tags$th("Correlation"), tags$th("p-value"), tags$th("Result"))),
          tags$tbody(
            corr_row("Pearson", pear),
            corr_row("Spearman", spear),
            corr_row("Kendall", kend)
          )
        ),
        va_details("Technical details: raw test outputs",
          c(if (!is.null(pear)) c("Pearson:", capture.output(print(pear)), "") else "Pearson: failed",
            if (!is.null(spear)) c("Spearman:", capture.output(print(spear)), "") else "Spearman: failed",
            if (!is.null(kend)) c("Kendall:", capture.output(print(kend))) else "Kendall: failed")
        )
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
        output$slrInterpExtrap <- renderUI({
          check <- ifelse(x0 > max(x_orig, na.rm = TRUE) | x0 < min(x_orig, na.rm = TRUE),
            "EXTRAPOLATION", "interpolation"
          )
          extrap_n <- sum(check == "EXTRAPOLATION")
          rows_ui <- lapply(seq_along(x0), function(i) {
            is_ext <- check[i] == "EXTRAPOLATION"
            tags$tr(
              tags$td(fmt_value(x0[i], 4)),
              tags$td(
                tags$span(class = paste("va-pill", if (is_ext) "bad" else "ok"),
                  if (is_ext) icon("exclamation-triangle") else icon("check-circle"),
                  check[i])
              ),
              tags$td(if (is_ext) "Unreliable — outside training range" else "Reliable — inside training range")
            )
          })
          tags$div(
            class = "va-summary",
            tags$div(
              class = "va-metric-grid",
              va_metric("X range (training)",
                paste0("[", fmt_value(min(x_orig, na.rm = TRUE), 3), ", ", fmt_value(max(x_orig, na.rm = TRUE), 3), "]"),
                "Observed X values used to fit the model", "neutral"),
              va_metric("Points queried", length(x0), "New X values entered", "neutral"),
              va_metric("Extrapolations", extrap_n,
                if (extrap_n > 0) "Predictions outside training range" else "All inside training range",
                if (extrap_n > 0) "bad" else "ok")
            ),
            va_interpretation(
              "Interpolation vs. Extrapolation",
              paste0("Predictions within the training range of X are interpolations and are generally reliable. ",
                "Predictions outside that range are extrapolations — the model's linear assumption may not hold ",
                "beyond observed data, so treat those predictions with caution."),
              pills = tagList(
                va_pill("Inside range: safe", "ok", "check-circle"),
                va_pill("Outside range: caution", if (extrap_n > 0) "bad" else "neutral", "exclamation-triangle")
              )
            ),
            tags$table(
              class = "va-table",
              tags$thead(tags$tr(tags$th("X Value"), tags$th("Classification"), tags$th("Guidance"))),
              tags$tbody(rows_ui)
            )
          )
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

    # ── Pre-compute shared MLR summary values ────────────────────────────────────
    sm_mlr    <- summary(model)
    cf_mlr    <- coef(sm_mlr)
    an_mlr    <- anova(model)
    ci_mlr    <- confint(model, level = 0.95)
    r2_mlr    <- sm_mlr$r.squared
    ar2_mlr   <- sm_mlr$adj.r.squared
    sig_mlr   <- sm_mlr$sigma
    fstat_mlr <- sm_mlr$fstatistic
    fp_mlr    <- if (!is.null(fstat_mlr)) pf(fstat_mlr[1], fstat_mlr[2], fstat_mlr[3], lower.tail = FALSE) else NA_real_

    sig_badge_mlr <- function(p) {
      if (is.na(p)) return(tags$span("N/A"))
      cls <- if (p < 0.001) "ok" else if (p < 0.01) "ok" else if (p < 0.05) "ok" else if (p < 0.1) "warn" else "bad"
      lbl <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else "ns"
      tags$span(class = paste("va-pill", cls), lbl, " ", format.pval(p, digits = 4, eps = 0.0001))
    }

    # ── Observed vs Predicted (ggplot2) ──────────────────────────────────────────
    output$mlrFitLine <- renderPlot({
      y_obs <- model.response(model.frame(model))
      y_hat <- fitted(model)
      df_obs <- data.frame(fitted = y_hat, observed = y_obs)
      ggplot2::ggplot(df_obs, ggplot2::aes(x = fitted, y = observed)) +
        ggplot2::geom_point(color = "#38bdf8", alpha = 0.72, size = 2.2) +
        ggplot2::geom_abline(slope = 1, intercept = 0, color = "#f97316", linewidth = 1, linetype = "dashed") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x,
          color = "#22c55e", fill = "#22c55e", alpha = 0.12, linewidth = 0.9) +
        ggplot2::labs(
          title = "Observed vs Predicted",
          subtitle = sprintf("R\u00b2 = %.4f  |  Adj R\u00b2 = %.4f  |  Residual SE = %.4f", r2_mlr, ar2_mlr, sig_mlr),
          x = "Predicted values", y = paste("Observed:", input$mlrY)
        ) +
        va_plot_theme()
    })

    # ── Model Summary (styled card) ───────────────────────────────────────────────
    output$mlrSummary <- renderUI({
      coef_df <- as.data.frame(cf_mlr)
      coef_df <- cbind(Term = rownames(coef_df), round(coef_df, 4))
      rownames(coef_df) <- NULL
      names(coef_df) <- c("Term", "Estimate", "Std.Error", "t value", "Pr(>|t|)")
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("R\u00b2", fmt_value(r2_mlr, 4), "Variation in Y explained by all predictors",
            if (r2_mlr >= 0.7) "ok" else if (r2_mlr >= 0.4) "warn" else "bad"),
          va_metric("Adjusted R\u00b2", fmt_value(ar2_mlr, 4), "Penalised for number of predictors",
            if (ar2_mlr >= 0.7) "ok" else if (ar2_mlr >= 0.4) "warn" else "bad"),
          va_metric("Residual SE", fmt_value(sig_mlr, 4), "Average prediction error in Y units", "neutral"),
          va_metric("F-test p-value", format.pval(fp_mlr, digits = 4),
            paste("df =", fstat_mlr[2], ",", fstat_mlr[3]), p_status(fp_mlr))
        ),
        va_interpretation(
          "What does the MLR model say?",
          paste0(
            "The model with ", length(xvars), " predictors explains ", round(100 * r2_mlr, 1),
            "% of the variation in ", input$mlrY, ". ",
            if (!is.na(fp_mlr) && fp_mlr < 0.05)
              paste0("The overall F-test is significant (p = ", format.pval(fp_mlr, digits = 4),
                "), indicating at least one predictor contributes meaningfully.")
            else
              paste0("The overall F-test is not significant (p = ", format.pval(fp_mlr, digits = 4), ").")
          ),
          pills = tagList(
            va_pill(paste("R\u00b2 =", fmt_value(r2_mlr, 4)),
              if (r2_mlr >= 0.7) "ok" else if (r2_mlr >= 0.4) "warn" else "bad", "chart-line"),
            va_pill(if (!is.na(fp_mlr) && fp_mlr < 0.05) "Significant F" else "Non-significant F",
              p_status(fp_mlr), if (!is.na(fp_mlr) && fp_mlr < 0.05) "check-circle" else "info-circle")
          )
        ),
        tags$h5(style = "margin:12px 0 6px; color:#93c5fd; font-weight:700;", "Coefficients"),
        tags$table(
          class = "va-table",
          tags$thead(tags$tr(
            tags$th("Term"), tags$th("Estimate"), tags$th("Std. Error"),
            tags$th("t value"), tags$th("p-value")
          )),
          tags$tbody(lapply(seq_len(nrow(coef_df)), function(i) {
            pv <- suppressWarnings(as.numeric(coef_df[["Pr(>|t|)"]][i]))
            tags$tr(
              tags$td(tags$strong(coef_df$Term[i])),
              tags$td(coef_df$Estimate[i]),
              tags$td(coef_df$Std.Error[i]),
              tags$td(coef_df$`t value`[i]),
              tags$td(sig_badge_mlr(pv))
            )
          }))
        ),
        va_details("Technical details: full R summary",
          c(capture.output(print(model)), "", capture.output(print(sm_mlr))))
      )
    })

    # ── ANOVA (styled card) ───────────────────────────────────────────────────────
    output$mlrAnova <- renderUI({
      anova_rows <- lapply(seq_len(nrow(an_mlr)), function(i) {
        row <- an_mlr[i, ]
        pv  <- row[["Pr(>F)"]]
        tags$tr(
          tags$td(rownames(an_mlr)[i]),
          tags$td(row$Df),
          tags$td(fmt_value(row[["Sum Sq"]], 4)),
          tags$td(fmt_value(row[["Mean Sq"]], 4)),
          tags$td(if (!is.na(row[["F value"]])) fmt_value(row[["F value"]], 4) else "\u2014"),
          tags$td(if (!is.null(pv) && !is.na(pv)) sig_badge_mlr(pv) else tags$span("\u2014"))
        )
      })
      regr_ss <- sum(an_mlr[["Sum Sq"]][-nrow(an_mlr)], na.rm = TRUE)
      res_ss  <- an_mlr[["Sum Sq"]][nrow(an_mlr)]
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("SS Regression", fmt_value(regr_ss, 4), "Variation explained by all predictors", "neutral"),
          va_metric("SS Residual",   fmt_value(res_ss, 4),  "Unexplained variation", "neutral"),
          va_metric("Predictors", length(xvars), "In the model", "neutral"),
          va_metric("Overall p-value", format.pval(fp_mlr, digits = 4), "H\u2080: all slopes = 0", p_status(fp_mlr))
        ),
        va_interpretation(
          "Sequential ANOVA (Type I)",
          paste0(
            "Each row shows how much variation that predictor adds, given the predictors already in the model. ",
            "A small p-value means that predictor adds explanatory power conditional on the earlier predictors. ",
            if (!is.na(fp_mlr) && fp_mlr < 0.05)
              paste0("Overall: Reject H\u2080 (p = ", format.pval(fp_mlr, digits = 4), ").")
            else
              paste0("Overall: Fail to reject H\u2080 (p = ", format.pval(fp_mlr, digits = 4), ").")
          ),
          pills = tagList(
            va_pill("Type I (sequential) SS", "neutral", "list-ol"),
            va_pill(
              if (!is.na(fp_mlr) && fp_mlr < 0.05) "Overall significant" else "Overall not significant",
              p_status(fp_mlr),
              if (!is.na(fp_mlr) && fp_mlr < 0.05) "check-circle" else "info-circle"
            )
          )
        ),
        tags$table(
          class = "va-table",
          tags$thead(tags$tr(
            tags$th("Source"), tags$th("Df"), tags$th("Sum Sq"),
            tags$th("Mean Sq"), tags$th("F value"), tags$th("p-value")
          )),
          tags$tbody(anova_rows)
        ),
        va_details("Technical details: ANOVA output", capture.output(print(an_mlr)))
      )
    })

    # ── Partial F-Tests (styled card) ────────────────────────────────────────────
    output$mlrPartialF <- renderUI({
      pf_results <- lapply(xvars, function(xv) {
        reduced_vars <- setdiff(xvars, xv)
        if (length(reduced_vars) == 0) {
          red_formula <- if (input$mlrNoIntercept) as.formula(paste(bt(input$mlrY), "~ 0 + 1")) else as.formula(paste(bt(input$mlrY), "~ 1"))
        } else {
          red_rhs    <- paste(sapply(reduced_vars, bt), collapse = " + ")
          red_formula <- if (input$mlrNoIntercept) as.formula(paste(bt(input$mlrY), "~ 0 +", red_rhs)) else as.formula(paste(bt(input$mlrY), "~", red_rhs))
        }
        red_model <- lm(red_formula, data = df_mlr)
        av <- anova(red_model, model)
        list(var = xv, f = av$F[2], p = av[["Pr(>F)"]][2], df1 = av$Df[2], df2 = av$Res.Df[2], tech = capture.output(print(av)))
      })
      sig_n <- sum(vapply(pf_results, function(r) !is.na(r$p) && r$p < 0.05, logical(1)))
      rows_ui <- lapply(pf_results, function(r) {
        tags$tr(
          tags$td(tags$strong(r$var)),
          tags$td(fmt_value(r$f, 4)),
          tags$td(paste0(r$df1, ", ", r$df2)),
          tags$td(sig_badge_mlr(r$p)),
          tags$td(if (!is.na(r$p) && r$p < 0.05)
            tags$span(class = "va-pill ok", icon("check-circle"), " Significant")
          else
            tags$span(class = "va-pill neutral", icon("info-circle"), " Not significant"))
        )
      })
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Predictors tested", length(xvars), "One partial test per predictor", "neutral"),
          va_metric("Individually significant", sig_n,
            paste0(sig_n, " of ", length(xvars), " carry unique explanatory power"),
            if (sig_n > 0) "ok" else "warn")
        ),
        va_interpretation(
          "Partial F-tests: H\u2080: \u03b2\u1d62 = 0",
          paste0(
            "Each test compares the full model against a reduced model without that predictor. ",
            "A significant result (p < 0.05) means removing that predictor significantly worsens the fit. ",
            "Unlike t-tests in the summary, these account for re-fitting the reduced model."
          ),
          pills = tagList(
            va_pill("Partial test: unique contribution", "neutral", "vial"),
            va_pill(paste(sig_n, "of", length(xvars), "significant"),
              if (sig_n > 0) "ok" else "warn",
              if (sig_n > 0) "check-circle" else "exclamation-triangle")
          )
        ),
        tags$table(
          class = "va-table",
          tags$thead(tags$tr(
            tags$th("Predictor"), tags$th("F-stat"), tags$th("df"),
            tags$th("p-value"), tags$th("Decision")
          )),
          tags$tbody(rows_ui)
        ),
        va_details("Technical details: each anova() comparison",
          unlist(lapply(pf_results, function(r) c(paste0("H\u2080: beta(", r$var, ") = 0"), r$tech, ""))))
      )
    })


    # ── Confidence Intervals (styled card) ────────────────────────────────────────
    output$mlrConfint <- renderUI({
      ci_df <- as.data.frame(round(ci_mlr, 4))
      ci_df <- cbind(Term = rownames(ci_df), ci_df)
      rownames(ci_df) <- NULL
      names(ci_df) <- c("Term", "Lower 2.5%", "Upper 97.5%")
      ci_df[["Excludes 0"]] <- ifelse(
        ci_df[["Lower 2.5%"]] > 0 | ci_df[["Upper 97.5%"]] < 0, "Yes", "No"
      )
      tags$div(
        class = "va-summary",
        va_interpretation(
          "95% Confidence intervals for all coefficients",
          paste0(
            "Each interval gives a plausible range for the true population coefficient. ",
            "Intervals that do not contain zero indicate the predictor is statistically significant at the 5% level. ",
            "Wider intervals indicate less precision due to small sample size or high variance."
          ),
          pills = tagList(
            va_pill("95% confidence level", "neutral", "arrows-alt-h"),
            va_pill("Excludes 0 \u2192 significant", "ok", "check-circle")
          )
        ),
        va_table(ci_df),
        va_details("Technical details: confint output", capture.output(print(ci_mlr)))
      )
    })

    # ── VIF (styled card + ggplot2 bar) ───────────────────────────────────────────
    output$mlrVIF <- renderUI({
      if (length(xvars) < 2) return(tags$div(class = "va-empty", "VIF requires at least 2 predictors."))
      vif_vals <- tryCatch(vif(model), error = function(e) NULL)
      if (is.null(vif_vals)) return(tags$div(class = "va-empty", "VIF could not be computed for this model."))
      vif_df <- data.frame(
        Predictor = names(vif_vals),
        VIF       = round(as.numeric(vif_vals), 4),
        Status    = ifelse(as.numeric(vif_vals) >= 10, "Severe",
                    ifelse(as.numeric(vif_vals) >= 5,  "Moderate", "OK")),
        stringsAsFactors = FALSE
      )
      max_vif   <- max(vif_df$VIF)
      mc_status <- if (max_vif >= 10) "bad" else if (max_vif >= 5) "warn" else "ok"
      tags$div(
        class = "va-summary",
        tags$div(
          class = "va-metric-grid",
          va_metric("Max VIF", fmt_value(max_vif, 3),
            if (max_vif >= 10) "Severe multicollinearity" else if (max_vif >= 5) "Moderate concern" else "No serious multicollinearity",
            mc_status),
          va_metric("Predictors checked", length(xvars), "All model predictors", "neutral")
        ),
        va_interpretation(
          "Variance Inflation Factors (VIF)",
          paste0(
            "VIF measures how much the variance of a coefficient is inflated due to correlation with other predictors. ",
            "VIF < 5: acceptable. 5\u201310: moderate concern. > 10: severe multicollinearity that may distort coefficient estimates."
          ),
          pills = tagList(
            va_pill("VIF < 5: OK", "ok", "check-circle"),
            va_pill("VIF \u2265 5: warning", "warn", "exclamation-triangle"),
            va_pill("VIF \u2265 10: severe", "bad", "times-circle")
          )
        ),
        tags$table(
          class = "va-table",
          tags$thead(tags$tr(tags$th("Predictor"), tags$th("VIF"), tags$th("Assessment"))),
          tags$tbody(lapply(seq_len(nrow(vif_df)), function(i) {
            st <- if (vif_df$Status[i] == "Severe") "bad" else if (vif_df$Status[i] == "Moderate") "warn" else "ok"
            tags$tr(
              tags$td(tags$strong(vif_df$Predictor[i])),
              tags$td(vif_df$VIF[i]),
              tags$td(tags$span(class = paste("va-pill", st), vif_df$Status[i]))
            )
          }))
        )
      )
    })

    output$mlrVIFPlot <- renderPlot({
      if (length(xvars) >= 2) {
        vif_vals <- tryCatch(vif(model), error = function(e) NULL)
        if (is.null(vif_vals)) { plot.new(); text(0.5, 0.5, "VIF could not be computed."); return() }
        vif_df2 <- data.frame(
          Predictor = factor(names(vif_vals), levels = names(vif_vals)[order(as.numeric(vif_vals))]),
          VIF       = as.numeric(vif_vals)
        )
        vif_df2$fill <- ifelse(vif_df2$VIF >= 10, "#ef4444", ifelse(vif_df2$VIF >= 5, "#f59e0b", "#22c55e"))
        ggplot2::ggplot(vif_df2, ggplot2::aes(x = Predictor, y = VIF, fill = fill)) +
          ggplot2::geom_col(width = 0.65) +
          ggplot2::geom_hline(yintercept = 5,  color = "#f59e0b", linewidth = 1, linetype = "dashed") +
          ggplot2::geom_hline(yintercept = 10, color = "#ef4444", linewidth = 1, linetype = "dotted") +
          ggplot2::geom_text(ggplot2::aes(label = round(VIF, 2)), hjust = -0.15, color = "#e5e7eb", size = 3.6) +
          ggplot2::coord_flip(clip = "off") +
          ggplot2::scale_fill_identity() +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
          ggplot2::labs(
            title = "Variance Inflation Factors (VIF)",
            subtitle = "Dashed = VIF 5 (moderate); Dotted = VIF 10 (severe)",
            x = NULL, y = "VIF"
          ) +
          va_plot_theme()
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

        # ── Leverage-based interpolation/extrapolation check ─────────────────────
        output$mlrInterpExtrap <- renderUI({
          hii   <- hatvalues(mlr_model())
          h_new <- (predict(mlr_model(),
            newdata = new_data, interval = "confidence", se.fit = TRUE
          )$se.fit / sigma(mlr_model()))^2
          result    <- ifelse(h_new > max(hii), "EXTRAPOLATION", "interpolation")
          extrap_n  <- sum(result == "EXTRAPOLATION")
          rows_ui   <- lapply(seq_along(result), function(i) {
            is_ext <- result[i] == "EXTRAPOLATION"
            tags$tr(
              tags$td(paste("Obs", i)),
              tags$td(fmt_value(h_new[i], 4)),
              tags$td(fmt_value(max(hii), 4)),
              tags$td(tags$span(class = paste("va-pill", if (is_ext) "bad" else "ok"),
                if (is_ext) icon("exclamation-triangle") else icon("check-circle"),
                result[i]
              ))
            )
          })
          tags$div(
            class = "va-summary",
            tags$div(
              class = "va-metric-grid",
              va_metric("Max training leverage", fmt_value(max(hii), 4), "h_max from fitted model", "neutral"),
              va_metric("New observations", length(result), "Queried data points", "neutral"),
              va_metric("Extrapolations", extrap_n,
                if (extrap_n > 0) "Exceeds max training leverage" else "All within training space",
                if (extrap_n > 0) "bad" else "ok")
            ),
            va_interpretation(
              "Interpolation vs. Extrapolation (leverage-based)",
              paste0(
                "A new point is classified as EXTRAPOLATION if its leverage h_new exceeds the ",
                "maximum leverage of any training point. Extrapolation means the new point lies ",
                "outside the predictor space used to fit the model \u2014 its prediction is unreliable."
              ),
              pills = tagList(
                va_pill("h_new \u2264 max(h_ii): interpolation", "ok", "check-circle"),
                va_pill("h_new > max(h_ii): EXTRAPOLATION", "bad", "exclamation-triangle")
              )
            ),
            tags$table(
              class = "va-table",
              tags$thead(tags$tr(
                tags$th("Observation"), tags$th("h_new"), tags$th("max(h_ii)"), tags$th("Classification")
              )),
              tags$tbody(rows_ui)
            )
          )
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
        names(out) <- rownames(vif_obj)
      } else {
        out <- vif_obj
        names(out) <- names(vif_obj)
      }
      out <- as.numeric(out)
      out
    }, error = function(e) NULL)
  }

  poly_metric_row <- function(label, fit, vif_vals = NULL) {
    sm <- summary(fit)
    max_vif <- if (is.null(vif_vals) || length(vif_vals) == 0 || all(is.na(vif_vals))) {
      NA_real_
    } else {
      max(vif_vals, na.rm = TRUE)
    }
    data.frame(
      Model = label,
      R_Squared = sm$r.squared,
      Adj_R_Squared = sm$adj.r.squared,
      Residual_SE = sm$sigma,
      AIC = AIC(fit),
      Max_VIF = max_vif,
      stringsAsFactors = FALSE
    )
  }

  reg_r2_status <- function(r2) {
    if (length(r2) == 0 || is.na(r2)) return("bad")
    if (r2 >= 0.7) "ok" else if (r2 >= 0.4) "warn" else "neutral"
  }

  reg_vif_status <- function(vif_value) {
    if (length(vif_value) == 0 || is.na(vif_value)) return("neutral")
    if (vif_value >= 10) "bad" else if (vif_value >= 5) "warn" else "ok"
  }

  reg_f_p_value <- function(fit) {
    fs <- summary(fit)$fstatistic
    if (is.null(fs)) return(NA_real_)
    pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
  }

  reg_coef_table <- function(fit) {
    cf <- as.data.frame(summary(fit)$coefficients)
    cf <- cbind(Term = rownames(cf), cf)
    rownames(cf) <- NULL
    names(cf) <- c("Term", "Estimate", "Std_Error", "t_value", "p_value")
    tags$table(
      class = "va-table",
      tags$thead(tags$tr(
        tags$th("Term"), tags$th("Estimate"), tags$th("Std. Error"),
        tags$th("t value"), tags$th("p-value"), tags$th("Decision")
      )),
      tags$tbody(lapply(seq_len(nrow(cf)), function(i) {
        pv <- cf$p_value[i]
        st <- p_status(pv)
        tags$tr(
          tags$td(tags$strong(cf$Term[i])),
          tags$td(fmt_value(cf$Estimate[i], 4)),
          tags$td(fmt_value(cf$Std_Error[i], 4)),
          tags$td(fmt_value(cf$t_value[i], 4)),
          tags$td(format.pval(pv, digits = 4, eps = 0.0001)),
          tags$td(tags$span(
            class = paste("va-pill", st),
            if (!is.na(pv) && pv < 0.05) icon("check-circle") else icon("info-circle"),
            p_label(pv)
          ))
        )
      }))
    )
  }

  reg_model_summary_ui <- function(title, fit, model_note, technical_lines, extra_metrics = NULL, extra_pills = NULL) {
    sm <- summary(fit)
    f_p <- reg_f_p_value(fit)
    tags$div(
      class = "va-summary",
      tags$div(
        class = "va-metric-grid",
        va_metric("R-squared", fmt_value(sm$r.squared, 4), "Variation explained by the model", reg_r2_status(sm$r.squared)),
        va_metric("Adjusted R-squared", fmt_value(sm$adj.r.squared, 4), "Penalized for model complexity", reg_r2_status(sm$adj.r.squared)),
        va_metric("Residual SE", fmt_value(sm$sigma, 4), "Typical error in response units", "neutral"),
        va_metric("Overall p-value", format.pval(f_p, digits = 4, eps = 0.0001), "H0: all slopes are zero", p_status(f_p)),
        extra_metrics
      ),
      va_interpretation(
        title,
        paste0(
          model_note, " The fitted model explains ", round(100 * sm$r.squared, 1),
          "% of the observed variation. ",
          if (!is.na(f_p) && f_p < 0.05) {
            paste0("The overall F-test is significant (p = ", format.pval(f_p, digits = 4),
              "), so the model has useful explanatory signal.")
          } else {
            paste0("The overall F-test is not significant (p = ", format.pval(f_p, digits = 4),
              "), so the fitted terms do not show clear explanatory signal at alpha = 0.05.")
          },
          " Interpret polynomial terms together as curve shape; individual powers can be unstable when predictors are highly correlated."
        ),
        pills = tagList(
          va_pill(if (!is.na(f_p) && f_p < 0.05) "Overall model significant" else "Overall model not significant",
            p_status(f_p), if (!is.na(f_p) && f_p < 0.05) "check-circle" else "info-circle"),
          va_pill(paste("R-squared =", fmt_value(sm$r.squared, 3)), reg_r2_status(sm$r.squared), "chart-line"),
          extra_pills
        )
      ),
      tags$h5(style = "margin:12px 0 6px; color:#93c5fd; font-weight:700;", "Coefficients"),
      reg_coef_table(fit),
      va_details("Technical details: full R summary", technical_lines)
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
    xgrid <- seq(min(r$df$x), max(r$df$x), length.out = 300)
    curve_df <- do.call(rbind, lapply(seq_along(r$raw_models), function(i) {
      data.frame(
        x = xgrid,
        y = predict(r$raw_models[[i]], newdata = data.frame(x = xgrid)),
        Model = names(r$raw_models)[i],
        stringsAsFactors = FALSE
      )
    }))
    ggplot2::ggplot(r$df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "#38bdf8", alpha = 0.68, size = 2.4) +
      ggplot2::geom_line(data = curve_df, ggplot2::aes(x = x, y = y, color = Model), linewidth = 1.05) +
      ggplot2::scale_color_manual(values = c("#f97316", "#22c55e", "#eab308", "#a78bfa")) +
      ggplot2::labs(
        title = "Polynomial Fits by Order",
        subtitle = "Compare curve shape against the observed data before choosing model complexity",
        x = r$x_label,
        y = r$y_label,
        color = "Model"
      ) +
      va_plot_theme()
  })

  output$polySummary <- renderUI({
    req(poly_results())
    r <- poly_results()
    degree_label <- names(r$raw_models)[r$selected_degree]
    fit <- r$raw_models[[r$selected_degree]]
    centered_fit <- r$centered_models[[r$selected_degree]]
    sm <- summary(fit)
    cent_sm <- summary(centered_fit)
    aic_best <- r$poly_compare$Model[which.min(r$poly_compare$AIC)]
    reg_model_summary_ui(
      paste(degree_label, "polynomial model"),
      fit,
      paste0(
        "This selected raw-x polynomial is compared with lower and higher orders. ",
        "The lowest AIC among the fitted orders is currently ", aic_best, "."
      ),
      c(
        paste(degree_label, "model (raw x)"),
        strrep("=", 45),
        capture.output(print(sm)),
        "",
        paste(degree_label, "model after centering x"),
        strrep("=", 45),
        capture.output(print(cent_sm))
      ),
      extra_metrics = tagList(
        va_metric("AIC", fmt_value(AIC(fit), 3), "Lower is better among compared models", "neutral"),
        va_metric("Best AIC order", aic_best, "From linear through quartic", if (aic_best == degree_label) "ok" else "warn")
      ),
      extra_pills = tagList(
        va_pill(paste("Selected:", degree_label), "neutral", "sliders-h"),
        va_pill(paste("Best AIC:", aic_best), if (aic_best == degree_label) "ok" else "warn", "trophy")
      )
    )
  })

  output$polyResidualPlot <- renderPlot({
    req(poly_results())
    r <- poly_results()
    resid_df <- do.call(rbind, lapply(seq_along(r$raw_models), function(i) {
      fit <- r$raw_models[[i]]
      data.frame(
        fitted = fitted(fit),
        residual = resid(fit),
        Model = names(r$raw_models)[i],
        stringsAsFactors = FALSE
      )
    }))
    ggplot2::ggplot(resid_df, ggplot2::aes(x = fitted, y = residual)) +
      ggplot2::geom_hline(yintercept = 0, color = "#f97316", linewidth = 0.9, linetype = "dashed") +
      ggplot2::geom_point(color = "#38bdf8", alpha = 0.66, size = 1.9) +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#22c55e", linewidth = 0.9) +
      ggplot2::facet_wrap(~Model, scales = "free_x") +
      ggplot2::labs(
        title = "Residual Diagnostics by Polynomial Order",
        subtitle = "A good fit has residuals scattered around zero without a clear curve",
        x = "Fitted values",
        y = "Residuals"
      ) +
      va_plot_theme()
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
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "VIF is not informative for a linear model with one predictor.", color = "#e5e7eb", size = 4.5) +
        ggplot2::xlim(-1, 1) +
        ggplot2::ylim(-1, 1) +
        ggplot2::labs(title = "Variance Inflation Factors") +
        va_plot_theme() +
        ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
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

    vif_df <- do.call(rbind, lapply(rownames(mat), function(scale_label) {
      data.frame(
        Term = names(mat[scale_label, ]),
        VIF = as.numeric(mat[scale_label, ]),
        Scale = scale_label,
        stringsAsFactors = FALSE
      )
    }))
    ggplot2::ggplot(vif_df, ggplot2::aes(x = Term, y = VIF, fill = Scale)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.65) +
      ggplot2::geom_hline(yintercept = 5, color = "#f59e0b", linewidth = 0.9, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 10, color = "#ef4444", linewidth = 0.9, linetype = "dotted") +
      ggplot2::scale_fill_manual(values = c(Raw = "#ef4444", Centered = "#22c55e")) +
      ggplot2::labs(
        title = "Variance Inflation Factors",
        subtitle = "Dashed = 5 (moderate concern); dotted = 10 (severe concern)",
        x = NULL,
        y = "VIF",
        fill = NULL
      ) +
      va_plot_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  })

  output$polyCenterText <- renderUI({
    req(poly_results())
    r <- poly_results()
    raw_vif <- r$raw_vifs[[r$selected_degree]]
    cent_vif <- r$centered_vifs[[r$selected_degree]]
    degree_label <- names(r$raw_models)[r$selected_degree]
    if (r$selected_degree == 1) {
      return(tags$div(
        class = "va-summary",
        va_interpretation(
          "Centering interpretation",
          "Centering does not change the shape of a first-order fit, and with only one predictor there is no polynomial-term multicollinearity to diagnose.",
          pills = tagList(va_pill("Linear model", "neutral", "info-circle"))
        ),
        va_details("Technical details",
          c("Selected order: Linear", "VIF is not informative for a first-order one-predictor model."))
      ))
    }
    raw_max <- if (is.null(raw_vif)) NA_real_ else max(raw_vif, na.rm = TRUE)
    cent_max <- if (is.null(cent_vif)) NA_real_ else max(cent_vif, na.rm = TRUE)
    reduction <- if (is.finite(raw_max) && raw_max != 0 && is.finite(cent_max)) 100 * (raw_max - cent_max) / raw_max else NA_real_
    raw_df <- data.frame(
      Term = names(raw_vif),
      Raw_VIF = round(as.numeric(raw_vif), 4),
      Centered_VIF = round(as.numeric(cent_vif[names(raw_vif)]), 4),
      check.names = FALSE
    )
    tags$div(
      class = "va-summary",
      tags$div(
        class = "va-metric-grid",
        va_metric("Selected order", degree_label, "Raw and centered fits compared", "neutral"),
        va_metric("Raw max VIF", fmt_value(raw_max, 3), "Before centering", reg_vif_status(raw_max)),
        va_metric("Centered max VIF", fmt_value(cent_max, 3), "After subtracting mean(x)", reg_vif_status(cent_max)),
        va_metric("VIF reduction", if (is.na(reduction)) "N/A" else paste0(fmt_value(reduction, 1), "%"), "Positive means centering helped", if (!is.na(reduction) && reduction > 0) "ok" else "neutral")
      ),
      va_interpretation(
        "Centering and multicollinearity",
        paste0(
          "Centering keeps the fitted curve and R-squared essentially unchanged, but it often reduces VIF by making x, x^2, x^3, and higher powers less collinear. ",
          "Use the centered coefficients for more stable inference; use the plot to communicate the fitted relationship."
        ),
        pills = tagList(
          va_pill("VIF < 5: acceptable", "ok", "check-circle"),
          va_pill("VIF 5-10: moderate", "warn", "exclamation-triangle"),
          va_pill("VIF > 10: severe", "bad", "times-circle")
        )
      ),
      va_table(raw_df),
      va_details("Technical details: raw and centered VIF values",
        c(
          "Raw VIF:",
          capture.output(print(raw_vif)),
          "",
          "Centered VIF:",
          capture.output(print(cent_vif))
        ))
    )
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
    curve_df <- rbind(
      data.frame(x = xgrid, y = predict(poly_fit, newdata = data.frame(x = xgrid)), Model = paste(names(r$raw_models)[r$selected_degree], "polynomial")),
      data.frame(x = xgrid, y = predict(cubic_spline, newdata = data.frame(x = xgrid)), Model = "Cubic spline")
    )
    curve_cols <- c("#f97316", "#22c55e")
    names(curve_cols) <- c(paste(names(r$raw_models)[r$selected_degree], "polynomial"), "Cubic spline")
    ggplot2::ggplot(r$df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "#38bdf8", alpha = 0.68, size = 2.4) +
      ggplot2::geom_line(data = curve_df, ggplot2::aes(x = x, y = y, color = Model, linetype = Model), linewidth = 1.1) +
      ggplot2::geom_vline(xintercept = r$knot_pair, color = "#cbd5e1", linewidth = 0.8, linetype = "dotted") +
      ggplot2::scale_color_manual(values = curve_cols) +
      ggplot2::labs(
        title = "Selected Polynomial vs Cubic Spline",
        subtitle = paste("Knots:", paste(fmt_value(r$knot_pair, 3), collapse = ", ")),
        x = r$x_label,
        y = r$y_label,
        color = NULL,
        linetype = NULL
      ) +
      va_plot_theme()
  })

  output$polySplineSummary <- renderUI({
    req(poly_results())
    r <- poly_results()
    fit <- r$spline_models[["Cubic spline (2 knots)"]]
    best_spline <- r$spline_compare$Model[which.min(r$spline_compare$AIC)]
    reg_model_summary_ui(
      "Cubic spline with two knots",
      fit,
      paste0(
        "The cubic spline allows the curve to bend around knots at ",
        paste(fmt_value(r$knot_pair, 3), collapse = " and "), ". ",
        "Among the displayed polynomial/spline candidates, the lowest AIC is currently ", best_spline, "."
      ),
      c(
        "Cubic spline with two knots",
        strrep("=", 35),
        paste("Knots:", paste(r$knot_pair, collapse = ", ")),
        "",
        capture.output(print(summary(fit)))
      ),
      extra_metrics = tagList(
        va_metric("Knots", paste(fmt_value(r$knot_pair, 3), collapse = ", "), "Piecewise breakpoints", "neutral"),
        va_metric("Best AIC model", best_spline, "From spline comparison table", if (best_spline == "Cubic spline (2 knots)") "ok" else "warn")
      ),
      extra_pills = tagList(
        va_pill("Spline terms act together", "neutral", "project-diagram"),
        va_pill(paste("Best AIC:", best_spline), if (best_spline == "Cubic spline (2 knots)") "ok" else "warn", "trophy")
      )
    )
  })

  output$polySplineResidualPlot <- renderPlot({
    req(poly_results())
    fit <- poly_results()$spline_models[["Cubic spline (2 knots)"]]
    resid_df <- data.frame(fitted = fitted(fit), residual = resid(fit))
    ggplot2::ggplot(resid_df, ggplot2::aes(x = fitted, y = residual)) +
      ggplot2::geom_hline(yintercept = 0, color = "#f97316", linewidth = 0.9, linetype = "dashed") +
      ggplot2::geom_point(color = "#22c55e", alpha = 0.72, size = 2.2) +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#38bdf8", linewidth = 0.95) +
      ggplot2::labs(
        title = "Cubic Spline Residuals vs Fitted",
        subtitle = "Look for random scatter around zero; patterns suggest remaining nonlinearity",
        x = "Fitted values",
        y = "Residuals"
      ) +
      va_plot_theme()
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
