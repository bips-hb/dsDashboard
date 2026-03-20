###############################################################################
#Program: app.R
#
#Project: STB
#
#R-Version: R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
#
#Purpose: A Shiny Dashboard using DataShield to access data from the Opal
#         servers at BIPS
#
#Prequeries:
#
#Output: A shiny dashboard
#
#Date/Changes/Author: 20/03/2026 / Andreas Mändle
###############################################################################


# Requirements:
# ----------------------
# (1) some R packages unavailable in CRAN are needed:
# devtools::install_github("haozhu233/kableExtra")  # new development version to make tables prettier
# devtools::install_github("datashield/dsBaseClient")  # this is now also available from CRAN
# devtools::install_github("datashield/dsBase")        # this is now also available from CRAN
# devtools::install_github("bips-hb/dsDashboard")
# ----------------------
# (2) some R packages might have to be installed:
# install.packages("gdtools") # if gdtools::addGFontHtmlDependency(family = c("Roboto Condensed","Roboto")) causes Error: object ‘match_fonts’ is not exported by 'namespace:systemfonts'
# update systemfonts if notifications fail with: "Warning: Error in : object ‘match_fonts’ is not exported by 'namespace:systemfonts'"
# devtools::install_github('r-lib/systemfonts')     # (CRAN version is too old)
# ----------------------
# (3) If unavailable and needed, download the Roboto font to the www/ folder:
# library(gdtools)
# gfonts::setup_font(
#   id = "roboto-condensed",
#   output_dir = "www/"
# )
# ----------------------
# (4) Shiny needs write access on the Shiny server:
# sudo chown -R shiny dash-test
#------------------------
# (5) Some stuff might have to be installed on the Shiny server:
# in ubuntu install:
# sudo apt-get install libfontconfig1-dev
# sudo apt-get install --reinstall xdg-utils
# sudo apt-get install libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
# For the fonts install in (Debian, Ubuntu):
#  # sudo apt-get install libcairo2-dev
#------------------------
# (6) If necessary, restart shiny server with:
# sudo systemctl restart shiny-server

# packages for the web interface
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinybusy)
library(gfonts)
library(gdtools)
# packages for data shield:
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(DSLite)
# packages for the plots
library(ggplot2)
library(ggiraph)
library(sparkline)
library(ggalluvial)
# devtools::install_github("bips-hb/ggiraphAlluvial") # requires access to the repository!!!
library(ggiraphAlluvial)
# packages for data handling
library(DT)
library(readxl)
library(dplyr)
# general functions for dashboards using DataSHIELD are collected in this package
library(dsDashboard)
# for asynchronous processes:
library(promises)
library(future)
future::plan(multisession)

# register the Roboto font for use in the dashboard
if(gdtools::match_family("Roboto Condensed")==""){
  systemfonts::register_font(name = "Roboto Condensed",
                             plain = list("www/RobotoCondensed.woff2", 0) )
}
gdtools::register_gfont("Roboto Condensed")
plot_font_set <- gdtools::font_set(sans = gdtools::font_google("Roboto Condensed"), serif= gdtools::font_google("Noto Serif"), mono= gdtools::font_google("Google Sans Code"),symbol= gdtools::font_google("Noto Color Emoji"))

# Set process global variables:
#-------------------------------
# some settings are specific to our dataset, and are not recommended for general use (turned off, if FALSE)
BIPS_inhouse <- F
# do not start in debug mode
debug_start <- F
# synthetic dataset path for DSlite mode
dataset_Rdata_path <- "data/datasample.Rdata"
# path for cached results
cache_path <- "cache"
# create cache directory, if it does not exist
dir.create(file.path(cache_path))
summary_prefix <- "summaryCollection"
alluvial_prefix <- "alluvialCollection"
metadata_prefix <- "metadataCollection"
selVars_prefix <- "selVars_"
numeric_prefix <- "isNumeric"
# path for login data for automatic global login
credentials_path <- "token.json"
# path for html footer, acknowledgement, data disclaimer
html_footer_path <- "ui_defaults/footer.html"
html_acknowledgement_path <- "ui_defaults/acknowledgement.html"
html_disclaimer_path <- "ui_defaults/disclaimer.html"
# read in the html files
html_footer <- readChar(html_footer_path, file.info(html_footer_path)$size)
html_acknowledgement <- readChar(html_acknowledgement_path, file.info(html_acknowledgement_path)$size)
html_disclaimer <- readChar(html_disclaimer_path, file.info(html_disclaimer_path)$size)
# path for ui default values
ui_defaults_path <- "ui_defaults/config.json"
ui_defaults <- jsonlite:::read_json(ui_defaults_path)
# cache results whenever possible?
cache_tabdata <- TRUE
workspace_name <- "DSdashdata" # name for the workspace where interim data are cached
hide_settings <- F   # hide settings (Burger menu)

# define UI elements

# Login-screen with buttons for
# (1) Adding OPAL servers
# (2) Login
# (3) DSLite demo
# (4) Local demo (without DataSHIELD)
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Sign in to ", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   uiOutput("allInputs"), # placeholder for inputs
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("appendInput", "Add another server", style = "color: white; background-color:#3c8dbc;
                                   padding: 10px 15px; width: 200px; cursor: pointer;
                                   font-size: 18px; font-weight: 600;"),
                     actionButton("login", "Sign in", style = "color: white; background-color:#3c8dbc;
                                   padding: 10px 15px; width: 200px; cursor: pointer;
                                   font-size: 18px; font-weight: 600;"),
                     actionButton("dsLiteLogin", "DS Lite Demo", style = "color: white; background-color:#3c8dbc;
                                   padding: 10px 15px; width: 200px; cursor: pointer;
                                   font-size: 18px; font-weight: 600;"),
                     actionButton("noLogin", "Local only Demo", style = "color: white; background-color:#3c8dbc;
                                   padding: 10px 15px; width: 200px; cursor: pointer;
                                   font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600;
                                              padding-top: 5px;font-size:16px;",
                                  class = "text-center")))
                   ),
                 )
)

# header for the dashboard (title, optional logout button)
header <- dashboardHeader(title = ui_defaults$header_title,
                          titleWidth = ui_defaults$header_title_width,
                          tags$li(class = "dropdown", conditionalPanel('input.show_logout == true', uiOutput("logoutbtn")) )
)

# left sideboard with additional functions and settings: disabled/collapsed
sidebar <- shinydashboardPlus::dashboardSidebar(uiOutput("sidebarpanel"), collapsed = TRUE, minified=FALSE, disable = F)

# main segment of the dashboard
body <- dashboardBody(
  # activate Shinyjs
  shinyjs::useShinyjs(),
  # logo segments for loading animation
  # below: CSS for svg height (interactive plot height)
  tags$head(tags$style(HTML("
      .sk-cube1 {
        background-color: transparent; background-size:300%; background-position: 0% 0%; background-image: url('logo_quad.png');
      }
      .sk-cube2 {
        background-color: transparent; background-size:300%; background-position: 50% 0%; background-image: url('logo_quad.png');
      }
      .sk-cube3 {
        background-color: transparent; background-size:300%; background-position: 100% 0%; background-image: url('logo_quad.png');
      }
      .sk-cube4 {
        background-color: transparent; background-size:300%; background-position: 0% 50%; background-image: url('logo_quad.png');
      }
      .sk-cube5 {
        background-color: transparent; background-size:300%; background-position: 50% 50%; background-image: url('logo_quad.png');
      }
      .sk-cube6 {
        background-color: transparent; background-size:300%; background-position: 100% 50%; background-image: url('logo_quad.png');
      }
      .sk-cube7 {
        background-color: transparent; background-size:300%; background-position: 0% 100%; background-image: url('logo_quad.png');
      }
      .sk-cube8 {
        background-color: transparent; background-size:300%; background-position: 50% 100%; background-image: url('logo_quad.png');
      }
      .sk-cube9 {
        background-color: transparent; background-size:300%; background-position: 100% 100%; background-image: url('logo_quad.png');
      }
      .girafe_container_std svg {
        max-height: 100vh;
      }")),
            # CSS for Roboto font
            tags$link(rel = "stylesheet", type = "text/css", href = "css/roboto-condensed.css"),
            # custom CSS to define where Roboto is used
            tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
            # in case of events log to browser console for debugging
            tags$script(HTML("$(document).on('shiny:value', (event) => {console.log('event.target');console.log(event.target);});")),
            # try too hide the burger (sidebar menu button)
            tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
  ),
  # add loading animation
  add_busy_spinner(spin = "cube-grid", margins = c(0, 10), color = "#112446"),
  # add space for all output (plots, summaries)
  uiOutput("body")
)

# build it all together (page = header + sidebar + body + footer)
# use footer from html_footer_path and insert it into the dashboard page
ui<-dashboardPage(header, sidebar, body,
                  footer = shinydashboardPlus::dashboardFooter(
                    left = HTML(html_footer)),
                  skin = "blue"
)

# global object that holds the DataSHIELD connections on the process scope
conns <- NULL

# global objects for summary information (available summaries, their label and description)
# vector of names of the summary statistic selections
summary_names <- c('min','25%','median','75%','max',
                   'mean','Nvalid','Ntotal', 'Nmissing',
                   'Ndistinct','sparkline_pie','sparkline_bar','densplot',     'missings',
                   'sd','var','boxplot',
                   'mode','levels','distinct','Mean',
                   '5%','10%','90%','95%','50%',
                   'used_levels','max_group_n')
# corresponding vector of descriptions for the popover on the summary statistics
summary_descriptions <- stringr::str_to_sentence(c(
  'smallest value','first quartile','median','third quartile','largest value',
  'sample mean','number of observations (without missing values)',   'total number of observations', 'number of missing values', #'number of observations (without missing values)',
  'number of distinct values (without missing values)','pie chart','bar chart','density plot', 'number of missing values',
  'empirical standard deviation', 'empirical variance','box plot',
  'most frequent value', 'list of possible factor values', 'number of distinct values (without missing values)','sample mean',
  '5% quantile','10% quantile','90% quantile','95% quantile','50% quantile, i.e. median',
  'distinct factor values with at least one observation', 'number of observations with the most frequent factor level'))
# corresponding vector of names (labels) for the summary statistics, as they are to be shown in the table
summary_labels    <-  c('min','25%','median','75%','max',
                        'mean','n<sub></sub>','n<sub>total</sub>','n<sub>miss</sub>',
                        'n<sub>distinct</sub>','pie chart','bar chart','density','n<sub>Msng</sub>',
                        'sd','var', 'boxplot',
                        'mode','levels','n<sub>distinct</sub>','mean',
                        '5%','10%','90%','95%','50%',
                        'levels','n<sub>group_max</sub>') # names used in table header (choices in selectize input are specified in the input)

#global login
do_global_login = F       # if TRUE, login using credentials in credentials_path + skip login screen
global_process_login = F  # status, is the app globally logged in? at the beginning FALSE

# if TRUE, start the global login (on process scope)
if (do_global_login) {
  # read in credentials from JSON file
  global_process_creds <- jsonlite::read_json(credentials_path) %>% lapply(jsonlite::fromJSON) %>% unlist()

  # login to each Opal server
  global_process_builder <- DSI::newDSLoginBuilder()
  global_process_builder$append(server=global_process_creds["server"],
                                url=global_process_creds["url"],
                                token= global_process_creds["token"],
                                profile=global_process_creds["profile"])
  global_process_logindata <<- global_process_builder$build()

  # deactivate the signature check... (prevents SSL issues)
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # try to perform login
  tryCatch( { conns <- DSI::datashield.login(logins = global_process_logindata) } #, assign = TRUE, symbol="tab1") }
            , error = function(e) {conns <<- NULL})

  # if there are DataSHIELD connections...
  if(!is.null(conns)) {
    # assign all tables
    global_process_allTables <- dsDashboard::assignAllTables(conns)
    message("--all tables assigned--")

    # restore saved workspace (precomputed subset tables)
    tryCatch( {
      DSI::datashield.workspace_restore(conns, workspace_name)
      message(paste("Workspace", workspace_name, "restored..."))
    }, error = function(e) { message(paste("Workspace", workspace_name, "could not be restored!"))})

    # update status - now global process is logged in
    global_process_login <- TRUE
    message("Login to DataSHIELD completed.")

  } else {
    # critical error if global login fails
    stop("Global process login to DataSHIELD unsuccessful")
  }

}

# define server logic
server <- function(input, output, session) {
  # logged in status for session is saved in variable login
  # if global login is used, then the session is also logged in
  login <- global_process_login
  # save login status as reactive value
  USER <- reactiveValues(login = login,                # session based login status
                         login_mode=NULL,              # 0: login screen, 1: DataSHIELD, 2: DSLite, 3: local data
                         data = NULL,                  # data slot for local data mode
                         dslite.server = NULL,         # DSLite server for DSLite mode
                         logindata=NULL,               # session based logindata for DataSHIELD/DSLite
                         useLabels=TRUE,               # show labels instead of variable names (if available)
                         hist_grp1_last=NULL,          # hold histogram input selection
                         hist_grp2_last=NULL,          # hold histogram input selection
                         hist_grp1_new=NULL,           # hold histogram input selection
                         hist_grp2_new=NULL,           # hold histogram input selection
  )

  globals <- reactiveValues(
    allTables = data.frame(),      # available tables
    selectedTable = character(),   # selected table
    selectedFtrs = character(),    # selected primary variables (numerical or categorical)
    selectedNumericalFtrs = character(),  # selected primary variables (only numerical)
    selectedGrps = character(),          # selected grouping variables
    selectedGrpsTD = character(),        # selected groups for alluvial plots - might contain JSON
    selectedGrpsTDVec = character(),     # selected groups for alluvial plots - no JSON, but character vector
    selectedX = character(),
    selectedY = character(),
    validModDepVar = FALSE,          # dependent variable for model fit is valid (dependent variables may not be unordered factors with more than 2 levels)
    modFitError = FALSE,             # true, if the requested modelFit cannot be generated due to restrictions in DataSHIELD and the data set
    extra_notifications = FALSE, # suppress additional notifications
    notification_duration = 5,   # duration of notifications
    group_names = character(),
    tabVars = character(),
    tabVarsNumeric = logical(),
    inpTableSelectChoices = character(),  # selectable tables in input field
    safemode = logical(),    # for checkboxInput: restricted mode on/off (no scatterplots, no regression, simplified dashboard)
    debugmode = logical(),   # for checkboxInput: debugging mode on/off
    use_names = logical(),   # for checkboxInput: are names used or labels?
    show_logout = logical(), # show logout button?
    choicesFeatureSelect = list(),          # selectable primary variables in input field
    choicesNumericalFeatureSelect = list(), # selectable numerical primary variables
    choicesGroupFeatureSelect = list(),
    choicesGroupSelect = character(),       # selectable grouping variables
    choicesTDFeatureSelect = list(),        # selectable (time dependent) variables for alluvial plots
    summaryDataCollection = NULL,  # NULL or a list of already computed data for summary tables
    alluvialDataCollection = NULL, # NULL or a list of already computed data for alluvial plots
    metaDataCollection = list(),   # list for storing metadata
    ptLimit = logical(),           # maximum number of points to plot (in scatterplot)
    bins = numeric(),              # current number of bins for histogram
    # ids of the interative plots (bar, box, alluvial and scatter)
    svgid_bar = character(),
    svgid_box = character(),
    svgid_alluvial = character(),
    svgid_scatter = character(),
    creds = NULL,   # credentials
    conns = NULL    # session scope DataSHIELD connections
  )

  # if globally logged in, copy table information from global process variable into globals
  if (global_process_login && !is.null(global_process_allTables)) {
    globals$allTables <- global_process_allTables
  }

  # if globally logged in, copy login data from global process variable into globals
  if (global_process_login && !is.null(global_process_logindata)) {
    USER$logindata <- global_process_logindata
  }

  # get the names of the available tables (as character vector)
  getTableList <- function() {
    rs.names <- globals$allTables[,2]
    rs <- paste0("tab", 1:length(rs.names))
    names(rs) <- rs.names
    return(rs)
  }

  # specify delay popover information in summary tables
  # to do this, the kableExtra::spec_popover function is extended:
  # cf. bootstrap documentation for further options: https://getbootstrap.com/docs/4.1/components/popovers/#usage
  spec_popover_delay <- function(content = NULL, title = NULL,
                                 trigger = "hover", position = "in right", delay = list(show=250,hide=500)
  ) {
    trigger <- match.arg(trigger, c("hover", "click", "focus", "manual"),
                         several.ok = TRUE)
    position <- match.arg(position, c("bottom", "top", "left", "right", "auto",
                                      "in bottom", "in top", "in left", "in right", "in auto"),
                          several.ok = TRUE)
    popover_options <- paste(
      'data-toggle="popover" data-container="body"',
      paste0('data-trigger="', paste(trigger, collapse=" "), '"'),
      ifelse(!is.null(delay), paste0('data-delay=\'', jsonlite::toJSON(delay, auto_unbox=T), '\''), ""),
      paste0('data-placement="', position, '"'),
      ifelse(!is.null(title), paste0('title="', title, '"'), ""),
      paste0('data-content="', content, '"'))
    popover_options_list <- list(
      'data-toggle' = 'popover',
      'data-container' = 'body',
      'data-trigger' = trigger,
      'data-placement' = position,
      'data-content' = content
    )
    if (!is.null(title)) {
      popover_options_list['title'] <- title
    }
    if (!is.null(delay)) {
      popover_options_list[['data-delay']] <- unlist(delay)
    }
    class(popover_options) <- "ke_popover"
    attr(popover_options, 'list') <- popover_options_list
    return(popover_options)
  }

  # handle URL parameters
  # (1) to choose login mode directly and skip the login screen
  #     (bypassing the global login on the process scope)
  # (2) to activate debug mode
  # ATTENTION: in a productive system this would be probably commented out
  # to prevent users from accessing other login modes
  observeEvent(session$clientData$url_search, {
    # read GET parameters from URL
    debug_mode <- NULL
    USER$login_mode <<- parseQueryString(session$clientData$url_search)$mode
    debug_mode <- parseQueryString(session$clientData$url_search)$debug

    if (is.null(debug_mode)) debug_mode <- F else debug_mode <- as.logical(as.numeric(debug_mode))
    isolate(globals$debugmode <- debug_mode)

    # check if login mode is specified by URL
    if (is.null(USER$login_mode)) { # nothing specified
      # set login mode to 0 (default)
      USER$login_mode <<- 0
      # hide burger menu (assuming we are in default kiosk mode, additional user setting should be hidden)
      if (hide_settings) shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';") # burger button hidden

      # copy process global connections to session scope connections
      if (do_global_login) {
        globals$conns <- conns
        globals$creds <- global_process_creds
      }
    } else {
      # login mode requested through URL
      message("session based login requested through URL")
      USER$login <<- FALSE
    }

    # log current login_mode
    if (USER$login_mode==0) {        # mode 0: show login screen
      message("default login mode") # no automatic session login -> show login screen
    } else if (USER$login_mode==3) { # mode 3: use local data
      message("local data login mode")
    } else if (USER$login_mode==2) { # mode 2: DSLite
      message("DSLite login mode")
    } else if (USER$login_mode==1) { # mode 1: auto login to DataSHIELD
      message("DataSHIELD login mode")
    }
  })

  # a function that assigns the DSLite demo data, including a pooled table
  doDSLiteAssignments <- function() {
    # to demonstrate/test how the dashboard works with multiple servers...
    # create a pooled dataset over all three servers
    datashield.assign.table(globals$conns, 'pooled', table = list(server1='syndat_group1', server2='syndat_group2'))
    globals$allTables$id <- as.list(globals$allTables$id)
    globals$allTables <- rbind(globals$allTables, c("  ","pool","pooled"))
    globals$allTables$id[[length(globals$allTables$id)]]<- I(c("server1","server2"))
  }

  get_n_save_metadata <- function(tab, tabsymbol, mode_str, selVars, cur_conn) {
    # metadata_prefix and cache_path are global variables

    # build up metadata path
    mode_str <- ifelse(USER$login_mode==0,"",paste0("mode",USER$login_mode))
    metadata_path <- file.path(cache_path,paste0(metadata_prefix,"_",tab,"_",mode_str,".rds"))

    if (file.exists(metadata_path)){
      # load metadata for table tab into object var_metadata
      msg <- paste("Load metadata from",metadata_path)
      message(msg)
      if (globals$extra_notifications) showNotification(msg, duration=globals$notification_duration, type="message")
      var_metadata <<- readRDS(metadata_path)
    } else {
      # show notification
      msg <- "Get metadata from DataShield. This might take several minutes depending on the number of available variables. Wait or come back later..."
      showNotification(msg, duration=globals$notification_duration*5, type="message")

      # extract metadata from DataSHIELD
      vars_4_meta <- sapply(selVars, function(x) paste0(tabsymbol,"$",x)) %>% unlist() # sapply, also works for empty selVars; in that case unlist turns empty list into NULL
      var_metadata <- dsDashboard::ds.meta2(vars_4_meta,
                                                cur_conn)
      # save metadata in file
      saveRDS(var_metadata, file=metadata_path)
    }
    # save metadata in globals
    globals$metaDataCollection[[tab]] <- var_metadata

    rm(metadata_path)
    # return metadata
    return(var_metadata)
  }

  observe({
    # if not logged in...
    if (USER$login == FALSE) {
      # automatic session scope login using DataSHIELD (if USER$login_mode==1)
      if (USER$login_mode==1) {
        # show messages
        message("mode 1: try to login to DataSHIELD automatically")
        showNotification("Login to Opal server...", duration=globals$notification_duration)

        # read credentials from JSON file
        isolate(globals$creds <- creds <- jsonlite::read_json(credentials_path) %>% lapply(jsonlite::fromJSON) %>% unlist() )

        # Login to each Opal-SERVER:
        # (1) build login object
        builder <- DSI::newDSLoginBuilder()
        builder$append(server=creds["server"],
                       url=creds["url"],
                       token= creds["token"],
                       profile=creds["profile"])
        isolate(USER$logindata <<- builder$build())
        # (2) deactivate the signature check to avoid SSL issues
        httr::set_config(httr::config(ssl_verifypeer = 0L))
        # (3) login and check if successful
        tryCatch( { session_conns <- DSI::datashield.login(logins = USER$logindata, assign = TRUE) }
                  , error = function(e) {session_conns <<- NULL})

        # save new DataSHIELD connections in globals$conns
        isolate(globals$conns <- session_conns)

        # if any DataSHIELD server is connected...
        if(!is.null(globals$conns)) {
          # assign all tables (to symbols tab1, tab2,...) and remember symbols in globals$allTables
          isolate(globals$allTables <- dsDashboard::assignAllTables(globals$conns))
          message("--all tables assigned--")
          # restore workspaces on the DataSHIELD servers
          tryCatch( {
            DSI::datashield.workspace_restore(globals$conns, workspace_name)
            # show messages
            message("Workspace restored...")
            showNotification("Workspace restored...", duration=globals$notification_duration*3)
          }, error = function(e) {
            # in case of error show messages
            message("Workspace not loaded")
            showNotification("Workspace could not be loaded.", duration=globals$notification_duration*3)
          })

          USER$login <<- TRUE
          # user feedback to announce that login is completed
          showNotification("Login to DataSHIELD completed.", duration=globals$notification_duration)
        } else {
          # if no DataSHIELD connection available, fade out and show warning
          shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          showNotification("Login to DataSHIELD unsuccessful", duration=globals$notification_duration, type="warning")
        }
      }
      else if (   (!is.null(input$login)) && (input$login > 0) ) {
        # DataSHIELD login via login button in the login screen using user defined credentials
        # show notification
        showNotification("Login to Opal servers...", duration=globals$notification_duration, type="message")

        # login procedure
        builder <- DSI::newDSLoginBuilder()
        # get number of selected servers
        nServer <- input$appendInput+1
        # add each server to login builder
        lapply(1:nServer,function(i){
          builder$append(server = paste0("server",i),
                         url = input[[paste0("input",i,"")]],
                         user = ifelse (input[[paste0("input",i,"ToP")]]==0, input[[paste0("input",i,"Usr")]], "" ),
                         password = ifelse (input[[paste0("input",i,"ToP")]]==0, input[[paste0("input",i,"Pwd")]], "" ),
                         token = ifelse (input[[paste0("input",i,"ToP")]]==1, input[[paste0("input",i,"Tkn")]], "" ),
                         profile = input[[paste0("input",i,"Pfl")]] )
        })
        USER$logindata <<- builder$build()
        # deactivate the signature check...
        httr::set_config(httr::config(ssl_verifypeer = 0L))
        # login and check if successful
        tryCatch( { globals$conns <- DSI::datashield.login(logins = USER$logindata, assign = TRUE) }
                  , error = function(e) {globals$conns <<- NULL})

        # if successful login...
        if(!is.null(globals$conns)) {
          # assign all tables to symbols (tab1, tab2, ...)
          globals$allTables <- dsDashboard::assignAllTables(globals$conns)
          message("--all tables assigned--")
          USER$login <<- TRUE
          showNotification("Login to DataSHIELD completed.", duration=globals$notification_duration)
        } else {
          # error if login unsuccessful
          # deactivate dashboard
          shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
          # wait 3 seconds
          shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          # warning / notification
          showNotification("Login to DataSHIELD unsuccessful", duration=globals$notification_duration, type="warning")
        }
      } else if (  (USER$login_mode==2) || ((!is.null(input$dsLiteLogin)) && (input$dsLiteLogin > 0))  ) {
        # mode 2: login to DSLite (initiated by URL parameter OR click on "DS Lite Demo")
        message("mode 2: DSlite mode")

        # set USER$login_mode (possible mismatch if login started by clicking input)
        isolate({ USER$login_mode <- 2})

        showNotification("Login to DSlite...", duration = globals$notification_duration)

        # in DSLite mode we need the dsBase package!
        library(dsBase)

        # load sample dataset defined in dataset_Rdata_path:
        # it consists of three data.frames, syndat_main, syndat_hungary and syndat_spain
        tmp_env <- new.env()
        df_name <- load(dataset_Rdata_path, tmp_env)
        USER$data <- lapply(df_name, function(x) get(x, envir=tmp_env))
        names(USER$data) <- df_name
        rm(tmp_env)

        # set options for DataSHIELD connection
        my_config <- DSLite::defaultDSConfiguration()
        my_config$Options$default.nfilter.levels.max <- 160 # for alluvial plots we need more factor levels
        # my_config$Options$default.nfilter.tab <- 3 # if set to 0, more plot data can be shown, but less data privacy
        USER$dslite.server <<- DSLite::newDSLiteServer(tables=USER$data,
                                                       config = my_config)

        # put each of the tables on its own virtual DSLite server
        for (i in seq_len(length(df_name))) {
          assign(paste0("dslite.server_obj",i),
                 DSLite::newDSLiteServer(tables=USER$data[i], config = my_config),
                 envir = rlang::global_env() ) # environment getOption("datashield.env", parent.frame()) would lead to trouble
        }

        # build DS login information
        builder <- DSI::newDSLoginBuilder()
        for (i in seq_len(length(df_name))) {
          builder$append(server = paste0("server",i), driver = 'DSLiteDriver', url = paste0('dslite.server_obj',i))
        }
        USER$logindata <<- builder$build()

        # do login and assign all tables
        globals$conns <- datashield.login(USER$logindata)
        globals$allTables <- dsDashboard::assignAllTables(globals$conns)

        # assign the DSLite demo data
        doDSLiteAssignments()

        # move pooled table (last row) to the top
        globals$allTables <- globals$allTables[c(nrow(globals$allTables), seq(nrow(globals$allTables)-1)), ]
        # the rownames are used later in the code and must be equal to the row numbers
        # removing rownames resets it to the correct row number
        rownames(globals$allTables) <- NULL

        # login completed
        USER$login <- TRUE
        showNotification("Login to DSlite completed.", duration=globals$notification_duration)
      } else if ((USER$login_mode==3) || ((!is.null(input$noLogin)) && (input$noLogin > 0))) {
        # mode 3: local mode, no DataSHIELD is used in this demo
        message("mode 3: local mode")

        # set USER$login_mode (possible mismatch if login started by clicking input)
        isolate({ USER$login_mode <- 3})

        showNotification("Use dashboard in local mode...", duration = globals$notification_duration)

        # if DataSHIELD connection has been active in the session, logout
        # ATTENTION: logout only from session connenctions, not from the process wide global connection
        if (!identical(conns, globals$conns)) DSI::datashield.logout(globals$conns)
        globals$conns <- NULL

        # load sample dataset defined in dataset_Rdata_path:
        # it consists of three data.frames, syndat_main, syndat_hungary and syndat_spain
        tmp_env <- new.env()
        df_name <- load(dataset_Rdata_path, tmp_env)
        USER$data <- lapply(df_name, function(x) get(x, envir=tmp_env))
        names(USER$data) <- df_name
        rm(tmp_env)

        # save a summary of the available tables in globals$allTables
        globals$allTables <- data.frame(id=rep("local",length(USER$data)), .x=names(USER$data))
        globals$allTables$id <- as.list(globals$allTables$id)

        # default selection to first table
        globals$selectedTable <- paste0("tab", 1)

        # save name of selected table in tabname
        tabname <- globals$allTables[readr::parse_number(globals$selectedTable),2]

        # get labels of all columns of selected table
        v_labels <- sapply(USER$data[[tabname]], function(x) if(is.null(attr(x,"label"))) "" else attr(x,"label"))
        # where no label is available, use column name instead
        v_labels[v_labels==""] <- names(v_labels[v_labels==""])

        # get names of all columns of selected table
        v_names <- names(USER$data[[tabname]])
        names(v_names) <- v_labels

        USER$login <- TRUE
        showNotification("Login to local mode completed.", duration = globals$notification_duration)
      }
      # load summary statistics, that are already computed
      mode_str <- ifelse(USER$login_mode==0,"",paste0("mode",USER$login_mode))
      summary_path <- file.path(cache_path,paste0(summary_prefix,mode_str,".rds"))
      if (cache_tabdata && file.exists(summary_path)) {
        globals$summaryDataCollection <<- readRDS(summary_path)
      }

    } else {
      # User is already logged in

      # if Shiny inputs are not not ready, stop here (table must be chosen!)
      req(input$inpTableSelect)

      message("User is already logged in")

      # keep global logins alive
      if (!is.null(conns)) lapply(conns,dsKeepAlive)   # connections on process scope
      if (!is.null(globals$conns)) lapply(globals$conns,dsKeepAlive)  # connections on session scope

      # check if table selection changed
      if (!is.null(input$inpTableSelect) && ((length(globals$selectedTable)==0) || (input$inpTableSelect != globals$selectedTable)) ) {
        globals$selectedTable<-input$inpTableSelect
        tab_changed <- T
      } else tab_changed <- F

      # update selections/settings/actions from inputs:
      # -----------------------------------------------
      # currently selected table
      tab <- input$inpTableSelect
      message(paste("current table:", tab))
      # get "number" of the current table
      tabnr <- readr::parse_number(tab)
      tabsymbol <- globals$allTables[tabnr,"symbol"]
      tabname <- globals$allTables[tabnr,".x"]
      cur_conn <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables
      # update histogram input variables (grouping type)
      USER$hist_grp1_new <<- input$hist_grp1_type
      USER$hist_grp2_new <<- input$hist_grp2_type
      # useLabels if input field checked
      USER$useLabels <<- !as.logical(input$use_names)

      # if group selection history unavailable, set it to current status
      if (is.null(USER$hist_grp1_last)) USER$hist_grp1_last <<- input$hist_grp1_type
      if (is.null(USER$hist_grp2_last)) USER$hist_grp2_last <<- input$hist_grp2_type

      # if new group styles are selected, update radio buttons in barplot panel
      if (!is.null(USER$hist_grp1_new) && !is.null(USER$hist_grp2_new) && !is.null(USER$hist_grp1_last) && !is.null(USER$hist_grp2_last) ) {
        if (USER$hist_grp2_last!=USER$hist_grp2_new) { # style for group 2 changed => change style for group 1
          updateRadioButtons(session, "hist_grp1_type", selected = if (USER$hist_grp2_new=="facets") USER$hist_grp2_last else "facets")
          # remember the selection to detect future changes
          USER$hist_grp2_last <<- input$hist_grp2_type
          return()
        } else if (USER$hist_grp1_last!=USER$hist_grp1_new) { # style for group 1 changed => change style for group 2
          updateRadioButtons(session, "hist_grp2_type", selected = if (USER$hist_grp1_new=="facets") USER$hist_grp1_last else "facets")
          # remember the selection to detect future changes
          USER$hist_grp1_last <<- input$hist_grp1_type
          return()
        }
      }

      # update selVars, var_metadata and numeric_vars
      if (is.null(cur_conn)) {
        # local mode:
        #------------------------------------
        # get corresponding table from USER$data and load it into theTable
        theTable <- data.frame(USER$data[[tabname]],
                               check.names = FALSE)
        # remember the available variables in selVars
        selVars <- colnames(theTable)

        # remember which variables are numeric, and which are not numeric
        globals$tabVarsNumeric <<- numeric_vars <- sapply(theTable, is.numeric)

        # collect metadata from attributes "spec", "label",...
        meta_attributes <- c("spec", "label","class","opal.value_type","opal.nature")
        var_metadata <- lapply(meta_attributes, function(x) lapply(setNames(selVars,selVars), function(v) attr(theTable[[v]],x)   ) )
        names(var_metadata) <- meta_attributes
        var_metadata <- do.call(cbind, var_metadata) %>% as.data.frame()
        # save metadata in globals
        globals$metaDataCollection[[tab]] <- var_metadata
      } else {
        # DataSHIELD / DSLite modes
        # check if table selection was changed and determine/update selVars
        if  ( (tab_changed) ||
              ( !(length(globals$selectedTable)>0 && globals$selectedTable==tab )  )) {
          # table tab was changed: update selectable variables "selVars":
          # name for cached files includes a table string "tab" and the login mode if login mode > 0
          mode_str <- ifelse(USER$login_mode==0,"",paste0("mode",USER$login_mode))
          # build up filename for cache file containing variable names for table "tab" in object "selVars"

          selVarFilename <- file.path(cache_path,paste0(selVars_prefix,tab,mode_str,".rds"))

          if (cache_tabdata && file.exists(selVarFilename)){
            # use the cache, it is available
            # show messages
            msg <- "Load cached variable names..."
            message(msg)
            if (globals$extra_notifications) showNotification(msg, duration=globals$notification_duration, type="message")
            # load cached variable names
            selVars <- readRDS(selVarFilename)
          } else {
            # no use of cached file ->  get variable names from server
            msg <- "Read variable names from DataSHIELD (this takes some seconds)..."
            message(msg)
            showNotification(msg, duration=globals$notification_duration, type="message")

            selVars <- dsDashboard::dsUniqueVarnames(tabsymbol, cur_conn)
            saveRDS(selVars, file=selVarFilename)
          }

          # save variable names in globals
          globals$tabVars <- selVars

        } else {
          message("table selection unchanged, no need to update variable names")
          # use the variable names which are saved in the globals
          selVars <- globals$tabVars
        }

        if (USER$useLabels) {
          # use variable labels instead of variable names

          # determine/update var_metadata
          if (is.null(globals$metaDataCollection[[tab]])) {
            # no metadata saved in globals

            # get metadata from file or DataSHIELD
            var_metadata <- get_n_save_metadata(tab, tabsymbol, mode_str, selVars, cur_conn)

          } else {
            # metadata are already available in globals$metaDataCollection
            message(paste0("globals$metaDataCollection[[",tab,"]] already contains the metadata for the current table"))
            # remember metadata in var_metadata
            var_metadata <- as.data.frame(globals$metaDataCollection[[tab]])
          }
        }

        # check if table selection was changed and determine/update numeric_vars
        if (tab_changed ||
            !(length(globals$selectedTable)>0 && globals$selectedTable==tab)) {
          # build filename for "isNumeric" cache file
          mode_str <- ifelse(USER$login_mode==0,"",paste0("mode",USER$login_mode))
          isNum_filename <- file.path(cache_path,paste0(numeric_prefix,tab,mode_str,".Rdata"))
          if (file.exists(isNum_filename)){
            # if available, load the information from the cache file into object numeric_vars
            load(file=isNum_filename)
          } else {
            # otherwise get information from DataSHIELD and save it to cache file
            msg <- "Check which variables are numeric using DataShield. This might take several minutes. Wait or come back later..."
            showNotification(msg, duration=globals$notification_duration*5, type="message")
            numeric_vars <- dsDashboard::dsIsNumeric(tabsymbol, unname(selVars), datasources=cur_conn)
            # save numeric_vars in file
            save(numeric_vars, file=isNum_filename)
          }
          # save numeric_vars in globals
          isolate(globals$tabVarsNumeric <- numeric_vars)
        } else {
          # get numeric_vars from globals
          numeric_vars <- globals$tabVarsNumeric
        }

        # remove entries in numeric_vars which have no match in selVars...
        # which happens only if variables have been removed from selVars
        numeric_vars <- numeric_vars[names(numeric_vars) %in% selVars]

        # save current table name tab in globals
        isolate(globals$selectedTable <- tab)
      }

      # use labels (possibly from spec attribute)
      if (USER$useLabels) {
        # standard names from label
        names(selVars) <- var_metadata["label"][selVars,,drop=F] %>% unlist() %>% unname()

        # if spec attribute available, update labels for the variables from the metadata
        if (!(is.null(var_metadata$spec) || all(unlist(lapply(var_metadata$spec,is.null))) ) ) {
          # add labels to the vector of variable names
          message("labels from spec attribute are added to selVars")
          names_from_spec <- unlist(lapply(as.matrix(var_metadata)[,"spec"], function(x) {
            if (is.null(x)) character(0) else jsonlite::fromJSON(x)$alias
          }))[selVars]
          names(selVars) <- ifelse(is.na(names_from_spec), names(selVars), names_from_spec)
        }
      }

      # get group (category or topic) from spec attribute for each variable in selVars
      globals$group_names <- setNames(sapply(var_metadata["spec"][selVars,], function(x) if (is.null(x)) NA_character_ else jsonlite::fromJSON(x)$cat ), selVars)

      # update input fields, if either no variables are in inputs or variables that do not belong to tab are in inputs
      if (length(globals$inpTableSelectChoices)==0 || !all(globals$inpTableSelectChoices %in% selVars) || !all(selVars %in% globals$inpTableSelectChoices)) {
        message("Input choices in globals will be updated...")
        if (globals$extra_notifications) showNotification("Input choices are updated...", duration=globals$notification_duration, type="default")
        globals$inpTableSelectChoices <- selVars

        # are the variables split into different categories (in metadata)?
        if (sum(!is.na(globals$group_names)) > 0) {
          # if groups (categories) are specified for the variables, they will be used
          labels_for_var <- setNames(names(selVars), selVars)
          select_items <- list()
          select_items_numeric <- list()
          select_items_nonnumeric <- list()
          for (i in sort(unique(  tidyr::replace_na(globals$group_names, "") ))) {
            features_in_group_i <- names(globals$group_names)[globals$group_names==i]
            features_in_group_i <- setNames(features_in_group_i, labels_for_var[features_in_group_i])
            features_in_group_i_numeric <- features_in_group_i[features_in_group_i %in% names(numeric_vars)[numeric_vars==TRUE]]
            features_in_group_i_nonnumeric <- features_in_group_i[features_in_group_i %in% names(numeric_vars)[numeric_vars==FALSE]]
            select_items[[i]] <- sort(features_in_group_i)
            select_items_numeric[[i]] <- sort(features_in_group_i_numeric)
            select_items_nonnumeric[[i]] <- sort(features_in_group_i_nonnumeric)
          }
          globals$choicesFeatureSelect <- select_items
          globals$choicesNumericalFeatureSelect <- select_items_numeric
          globals$choicesGroupFeatureSelect <- select_items_nonnumeric
        } else {
          ## otherwise the variables are grouped into numerical variables and factors
          globals$choicesFeatureSelect <- list("Numerical variables"=selVars[numeric_vars],
                                               "Factor variables" =selVars[!numeric_vars]  )
          globals$choicesNumericalFeatureSelect <- globals$choicesFeatureSelect$Num
          globals$choicesGroupFeatureSelect <- selVars[!numeric_vars]
        }

        # determine input choices for alluvial plot (for data selection over all cohorts)
        ## these variables need to be non-numeric ...
        choicesTDFeatureSelect  <- selVars[!numeric_vars]
        ## determine if a variable is a cohort variable:
        ## if there are more than one variables with
        ## - the same label (in case of spec attribute)
        ## - or with the same first line of the label (in case of label attribute)
        ## they are considered to be variables from different cohorts

        # get labels
        labels_attr <- sapply(names(choicesTDFeatureSelect), function(x) sub("\n.*", "", x) ) %>% unname()
        labels_spec <- sapply(var_metadata[choicesTDFeatureSelect,"spec"], function(x) if (is.null(x)) NA_character_ else jsonlite::fromJSON(x)[["label"]] )
        labels_use <- ifelse(sapply(labels_spec,shiny::isTruthy), labels_spec, labels_attr)
        cohort_vars <- setNames(choicesTDFeatureSelect, labels_use)[labels_use %in% unique(labels_use[duplicated(labels_use)])]
        ## get names labels of variables which are available in at least two stages
        var_names <- unique(names(cohort_vars))

        # save the vector of names of features for the alluvial plots temporarily in foo
        # variables that belong together (same feature, different cohorts) are coupled in a json string
        foo <- lapply(var_names, function(x) jsonlite::toJSON(unname(cohort_vars[names(cohort_vars)==x]) ) )
        names(foo) <- var_names
        # they are sorted alphabetically (we have few variables for the alluvial plots, therefore no ordering according to their category is performed)
        foo <- foo[sort(names(foo))]

        if (BIPS_inhouse) {
          # remove age group, smoking status and status of participant from alluvial plot input
          foo[["Age group"]] <- NULL
          foo[["Smoking status"]] <- NULL
          foo[["Status of participant"]] <- NULL

          # remove "in:" variables from alluvial plot input
          in_names <- names(foo[startsWith(names(foo), "In: ")])
          for (i in in_names) foo[[i]] <- NULL
        }

        # save variable names for alluvial plots in globals$choicesTDFeatureSelect
        globals$choicesTDFeatureSelect  <- foo

        # remove variables
        rm(foo)
        rm(var_names)

        # update inputs: fill in variable names as choices
        message("update shiny inputs")

        # check for consistency: are there numeric variables, that should not be available any more?
        vars_too_much <- setdiff(names(numeric_vars), selVars)
        # then keep only variables, that are still available and continue
        if (length(vars_too_much)>0) {
          showNotification("Numeric vector list is not up to date!", duration = globals$notification_duration*3, type="warning")
          safe_vars <- intersect(names(numeric_vars), selVars)
          numeric_vars <- numeric_vars[safe_vars]
          safe_vars <- intersect(selVars, names(numeric_vars))
          selVars <- safe_vars
        }

        # if TRUE, define categories which go on the top: General and Demographic information
        if (BIPS_inhouse) first_section_names <- c("General", "Demographic information") else first_section_names <- c()

        # update the shiny inputs
        updateSelectInput(session, "inpFeatureSelectXY", choices = globals$choicesFeatureSelect[c(first_section_names, setdiff(names(globals$choicesFeatureSelect), first_section_names) )] )
        updateSelectInput(session, "inpNumericalFeatureSelectXY", choices = globals$choicesNumericalFeatureSelect[c(first_section_names, setdiff(names(globals$choicesNumericalFeatureSelect), first_section_names) )] )
        updateSelectInput(session, "inpGroupSelectXY", choices = globals$choicesGroupFeatureSelect[c(first_section_names, setdiff(names(globals$choicesGroupFeatureSelect), first_section_names) )]  )
        updateSelectInput(session, "inpTDFeatureSelect", choices = globals$choicesTDFeatureSelect)
      }

      # when feature selection is changed, trigger plot update (by updating globals): this activates the observer after input is changed
      message("trigger plot update")
      # update groups in globals for alluvial plots (JSON)
      globals$selectedGrpsTD <- grpsTD <- input$inpTDFeatureSelect
      # update decoded groups object in globals for alluvial plots
      if (!is.null(grpsTD) && jsonlite::validate(grpsTD))
        globals$selectedGrpsTDVec <-  jsonlite::fromJSON(grpsTD) else grpsTD
      # update grouping variables in globals
      globals$selectedGrps <- grps <- input$inpGroupSelectXY
      # update primary variables in globals
      ftrs_all <- input$inpFeatureSelectXY
      # update numerical primary variables in globals
      ftrs_num <- ftrs_all[ftrs_all %in% names(numeric_vars)[numeric_vars==T]]
      rm(numeric_vars)

      # check for updates in numerical feature input
      num_features <- input$inpNumericalFeatureSelectXY
      if (!identical(globals$selectedNumericalFtrs, num_features)) globals$selectedNumericalFtrs <- num_features

      # check for input changes for barplots
      if (!is.null(ftrs_all) && length(ftrs_all)>0 && # are there features selected?
          (length(globals$selectedFtrs) != length(ftrs_all) || # or:  mismatch of length of features in globals and length of selected features
           !all(globals$selectedFtrs == ftrs_all))) {          # or:  mismatch of features in globals and selected features
        # input changed

        # special case: histogram, trigger a new bin default
        if (length(ftrs_num)>0 ) {
          message("selected numerical features for histogram changed - determine new default bin number")
          # determine number of bins by Sturges method
          if (is.null(globals$conns)) {
            lengths <- as.list(sum(!is.na(data.frame(USER$data[[globals$allTables[readr::parse_number(tab),]$.x]],check.names = FALSE)[[ftrs_num[1]]])))
          } else {
            # if available, read the information
            if (!is.null(globals$summaryDataCollection[[tab]]) &&
                !is.null(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(grps)]]) &&
                length(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(grps)]]) > 0  ) {
              # read Nvalid from summaryDataCollection
              lengths <- list(combined = jsonlite::fromJSON(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]])  %>%
                                dplyr::filter(variable==ftrs_num[1], feature=="Nvalid") %>%
                                dplyr::pull(value) %>% as.numeric() )
            } else {
              # ask data shield
              lengths <- DSI::datashield.aggregate(conns=cur_conn, call("lengthDS", paste0(tabsymbol,"$",ftrs_num[1])))
              n_NA <- DSI::datashield.aggregate(conns=cur_conn, call("numNaDS", paste0(tabsymbol,"$",ftrs_num[1])))
              for (i in names(lengths)) lengths[[i]] <- lengths[[i]] - n_NA[[i]]
            }
          }
          lengths$combined <- sum(unlist(lengths),na.rm=T)
          bins <- sapply(lengths$combined, function(x) ceiling(log2(x) + 1) )[1]
          # update the sliders
          updateSliderInput(session, "bins", value =bins)
          updateSliderInput(session, "ptLimit", min = min(10, lengths$combined), max = min(5000, lengths$combined))
          # update globals
          globals$bins <<- bins #"Sturges"
        }

        # general case of barplots
        globals$selectedTable <<- tab
        globals$selectedFtrs <<- ftrs_all
      } else {
        # plot related inputs did not change..
        warning("check this case for scatterplots")
        # save chosen scatterplot point limit to globals
        if (!is.null(input$ptLimit)) globals$ptLimit <<- input$ptLimit
      }

      # show in console that the dashboard is ready
      message("------ Dashboard is ready... ------")
    }
  })

  # event: input slider changed
  observeEvent(input$bins, {
    message("Bin slider updated -> update globals$bins")
    isolate({ globals$bins <- input$bins })
  })

  # event: logout button pressed (logs out of active DataSHIELD session, local or global)
  observeEvent(input$logoutClc, {
    DSI::datashield.logout(globals$conns)
    USER$login <<- F
    showNotification("You logged out... Session will be restarted", duration = globals$notification_duration)
    session$reload()
  })

  # event: selection under "Select groups" changed
  observeEvent(input$inpGroupSelectXY, {
    # in DataSHIELD/DSLite mode: event will trigger creation of table subsets
    req(globals$conns)
    req(input$inpTableSelect)

    # save input selections in variables
    tab <- input$inpTableSelect
    selectedGrps <- input$inpGroupSelectXY
    # check if aggregations for selectedGrps have been already computed and saved
    req((is.null(globals$summaryDataCollection[[tab]]) ||
           is.null(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(selectedGrps)]]) ||
           length(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(selectedGrps)]]) > 0   ))

    # if not, then create table subsets
    message("Promise to compute the aggregation tables.")
    showNotification("Tables for group selection will be prepared...", duration = globals$notification_duration)

    req(globals$creds)
    creds <- globals$creds

    # create subset tables in a background process
    promises::future_promise({
      tryCatch({
        message("------ in promise -----")
        library(DSOpal)
        library(dsBaseClient)
        library(dsDashboard)

        # LOGIN to Opal-SERVER
        builder <- DSI::newDSLoginBuilder()
        builder$append(server=creds["server"],
                       url=creds["url"],
                       token= creds["token"],
                       profile=creds["profile"])
        logindata <<- builder$build()

        # deactivate the signature check...
        httr::set_config(httr::config(ssl_verifypeer = 0L))

        # login
        conn <- DSI::datashield.login(logins = logindata, restore = workspace_name)

        # assign all tables
        tmpAllTables <- dsDashboard::assignAllTables(conn)
        message("in promise: tables assigned")

        tryCatch({
          dsGapply(tab, selectedGrps, (function(x, datasources) return(data.frame(variable=x))), datasources=conn, lazy=T)
        },
        error = function(cond) {
          warning(paste("dsGapply failed:",cond, datashield.errors()) )
        })

        message("in promise: logout")
        DSI::datashield.logout(conn, save=workspace_name)
      },
      error = function(cond) {
        warning(paste("fail in promise after selecting a group variable", cond, datashield.errors()) )
      })
    }, seed=NULL)
  }, ignoreInit = F)

  # output: rendering of the logout button with "right-from-bracket" icon
  output$logoutbtn <- renderUI({
    req(USER$login)
    actionButton("logoutClc", tagList(icon("right-from-bracket"), "Logout"), style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })

  # output: buttons in the "Tables" tab
  output$tableTools <- renderUI({
    req(USER$login)
    box(width = 12, title="", #"Pool or Summarize tables",
        # button: "Pool Tables" (to pool the selected tables, if possible)
        actionButton("poolTabs", tagList(icon("link"), "Pool tables"), style = "color: white; background-color:#3c8dbc;
                                         padding: 10px 15px; width: 200px; cursor: pointer;
                                         font-size: 18px; font-weight: 600;")
    )
  })

  # Output: create Sankey plot/alluvial plot
  output$sankeyPlot <- ggiraph::renderGirafe({
    # continue only if login is completed
    req(USER$login)
    # continue only if groups are selected
    req(ifelse(globals$safemode,length(globals$selectedGrpsTDVec)>0, length(globals$selectedGrps)>0))

    # get selected groups (in simplified "safemode" the selection is limited to cohort variables which are available at least in two waves)
    group_vars <- if (input$safemode) sort(globals$selectedGrpsTDVec) else globals$selectedGrps
    message(paste("alluvial plot for:", paste0(group_vars, collapse=", ")))
    # number of selected group variable needs to be larger than 0
    req(length(group_vars)>0)
    # if globals$use_names is JSON, decode it ; save it in USER$useLabels
    if ((!is.null((globals$use_names))) && is.character(globals$use_names) && jsonlite::validate(globals$use_names)) {
      USER$useLabels <<- !jsonlite::fromJSON(globals$use_names)
    } else {
      USER$useLabels <<- !globals$use_names
    }

    # optional notification
    if (globals$extra_notifications) showNotification("Alluvial plot will be generated...", duration = globals$notification_duration)
    # get table, its id, name  (both for local and DataSHIELD mode)
    tab <- globals$selectedTable
    # length of selected tables must be larger than 0
    req(length(tab)>0)
    tabnr <- readr::parse_number(tab)
    tabname <- globals$allTables[tabnr,".x"]

    if (is.null(globals$conns)) {
      # get the table content
      theTab <- as.data.frame(USER$data[[tabname]], check.names = FALSE)

      # group selection must match the table columns
      req(all(group_vars %in% colnames(theTab)))

      # metadata (for alluvial plot)
      isolate({var_metadata <- globals$metaDataCollection[[tab]]})
      var_groups_spec <- var_metadata[group_vars,][["spec"]]
      var_groups_spec <- lapply(var_groups_spec, function(x) if (is.null(x)) NULL else jsonlite::fromJSON(x))

      # get label and wave/stage and alias and description and levels
      tryCatch({
        var_alias <- lapply(var_groups_spec, function(x) x %>% .[["alias"]] ) %>% unlist()
        var_label <- lapply(var_groups_spec, function(x) x %>% .[["label"]] ) %>% unlist()
        var_T     <- lapply(var_groups_spec, function(x) x %>% .[["time_id"]] ) %>% unlist()
        var_descriptions <- lapply(var_groups_spec, function(x) x %>% .[["desc"]] ) %>% unlist()
        levelList <- lapply(var_groups_spec, function(x) x %>% .[["levels"]] %>% jsonlite::fromJSON() )
      },
      error = function(cond) {
        warning(paste("label and wave could not be read from spec attribute:", conditionMessage(cond)))
        warning(paste("DataSHIELD errors:", datashield.errors()))
      })
      # alternative if spec attribute is not used
      if (!exists("var_alias") || is.null(var_alias)) var_alias <- Hmisc::label(theTab)[group_vars]
      if (!exists("var_label") || is.null(var_label)) var_label <- gsub("(.*)(\\sat T\\d$)","\\1",var_alias)
      if (!exists("var_T") || is.null(var_T)) var_T <- gsub("(.*at )(T\\d$)","\\2",var_alias)
      if (!exists("var_descriptions") || is.null(var_descriptions)) var_descriptions <- var_alias
      if (!exists("levelList") || is.null(levelList)) levelList <- lapply(theTab[group_vars], function(x) levels(x) )

      # set levels in table theTab
      for (gvar in group_vars) {
        levels(theTab[[gvar]]) <- unlist(levelList[[gvar]])
      }

      # prepare plot
      reshapeddf <- as.data.frame(theTab) %>%
        dplyr::group_by( dplyr::pick( dplyr::all_of(c(group_vars)) ) ) %>% dplyr::count( ) %>% dplyr::ungroup() %>% dplyr::mutate(across(where(is.factor),as.character)) %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        tidyr::  pivot_longer(
          cols = -c(n,id),
          names_to = "stage",
          values_to = "response",
          values_drop_na = TRUE
        ) %>% dplyr::mutate(stage=factor(stage, levels=group_vars))

      # reshapedff has the response factors saved as characters, i.e. unordered factors!
      # get some order into the factor levels (this is not optimal if at different stages share share some but not all factor levels)
      reshapeddf$response <- factor(reshapeddf$response, levels=unique(unlist(lapply(group_vars, function(x) if (is.ordered(theTab[[x]])) sort(levels(theTab[[x]])) else levels(theTab[[x]]) ))))

      # if all stages are of the same variable (but different time points) then
      # we have a unique titlename
      titlename <- unique(gsub("\\sat T\\d$" , "", var_label[unique(as.character(reshapeddf$stage))]))
      # is there only one variable (from different waves) or are different variables combined?
      variables_unique <- length(titlename)==1
      # join possible vector into a single title containing all variables
      titlename <- paste(paste(titlename, collapse=", " ) , "(full case analysis)")

      # get number of individuals
      n_of_rows <- sum(reshapeddf$n)

      # if the x-labels are multiline labels, we need to handle the tooltips and data_ids in a special way:
      xlabel_rlen <-  sapply(var_label, function(x) stringr::str_count( stringr::str_wrap(x,20) , "\n")+1 ) #var_label[group_vars]
      xids <- rep(names(xlabel_rlen), times=xlabel_rlen)
      xlabels <- var_alias[xids] # instead of var_label[xids]
      xdescriptionunique <- paste(unique(var_descriptions[xids]),collapse="\n----\n")
    } else {
      # make sure we login again if connection is lost (local and global login)
      if (do_global_login) tryCatch( {DSI::dsListWorkspaces(conns[[1]])}, error=function(e) {conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(conns)})
      tryCatch( {DSI::dsListWorkspaces(globals$conns[[1]])}, error=function(e) {globals$conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(globals$conns)})
      # development note: if connection is lost and tables have been pooled in the dashboard, the pooled tables have to be created again

      tabsymbol <- globals$allTables[tabnr,"symbol"]
      cur_conn <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables

      # set path for alluvial data
      mode_str <- ifelse(USER$login_mode==0,"",paste0("mode",USER$login_mode))
      alluvial_path <- if (identical(conns,globals$conns)) alluvial_path_global else file.path(cache_path,paste0(alluvial_prefix,"_",tabname,"_",mode_str,".rds"))

      # create entry for the current table and variable selection globals$alluvialDataCollection (if not existing)
      if (is.null(globals$alluvialDataCollection)) {
        globals$alluvialDataCollection <- setNames(list(setNames(list(NULL), "empty")), tab) #jsonlite::toJSON(group_vars))), tabname)
      }

      # load data
      if (file.exists(alluvial_path)){
        globals$alluvialDataCollection[[tab]] <<- readRDS(alluvial_path)
      }

      # are no metadata available for the table?
      warning("check metadata production for alluvials selection")
      if (is.null(globals$metaDataCollection[[tab]]) ||                                          # no metadata
          nrow(globals$metaDataCollection[[tab]]) == 0 ||                                        # empty metadata - check is necessary despite of followin line
          nrow(globals$metaDataCollection[[tab]][group_vars,]) != length(unique(group_vars))) {  # OR: not all the requested metadata
        # get metadata from DataSHIELD and save it in globals
        message("get metadata from DataSHIELD for alluvial plot")
        vars_4_meta <- sapply(group_vars, function(x) paste0(tabsymbol,"$",x)) # sapply, because for empty group_vars nothing can be done...
        var_metadata <- dsDashboard::ds.meta2(vars_4_meta, cur_conn)
        globals$metaDataCollection[[tab]] <<- var_metadata
      } else {
        # copy metadata from globals to var_metadata
        message("get metadata for alluvial plot from globals")
        var_metadata <- as.data.frame(globals$metaDataCollection[[tab]])
      }

      # read metadata from spec attribute
      var_desc <- sapply(as.matrix(var_metadata)[,"spec",drop=F],function(x) jsonlite::fromJSON(x)$desc) %>% setNames(rownames(var_metadata)) %>% .[group_vars]
      var_legacy_label <- unlist(as.matrix(var_metadata)[,"label",drop=F]) %>% setNames(rownames(var_metadata)) %>% .[group_vars]
      var_label <- sapply(as.matrix(var_metadata)[,"spec",drop=F],function(x) jsonlite::fromJSON(x)$label) %>% setNames(rownames(var_metadata)) %>% .[group_vars]
      #var_alias <- sapply(as.matrix(var_metadata)[,"spec",drop=F],function(x) jsonlite::fromJSON(x)$alias) %>% setNames(rownames(var_metadata)) %>% .[group_vars]
      var_levs_JSON <- sapply(as.matrix(var_metadata)[,"spec",drop=F],function(x) jsonlite::fromJSON(x)$levels) %>% setNames(rownames(var_metadata)) %>% .[group_vars]

      # check: alluvial plots are only for categorical variables
      all_vars <- dsDashboard::dsIsNumeric(tabsymbol, group_vars, datasources=cur_conn)
      if (sum(all_vars[group_vars])>0) warning("variables should be categories, but there is a numerical variable chosen... numerical variables will be dropped")
      # filter out numerical variables and save to cat_group_vars
      cat_group_vars <- group_vars[all_vars[group_vars]==FALSE]

      # check if data for alluvial plot data are already available
      if (is.null(globals$alluvialDataCollection[[tab]][[jsonlite::toJSON(cat_group_vars)]])) {
        # alluvial data not yet available -> have to compute the data
        message(paste("data for",jsonlite::toJSON(cat_group_vars),"alluvial plot not available => will be generated"))

        # determine DataSHIELD object names for the requested variables
        var_obnames <- if (is.null(cat_group_vars)) NULL else paste0(tabsymbol,"$",cat_group_vars)

        # we need all possible combinations of the categorical variables
        all_levels <- lapply(var_obnames,
                             function(x) {
                               # Ndistinct
                               levels <- NULL
                               # get levels from all connected servers
                               lapply(DSI::datashield.aggregate(cur_conn, as.symbol(paste0('levelsDS(', x, ')' ))),
                                      function(x) levels <<- union(levels,x$Levels) )
                               return(levels)
                             }
        ) %>% setNames(cat_group_vars)

        # use descriptive level names instead of integers
        all_levels <- lapply(names(all_levels), function(x) unlist(jsonlite::fromJSON(var_levs_JSON[paste0(x)]))) %>% setNames(names(all_levels))
        # get all possible combinations of the categorical variables
        all_combinations <- expand.grid(all_levels)
        all_level_len <- lapply(all_levels, function(x) length(x))

        if (input$debugmode) browser()

        # build up the arithmetic expression to get the corresponding id (corresponding to a factor level combination as in all_combinations) for each observation
        make_call <- paste0(c("1",
                              sprintf("((as.numeric(%s) - 1)*%s)",  # space around "-" obligatory! AND DataSHIELD seems to need this parenthesis around "(as.numeric(%s) - 1)*%s"
                                      var_obnames,
                                      c(1,head(cumprod(all_level_len),-1)))
        ), collapse=" + ")
        # caution: as.numeric for this variable in DataSHIELD does not return
        # levels starting with 1 (1,2,3,4,5) but starting with 0 (0,1,2,3,4)!
        # to see this check out: make_call <- paste0(c("1", sprintf("as.numeric(%s)*%s", var_obnames, c(1,head(cumprod(all_level_len),-1))) ), collapse=" + ")
        ds.make(toAssign=make_call, newobj = "alluvial_data", datasources = cur_conn)

        # try to read the results
        tryCatch({
          # factor levels are numeric levels as defined above
          f_range <- DSI::datashield.aggregate(cur_conn, call("asFactorDS1", "alluvial_data")) %>% unlist() %>% as.vector() %>% as.numeric() %>% range()
        },
        # fails, if the DataSHIELD options are too strict, e.g.dsBase::asFactorDS1 fails with:
        # "this variable has too many levels and may be disclosive.
        # It exceeds the max number of levels allowed by nfilter.levels.max: that is 40.
        # In this study this variable has 84 factor levels"
        error = function(cond) {
          warning(conditionMessage(cond))
          warning(datashield.errors())
          if (input$debugmode)
            showNotification(datashield.errors(), duration=globals$notification_duration, type="error")
          else
            showNotification("'Restricted output due to privacy parameters", duration=globals$notification_duration, type="warning")
        })
        # fallback if f_range could not be determined above
        if (!exists("f_range")) f_range <- c(1,1+sum((unlist(all_level_len)-1)*c(1,head(cumprod(all_level_len),-1))))

        tryCatch({
          histobj <- dsDashboard::ggHistDS("alluvial_data", plot=F, datasources = cur_conn, bins=diff(f_range)+1, range=f_range+c(-0.5,0.5))
          alluvial_frequencies <- setNames(histobj$combined$histobject$counts, seq(f_range[1],f_range[2]))
        },
        # fails, if the DataSHIELD options are too strict, e.g. dsBase::asFactorDS1 fails with:
        # "this variable has too many levels and may be disclosive.
        # It exceeds the max number of levels allowed by nfilter.levels.max: that is 40.
        # In this study this variable has 84 factor levels"
        error = function(cond) {
          message(conditionMessage(cond))
          message(datashield.errors())
          if (input$debugmode)
            showNotification(datashield.errors(), duration=globals$notification_duration, type="error")
          else
            showNotification("Restricted output due to privacy parameters", duration=globals$notification_duration, type="warning")
        })

        # replace NA values with zero - strata with missing counts are not displayed in the plot
        for (i in 1:dim(all_combinations)[1]) {
          if (is.na(alluvial_frequencies[paste(i)]))
            alluvial_frequencies[paste(i)] <- 0
        }

        all_combinations$n <- alluvial_frequencies[paste(1:dim(all_combinations)[1])]
        all_combinations$id <- names(alluvial_frequencies[paste(1:dim(all_combinations)[1])])

        # reshape from wide to long table
        reshapeddf <- all_combinations %>%
          tidyr::pivot_longer(cat_group_vars,names_to = "stage", values_to = "response")  %>%
          dplyr::mutate(stage=factor(stage, levels=cat_group_vars))  # factor property is used for stratum overlay text

        # save the data as JSON string in globals
        globals$alluvialDataCollection[[tab]][[jsonlite::toJSON(cat_group_vars)]] <- jsonlite::toJSON(reshapeddf)

        # save the data in alluvial data file
        message(paste("save to",alluvial_path))
        saveRDS(globals$alluvialDataCollection[[tab]], file=alluvial_path)

      } else {
        message(paste("data for",jsonlite::toJSON(cat_group_vars),"alluvial plot already available"))

        # decode JSON data
        reshapeddf <- jsonlite::fromJSON(globals$alluvialDataCollection[[tab]][[jsonlite::toJSON(cat_group_vars)]])

        # levels in reshapeddf$response are characters
        # -> turn it to a factor with the levels in a reasonable order
        foo <- lapply(var_levs_JSON, jsonlite::fromJSON)
        all_unique_levels <- lapply(names(foo), function(x) paste0(x,"->",as.character(foo[[x]]))) %>% unlist()
        all_nonunique_labels <- lapply(names(foo), function(x) paste0(as.character(foo[[x]]))) %>% unlist()
        ## add whitespace to duplicated label to make it unique?
        #while (sum(duplicated(all_nonunique_labels))>0) {all_nonunique_labels[duplicated(all_nonunique_labels)] <- all_nonunique_labels[duplicated(all_nonunique_labels)] + " "}
        reshapeddf$response <- factor(paste0(reshapeddf$stage,"->",reshapeddf$response), levels = all_unique_levels, labels = all_nonunique_labels)
      }

      titlename <- paste(paste(unique(var_label), collapse=", " ) , "(full case analysis)")
      # is there only one variable (from different waves) or are different variables combined?
      variables_unique <- length(unique(var_label))==1

      # number of observations over all stages and levels
      n_of_rows <- sum(reshapeddf$n)

      # extract some metadata
      var_alias <- lapply(var_metadata[cat_group_vars,"spec"],function(x) jsonlite::fromJSON(x)$alias) %>% unlist() %>% setNames(cat_group_vars)
      var_descriptions <- lapply(var_metadata[cat_group_vars,"spec"],function(x) jsonlite::fromJSON(x)$desc) %>% unlist() %>% setNames(cat_group_vars)
      var_T <- gsub("(.*at )(T\\d$)","\\2",var_alias)
      # if the x-labels are multiline labels, we need to handle the tooltips and data_ids in a special way:
      xlabel_rlen <-  sapply(var_label[cat_group_vars], function(x) stringr::str_count( stringr::str_wrap(x,20) , "\n")+1 )
      xids <- rep(names(xlabel_rlen), times=xlabel_rlen)
      xlabels <- paste(var_label[xids],"at", var_T[xids])
      xdescriptionunique <- paste(unique(var_descriptions[xids]),collapse="\n----\n")  # description for title overlay

      # to plot the stages in the right order, turn reshapeddf$stage into a factor with factor levels in the desired order
      reshapeddf$stage <- factor(reshapeddf$stage, levels=cat_group_vars)
    }

    if (input$debugmode) browser()

    # interactive alluvial plot (local case and DataSHIELD case) using ggplot2 and ggiraph
    pl <- ggplot(reshapeddf,
                 aes(x = stage, stratum = response, alluvium = id,
                     y = n,
                     fill = response, label = response)) +
      ggplot2::scale_x_discrete(expand = c(.1, .1)) +
      geom_flow_interactive(
        aes(
          tooltip = after_stat( {
            paste0(.data$stratum, " &#x279C; ", stratum_to(.data), "<br>",
                   .data$count, " observations<br>",
                   round(100*.data$count/sapply(.data$deposit, function(x) sum(.data$count[x==.data$deposit & .data$flow=="from"]) ), 1),
                   "&#37; of group<br>",
                   round(100*.data$count/n_of_rows, 1),
                   "&#37; of total") } ), # &#37 is arrow character ➜
          #data_id = after_stat( { paste0("flow:",.data$stratum, "-to-", stratum_to(.data)) } ),
          `flow-id` = after_stat( { paste0("flow:",.data$stratum, "-to-", stratum_to(.data)) } ),
          #`data-id` = after_stat( { paste0("stratum:",.data$stratum) } ),
          `key-id` = after_stat( { paste0("stratum:",.data$stratum) } ),
          onmouseenter = after_stat( {paste0("d3.selectAll(`[key-id=\"",paste0("flow:",.data$stratum, "-to-", stratum_to(.data)),"\"]`).raise(); d3.selectAll(`",paste0("text"),"`).raise(); ") } ),
        ),
        extra_interactive_params = c("onmouseenter","flow-id","key-id") #"data-id",
      ) +
      geom_stratum_interactive(
        alpha = .5,
        aes(
          tooltip = after_stat( { paste0(if (USER$useLabels) stringr::str_wrap(var_label[levels(as.factor(reshapeddf$stage))[x]],20) else levels(as.factor(reshapeddf$stage))[x],
                                         ": <br>Value: ",.data$stratum,"<br>",.data$count," observations (",round(100*.data$count/n_of_rows,1),"&#37;)") } ),
          #data_id = after_stat( { paste0("stratum:",.data$stratum) } ),
          `key-id` = after_stat( { paste0("stratum:",.data$stratum) } ),
          #`key-id` = after_stat( { paste0("flow:",.data$stratum,"->") } ),
          `flow-id` = after_stat( { paste0("flow:",.data$stratum,"->") } ),
          onmouseenter = after_stat( {paste0("d3.selectAll(`[data-id=\"",paste0("stratum:",.data$stratum),"\"]`).raise()") } )
        ),
        extra_interactive_params = c("key-id","onmouseenter","flow-id")
      ) +
      ggplot2::scale_x_discrete(labels=function(x) if (USER$useLabels) {if (variables_unique) var_T[x] else stringr::str_wrap(var_label[x],20)} else x ) +
      scale_alpha_discrete(range = c(0, 1)) + # necessary to achieve complete transparency of text when alpha = 0
      ggiraph::geom_text_interactive(
        stat = "stratum",
        aes(
          ##show text only if stratum has at least 2.5% of observations in it
          #alpha = factor(after_stat( { ifelse(100*.data$count/n_of_rows>=2.5, 1, 0.5) } )), # no continuous transparency steps, because factor
          #hide text if not hovered
          alpha = factor(0),
          tooltip = after_stat( { paste0(if (USER$useLabels) stringr::str_wrap(var_label[levels(as.factor(reshapeddf$stage))[x]],20) else levels(as.factor(reshapeddf$stage))[x],
                                         ": <br>Value: ",.data$stratum,"<br>",.data$count," observations (",round(100*.data$count/n_of_rows,1),"&#37;)") } ), # same as in geom_stratum_interactive,
          #`key-id` = after_stat( { paste0("flow:",.data$stratum,"->") } )
          #data_id = after_stat( { paste0("stratum:",.data$stratum) } ),
          `flow-id` = after_stat( { paste0("flow:",.data$stratum,"->") } ),
          `key-id` = after_stat( { paste0("stratum:",.data$stratum) } )

        ),
        extra_interactive_params = c("key-id","flow-id")
      ) +
      #ggplot2::theme_grey(base_family = "Roboto Condensed") +
      ggplot2::theme(legend.position = "none",
                     aspect.ratio = 2/(1+sqrt(5)),
                     axis.text.x= ggiraph::element_text_interactive(
                       angle = if (input$safemode) 0 else -90, hjust = 0, vjust=0.5,
                       data_id = if (variables_unique) paste0("axis.title.x.ticks",var_T) else if (USER$useLabels) paste0("axis.title.x.ticks",xids) else paste0("axis.title.x.ticks",group_vars),
                       tooltip = if (variables_unique && USER$useLabels)
                         paste(var_alias, "\n\n",var_descriptions)
                       else if (variables_unique && !USER$useLabels)
                         group_vars
                       else if (!variables_unique && USER$useLabels)
                         xlabels
                       else
                         xids ##if (input$safemode) group_vars
                     ),
                     title = element_text_interactive(
                       data_id = "title",
                       tooltip = xdescriptionunique
                     ),
                     axis.title.x = element_text_interactive(
                       data_id = "axis.title.x",
                       tooltip = "Temporal or logical stages"
                     ),
                     axis.title.y = element_text_interactive(
                       data_id = "axis.title.y",
                       tooltip = "Number of observations")
      ) +
      ylab("Frequency") + xlab("Stage") + ggplot2::theme(legend.position="top",
                                                         legend.justification='left',
                                                         legend.direction='horizontal') +
      ggplot2::guides(alpha = FALSE) +
      ggiraph::scale_fill_brewer_interactive( # legend filled box
        name = NULL,
        data_id = function(breaks) { paste0("stratum:",as.character(breaks))}, #key-id - seems to be the only supported id by scale_fill_brewer_interactive!
        #tooltip = function(breaks) { as.character(breaks)},
        #extra_interactive_params = c("data-id","theme-id"), # unfunctional in scale_fill_brewer_interactive
        palette="Set1",
        guide = guide_legend_interactive(
          #title=NULL,
          override.aes=list(shape=NA), # make unwanted points in legend completely invisible
          title.theme = element_text_interactive(),
          label.theme = element_text_interactive() ),
        labels = function(breaks) {
          lapply(breaks, function(br) {
            label_interactive(
              as.character(br),
              `key-id` = paste0("stratum:",as.character(br)), #key-id
              extra_interactive_params = c("key-id"),
            )
          })
        }
      )

    # if all stages are of the same variable (but different time points) then
    # put  name in plot title
    if (length(titlename)==1) pl <- pl + ggtitle(titlename)

    gpl <- ggiraph::girafe(ggobj = pl, font_set = plot_font_set, #fonts = list(sans = c("Roboto Condensed")), # specifying this, gives misaligned fonts when using the dashboard locally
                           width_svg=1.2*5*(1+sqrt(5))/2, height_svg=1.2*(5))

    gpl <- girafe_options(gpl,
                          opts_sizing(rescale = TRUE, width = 1),
                          opts_zoom(min = .5, max = 4),
                          opts_toolbar(hidden=c("lasso_select", "lasso_deselect","saveaspng")),
                          opts_hover_key(css = girafe_css(
                            css = "fill-opacity:1;",
                            text = "fill:black;font-weight: bold;" # legend text color when hovering legend (not bars)
                          )),
                          opts_hover(css = girafe_css(
                            css = "fill-opacity:1;",
                            area = "fill-opacity:1;",
                            text= "fill:black;font-weight: bold;" # text color when hovering bar (not legend)
                          )),
                          opts_hover_theme(css = girafe_css( # when hovering legend text
                            css = "",
                            text = "fill:black;font-weight: bold;"
                          ))
    )

    gpl <- girafe_options(gpl,
                          opts_selection(type = "none", only_shiny = FALSE),
                          opts_selection_key(type = "none", only_shiny = FALSE  ),
                          opts_selection_theme(type = "none", only_shiny = FALSE  ),
                          opts_hover_inv(css = girafe_css(css = "fill-opacity:0.3;stroke:grey;",
                                                          text = "fill:grey;stroke-width:0px;",
                                                          area = "fill:grey;stroke-width:0px;"))
    )

    # remember the svg id
    globals$svgid_alluvial <- gpl$x$uid

    return(gpl)

  })

  # returns the names of the numeric variables => necessary so that conditional panel changes its settings for numeric vs. non-numeric data (histogram vs. barplot)
  output$featureIsNumeric <- reactive({
    globals$selectedFtrs %in% names(globals$tabVarsNumeric)[unlist(globals$tabVarsNumeric)] # unlist, because globals$tabVarsNumeric can be an empty list
  })

  # featureIsNumeric must be computed, although it is a hidden element
  outputOptions(output, 'featureIsNumeric', suspendWhenHidden = FALSE)

  # widget for the distributional plot (histogram or barplot)
  output$distPlot <- ggiraph::renderGirafe({
    # must be logged in
    req(USER$login)
    # input fields for histogram must be available
    req(input$hist_yax)
    # no inconsistent group style must be selected (only one group style can be "facets")
    req(USER$hist_grp1_last != USER$hist_grp2_last)
    # make sure the information about which variables are numeric is available
    req(length(globals$tabVarsNumeric)>0)
    # table must be selected and one (and only one) feature variable (two grouping variables are allowed)
    if( (!is.null(globals$selectedTable) && !(length(globals$selectedTable)==0) ) &    # check for table
        (!is.null(globals$selectedFtrs) && !(length(globals$selectedFtrs)==0) && (!(length(globals$selectedFtrs)>=2)) ) # check for selected variable
    ) {
      # write message to console
      message("create distributional plot")

      # counts or relative frequency? (for y-axis)
      ycounts <- (input$hist_yax == "counts")

      # number of bins: default is to compute bin number with Sturges method
      bins <- "Sturges"
      if (!is.null(globals$bins)) bins <- globals$bins

      # remember selected variable in var_selected
      var_selected <- globals$selectedFtrs

      if (input$debugmode) browser()

      # react to changes in option selection
      USER$hist_grp1_new
      USER$hist_grp2_new

      # handle grouped histograms/boxplots
      grp1 <- if (length(globals$selectedGrps)>=1) globals$selectedGrps[1] else NULL
      grp2 <- if (length(globals$selectedGrps)>=2) globals$selectedGrps[2] else NULL
      # if "facets" is chosen for first group, then switch group1 and group2
      if (USER$hist_grp1_new=="facets") {
        grp1_type <- USER$hist_grp2_new
        grp2_type <- USER$hist_grp1_new
        tmp <- grp1
        grp1 <- grp2
        grp2 <- tmp
        rm(tmp)
      } else {
        grp1_type <- USER$hist_grp1_new
        grp2_type <- USER$hist_grp2_new
      }

      # check if we are in local mode
      if (is.null(globals$conns)) {

        # name of currently selected table
        tab <- input$inpTableSelect
        # get "number" and "name" of the current table
        tabnr <- readr::parse_number(tab)
        tabname <- globals$allTables[tabnr,".x"]

        # local mode: save local table in "theTab"
        theTab <- as.data.frame(USER$data[[tabname]], check.names = FALSE)

        # column selection must be valid
        req(all(var_selected %in% colnames(theTab)))

        # for numeric variable get histogram range
        if (is.numeric(theTab[,var_selected])) {
          h_min <- min(theTab[,var_selected], na.rm=T)
          h_max <- max(theTab[,var_selected], na.rm=T)
        }

        if (input$debugmode) browser()

        # create histogram object
        if (is.numeric(theTab[,var_selected])) {
          hist_obj <- hist(theTab[,var_selected], plot=F, breaks=seq(from=h_min, to=h_max, by=(h_max-h_min)/bins))
        } else {
          tmp_tab <- table(theTab[,var_selected])
          hist_obj <- data.frame(
            "counts" = as.numeric(tmp_tab),
            "density" = as.numeric(tmp_tab/sum(tmp_tab)),
            "mids" = factor(names(tmp_tab), levels = levels(theTab[,var_selected]))
          )
        }

        # general metadata (for grouped and ungrouped histogram)
        isolate({var_metadata <- globals$metaDataCollection[[tab]]})
        var_x_spec <- var_metadata[var_selected,][["spec"]] %>% unlist()
        var_x_spec <- if (is.null(var_x_spec)) NULL else jsonlite::fromJSON(var_x_spec)

        # the label
        # standard names from label
        x_label <- var_metadata[var_selected,][["label"]] %>% unlist()
        if (!is.null(var_x_spec)) x_label <- var_x_spec$label
        # description of feature,if available
        xdesc <-  var_x_spec$desc
        labels4ticks <- NULL
        if (!is.null(var_x_spec$levels)) labels4ticks <- var_x_spec$levels %>% jsonlite::fromJSON() %>% unlist()
        # get metadata specific for grouped histogram
        var_grp1_spec <- if (is.null(grp1)) NULL else var_metadata[grp1,][["spec"]] %>% unlist() %>% (function(x) {if (is.null(x)) NULL else jsonlite::fromJSON(x)})()
        var_grp2_spec <- if (is.null(grp2)) NULL else var_metadata[grp2,][["spec"]] %>% unlist() %>% (function(x) {if (is.null(x)) NULL else jsonlite::fromJSON(x)})()
        grp1desc <- var_grp1_spec$desc
        grp2desc <- var_grp2_spec$desc
        # standard names for groups from label
        grp1label <- var_metadata[grp1,][["label"]] %>% unlist()
        grp2label <- var_metadata[grp2,][["label"]] %>% unlist()
        # if spec attribute available, update labels
        if (!is.null(var_grp1_spec)) grp1label <- var_grp1_spec$label
        if (!is.null(var_grp2_spec)) grp2label <- var_grp2_spec$label
        levs1 <- if (is.null(var_grp1_spec$levels)) NULL else var_grp1_spec$levels %>%jsonlite::fromJSON() %>% unlist()
        levs2 <- if (is.null(var_grp2_spec$levels)) NULL else var_grp2_spec$levels %>%jsonlite::fromJSON() %>% unlist()
        if (!is.null(levs1)) levels(theTab[,grp1]) <- levs1
        if (!is.null(levs2)) levels(theTab[,grp2]) <- levs2

        # check if groups are selected
        if (!is.null(grp1) || !is.null(grp2)) {
          # facets are never for group 1 (otherwise group 1 and group 2 are switched before plotting)
          req(grp1_type != "facets")

          # grouped histogram
          cross_levels <- tidyr::crossing(theTab[,grp1],theTab[,grp2])

          # check if only one group, but with facets
          if (USER$hist_grp1_new=="facets" && is.null(grp1)) {
            # in that case only one column with group 2
            colnames(cross_levels) <- "levs2"
          } else {
            colnames(cross_levels) <- c("levs1","levs2")
          }

          # get histograms for all groups/group combinations
          if (is.numeric(theTab[,var_selected])) {
            hist_data_groups <- apply(cross_levels, 1,
                                      function(crosslev) {
                                        filter1 <- if (is.null(grp1)) TRUE else theTab[,grp1]==crosslev["levs1"]
                                        filter2 <- if (is.null(grp2)) TRUE else theTab[,grp2]==crosslev["levs2"]
                                        if (ycounts)
                                          hist(theTab[filter1 & filter2,][,var_selected], plot=F, breaks=seq(from=h_min, to=h_max, by=(h_max-h_min)/bins))$counts
                                        else
                                          hist(theTab[filter1 & filter2,][,var_selected], plot=F, breaks=seq(from=h_min, to=h_max, by=(h_max-h_min)/bins))$density
                                      })
            colnames(hist_data_groups) <- 1:dim(hist_data_groups)[2]
          } else {
            hist_data_groups <- apply(cross_levels, 1,
                                      function(crosslev) {
                                        filter1 <- if (is.null(grp1)) TRUE else theTab[,grp1]==crosslev["levs1"]
                                        filter2 <- if (is.null(grp2)) TRUE else theTab[,grp2]==crosslev["levs2"]
                                        tmp_tab <- table(theTab[filter1 & filter2,][,var_selected])
                                        if (ycounts)
                                          (counts <- as.numeric(tmp_tab) )
                                        else
                                          (density <- as.numeric(tmp_tab/sum(tmp_tab)))
                                      })

            colnames(hist_data_groups) <- 1:dim(hist_data_groups)[2]
          }

          # build histogram table
          hist_df <- cbind(data.frame(mids = hist_obj$mids), hist_data_groups)
          hist_df <- reshape2::melt(hist_df, measure.vars = colnames(hist_data_groups), rm.na = TRUE, variable.name="grpid")
          # add column "levs1" with levels 1,2,...,<NA> and "levs2"
          hist_df <- cbind(hist_df, cross_levels[  as.numeric(levels(hist_df$grpid))[hist_df$grpid]    ,,drop=F])

          # the plot data: use the same name for counts as in DataSHIELD case and add density
          the_data <- hist_df %>% dplyr:::mutate(counts=value) %>% dplyr::group_by(grpid) %>% dplyr::mutate(density = value/sum(value)) %>% dplyr::select(-value)

        } else {
          # else: the plot data for the ungrouped histogram
          the_data <- data.frame(counts= hist_obj$counts, density=hist_obj$density ,mids = hist_obj$mids)
        }
      } else {
        # DataSHIELD mode

        # make sure we login to DataSHIELD if connection is lost (global and local connection)
        if (do_global_login) tryCatch( {DSI::dsListWorkspaces(conns[[1]])}, error=function(e) {conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(conns)})
        tryCatch( {DSI::dsListWorkspaces(globals$conns[[1]])}, error=function(e) {globals$conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(globals$conns)})
        # development note: if connection is lost and tables have been pooled in the dashboard, the pooled tables have to be created again

        # currently selected table
        tab <- input$inpTableSelect

        # get "number" of the current table
        tabnr <- readr::parse_number(tab)

        # get tab symbol
        tabsymbol <- globals$allTables[tabnr,"symbol"]

        # get current connection
        cur_conn <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables

        # get names of all variables in tab
        selVars <- globals$tabVars

        # get variable names and DataSHIELD object names for the selected variables
        variable_names <- c(var_selected, grp1, grp2)
        variable_objnames <- paste0(tab,"$",variable_names)

        # get metadata
        if (is.null(globals$metaDataCollection[[tab]]) ||                # no metadata for requested table
            !all(c(var_selected,grp1,grp2) %in% rownames(globals$metaDataCollection[[tab]]))  # or at least metadata not for all feature and group variables
        ) {
          # get decoded spec attribute from metadata in DataSHIELD
          cur_metadata <- dsDashboard::ds.meta2(variable_objnames, cur_conn)
        } else {
          # get metadata for selected variables from globals
          cur_metadata <- globals$metaDataCollection[[tab]]
          cur_metadata <- cur_metadata[rownames(cur_metadata) %in% variable_names,, drop=F]
        }

        # decode json string in spec to extract descriptions, labels, ...
        spec_metadata <- lapply(cur_metadata[["spec"]], function(x) jsonlite::fromJSON(x)) %>% setNames(rownames(cur_metadata))

        # extract descriptions and labels from metadata
        var_x_desc <- lapply(spec_metadata, function(x) x$desc) %>% unlist()
        var_label <- lapply(spec_metadata, function(x) x$alias) %>% unlist()
        # get variable descriptions (for popover)
        grp1desc <- var_x_desc[grp1];  grp1label <- var_label[grp1]
        grp2desc <- var_x_desc[grp2];  grp2label <- var_label[grp2]
        xdesc <- var_x_desc[var_selected]; x_label <- var_label[var_selected]
        levs1 <- if (is.null(grp1) || is.null(spec_metadata[[grp1]]$levels)) NULL else unlist(jsonlite::fromJSON(spec_metadata[[grp1]]$levels))
        levs2 <- if (is.null(grp2) || is.null(spec_metadata[[grp2]]$levels)) NULL else unlist(jsonlite::fromJSON(spec_metadata[[grp2]]$levels))

        # check if the selected variable (var_selected) is numeric
        varIsNum <- globals$tabVarsNumeric[var_selected]

        if (!varIsNum) {
          # DataSHIELD mode with categorical data
          message("create barplot for categorical data")

          # if available, read data from saved summaries
          if (!is.null(globals$summaryDataCollection[[tab]]) &&
              !is.null(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]]) &&
              length(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]]) > 0  &&
              jsonlite::fromJSON(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]]) %>% dplyr::filter(variable == var_selected, feature == "levels") %>% dim() %>% .[1] > 0
          ) {
            summ_stats1_tmp <- jsonlite::fromJSON(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]])
            varLabels <- summ_stats1_tmp %>% dplyr::filter(variable == var_selected, feature == "levels") %>%
              dplyr::pull(value)  %>%
              (function(x) x[!is.na(x)])() %>% # drops NAs for subsequent fromJSON
              lapply(jsonlite::fromJSON) %>% unlist() %>%
              (function(x) x[!duplicated(x)])()  # keeps the attributes (unlike "unique")  ### <= this seems to be unnecessary and at least prone to errors (e.g. when two integer categories do have the same name)


            levels_tab <- summ_stats1_tmp %>% dplyr::filter(variable == var_selected, feature %in% seq_along(varLabels)) %>% dplyr::select(-one_of("variable", "tabname")) %>% tidyr::pivot_wider(names_from=feature, values_from=value)
            tmp_tab <- summ_stats1_tmp %>%
              dplyr::filter(variable == var_selected, feature %in% as.character(seq_along(varLabels))) %>%
              dplyr::select(-one_of("variable", "tabname")) %>%
              dplyr::rename(levels=feature) %>%
              dplyr::mutate(value = as.integer(value))
            tmp_tab <- tmp_tab %>%
              dplyr::rename_with(.fn=~paste0(tabsymbol,"$",var_selected), .cols=all_of("levels")) %>%
              dplyr::rename_with(.fn=~if (is.null(grp1)) character(0) else paste0(tabsymbol,"$",grp1), .cols=all_of(grp1)) %>%
              dplyr::rename_with(.fn=~if (is.null(grp2)) character(0) else paste0(tabsymbol,"$",grp2), .cols=all_of(grp2))


            tmp_tab[[paste0(tabsymbol,"$",var_selected)]] <- varLabels[as.numeric(tmp_tab[[paste0(tabsymbol,"$",var_selected)]])]
            # example: Italy, Estonia,... --> country.1, country.2, ...
          } else {
            # requested summary statistics unavailable in globals$summaryDataCollection
            # development hints: possible changes - (1) use function all_summaries(), (2) save summary statistics

            # get (possibly grouped) data for barplot
            tryCatch(
              expr = {
                # dsBaseClient::ds.table does not allow for a group2 if group1 is NULL, therefore switch values in this case
                if (is.null(grp1) && !is.null(grp2)) {
                  tmpgrp1 <- grp2
                  tmpgrp2 <- grp1
                } else {
                  tmpgrp1 <- grp1
                  tmpgrp2 <- grp2
                }
                tmp_obj <- dsBaseClient::ds.table(paste0(tabsymbol,"$",var_selected),
                                                  if (!is.null(tmpgrp1)) paste0(tabsymbol,"$",tmpgrp1) else NULL,
                                                  if (!is.null(tmpgrp2)) paste0(tabsymbol,"$",tmpgrp2) else NULL,
                                                  datasources=cur_conn)
              },
              error = function(cond) {
                tmp_obj <<- list(error.messages = datashield.errors())
              })

            if (!is.null(tmp_obj$error.messages)) {
              # ERROR: data could not be retrieved. Show message that no plot for this selection possible
              # in console
              warning(paste("Datashield prevented this output - reason:", tmp_obj$error.messages))
              # dashboard notification
              showNotification("'Restricted output due to privacy parameters", duration=globals$notification_duration, type="warning")
              # message in plot area
              p <- ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = paste("This cannot be output due to \nlow numbers in at least one cell.\n"),
                         size = 8, color = "red", hjust = 0.5, vjust = 0.5) +
                theme_void() +
                theme(plot.title = element_text(hjust = 0.5), base_family = "Roboto Condensed")
              gpl <- ggiraph::girafe(ggobj = p,
                                     font_set = plot_font_set #fonts = list(sans = c("Roboto Condensed","Roboto")) # specifying this misaligns the font when using the dashboard locally
              )
              return(gpl)
            }

            # extract data and save in tmp_tab
            tmp_tab <-  tmp_obj$output.list$TABLES.COMBINED_all.sources_counts
            # reshape table to a wide form
            tmp_tab <- reshape2::melt(tmp_tab)
          }

          # extract levels for the categorical variables from metadata
          if (cur_metadata[var_selected,]$opal.nature=="CATEGORICAL") {
            if (!exists("varLabels")) varLabels <- spec_metadata[[var_selected]]$levels %>% jsonlite::fromJSON() %>% unlist()
          }

          # create histogram object from tmp_tab data
          hist_obj <- tmp_tab %>% dplyr::rename(counts=value) %>% dplyr::mutate(density = counts/sum(counts))
          # do not show a bar for missing values
          hist_obj <- hist_obj[complete.cases(hist_obj),]
          midnames <- paste0(hist_obj[[paste0(tabsymbol,"$",var_selected)]]) %>% unique()
          # use levels from metadata for the bar labels
          midnames[midnames %in% names(varLabels)] <- varLabels[midnames[midnames %in% names(varLabels)]]
          hist_obj$mids <- factor( midnames , levels=unique(midnames))

          # set label for selected variable
          varlabel <- if (USER$useLabels) x_label else var_selected

          # check if groups are selected
          if (!is.null(grp1) || !is.null(grp2)) {
            # grouped barplot:
            # grouping type for the first variable must not be facets (group 1 and 2 will have to be flipped)
            req(grp1_type != "facets")

            ## get variable descriptions (for popover)
            #if (is.null(grp1)) grp1Metadata <- NULL else grp1Metadata <- as.matrix(var_metadata)[,"spec"][[grp1]] %>% jsonlite::fromJSON()
            #if (is.null(grp2)) grp2Metadata <- NULL else grp2Metadata <- as.matrix(var_metadata)[,"spec"][[grp2]] %>% jsonlite::fromJSON()

            hist_obj$breaks <- hist_obj$mids

            # set group levels as factors
            if (!is.null(grp1)) hist_obj[[paste0(tabsymbol,"$",grp1)]] <-  factor( hist_obj[[paste0(tabsymbol,"$",grp1)]] , levels=seq_along(levs1), labels = levs1)
            if (!is.null(grp2)) hist_obj[[paste0(tabsymbol,"$",grp2)]] <-  factor( hist_obj[[paste0(tabsymbol,"$",grp2)]] , levels=seq_along(levs2), labels = levs2)

            # save levels as levs1 and levs2 in histogram object
            hist_obj$levs1 <- hist_obj[[paste0(tabsymbol,"$",grp1)]]
            hist_obj$levs2 <- hist_obj[[paste0(tabsymbol,"$",grp2)]]
          }
          the_data <- hist_obj

          labels4ticks <- NULL
        } else {
          #DATA SHIELD mode with numeric data

          all_hist <- function(tab, feature, bins, range=NULL, datasources=DSI::datashield.connections_find()) {
            var <- paste0(tab,"$",feature)
            if (input$debugmode) browser()
            options(datashield.progress=F)
            # catch possible dataSHIELD errors
            tryCatch(
              expr = {
                hist_data <- dsDashboard::ggHistDS(var, plot="nope", datasources = datasources, bins=bins, range=range)
                options(datashield.progress=T)
                hist_obj <- hist_data$combined$histobject
                data.frame(left=head(hist_obj$breaks,-1), right=tail(hist_obj$breaks,-1), counts=hist_obj$counts, density=hist_obj$density, mids=hist_obj$mids)
              },
              error = function(cond) {
                options(datashield.progress=T)
                message(paste("ggHistDS error:",conditionMessage(cond)));
                message(datashield.errors());
                if (input$debugmode)
                  showNotification(datashield.errors(), duration=globals$notification_duration, type="error")
                else
                  showNotification("'Restricted output due to privacy parameters", duration=globals$notification_duration, type="warning")
                return(data.frame(left=NA, right=NA, counts=NA, density=NA, mids=NA))
              })

          }

          # get range '(assumes only _one_ plot is created)
          hist_range <- dsDashboard::getRange(paste0(tabsymbol,"$",var_selected), type="combined", datasources=cur_conn)
          var_sd <- dsBaseClient::ds.var(paste0(tabsymbol,"$",var_selected), type = "combined", checks = F, datasources=cur_conn)
          safer_range <- T; if(safer_range) hist_range <- hist_range +  0.01*sqrt(var_sd$Global.Variance[,"EstimatedVar"])* c(-1,1)
          if (input$debugmode) browser()
          # get histogram data for each group
          grouped_hist_data <- dsGapply(x=tabsymbol, G=globals$selectedGrps, FUN=all_hist, range=hist_range, bins=bins, feature=var_selected, lazy=TRUE, datasources=cur_conn)

          req(grp1_type != "facets")  # is it even possible to fail?

          the_data <- grouped_hist_data
          # put correct level labels for group1 in the_data
          if (length(levs1) > 0) {
            lev_mapping1 <- lapply(as.matrix(globals$metaDataCollection[[tab]])[,"spec"][names(grp1desc)], function(x) jsonlite::fromJSON(x)  %>% .$levels %>% jsonlite::fromJSON()  %>% unlist()  ) %>% unlist() %>% unname()
            #lev_mapping1 <- globals$metaDataCollection[[tab]] %>% as.data.frame() %>% .$spec %>% .[[names(grp1desc)]] %>% jsonlite::fromJSON() %>% .$levels %>% jsonlite::fromJSON() %>% unlist()
            #lev_mapping1 <- globals$metaDataCollection[[tab]]$spec[[names(grp1desc)]] %>% jsonlite::fromJSON() %>% .$levels %>% jsonlite::fromJSON()
            the_data[[grp1]] <-  lev_mapping1[as.integer(the_data[[grp1]])]
          }
          # put correct level labels for group1 in the_data
          if (length(levs2) > 0) {
            lev_mapping2 <- lapply(as.matrix(globals$metaDataCollection[[tab]])[,"spec"][names(grp2desc)], function(x) jsonlite::fromJSON(x)  %>% .$levels %>% jsonlite::fromJSON()  %>% unlist()  ) %>% unlist() %>% unname()
            #lev_mapping2 <- jsonlite::fromJSON( globals$metaDataCollection[[tab]][names(grp2desc)[1],]$spec) %>% .$levels %>% jsonlite::fromJSON() %>% unlist()
            the_data[[grp2]] <-  lev_mapping2[as.integer(the_data[[grp2]])]
          }

          # rename columns for groups to levs1 and levs2 (the name used in the following plot code)
          if (!is.null(grp1)) {
            the_data <- the_data %>% dplyr::rename(levs1=dplyr::all_of(grp1))
            if (grp1!="country") the_data$levs1 <- factor(the_data$levs1, levels=lev_mapping1, ordered=F)  # with ordered the ggplot2 colors would be different
          }
          if (!is.null(grp2)) {
            the_data <- the_data %>% dplyr::rename(levs2=dplyr::all_of(grp2))
            if (grp2!="country") the_data$levs2 <- factor(the_data$levs2, levels=lev_mapping2, ordered=F)  # with ordered the ggplot2 colors would be different
          }
          labels4ticks <- prettyNum(signif(unique(the_data$mids),3))
          the_data <- the_data %>% mutate(mids=as.factor(mids))
        }
      }

      message("distributional plot will be created")
      # unified plot code for numerical and non-numerical data:
      pl <- ggplot2::ggplot(the_data,
                            aes(x = mids, y = if (ycounts) counts else density, fill=levs1,
                                data_id=levs1
                            )) +
        xlab(if (USER$useLabels) x_label else varlabel) +
        ylab(ifelse(grp1_type=='fill','Proportion',ifelse(ycounts,"Absolute frequency","Relative frequency"))  ) +
        ggiraph::geom_bar_interactive(stat = "identity",alpha = ifelse(grp1_type=='identity',0.5,0.8), position=grp1_type,
                                      aes(data_id = levs1,
                                          onmouseenter = paste0("console.log(\"raise: ",levs1,"\");d3.selectAll(`[data-id=\"",levs1,"\"]`).raise()"),
                                          `data-id` = levs1),
                                      extra_interactive_params = c("onmouseenter","data-id")) +
        ggiraph::scale_fill_discrete_interactive(
          data_id = function(breaks) { as.character(breaks)},
          tooltip = function(breaks) { as.character(breaks)},
          onmouseenter = function(breaks) { paste0("console.log(\"raise: ",as.character(breaks),"\");d3.selectAll(`[data-id=\"",as.character(breaks),"\"]`).raise()") },
          `data-id` = function(breaks) { as.character(breaks)},
          extra_interactive_params = c("onmouseenter","data-id"),
          guide = guide_legend_interactive(
            title = if (USER$useLabels) stringr::str_wrap(grp1label,20) else grp1,
            title.theme = element_text_interactive(
              tooltip = grp1desc),
            label.theme = element_text_interactive()
          ),
          labels = function(breaks) {
            lapply(breaks, function(br) {
              label_interactive(
                as.character(br),
                data_id = as.character(br),
                `data-id` = as.character(br),
                onmouseenter = paste0("console.log(\"raise: ",as.character(br),"\");d3.selectAll(`[data-id=\"",as.character(br),"\"]`).raise()"),
                extra_interactive_params = c("onmouseenter","data-id"),
                tooltip = as.character(br)
              )
            })
          }
        )
      remove_elem <- function(x,n) {
        for (i in (1:length(x))) {
          if ((i+n-1) %% n != 0) {x[i]<-''}
        }
        return(x)
      }
      if(!is.null(labels4ticks)) {
        if ((!is.null(levs2)) && length(levs2)>1)
          skip_n <- max(1,round(length(labels4ticks)/5))
        else
          skip_n <- max(1,round(length(labels4ticks)/9))
        pl <-  pl +
          ggplot2::scale_x_discrete(labels = remove_elem(labels4ticks, skip_n))
      }
      if(grp1_type=="dodge") {
        n_pl_x <- length(layer_scales(pl)$x$range$range)
        len_pl_y <- diff(range(as.numeric(layer_scales(pl)$y$range$range)))
        pl <- pl+    ggbrace::stat_brace(data=data.frame(x=rep(1:n_pl_x ,each=2)+c(0.04,1-0.04)-0.5, y=0,group=as.factor(rep(1:n_pl_x ,each=2))), aes(x,y, group), outerstart=0, inherit.aes=F, outside=T, width=len_pl_y/40 , rotate=180) +
          scale_y_continuous(expand=c(0,len_pl_y/50))
      }
      pl <- pl +
        theme(
          axis.title.x = element_text_interactive(
            data_id = "axis.title.x",#"axis.title",#"axis.title.x",
            tooltip = xdesc #var_x_desc[var_selected]#,  var_x_desc
          ),
          axis.title.y = element_text_interactive(
            data_id = "axis.title.y",#"axis.title",
            tooltip = ifelse(grp1_type=='fill','Proportion of observations',ifelse(ycounts,"Number of observations","Frequency density - proportion of observations"))
          ),
          base_family = "Roboto Condensed"
        )
      if (!is.null(grp2)) {
        pl <- pl +
          facet_wrap_interactive(~levs2,
                                 labeller = labeller_interactive(
                                   aes(
                                     label = stringr::str_wrap(paste( if (USER$useLabels) grp2label else grp2,levs2, sep=": "),20),
                                     tooltip = paste0(grp2desc,": ", levs2),
                                   )))
      }

      # in any case we make the plot interactive
      gpl <- ggiraph::girafe(ggobj = pl, font_set = plot_font_set#, fonts = list(sans = c("Roboto Condensed","Roboto")) # specifying font here would misalign the font in local dashboard mode
      )
      gpl <- girafe_options(gpl,
                            opts_sizing(rescale = TRUE),
                            opts_zoom(min = .5, max = 4),
                            opts_toolbar(hidden=c("lasso_select", "lasso_deselect","saveaspng")),
                            opts_hover_key(css = girafe_css(
                              css = "fill-opacity:1;stroke:transparent;stroke-width:0px;",
                              text = "fill:black" # legend text color when hovering legend (not bars)
                            )
                            ),
                            opts_hover(css = girafe_css(
                              css = "fill-opacity:1;stroke:blue;stroke-width:1px;", # transparent
                              area = "stroke-width:1px;stroke:black",
                              point = "stroke-width:1px;stroke:red",
                              line = "stroke-width:1px;stroke:red",
                              image = "stroke-width:1px;stroke:red",
                              text= "fill:black;stroke-width:0px;" # text color when hovering bar (not legend)
                            )
                            )
      )

      if (grp1_type=="identity") {
        gpl <- girafe_options(gpl,
                              opts_selection(type = "multiple", only_shiny = FALSE, # multiple
                                             css = girafe_css(
                                               css = "fill:transparent;stroke-width:0px;", #;fill-opacity:0.0
                                               text = "fill:grey"
                                             )  ),
                              opts_selection_key(type = "none", only_shiny = FALSE  ),
                              opts_selection_theme(type = "none", only_shiny = FALSE  ),
                              opts_hover_inv(css = girafe_css(css = "fill-opacity:0.4;stroke:grey;stroke-width:1px;",
                                                              text = "fill:grey;stroke-width:0px;",
                                                              area = "stroke-width:0px;"))
        )
      } else {
        gpl <- girafe_options(gpl,
                              opts_selection(type = "none", only_shiny = FALSE ),
                              opts_selection_key(type = "none", only_shiny = FALSE  ),
                              opts_selection_theme(type = "none", only_shiny = FALSE  ),
                              opts_hover_inv(css = "fill:grey;fill-opacity:0.4;stroke:transparent;stroke-width:0px;")
        )
      }

      # remember the svg id
      globals$svgid_bar <- gpl$x$uid
      # return a plot
      return(gpl)

    }
  })

  outputOptions(output, "distPlot", suspendWhenHidden = T)

  output$distPlotInstructions <- renderUI({
    box(width=12, title = "Instructions", status="info",
        "Please choose one feature. Up to two groups can be selected additionally for grouped plots." )
  })

  output$distPlotWarning <- renderUI({
    box(width=12, title = "Instructions", status="warning",
        "Please choose ",tags$b("only one feature"),". Up to two groups can be selected additionally for grouped plots." )
  })

  output$pairPlotInstructions <- renderUI({
    box(width=12, title = "Instructions", status="info",
        "Please choose two features. One group can be selected additionally." )
  })

  output$pairPlotWarning <- renderUI({
    box(width=12, title = "Instructions", status="warning",
        "Please choose ",tags$b("only one group"),"." )
  })

  output$boxPlotWarning <- renderUI({
    box(width=12, title = "Instructions", status="warning",
        "Please choose ",tags$b("less than three groups"),"." )
  })

  output$pairPlot <- ggiraph::renderGirafe({
    # must be logged in
    req(USER$login)

    # name and number of currently selected table
    tab <- input$inpTableSelect
    tabnr <- readr::parse_number(tab)
    tabname <- globals$allTables[tabnr,".x"]
    tabsymbol <- globals$allTables[tabnr,"symbol"]

    # a table must be selected (length larger than 0)
    # table selection must not mismatch the selection saved in globals
    req(length(globals$selectedTable)>0 && globals$selectedTable==tab)

    # two numerical features must be selected
    ftrs_num <- globals$selectedNumericalFtrs
    group_vars <- globals$selectedGrps
    req(length(ftrs_num) >= 2)
    req(length(group_vars) < 2) # current limit is one group variable

    # get current connection
    cur_conn <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables

    message("pairplot will be plotted")
    if (input$debugmode) browser()

    if (is.null(cur_conn)) {

      # local mode: save local table in "theTable"
      theTable <- as.data.frame(USER$data[[tabname]], check.names = FALSE)

      # group selection must match the table columns
      req(all(ftrs_num %in% colnames(theTable)))

      comb_vars <- as.data.frame(combn(ftrs_num,2))
      comb_list <- lapply(comb_vars,
                          function(v) {data.frame(xName=v[1], yName=v[2],
                                                  x = theTable[,v[1]],
                                                  y = theTable[,v[2]],
                                                  g = theTable[,group_vars]
                          )} )
      # rbind list elements
      comb_df <- do.call("rbind", comb_list)
      rownames(comb_df) <- NULL

      if (!is.null(group_vars)) names(comb_df)[5] <- group_vars
      xydat <- comb_df
    } else {
      # DataSHIELD mode

      # make sure we login to DataSHIELD if connection is lost (global and local connection)
      if (do_global_login) tryCatch( {DSI::dsListWorkspaces(conns[[1]])}, error=function(e) {conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(conns)})
      tryCatch( {DSI::dsListWorkspaces(globals$conns[[1]])}, error=function(e) {globals$conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(globals$conns)})
      # development note: if connection is lost and tables have been pooled in the dashboard, the pooled tables have to be created again

      # get current connection
      cur_conn <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables
      # get plot data
      xydat <- dsGetPairs(tabsymbol, ftrs_num, group_vars[1],
                          datasources = cur_conn)
    }

    # filter for complete cases (notice: this affects the density plots also)
    xydat <- xydat[complete.cases(xydat[,c("x","y")]),]

    # limit the number of plotted data points (according to the slider value)
    # (refers to the number of points in the upper or lower half triangle in the scatterplot matrix)
    ptlim <- input$ptLimit
    if (!is.null(ptlim) && ptlim < nrow(xydat)) xydat <- xydat %>% dplyr::sample_n(ptlim)

    # we always show the points on lower and upper triangle
    both_sides <- T
    if (!both_sides) {
      # creates NA dummy observations "fooflip" for the empty plots in the scatterplot matrix
      foo <- xydat %>% dplyr::select(xName,yName) %>% dplyr::distinct()
      fooflip <- foo
      fooflip$x <- NA
      fooflip$xName <- foo$yName
      fooflip$y <- NA
      fooflip$yName <- foo$xName
      if (dim(xydat)[2]>4) fooflip <- fooflip %>% dplyr::bind_cols(xydat[5:dim(xydat)[2]]%>%dplyr::filter(dplyr::row_number()==1) ) # col name stays intact

      # half triangle dataset for the scatterplots is generated
      foos <- rbind(xydat,fooflip)
      foos$xName <- factor(foos$xName, levels=ftrs_num)
      foos$yName <- factor(foos$yName, levels=ftrs_num)
      xydat_triangle <- foos
    }

    # even if we want a reduced scatterplot matrix,
    # for the diagonal of density plots the data need to be doubled with their flipped counterpart
    # for case "both sides" the flipped data is always used (for scatterplot and density plot in the scatterplot)
    foo <- xydat
    fooflip <- foo
    fooflip$x <- foo$y
    fooflip$xName <- foo$yName
    fooflip$y <- foo$x
    fooflip$yName <- foo$xName
    foos <- rbind(foo,fooflip)
    foos$xName <- factor(foos$xName, levels=ftrs_num)
    foos$yName <- factor(foos$yName, levels=ftrs_num)
    xydat_both <- foos

    grp1 <- if (length(group_vars)>=1) group_vars[1] else NULL
    grp2 <- if (length(group_vars)>=2) group_vars[2] else NULL

    # filter for feature variable and selected summary statistics

    # collect variable names with variable labels (if labels shall be used)
    selVars <- c(ftrs_num, grp1, grp2)
    # get metadata from spec attribute for these variables
    var_metadata <- as.data.frame(globals$metaDataCollection[[tab]])
    # filter rows by sleVars
    var_metadata <- var_metadata[rownames(var_metadata) %in% selVars,]

    # decode json string in spec to extract descriptions, labels, ...
    spec_metadata <- lapply(var_metadata[["spec"]], function(x) if (is.null(x)) NULL else jsonlite::fromJSON(x)) %>% setNames(rownames(var_metadata))

    # get labels
    # standard Hmisc-labels
    names(selVars) <- var_metadata["label"][selVars,,drop=F] %>% unlist() %>% unname()
    # if available, override by values from spec attribute
    if (!(is.null(var_metadata$spec) || all(unlist(lapply(var_metadata$spec,is.null))) ) ) {
      names_from_spec <- unlist(lapply(as.matrix(var_metadata)[,"spec"], function(x) {
        if (is.null(x)) character(0) else jsonlite::fromJSON(x)$alias
      }))
      names(selVars) <- ifelse(is.na(names_from_spec), names(selVars), names_from_spec)
    }
    var_label <- setNames(names(selVars) , selVars)
    var_desc <- lapply(spec_metadata, function(x) if (is.null(x)) NA_character_ else x$desc) %>% unlist() %>%
      .[names(var_label)]   # sort according to var_label
    var_desc <- ifelse(is.na(var_desc), var_label, var_desc)
    var_levs_JSON <- sapply(spec_metadata, function(x) x$levels) %>% setNames(rownames(var_metadata))
    # use descriptive level names instead of integers
    var_levs_decoded <- lapply(var_levs_JSON, function(x) if (is.null(x)) NULL else unlist(jsonlite::fromJSON(x)))
    # update level labels in xydat_triangle and xydat_both
    for (vname in names(var_levs_decoded)) {
      v_levs <- var_levs_decoded[[vname]]
      if (!is.null(v_levs)) {
        if (!is.factor(xydat_both[[vname]])) { # in case of DataSHIELD mode this might be a character
          xydat_both[[vname]] <- factor(xydat_both[[vname]], labels=v_levs)
        } else {
          levels(xydat_both[[vname]]) <- v_levs
        }

        if (exists("xydat_triangle")) {
          if (!is.factor(xydat_triangle[[vname]])) { # in case of DataSHIELD mode this might be a character
            xydat_triangle[[vname]] <- factor(xydat_triangle[[vname]], labels=v_levs)
          } else {
            levels(xydat_triangle[[vname]]) <- v_levs
          }
        }
      }
    }

    if (both_sides) xydat <- xydat_both else xydat_triangle

    # 1. add scatterplots in lower/upper triangle
    pl <- ggplot2::ggplot(xydat, ggplot2::aes(x=x, y=y)) +
      ggiraph::facet_grid_interactive(yName ~ xName, scales = "free",
                                      labeller = labeller(
                                        yName = labeller_interactive(
                                          ggplot2::aes(
                                            label= if (USER$useLabels) stringr::str_wrap(var_label[.label],20) else .label,
                                            tooltip = paste0(yName, ":\n", var_desc[formulaic::add.backtick(yName)] )
                                          )
                                        ),
                                        xName = labeller_interactive(
                                          ggplot2::aes(
                                            label= if (USER$useLabels) stringr::str_wrap(var_label[.label],20) else .label,
                                            tooltip = paste0(xName, ":\n", var_desc[formulaic::add.backtick(xName)] )
                                          )
                                        )
                                      )  ) +
      ggiraph::geom_point_interactive(ggplot2::aes_string(colour=formulaic::add.backtick(grp1),
                                                          tooltip = formulaic::add.backtick(grp1),
                                                          data_id = formulaic::add.backtick(grp1),
                                                          `key-id` = formulaic::add.backtick(grp1)),
                                      extra_interactive_params = c("onmouseenter","data-id"),
                                      na.rm = TRUE, alpha=0.6)
    # 2. add density plots on diagonal
    xydatall <- xydat_both
    for (i_Var in as.character(unique(xydatall$xName))) {
      pl <- pl + ggplot2::stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)),
                                       data =  data.frame(xName=xydatall$xName,yName=xydatall$xName,x=xydatall$x) %>% dplyr::filter(xName==i_Var) %>% dplyr::filter(!is.na(x)),
                                       position = "identity",
                                       colour = "grey20", geom = "line")
    }
    # axis labels are obsolete for this kind of plot
    pl <- pl + ggplot2::xlab("") + ggplot2::ylab("")

    # get all combinations of feature variables
    combs <- as.data.frame(cbind(combn(ftrs_num,2),combn(ftrs_num,2)[2:1,]))
    # get correlations for each combination
    use_all_data <- TRUE
    if (use_all_data) {
      # calculate correlation based on all (not only the plotted) data points
      if (is.null(globals$conns)) {
        cors <- sapply(combs, function(pair) { cor(theTable[,pair], use="pairwise.complete.obs")[[2]] } )
      } else {
        cors <- sapply(combs, function(pair) ds.cor(x = paste0(tabsymbol,"$",pair[1]), y = paste0(tabsymbol,"$",pair[2]), type = "combine",
                                                    datasources = cur_conn)$`Correlation Matrix`[[2]] )
      }
      cors <- data.frame(xName=as.character(combs[1,]), yName=as.character(combs[2,]), cor=cors)
    } else {
      # Calculate correlation (rounded) for each group based on plot data
      cors <- xydat %>%
        group_by(xName, yName) %>%
        summarise(
          cor = cor(x, y, use = "pairwise.complete.obs"),
          .groups = "drop"
        )
    }
    # if plots are only in lower triangle, then write correlations only in upper triangle
    if (!both_sides) cors$cor[1:round(length(cors$cor)*0.5)] <- NA

    # plot correlation values into scatterplots (upper left corner)
    # drop NA mskes sure to plot only when values are available (e.g. if !both_sides, then plot values only in upper triangle)
    pl <- pl + geom_label(data=cors %>% tidyr::drop_na(),
                          aes(label=paste("cor.: ", round(cor,2+input$inpDigits), sep="")),
                          x=-Inf, y=Inf, vjust = "inward", hjust = "inward", alpha=0.7, fill="darkgrey", colour = "black", fontface = "bold")
    # set aspect ratio and font, define legend properties
    pl <- pl + theme(aspect.ratio = 1,
                     base_family = "Roboto Condensed"
    ) +
      ggiraph::scale_color_discrete_interactive(
        data_id = function(breaks) { as.character(breaks)}, #=key-id
        tooltip = function(breaks) { as.character(breaks)},
        `data-id` = function(breaks) { as.character(breaks)},
        extra_interactive_params = c("data-id"),
        guide = ggiraph::guide_legend_interactive(
          title = if (USER$useLabels) stringr::str_wrap(var_label[grp1],20) else grp1,
          title.theme = ggiraph::element_text_interactive(tooltip = paste0(grp1,": ",var_desc[grp1]) ),
          label.theme = ggiraph::element_text_interactive()
        ),
        labels = function(breaks) {
          lapply(breaks, function(br) {
            label_interactive(
              as.character(br),
              data_id = as.character(br),
              `data-id` = as.character(br),
              extra_interactive_params = c("data-id"),
              tooltip = as.character(br)
            )
          })
        }
      )
    # create ggiraph object
    gpl <- ggiraph::girafe(ggobj = pl,
                           width_svg = 6.5, height_svg = 5)
    gpl <- girafe_options(gpl,
                          opts_sizing(rescale = TRUE, width = 1),
                          opts_zoom(min = .5, max = 4),
                          opts_toolbar(hidden=c("lasso_select", "lasso_deselect","saveaspng")),
                          opts_hover(css = girafe_css(                             # when hovering point in plot or legend styles can be adapted for css, area, point, line or image, additionally to "_hover_" there is also "_selection_"
                            css = "fill-opacity:1;stroke:black;stroke-width:1px;", # point style when hovering point
                            text= "font-weight: bold;stroke-width:0px;"            # text style
                          )
                          ),
                          opts_hover_inv(css = girafe_css(css = "fill-opacity:0;stroke-width:0px;",
                                                          text = "fill-opacity:0.4;stroke-width:0px;"
                          )),
                          opts_hover_key(css = girafe_css(                       # when hovering over legend bullet
                            css = "",
                            point = "stroke:black;stroke-width:1px;"            # bullet style
                          )),
                          opts_hover_theme(css = girafe_css(                     # when hovering legend text
                            css = "",
                            text = "fill:black;"
                          ))

    )

    # no selectable objects in plot
    gpl <- girafe_options(gpl,
                          opts_selection(type = "none", only_shiny = FALSE
                          ),
                          opts_selection_key(type = "none", only_shiny = FALSE  ),
                          opts_selection_theme(type = "none", only_shiny = FALSE  )
    )

    return(gpl)
  })

  # decodes the metadata that we have saved within the spec attribute
  # (DataSHIELD has only few possible attributes for metadata - to circumvent this restriction
  # we save all metadata as a JSON string in the spec attribute)
  decode_spec_attribute <- function(var_spec) {
    # spec attribute available?
    has_spec <- sapply(var_spec, function(x) !is.null(x))

    # decode json string in spec to extract descriptions, labels, ...
    spec_decoded <- lapply(var_spec, function(x) if (is.null(x)) NULL else jsonlite::fromJSON(x))
    # spec_metadata <- do.call(rbind,spec_decoded)

    # get labels
    spec_label <- sapply(spec_decoded, function(x) if(!is.null(x)) x$label else NA_character_)
    spec_alias <- sapply(spec_decoded, function(x) if (is.null(x)) NA_character_ else x$alias)
    # get description
    spec_desc <- sapply(spec_decoded, function(x) if(!is.null(x)) x$desc else NA_character_)
    # get levels
    spec_levels_JSON <- sapply(spec_decoded, function(x) if(!is.null(x)) x$levels else NA_character_)
    spec_levels<- lapply(spec_levels_JSON, function(x) if (is.na(x)) character(0) else unlist(jsonlite::fromJSON(x)))
    # get decimals
    spec_decimals <- sapply(spec_decoded, function(x) if (is.null(x)) NA_integer_ else x$decimals)

    return(list(label=spec_label, alias=spec_alias, description=spec_desc, levels=spec_levels,
                decimals=spec_decimals, has_spec=has_spec))
  }

  # join metadata from spec attributes and labels iout of a local table
  get_metadata_local <- function(the_table) {
    # check if spec attribute exists and extract metadata
    var_spec <- sapply(the_table, function(x) attr(x,"spec"))
    extracted_metadata <- decode_spec_attribute(var_spec)

    # if information is missing, replace label and description by the label from label attribute
    extracted_metadata$label <- ifelse(extracted_metadata$has_spec,
                                       extracted_metadata$label,
                                       Hmisc::label(the_table) )
    extracted_metadata$alias <- ifelse(extracted_metadata$has_spec,
                                       extracted_metadata$alias,
                                       Hmisc::label(the_table) )
    extracted_metadata$description <- ifelse(extracted_metadata$has_spec,
                                             extracted_metadata$description,
                                             Hmisc::label(the_table) )
    # if information is missing, replace decimals by values from attribute "decimals"
    decimals_attr <- lapply(the_table, function(x) attr(x,"decimals"))
    decimals_attr[sapply(decimals_attr, is.null)] <- default_decimals <- 1
    extracted_metadata$decimals <- ifelse(extracted_metadata$has_spec,
                                          extracted_metadata$decimals,
                                          decimals_attr )
    return(extracted_metadata)
  }


  # returns variable labels and descriptions as named vector
  extract_from_metadata_ds <- function(tab, selected_vars) {
    # get metadata either from DataSHIELD or from globals:
    #  are the required metadata in globals?
    if (is.null(globals$metaDataCollection[[tab]]) ||                                          # no metadata
        nrow(globals$metaDataCollection[[tab]]) == 0 ||                                        # empty metadata - check is necessary despite of followin line
        nrow(globals$metaDataCollection[[tab]][selected_vars,]) != length(unique(selected_vars))) {  # OR: not all the requested metadata
      # no, then get metadata from DataSHIELD and save it in globals
      # build object names for selected variables
      vars_4_meta <- sapply(selected_vars, function(x) paste0(tabsymbol,"$",x)) # sapply, because for empty selected_vars nothing can be done...
      var_metadata <- as.data.frame( dsDashboard::ds.meta2(vars_4_meta, cur_conn) )
      isolate( globals$metaDataCollection[[tab]] <<- var_metadata )
    } else {
      # yes, copy metadata for the chosen variables from globals to var_metadata
      var_metadata <- as.data.frame(globals$metaDataCollection[[tab]][selected_vars,])
    }

    # fall back to labels if spec metadata are incomplete or missing
    decoded_spec <- decode_spec_attribute( setNames(var_metadata$spec, rownames(var_metadata)) )
    var_label <- ifelse( !is.na(decoded_spec$label), decoded_spec$label, setNames(as.character(var_metadata$label), rownames(var_metadata)) )
    var_alias <- ifelse( !is.na(decoded_spec$alias), decoded_spec$alias, setNames(as.character(var_metadata$label), rownames(var_metadata)) )
    var_desc <- ifelse( !is.na(decoded_spec$description), decoded_spec$description ,  var_label)

    return(list(label=var_label, alias=var_alias,
                description=var_desc,
                levels=decoded_spec$levels,
                decimals=decoded_spec$decimals))
  }

  #' @title Get summary statistics for all or some variables of a table in unified form
  #'
  #' @description This function is a wrapper for specialized summary functions.
  #'
  #' It returns for each of the requested variables (`vars`) in table `tab` from the
  #' `datasources` the summary statistics specified in `summaries`.
  #' @param tab A character string. Name of the DataShield table object.
  #' @param vars A vector of character strings. Names of the variables in the table object `tab`.
  #' @param summaries A vector of character strings. Possible values are:
  #' "quantiles", "5%","25%","50%","75%","95%", "min", "max", "mean", "N", "median", "Msng", "sd", "var", "density", "boxplot", "pie", "barplot", "mode", "levels", "distinct"
  #' @param current_data ....
  #' @param shiny_notification A logical or integer. If not `FALSE` the parameter specifies the number
  #' of seconds that a notification is shown in case of errors.
  #' @param datasources a list of \code{\link{dsBaseClient::DSConnection-class}}
  #' objects obtained after login. If the \code{dsBaseClient::datasources} argument is not specified
  #' the default set of connections will be used: see \code{\link{dsBaseClient::datashield.connections_default}}.
  #' @return A data.frame containing the summaries for each requested variable.
  #' @examples
  #' all_summaries("D")
  #' # A tibble: 1 × 3
  #'  variable feature  value
  #'  <chr>    <chr>    <chr>
  #'  1 bmi_T1   missings 2228
  all_summaries <- function(tab, vars=NULL, summaries=NULL, current_data=NULL, datasources=datashield.connections_find()) {
    # initialize variables with NULL
    summ1 <- summ2 <- summ3 <- summ4 <- summ5 <- summ6 <- summ7 <- sparkline_pieplot <- sparkline_barplot<- NULL

    # if no table selected, return empty data.frame:
    if (is.na(tab)) return(dplyr::as_tibble(data.frame(variable = character(), feature=character(), value=double())))

    # make sure only datasources with available table tab are used
    datasources <- datasources[unlist(ds.exists(tab, datasources))]

    # if no valid datasources, return empty data.frame:
    if (length(datasources) < 1) return(dplyr::as_tibble(data.frame(variable = character(), feature=character(), value=double())))

    # if no variable selected in vars, then dsIsNumeric chooses all variables of table tab
    vars_numeric <- dsDashboard::dsIsNumeric(tab, vars, datasources=datasources)
    vars_names <- names(vars_numeric)
    vars_numeric_names <- names(vars_numeric)[vars_numeric==TRUE]
    vars_nonnumeric_names <- names(vars_numeric)[vars_numeric==FALSE]

    # if no current data present, then create empty tibble
    if (is.null(current_data) )  # current_data might be an empty list()
      current_data <- tibble(variable=character(), feature=character(), value=character())

    #if there is a tabname coloumn, filter for table tab
    if (!is.null(current_data$tabname))
      current_data <- current_data %>% dplyr::filter(tabname == tab) %>% dplyr::select(variable,feature,value)

    # suppress DataSHIELD progress in console
    options(datashield.progress=F)

    # quantiles or mean requested? (for numeric variables only)
    if (is.null(summaries) || length(intersect(summaries, c( "5%","10%","25%","50%","75%","90%","95%","median","Mean") ))>0 ) {
      vars_with_missing_stats <- setdiff(vars_numeric_names,
                                         current_data %>% dplyr::filter(feature=="median") %>% dplyr::pull(variable))
      # are there variables with missing and requested quantiles/mean?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Request quantiles/mean for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        # get the summaries
        summ1 <- dsDashboard::ds_get_quantile_mean(tab, vars_with_missing_stats, datasources=datasources)
        # 50% qunatile is added additionally as feature "median"
        median <- summ1[summ1$feature=="50%",]
        median$feature <- "median"
        summ1 <- rbind(summ1,median) %>%
          mutate_all(~ifelse(is.nan(.), NA, .))  # recode, because for 0 observations the mean, sd, var are NaN while the other features are NA!
      }
    }

    # density plot or extrema requested? (for numeric variables only)
    if (is.null(summaries) || length(intersect(summaries, c( "densplot","max","min") ))>0 ) { # densplot returned as (breaks, mids, counts, density)
      vars_with_missing_stats <- setdiff(vars_numeric_names,
                                         current_data %>% dplyr::filter(feature=="density") %>% dplyr::pull(variable))
      # are there variables with missing and requested density plot/extrema?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Get density/range for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        # summary "density sparkline" will be generated from a 20 bin histogram (includes information about range)
        hist_bins <- 20
        summ2 <- dsDashboard::ds_get_hist(tab, vars_with_missing_stats, bins=hist_bins, shiny_notification=globals$notification_duration, datasources=datasources)
      }
    }

    # missings Msng requested? (for numeric variables; exists also for non-numeric variables, but for categorical variables we get the missings below in summ5)
    if (is.null(summaries) || length(intersect(summaries, c( "missings") ))>0 ) {
      vars_with_missing_stats <- setdiff(vars_numeric_names,
                                         current_data %>% dplyr::filter(feature=="missings") %>% dplyr::pull(variable))
      # are there variables with missing and requested feature "Msng"?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Get missings for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        summ3 <- dsDashboard::ds_get_NA(tab, vars_with_missing_stats, datasources=datasources)
      }
    }

    # var, Nmissing, Nvalid, Ntotal or sd requested? (for numeric variables only)
    if (is.null(summaries) || length(intersect(summaries, c( "var","sd","Nmissing","Nvalid", "Ntotal" ) ))>0 ) {
      vars_with_missing_stats <- setdiff(vars_numeric_names,
                                         current_data %>% dplyr::filter(feature=="Nvalid") %>% dplyr::pull(variable))
      # are there variables with missing and requested feature var/Nmissing/Nvalid/Ntotal/sd?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Get variance/sd/N/missings for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "), # shows current subgroup in parentheses
                                " (",gsub("^.*?\\.", "", tab),")"),
                         duration=globals$notification_duration)
        summ4 <- ds_get_variance(tab, vars_with_missing_stats, datasources=datasources)  %>%
          mutate_all(~ifelse(is.nan(.), NA, .))  # recode, because for 0 observations sd, var is NaN while the other features are NA!
      }
    }

    # Nmissing, Nvalid, Ntotal, mode, Ndistinct, levels, pie, barplot requested? (for non-numeric vars only)
    if (is.null(summaries) ||
        length(intersect(summaries,
                         c( "Nmissing","Nvalid", "Ntotal","missings","sparkline_bar", "sparkline_pie",   # "pie"/"barplot" information is returned as levels 1,2,3...
                            "Ndistinct","mode","levels",
                            "used_levels", "max_group_n") ))>0 ) {
      vars_with_missing_stats <- setdiff(vars_nonnumeric_names,
                                         current_data %>% dplyr::filter(feature=="Ndistinct") %>% dplyr::pull(variable))
      # are there variables with missing and requested feature Nmissing/Nvalid/Ntotal/mode/Ndistinct/levels/pie/barplot?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Get categorical summaries for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        summ5 <- dsDashboard::ds_get_cat_summary(tab, vars_with_missing_stats, datasources=datasources)

        # usually we want to correct the level labels here
        correct_level_labels <- TRUE
        if (correct_level_labels) {
          vars_to_loop <- summ5[summ5$feature=="levels",][["variable"]]

          # get main parent tab name (assuming no "." in main tab name!!!)
          main_tab <- sub("\\..*", "", tab)
          # get the table id (available in either globals$allTables or globals$metaDataCollection)
          main_tab_id <- paste0("tab",rownames(globals$allTables[globals$allTables$symbol==main_tab,]))
          # read the levels from globals$metaDataCollection for the parent table
          var_levels <- lapply(as.matrix(globals$metaDataCollection[[main_tab_id]])[,"spec"][vars_to_loop], function(x) jsonlite::fromJSON(x) %>% .$levels %>% jsonlite::fromJSON()  %>% unlist()  )

          # go through all variable which have the feature "levels"
          for (j in vars_to_loop) { # NAs are not handled as factor level (level NA removed)
            # if levels feature is not missing
            if (!is.na(summ5[summ5$variable==j & summ5$feature=="levels","value"]$value) ) {
              # check if we have to correct the label of feature "mode"
              if (!is.na( summ5[summ5$variable==j & summ5$feature=="mode","value"]$value ) &&
                  all(!is.na(as.numeric(jsonlite::fromJSON(summ5[summ5$variable==j & summ5$feature=="mode","value"]$value))))) {
                # correct "mode"
                mode_in_summ5 <- summ5[summ5$variable==j & summ5$feature=="mode",]$value %>% (function(x) {if (is.na(x)) x else jsonlite::fromJSON(x)})()
                summ5[summ5$variable==j & summ5$feature=="mode","value"] <- if (length(mode_in_summ5)==1 && is.na(mode_in_summ5)) NA else as.character(jsonlite::toJSON(paste0(var_levels[[j]][mode_in_summ5], collapse=",")))
              }
              # check if we have to correct levels
              if (!is.na( summ5[summ5$variable==j & summ5$feature=="levels","value"]$value ) &&
                  all(!is.na(as.numeric(jsonlite::fromJSON(summ5[summ5$variable==j & summ5$feature=="levels","value"]$value))))) {
                # correct "levels"
                indices_in_summ5 <- summ5[summ5$variable==j & summ5$feature=="levels","value"]$value %>% jsonlite::fromJSON()
                summ5[summ5$variable==j & summ5$feature=="levels","value"] <- as.character(jsonlite::toJSON(var_levels[[j]][indices_in_summ5]))
              }
              # check if we have to correct used_levels
              # rm_dots: little helper to filter out "..." of a character vector in the if check below
              rm_dots <- function(x) x[x!="..."]
              if (!is.na( summ5[summ5$variable==j & summ5$feature=="used_levels","value"]$value ) &&
                  all(!is.na(as.numeric(rm_dots(jsonlite::fromJSON(summ5[summ5$variable==j & summ5$feature=="used_levels","value"]$value)))))) {
                # correct "levels"
                indices_in_summ5 <- summ5[summ5$variable==j & summ5$feature=="used_levels","value"]$value %>% jsonlite::fromJSON() %>% as.character()
                # NA2dots: little helper to replace NA by "..."
                NA2dots <- function(x) {x[is.na(x)] <- "..."; x}
                summ5[summ5$variable==j & summ5$feature=="used_levels","value"] <- if (length(indices_in_summ5)==0) NA else as.character(jsonlite::toJSON(NA2dots(var_levels[[j]][indices_in_summ5])))
              }
            }

          }
        }
      }
    }

    # boxplot requested? (for numeric variables)
    if (is.null(summaries) || length(intersect(summaries, c("boxplot") ))>0 ) { # boxplot information is returned as ("ymin","lower","middle","upper","ymax")
      vars_with_missing_stats <- setdiff(vars_numeric_names,
                                         current_data %>% dplyr::filter(feature=="lower") %>% dplyr::pull(variable))
      # are there variables with missing and requested feature boxplot?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Get boxplot data for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        summ6 <- dsDashboard::ds_get_boxplot_data(tab, vars_with_missing_stats,
                                                      message_fun=function(x) showNotification(x, duration=globals$notification_duration),
                                                      datasources=datasources) %>%
          mutate_all(~ifelse(is.nan(.), NA, .))  # recode, because for 0 observations ymin,lower,middle,upper,ymax is NaN while the other features are NA!
      }
    }

    # get the feature variable type always, because it is used anyway
    if (TRUE || is.null(summaries) || length(intersect(summaries, c( "type"   ) ))>0 ) {
      vars_with_missing_stats <- setdiff(vars_names,
                                         current_data %>% dplyr::filter(feature=="type") %>% dplyr::pull(variable))
      # are there variables with missing and requested feature "type"?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Get variable type for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        summ7 <- dsDashboard::ds_get_type(tab, vars_with_missing_stats, datasources=datasources)
      }
    }

    # collect all available categorical summaries (needed for pie plot and barplot)
    pre_comp_data <- rbind(summ5, current_data)

    # pie plot requested? (for categorical variables)
    if (is.null(summaries) ||
        length(intersect(summaries, c( "sparkline_pie") ))>0 ) { # "pie" is returned directly as sparkline
      vars_with_missing_stats <- setdiff(vars_nonnumeric_names,
                                         current_data %>% dplyr::filter(feature=="sparkline_pie") %>% dplyr::pull(variable))
      # are there variables with missing and requested feature "pie plot"?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Generating pie plot sparklines for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        sparkline_pieplot <- lapply(vars_with_missing_stats,
                                    function(x) {
                                      # filter for summaries of categorical variable x
                                      categorical_data <- pre_comp_data %>%
                                        dplyr::filter(variable==x) %>%
                                        dplyr::select(-variable) %>%
                                        tidyr::pivot_wider(names_from = feature, values_from = value)

                                      # in case of invalid/missing data return NA
                                      if (length(categorical_data)==1 && names(categorical_data)=="invalid") return(NA)
                                      else if (categorical_data$Nvalid==0) return(NA)
                                      else tryCatch({
                                        # build sparkline
                                        categories <- categorical_data$levels %>% jsonlite::fromJSON() %>% unlist() %>% unname()  # try block fails if categorical_data$levels == "NA"
                                        cat_frequencies <- categorical_data %>% dplyr::select(all_of(as.character(seq_along(categories)))) %>% setNames(categories)
                                        cat_names <- ifelse(!is.na(names(cat_frequencies)),names(cat_frequencies),"NA")
                                        # => as.list is used below due to a change in jsonlite:
                                        # "Input to asJSON(keep_vec_names=TRUE) is a named vector. In a future version of jsonlite, this option will not be supported, and named vectors will be translated into arrays instead of objects. If you want JSON object output, please use a named list instead. See ?toJSON."
                                        sparkline::spk_chr(as.list(setNames(as.numeric(cat_frequencies),cat_names)),
                                                           type ='pie',
                                                           tooltipFormatter = htmlwidgets::JS(
                                                             sprintf(
                                                               "function(sparkline, options, field){
                                          											  return %s[field.offset] + '<br/>' + field.value + ' ('+Math.round((field.percent + Number.EPSILON) * 100) / 100 +'%%)';
                                          										  }",
                                                               jsonlite::toJSON( cat_names )
                                                             )
                                                           )
                                        )
                                      },
                                      error = function(cond) {
                                        categorical_data$levels
                                        return(NA)
                                      })
                                    })

        # finalize sparkline
        sparkline_pieplot <- sparkline_pieplot %>%
          unlist() %>%
          cbind(vars_with_missing_stats, "sparkline_pie") %>%
          as.data.frame() %>%
          setNames(c("value","variable","feature"))  %>%
          dplyr::filter(value!="")
      }
    }

    # bar plot requested? (for categorical variables)
    if (is.null(summaries) ||
        length(intersect(summaries, c( "sparkline_bar") ))>0 ) { # "barplot" is returned directly as sparkline
      vars_with_missing_stats <- setdiff(vars_nonnumeric_names,
                                         current_data %>% dplyr::filter(feature=="sparkline_bar") %>% dplyr::pull(variable))
      # are there variables with missing and requested feature "barplot"?
      if (length(vars_with_missing_stats)>0) {
        # show status message in dashboard
        showNotification(paste0("Generating bar plot sparklines for variables: ",
                                paste0(vars_with_missing_stats, collapse = ", "),
                                " (",gsub("^.*?\\.", "", tab),")"), # shows current subgroup in parentheses
                         duration=globals$notification_duration)
        sparkline_barplot <- lapply(vars_with_missing_stats,
                                    function(x) {
                                      # filter for summaries of categorical variable x
                                      categorical_data <- pre_comp_data %>%
                                        dplyr::filter(variable==x) %>%
                                        dplyr::select(-variable) %>%
                                        tidyr::pivot_wider(names_from = feature, values_from = value)

                                      # in case of invalid/missing data return NA
                                      if (length(categorical_data)==1 && names(categorical_data)=="invalid") return(NA)
                                      else if (categorical_data$Nvalid==0) return(NA)
                                      else tryCatch({
                                        # build sparkline
                                        categories <- categorical_data$levels %>% jsonlite::fromJSON() %>% unlist() %>% unname() # info: if categorical_data$levels is NA then try fails with: Fehler in if (is.character(txt) && length(txt) == 1 && nchar(txt, type = "bytes") <  : \n Fehlender Wert, wo TRUE/FALSE nötig ist
                                        cat_frequencies <- categorical_data %>% dplyr::select(all_of(as.character(seq_along(categories)))) %>% setNames(categories)
                                        cat_names <- ifelse(!is.na(names(cat_frequencies)),names(cat_frequencies),"NA")
                                        # attention: sparkline popover has problems when there is a bar with count "NA"/"null" - recode NA to 0.5!
                                        cat_frequencies[is.na(cat_frequencies)] <- as.character(0.5)

                                        sparkline::spk_chr(as.numeric(cat_frequencies),
                                                           type ='bar',
                                                           # in the tooltipFormatter 0.5 is replaced back to NA!
                                                           tooltipFormatter = htmlwidgets::JS(
                                                             sprintf(
                                                               "function(sparkline, options, field){
                                          											  return %s[((field[0] == null) ? \"NA\" : field[0].offset)] + '<br/>' + ((field[0].value == '0.5') ? \"NA\" : field[0].value);
                                          										  }",
                                                               jsonlite::toJSON( cat_names )
                                                             )
                                                           ),
                                                           chartRangeMin=0
                                        )
                                      },
                                      error = function(cond) {
                                        categorical_data$levels
                                        return(NA)
                                      })
                                    })
        # finalize sparkline
        sparkline_barplot <- sparkline_barplot %>%
          unlist() %>%
          cbind(vars_with_missing_stats, "sparkline_bar") %>%
          as.data.frame() %>%
          setNames(c("value","variable","feature")) %>%
          dplyr::filter(value!="")
      }
    }

    # turn on DataSHIELD progress in console again
    options(datashield.progress=T)

    # return summaries
    rbind(summ1,summ2,summ3,summ4,summ5,summ6, summ7, sparkline_pieplot, sparkline_barplot, current_data) # sparkline_density and sparkline_boxplot are created outside this function, as they require global common ranges for all groupwise plots
  }

  output$summary <- renderUI({
    req(USER$login)

    # continue only if table, features and summary statistic are selected
    req(input$inpTableSelect)
    req(length(globals$selectedFtrs) > 0)
    req(input$inpSumStatSelect)

    # debug if in debug mode
    if (input$debugmode) browser()

    # currently selected table
    tab <- input$inpTableSelect
    # get "number" of the current table
    tabnr <- readr::parse_number(tab)
    # get symbol used for the table tab
    tabsymbol <- globals$allTables[tabnr,"symbol"]
    # get table name
    tabname <- globals$allTables[tabnr,".x"]
    # get connection for current table
    cur_conn <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables

    # table selection must not mismatch the selection saved in globals
    req(length(globals$selectedTable)>0 && globals$selectedTable==tab)

    # define variables (features, groups, active selection)
    mod_vars <- c(globals$selectedFtrs)
    group_vars <- c(globals$selectedGrps)
    selected_vars <- c(group_vars, mod_vars)

    # check for invalid feature selection
    if ((length(mod_vars) > 0) && (length(intersect(mod_vars, group_vars))>0))
      return(
        box(width=12, title = "Invalid variable selection", status = "warning", HTML(paste("A variable which is selected as a <b>feature</b> may not be selected as <b>group</b>:", paste0(intersect(globals$selectedFtrs, globals$selectedGrps), collapse=", ")) ) )
      )

    # define variables (digits, selectable features)
    sum_stats_str <- input$inpSumStatSelect
    extraDigits <- if (!is.null(input$inpDigits)) input$inpDigits else 0
    # get digits per variable further below...

    # check: are summary statistics available (already computed)? (only used in DataSHIELD mode)
    # if there is no global summary data collection create an empty template
    if (  isolate(length(globals$summaryDataCollection) < 1)  ) {
      summaryDataCollection <- setNames(list(list()), tab)
      globals$summaryDataCollection <<- summaryDataCollection
      rm(summaryDataCollection)
    }

    # if there is no information about the table tab in the global summary data, create an empty list for table tab
    if (  isolate(is.null(globals$summaryDataCollection[[tab]])  )  ) {
      globals$summaryDataCollection[[tab]] <<- list()
    }

    # if there is no information about the table tab grouped by group_vars, create an empty list for it
    if (  isolate(is.null(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(group_vars)]])  )  ) {
      globals$summaryDataCollection[[tab]][[jsonlite::toJSON(group_vars)]] <<- list()
    }

    # show messages (summaries will be created...)
    message("output$summary")
    showNotification("Summary will be updated...", type="message", duration=globals$notification_duration)

    # define sparklines (inline plots) as summary statistic functions (used in local data mode)
    sparkline_density <- function(x, chartRangeMinX=NULL, chartRangeMaxX=NULL, digits=0) {
      # no result if insufficient data points
      if (sum(!(is.na(x))) <2) return(NA)
      # compute density estimate
      dens <- density(x, na.rm=T);
      sparkline::spk_chr(as.numeric(dens$y),
                         chartRangeMinX=as.numeric(chartRangeMinX),
                         chartRangeMaxX=as.numeric(chartRangeMaxX),
                         tooltipFormat = paste0('x: {{x.',digits,'}} - density: {{y.',digits,'}}'),
                         xvalues=as.numeric(dens$x), minSpotColor=F, spotColor=F)}
    sparkline_boxplot <- function(x, minValue=NULL, maxValue=NULL, digits=0) {
      if (all(is.na(x))) return(NA)
      sparkline::spk_chr(x, type ='box',
                         chartRangeMin=as.numeric(minValue),
                         chartRangeMax=as.numeric(maxValue),
                         tooltipFormatFieldlist=c('med', 'lq', 'uq'),
                         tooltipFormat = paste0("{{field:fields}}: {{value.",digits,"}}") )}
    sparkline_pieplot <- function(x, classes=as.character(1:length(unique(x)))) {
      if (all(is.na(x))) return(NA)
      sparkline::spk_chr(as.numeric(table(x)), type ='pie',
                         tooltipFormatter = htmlwidgets::JS(
                           sprintf(
                             "function(sparkline, options, field){
                             console.log(sparkline); console.log(options); console.log(field); console.log(%s);
                                return %s[field.offset] + '<br/>' + field.value + ' ('+Math.round((field.percent + Number.EPSILON) * 100) / 100
+'%%)';
                              }",
                             jsonlite::toJSON( classes ),
                             jsonlite::toJSON( classes )
                           )
                         )
      )}
    sparkline_barplot <- function(x, classes=as.character(1:length(unique(x)))) {
      if (all(is.na(x))) return(NA)
      sparkline::spk_chr(as.numeric(table(x)), type ='bar',
                         tooltipFormatter = htmlwidgets::JS(
                           sprintf(
                             "function(sparkline, options, field){
                                return %s[field[0].offset] + '<br/>' + field[0].value;
                              }",
                             jsonlite::toJSON( classes )
                           )
                         ),
                         chartRangeMin=0
      )}

    # the summary statistic "quantiles" includes by our default the 5%, 25%, 50%, 75% and 95% quantiles
    if ("quantiles" %in% sum_stats_str) {
      split_pos <- which("quantiles"==sum_stats_str)[1]
      sum_stats_str <- c(head(sum_stats_str, split_pos-1),"5%","25%","50%","75%","95%",tail(sum_stats_str, -split_pos))
    }

    # filter summary statistic strings for either numeric or categorical summaries
    for_numeric_str <- function(stats_str) stats_str[stats_str %in% c("Nvalid","Nmissing","Ntotal","min","5%","25%","50%","median","75%","95%","max","Mean","sd","var","densplot","boxplot")]
    for_factor_str  <- function(stats_str) stats_str[stats_str %in% c("Nvalid","Nmissing","Ntotal","sparkline_pie","sparkline_bar","Ndistinct","mode","levels", "used_levels","max_group_n")]

    # get variable descriptions (for popover), variable labels and numeric status of the variables
    if (is.null(cur_conn)) {
      # local data mode
      the_table <- USER$data[[tabname]] %>% as.data.frame(check.names = FALSE)

      # get metadata
      extracted_metadata <- get_metadata_local(the_table)

      # which are numeric?
      are_numeric <- sapply(the_table, function(x) is.numeric(x))[mod_vars]

      rm(the_table)
    } else {
      # make sure we login again if login is lost
      if (do_global_login) tryCatch( {DSI::dsListWorkspaces(conns[[1]])}, error=function(e) {conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(conns)})
      tryCatch( {DSI::dsListWorkspaces(globals$conns[[1]])}, error=function(e) {globals$conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(globals$conns)})
      # development note: if connection is lost and tables have been pooled in the dashboard, the pooled tables have to be created again

      # DataSHIELD mode
      extracted_metadata <- extract_from_metadata_ds(tab,selected_vars)
      # which of the selected variables are numeric and which not?
      #   attention: length of "class" can be > 1! (e.g. avector and numeric)
      var_metadata <- globals$metaDataCollection[[tab]]
      are_numeric <- sapply(as.matrix(var_metadata[mod_vars,])[,"class"], function(x) is.null(x) || ( (length(x)==1 && is.na(x)) || "numeric" %in% x) )
    }

    # get label and description and digits
    var_lab <- extracted_metadata$alias
    var_desc <- extracted_metadata$description
    var_digits <- extracted_metadata$decimals
    var_levels <- extracted_metadata$levels

    # check for inapplicable summary selections
    if (all(are_numeric==TRUE)) {
      message("only numerical variables selected")
      if (length(for_numeric_str(sum_stats_str))<1)
        return(
          box(width=12, title = "Invalid summary selection", status = "warning", HTML(paste("The selected summaries (", paste0(sum_stats_str, collapse=", ") ,") are not applicable to <b>numerical</b> variables:", paste0(globals$selectedFtrs, collapse=", ")) ) )
        )
    } else if (all(are_numeric==FALSE)) {
      message("only categorical variables selected")
      if (length(for_factor_str(sum_stats_str))<1)
        return(
          box(width=12, title = "Invalid summary selection", status = "warning", HTML(paste("The selected summaries (", paste0(sum_stats_str, collapse=", ") ,") are not applicable to <b>categorical</b> variables:", paste0(globals$selectedFtrs, collapse=", ")) ) )
        )
    } else{
      message("numerical and categorical variables selected")
    }

    # add label and popover descriptions

    # function that relabels variables and adds a context sensitive popover
    # (for the summary table headers)
    label_variables <- function(xvec, from_values, to_values, to_popover_values=to_values, useLabels=F) {
      lapply(xvec, function(x) {
        idx <- which(x==from_values)
        if (length(idx)>0) {
          kableExtra::cell_spec(ifelse(useLabels,to_values[idx],x),
                                format = "html",
                                underline=F,color=blues9[8],extra_css="cursor: pointer;",
                                popover=spec_popover_delay(content=to_popover_values[idx],
                                                           trigger = c("click", "hover"),
                                                           title=NULL),
                                escape=F)
        } else {
          x
        } } )
    }

    if (is.null(cur_conn)){
      # local data mode

      # function for getting ranges for density plots or boxplots
      density_range <- function(x) range(density(x, na.rm=T)$x)

      # get ranges for selected numeric features (needed for summaries density and boxplot)
      var_range <- USER$data[[tabname]] %>%
        dplyr::select(dplyr::all_of(mod_vars)) %>%
        dplyr::select_if(is.numeric) %>%
        dplyr::reframe(across(everything(), density_range))
      if (dim(var_range)[1]==2) {
        range_left <- var_range[1,, drop=F] %>% unlist()
        range_right <- var_range[2,, drop=F] %>% unlist()
      }

      # get number of digits for all summary statistics
      digits <- c(
        unlist(sapply( rep(names(are_numeric)[are_numeric],  each=length(for_numeric_str(sum_stats_str))  ), function(x) var_digits[[x]]+extraDigits )), # sapply on NULL returns named list() instead of NULL -> unlist
        unlist(sapply( rep(names(are_numeric)[!are_numeric], each=length(for_factor_str(sum_stats_str))   ), function(x) 1)) # (1) return 1 (it would not work for kable with NA_integer or NULL) (2) sapply on NULL returns named list() instead of NULL -> unlist
      )

      # map the summary statistic strings to the corresponding functions
      # first a helper function that is used by several level based summary statistics
      level_update_with_spec <- function(x) {
        # if levels are just number starting with 1, try to update using the levels saved in the attributes
        if (identical(levels(x), as.character(1:length(levels(x))) ) ) {
          if (!is.null(attr(x,"spec"))) {
            spec_attributes <- jsonlite::fromJSON(attr(x,"spec"))
            if (!is.null(spec_attributes$levels)) {
              spec_levels <- unlist(jsonlite::fromJSON(spec_attributes$levels))
              levels(x) <- spec_levels[levels(x)]
            }
          }
        }
        return(x)
      }
      sum_stats_map <- list(min = function(x) ifelse(all(is.na(x)), NA, min(x,na.rm=T)),
                            max = function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T)),
                            Mean = function(x) ifelse(all(is.na(x)), NA, mean(x,na.rm=T)),
                            Nvalid=function(x) sum(!is.na(x)),
                            Nmissing = function(x) sum(is.na(x)),
                            Ntotal = function(x) length(x),
                            `5%`=function(x) as.numeric(quantile(x,probs=0.05,na.rm=T)),  # as.numeric necessary for kableExtra labels!
                            `10%`=function(x) as.numeric(quantile(x,probs=0.1,na.rm=T)),  # as.numeric necessary for kableExtra labels!
                            `25%`=function(x) as.numeric(quantile(x,probs=0.25,na.rm=T)), # as.numeric necessary for kableExtra labels!
                            `50%`=function(x) as.numeric(quantile(x,probs=0.5,na.rm=T)),  # as.numeric necessary for kableExtra labels!
                            median=function(x) as.numeric(quantile(x,probs=0.5,na.rm=T)), # as.numeric necessary for kableExtra labels!
                            `75%`=function(x) as.numeric(quantile(x,probs=0.75,na.rm=T)), # as.numeric necessary for kableExtra labels!
                            `90%`=function(x) as.numeric(quantile(x,probs=0.9,na.rm=T)),  # as.numeric necessary for kableExtra labels!
                            `95%`=function(x) as.numeric(quantile(x,probs=0.95,na.rm=T)), # as.numeric necessary for kableExtra labels!
                            sd = function(x) sd(x,na.rm=T),
                            var = function(x) var(x,na.rm=T),
                            densplot = ~ sparkline_density (.x, range_left[dplyr::cur_column()], range_right[dplyr::cur_column()], digits=1),
                            `boxplot` = ~ sparkline_boxplot (.x, range_left[dplyr::cur_column()], range_right[dplyr::cur_column()], digits=1),
                            sparkline_pie = ~ sparkline_pieplot (.x,  xtable::sanitize(levels(.x), type='html') ),
                            sparkline_bar = ~ sparkline_barplot(.x, xtable::sanitize(levels(.x), type='html') ),
                            mode = function(x) {
                              if (all(is.na(x))) return(NA)
                              x <- level_update_with_spec(x)
                              ux <- levels(x)
                              tab <- tabulate(match(x, ux))
                              paste(ux[tab == max(tab)], collapse=", ")
                            },
                            levels = function(x) {
                              x <- level_update_with_spec(x)
                              as.character(jsonlite::toJSON(xtable::sanitize(levels(x), type='html')))
                            },
                            Ndistinct = function(x) dplyr::n_distinct(x, na.rm=T),
                            used_levels = function(x) {
                              if (all(is.na(x))) return(NA)
                              x <- level_update_with_spec(x)
                              as.character(jsonlite::toJSON(xtable::sanitize(levels(x)[levels(x) %in% unique(x)], type='html')))
                            },
                            max_group_n = function(x) {
                              x <- level_update_with_spec(x)
                              ux <- levels(x)
                              tab <- tabulate(match(x, ux))
                              max(tab)
                            }
      )

      # save the functions for the selected summary statistics in variable
      sum_stats <- sum_stats_map[sum_stats_str]
      sum_stats_factor <- sum_stats_map[for_factor_str(sum_stats_str)]
      sum_stats_numeric <- sum_stats_map[for_numeric_str(sum_stats_str)]

      # create data frame of the requested data
      df <- USER$data[[tabname]] %>%
        dplyr::select(dplyr::all_of(c(mod_vars,group_vars))) %>%
        dplyr::group_by(across(all_of(group_vars))) %>%
        dplyr::summarise(across(names(are_numeric)[are_numeric], sum_stats_numeric,
                                # new column names will be a JSON string with variable as first element and summary statistic as second element
                                .names = "{mapply(function(col,fun) jsonlite::toJSON(c(fun,col), auto_unbox = TRUE), .col, .fn)}"),
                         across(names(are_numeric)[!are_numeric], sum_stats_factor,
                                # new column names will be a JSON string with variable as first element and summary statistic as second element
                                .names = "{mapply(function(col,fun) jsonlite::toJSON(c(fun,col), auto_unbox = TRUE), .col, .fn)}"),
                         .groups = 'drop')%>%
        mutate(across(all_of(group_vars), ~ var_levels[[cur_column()]][.]))  # replace the numeric level labels in the group variables with their metadata levels

      # rename also the group variable columns, if present
      existing_group_vars <- intersect(group_vars, df %>% names(.))
      if (length(existing_group_vars) > 0)
        df <- df %>% dplyr::rename_with(~ sapply(as.vector(existing_group_vars), function(x) as.character(jsonlite::toJSON(rep(x,2)))), all_of(existing_group_vars))

      # escape/sanitize data
      # (but not:
      #  - the column names, because we have to use escape=F in kable!
      #  - sparkline plots (i.e. don't sanitize html code)
      #  - numeric feature values
      # )
      is_html_or_num <- sapply(df,function(x) (is.numeric(x) ||  "html" %in% class(x)))
      df[,!is_html_or_num] <- lapply(df[,!is_html_or_num], function(x) xtable::sanitize(x, type='html'))

      # order by summary statistics sum_stats_str
      df <- df[  order( match( sapply(colnames(df), function(x) jsonlite::fromJSON(x)[1] ), sum_stats_str ) ) ]
      # order by selected_vars
      df <- df[ order( match( sapply(colnames(df), function(x) jsonlite::fromJSON(x)[2] ), selected_vars))]
    }  else {
      # DataSHIELD mode
      # summary data already present will be used
      # only missing summaries will be created and saved as JSON to globals$summaryDataCollection
      # 1. Check if there is a summary  collection
      # 2. Are there data for the selected table tab and the selected groups group_vars?
      # if summary data available for table tab and groups group_vars, then load it to variable grp_summ_stats
      if (is.null(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(group_vars)]]) ||
          length(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(group_vars)]]) == 0)
        grp_summ_stats <- NULL
      else
        grp_summ_stats <- jsonlite::fromJSON(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(group_vars)]])

      # request summaries for mod_vars, give function all_summaries the data already available (parameter current_data)
      isolate(summ_stats1 <- dsDashboard::dsGapply(tabsymbol, group_vars, all_summaries,
                                      vars=mod_vars, summaries=sum_stats_str,
                                      current_data=grp_summ_stats, datasources=cur_conn, lazy=T)  )

      # get global ranges for each numeric variable (necessary for comparison of boxplots and density plots over groups)
      if ("boxplot" %in% sum_stats_str | "densplot" %in% sum_stats_str) {
        isolate({
          # which of the successfully requested variables are numeric?
          numericVars <- unique(summ_stats1[summ_stats1$feature=="type" & summ_stats1$value=="numeric",]$variable)
          # get their ranges
          globalRanges <- lapply(numericVars, function(x){
            c(suppressWarnings(min(summ_stats1[summ_stats1$variable==x & summ_stats1$feature %in% c("min","ymin"),]$value,na.rm = T)), # works also for NaN
              suppressWarnings(max(summ_stats1[summ_stats1$variable==x & summ_stats1$feature %in% c("max","ymax"),]$value,na.rm = T))
            )
            # warnings for "no non-missing argument" in case of only NA or NaN in min/max are suppressed
          }) %>% setNames(numericVars)
        })
      }

      # create boxplot sparklines with common range for each group level combination
      if ("boxplot" %in% sum_stats_str) {
        # variables with boxplot data
        vars_with_boxplot <- summ_stats1 %>% dplyr::filter(variable %in% mod_vars, feature=="lower") %>% dplyr::pull(variable)
        # variables with boxplot data and missing sparklines
        vars_with_missing_sparkline <- setdiff(vars_with_boxplot,
                                               summ_stats1 %>% dplyr::filter(variable %in% vars_with_boxplot, feature=="boxplot") %>% dplyr::pull(variable))

        if (length(vars_with_missing_sparkline) > 0) {
          # create boxplot-sparklines - considering the global ranges
          message("boxplot is being created as summary")
          boxplotlines <- summ_stats1 %>% dplyr::filter(variable %in% vars_with_missing_sparkline) %>%
            dplyr::group_by(dplyr::across(-c("feature","value"))) %>% dplyr::summarise(boxplot = (function(feat,val,variable) {
              # continue only for numeric data
              type <- val[which(feat=="type")]
              if (type != "numeric") return(NA)

              # collect boxplot data
              ymin <- val[which(feat=="ymin")]
              lower <- val[which(feat=="lower")]
              middle <- val[which(feat=="middle")]
              upper <- val[which(feat=="upper")]
              ymax <- val[which(feat=="ymax")]

              # global ranges for variable ("variable" is a vector which contains the same variable name for each feature in summ_stats1)
              chartRangeMin <- as.numeric(globalRanges[variable[1]][[1]][1])
              chartRangeMax <- as.numeric(globalRanges[variable[1]][[1]][2])

              # get number of digits for current variable ("variable" is a vector which contains the same variable name for each feature in summ_stats1)
              current_digits <- var_digits[variable[1]]

              # if a boxplot feature is missing, do not return a boxplot
              if(any(sapply(c(ymin,lower,middle,upper,ymax),is.na)))
                return(NA)
              else
                # create sparkline
                sparkline::spk_chr(c(ymin,lower,middle,upper,ymax),
                                   type = 'box', raw = TRUE, showOutliers = FALSE,
                                   chartRangeMin=as.numeric(chartRangeMin),
                                   chartRangeMax=as.numeric(chartRangeMax),
                                   minValue=as.numeric(chartRangeMin),
                                   maxValue=as.numeric(chartRangeMax),
                                   tooltipFormatFieldlist=c('lw', 'lq', 'med', 'uq', 'rw'),
                                   tooltipFormat =  paste0("{{field:fields}}: {{value.",current_digits,"}}")  ) # no extraDigits in sparkline (as they are created once and then saved, considering extraDigits would require to change the html sparkline code before output)
            })(feature,value,variable), .groups = 'drop')  %>%
            tidyr::drop_na(boxplot) %>% dplyr::mutate(feature = "boxplot") %>%
            dplyr::rename(value="boxplot")  %>% dplyr::mutate(value = as.character(value))
          # add boxplot-sparklines to summ_stats1
          summ_stats1 <- summ_stats1 %>% dplyr::bind_rows(boxplotlines)
        }
      }

      # create density sparklines with common range for each group level combination
      if ("densplot" %in% sum_stats_str) {
        # variables with density plot data
        vars_with_density <- summ_stats1 %>% dplyr::filter(variable %in% mod_vars, feature=="density") %>% dplyr::pull(variable)
        # variables with density plot data and missing sparklines
        vars_with_missing_sparkline <- setdiff(vars_with_density,
                                               summ_stats1 %>% dplyr::filter(variable %in% vars_with_density, feature=="densplot") %>% dplyr::pull(variable))

        if (length(vars_with_missing_sparkline) > 0) {
          # create density-sparklines - considering the global ranges
          message("density plot is being created as summary")
          densplotlines <- summ_stats1 %>% dplyr::filter(variable %in% vars_with_missing_sparkline) %>%
            dplyr::group_by(dplyr::across(-c("feature","value"))) %>% dplyr::summarise(densplot = (function(feat,val,variable) {
              # continue only for numeric data
              type <- val[which(feat=="type")]
              if (type != "numeric") return(NA)

              # if data is missing, do not return a boxplot
              if (!("mids" %in% feat) || is.null(val[which(feat=="mids")]) || is.na(val[which(feat=="mids")])) return(NA)

              # use global ranges for the variable
              chartRangeMinX <- as.numeric(globalRanges[variable[1]][[1]][1])
              chartRangeMaxX <- as.numeric(globalRanges[variable[1]][[1]][2])

              # histogram data is used to create density plot data with appropriate bandwidth
              mids <- jsonlite::fromJSON(val[which(feat=="mids")])
              counts <- jsonlite::fromJSON(val[which(feat=="counts")])
              dummy_data <- rep(mids,counts)
              hist_bins <- length(mids)

              if (is.infinite(chartRangeMinX)) {
                warning(paste("Infinite minimum in densplot for",variable[1]))
                chartRangeMinX <- min(mids, na.rm=T)
              }
              if (is.infinite(chartRangeMaxX)) {
                warning(paste("Infinite maximum in densplot for",variable[1]))
                chartRangeMaxX <- max(mids, na.rm=T)
              }

              # density estimator based on dummy_data
              dens <- density(dummy_data, bw=(chartRangeMaxX-chartRangeMinX)/hist_bins, from=chartRangeMinX, to=chartRangeMaxX, na.rm=T)

              # get number of digits for current variable ("variable" is a vector which contains the same variable name for each feature in summ_stats1)
              current_digits <- var_digits[variable[1]]

              # create sparkline
              sparkline::spk_chr(as.numeric(dens$y),
                                 chartRangeMinX=as.numeric(chartRangeMinX),
                                 chartRangeMaxX=as.numeric(chartRangeMaxX),
                                 tooltipFormat = paste0('x: {{x.',current_digits,'}} - density: {{y.',1,'}}'), # current_digits will be increased by extraDigits further below in each sparkline
                                 xvalues=as.numeric(dens$x), minSpotColor=F, spotColor=F)
            })(feature,value,variable), .groups = 'drop')  %>%
            tidyr::drop_na(densplot) %>% dplyr::mutate(feature = "densplot") %>%
            dplyr::rename(value="densplot")  %>% dplyr::mutate(value = as.character(value))
          # add density-sparklines to summ_stats1
          summ_stats1 <- summ_stats1 %>% dplyr::bind_rows(densplotlines)
        }
      }

      # save summaries in json format
      new_json <- summ_stats1 %>% jsonlite::toJSON()
      # hint: no data should be lost, because current_data were given to function all_summaries
      isolate( globals$summaryDataCollection[[tab]][[jsonlite::toJSON(group_vars)]] <<- new_json )

      # build up filename for cache file containing variable names for table tab
      mode_str <- ifelse(USER$login_mode==0,"",paste0("mode",USER$login_mode))
      summary_path <- file.path(cache_path,paste0(summary_prefix,mode_str,".rds"))

      # promise to save it back  (... happens also if nothing has been changed)
      promises::future_promise({
        isolate(  summaryDataCollection <- globals$summaryDataCollection  ) # step necessary (save cannot access globals)
        saveRDS(summaryDataCollection, file=summary_path)
        message("summaryDataCollection saved in promise")
      })

      # unlock the screen
      removeModal();

      # quantiles selector select the defined default quantiles
      if ("quantiles" %in% sum_stats_str) sum_stats_str <- append(sum_stats_str, c("5%", "10%", "25%","50%", "75%", "90%", "95%"), after=which("quantiles"==sum_stats_str)[1])

      # filter for feature variable and selected summary statistics
      summ_stats <- summ_stats1[summ_stats1$variable %in% selected_vars & summ_stats1$feature %in% sum_stats_str,]

      # some variables might be missing, because the chosen statistics are missing (due to DataSHIELD)
      # therefore check which ones are available
      # also the group variables are filtered out (as they are not in summ_stats$variable)
      avail_vars <- unique(selected_vars[selected_vars %in% unique(summ_stats$variable)])

      # order by summary statistics sum_stat_str_mapping[sum_stats_str]
      summ_stats <- summ_stats[  order( match( summ_stats$feature, sum_stats_str ) ) , ]
      # order by avail_vars
      summ_stats <- summ_stats[order(match(summ_stats$variable, avail_vars)), ]
      # remove column "tabname"
      summ_stats <- summ_stats[,!names(summ_stats) %in% "tabname"]
      # Get level names for group variables from metadata spec attribute, if available
      lev_mapping1 <- extracted_metadata$levels[group_vars]

      # in summ_stats replace numeric factor level codes by level names from metadata
      for (gv in group_vars) {
        if (is.null(lev_mapping1[[gv]]))
          warning(paste0("lev_mapping1 has no entry for ", gv,"!"))
        else
          summ_stats[[gv]] <- lev_mapping1[[gv]][summ_stats[[gv]]]
      }

      # format "levels"/"used_levels"/"mode" summaries from JSON to comma separated string
      row_index <- summ_stats$feature %in% c("levels","used_levels","mode")
      if (!is.null(summ_stats[row_index,"value"]) &&
          length(summ_stats[row_index,"value"])>0 &&
          !(is.data.frame(summ_stats[row_index,"value",drop=F]) && nrow(summ_stats[row_index,"value",drop=F])==0)) {
        summ_stats[row_index,"value"] <- sapply(summ_stats[row_index,"value"],
                                                function(x)
                                                  tryCatch( {
                                                    paste(jsonlite::fromJSON(x),collapse=", ")
                                                  }, error = function(e) {
                                                    x
                                                  })
        ) %>% unname()
      }

      # make sure to show (only) the desired number of digits (for float variables)
      # select indices of numeric values with numeric summary statistics
      selected_ind <- !is.na( as.numeric(summ_stats$value) )
      # known integer summaries and other non-numeric summaries
      not_float_summ <- c("Ntotal","Nmissing","densplot","boxplot","Ndistinct","mode",
                          "Nvalid", "max_group_n", "used_levels")
      # decimal_summ <- c("Mean","max","min","5%","25%","50%","75%","95%","median","sd","var")
      selected_ind <- selected_ind & !(summ_stats$feature %in% not_float_summ)
      # save decimals for all summaries (also not selected summaries) of the variables in "digits"
      digits <- sapply(setNames(globals$metaDataCollection[[tab]][summ_stats[["variable"]],"spec"],
                                summ_stats[["variable"]]),
                       function(x) jsonlite::fromJSON(x)$decimals) + extraDigits

      # round the summary statistics (although rounding is also performed in kable...)
      if (sum(selected_ind)>0) summ_stats[selected_ind,"value"] <- prettyNum(round(as.numeric(summ_stats[["value"]][selected_ind]) ,
                                                                                   digits=digits[selected_ind]
      ))

      # transform table to the final shape (wide):
      # build column name in JSON format to keep features and variables per column in a structured way
      # then make the table wider and drop feature and variable
      df <- summ_stats %>%
        dplyr::mutate(variable_feature_JSON = mapply(
          function(f, v) jsonlite::toJSON(list(f, v), auto_unbox = TRUE),
          feature,
          variable)) %>%
        dplyr::select(-c(variable,feature)) %>%
        tidyr::pivot_wider(names_from = variable_feature_JSON,
                           values_from = c(value))
    } # end: if DataSHIELD mode

    # get names for two headers separate from the column labels (JSON)
    head1_raw <- sapply(colnames(df), function(x) if (jsonlite::validate(x)) jsonlite::fromJSON(x)[2] else x) %>% unname()
    head2 <- sapply(colnames(df), function(x) if (jsonlite::validate(x)) jsonlite::fromJSON(x)[1] else x) %>% unname()

    # adjust header 1 (= header with variable labels)
    head1_rle <- rle(head1_raw)
    head1 <- head1_rle$lengths
    names(head1) <- head1_rle$values

    # build kable headers
    names(head1) <- c(rep(" ",length(group_vars)), # space is necessary
                      kableExtra::text_spec(underline=F,
                                            color=blues9[8],
                                            extra_css="cursor: pointer;",
                                            if (USER$useLabels) var_lab[mod_vars] else mod_vars , # don't sanitize here! (don't replace &, has to be kept e.g. in the mu sign &+181;)
                                            popover = spec_popover_delay(
                                              content = xtable::sanitize(gsub('"','\'',gsub('[\r]','', gsub('[\n]','<br/>',var_desc[mod_vars]))), type='html'),
                                              trigger = c("click", "hover"),
                                              title = NULL),
                                            escape = T) # tricky: consider that there are column labels with "&" sign!
    )

    # label the second header (summary statistics and group variables)
      tab_head_names <- head2 %>%
        # this labels the summaries with a context sensitive popover
        label_variables(from_values = summary_names,
                        to_values = summary_labels,
                        to_popover_values = summary_descriptions,
                        useLabels = USER$useLabels) %>%
        # label the group variables with their name and a context sensitive popover
        label_variables(from_values = selected_vars,
                        to_values = var_lab[selected_vars],
                        to_popover_values = var_desc[selected_vars],
                        useLabels = USER$useLabels)

    # replace number of digits in the sparkline plots
    df_digits <- sapply(setNames(globals$metaDataCollection[[tab]][head1_raw,"spec"],
                    head1_raw),
           function(x) jsonlite::fromJSON(x)$decimals)
    df <- mapply(function(x, digits) {
      if ("html" %in% class(x)) {
        stringr::str_replace_all(
          x,
          '"tooltipFormat":"(.*?)\\b(value|x|y)\\.0(.*?)"',
          sprintf('"tooltipFormat":"\\1\\2.%s\\3"', digits)  # Insert `digits` dynamically
        )
      } else {
        x
      }
    }, df, df_digits + extraDigits + 2, SIMPLIFY = F)  %>% as_tibble()

    # create the summary table with kable
    model_tab <- df %>% kableExtra::kbl(escape=F, format = "html", digits=digits+extraDigits, col.names = tab_head_names) %>%
      kableExtra::kable_styling()   %>%
      kableExtra::add_header_above(head1, escape=F)

    # show group levels bold; if subsequent group levels are identical, print only the first appearance
    if (length(group_vars)>=1) model_tab <- model_tab %>%
      kableExtra::column_spec(1:length(group_vars), bold = T) %>%
      kableExtra::collapse_rows(columns = 1:max(1,(length(group_vars)-1)), valign = "top")

    # output box
    ## configure the popovers javascript code
    ## - to make the html work inside the popover, we set "html: true" in the <script>
    ## - show on mouseover or click, hide after waiting time or by click
    box(title = "Summary statistics", width=12, style = "overflow-x: auto",
        HTML('<script>$(document).ready(function(){ $(\'[data-toggle="popover"]\').popover({html: true}); });
                $(\'[data-toggle=popover]\').popover({
                    html: true,
                    trigger: \"manual\",
                    content: function () {
                        return $(\'#content\').html();
                  }
                }).on(\"mouseenter\", function() {
                  var _this = this;
                  $(this).popover(\"show\");
                }).on(\"mouseleave\", function() {
                  var _this = this;
                  setTimeout(function() {
                    if (!$(\".popover:hover\").length) {
                      $(_this).popover(\"hide\");
                    }
                  }, 2000);
                }).click(function (e) {
                        $(\'[data-toggle=popover]\').not(this).popover(\'hide\');
                        });
                $(document).click(function (e) {
                    if (($(".popover").has(e.target).length == 0) || $(e.target).is(".close")) {
                        $(".popover").popover("hide");
                  }
                });
              </script>',
             model_tab)) %>%
      sparkline::spk_add_deps()
  })

  # output returns if a model for the selected variables is available - if FALSE, a warning is shown
  output$modFitAvailable <- reactive({
    globals$validModDepVar == TRUE
  })
  outputOptions(output, "modFitAvailable", suspendWhenHidden = FALSE)

  # output returns if a model for the selected variables can be created - otherwise a warning is shown
  output$modFitError <- reactive({
    globals$modFitError == TRUE
  })
  outputOptions(output, "modFitError", suspendWhenHidden = FALSE)

  output$modFit <- renderUI({
    # require the model tab is active
    req(input$mainTabs == "modFit")

    # require login and at least two selected variables
    req(USER$login,
        length(globals$selectedFtrs)+length(globals$selectedGrps) > 1)

    globals$validModDepVar <- TRUE
    globals$modFitError <- FALSE

    # currently selected table
    tab <- input$inpTableSelect
    # get "number" of the current table
    tabnr <- readr::parse_number(tab)
    # get symbol used for the table tab
    tabsymbol <- globals$allTables[tabnr,"symbol"]
    # get table name
    tabname <- globals$allTables[tabnr,".x"]
    # get connection for current table
    cur_conns <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables

    # table selection must not mismatch the selection saved in globals
    req(length(globals$selectedTable)>0 && globals$selectedTable==tab)

    if (input$debugmode) browser()

    # in DataSHIELD mode: make sure we login again if login is lost
    if (!is.null(cur_conns)) {
      if (do_global_login) tryCatch( {DSI::dsListWorkspaces(conns[[1]])}, error=function(e) {conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(conns)})
      tryCatch( {DSI::dsListWorkspaces(globals$conns[[1]])}, error=function(e) {globals$conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(globals$conns)})
      # development note: if connection is lost and tables have been pooled in the dashboard, the pooled tables have to be created again
    }

    # define variables (features, groups, active selection)
    mod_vars <- c(globals$selectedFtrs)
    group_vars <- c(globals$selectedGrps)

    # get name and object name for dependent variable
    dep_var <- mod_vars[1]

    # create structure to keep relevant results
    mod_result <- list()

    # in DataShield mode: create boolean factors for variables
    if (!is.null(cur_conns)){
      check_vars <- if (length(mod_vars)==0) c() else paste0(tabsymbol,"$",mod_vars)
      for (vname in check_vars) {
        if ("factor" %in% unlist(ds.class(vname, datasources=cur_conns)) ) dsDashboard::createFactorVars(vname, datasources=cur_conns)
      }
    }

    # build model formula...
    # ... for binary/ordinal regression without mixed effects
    formula_glm   <- paste(formulaic::add.backtick(dep_var), "~", paste(formulaic::add.backtick(mod_vars[-1]), collapse=" + ") )
    # ... for mixed-effects model
    formula_glmer <- paste(formulaic::add.backtick(dep_var), "~", paste0(paste(formulaic::add.backtick(mod_vars[-1]), collapse=" + "),paste0(ifelse(length(mod_vars[-1])>0," + ",""),paste0("(1 | ",formulaic::add.backtick(group_vars),")", collapse=" + ")))  )
    # choose the formula, that is required for the current request:
    mod_formula <- if (length(group_vars)>0) formula_glmer else formula_glm

    # in DataSHIELD mode, additionally get formula with complete object names
    if (!is.null(cur_conns)) {
      # DataSHIELD mode

      # variable object names
      dep_var_obj <- paste0(tabsymbol,"$",dep_var)
      mod_vars_obj <- paste0(tabsymbol,"$",mod_vars)
      group_vars_obj <- if (length(group_vars)==0) character() else paste0(tabsymbol,"$",group_vars)

      # ... for binary/ordinal regression without mixed effects
      formula_glm_obj   <- paste(dep_var_obj, "~", paste(mod_vars_obj[-1], collapse=" + ") )
      # ... for mixed-effects model
      formula_glmer_obj <- paste(dep_var_obj, "~", paste0(paste(mod_vars_obj[-1], collapse=" + "),paste0(ifelse(length(mod_vars_obj[-1])>0," + ",""),paste0("(1 | ",group_vars_obj,")", collapse=" + ")))  )
      # choose the formula, that we need now:
      mod_formula_obj <- if (length(group_vars_obj)>0) formula_glmer_obj else formula_glm_obj
    }

    # get class of dependent variable
    var_class <- if (is.null(cur_conns)) {
     the_data <- data.frame(USER$data[[tabname]], check.names = FALSE)
     class(the_data[,dep_var])
    } else {
     ds.class(dep_var_obj, cur_conns)
    }

    if ("factor" %in% unlist(var_class)) {
      message("dependent factor variable")

      # get number of factor levels
      if (is.null(cur_conns)) {
        # local mode
        n_levels <- length(levels(the_data[,dep_var]))
      } else {
        # DataSHIELD mode
        n_levels <- dim(dsDashboard::dsCatAgg(dep_var_obj, cur_conns))[1]
      }

      # if else to switch the following cases
      # 1.) binomial, no mixed effects
      # 2.) multinomial, no mixed effects (only Poisson regression is implemented)
      # 3.) mixed effects model (binomial or Poisson regression)
      if (n_levels == 2 && length(group_vars)==0) {
        # binomial regression => glm functions can be used
        message("binomial dependant variable ; no mixed effects")

        mod_result$formula <- mod_formula
        mod_result$model_type_string <- "Binomial regression model"

        # fit the model
        if (is.null(cur_conns)) {
          mod_fit <- glm(formula = mod_formula, family = "binomial",
                         data = the_data)
        } else {
          tryCatch( {
            mod_fit <- dsDashboard::dsGLM(formula = mod_formula_obj,
                                              family = "binomial", datasources = cur_conns, add_tab_to_depvar=F)
            globals$modFitError <<- FALSE
          },
          error=function(e) {
            # error happens e.g. if one datasource contains only one level of an independent variable
            # (error in dsBaseClient::ds.glm: contrasts need factors with 2 or more levels)
            globals$modFitError <<- TRUE
            req(FALSE)
          })

          class(mod_fit) <- "glm"
          mod_fit$call <- mod_fit$formula
          mod_fit$deviance <- mod_fit$dev # required later in format(signif(mod_fit$deviance, max(3L,getOption("digits")-3L)))
        }
      } else if (n_levels > 2 && length(group_vars)==0) {
        message("ordinal dependant variable, > 2 factor levels; no mixed effects")

        # is the factor ordered?
        if (("ordered" %in% unlist(var_class))) {
          # Poisson regression
          message("=> Poisson regression")

          mod_result$formula <- mod_formula
          mod_result$model_type_string <- "Poisson regression model"

          if (is.null(cur_conns)) {
            # local mode
            asnumeric <- as.numeric(as.character(the_data[[dep_var]]))
            if (sum(!is.na(asnumeric))<3) {
              warning("not enough non-NAs - are dependent variable levels not positive **integers**?")
              the_data <- NULL
              return()
            } else if (sum(asnumeric<0, na.rm=T)>0) {
              warning("negative values! - are dependent variable levels not **positive** integers?")
              the_data <- NULL
              return()
            } else {
              the_data[[dep_var]] <- asnumeric
              mod_fit <- glm(formula = mod_formula, family = "poisson",
                             data = the_data)
            }
          } else {
            # DataSHIELD mode
            tryCatch( {
              mod_fit <- dsDashboard::dsGLM(formula = mod_formula_obj,
                                                family = "poisson",
                                                datasources = cur_conns, add_tab_to_depvar=F)
              globals$modFitError <<- FALSE
            },
            error=function(e) {
              # error happens e.g. if one datasource contains only one level of an independent variable
              # (error in dsBaseClient::ds.glm/dsBase::glmDS1: contrasts need factors with 2 or more levels)
              globals$modFitError <<- TRUE
              req(FALSE)
            })

            class(mod_fit) <- "glm"
            mod_fit$call <- mod_fit$formula
            mod_fit$deviance <- mod_fit$dev # required in format(signif(mod_fit$deviance, max(3L,getOption("digits")-3L)))
          }
        } else {
          warning("no regression model for unordered multi factor dependent variables implemented")
          isolate({globals$validModDepVar <- FALSE })
          req(FALSE)
        }
      } else if (length(group_vars)>0) {
        # glmer model
        message("model with mixed effects")

        if (n_levels==2) {
          # binomial mixed-effects model

          if (is.null(cur_conns)) {
            # local mode

            # fit the mixed-effects model
            mod_fit <- lme4::glmer(formula = mod_formula, family = "binomial",
                                   data = the_data)
          } else {
            # DataSHIELD mode, mixed-effects model

            # build the dependent variable name of the binary dummy created above by createFactorVars
            dep_var_binary <- paste0(dep_var,"2")

            mod_formula_replaced <- sub(paste0("^", dep_var), dep_var_binary, mod_formula)
            #mod_formula_replaced2 <- sub(paste0("^",tabsymbol,"\\$",dep_var), dep_var_binary, mod_formula_obj)

            tryCatch( {
              # ds.glmerSLMA is currently disfunct in the case that complete object names with table$ are given
              # therefore define it with data parameter
              mod_fit <- ds.glmerSLMA(formula = mod_formula_replaced,
                                      data = tabsymbol,
                                      family = "binomial",
                                      datasources = cur_conns )
              # the creators of ds.glmerSLMA forgot to name the lines of the pooled estimates matrix
              # (at least in the case of only one study this error occurs)
              # lets just copy the coefficient names from sematrix.valid
              rownames(mod_fit$SLMA.pooled.ests.matrix) <- names(mod_fit$sematrix.valid)

              globals$modFitError <<- FALSE
            },
            error=function(e) {
              # error happens e.g. if one datasource contains only one level of an independent variable
              # (error in dsBaseClient::ds.glm/dsBase::glmDS1: contrasts need factors with 2 or more levels)
              globals$modFitError <<- TRUE
              req(FALSE)
            })

            class(mod_fit) <- "ds.glmerSLMA"
            mod_fit$call <- mod_fit$formula
          }
        } else if (("ordered" %in% unlist(var_class))) {
          # Poisson regression with mixed-effects"

          # fit the Poisson model with mixed-effects
          if (is.null(cur_conns)) {
            # local mode
            # convert dependent ordinal factor to numeric
            asnumeric <- as.numeric(as.character(the_data[[dep_var]]))

            # check for issues
            if (sum(!is.na(asnumeric))<3) {
              msg <- "not enough non-NAs in dependent variable - check variable format"
              warning(msg)
              showNotification(msg, duration=globals$notification_duration, type="warning")
              req(FALSE)
            } else if (sum(asnumeric<0, na.rm=T)>0) {
              msg <- "negative values in dependent variable - check variable format"
              warning(msg)
              showNotification(msg, duration=globals$notification_duration, type="warning")
              req(FALSE)
            } else {
              the_data[[dep_var]] <- asnumeric
              mod_fit <- lme4::glmer(formula = formula_glmer, family = "poisson",
                                     data = the_data)
            }
          } else {
            # DataSHIELD mode
            # convert dependent ordinal factor to numeric
            ds.asNumeric(x.name = paste0(tabsymbol,"$",dep_var),
                         newobj = "glmPoissonY",
                         datasources = cur_conns)

            mod_formula_replaced <- sub(paste0("^", dep_var), "glmPoissonY", mod_formula)
            #mod_formula_replaced2 <- sub(paste0("^",tabsymbol,"\\$",dep_var), "glmPoissonY", mod_formula_obj)

            tryCatch( {
              # ds.glmerSLMA is currently disfunct in the case that complete object names with table$ are given
              # (especially the error occurs if the mixed effect variable is given as complete object name, e.g. table$var)
              # therefore define formula with data parameter
              mod_fit <- ds.glmerSLMA(formula = mod_formula_replaced,
                                      data = tabsymbol,
                                      family = "poisson",
                                      datasources = cur_conns)
              # the creators of ds.glmerSLMA forgot to name the lines of of the pooled estimates matrix
              # (at least in the case of only one study this error occurs)
              # lets just copy the coefficient names from sematrix.valid
              rownames(mod_fit$SLMA.pooled.ests.matrix) <- names(mod_fit$sematrix.valid)
              globals$modFitError <<- FALSE
            },
            error=function(e) {
              # error happens e.g. if one datasource contains only one level of an independent variable
              # (error in dsBaseClient::ds.glm/dsBase::glmDS1: contrasts need factors with 2 or more levels)
              globals$modFitError <<- TRUE
              req(FALSE)
            })

            class(mod_fit) <- "ds.glmerSLMA"
            mod_fit$call <- mod_fit$output.summary[[1]]$call #mod_fit$formula
          }
        } else {
          warning("no regression model for unordered multi factor dependent variables implemented")
          isolate({globals$validModDepVar <- FALSE })
          isolate({globals$modFitError <- FALSE})
          req(FALSE)
        }
      }
    } else {
      # treat dependent variable as numeric
      message("numeric dependant variable")

      # check: are groups selected (for mixed effects)?
      if (length(group_vars)==0) {
        # no mixed effects: ordinary linear model "lm"
        mod_result$formula <- mod_formula
        mod_result$model_type_string <- "Linear regression model"

        # fit linear model
        if (is.null(cur_conns)) {
          # local mode
          mod_fit <- glm(formula = mod_formula,
                         data = data.frame(USER$data[[tabname]], check.names = FALSE))
        } else {
          # DatasHIELD mode
          tryCatch( {
            mod_fit <- dsDashboard::dsGLM(formula = mod_formula_obj, add_tab_to_depvar = F,
                                              family = "gaussian",
                                              datasources = cur_conns)
            globals$modFitError <<- FALSE
          },
          error=function(e) {
            # error happens e.g. if one datasource contains only one level of an independent variable
            # (error in dsBaseClient::ds.glm/dsBase::glmDS1: contrasts need factors with 2 or more levels)
            globals$modFitError <<- TRUE
            req(FALSE)
          })
        }
      } else {
        message("linear mixed effects model")

        # fit linear mixed-effects model
        if (is.null(cur_conns)) {
          # local mode
          mod_fit <- lme4::lmer(formula = mod_formula,
                                data = data.frame(USER$data[[tabname]], check.names = FALSE))
        } else {
          # DataSHIELD mode
          tryCatch( {
            mod_fit <- ds.lmerSLMA(formula = mod_formula,
                                   dataName = tabsymbol,
                                   datasources = cur_conns)
            # the creators of ds.lmerSLMA forgot to name the lines of the
            # "pooled estimates matrix" in case of only one study (naming works wore than one datasources are defined)
            # lets just copy the coefficient names from sematrix.valid
            rownames(mod_fit$SLMA.pooled.ests.matrix) <- names(mod_fit$sematrix.valid)

            globals$modFitError <<- FALSE
            },
            error=function(e) {
              # error happens e.g. if one datasource contains only one level of an independent variable
              # (error in dsBaseClient::ds.lmerSLMA/dsBase::lmerSLMADS2: contrasts need factors with 2 or more levels)
              globals$modFitError <<- TRUE
              req(FALSE)
          })
        }
      }
    }

    # collect data for output
    isLmer <- max(class(mod_fit)=="lmerMod")
    isglmer <- max(class(mod_fit)=="glmerMod")
    isDSmer <- !(isS4(mod_fit)) && class(mod_fit$output.summary[[1]])=="summary.merMod" # S4 classes  like glmer do not have "$" slots

    # save necessary data in variable mod_result
    if (length(group_vars)==0) {
      # for lm and glm models... (no mixed effects)
      mod_result$call_string <- paste0("\nCall:\n", paste(deparse(mod_fit$call), sep = "\n",
                                                          collapse = "\n"), "\n\n", sep = "")
      mod_result$family_string <-  paste0("Family: ", mod_fit$family$family, "\n<br>",
                                          "Link function: ", mod_fit$family$link, "\n\n")
      mod_result$na_count <- if (!is.null(mod_fit$Nmissing))
        mod_fit$Nmissing # (if DS mode for (generalized) linear model)
      else
        length(mod_fit$na.action)

      # from summary
      if (is.null(cur_conns)) {
        mod_fit_summary_lm <- summary(mod_fit)
        mod_result$summary_coefficients  <- mod_fit_summary_lm$coefficients # (coefficients in summary are with Estimate, Std. error, t value and Pr)

        # confidence interval
        mod_result$confidence_bounds <- confint(mod_fit)
        colnames(mod_result$confidence_bounds) <- paste("confint", colnames(mod_result$confidence_bounds))
        # adds confidence interval columns to summary_coefficients
        # simple cbind() can fail (e.g. for ag_T3.C there are rows for
        # confidence intervals with NA entries but no rows for the coefficient)
        # therefore bind confidence intervals using the row names of mod_result$summary_coefficients
        mod_result$summary_coefficients <- cbind(mod_result$summary_coefficients,
                                                 mod_result$confidence_bounds[rownames(mod_result$summary_coefficients),])
      } else {
        mod_result$summary_coefficients  <- mod_fit$coefficients
      }

      mod_result$dev_print <- format(signif(mod_fit$deviance, max(3L,getOption("digits")-3L)))
    } else {
      # models with mixed effects

      # is the (possibly generalized) mixed-effects model a local model from lme4 package
      # or a merMod from DataSHIELD function ds.lmerSLMA?
      if (isglmer || isLmer) {
        # extract the needed slots from the lmer model
        mod_result$formula <- paste0(deparse(mod_fit@call$formula), collapse="")
        mod_result$call_string <- Reduce(paste, deparse(mod_fit@call))
        mod_result$class <- class(mod_fit)
        mod_result$model_string <- lme4:::methTitle(mod_fit@devcomp$dims) ##############

        mod_result$famlink <- lme4:::famlink(mod_fit, resp = mod_fit@resp)
        mod_result$family_string <- paste0("Family: ",mod_result$famlink$family, " (",mod_result$famlink$link,")")
        mod_result$coeff <- as.data.frame(unlist(coefficients(mod_fit)))

        # data from summary
        mod_result_summary <- lme4:::summary.merMod(mod_fit)
        mod_result$summary_coefficients <- mod_result_summary$coefficients
        mod_result$summary_random_eff <- lme4:::.prt.VC(mod_result_summary$varcor, digits = 3, useScale = summary(mod_fit)$useScale,
                                                        comp = c("Variance", "Std.Dev."), corr = ranef.corr)

        # random effects, deviance, residuals
        mod_result$reff <- lme4::ranef(mod_fit)
        mod_result$dev <- mod_fit@devcomp["dev"] # = NULL
        if  (isglmer) {
          mod_result$residuals <- mod_fit@resp$wrkResids()
        } else {
          mod_result$residuals <- mod_fit@resp$wtres
        }
      } else if (isDSmer) {
        # mixed effects model from ds.lmerSLMA (DataSHIELD)
        mod_result$formula <- mod_formula

        # collect study-wise summaries
        mod_result_summary_list <- lapply(1:mod_fit$num.valid.studies, function(i) mod_fit$output.summary[[paste0("study",i)]] ) # list of summaries for all studies

        # extract some information
        use_scale_vec <- sapply(mod_result_summary_list, function(x) x$useScale )
        # std. dev. of the mixed effects
        varcor_list <- lapply(mod_result_summary_list, function(x) x$varcor)
        varcor_df_list <- lapply(mod_result_summary_list, function(x) x$varcor)
        # join the information of all studies into one table
        varcor_df_list <- lapply(varcor_list, function(x) as.data.frame(x) %>% dplyr::select(grp,vcov,sdcor) )
        varcor_df_list <- mapply(function(x,y) cbind(data.frame(study=y), x) , varcor_df_list, 1:length(varcor_df_list), SIMPLIFY = F)
        mod_result$summary_random_eff <- do.call(rbind, varcor_df_list)
        # coefficients of the model
        mod_result$summary_coefficients <- mod_fit$SLMA.pooled.ests.matrix %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "effect")
        # aggregate number of missings
        mod_result$Nmissing <- sapply(1:mod_fit$num.valid.studies, function(x) mod_fit$output.summary[[paste0("study",x)]]$Nmissing) %>% sum(na.rm=T)

        # set class and description of model
        not_generalized <- (startsWith(mod_fit$output.summary$study1$methTitle,"Linear mixed"))
        class(mod_fit) <- if (not_generalized) "lmerSLMA" else "glmerSLMA"
        # mod_result$model_type_string <- "Generalized linear mixed-effect model via study-level meta-analysis"
        mod_result$family_string <- if (not_generalized)
          "Linear mixed-effect model"
        else
          "Generalized linear mixed-effect model"
        if ("study2" %in% names(mod_fit$output.summary)) {
          mod_result$family_string <- paste(mod_result$family_string,
                                            "via study-level meta-analysis")
        }
      } else {
        stop("unimplemented mixed-effects model")
      }
    }

    # build the output tables an optionally a plot
    mod_result$reff.df <- if (is.null(mod_result$summary_random_eff)) mod_result$reff else mod_result$summary_random_eff
    mod_result$reff.df <- mod_result$reff.df %>% kableExtra::kbl(digits=3) %>%
      kableExtra::kable_paper("striped", full_width = T) %>% kableExtra::kable_material_dark("striped", full_width = T) %>%
      kableExtra::kable_styling()

    # required objects:
    # for modelbox: mod_result$formula, mod_result$call_string, mod_fit$isLmer
    # for model_tab: mod_result$summary_coefficients / mod_result$coeff
    # for mod_family: mod_result$family_string, mod_fit$family$family, mod_fit$family$link

    # create the shiny box, that displays the model formula
    modelBox <- box(title = "Model",
                    ifelse(is.null(mod_result$formula), mod_result$call_string, mod_result$formula),
                    width = ifelse(isLmer, 12, 6),
                    style = "overflow-x: auto")

    # create the summary table for the coefficient estimates, including confidence intervals
    model_tab <- if (!is.null(mod_result$summary_coefficients)) {
      mod_result$summary_coefficients
    } else {
      mod_result$coeff
    }

    # the table is converted to a kable
    model_tab <- model_tab %>% kableExtra::kbl(digits=3) %>%
      kableExtra::kable_paper("striped", full_width = T) %>%
      kableExtra::kable_material_dark("striped", full_width = T) %>%
      kableExtra::kable_styling()

    mod_family <- if(!is.null(mod_result$family_string)) mod_result$family_string else paste("Family:", mod_fit$family$family, tags$br(), "Link function:", mod_fit$family$link   )

    # output will be produced, don't show instructions any more
    isolate({globals$validModDepVar <- TRUE })
    isolate({globals$modFitError <- FALSE})

    list(modelBox,
         if (!isLmer) { box(title=paste("Model class", paste(as.character(class(mod_fit)) , collapse=", ") ), if (!is.null(mod_result$model_type_string)) HTML(mod_result$model_type_string,"<br>"), HTML(mod_family) ) } else NULL,
         box(title = "Coefficients", width=12, HTML(model_tab), style = "overflow-x: auto"),
         if (isLmer || isglmer || isDSmer) box(title = "Random effects", width=12, HTML( mod_result$reff.df ), style = "overflow-x: auto") else NULL,
         if (!isLmer && !isglmer && !isDSmer) {box(title = "Degree of freedom", width=4, HTML(ifelse (is.null(mod_fit$df.residual), ifelse(is.null(mod_fit$df), "", mod_fit$df), mod_fit$df.residual)))} else NULL,
         if (!isLmer && !isglmer) {box(title = "Number of missing values", width=4, HTML(ifelse(is.null(mod_result$na_count), mod_result$Nmissing, mod_result$na_count) )) } else NULL,
         if (!isLmer && !isglmer && !isDSmer) {box(title = "Residual deviance", width=4, HTML(ifelse(is.null(mod_result$dev), "", mod_result$dev) )) } else NULL, ## dev_print for ds.glm
         if (!isDSmer) { box(title = "Diagnostic plot",
                             width=12,
                             ggiraph::renderGirafe({
                               # save residuals and their ranks in a variable
                               residualData <- if (isLmer || isglmer)
                                 resid(mod_fit)
                               else
                                 mod_fit$residuals
                               rankData <- rank(residualData)

                               # define shape and size for plot dots
                               shapes <- rep(19, length(rankData))
                               sizes <- rep(1, length(rankData))

                               # in case of DataSHIELD mode, mark not exact values with arrows
                               if (!is.null(cur_conns)) {
                                 shapes[1:2] <- -0x1F817
                                 shapes[(length(shapes)-1):length(shapes)] <- -0x1F815
                                 sizes[c(1,length(sizes))] <- 4
                                 sizes[c(2,length(sizes)-1)] <- 3
                               }

                               # create the ggplot
                               pl <- data.frame(residuals=(residualData-mean(residualData))/(sd(residualData))) |>
                                 ggplot2::ggplot(aes(sample=residuals)) +
                                 stat_qq(shape = shapes,
                                         size = sizes) +
                                 stat_qq_line()  +
                                 labs(title = "Normal Q-Q Plot of Standardized Residuals",
                                      x = "Theoretical Quantiles",
                                      y = "Standardized Residuals")

                               # create ggiraph object - same code for local mode and DataSHIELD mode
                               gpl <- ggiraph::girafe(ggobj = pl,
                                                      width_svg = 6.5, height_svg = 5)

                               # set plot behaviour
                               gpl <- girafe_options(gpl,
                                                     opts_sizing(rescale = TRUE, width = 1),
                                                     opts_zoom(min = .5, max = 4),
                                                     opts_toolbar(hidden=c("lasso_select", "lasso_deselect","saveaspng")))

                               # no selectable elements in plot
                               gpl <- girafe_options(gpl,
                                                     opts_selection(type = "none", only_shiny = FALSE),
                                                     opts_selection_key(type = "none", only_shiny = FALSE),
                                                     opts_selection_theme(type = "none", only_shiny = FALSE))
                             })
         )
         }
    )
  })

  # modFit must be computed, even when it is hidden element (otherwise it will be stuck being hidden after an error occurs in the model fit)
  outputOptions(output, 'modFit', suspendWhenHidden = FALSE)

  output$boxPlot <- ggiraph::renderGirafe({
    # login is required
    req(USER$login)
    # make sure required features are selected
    req(!is.null(globals$selectedNumericalFtrs))
    req(length(globals$selectedNumericalFtrs)>0)
    req(length(globals$selectedGrps)<3)

    # get currently selected table information
    tab <- input$inpTableSelect
    # get "number" of the current table
    tabnr <- readr::parse_number(tab)
    tabsymbol <- globals$allTables[tabnr,"symbol"]
    tabname <- globals$allTables[tabnr,".x"]
    cur_conn <- globals$conns[globals$allTables[tabnr,"id"][[1]]] # works also for pooled tables

    # table selection must not mismatch the selection saved in globals
    req(length(globals$selectedTable)>0 && globals$selectedTable==tab)

    if (input$debugmode) browser()

    # get feature and group variables
    ftr_vars <- globals$selectedNumericalFtrs
    grp1 <- if (length(globals$selectedGrps)>=1) globals$selectedGrps[1] else NULL
    grp2 <- if (length(globals$selectedGrps)>=2) globals$selectedGrps[2] else NULL
    selVars <- c(ftr_vars,grp1,grp2)

    # get metadata
    if (is.null(cur_conn)) { # local data mode
      # get the variables from the table
      theTab <- USER$data[[tabname]][,c(ftr_vars, grp1, grp2), drop=F]
      # get the metadata
      extracted_metadata <- get_metadata_local(theTab)
    } else { # DataShield mode
      extracted_metadata <- extract_from_metadata_ds(tab,selVars)
    }

    # save metadata in variables
    var_lab <- extracted_metadata$alias
    var_desc <- extracted_metadata$description
    grp1desc <- var_desc[grp1]
    grp2desc <- var_desc[grp2]
    grp1label <- var_lab[grp1]
    grp2label <- var_lab[grp2]
    xlabels <- var_lab[ftr_vars]
    xdesc <- var_desc[ftr_vars]
    grp1levs <- if (is.null(grp1)) NULL else extracted_metadata$levels[[grp1]]
    grp2levs <- if (is.null(grp2)) NULL else extracted_metadata$levels[[grp2]]

    if (is.null(cur_conn)) { # local data mode
      # melt data
      dataTab <- reshape2::melt(theTab, measure.vars = ftr_vars, rm.na = TRUE, variable.name="x")

      # set level names to label names
      if (!is.null(grp1)) levels(dataTab[[grp1]]) <- grp1levs
      if (!is.null(grp2)) levels(dataTab[[grp2]]) <- grp2levs

      # aggregate data to get quantiles and number of observations (similar to dsBase::boxPlotGGDS)
      form1 <- as.formula(paste0(".~",(paste0(formulaic::add.backtick(c(grp1,grp2,"x")),collapse="+"))))
      stats_full <- stats::aggregate(form1, dataTab, function(x){stats::quantile(x,c(0,0.25,0.5,0.75,1))})

      # flatten and arrange the data.frame
      stats_full <- cbind(stats_full, stats_full$value)
      stats_full$value <- NULL

      # rename the columns
      colnames(stats_full) <- c(unlist(ifelse(is.null(grp1),list(NULL),"group")), # group if there is a group 1
                                unlist(ifelse(is.null(grp2),list(NULL),"group2")), # group2 if there is a group 2
                                "x", "ymin", "lower", "middle", "upper", "ymax")

    } else { # DataShield mode
      # make sure we login again if login is lost
      if (do_global_login) tryCatch( {DSI::dsListWorkspaces(conns[[1]])}, error=function(e) {conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(conns)})
      tryCatch( {DSI::dsListWorkspaces(globals$conns[[1]])}, error=function(e) {globals$conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(globals$conns)})
      # development note: if connection is lost and tables have been pooled in the dashboard, the pooled tables have to be created again

      # get boxplot data from DataSHIELD
      boxplot_data <- NULL
      tryCatch( {
        boxplot_data <- dsDashboard::ds.boxplot_data(tabsymbol, ftr_vars, group = grp1, group2=grp2,
                                                         datasources = cur_conn )
      },
      error=function(e) {
        boxplot_data <<- list(error.messages = datashield.errors())
      })

      # workaround: if the above fails due to DataSHIELD privacy restrictions,
      # create first subgroups and than get boxplot data for each valid subgroup
      if (!is.null(boxplot_data$error.messages)) {
        # if globals$summaryDataCollection[[tab]] is empty, then it is a list of lists (as defined in output$summary)
        if (is.null(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]]) ||
            length(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]]) == 0  )
          grp_summ_stats <- NULL
        else
          grp_summ_stats <- jsonlite::fromJSON(globals$summaryDataCollection[[tab]][[jsonlite::toJSON(globals$selectedGrps)]])

        # apply boxplot function to each subgroup
        isolate(summ_stats1 <- dsGapply(tabsymbol, c(grp1, grp2), all_summaries, vars=ftr_vars, summaries="boxplot", current_data=grp_summ_stats, datasources=cur_conn, lazy=T)  )
        # save boxplot data in boxplot_data object
        boxplot_data$combined <- summ_stats1 %>%
          dplyr::filter(variable %in% ftr_vars) %>%
          tidyr::pivot_wider(names_from = "feature") %>% dplyr::rename(group=all_of(grp1), group2=all_of(grp2), x=variable) %>% dplyr::select(any_of(c("group","group2","x","ymin","lower","middle","upper","ymax"))) %>% dplyr::mutate_at(vars(c("ymin","lower","middle","upper","ymax")), as.numeric)
      }

      # this adds all levels (also the ones that do not occur in the subset of data)
      if (!is.null(boxplot_data$combined$group) && is.character(boxplot_data$combined$group)) boxplot_data$combined$group <- factor(boxplot_data$combined$group, levels=paste0(1:length(grp1levs)))
      if (!is.null(boxplot_data$combined$group2) && is.character(boxplot_data$combined$group2)) boxplot_data$combined$group2 <- factor(boxplot_data$combined$group2, levels=paste0(1:length(grp2levs)))

      # label the levels correctly for the plot (giving the labels instead above inside factor() would remove levels, that do not occur)
      if (length(grp1levs)>0) levels(boxplot_data$combined$group) <- grp1levs
      if (length(grp2levs)>0) levels(boxplot_data$combined$group2) <- grp2levs

      #build data object for the plot
      stats_full <- boxplot_data$combined
    }

    # create boxplot
    pl <- ggplot2::ggplot(stats_full) +
      ggiraph::geom_boxplot_interactive(stat = "identity",
                                        ggplot2::aes(x=x, lower=lower,
                                                     upper=upper, ymin=ymin,
                                                     ymax=ymax, middle=middle,
                                                     fill = if (is.null(grp1)) NULL else group,
                                                     data_id=if (is.null(grp1)) 'all' else group,
                                                     `data-id` = if (is.null(grp1)) 'all' else group
                                        ),
                                        extra_interactive_params = c("data-id"))
    if (!is.null(grp2)) pl <- pl +
      ggiraph::facet_wrap_interactive(~ group2,
                                      labeller = labeller_interactive(
                                        aes(
                                          label= if (USER$useLabels) stringr::str_wrap(paste0(grp2label,": ",group2),20) else group2,
                                          tooltip = paste0(grp2,": ", group2, "\n\n", grp2desc),
                                        )) )

    # if the x-labels are multiline labels, we need to handle the tooltips and data_ids in a special way:
    # number of lines for each feature
    xlabel_rlen <-  sapply(var_lab[ftr_vars], function(x) stringr::str_count( stringr::str_wrap(x,20) , "\n")+1 )
    xids <- rep(names(xlabel_rlen), times=xlabel_rlen)

    # adjust ggPlot
    pl <- pl +
      ggplot2::scale_x_discrete(labels=function(x) if (USER$useLabels) stringr::str_wrap(var_lab[x],20) else x ) +
      ggiraph::scale_fill_brewer_interactive(name = if (USER$useLabels) stringr::str_wrap(grp1label,20) else grp1, palette="Set1",
                                             data_id = function(breaks) { as.character(breaks)},
                                             tooltip = function(breaks) { as.character(breaks)},
                                             `data-id` = function(breaks) { as.character(breaks)},
                                             extra_interactive_params = c("data-id"),
                                             guide = guide_legend_interactive(
                                               title.theme = element_text_interactive(
                                                 tooltip = grp1desc),
                                               label.theme = element_text_interactive()# e.g. size = 8 can be set here
                                             ),
                                             labels = function(breaks) {
                                               lapply(breaks, function(br) {
                                                 label_interactive(
                                                   as.character(br),
                                                   data_id = as.character(br),
                                                   `data-id` = as.character(br),
                                                   extra_interactive_params = c("data-id"),
                                                   tooltip = as.character(br)
                                                 )
                                               })
                                             }) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::theme(
        axis.text.x= ggiraph::element_text_interactive(
          angle = -90, hjust = 0, vjust=0.5,
          data_id = xids,
          tooltip = xdesc
        )
      )
    # create ggiraph object - same code for local mode and DataSHIELD mode
    gpl <- ggiraph::girafe(ggobj = pl)
    gpl <- girafe_options(gpl,
                          opts_sizing(rescale = TRUE, width = 1),
                          opts_zoom(min = .5, max = 4),
                          opts_toolbar(hidden=c("lasso_select", "lasso_deselect","saveaspng")),
                          opts_hover_key(css = girafe_css(
                            css = "fill-opacity:1;",
                            text = "fill:black;font-weight: bold;" # legend text color when hovering legend (not bars)
                          )),
                          opts_hover(css = girafe_css(
                            css = "fill-opacity:1;",
                            area = "fill-opacity:1;",
                            text= "fill:black;font-weight: bold;" # text color when hovering bar (not legend)
                          )),
                          opts_hover_theme(css = girafe_css( # when hovering legend text
                            css = "",
                            text = "fill:black;font-weight: bold;"
                          ))
    )
    gpl <- girafe_options(gpl,
                          opts_selection(type = "none", only_shiny = FALSE),
                          opts_selection_key(type = "none", only_shiny = FALSE  ),
                          opts_selection_theme(type = "none", only_shiny = FALSE  ),
                          opts_hover_inv(css = girafe_css(css = "fill-opacity:0.3;stroke:grey;",
                                                          text = "fill:grey;stroke-width:0px;",
                                                          area = "fill:grey;stroke-width:0px;"))
    )

    # remember the svg id
    globals$svgid_box <- gpl$x$uid

    return(gpl)
  })

  # info/help message boxes for different plot types
  output$boxPlotInstructions <- renderUI({
    box(width=12, title = "Instructions", status="info", "Please choose one ",tags$u("numerical")," feature. Up to two groups can be selected additionally for grouped boxplots." )
  })
  outputOptions(output, "boxPlotInstructions", suspendWhenHidden = F)  # reason: uses output in conditional panel

  output$summaryInstructions <- renderUI({
    box(width=12, title = "Instructions", status="info", "Please choose features and groups for the summary statistics.")
  })

  output$alluvialPlotInstructions <- renderUI({
    if (globals$safemode)
      box(width=12, title = "Instructions", status="info", "Please choose a feature group for the alluvial plot." )
    else
      box(width=12, title = "Instructions", status="info", "Please choose at least two features for the alluvial." )
  })

  output$modelFitInstructions <- renderUI({
    box(width=12, title = "Instructions", status="info",
        tags$p("Please choose at least two features for a regression model."),
        tags$p("The first chosen feature is the dependent variable and must be either ",tags$u("numeric")," or an ",tags$u("ordered")," factor."),
        tags$p("Optionally, group variables can be chosen for Mixed-Effect models." )
    )
  })

  output$modelFitWarning <- renderUI({
    box(width=12, title = "Warning", status="warning",
        "Models for ",tags$b("unordered categorical dependent variables")," with more than two levels are currently not implemented." )
  })

  output$modelFitError <- renderUI({
    box(width=12, title = "Warning", status="warning",
        "Unfortunately the requested model cannot be created." )
  })

  # footer (defined for all output types)
  output$pairplot_footer <- output$model_footer <- output$sankey_footer <- output$boxplot_footer <- output$hist_footer <- output$summary_footer <- renderUI({
    list(
      box(width=12, title = "Data disclaimer", collapsible = TRUE, closable = F,
          HTML(html_disclaimer)
      ),
      box(width=12, title = "Acknowledgement", collapsible = TRUE, closable = F,
          HTML(html_acknowledgement)
      )
    )
  })

  observeEvent(input$dt_dblclick, {
    # select only 1 row on double click (removes multiple selection)
    selectRows(dataTableProxy("allTablesTab"), input$dt_dblclick$dt_row+1)
  })

  observeEvent(input$poolTabs, {
    message("pool tabs || conns:")
    if (input$debugmode) browser()

    # get "selected" tables from reactive value
    current_selection <- df_bool()

    # helper function to keep the attributes in pooled tables in local mode
    copyAttributes <- function(from, to) {
      from_names <- colnames(from)
      to_names <- colnames(to)
      both_names <- intersect(from_names,to_names)
      for (i in both_names) {
        attributes(to[[i]]) <- attributes(from[[i]])
      }
      return(to)
    }

    if (sum(current_selection)>1) {
      # message to console: which tables are selected?
      message(c( paste("try to pool rows", paste(which(current_selection), collapse=", ")),
                 paste0(" (", paste(globals$allTables[current_selection,2], collapse=", "), ")")  ))
    } else {
      msg <- "First check at least two compatible tables and then click the pool button."
      showNotification(msg, duration=globals$notification_duration, type="message")
      req(F)
    }
    all_servers <- unique(unlist(globals$allTables[,"id"]))
    cur_conn <- globals$conns[all_servers]

    if (!is.null(cur_conn)) {
      # DataSHIELD mode
      # make sure we login to DataSHIELD if connection is lost (global and local connection)
      if (do_global_login) tryCatch( {DSI::dsListWorkspaces(conns[[1]])}, error=function(e) {conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(conns)})
      tryCatch( {DSI::dsListWorkspaces(globals$conns[[1]])}, error=function(e) {globals$conns <<- DSI::datashield.login(logins = USER$logindata, assign = TRUE); globals$allTables <- dsDashboard::assignAllTables(globals$conns)})
      # development note: if connection is lost and tables have been pooled in the dashboard, the pooled tables have to be created again
    }

    # save new table name in a variable
    new_tabname <- paste0(c("pool",which(current_selection)),collapse="_")

    # if tryCatch below fails, this variable will still be empty
    allTables_new_line <- NULL

    tryCatch( {
      used_servers <- unique(unlist(globals$allTables[current_selection,"id"]))
      if (!is.null(cur_conn)) {
        if (any(startsWith(  (globals$allTables[current_selection,".x"]), "pool"))) {
          mess <- "Pooling of already pooled tables is not supported in DataSHIED mode."
          showNotification(mess, duration=globals$notification_duration, type="warning")
          stop(mess)
        }
        DSI::datashield.assign.table(conns = globals$conns[used_servers],
                                     symbol = paste0("tab",dim(globals$allTables)[1]+1),
                                     table = rep(globals$allTables[current_selection,".x"], sapply(globals$allTables$id[current_selection],length)) )
      } else {
        tmp_df <<- dplyr::bind_rows(USER$data[current_selection])
        for (curr_df in USER$data[current_selection]) {
          tmp_df <<- copyAttributes(curr_df, tmp_df)
        }
        USER$data[[new_tabname]] <<- tmp_df
      }

      # collect information for pooled tables
      allTables_new_line <- data.frame(id = I(list(I(used_servers))),
                                       .x = new_tabname,
                                       symbol = paste0("tab",nrow(globals$allTables)+1) )
    },
    error = function(e) {
      warning("table assignment failed in input$poolTabs");
      showNotification("Pooling of the selected tables failed.", duration=2*globals$notification_duration, type="warning")
      message(datashield.errors());
    })

    # add information to globals$allTables
    globals$allTables <- dplyr::bind_rows(globals$allTables,
                               allTables_new_line)
  })

  # SIDEBAR MENU
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){
      sidebarMenu(id="sbMenu",
                  menuItem("Tables", tabName = "tabTables", icon = icon("th", verify_fa = FALSE)),
                  menuItem("Plots & Analysis", selected=T, tabName = "dashboard", icon = icon("dashboard", verify_fa = FALSE)),
                  menuItem("Settings", selected=F, tabName = "tabSettings", icon = icon("gear", verify_fa = FALSE))
      )
    }
  })

  # main area of the dashboard
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      # set default settings; isolate code to avoid updating the body
      isolate({
      if (!is.null(input$safemode)) {
        globals$safemode <<- input$safemode
      } else {
        globals$safemode <<- TRUE
      }

      if (!is.null(input$use_names)) {
        globals$use_names <<- input$use_names
      } else {
        globals$use_names <<- FALSE
      }

      if (!is.null(input$show_logout)) {
        globals$show_logout <<- input$show_logout
      } else {
        globals$show_logout <<- FALSE
      }

      if (!is.null(input$debugmode)) {
        globals$debugmode <<- input$debugmode
      } else {
        globals$debugmode <<- FALSE
      }
      })

      # as long as no tab is selected, we go to the dashboard page
      activeTab <- if (!is.null(input$sbMenu)) input$sbMenu else "dashboard"

      message("----------user is logged in-------")
      tabItems(
        # "Settings"-tab
        tabItem(tabName = "tabSettings", class = ifelse(activeTab=="tabSettings","active",""), # active tab does not get inactive
                fluidRow(
                  box(width = 12, h3("Advanced settings"),
                      checkboxInput("safemode", "Simplified dashboard (safe options only)", value = globals$safemode),
                      checkboxInput("debugmode", "Debug mode", value = globals$debugmode),
                      checkboxInput("use_names", "Use variable names instead of variable labels", value = globals$use_names),
                      checkboxInput("show_logout", "Show logout button", value = globals$show_logout)
               ))),
        # "Tables"-tab
        tabItem(tabName = "tabTables", class = ifelse(activeTab=="tabTables","active","inactive"),
                fluidRow(
                  box(width = 12, title="Table selection", dataTableOutput('allTablesTab')),
                  uiOutput("tableTools"),
                  #box(width = 12, title="Table summary output", uiOutput('tabsummary'))
               )),
        # "Dashboard output"-tab
        tabItem(tabName ="dashboard", class = ifelse(activeTab=="dashboard","active","inactive"),
                fluidRow(
                  # a sidebar panel, where data features and variables can be selected (input fields)
                  sidebarPanel(
                    gdtools::addGFontHtmlDependency(family = c("Roboto Condensed","Roboto")),
                    # "Select table"-input
                    conditionalPanel('input.safemode == false',
                                     selectInput("inpTableSelect", "Select table",
                                                  getTableList(),
                                                  multiple=F)), # hidden in safemode, but value is still accessible
                    # container, which can hold inputs "Select feature" or "Select numerical feature"
                    conditionalPanel(
                      '["summary","histPlBox","pairPlBox","boxPlBox","modFit"].includes(input.mainTabs)',
                      # "Select features"-input (with numerical and categorical variables)
                      conditionalPanel(
                       '!["pairPlBox","boxPlBox"].includes(input.mainTabs)',
                       selectizeInput(
                         'inpFeatureSelectXY', 'Select features',
                         choices=isolate({globals$choicesFeatureSelect}),
                         multiple = TRUE,
                         options = list(placeholder = "<none selected>",
                                        maxItems = 5, plugins = list("remove_button","drag_drop"),
                                        create = TRUE,
                                        persist = TRUE,
                                        # render options: how the selected input gets displayed
                                        # item.value instead of item.label gives the real name of the variable
                                        # option: the selectable options in the list
                                        # item: the actually selected option
                                        render = I("{
                                          item: function(item, escape) {
                                            return '<div class=\"item ui-sortable-handle\">' + item.label + '</div>';
                                          },
                                          option: function(item, escape) {
                                            return '<div class=\"option\">' + item.label + '</div>';
                                          }
                                        }"))
                         )),
                      # "Select numerical feature"-input (for pair plots and boxplots)
                      conditionalPanel(
                        '["pairPlBox","boxPlBox"].includes(input.mainTabs)',
                        selectizeInput(
                          'inpNumericalFeatureSelectXY', 'Select numerical feature',
                          choices = isolate({globals$choicesNumericalFeatureSelect}),
                          multiple = TRUE,
                          options = list(placeholder = "<none selected>",
                                        maxItems = 500, plugins = list("remove_button"), mode="multi", create = TRUE, persist = TRUE,
                                        render = I("{
                                          item: function(item, escape) {
                                          return '<div class=\"item ui-sortable-handle\">' + item.label + '</div>';
                                          },
                                          option: function(item, escape) {
                                          return '<div class=\"option\">' + item.label + '</div>';
                                          }
                                          }"))
                        ))),
                    # "Select group"-input (single group selection over all cohorts for alluvial/Sankey plots in simplified mode(=safemode))
                    conditionalPanel(
                      '["sankeyBox"].includes(input.mainTabs) && input.safemode == true',
                      selectizeInput(
                        'inpTDFeatureSelect', 'Select group',
                        choices = isolate({if (!is.null(globals$choicesTDFeatureSelect) | length(globals$choicesTDFeatureSelect)==0) globals$choicesTDFeatureSelect else "empty"}),
                        multiple = TRUE,
                        options = list(
                          placeholder = "<none selected>",
                          mode = "multi",
                          maxItems = 1,
                          plugins = list("remove_button"),
                          create = TRUE,
                          optgroups=NULL,
                          persist = TRUE,
                          render = I("{
                            item: function(item, escape) {
                             return '<div class=\"item ui-sortable-handle\">' + item.label + '</div>';
                            },
                            option: function(item, escape) {
                             return '<div class=\"option\">' + item.label + '</div>';
                            }
                          }")
                      ))),
                    # "Select groups"-input (multiple group selection)
                    conditionalPanel(
                      '["summary","histPlBox","pairPlBox","boxPlBox","modFit"].includes(input.mainTabs) || (["sankeyBox"].includes(input.mainTabs) && input.safemode == false)',
                      selectizeInput(
                        'inpGroupSelectXY', 'Select groups',
                        choices = isolate({globals$choicesGroupFeatureSelect}),
                        multiple = TRUE,
                        options = list(
                          placeholder = "<none selected>",
                          maxItems = ifelse(globals$safemode==T,2,5),
                          plugins = list("remove_button","drag_drop"),
                          create = TRUE,
                          persist = TRUE,
                          render = I("{
                             item: function(item, escape) {
                              return '<div class=\"item ui-sortable-handle\">' + item.label + '</div>';
                            },
                            option: function(item, escape) {
                              return '<div class=\"option\">' + item.label + '</div>';
                            }
                          }")
                    ))),
                    # container for "Pairplot settings" (number of plotted observations)
                    conditionalPanel(
                      'input.mainTabs=="pairPlBox"',
                      wellPanel(id = "pairpl_settings",
                                title = "Pairplot settings",
                                width=12,
                                sliderInput("ptLimit", "Number of plotted observations:", min = min(10), max = max(5000), value = 3000) )),
                    # container for "Histogram settings"
                    conditionalPanel(
                      'input.mainTabs=="histPlBox"',
                       wellPanel(id = "hist_settings",
                                 title = "Histogram settings",
                                 width=12,
                                 conditionalPanel('output.featureIsNumeric',
                                                  sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)  ),
                                 radioButtons("hist_yax", "y-axis:",
                                              c("Absolute frequency" = "counts",
                                                "Relative frequency (missing percentages due to missing values)" = "density")),
                                 conditionalPanel('input.inpGroupSelectXY.length>=1',
                                                  radioButtons("hist_grp1_type", "Group 1:",
                                                               c("Separate plots" = "facets",
                                                                 "Stacked" = "stack",
                                                                 "Side-by-side" = "dodge",
                                                                 'Overlay' = 'identity',
                                                                 "Percent stacked" = "fill"
                                                               ), selected="stack")),
                                 conditionalPanel('input.inpGroupSelectXY.length>=2',
                                                  radioButtons("hist_grp2_type", "Group 2:",
                                                               c("Separate plots" = "facets",
                                                                 "Stacked" = "stack",
                                                                 "Side-by-side" = "dodge",
                                                                 'Overlay' = 'identity',
                                                                 "Percent stacked" = "fill"
                                                               ), selected="facets"))
                    )),
                    # container for "Summary settings"
                    conditionalPanel(
                      'input.mainTabs=="summary"',
                       wellPanel(
                         id = "summary_settings", title = "Summary settings", width=12,
                         # number of digits is (not availale in simplified mode (=safemode))
                         conditionalPanel(
                           'input.safemode == false',
                           numericInput('inpDigits', 'Extra digits',
                                        0, min = 0, max = NA, step = 1)
                           ),
                         # "Select summary statistics"-input
                         selectizeInput(
                           'inpSumStatSelect', 'Select summary statistics',
                           # possible and impemented summaries are:
                           # "min"           "5%"            "10%"           "25%"           "50%"           "median"        "75%"
                           # "90%"           "95%"           "max"           "Mean"          "Nvalid"        "Nmissing"      "sd"
                           # "var"           "densplot"      "boxplot"       "sparkline_pie" "sparkline_bar" "Ndistinct"     "mode"
                           # "quantiles"     "levels"        "used_levels"   "max_group_n"
                           choices = list(
                             `n<sub></sub>`="Nvalid", mean="Mean",sd="sd", var="var",
                             quantiles="quantiles", mode="mode", `n<sub>distinct</sub>`="Ndistinct",
                             `n<sub>group_max</sub>`="max_group_n", levels="used_levels",
                             density="densplot", boxplot="boxplot", `pie chart`="sparkline_pie", `bar chart`="sparkline_bar",
                             min="min", median="median", max="max", `n<sub>miss</sub>`="Nmissing"#, `n<sub>Msng</sub>`="missings", `levels<sub>all</sub>`="levels", `n<sub>total</sub>`="Ntotal",`10%`="10%"
                           ),
                           selected = c("Nvalid"),
                           multiple = TRUE,
                           options = list(
                             placeholder = "<none selected>",
                             plugins = list("remove_button","drag_drop"),
                             create = TRUE,
                             persist = TRUE,
                             render = I("{
                                        item: function(item, escape) {
                                          return '<div class=\"item ui-sortable-handle\">' + item.label + '</div>';
                                        },
                                        option: function(item, escape) {
                                          return '<div class=\"option\">' + item.label + '</div>';
                                        }
                                      }")))
                    ))
                  ),
                  # set available tabPanels for advanced mode (= safemode is FALSE))
                  if (!is.null(input$safemode) && input$safemode == FALSE)
                  tabBox(
                    side = "right", height = "250px", id="mainTabs",
                    tabPanel("Summary",value="summary",
                             fluidRow( # fluidRow ensures that the elements really stay inside the tabPanel area
                               conditionalPanel('input.inpFeatureSelectXY.length == 0', htmlOutput("summaryInstructions")),
                               conditionalPanel('input.inpFeatureSelectXY.length > 0', htmlOutput("summary")),
                               htmlOutput("summary_footer"))),
                    tabPanel("Barplot",value="histPlBox",
                             fluidRow(
                               conditionalPanel('input.inpFeatureSelectXY.length < 1', htmlOutput("distPlotInstructions")),
                               conditionalPanel('input.inpFeatureSelectXY.length == 1',
                                                htmlwidgets::shinyWidgetOutput("distPlot", 'girafe', package = 'ggiraph', width = "100%", height = "auto"),
                                                style="padding-right: 15px; padding-left: 15px;"),
                               htmlOutput("hist_footer"))),
                    tabPanel("Pairplot",value="pairPlBox",
                             fluidRow(
                             conditionalPanel('input.inpNumericalFeatureSelectXY.length != 2 && input.inpGroupSelectXY.length < 2', htmlOutput("pairPlotInstructions")),
                             conditionalPanel('input.inpGroupSelectXY.length > 1', htmlOutput("pairPlotWarning")),
                             conditionalPanel('input.inpNumericalFeatureSelectXY.length == 2 && input.inpGroupSelectXY.length < 2',
                                              htmlwidgets::shinyWidgetOutput("pairPlot", 'girafe', package = 'ggiraph', width = "100%", height = "auto"),
                                              style="padding-right: 15px; padding-left: 15px;"),
                             htmlOutput("hist_footer"))),
                    tabPanel("Boxplot",value="boxPlBox",
                             fluidRow(
                               conditionalPanel('input.inpNumericalFeatureSelectXY.length < 1', htmlOutput("boxPlotInstructions")),
                               conditionalPanel('input.inpGroupSelectXY.length > 2', htmlOutput("boxPlotWarning")),
                               conditionalPanel('input.inpNumericalFeatureSelectXY.length >= 1 && input.inpGroupSelectXY.length < 3',
                                                htmlwidgets::shinyWidgetOutput("boxPlot", 'girafe', package = 'ggiraph', width = "100%", height = "auto"),
                                                style="padding-right: 15px; padding-left: 15px;"),
                               htmlOutput("boxplot_footer"))),
                    tabPanel("Alluvial plot",value="sankeyBox",
                             fluidRow(
                               conditionalPanel('input.inpGroupSelectXY.length < 2', htmlOutput("alluvialPlotInstructions")),
                               conditionalPanel('input.inpGroupSelectXY.length > 1',
                                                htmlwidgets::shinyWidgetOutput("sankeyPlot", 'girafe', package = 'ggiraph', width = "100%", height = "auto"),
                                                style="padding-right: 15px; padding-left: 15px;"),
                               htmlOutput("sankey_footer"))),
                    tabPanel("Model fit",value="modFit",
                             fluidRow(
                               conditionalPanel('(input.inpFeatureSelectXY.length + input.inpGroupSelectXY.length) < 2', htmlOutput("modelFitInstructions")),
                               conditionalPanel('!output.modFitAvailable && (input.inpFeatureSelectXY.length + input.inpGroupSelectXY.length ) >= 2', htmlOutput("modelFitWarning")),
                               conditionalPanel('output.modFitError', htmlOutput("modelFitError")),
                               conditionalPanel('!output.modFitError && (input.inpFeatureSelectXY.length + input.inpGroupSelectXY.length) >= 2', htmlOutput("modFit")),
                               htmlOutput("model_footer")))
                  ) else # set available tabPanels for simplified mode (= safemode is TRUE))
                  tabBox(
                    side = "right", height = "250px", id="mainTabs",
                    tabPanel("Summary",value="summary",
                             fluidRow(
                               conditionalPanel('input.inpFeatureSelectXY.length == 0', htmlOutput("summaryInstructions")),
                               conditionalPanel('input.inpFeatureSelectXY.length > 0', htmlOutput("summary")),
                               htmlOutput("summary_footer"))),
                    tabPanel("Barplot",value="histPlBox",
                             fluidRow(
                               conditionalPanel('input.inpFeatureSelectXY.length < 1', htmlOutput("distPlotInstructions")),
                               conditionalPanel('input.inpFeatureSelectXY.length > 1', htmlOutput("distPlotWarning")),
                               conditionalPanel('input.inpFeatureSelectXY.length == 1',
                                                # shinyWidgetOutput, because height "auto" is not supported by function girafeOutput
                                                htmlwidgets::shinyWidgetOutput("distPlot", 'girafe', package = 'ggiraph', width = "100%", height = "auto"),
                                                style="padding-right: 15px; padding-left: 15px;"),
                               htmlOutput("hist_footer"))),
                    tabPanel("Boxplot",value="boxPlBox",
                             fluidRow(
                               conditionalPanel('input.inpNumericalFeatureSelectXY.length == 0', htmlOutput("boxPlotInstructions")),
                               conditionalPanel('input.inpNumericalFeatureSelectXY.length > 0',
                                                htmlwidgets::shinyWidgetOutput("boxPlot", 'girafe', package = 'ggiraph', width = "100%", height = "auto"),
                                                style="padding-right: 15px; padding-left: 15px;"),
                               htmlOutput("boxplot_footer"))),
                    tabPanel("Alluvial plot",value="sankeyBox",
                             fluidRow(
                               conditionalPanel('input.inpTDFeatureSelect.length == 0', htmlOutput("alluvialPlotInstructions")),
                               conditionalPanel('input.inpTDFeatureSelect.length > 0',
                                                htmlwidgets::shinyWidgetOutput("sankeyPlot", 'girafe', package = 'ggiraph', width = "100%", height = "auto"),
                                                style="padding-right: 15px; padding-left: 15px;"),
                               htmlOutput("sankey_footer")))
                  )
                ))
      )
    } else loginpage # if not logged in show login page
  })

  # define reactive values for tab "tables"
  df_all <- reactiveVal()
  df_bool <- reactiveVal()

  # tab "tables": output overview of available tables
  output$allTablesTab <-  DT::renderDataTable({
    message("summary tab for available tables")
    allTables <- globals$allTables
    isolate({
      if (is.null(allTables$symbol))
        allTables$symbol <- NA
      })

    if (is.null(globals$conns)) {
      # local mode
      # isolate, to avoid that update of USER$data directly triggers update of this tab
      isolate({
        df <- allTables
        df$server <- df$id
        df$id <- 1:nrow(globals$allTables)
        df$rows <- sapply(df$.x, function(x) { dat <- data.frame(USER$data[[x]],check.names = FALSE); return(dim(dat)[1]) })
        df$cols <- sapply(df$.x, function(x) { dat <- data.frame(USER$data[[x]],check.names = FALSE); return(dim(dat)[2]) })
      })
    } else {
      # DataSHIELD mode
      df <- dsDashboard::tabSummary(allTables, globals$conns)
    }
    # updates of df_all and df_bool shall not directly request an update of this tab
    isolate({ df_curr <- df_all()})
    isolate({ df_curr_bool <- df_bool()})

    # helper function to create shiny inputs in each row of the table summary:
    # builds up a number len of ids and passes them to a function FUN
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
      }
      inputs
    }

    # completely rebuild table summary, if new tables added by pooling
    if (  ( is.null(globals$conns) && (is.null(df_curr) || !identical(df_curr$.x, df$.x)    ) ) ||
          (!is.null(globals$conns) && (is.null(df_curr) || !identical(df_curr$name, df$name)) ) )
      isolate({
        # updates of df_all and df_bool will trigger an update of the table summary tab
        df_all(as.data.frame(cbind(
          df,
          bool=FALSE,
          check = shinyInput(checkboxInput, nrow(df), "checkb") )))
        df_bool(rep(F,nrow(df)))
      })

    # javascript code creates allTablesTab_cell_edit event when checkbox is ticked
    js <- c(
      "$('body').on('click', '[id^=checkb]', function(){",
      "  var id = this.getAttribute('id');",
      "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
      "  var value = $(this).prop('checked');",
      "  var info = [{row: i, col: 7, value: value}];",  # column 7 is the checkbox status
      "  Shiny.setInputValue('allTablesTab_cell_edit:DT.cellInfo', info);",
      "})"
    )

    # show table summary as DT datatable
    DT::datatable(df_all(),
                  rownames=T, # show rownames
                  options = list(autoWidth = TRUE, searching = TRUE, pageLength=5,
                                 columnDefs = list(list(visible=FALSE, targets="id"))  # hide coloumn "id"
                  ),
                  escape = FALSE, # checkbox code has to be interpreted as html
                  editable = list(target = "cell", disable = list(columns = 2:6)), # only first column is editable
                  selection = "single", # do not allow direct selection of multiple rows (we use the checkboxes for selection of multiple rows)
                  callback = htmlwidgets::JS(js) # use the javascript function as callback to enable selection by checkboxes
                  )
    }, server=FALSE
  )

  # event: table selected using checkbox
  observeEvent(input[["allTablesTab_cell_edit"]], {
    # when a table has been checked/unchecked, remember its current state (in df_bool)

    # get event information (which table has been (un)selected?)
    info <- input$allTablesTab_cell_edit

    # get last known table selections from df_bool and update df_bool according to info
    df_bool_update <- df_bool()
    if (is.null(df_bool_update)) df_bool_update <- rep(F,nrow(df_all()))
    df_bool_update[info$row] <- info$value
    df_bool(df_bool_update)
  })

  # build input fields for the login screen
  output$allInputs <- renderUI({
    message("Login-page: output$allInputs <- renderUI")

    # set default options
    defaultOptions <- unlist(ui_defaults$login_default_servers)

    # Get number of inputs added
    inputsToShow <- input$appendInput+1
    # create list of inputs
    inputTagList <- tagList()
    lapply(1:inputsToShow,function(i){
      newInputId <- paste0("input", i)
      # Prevent dynamic inputs from resetting
      newInputValue <- defaultOptions[min(i,length(defaultOptions))] #"Option 2"
      if (newInputId %in% names(input)) {
        newInputValue <- input[[newInputId]]
      }
      # add new input fields
      newSrvInp <- selectInput(newInputId, tagList(icon("server"),paste("Server",i)), defaultOptions, selected=newInputValue)
      newPflInp <- textInput(paste0(newInputId,"Pfl"), placeholder="Profile", label = tagList(icon("id-card"), paste("Profile",i)), value=ui_defaults$login_default_profile)
      newUsrInp <- conditionalPanel(condition = paste0("input.",newInputId,"ToP==0"), textInput(paste0(newInputId,"Usr"), placeholder="Username", label = tagList(icon("user"), paste("Username",i)), value=ui_defaults$login_default_user) )
      newPwdInp <- conditionalPanel(condition = paste0("input.",newInputId,"ToP==0"), passwordInput(paste0(newInputId,"Pwd"), placeholder="Password", label = tagList(icon("fas fa-unlock"), paste("Password",i)), value=ui_defaults$login_default_passwort) )
      newTknInp <- conditionalPanel(condition = paste0("input.",newInputId,"ToP==1"), passwordInput(paste0(newInputId,"Tkn"), placeholder="Token", label = tagList(icon("fas fa-unlock"), paste("Token",i)), value=ui_defaults$login_default_token) )
      newToPInp <- radioButtons(paste0(newInputId,"ToP"), label = "Token or Password?", choices = c("Username/Password"=0,"Token"=1), selected = as.numeric(ui_defaults$login_token_selected), inline = T)
      inputTagList <<- tagAppendChildren(inputTagList, hr(), newSrvInp, newPflInp, newUsrInp, newPwdInp, newTknInp, newToPInp)
    })
    inputTagList
  })
}

shinyApp(ui, server)
