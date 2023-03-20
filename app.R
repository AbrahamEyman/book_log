library(bslib)
library(tidyverse)
library(plotly)
library(arsenal)
library(googlesheets4)
library(gridExtra)
library(scales)
library(ggdark)
library(lubridate)
library(zoo)
library(rvest)
library(purrr)
library(stringr)
library('xml2')
library(gmailr)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(showtext)
#library(shiny)
library(grid)
library(stringi)
library(shinydashboard)
library(data.table)
library(xfun)
library(shinyBS)
font_add_google("Cabin Sketch", "Cabin Sketch")
showtext_auto()
version <- "1.0.0"

#setwd("D:/Denver/personal_rpgm/book_log/")
source('functions/functions.R')
gs4_auth(path = "data/sheets_credentials.json",
         email = "abrahameymancasey@gmail.com")
gm_auth_configure(path = "data/gmail_credentials.json")
options(
  gargle_oauth_cache = ".secret",
  gargle_oauth_email = "abrahameymancasey@gmail.com"
)
gm_auth(path = "data/gmail_credentials.json",
        email = "abrahameymancasey@gmail.com",
        cache = ".secret")
years <- rev(seq(2022, year(Sys.Date()), by = 1))
read_df <- get_data("read")
ui <- fluidPage(
  align = 'center',
  theme = bs_theme(
    bg = "#101010",
    fg = "#FDF7F7",
    primary = "white",
    bootswatch = "sketchy"
  ),
  includeCSS("css/book_log.css"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "css/book_log.css"),
    tags$style('
      ul.nav.nav-pills{
        display: flex !important;
        justify-content: center !important;
        -webkit-flex-wrap: nowrap !important;
      }'
    )
  ),
  titlePanel("book log"),
  navbarPage(
    title = "",
    tags$head(
      tags$style(HTML('.navbar-nav {
                              align-items: left !important;
                              justify-content: left !important;
                              text-align: left !important;
                              top: 0% !important;
                              -ms-transform: translateY(-50%);
                              transform: translateY(-50%);
                              position: absolute;
                      } 
                      .selectize-input.full {
                          background-color: #101010;
                          color: white;
                          font-size: 16px !important
                      }
                      .navbar-expand-sm, 
                      .navbar:not(.navbar-expand):not(.navbar-expand-sm):not(.navbar-expand-md):not(.navbar-expand-lg):not(.navbar-expand-xl) {
                          border-color: #101010 !important;
                      }
                      .navbar:not(.fixed-bottom):not(.navbar-fixed-bottom):not(.navbar-fixed-bottom) {
                          margin-bottom: 0px;
                      }
                      '))
    ),
    navbarMenu("", icon = icon("ellipsis-v"),
               tabPanel(
                 "home", icon = icon("home"),
                 tags$style('.input-sm {font-family: Cabin Sketch; font-size: 16px !important}'),
                 tabsetPanel(
                   type = "pills",
                   tags$style('.input-sm {font-family: Cabin Sketch; 
                                          margin-left: -50px !important}'),
                   tabPanel(
                     "catalog", icon = icon("book", lib = "font-awesome"),
                     mainPanel(align = 'center',
                               tags$style('.input-sm {font-family: Cabin Sketch;}
                                          figcaption {display: block;
                                                      font-weight: normal !important;
                                                      }
                                          .shiny-split-layout {
                                              white-space: nowrap;
                                              margin-top: 10px;
                                              margin-left: 10px;
                                              margin-bottom: -30px;
                                          }
                                          label#cat_sorter-label.control-label {
                                              display: inline-block;
                                              font-weight: normal;
                                          }
                                          .selectize-input.full {
                                              background-color: #101010;
                                              color: white;
                                              font-size: 80%;
                                          }
                                          .form-label, .shiny-input-container .control-label {
                                              font-weight: normal;
                                          }
                                          .selectize-control {
                                              margin-top: -10px;
                                          }
                                          #suggested_book {
                                              margin-top: 13px !important;
                                          }
                                          input#title_rec-input.control-input {
                                              margin-botton: -20px !important;
                                          }
                                          #clear {
                                              font-weight: normal;
                                              float: left !important;
                                              left: 20px !important;
                                              position: fixed;
                                              bottom: 20px !important;
                                          }
                                          #search_suggestion {
                                              margin-bottom: 3px !important;
                                          }
                                          ',
                                          type = "text/css"),
                               splitLayout(cellWidths = c("50%", "20%"),
                                           textInputIcon("searcher", "", icon = icon("search")),
                                           radioButtons("cat_sorter", "sort", choiceNames = list(icon("calendar"), 
                                                                                                 icon("thumbs-up")),
                                                        choiceValues = list("date", "rating"))
                                           
                                           #, materialSwitch("show_dates", value = FALSE)
                               ),
                               selectInput("n_catalog", "# books", 
                                           choices = c(seq(from = 10, 
                                                           to = length(read_df$book_title), 
                                                           by = 10), 
                                                       length(read_df$book_title)),
                                           width = "20%"),
                               # dateRangeInput("date_catalog", "",
                               #                start = max(c(Sys.Date() - months(3),
                               #                              as.Date("2022-10-31"))),
                               #                end = Sys.Date(),
                               #                max = Sys.Date(),
                               #                min = "2022-10-31"),
                               useShinyjs(),
                               textOutput("no_results"),
                               tableOutput("catalog") #results_by_genre formerly
                     )
                   ),
                   tabPanel(
                     "recommend", icon = icon("lightbulb"),
                     mainPanel(align = 'center',
                               splitLayout(cellWidths = c("60%", "30%", "10%"),
                                           textInput("title_rec", "", value = "", placeholder = "book title*"),
                                           radioButtons("series_indicator", "series", choiceNames = c("yes", "no"),
                                                        choiceValues = c("yes", "no"), selected = "no")),
                               splitLayout(cellWidths = c("40%", "40%", "10%"),
                                           textInput("author_name", "", value = "", placeholder = "author name*"),
                                           textInput("rec_name", "", value = "", placeholder = "your name*")),
                               splitLayout(cellWidths = c("50%", "20%", "20%"),
                                           uiOutput("series_name"),
                                           uiOutput("series_book_num"))),
                     titlePanel(""),
                     uiOutput("search_suggestion"),
                     uiOutput("progress_bar_search"),
                     tableOutput("suggested_book"),
                     textOutput("error"),
                     textOutput("already_read"),
                     uiOutput("submit_suggestion"),
                     textOutput("complete"),
                     actionButton("clear", "clear", icon = icon("trash"),
                                  style='font-family: Cabin Sketch; padding:4px; font-size:79%')
                   ),
                   tabPanel(
                     "ratings", icon = icon("star"),
                     mainPanel(align = 'center',
                               tabsetPanel(
                                 type = "pills",
                                 tags$style('.input-sm {font-family: Cabin Sketch; }
                                              ul.nav.nav-pills{
                                                display: flex !important;
                                                justify-content: center !important;
                                              }'),
                                 tabPanel(
                                   "top rated", icon = icon("table"),
                                   tags$style('.shiny-options-group {
                                                          margin-top: -10px;
                                                          font-family: Cabin Sketch !important;,
                                                          font-weight: normal;
                                                          font-size: 75%;}
                                                      .noUi-connect {
                                                        background: white !important;
                                                      }
                                                      
                                                      .dataTables_wrapper .dataTables_length select {
                                                            color: white !important;
                                                            background-color: #101010 !important;
                                                            border-color: white !important;
                                                        }
                                                      .noUi-background {
                                                          background: #101010 !important;
                                                      }
                                                      .form-control {
                                                          font-weight: normal;
                                                          font-size: 16px;
                                                      }
                                                      
                                                      .dataTables_wrapper .dataTables_length select {
                                                          color: white !important;
                                                      }
                                                      tr:not(:last-child), tr:nth-child(4), tr:nth-child(5),
                                                      tr:nth-child(10), tr:nth-child(15), tr:nth-child(20),
                                                      tr:nth-child(1) {
                                                          color: white !important;
                                                          font-weight: normal !important;
                                                      }
                                                     div.datatables {
                                                          color: #333;
                                                          font-size: 75%;
                                                     }
                                                      #n_catalog-label.control-label{
                                                          margin-top: 10px !important
                                                      }
                                                     table.dataTable thead .sorting_asc {
                                                          background-image: url(../images/sort_asc.png) !important;
                                                      }
                                                      .progress {
                                                          border: 2px solid #CEC9C9;
                                                          border-radius: 255px 25px 225px 25px/25px 225px 25px 255px;
                                                          width: 75%;
                                                      }'),
                                   prettyRadioButtons("ratings_type",
                                                      "", choices = c("fiction", "non-fiction")),
                                   column(
                                     DT::dataTableOutput("ratings_table"), width = 6
                                   )
                                   
                                 ),
                                 tabPanel(
                                   "", icon = icon('thumbs-up'),
                                   mainPanel(align = "center",
                                             dateRangeInput("daterange", "",
                                                            start = max(c(Sys.Date() - months(3),
                                                                          as.Date("2022-10-31"))),
                                                            end = Sys.Date(),
                                                            max = Sys.Date(),
                                                            min = "2022-10-31"),
                                             titlePanel(h3("best fiction", align = "center")),
                                             tableOutput("best_fiction"),
                                             titlePanel(h3("best non-fiction", align = "center")),
                                             tableOutput("best_nonfiction"),
                                   )
                                 )
                               )
                     )
                   )
                 )
                 
               ),
               tabPanel(
                 "insights", icon = icon("line-chart"),
                 mainPanel(align = 'center',
                           tabsetPanel(
                             type = "pills",
                             tabPanel("hours", icon = icon("clock"),
                                      #titlePanel(icon('clock')),
                                      selectInput("months", label = "",
                                                  choices = c("last 6 months", "all",
                                                              years[1], years[2:length(years)])),
                                      plotlyOutput("big_cal_plot", height = 'auto', width = "auto")),
                             tabPanel("top genres", icon = icon("table"),
                                      tags$style(''),
                                      prettyRadioButtons("genre_type",
                                                         "", choices = c("fiction", "non-fiction")),
                                      column(
                                        DT::dataTableOutput("top_genres"), width = 6
                                      )
                             ),
                             tabPanel(
                               "ratings plot", icon = icon("bar-chart"),
                               mainPanel(align = "center",
                                         dateRangeInput("daterange1", "",
                                                        start = max(c(Sys.Date() - months(3),
                                                                      as.Date("2022-10-31"))),
                                                        end = Sys.Date(),
                                                        max = Sys.Date(),
                                                        min = "2022-10-31"),
                                         plotlyOutput("detailed_view")
                               )
                             )
                           )
                 )
               ),
               tabPanel(
                 "", icon = icon("info"),
                 mainPanel(align = 'center',
                           tabsetPanel(
                             type = "pills",
                             tabPanel(
                               "", icon = icon("info"),
                               bsAlert("info_alert")
                             ),
                             tabPanel(
                               "", icon = icon("wrench"),
                               actionButton("update_data", "update data", icon = icon("refresh"),
                                            style = 'font-family: Cabin Sketch'),
                               progressBar(id = "pb1", value = 0, display_pct = T),
                               textOutput("success_update")
                             )
                           )
                 )
               )
    )
  )
)

server <- function(input, output, session){
  read_df <- clean_read_df()
  to_read_df <- get_data("to_read")
  six_month_email_reminder(read_df)
  #### catalog data table render UI. careful here lad it's finicky
  output$catalog <- renderUI({
    num_books_per_row <- 2
    catalog_df <- read_df %>%
      mutate(date_finished = as.Date(date_finished)) %>%
      rowwise() %>%
      mutate(dummy_date_finished = replace_na(date_finished,
                                              Sys.Date())) %>%
      ungroup() %>%
      as.data.frame()
    
    if(input$searcher != ""){
      catalog_df <- catalog_df %>%
        filter(str_detect(book_title, tolower(paste0(input$searcher))) |
                 str_detect(author, tolower(paste0(input$searcher))) |
                 str_detect(recommended, tolower(paste0(input$searcher))) |
                 str_detect(genre, tolower(paste0(input$searcher))))
    }
    if(input$cat_sorter == "date"){
      catalog_df <- catalog_df %>%
        arrange(dummy_date_finished)
    } else if(input$cat_sorter == "rating"){
      catalog_df <- catalog_df %>%
        mutate(rating_dummy = replace_na(rating_arrange, 0)) %>%
        arrange(rating_dummy)
    }
    catalog_df <- catalog_df %>% ungroup() %>% arrange(-row_number())
    catalog_df <- catalog_df[1:input$n_catalog,] %>% drop_na(book_title)
    catalog_df <- catalog_df %>% ungroup() %>% arrange(-row_number())
    all_urls <- catalog_df$img_url
    baseline <- nrow(catalog_df)
    if(length(all_urls)>0){
      list(
        lapply(seq(1, length(all_urls), by = 2), function(i){
          div(
            a(
              tags$figure(
                tags$img(src = all_urls[(baseline-i+1)], height = 180, 
                         style="cursor:pointer;"),
                tags$figcaption(#paste0(catalog_df[baseline - i+1,]$book_title),
                  get_title_text(catalog_df[(baseline - i+1),]$book_title),
                  tags$br(),
                  "completed:",
                  ifelse(is.na(catalog_df[(baseline - i+1),]$date_finished), 
                         "tbd", 
                         format(catalog_df[(baseline - i+1),]$date_finished,
                                "%m-%d-%y")),
                  tags$br(),
                  HTML(paste0("rating: ", 
                              ifelse(is.na(catalog_df[(baseline - i+1),]$rating), 
                                     "tbd", 
                                     catalog_df[(baseline - i+1),]$rating), icon("star")))
                )
              ), href = paste0("https://www.goodreads.com/search?q=",
                               gsub(": ", "", catalog_df[(baseline - i+1),]$isbn)),
              style = "display: inline-block;",
              target = "_blank"),
            a(HTML('&nbsp;'), style = "display: inline-block;"),
            if((baseline-i) > 0) {
              a(
                tags$figure(
                  tags$img(src = all_urls[(baseline-i)], height = 180, 
                           style="cursor:pointer;"),
                  tags$figcaption(get_title_text(catalog_df[(baseline - i),]$book_title),
                                  tags$br(),
                                  "completed:",
                                  paste0(format(catalog_df[(baseline - i),]$date_finished,
                                                "%m-%d-%y")),
                                  tags$br(),
                                  HTML(paste0("rating: ", catalog_df[(baseline - i),]$rating, 
                                              icon("star")))
                  )
                ), href = paste0("https://www.goodreads.com/search?q=",
                                 gsub(": ", "", catalog_df[(baseline - i),]$isbn)),
                target = "_blank",
                style = "display: inline-block;"
              )} else {
                a(
                  ""
                )
              }
            
          )
          
        }
        )
      )
    } else {
      "no results found"
    }
  })
  
  #### ratings tab with scatter plot and best fiction/non-fiction within date range
  
  output$best_fiction <- renderUI({
    num_books_per_row <- 2
    fiction_cat_df <- read_df %>%
      mutate(date_finished = as.Date(date_finished)) %>%
      rowwise() %>%
      mutate(dummy_date_finished = replace_na(date_finished,
                                              Sys.Date())) %>%
      filter(dummy_date_finished >= input$daterange[1],
             dummy_date_finished <= input$daterange[2]) %>%
      filter(fiction_bool == "fiction") %>%
      arrange(desc(rating))
    
    fiction_cat_df <- fiction_cat_df %>%
      filter(rating == max(fiction_cat_df$rating, na.rm = TRUE))
    
    all_urls <- fiction_cat_df$img_url
    baseline <- nrow(fiction_cat_df)
    
    list(
      lapply(seq(1, length(all_urls), by = 2), function(i){
        #fluidRow(
        div(
          a(
            tags$figure(
              tags$img(src = all_urls[baseline-i+1], height = 200, 
                       style="cursor:pointer;"),
              tags$figcaption(#paste0(catalog_df[baseline - i+1,]$book_title),
                get_title_text(fiction_cat_df[baseline - i+1,]$book_title),
                tags$br(),
                paste0("rating: ", 
                       ifelse(is.na(fiction_cat_df[baseline - i+1,]$rating), 
                              "tbd", 
                              fiction_cat_df[baseline - i+1,]$rating))
              )
            ), href = paste0("https://www.goodreads.com/search?q=",
                             gsub(": ", "", fiction_cat_df[baseline - i+1,]$isbn)),
            style = "display: inline-block;",
            target = "_blank"),
          a(HTML('&nbsp;'), style = "display: inline-block;"),
          if((baseline-i) > 0) {
            a(
              tags$figure(
                tags$img(src = all_urls[baseline-i], height = 200, 
                         style="cursor:pointer;"),
                tags$figcaption(get_title_text(fiction_cat_df[baseline - i,]$book_title),
                                tags$br(),
                                paste0("rating: ", fiction_cat_df[baseline - i,]$rating)
                )
              ), href = paste0("https://www.goodreads.com/search?q=",
                               gsub(": ", "", fiction_cat_df[baseline - i,]$isbn)),
              target = "_blank",
              style = "display: inline-block;"
            )} else {
              a(
                ""
              )
            }
          
        )
        #)
        
      }
      )
    )
    
  })
  
  
  output$best_nonfiction <- renderUI({
    num_books_per_row <- 2
    nonfiction_cat_df <- read_df %>%
      mutate(date_finished = as.Date(date_finished)) %>%
      rowwise() %>%
      mutate(dummy_date_finished = replace_na(date_finished,
                                              Sys.Date())) %>%
      filter(dummy_date_finished >= input$daterange[1],
             dummy_date_finished <= input$daterange[2]) %>%
      filter(fiction_bool == "non-fiction") %>%
      arrange(desc(rating))
    
    nonfiction_cat_df <- nonfiction_cat_df %>%
      filter(rating == max(nonfiction_cat_df$rating, na.rm = TRUE))
    
    all_urls <- nonfiction_cat_df$img_url
    baseline <- nrow(nonfiction_cat_df)
    
    list(
      lapply(seq(1, length(all_urls), by = 2), function(i){
        #fluidRow(
        div(
          a(
            tags$figure(
              tags$img(src = all_urls[baseline-i+1], height = 200, 
                       style="cursor:pointer;"),
              tags$figcaption(#paste0(catalog_df[baseline - i+1,]$book_title),
                get_title_text(nonfiction_cat_df[baseline - i+1,]$book_title),
                tags$br(),
                paste0("rating: ", 
                       ifelse(is.na(nonfiction_cat_df[baseline - i+1,]$rating), 
                              "tbd", 
                              nonfiction_cat_df[baseline - i+1,]$rating))
              )
            ), href = paste0("https://www.goodreads.com/search?q=",
                             gsub(": ", "", nonfiction_cat_df[baseline - i+1,]$isbn)),
            style = "display: inline-block;",
            target = "_blank"),
          a(HTML('&nbsp;'), style = "display: inline-block;"),
          if((baseline-i) > 0) {
            a(
              tags$figure(
                tags$img(src = all_urls[baseline-i], height = 200, 
                         style="cursor:pointer;"),
                tags$figcaption(get_title_text(nonfiction_cat_df[baseline - i,]$book_title),
                                tags$br(),
                                paste0("rating: ", nonfiction_cat_df[baseline - i,]$rating)
                )
              ), href = paste0("https://www.goodreads.com/search?q=",
                               gsub(": ", "", nonfiction_cat_df[baseline - i,]$isbn)),
              target = "_blank",
              style = "display: inline-block;"
            )} else {
              a(
                ""
              )
            }
          
        )
        #)
        
      }
      )
    )
    
  })
  
  
  
  output$detailed_view <- renderPlotly({
    make_detailed_plot(read_df, input$daterange1[1], input$daterange1[2])
  })
  
  output$ratings_table <- DT::renderDataTable(
    read_df %>% 
      mutate(book_title =  paste0('<a  target=_blank href=', 
                                  "https://www.goodreads.com/search?q=", 
                                  gsub(": ", "", isbn), '>', book_title,'</a>'),
             rating_star = paste0(rating, as.character(icon("star")))) %>% 
      filter(fiction_bool == input$ratings_type) %>%
      arrange(desc(rating_arrange))  %>%
      dplyr::select(book_title, author, rating, 
                    date_finished) 
    # %>%
    #   dplyr::rename(paste("rating", as.character(icon("star"))) = "rating")
    ,
    options = list(
      #columnDefs = list(list(visible=FALSE, targets=c(5))),
      scrollX = TRUE,
      pageLength = 5, 
      lengthMenu = list(c(5, 10, 15, 20, -1), 
                        c('5', '10', '15', '20', 'all')),
      dom = 'lt'
    ), filter = 'top', server = TRUE, rownames = FALSE, escape = FALSE
  )
  
  ### monthly hour tab bar graph aka insights tab
  big_cal_df <- make_data(read_df) %>%
    mutate(date_finished = as.Date(date_finished)) %>%
    rowwise() %>%
    mutate(date_finished = replace_na(date_finished,
                                      Sys.Date()))
  
  
  output$big_cal_plot <- renderPlotly({
    make_big_plot(big_cal_df, input$months)
  })
  
  #### update data event
  observeEvent(input$update_data, {
    read_df <- get_data(option = "read")
    to_read_df <- get_data(option = "to_read")
    length_df <- read_df %>%
      dplyr::select(book_title, length, author, isbn, pages) %>%
      as.data.frame()
    updateProgressBar(session = session, id = "pb1", value = 2)
    
    df <- data.frame(c(NA), c(NA), c(NA)) %>% setNames(c("title", "author", "length"))
    for(i in 1:nrow(length_df)){
      use_df <- length_df %>%
        filter(row_number() == i)
      length <- get_audio_length(use_df$book_title, use_df$author, use_df$length)
      df <- df %>%
        bind_rows(
          data.frame(use_df$book_title, use_df$author,
                     length) %>%
            setNames(c("title", "author", "length"))
        )
    }
    
    df %>%
      filter(row_number() != 1) %>%
      distinct() %>%
      dplyr::select(length) %>%
      mutate(length = gsub(" and ", "&", length)) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', sheet = "active", range = "E1", reformat = FALSE)
    
    updateProgressBar(session = session, id = "pb1", value = 9)
    
    length_df <- to_read_df %>%
      dplyr::select(book_title, length, author, isbn, pages) %>%
      as.data.frame()
    df <- data.frame(c(NA), c(NA), c(NA)) %>% setNames(c("title", "author", "length"))
    for(i in 1:nrow(length_df)){
      use_df <- length_df %>%
        filter(row_number() == i)
      length <- get_audio_length(use_df$book_title, use_df$author, use_df$length)
      df <- df %>%
        bind_rows(
          data.frame(use_df$book_title, use_df$author,
                     trimws(gsub(" and ", "&", length))) %>%
            setNames(c("title", "author", "length"))
        )
    }
    
    df %>%
      filter(row_number() != 1) %>%
      distinct() %>%
      dplyr::select(length) %>%
      mutate(length = gsub(" and ", "&", length)) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                  sheet = "to_read", range = "E1", reformat = FALSE)
    
    updateProgressBar(session = session, id = "pb1", value = 14)
    
    to_read_isbn_page_df <- to_read_df %>%
      dplyr::select(book_title, author, isbn, pages) %>%
      rowwise()
    
    df <- data.frame(c(NA), c(NA), c(NA), c(NA)) %>% setNames(c("title", "author", "isbn", "pages"))
    for(i in 1:nrow(length_df)){
      use_df <- to_read_isbn_page_df %>%
        as.data.frame() %>%
        filter(row_number() == i) %>%
        mutate(new = page_isbn_retriever(book_title, author, isbn, pages),
               isbn = ifelse(str_detect(new, "ISBN: "),
                             str_split(str_split(new, "-")[[1]][1], "N")[[1]][2],
                             ifelse(str_count(str_split(new, "-")[[1]][1], ": ") == 0,
                                    paste0(": ", str_split(new, "-")[[1]][1]),
                                    paste0(": ", gsub(": ", "", str_split(new, "-")[[1]][1])))
               ),
               pages = str_split(str_split(new, "-")[[1]][2], " ")[[1]][1]) %>%
        dplyr::select(-new)
      df <- df %>%
        bind_rows(
          data.frame(use_df$book_title, use_df$author,
                     use_df$isbn, use_df$pages) %>%
            setNames(c("title", "author", "isbn", "pages"))
        )
    }
    
    df %>%
      filter(row_number() != 1) %>%
      distinct()  %>%
      dplyr::select(isbn, pages) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', sheet = "to_read", range = "M1", reformat = FALSE)
    
    updateProgressBar(session = session, id = "pb1", value = 26)
    
    isbn_page_df <- read_df %>%
      dplyr::select(book_title, author, isbn, pages) %>%
      rowwise() %>%
      mutate(new = page_isbn_retriever(book_title, author, isbn, pages),
             isbn = ifelse(str_detect(new, "ISBN:"),
                           str_split(str_split(new, "-")[[1]][1], "N")[[1]][2],
                           ifelse(str_count(str_split(new, "-")[[1]][1], ": ") == 0,
                                  paste0(": ", str_split(new, "-")[[1]][1]),
                                  paste0(": ", gsub(": ", "", str_split(new, "-")[[1]][1])))
             ),
             pages = str_split(str_split(new, "-")[[1]][2], " ")[[1]][1]) %>%
      dplyr::select(-new)
    
    isbn_page_df %>%
      dplyr::select(isbn, pages) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', sheet = "active", range = "M1", reformat = FALSE)
    
    updateProgressBar(session = session, id = "pb1", value = 37)
    
    to_read_author_gender <- to_read_df %>%
      dplyr::select(book_title, author, isbn, author_gender, fiction_bool, genre) %>%
      rowwise()
    
    df <- data.frame(c(NA), c(NA), c(NA), c(NA), c(NA), c(NA)) %>%
      setNames(c("title", "author", "isbn", "author_gender", "fiction_bool", "genre"))
    for(i in 1:nrow(to_read_author_gender)){
      use_df <- to_read_author_gender %>%
        as.data.frame() %>%
        filter(row_number() == i) %>%
        mutate(result = get_author_gender(gsub(": ", "", isbn), author_gender, fiction_bool, genre),
               author_gender = str_split(result, "_", n = 3)[[1]][1],
               fiction_bool = str_split(result, "_", n = 3)[[1]][2],
               genre = str_split(result, "_", n = 3)[[1]][3]) %>%
        dplyr::select(-result)
      df <- df %>%
        bind_rows(
          data.frame(use_df$book_title, use_df$author,
                     use_df$isbn, use_df$author_gender, use_df$fiction_bool,
                     use_df$genre) %>%
            setNames(c("title", "author", "isbn", "author_gender",
                       "fiction_bool", "genre"))
        )
    }
    
    
    df %>%
      filter(row_number() != 1) %>%
      distinct()  %>%
      dplyr::select(author_gender) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', sheet = "to_read", range = "P1", reformat = FALSE)
    
    updateProgressBar(session = session, id = "pb1", value = 49)
    
    df %>%
      filter(row_number() != 1) %>%
      distinct() %>%
      dplyr::select(fiction_bool, genre) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', sheet = "to_read", range = "K1", reformat = FALSE)
    
    
    updateProgressBar(session = session, id = "pb1", value = 69)
    
    df <- data.frame(c(NA), c(NA), c(NA)) %>%
      setNames(c("title", "isbn", "img_url"))
    for(i in 1:nrow(read_df)){
      print(i)
      use_df <- read_df %>%
        as.data.frame() %>%
        filter(row_number() == i) %>%
        mutate(img_url = get_cover_url(gsub(": ", "", isbn), img_url))
      df <- df %>%
        bind_rows(
          data.frame(use_df$book_title, use_df$isbn, use_df$img_url) %>%
            setNames(c("title", "isbn", "img_url"))
        )
    }
    
    df %>%
      filter(row_number() != 1) %>%
      distinct()  %>%
      dplyr::select(img_url) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                  sheet = "active", range = "x1", reformat = FALSE)
    
    updateProgressBar(session = session, id = "pb1", value = 83)
    
    df <- data.frame(c(NA), c(NA), c(NA)) %>%
      setNames(c("title", "isbn", "img_url"))
    for(i in 1:nrow(to_read_df)){
      print(i)
      use_df <- to_read_df %>%
        as.data.frame() %>%
        filter(row_number() == i) %>%
        mutate(img_url = get_cover_url(gsub(": ", "", isbn), img_url))
      df <- df %>%
        bind_rows(
          data.frame(use_df$book_title, use_df$isbn, use_df$img_url) %>%
            setNames(c("title", "isbn", "img_url"))
        )
    }
    
    df %>%
      filter(row_number() != 1) %>%
      distinct()  %>%
      dplyr::select(img_url) %>%
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                  sheet = "to_read", range = "x1", reformat = FALSE)
    updateProgressBar(session = session, id = "pb1", value = 100)
    output$success_update <- renderText({
      "successfully updated sheets!"
    })
    
  })
  
  
  #### other insight additions?
  output$top_genres <- DT::renderDataTable(
    get_top_genres(
      make_long_genre_df(read_df), 
      read_df,
      type = input$genre_type),
    options = list(
      scrollX = TRUE,
      pageLength = 5, 
      lengthMenu = list(c(5, 10, 15, 20, -1), 
                        c('5', '10', '15', '20', 'all')),
      dom = 'lt'
    ), filter = 'top', server = TRUE, rownames = FALSE
  )
  
  
  
  
  #### recommendation tab
  rec_vals <- reactiveValues()
  rec_vals$isbn_suggested <- rec_vals$isbn_to_read_check <- rec_vals$read_url_check <- 
    rec_vals$url_suggested <- rec_vals$submit_final <- NA
  observeEvent(input$clear, {
    updateTextInput(session = session, "title_rec", value = "")
    updateTextInput(session = session, "rec_name", value = "")
    updateRadioButtons(session = session, "series_indicator", selected = "no")
    updateTextInput(session = session, "author_name", value = "")
    updateProgressBar(session = session, id = "pb_search", value = 0)
    rec_vals$isbn_suggested <- rec_vals$isbn_to_read_check <- rec_vals$read_url_check <- 
      rec_vals$url_suggested <- rec_vals$submit_final <- NULL
  }
  )
  
  output$progress_bar_search <- renderUI({
    shiny::validate(
      shiny::need(gsub(" ", "", input$rec_name) != "" &
                    gsub(" ", "", input$author_name) != "" &
                    gsub(" ", "", input$title_rec) != "", 
                  message = "")
    )
    progressBar(id = "pb_search", value = 0, display_pct = T)
  })
  
  output$series_book_num <- renderUI({
    shiny::validate(
      shiny::need(input$series_indicator == "yes",
                  message = "")
    )
    numericInput("series_num", label = "book #", value = 1, min = 1)
  })
  
  output$series_name <- renderUI({
    shiny::validate(
      shiny::need(input$series_indicator == "yes",
                  message = "")
    )
    textInput("series_name_text", "", value = "", placeholder = "series title")
  })
  
  updateProgressBar(session = session, id = "pb_search", value = 0)
  
  output$search_suggestion <- renderUI({
    shiny::validate(
      shiny::need(gsub(" ", "", input$rec_name) != "" &
                    gsub(" ", "", input$author_name) != "" &
                    gsub(" ", "", input$title_rec) != "", 
                  message = "")
    )
    
    actionButton("search_suggestion", "check", icon = icon("search"),
                 style = 'font-family: Cabin Sketch; margin-top: 0px !important;
                 margin-bottom: 0px !important')
  })
  
  observeEvent(input$search_suggestion, {
    rec_vals$submit_final <- NULL
    if(gsub(" ", "", input$title_rec) == "" |
       gsub(" ", "", input$author_name) == "" |
       gsub(" ", "", input$rec_name) == ""){
      
      output$error <- renderText({
        'please fill in all boxes'
      })
    } else {
      
      updateProgressBar(session = session, id = "pb_search", value = 9)
      tryCatch(
        {
          isbn_check <- gsub(": ", "", read_df %>%
                               filter(str_detect(book_title, str_squish(trimws(input$title_rec))),
                                      str_detect(author, gsub(" ", "|", str_squish(trimws(input$author_name))))) %>%
                               pull(isbn))[1]
          
          rec_vals$isbn_to_read_check <- gsub(": ", "", to_read_df %>%
                                                filter(str_detect(book_title, str_squish(trimws(input$title_rec))),
                                                       str_detect(author, gsub(" ", "|", 
                                                                               str_squish(trimws(input$author_name))))) %>%
                                                pull(isbn))[1]
          updateProgressBar(session = session, id = "pb_search", value = 16)
          if(!is.na(isbn_check)){
            rec_vals$isbn_suggested <- isbn_check
          } else if(!is.na(rec_vals$isbn_to_read_check)){
            rec_vals$isbn_suggested <- rec_vals$isbn_to_read_check
          } else {
            updateProgressBar(session = session, id = "pb_search", value = 31)
            if(input$series_indicator == "yes"){
              rec_vals$isbn_suggested <- isbn_retriever_2(title = str_squish(trimws(input$title_rec)), 
                                                          str_squish(trimws(input$author_name)), NA,
                                                          series = TRUE,
                                                          book_num = input$series_num,
                                                          series_title = str_squish(trimws(tolower(input$series_name_text)))) %>% 
                pull(isbn)
            } else {
              rec_vals$isbn_suggested <- isbn_retriever_2(title = str_squish(trimws(input$title_rec)), 
                                                          str_squish(trimws(input$author_name)), NA) %>% pull(isbn)
            }
          }
        },
        error = function(e) {
          showNotification(paste("error: could not locate book isbn"), type = "error")
        }
      )
      updateProgressBar(session = session, id = "pb_search", value = 63)
      tryCatch(
        {
          #rec_vals$url_suggested <- c()
          #for(i in 1:length(isbn_suggested)){
          rec_vals$read_url_check <- read_df %>%
            filter(gsub(": ", "", isbn) == rec_vals$isbn_suggested[1]) %>%
            pull(img_url)
          
          to_read_url_check <- to_read_df %>%
            filter(gsub(": ", "", isbn) == rec_vals$isbn_suggested[1]) %>%
            pull(img_url)
          
          if(length(rec_vals$read_url_check) > 0){
            rec_vals$url_suggested <- rec_vals$read_url_check
          } else if(length(to_read_url_check) > 0 |
                    length(to_read_url_check) > 0){
            rec_vals$url_suggested <- to_read_url_check
          } else {
            rec_vals$url_suggested <- get_cover_url(rec_vals$isbn_suggested[1], NA)
          }
          #        }
          
        },
        error = function(e){
          showNotification(paste("error: could not locate book cover"), type = "error")
        }
      )
      updateProgressBar(session = session, id = "pb_search", value = 67)
      output$suggested_book <- renderUI({
        shiny::validate(
          shiny::need(!is.na(rec_vals$url_suggested) &
                        gsub(" ", "", input$rec_name) != "" &
                        gsub(" ", "", input$author_name) != "" &
                        gsub(" ", "", input$title_rec) != "", message = "")
        )
        if(gsub(" ", "", input$rec_name) != "" &
           gsub(" ", "", input$author_name) != "" &
           gsub(" ", "", input$title_rec) != ""){
          
          all_urls <- rec_vals$url_suggested
          baseline <- length(rec_vals$isbn_suggested)
          
          list(
            div(
              a(
                tags$figure(
                  tags$img(src = rec_vals$url_suggested[1],
                           height = 250,
                           style="cursor:pointer;"),
                  tags$figcaption(
                    paste0(rec_vals$isbn_suggested[1]),
                    tags$br()
                  )
                ), href = paste0("https://www.goodreads.com/search?q=",
                                 rec_vals$isbn_suggested[1]),
                style = "display: inline-block;",
                target = "_blank")
            )
          )
        }
        
        
        
      })
      
      
      output$error <- renderText({
        shiny::validate(
          shiny::need(!is.na(rec_vals$url_suggested) &
                        gsub(" ", "", input$rec_name) != "" &
                        gsub(" ", "", input$author_name) != "" &
                        gsub(" ", "", input$title_rec) != "", message = "")
        )
        
        if(length(rec_vals$read_url_check) > 0){
          paste0("already read, see rating in the catalog!")
        } else if(length(to_read_url_check) > 0){
          paste0("if correct, please submit")
        } else {
          paste0("if correct, please submit")
        }
      })
      updateProgressBar(session = session, id = "pb_search", value = 100)
      output$submit_suggestion <- renderUI({
        shiny::validate(
          shiny::need(!is.na(rec_vals$url_suggested) &
                        length(rec_vals$read_url_check) == 0 &
                        gsub(" ", "", input$rec_name) != "" &
                        gsub(" ", "", input$author_name) != "" &
                        gsub(" ", "", input$title_rec) != "", message = "")
        )
        
        actionButton("submit", "submit", icon = icon("check"),
                     style = 'font-family: Cabin Sketch')
      })
      
      
      
    }
  })
  observeEvent(input$submit, {
    rec_vals$submit_final <- 1
    shiny::validate(
      shiny::need(!is.na(rec_vals$url_suggested) &
                    length(rec_vals$read_url_check) == 0 &
                    rec_vals$submit_final == 1 &
                    gsub(" ", "", input$rec_name) != "" &
                    gsub(" ", "", input$author_name) != "" &
                    gsub(" ", "", input$title_rec) != "", message = "")
    )
    
    if(!is.na(rec_vals$isbn_to_read_check)){
      to_read_df <- get_data("to_read")
      write_new_rec <- to_read_df %>%
        dplyr::select(isbn, recommended) %>%
        mutate(recommended = ifelse(
          gsub(": ", "", isbn) == rec_vals$isbn_to_read_check &
            str_detect(paste0(gsub(", ", " , ", recommended), " "), 
                       paste0(tolower(str_squish(trimws(input$rec_name, c("both")))), " "), 
                       negate = TRUE),
          paste0(recommended, ", ", str_squish(tolower(trimws(input$rec_name, c("both"))))),
          recommended
        )) %>%
        dplyr::select(recommended) 
      
      already_rec <- to_read_df %>%
        dplyr::select(isbn, recommended) %>%
        filter(gsub(": ", "", isbn) == rec_vals$isbn_to_read_check,
               str_detect(paste0(gsub(", ", " , ", recommended), " "), 
                          paste0(str_squish(tolower(trimws(input$rec_name, c("both")))), " ")))
      #print(dim(already_rec)[1])
      if(dim(write_new_rec)[1] > 0 & dim(already_rec)[1] == 0){
        write_new_rec %>%
          range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                      sheet = "to_read", range = "G1", reformat = FALSE)
      } 
    } else {
      maybe <- read_sheet('https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                          sheet = "maybe")
      
      #title_author <-  get_title_author_secure(rec_vals$isbn_suggested[1])
      
      maybe %>% 
        bind_rows(data.frame(c(str_squish(trimws(input$title_rec))), c(str_squish(trimws(input$author_name))), 
                             c(str_squish(trimws(input$rec_name))), 
                             c(as.character(Sys.Date())), c("0"), c(paste0(": ", rec_vals$isbn_suggested[1])),
                             c(rec_vals$url_suggested)) %>% 
                    setNames(names(maybe))) %>%
        mutate_all(tolower) %>% 
        distinct(book_title, author, recommended, isbn, .keep_all = TRUE) %>%
        range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                    sheet = "maybe", range = "A1", reformat = FALSE)
      
      
    }
    
    output$complete <- renderText({
      shiny::validate(
        shiny::need(!is.na(rec_vals$url_suggested) &
                      length(rec_vals$submit_final) > 0 &
                      length(rec_vals$read_url_check) == 0 &
                      gsub(" ", "", input$rec_name) != "" &
                      gsub(" ", "", input$author_name) != "" &
                      gsub(" ", "", input$title_rec) != "", message = "")
      )
      paste0("complete!")
    })
    rec_vals$submit_final <- NULL
    shinyjs::delay(3000,
                   shinyjs::click("clear")
    )
    shinyjs::delay(3000,
                   updateProgressBar(session = session, id = "pb_search", value = 0))
    
  })
  
  
  
  createAlert(session, "info_alert", style = "info", title = "Book Log",
              content = "Developed and maintained by Abe Eyman. Please send bug reports, 
                suggestions, and feedback directly to the developer.", 
              append = FALSE, dismiss = FALSE)
  
}


shinyApp(ui = ui, server = server)
