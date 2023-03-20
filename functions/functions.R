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
library(httr)
library(xml2)
library(xfun)
font_add_google("Cabin Sketch", "Cabin Sketch")
showtext_auto()

isbn_retriever_2 <- function(title, author, isbn, series = FALSE, book_num = 1, series_title = ""){
  if(is.na(isbn)){
    title <- str_squish(trimws(tolower(title), c("both")))
    author <- str_squish(trimws(tolower(author), c("both")))
    if(series){
      book_num <- paste0("+", book_num)
      isbn_url <- paste0("https://bookscouter.com/search?query=", 
                         paste0(gsub(" ", "+", str_squish(tolower(series_title)))), "+",
                         paste0(gsub(" ", "+", title), "+", 
                                tail(str_split(author, " ")[[1]], 1)), book_num)
    } else {
      isbn_url <- paste0("https://bookscouter.com/search?query=", 
                         paste0(gsub(" ", "+", title), "+", 
                                tail(str_split(author, " ")[[1]], 1)))
    }
    
    s <- session(isbn_url)
    authors <- s %>% 
      html_nodes(".BookContent_b1wo7oya:nth-child(1) .BookDetailContent_bprexjj:nth-child(1) .BookText_b1ofiyxa") %>%
      html_text()
    
    isbns <- s %>% 
      html_nodes(".BookCodesContent_bzopiem .BookDetailContent_bprexjj:nth-child(1) .BookText_b1ofiyxa") %>% 
      html_text()
    titles <- s %>% html_nodes(".BookTitle_b1xw0hok") %>% html_text()
    searched_df <- data.frame(titles, authors, isbns) 
    
    filtered_df <- searched_df %>%
      mutate(titles = tolower(titles),
             titles =  gsub("???",'', titles),
             titles = gsub(",", "", titles), 
             titles =  gsub("\\.",'', titles),
             titles =  gsub("'",'', titles),
             authors =  gsub("'",'', authors),
             authors = tolower(gsub("Authors:", "", authors))) %>%
      filter(str_detect(authors, tolower(tail(str_split(author, " ")[[1]], 1))),
             str_detect(titles, title)) %>%
      filter(str_detect(tolower(titles), "boxed set", negate = TRUE))
    
    return_isbn <- (filtered_df %>% setNames(c("book_title", "author", "isbn")))
  } else {
    return_isbn <- data.frame(book_title = c(tolower(title)), 
                              author = c(tolower(author)), 
                              isbn = c(isbn))
  }
  if(dim(return_isbn)[1] == 0){
    stop("error: cannot find book isbn")
  } else {
    return_isbn
  }
}

page_isbn_retriever <- function(title, author, isbn, pages){
  if(is.na(isbn)){
    title <- tolower(title)
    author <- tolower(author)
    #print("checking first isbn")
    isbn_url <- paste0("https://bookscouter.com/search?query=", 
                       paste0(gsub(" ", "+", title), "+", 
                              tail(str_split(author, " ")[[1]], 1)))
    
    s <- session(isbn_url)
    authors <- s %>% 
      html_nodes(".BookContent_b1wo7oya:nth-child(1) .BookDetailContent_bprexjj:nth-child(1) .BookText_b1ofiyxa") %>%
      html_text()
    
    isbns <- s %>% 
      html_nodes(".BookCodesContent_bzopiem .BookDetailContent_bprexjj:nth-child(1) .BookText_b1ofiyxa") %>% 
      html_text()
    titles <- s %>% html_nodes(".BookTitle_b1xw0hok") %>% html_text()
    searched_df <- data.frame(titles, authors, isbns) 
    
    filtered_df <- searched_df %>%
      mutate(titles = tolower(titles),
             titles =  gsub("???",'', titles),
             titles = gsub(",", "", titles), 
             titles =  gsub("\\.",'', titles),
             titles =  gsub("'",'', titles),
             authors =  gsub("'",'', authors),
             authors = tolower(gsub("Authors:", "", authors))) %>%
      filter(str_detect(authors, tolower(tail(str_split(author, " ")[[1]], 1))),
             str_detect(titles, title)) %>%
      slice(1) %>%
      dplyr::select(isbns) 
    
    return_isbn <- (filtered_df %>% pull(isbns))
  } else {
    return_isbn <- isbn
  }
  #print("found first isbn")
  #print(return_isbn)
  if(is.na(pages)){
    #print("searching pages")
    page_url <- paste0("https://www.readinglength.com/book/isbn-", 
                       tail(str_split(return_isbn, ": ")[[1]], 1))
    html_url <- session(page_url)
    page_val <- html_url %>%
      html_nodes(".book-data .ng-star-inserted:nth-child(3) p") %>%
      html_text()
    if(is.na(page_val)){
      return_page_val <- NA
    } else {
      return_page_val <- (page_val)
    }
  } else if(!is.na(pages)){
    return_page_val <- pages
  }
  
  #print("found pages")
  if(is.na(return_isbn) || !str_detect(return_page_val, "[0-9]")){
    #print("error in pages, trying other isbn")
    isbn_url2 <- paste0("https://isbndb.com/search/books/", 
                        paste0(gsub(" ", "%2B", title), 
                               "%2B", gsub(" ", "%2B", 
                                           tail(str_split(author, " ")[[1]], 1))))
    html_url <- session(isbn_url2)
    authors <- html_url %>% html_nodes("dt:nth-child(1)") %>% html_text()
    isbns <- html_url %>% html_nodes("dt:nth-child(2)") %>% html_text()
    titles <- html_url %>% html_nodes(".search-result-title a") %>% html_text()
    searched_df <- data.frame(titles, authors, isbns) %>%
      mutate(titles = tolower(titles),
             titles =  gsub("???",'', titles),
             titles =  gsub("'",'', titles),
             authors =  gsub("'",'', authors),
             authors = tolower(gsub("Authors:", "", authors))) %>%
      filter(str_detect(authors, tail(str_split(author, " ")[[1]], 1)),
             str_detect(titles, title)) %>%
      dplyr::select(isbns) %>%
      slice(1)
    
    return_isbn <- (searched_df %>% pull(isbns))
    page_url <- paste0("https://www.readinglength.com/book/isbn-", 
                       tail(str_split(return_isbn, ": ")[[1]], 1))
    html_url <- read_html(page_url)
    page_val <- html_url %>%
      html_nodes(".book-data .ng-star-inserted:nth-child(3) p") %>%
      html_text()
    if(is.na(page_val)){
      return_page_val <- NA
    } else {
      return_page_val <- (page_val)
    }
  } else {
    return_isbn <- return_isbn
  }
  
  
  return(paste0(return_isbn, "-", return_page_val))
}

get_title_text <- function(title){
  tit_length <- nchar(title)
  if(tit_length > 18){
    parts <- str_split(title, " ")[[1]]
    HTML(paste(
      paste(parts[1:ceiling(length(parts)/2)], collapse = " "),
      paste(tail(parts, (length(parts) -ceiling(length(parts)/2))), collapse = " "),
      sep="<br/>"
    )
    )
  } else {
    title
  }
}

get_cover_url <- function(isbn, img_url){
  if(!is.na(img_url)){
    img_url
  } else {
    book_url <- paste0("https://www.goodreads.com/search?q=", isbn)
    atts <- read_html(book_url) %>%
    html_nodes('.ResponsiveImage') %>% 
    html_attr('src')
    # s <- session(book_url)
    # atts <- (s %>% html_nodes('.ResponsiveImage') %>% html_attr('src'))
    if(length(atts) == 0){
      amz_url <- paste0("https://www.amazon.com/s?k=", isbn, "&i=stripbooks")
      atts <- read_html(amz_url) %>%
        html_nodes('.s-image') %>%
        html_attr('src')
      if(length(atts) == 0){
        stop("cannot find book cover")
      }
    } 
    atts
  }
}


get_title_author_secure <- function(isbn){
  book_url <- paste0("https://www.goodreads.com/search?q=", isbn)
  s <- session(book_url)
  text <- (s %>% html_nodes('.ContributorLinksList .ContributorLink__name , .Text__title1') %>% html_text())
  if(length(text) == 0){
    stop("cannot confirm book title + author")
  } else {
    text <- tolower(text)
  }
  text
}

get_audio_length <- function(title, author, length){
  if(!is.na(length)){
    return(length)
  } else if(is.na(length)){
    searcher <- paste0(gsub(" ", "+", title), "+audiobook+", tail(str_split(author,  " ")[[1]], 1)) 
    base_url <- "https://www.audible.com/search?keywords="
    test_url <- read_html(paste0(base_url, searcher))
    x <- test_url %>% html_nodes('.bc-col-6 .bc-col-12 , .bc-size-medium , .subtitle') %>% html_text()
    
    pieces <- gsub("[\r\n]", "", x) %>%
      tolower() %>%
      gsub(pattern = "???",replacement = '')  %>%
      gsub(pattern = "'",replacement = '') %>%
      keep(~ str_detect(.x, tail(str_split(author,  " ")[[1]], 1))) %>%
      str_squish()
    
    pieces %>%
      as.data.frame() %>%
      setNames("full") %>%
      rowwise() %>%
      mutate(full = str_split(full, "release date:")[[1]][1],
             titles = str_split(full, " by: ", n = 2)[[1]][1],
             full = str_split(full, " by: ", n = 2)[[1]][2], 
             author = str_split(full, " narrated by: ", n = 2)[[1]][1], 
             full = str_split(full, " narrated by: ", n = 2)[[1]][2],
             length = str_split(full, " length: ", n = 2)[[1]][2]) %>%
      filter(!is.na(length)) %>%
      dplyr::select(-full) %>%
      filter(str_detect(author, tail(str_split(author,  " ")[[1]], 1)),
             str_detect(title, title)) %>%
      head(1) %>%
      pull(length)
  }
  
}


get_author_gender <- function(isbn, author_gender, fiction_bool, genre){
  if(!is.na(author_gender) & !is.na(fiction_bool) & !is.na(genre)){
    return(paste0(author_gender, "_", fiction_bool, "_", genre))
  } else if(is.na(author_gender) | is.na(fiction_bool) | is.na(genre)){
    #isbn <- "0765385872"
    book_url <- paste0("https://www.goodreads.com/search?q=", isbn)
    s <- session(book_url)
    if(is.na(author_gender)){
      x <- s %>% 
        html_nodes(".TruncatedContent__text--medium .DetailsLayoutRightParagraph__widthConstrained") %>% 
        html_text()
      new_x <- tolower(gsub('[[:punct:] ]+',' ', x))
      she_count <- sum(map2_dbl(new_x, c(" she ", " her ", " hers "), str_count))
      he_count <- sum(map2_dbl(new_x, c(" he ", " him ", " his "), str_count))
      they_count <- sum(map2_dbl(new_x, c(" they ", " them ", " theirs "), str_count))
      counts <- c("fem" = she_count, "masc" = he_count, "nb" = they_count)
      author_gender <- names(counts[counts == max(counts)])
      if(length(author_gender) == 1){
        author_gender
      } else {
        NA
      }
      fiction_bool <- fiction_bool
    }
    if(is.na(fiction_bool)){
      x <- s %>% 
        html_nodes(".Button--tag-inline .Button__labelItem") %>% 
        html_text()
      fic_count <- sum(str_detect(tolower(str_pad(x, 30, "both")), "[^n]fiction"))
      nonfic_count <- sum(str_detect(gsub("-", "", tolower(x)), "nonfiction"))
      if(fic_count >= 1 & nonfic_count == 0){
        fiction_bool <- "fiction"
      } else if(nonfic_count >= 1 & fic_count < nonfic_count){
        fiction_bool <- "non-fiction"
      } else {
        fiction_bool <- fiction_bool
      }
      
    }
    if(is.na(genre)){
      x <- s %>% 
        html_nodes(".Button--tag-inline .Button__labelItem") %>% 
        html_text() 
      x <- gsub("-", " ", x)
      x_sub <- c()
      not_include <- c("...more", "retellings", 
                       "borrowed-library", "ala-2010", 
                       "arc-or-galley", "best of the best", 
                       "favorites i think", paste(2010:2027), 
                       "young adult fantasy", "fiction")
      for(i in x){
        if((tolower(i) %in% not_include == FALSE) & (str_detect(tolower(i), "stars") == FALSE) &
           (str_detect(tolower(i), "releases") ==  FALSE)){
          x_sub <- c(x_sub, i)
        }
      }
      x_sub <- x_sub[0:9]
      x_new <- c()
      for(i in x_sub){
        if(sum(str_detect(x_new, tolower(i)), na.rm = TRUE) == 0){
          x_new <- c(x_new, tolower(i))
        }
      }
      x_new <- x_new[!is.na(x_new)]
      
      genre <- paste(tolower(x_new[0:4]), collapse = ", ")
      genre <- gsub(", NA", "", genre)
      
    }
    return(paste0(author_gender, "_", fiction_bool, "_", genre))
    
  }
  
}


get_data <- function(option = "read"){
  if(option == "read"){
    read_df <- read_sheet('https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=0')
    read_df %>%
      rowwise() %>%
      mutate(book_title = gsub("'", "", book_title))
  } else if(option == "to_read"){
    to_read_df <- read_sheet('https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', sheet = "to_read")
    to_read_df$pages <- as.character(to_read_df$pages)
    to_read_df %>%
      rowwise() %>%
      mutate(book_title = gsub("'", "", book_title), 
             pages = ifelse(pages == "NULL", NA, pages))
  }
  
}

# get_local_data <- function(option = "read"){
#     if(option == "read"){
#       read_df <- fread("data/read_df.csv")
#       read_df %>%
#         rowwise() %>%
#         mutate(book_title = gsub("'", "", book_title))
#     } else if(option == "to_read"){
#       to_read_df <- fread("data/to_read_df.csv")
#       
#       to_read_df %>%
#         rowwise() %>%
#         mutate(book_title = gsub("'", "", book_title))
#     }  
# }

clean_read_df <- function(){
  read_df <- get_data("read")
  read_df %>%
    mutate(days_to_finish = difftime(date_finished, 
                                     date_started, units = "days"),
           midpoint_date = interval(ymd(date_started), ymd(date_finished))@start + 
             as.duration(interval(ymd(date_started), ymd(date_finished))/2)
    ) %>%
    mutate(hours = str_split(length, "hrs&")[[1]][1],
           hours = gsub("hrs", "", hours),
           mins = str_split(length, "hrs&")[[1]][2],
           mins = gsub(" mins", "", mins), 
           mins = ifelse(is.na(mins), "0", mins),
           hour_length = as.numeric(hours) + (as.numeric(mins)/60),
           hour_length = ceiling(hour_length*4) / 4) %>%
    mutate(date_finished = as.Date(date_finished)) %>%
    as.data.frame()
}

make_big_plot <- function(df, year_filter){
  if(year_filter == "all"){
    year_filter <- seq(2022, year(Sys.Date()), by = 1)
  } else if(year_filter == "last 6 months"){
    good_months <- map_dbl(seq(Sys.Date(), length = 6, by = "-1 months"), month)
    year_filter <- seq(2022, year(Sys.Date()), by = 1)
    df <- df %>%
      mutate(midpoint_month_num = month(as.Date(as.yearmon(midpoint_month)))) %>%
      filter(midpoint_month_num %in% good_months)
  }
  
  ggplotly(
    df %>%
      mutate(midpoint_year_num = year(as.Date(as.yearmon(midpoint_month)))) %>%
      filter(midpoint_year_num %in% year_filter) %>%
      as.data.frame() %>%
      mutate(text = ifelse(!is.na(rating),
                           paste0("book title: ", book_title, "\n",
                                  "book rating: ", rating, "\n",
                                  "days taken to read: ", days_to_finish, "\n",
                                  "audiobook length: ", hour_length, " hours", "\n",
                                  "book length: ", pages, " pages", "\n", 
                                  "monthly hours read: ", month_total_hours, "\n",
                                  "monthly books read: ", month_total, "\n"
                           ),
                           paste0("book title: ", book_title, "\n",
                                  "book rating: ", "tbd", "\n",
                                  "days taken to read: ", "tbd", "\n",
                                  "audiobook length: ", hour_length, " hours", "\n",
                                  "book length: ", pages, " pages", "\n", 
                                  "monthly hours read: ", month_total_hours, "\n",
                                  "monthly books read: ", month_total, "\n"
                           ))) %>%
      ggplot(aes(x = as.Date(as.yearmon(midpoint_month)), 
                 fill = rating,
                 color = forcats::fct_reorder(book_title, as.Date(date_finished, format = "%Y-%m-%d"),
                                              .desc = TRUE, .na_rm = FALSE),
                 alpha = month_total,
                 text = text)) +
      geom_bar() +
      scale_x_date(date_breaks="1 month", date_labels="%m-%y")  + 
      scale_fill_gradient2(low = "white", high = "firebrick4",
                           limits = c(1, 5), midpoint = 1.5, 
                           guide = "none") + 
      scale_alpha_continuous(range = c(.69, 1), limits = c(1, 5)) +
      scale_color_manual(guide = "none", 
                         values = rep("white", 100)) +
      dark_theme_bw() +
      #coord_flip() +
      theme(plot.background = element_rect(fill = "#101010")) +
      labs(x = "Month", y = "Total Hours Read", 
           fill = "Rating", alpha = "Rating")
    ,
    tooltip = 'text'
  ) %>%
    plotly::config(displayModeBar = FALSE) %>%
    layout(xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE),
           hoverlabel = list(font=list(family = "Cabin Sketch", 
                                       size = 16)),
           font = list(family = "Cabin Sketch"),
           xaxis = list(font = list(size = 16)),
           yaxis = list(font = list(size = 16)),
           title = list(text = paste0("<sup>", "tap boxes in graph for more deails", "<br> </sup>"),
                        font = list(size = 15))) %>%
    hide_legend()
}

make_data <- function(read_df){
  titles <- unique(read_df$book_title)
  
  df <- read_df %>%
    mutate(midpoint_month = format(midpoint_date, "%Y-%m"), 
           midpoint_month = ifelse(is.na(midpoint_month),
                                   format(Sys.Date(), "%Y-%m"),
                                   midpoint_month)) %>%
    group_by(midpoint_month) %>%
    add_tally(name = "month_total") %>%
    mutate(month_total_hours = sum(hour_length)) %>%
    dplyr::select(book_title, hour_length,
                  midpoint_month, month_total, 
                  month_total_hours, days_to_finish,
                  rating, pages, date_finished) %>%
    as.data.frame() 
  
  df %>% 
    group_by(book_title) %>%
    group_modify(~ .x %>%
                   add_row(date_finished = NA, hour_length = 1, .before = 1)) %>% 
    complete(hour_length = seq(min(hour_length, na.rm = TRUE),
                               max(round(hour_length), na.rm = TRUE), by = 1)) %>%
    slice(-1) %>%
    ungroup %>%
    mutate(hour = 1) %>%  
    arrange(book_title, midpoint_month) %>% 
    fill(names(df)) %>%
    dplyr::select(-hour_length) %>%
    left_join(
      df %>%
        mutate(curr_book = ifelse(is.na(rating), 1, 0)) %>%
        dplyr::select(book_title, hour_length, curr_book)
    ) %>% 
    mutate(rating = ifelse(curr_book == 1, NA, rating),
           date_finished = ifelse(curr_book == 1, NA, date_finished), 
           days_to_finish = ifelse(curr_book == 1, NA, days_to_finish)) %>%
    dplyr::select("book_title", "hour", "hour_length", "midpoint_month", "month_total",
                  "month_total_hours", "days_to_finish", "rating", "pages", "date_finished") %>%
    as.data.frame()
}

make_detailed_plot <- function(df, min_date, max_date){
  
  ggplotly(
    df %>%
      filter(date_finished <= max_date,
             date_finished >= min_date) %>%
      ggplot(aes(x = as.Date(date_finished), y = rating,
                 color = fiction_bool,
                 text = paste0("book title: ", book_title, "\n",
                               "book type: ", fiction_bool, "\n",
                               "book rating: ", rating, "\n",
                               "date finished: ", format(date_finished, "%m-%d-%Y"), "\n",
                               "audiobook length: ", hour_length, " hours", "\n",
                               "book length: ", pages, " pages", "\n",
                               "days taken to read: ", days_to_finish, "\n"))) +
      geom_point() +
      dark_theme_bw() +
      scale_x_date(date_breaks="1 week", date_labels =  "%b %d, %y") +
      scale_color_manual(values = c("grey", "white")) +
      labs(title = "", x = "Date Finished", y = "Rating", 
           color = "") +
      theme(plot.background = element_rect(fill = "#101010")) +
      theme(axis.text.x = element_text(angle = 30), 
            legend.position = "top", 
            legend.background = element_rect(fill = "#101010")),
    tooltip = 'text'
  ) %>%
    plotly::config(displayModeBar = FALSE) %>%
    layout(xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE),
           hoverlabel = list(font=list(family = "Cabin Sketch", 
                                       size = 16)),
           font = list(family = "Cabin Sketch"),
           xaxis = list(font = list(size = 16)),
           yaxis = list(font = list(size = 16)),
           title = list(text = paste0("<sup>", "tap points for more details", "<br> </sup>"),
                        font = list(size = 15)),
           legend = list(
             orientation = "h", y = 1.11
           ))
    
}


make_long_genre_df <- function(read_df){
  max_num_genre <- read_df %>%
    rowwise() %>%
    mutate(count_comma = str_count(genre, ",")) %>%
    arrange(desc(count_comma)) %>%
    head(1) %>%
    pull(count_comma) + 1
  
  genre_cols <- map_chr(seq(1, max_num_genre), function(x) paste0("genre", x))
  
  long_genre_df <- read_df %>%
    separate(col = genre, into = all_of(genre_cols),
             sep = ",") %>%
    pivot_longer(all_of(genre_cols), 
                 names_to = 'category', values_to = 'genre') %>%
    dplyr::select(book_title, rating, genre, everything(), -category) %>%
    filter(!is.na(genre)) %>%
    rowwise() %>%
    mutate(genre = str_squish(trimws(genre, c("both"))))
  
  long_genre_df
}

get_top_genres <- function(df, read_df, type = "fiction"){
  top_genre_df <- df %>%
    filter(fiction_bool == type) %>%
    filter(genre != "nonfiction") %>%
    filter(!is.na(rating)) %>%
    group_by(genre) %>%
    mutate(median_rating = median(rating)) %>%
    add_tally() %>%
    dplyr::select(genre, book_title, median_rating, n) %>%
    as.data.frame() 
  
  top_genre_df %>%
    pivot_wider(id_cols = genre, 
                values_from = book_title, 
                names_from = book_title) %>%
    unite("books", 2:nrow(read_df %>% 
                            filter(fiction_bool == type)), 
          sep = ", ") %>%
    rowwise() %>%
    mutate(books = gsub(", NA", "", books),
           books = gsub("NA, ", "", books),
           books = gsub(", ", ", \n", books)) %>%
    left_join(top_genre_df %>%
                dplyr::select(genre, median_rating, n) %>%
                as.data.frame() %>%
                distinct()) %>%
    mutate(rating_dummy = (2*median_rating + n)/2) %>%
    arrange(desc(rating_dummy)) %>%
    dplyr::select(genre, median_rating, n) %>%
    filter(n > 1) %>%
    rename("n_books" = n)
}

six_month_email_reminder <- function(read_df){
  today <- Sys.Date()
  email <- read_sheet('https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                           sheet = "email_day") %>%
    pull(day_check) %>%
    as.Date()
  if(!today == email){
    email_df <- read_df %>%
      rowwise() %>%
      mutate(send_email_num = case_when(
        date_finished == (Sys.Date() - months(1)) ~ 1,
        date_finished == (Sys.Date() - months(6)) ~ 6
      )) %>%
        filter(send_email_num %in% c(1, 6))
    
    if(dim(email_df)[1] > 0){
      for(i in 1:nrow(email_df)){
        email <- gm_mime() %>%
          gm_to("aecvikings@gmail.com") %>%
          gm_from("AbrahamEymanCasey@gmail.com") %>%
          gm_subject(paste0(email_df$send_email_num[i], " Month Rating Reminder: ", email_df$book_title[i])) %>%
          gm_html_body(
            paste0("<b>Abe</b>, <br> <br>", "    You finished reading <b>", email_df$book_title[i], "</b> ", 
            numbers_to_words(email_df$send_email_num[i]), " months ago today. 
        At the time of reading, you rated this book <i>", email_df$rating[i], "/5</i>.
               It's time to give this book your ", email_df$send_email_num[i], " month review! Please click 
               <a href='https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=0'>
               this link</a> 
               to review ", email_df$book_title[i], ". <br> <br> <br> Happy Reading, <br> <br> <i>Reading Log Bot</i>")
          )
        
        gm_send_message(email)
      }
    }
    new_email <- data.frame(c(Sys.Date())) %>%
      setNames(c("day_check"))
    new_email %>% 
      range_write(ss = 'https://docs.google.com/spreadsheets/d/186m_M61F8kKnMe9XUCO0cI2LMPdcueFJTJN7E8-vXWo/edit#gid=2004428247', 
                  sheet = "email_day", range = "A1", reformat = FALSE)
    
  }
}
