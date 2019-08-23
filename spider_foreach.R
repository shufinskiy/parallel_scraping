library(data.table)
library(doParallel)
library(foreach)
library(snow)
library(dplyr)
# rm(list = ls())

sys2 <- system.time({
  labirint_foreach_spider <- function(start, end){
    list_url <- paste0("https://www.labirint.ru/books/", 
                       str_pad(c(start:end), 7, side = "left", pad = 0), "/")
    number_cl <- detectCores()
    # cluster <- makePSOCKcluster(number_cl)
    cluster <- makeCluster(number_cl, type = "SOCK")
    registerDoParallel(cluster)
    
    scraping_func <- function(n){
      library(rvest)
      library(purrr)
      library(stringr)
      
      book_html <- possibly(read_html, "NA")(n)
      
      isbn <- if(book_html != "NA") {
        html_nodes(book_html, css = ".isbn") %>% 
          html_text() %>%
          str_replace_all("ISBN: ", "") %>%
          str_replace_all("-", "") %>%
          str_extract("[:digit:]{1,}")
      } else { NA
      }
      
      price <- if(book_html != "NA"){
        price_html <- html_nodes(book_html, ".buying-pricenew-val-number")
        price_html1 <- html_nodes(book_html, ".buying-price-val-number")
        price <- ifelse(length(price_html) != 0, price_html, 
                        ifelse(length(price_html1) != 0, price_html1, "NA"))
        
        y <- ifelse(price != "NA", 1, 0)
        
        price <- if(y == 1){
          class(price) <- "xml_nodeset"
          price %>%
            html_text() %>%
            as.numeric()
        } else {
          NA
        }
      } else {
        NA
      }
      
      name <- if(book_html != "NA") {
        html_nodes(book_html, css = "#product-title h1") %>%
          html_text()
      } else {
        NA
      }
      
      author <- if(book_html != "NA") {
        author_html <- html_nodes(book_html, css = ".authors")
        author <- if(length(author_html) == 0){
          NA
        } else {
          author_html %>%
            html_text() %>%
            str_c(collapse = ", ")
        }
      } else {
        NA
      }
      
      publisher <- if(book_html != "NA") {
        publishing_html <- html_nodes(book_html, ".publisher a")
        publishing <- if(length(publishing_html) == 0){
          NA
        } else {
          html_text(publishing_html)
        }
      } else {
        NA
      }
      
      year <- if(book_html != "NA") {
        year_html <- html_nodes(book_html, ".publisher")
        year <- if(length(year_html) == 0){
          NA
        } else {
          year_html %>%
            html_text() %>%
            str_extract("[:digit:]{4}") %>%
            as.numeric()
        }
      } else {
        NA
      }
      
      page <- if(book_html != "NA") {
        page_html <- html_nodes(book_html, ".pages2")
        page <- if(length(page_html) == 0) {
          NA
        } else {
          page_html %>%
            html_text() %>%
            str_extract("[:digit:]{2,}") %>%
            as.numeric()
        }
      } else {
        NA
      }
      
      list12 <- list(isbn, price, name, author, publisher, year, page)
      return(list12)
    }
    
    big_list37 <- foreach(i = seq_along(list_url)) %dopar% scraping_func(list_url[i])
    stopCluster(cluster)
    
    table <- rbindlist(big_list37)
    colnames(table) <- c("ISBN",
                         "PRICE", 
                         "NAME", 
                         "AUTHOR", 
                         "PUBLISHER", 
                         "YEAR", 
                         "PAGE")
    return(table)
  }
  
  df <- labirint_foreach_spider(start = 701601, end = 701700)
})
