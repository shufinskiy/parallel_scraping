library(rvest)
library(stringr)
library(purrr)
# Scraping 100 страниц Лабиринта с помощью функции ContentScraper пакета  Rcrawler.
# system.time - функция, возвращающая время выполнения выражения.

sys1 <- system.time({
  labirint_spider <- function(start, end){
    
    # Получение 100 адресов страниц книжного магазина Лабиринт 
    list_url <- paste0("https://www.labirint.ru/books/", 
                       str_pad(c(start:end), 7, side = "left", pad = 0), "/")
    
    # "Прочтение" всех HTML-страниц из полученных адресов
    # possibly - функция, которая использует значение по умолчанию (здесь NA)
    # каждый раз, когда возникает ошибка. Выполнение кода не останавливается.
    list_html <- lapply(list_url, function(n){
      book_html <- possibly(read_html, "NA")(n)
    })
    
    ISBN <- unlist(lapply(list_html, function(n){
      
      isbn <- if(n != "NA"){
        isbn <- n %>%
          html_nodes(".isbn") %>%
          html_text() %>%
          str_replace_all("ISBN: ", "") %>%
          str_replace_all("-", "") %>%
          str_extract("[:digit:]{1,}")
      } else {
        NA
      }
    }))
    
    PRICE <- unlist(lapply(list_html, function(n){
      
      price <- if(n != "NA") {
        price_html <- html_nodes(n, ".buying-pricenew-val-number")
        price_html1 <- html_nodes(n, ".buying-price-val-number")
        price <- ifelse(length(price_html) != 0, price_html, 
                        ifelse(length(price_html1) != 0, price_html1, "NA"))
        
        y <- ifelse(price != "NA", 1, 0)
        
        price <- if(y == 1){
          class(price) <- "xml_nodeset"
          price <- price %>%
            html_text() %>%
            as.numeric()
        } else {
          NA
        }
      } else {
        NA
      }
    }))
    
    NAME <- unlist(lapply(list_html, function(n){
      
      name <- if(n != "NA") {
        name <- n %>%
          html_nodes("#product-title h1") %>%
          html_text()
      } else {
        NA
      }
      
    }))
    
    AUTHOR <- unlist(lapply(list_html, function(n){
      
      author <- if(n != "NA"){
        author_html <- html_nodes(n, ".authors")
        author <- if(length(author_html) == 0){
          NA
        } else {
          author <- author_html %>%
            html_text() %>%
            str_c(collapse = ", ")
        }
      } else {
        NA
      }
    }))
    
    PUBLISHER <- unlist(lapply(list_html, function(n){
      publishing <- if(n != "NA") {
        publishing_html <- html_nodes(n, ".publisher a")
        publishing <- if(length(publishing_html) == 0){
          NA
        } else {
          publishing <- html_text(publishing_html)
        }
      } else {
        NA
      }
    }))
    
    YEAR <- unlist(lapply(list_html, function(n){
      
      year <- if(n != "NA") {
        year_html <- html_nodes(n, ".publisher")
        year <- if(length(year_html) == 0){
          NA
        } else {
          year <- year_html %>%
            html_text() %>%
            str_extract("[:digit:]{4}") %>%
            as.numeric()
        }
      } else {
        NA
      }
    }))
    
    PAGE <- unlist(lapply(list_html, function(n){
      page <- if(n != "NA") {
        page_html <- html_nodes(n, ".pages2")
        page <- if(length(page_html) == 0) {
          NA
        } else {
          page <- page_html %>%
            html_text() %>%
            str_extract("[:digit:]{2,}") %>%
            as.numeric()
        }
      } else {
        NA
      }
    }))
    
    df <- data.frame(ISBN = ISBN, 
                     PRICE_LAB = PRICE, 
                     NAME_LAB = NAME,
                     AUTHOR_LAB = AUTHOR,
                     PUBLIHER_LAB = PUBLISHER,
                     YEAR_LAB = YEAR,
                     PAGE_LAB = PAGE,
                     stringsAsFactors = FALSE)
    return(df)
  }
  
  table <- labirint_spider(701601, 701700)
})
