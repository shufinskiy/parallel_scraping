library(data.table)
library(doParallel)
library(stringr)

# Scraping 100 страниц Лабиринта с помощью функции ContentScraper пакета  Rcrawler.
# system.time - функция, возвращающая время выполнения выражения.

sys3 <- system.time({
  labirint_parallel_spider <- function(start, end) {
    
    # Получение 100 адресов страниц книжного магазина Лабиринт 
    list_url <- paste0("https://www.labirint.ru/books/", 
                       str_pad(c(start:end), 7, side = "left", pad = 0), "/")
    
    # detectCores - функция, считающая количество ядер процессора
    number_cl <- detectCores()
    
    # makePSOCKcluster - функция создающая копии R, которые работают параллельно
    cluster <- makePSOCKcluster(number_cl)
    registerDoParallel(cluster)
    
    # Функция для извлечения и очистки данных с веб-страниц сайта Лабиринт
    scraping_parellel_func <- function(n){
      library(rvest)
      library(purrr)
      library(stringr)
      
      # possibly - функция, которая использует значение по умолчанию (здесь NA)
      # каждый раз, когда возникает ошибка. Выполнение кода не останавливается.
      book_html <- possibly(read_html, "NA")(n)
      
      isbn <- if(book_html != "NA") {
        html_nodes(book_html, css = ".isbn") %>% 
          html_text() %>%
          str_replace_all("ISBN: ", "") %>%
          str_replace_all("-", "") %>%
          str_extract("[:digit:]{1,}")
      } else { 
        NA
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
      
      list <- list(isbn, price, name, author, publisher, year, page)
      return(list)
    }
    
    # parLapply - аналог lapply функции для параллельных вычислений
    big_list <- parLapply(cluster, list_url, scraping_parellel_func)
    
    # Прекращение параллельных вычислений
    stopCluster(cluster)
    
    # Создание таблицы данных из списка
    table <- rbindlist(big_list12)
    colnames(table) <- c("ISBN",
                         "PRICE", 
                         "NAME", 
                         "AUTHOR", 
                         "PUBLISHER", 
                         "YEAR", 
                         "PAGE")
    return(table)
    
  }
  
  df <- labirint_parallel_spider(701601, 701700)
})
