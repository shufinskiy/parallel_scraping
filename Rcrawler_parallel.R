# Загружаемый пакет для параллельного вычисления
library(doParallel)

# Scraping 100 страниц Лабиринта с помощью функции ContentScraper пакета  Rcrawler.
# system.time - функция, возвращающая время выполнения выражения.

sys <- system.time({
  
  # Получение 100 адресов страниц книжного магазина Лабиринт 
  list_url <- paste0("https://www.labirint.ru/books/", 
                     str_pad(c(701601:701700), 7, side = "left", pad = 0), "/")
  
  # detectCores - функция, считающая количество ядер процессора
  number_cl <- detectCores()
  
  # makePSOCKcluster - функция создающая копии R, которые работают параллельно 
  cluster <- makePSOCKcluster(number_cl)
  
  registerDoParallel(cluster)

  # Аргументы функции ContentScraper:
  # CssPatterns - один или несколько CSS шаблонов для извлечения данных.
  
  # ExcludeCSSPat - один или несколько CSS шаблонов, которые не нужно извлекать.
  # Полезно, когда нужный CSS содержит в себе ещё CSS элементы, которые нам не нужны.
  
  # ManyPerPattern - если FALSE, то извлекается только первый элемент со страницы,
  # подходящий шаблону. Если TRUE, то все элементы со страницы, подходящие по шаблону.
  
  # PatternsName - имена для каждого извлеченного элемента страницы. Нужно для
  # дальнейшего преобразования спика в таблицу, когда элементы списка неодинаковой длины
  
  t_func <- function(n){
    library(Rcrawler)
    t <- ContentScraper(n, CssPatterns = c("#product-title", 
                                           ".authors", 
                                           ".buying-price-val-number",
                                           ".buying-pricenew-val-number",
                                           ".publisher",
                                           ".isbn",
                                           ".pages2"),
                        ExcludeCSSPat = c(".prodtitle-availibility",
                                          ".js-open-block-page_count"), 
                        ManyPerPattern = FALSE,
                        PatternsName = c("title",
                                         "author",
                                         "price1",
                                         "price2",
                                         "publisher",
                                         "isbn",
                                         "page"))
    return(t)
  }
  
  # parLapply - аналог lapply функции для параллельных вычислений
  big_list12 <- parLapply(cluster, list_url, t_func)
  
  # Прекращение параллельных вычислений
  stopCluster(cluster)
})

# Создание таблицы данных из списка
f <- data.table::rbindlist(big_list12, fill = TRUE)
