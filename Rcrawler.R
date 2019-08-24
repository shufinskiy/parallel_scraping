library(Rcrawler)
library(stringr)

# Получение 100 адресов страниц книжного магазина Лабиринт
list_url <- paste0("https://www.labirint.ru/books/", 
                   str_pad(c(701601:701700), 7, side = "left", pad = 0), "/")

# Scraping 100 страниц Лабиринта с помощью функции ContentScraper пакета  Rcrawler.
# system.time - функция, возвращающая время выполнения выражения.

# Аргументы функции ContentScraper:
# CssPatterns - один или несколько CSS шаблонов для извлечения данных.
# ExcludeCSSPat - один или несколько CSS шаблонов, которые не нужно извлекать.
# Полезно, когда нужный CSS содержит в себе ещё CSS элементы, которые нам не нужны.
# ManyPerPattern - если FALSE, то извлекается только первый элемент со страницы,
# подходящий шаблону. Если TRUE, то все элементы со страницы, подходящие по шаблону.

sys <- system.time(t <- ContentScraper(list_url, CssPatterns = c("#product-title", 
                                                                 ".authors", 
                                                                 ".buying-price-val-number",
                                                                 ".buying-pricenew-val-number",
                                                                 ".publisher",
                                                                 ".isbn",
                                                                 ".pages2"),
                                       ExcludeCSSPat = c(".prodtitle-availibility",
                                                         ".js-open-block-page_count"), 
                                       ManyPerPattern = FALSE))
