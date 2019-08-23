library(Rcrawler)
library(stringr)

list_url <- paste0("https://www.labirint.ru/books/", 
                   str_pad(c(701601:701700), 7, side = "left", pad = 0), "/")

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
