library(doParallel)
sys <- system.time({
  list_url <- paste0("https://www.labirint.ru/books/", 
                     str_pad(c(701601:701700), 7, side = "left", pad = 0), "/")
  number_cl <- detectCores()
  cluster <- makePSOCKcluster(number_cl)
  registerDoParallel(cluster)
  
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
  big_list12 <- parLapply(cluster, list_url, t_func)
  stopCluster(cluster)
})
