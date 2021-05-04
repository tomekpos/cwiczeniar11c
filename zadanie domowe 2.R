library(rvest)
library(stringr)
library(progress)
library(dplyr)

link = 'https://www.otomoto.pl/osobowe/hyundai/'


N = 5
link = 'https://www.otomoto.pl/osobowe/hyundai/?search%5Border%5D=created_at%3Adesc&page='
results <- data.frame('price'=numeric(), 'year'=numeric(), 'mileage'=numeric(), 'fuel'=c(), 'region'=c(), 'engine'=numeric())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  download.file(paste0(link, i), destfile = "scrapedpage.html", quiet=TRUE)
  page_nodes <- read_html('scrapedpage.html') %>% html_nodes(xpath = '//*[@class="offer-item__content ds-details-container"]')
  
  for(node in page_nodes){
    
    #-------------- 
    node %>%
      html_nodes(xpath = './/*[@class="offer-title__link"]') %>%
      html_text() -> what_model

    #--------------
    
    node %>%
      html_nodes(xpath = ".//span[@class='offer-price__number ds-price-number']/span[1]") %>%
      html_text() %>%
      str_replace_all(., " ", "") %>%
      as.numeric() -> price
    
    node %>%
      html_nodes(xpath = './/*[@data-code="year"]') %>%
      html_text() %>%
      str_extract(., regex('\\d{4}')) %>%
      as.numeric() -> year
    
    node %>%
      html_nodes(xpath = './/*[@data-code="mileage"]') %>%
      html_text() %>%
      str_replace_all(., " ", "") %>%
      str_extract(regex('[0-9]+')) %>%
      as.numeric() -> mileage
    
    node %>% 
      html_nodes(xpath = './/*[@data-code="engine_capacity"]/span') %>%
      html_text() %>%
      str_replace_all(., " ", "") %>%
      str_extract(regex('[0-9]+')) %>%
      as.numeric() -> engine
    
    node %>%
      html_nodes(xpath = './/*[@data-code="fuel_type"]/span') %>%
      html_text() -> fuel
    
    node %>%
      html_nodes(xpath = './/*[@class="ds-location-region"]') %>%
      html_text() -> region
    
    
    if(length(price) == 0) price <- NA
    if(length(year) == 0) year <- NA
    if(length(mileage) == 0) mileage <- NA
    if(length(fuel) == 0) fuel <- NA
    if(length(engine) == 0) engine <- NA
    if(length(region) == 0) region <- NA
    
    if(length(what_model) == 0) what_model <- NA
    
    results <- rbind(results, data.frame('model' = what_model, 'price' = price, 'year' = year, 'mileage' = mileage, 'fuel' = fuel,
                                         'region' = region, 'engine' = engine))
  }
  pb$tick()
}
