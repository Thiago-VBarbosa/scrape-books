#obtem os atributos selecionados dos livros
##title, type, gender, stars, price_excl_tax, price_incl_tax, tax, n_avaliable, n_reviews

get_attributes <- function(url){
  get_attribute_unique <- function(x){
    page <- httr::GET(x,
                      httr::user_agent("Mozilla/5.0 (Windows NT 6.3; Win64; x64) 
                                      AppleWebKit/537.36 (KHTML, like Gecko) 
                                      Chrome/88.0.4324.146 Safari/537.36")) %>% 
      xml2::read_html()
    title <- page %>% 
      xml2::xml_find_all("//h1") %>% 
      xml2::xml_text()
    type <- page %>% 
      xml2::xml_find_all("//table/tr/th[text() = 'Product Type']
                          /parent::tr/td") %>% 
      xml2::xml_text()
    gender <- page %>% 
      xml2::xml_find_all("//ul[@class = 'breadcrumb']/li/a") %>% 
      .[3] %>% 
      xml2::xml_text()
    stars <- page %>% 
      xml2::xml_find_all("//div[@class = 'row']
                          /div[contains(@class,'product_main')]
                          /p[contains(@class,'star-rating')]") %>% 
      xml2::xml_attr('class') %>% 
      stringr::str_remove('star-rating ')
    price_excl_tax <- page %>% 
      xml2::xml_find_all("//table/tr/th[text() = 'Price (excl. tax)']
                          /parent::tr/td") %>% 
      xml2::xml_text() %>% 
      stringr::str_remove_all('£')
    price_incl_tax <- page %>% 
      xml2::xml_find_all("//table/tr/th[text() = 'Price (incl. tax)']
                          /parent::tr/td") %>% 
      xml2::xml_text() %>% 
      stringr::str_remove_all('£')
    tax <- page %>% 
      xml2::xml_find_all("//table/tr/th[text() = 'Tax']/parent::tr/td") %>% 
      xml2::xml_text() %>% 
      stringr::str_remove_all('£')
    n_avaliable <- page %>% 
      xml2::xml_find_all("//table/tr/th[text() = 'Availability']/parent::tr/td") %>% 
      xml2::xml_text() %>% 
      stringr::str_remove_all('[A-Za-z]|\\(|\\)| ')
    n_reviews <- page %>% 
      xml2::xml_find_all("//table/tr/th[text() = 'Number of reviews']
                          /parent::tr/td") %>% 
      xml2::xml_text()
    table_unique <- data.frame(title, type, gender, stars, price_excl_tax,
                               price_incl_tax, tax, n_avaliable, n_reviews, stringsAsFactors = FALSE)
    cat(paste(x,'\n'))
    table_unique
  }
  table <- lapply(url, get_attribute_unique) %>% 
    do.call('rbind', .)
  table
}
