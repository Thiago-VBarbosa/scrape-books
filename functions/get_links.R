#obtem os links dos livros em cada pagina do catalogo

get_links <- function(url){
  get_link_unique <- function(x){
    page <- httr::GET(x, 
                      httr::user_agent("Mozilla/5.0 (Windows NT 6.3; Win64; x64) 
                                        AppleWebKit/537.36 (KHTML, like Gecko) 
                                        Chrome/88.0.4324.146 Safari/537.36"))
    links_page <- page %>% 
      xml2::read_html() %>% 
      xml2::xml_find_all("//div/ol[@class = 'row']/li
                          /article[@class = 'product_pod']/h3/a") %>% 
      xml2::xml_attr('href') %>% 
      paste0("http://books.toscrape.com/catalogue/",.)
    cat(paste(x, '\n'))
    links_page
  }
  all_links <- lapply(url, get_link_unique) %>% 
    do.call('c', .)
  all_links
}