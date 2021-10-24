options(encoding = "UTF-8")
library(magrittr)

#importando funções para raspar as informações
source("functions/get_links.R")
source("functions/get_attributes.R")

#gerando paginas dos catalogos dos livros
urls_base <- paste0("http://books.toscrape.com/catalogue/page-",1:50,".html")

#obtendo os links dos livros a partir das paginas do catalogo
all_links <- get_links(urls_base)

#gerando clusters para realizar as requisições HTTP paralelamente
library(parallel)
cl <- makeCluster(3)

clusterEvalQ(cl, {
  library(magrittr)
})

#fazendo as requisições http paralelamente
system.time(books <- clusterApply(cl, all_links, get_attributes))

stopCluster(cl)

#gerando data.frame com os dados
books <- books %>% 
  do.call('rbind',.)

#converção de variaveis
books <- books %>%
  dplyr::mutate(price_incl_tax = as.numeric(price_incl_tax),
                price_excl_tax = as.double(price_excl_tax),
                tax = as.double(tax),
                n_avaliable = as.double(n_avaliable),
                n_reviews = as.double(n_reviews),
                gender = as.factor(gender),
                stars = factor(stars, levels = c('One', 'Two', 'Three', 'Four', 'Five'))) %>% 
  tibble::as_tibble()

dplyr::glimpse(books)

#exportando o data.frame em CSV
dir.create("data")
write.csv2(books, "data/books.CSV",row.names = FALSE)

#gerando alguns plots

dir.create("output")
#quantidade de livros por numero de estrelas
png("output/qtde_books_for_nstars.png",
    width = 742,height = 664)

books %>% 
  ggplot2::ggplot(ggplot2::aes(x = stars)) +
  ggplot2::geom_bar() + 
  ggplot2::xlab("Estrelas") +
  ggplot2::ylab("Quantidade") +
  ggplot2::ggtitle("Quantidade de Livros por Estrelas")

dev.off()

#quantidade de livros por genero
png("output/qtde_books_for_gender.png",
    width = 742,height = 664)

books %>% 
  dplyr::group_by(gender) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(ggplot2::aes(y = reorder(gender, n), x = n)) +
  ggplot2::geom_col() +
  ggplot2::xlab("Quantidade") +
  ggplot2::ylab("Gênero")+
  ggplot2::ggtitle("Quantidade de Livros por Gênero")

dev.off()

#quantidade disponivel de livros por genero
png("output/qtde_avaliable_for_gender.png",
    width = 742,height = 664)

books %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(n_avaliable = sum(n_avaliable)) %>% 
  ggplot2::ggplot(ggplot2::aes(y = reorder(gender, n_avaliable), x = n_avaliable)) +
  ggplot2::geom_col() +
  ggplot2::xlab("Quantidade disponível") +
  ggplot2::ylab("Gênero") +
  ggplot2::ggtitle("Quantidade de exemplares disponíveis por Gênero")

dev.off()

#preco medio dos livros por genero
png("output/price_mean_for_gender.png",
    width = 742,height = 664)

books %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(price_mean = mean(price_incl_tax)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = price_mean, y  = reorder(gender, price_mean))) +
  ggplot2::geom_col() +
  ggplot2::xlab("Preço médio") +
  ggplot2::ylab("Gênero") +
  ggplot2::ggtitle("Preço médio (EUR) de Livros por Gênero")

dev.off()

