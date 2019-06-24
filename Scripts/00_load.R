library(sRa)
library(rvest)
library(httr)
library(lubridate)
library(chron)
library(glue)

read_page = function(link){
  read_html(link) %>% 
    html_nodes(".row") %>% 
    html_text()
}

df = 1:22 %>% map_df(function(i){
  url = glue("https://medicinehatnews.com/commentary/ticked-off-and-tickled-pink/page/{i}/")
  pg = read_html(url)
  
  html_nodes(pg, "h2 a") %>% 
    html_attrs() %>% 
    as.character() %>% enframe()
}) %>% 
  mutate(text = map_chr(value, read_page))


write_rds(df, "totp.rds")

