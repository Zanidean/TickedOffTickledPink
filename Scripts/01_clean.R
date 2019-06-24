library(sRa)

df = read_rds("totp.rds")

clean_text = function(col){
  col %>% 
    str_replace_all("\n", "") %>% 
    str_replace_all("\t", "") %>% 
    str_remove(".*NOW") %>% 
    str_remove("Share.*") %>% 
    str_split(".Tick")
  
}

df_clean = df %>% 
  mutate(text = map(text, clean_text)) %>% 
  unnest %>% 
  unnest %>% 
  mutate(Type = case_when(
    str_detect(text, "led Pink") ~ "Tickled Pink",
    str_detect(text, "ed Off") ~ "Ticked Off"),
    Keep = ifelse(str_detect(text, '^led Pink') | 
                    str_detect(text, '^ed Off') , 1, 0)) %>% 
  filter(Keep == 1) %>% 
  mutate(text = text %>% str_remove("led Pink") %>% str_remove("ed Off")) %>% 
  select(-Keep, -name) %>% 
  mutate(date = value %>% 
           str_extract("\\/[:digit:]{4}\\/[:digit:]{2}\\/[:digit:]{2}\\/")) %>% 
  select(-value) %>% 
  mutate(date = date %>% str_remove("\\/") %>% 
           str_replace_all("\\/", "-") %>% 
           as.Date()) %>% 
  rename(Text = text)

write_rds(df_clean, "Data.rds")
