library(sRa)

#' df = read_rds('Data.rds')  %>%
#'   get_sentiment(Text, emote = T, distance = 0.01) %>% 
#'   get_sentiment(Text, emote = F, distance = 0.01)
#' 
#' write_rds(df, "Data_WithSentiments.rds")
#' 
df = read_rds("Data/Data_WithSentiments.rds")

df %>% 
  group_by(date) %>% 
  count(Type) %>% 
  mutate(Percent = n/sum(n)) %>>% t %>%  
  ungroup %>% 
  ggplot(aes(x = date, y = Percent, fill = Type)) + 
  geom_area()

df %>%
  split(.$Type) %>% 
  map(select_if, is.numeric) %>% 
  map_df(function(d) map(d, mean))  %>%
  mutate(Type = c("Ticked Off", "Tickled Pink")) %>% 
  gather(Feel, Amount, -Type) %>% 
  group_by(Feel) %>% mutate(Sort = sum(Amount)) %>% 
  ungroup %>% mutate(Feel = fct_reorder(Feel, Sort)) %>% 
  filter(!Feel %in% c("Positive", "Negative")) %>% 
  ggplot(aes(x = Feel, y = Amount, fill = Type)) + 
  geom_col(position = position_dodge(0.7)) + 
  geom_label(aes(y = Amount, label = percent(Amount, accuracy = 1)),
            position = position_dodge(0.95), hjust = -0.25, size = 4, 
            label.size = NA, label.padding = unit(0.01, "inches")) + 
  coord_flip() + 
  scale_fill_manual(values = c("grey", "pink")) + 
  scale_color_manual(values = c("grey", "pink")) + 
  scale_y_continuous(limits = c(0, .5)) + 
  ggtitle("What Does Ticked Off & Tickled Pink Feel Like?", 
          "An Experiment in Sentiment Analysis") +
  labs(caption = "Source: Medicine Hat News - Ticked Off and Tickled Pink Archives")+ 
  theme_minimal() + 
  theme(axis.title = element_blank(),
        panel.grid = element_line(size = 0.47, color = "#f2f2f2"), 
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, face = "italic"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 25, face = "bold"),
        plot.subtitle = element_text(size = 15)) 

ggsave("totp.pdf", device = cairo_pdf, height = 7, width = 9, plot = last_plot())
ggsave("totp.jpg", height = 7, width = 9, plot = last_plot(), dpi = 600)
