library(tidyverse)
library(lubridate)

dat_slurs <- str_c( 'ወያነ', 'ወያኔ',  'Woyane', 'Wayane',  'neftegna',  'neftagna' , 'ነፍጠኛ', 'Shanqella', '"አንበጣው የኤርትራ አራዊት"', 'ሻንቅላ',
       'ጁንታ', 'ዓጋመ', 'ጋላ', 'ሰፋሪ', 'ሰፋሪዎች', '"የቀን ጅብ"', 'ምኒልክ', 'ምኒልካዊ', '"ፎጣ ለባሽ"', 'ፋኖ', 'ቆማጣ', '"ጥምብ ጋላ"', 'መጤ', 'አሀዳዊ','ጠባብ', 'ጎጠኛ','አድጊ','ጨናዊ', sep = ' OR ')

dat_slurs_vec <- c('ወያነ', 'ወያኔ',  'Woyane', 'Wayane',  'neftegna',  'neftagna' , 'ነፍጠኛ', 'Shanqella', 'አንበጣው የኤርትራ አራዊት', 'ሻንቅላ',
                    'ጁንታ', 'ዓጋመ', 'ጋላ', 'ሰፋሪ', 'ሰፋሪዎች', 'የቀን ጅብ', 'ምኒልክ', 'ምኒልካዊ', 'ፎጣ ለባሽ', 'ፋኖ', 'ቆማጣ', 'ጥምብ ጋላ', 'መጤ', 'አሀዳዊ','ጠባብ', 'ጎጠኛ','አድጊ','ጨናዊ')

dat_no_RT <-
  rtweet::search_tweets(
    q= dat_slurs,
    n = 18000,
    retryonratelimit = TRUE,
    include_rts = FALSE
  )



# create search string divided by | to not privilage digital_woyane
# use the AFTER the initial data pull
dat_slurs_pipe <- str_c( 'ወያነ', 'ወያኔ',  'Woyane', 'Wayane',  'neftegna',  'neftagna' , 'ነፍጠኛ', 'Shanqella', 'ሻንቅላ',
                         'ጁንታ', 'ዓጋመ', 'ጋላ', 'ሰፋሪ', 'ሰፋሪዎች', 'የቀን ጅብ', 'ምኒልክ', 'ምኒልካዊ', 'ፎጣ ለባሽ', 'ፋኖ', 'ቆማጣ', 'ጥምብ ጋላ', 'መጤ', 'አሀዳዊ','ጠባብ', 'ጎጠኛ','አድጊ','ጨናዊ', sep = '|')

# dat %>% rtweet::save_as_csv('data/dat_slurs')

dat_no_RT_filtered <- dat_no_RT %>% filter(str_detect(text, dat_slurs_pipe))

rtweet::save_as_csv(dat_no_RT_filtered, 'data/dat_no_RT_2021-01-15.csv')



dat_sorted <- dat_no_RT_filtered %>%
  mutate(slur = case_when(
    str_detect(text, "ወያነ|ወያኔ|Woyane|Wayane" ) ~ "Woyane (TPLF)",
    str_detect(text, "neftegna|neftangna|ነፍጠኛ" ) ~ "Neftenga (Amharans)",
    str_detect(text, "shanqella|ሻንቅላ" ) ~ "Shanqella",
    str_detect(text, "ጁንታ" ) ~ "Junta (TPLF)",
    str_detect(text, "agame|ዓጋመ" ) ~ "Agame (Tigrayans)",
    str_detect(text, "ጋላ") ~ "Galla",
    str_detect(text, "ሰፋሪ|ሰፋሪዎች") ~ "Safari (Amharans)",
    str_detect(text, "የቀን ጅብ") ~ "Day Hyena",
    str_detect(text, "ምኒልክ|ምኒልካዊ") ~ "Minilik (Amharans)",
    str_detect(text, "ፎጣ ለባሽ") ~ "Fota-laba (Amharans)",
    str_detect(text, "ፋኖ") ~ "Fano (Amharans)",
    str_detect(text, "ቆማጣ") ~ "Qmoata",
    str_detect(text, "ጥምብ ጋላ") ~ "Timb Galla",
    str_detect(text, "መጤ") ~ "Mete (Amharans)",
    str_detect(text, "አሀዳዊ") ~ "Ahadawi",
    str_detect(text, "ጠባብ") ~ "Tebab (Oromo and Tigray) ",
    str_detect(text, "ጎጠኛ") ~ "Gotegna",
    str_detect(text, "አድጊ") ~ "Adgi",
    str_detect(text, "ጨናዊ") ~ "Chenawi"      
  )
  )

# add_count totals unique values and adds a new column at end
# is use it here to filter out any slur with less than n instances.
dat_sorted_count_tot <- dat_sorted %>% add_count(slur, name = "count_total") %>% mutate(fct_slur = as.vector(slur) ) 

rtweet::save_as_csv(dat_sorted_count_tot, paste('data/ETH_slurs_', Sys.Date(), sep = ""))



dat_sorted_count_tot %>%
  filter(!is.na(slur)) %>%
  group_by(fct_slur)  %>%   # group by factor
  filter(count_total > 14)  %>%
  rtweet::ts_plot(by = 'day', trim = 1L) + geom_line(size = 1.5) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    title = 'Occurrence of Ethnic Slurs in Ethiopian Twitter (no retweets included)'
    ,
    subtitle = paste("Date Range: ", min(as_date(dat_sorted_count_tot$created_at)) , " to ", max(as_date(dat_sorted_count_tot$created_at)),  sep = "")
    ,
    x = "all times GMT. Ethiopan local time GMT +3"
    ,
    y = "tweets per day"
    ,
    caption = ""
  ) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(20, 6, 15, 10), "pt"),
    plot.title    = element_text(size = rel(2.0)),
    plot.subtitle = element_text(size = rel(1.5)),
    plot.caption  = element_text(size = rel(1.3)),
    axis.title.x  = element_text(size = rel(1.3), vjust = -2),
    axis.text.x   = element_text(size = rel(1.7)),
    axis.title.y  = element_text(size = rel(1.7)),
    axis.text.y   = element_text(size = rel(1.7)),
    legend.position = c(.90, .87),
    legend.background = element_rect(fill = 'white'),
    legend.text = element_text(size= rel(1.2)),
    legend.title = element_blank()
  ) 

# "Hachalu Hundessa"
# "ሃጫሉ ሁንዴሳን"

# dots by data and retweets

dat_sorted_count_tot %>%
  filter(!is.na(slur), count_total > 12, retweet_count > 0) %>%
  filter(status_id != "1349098372460908544" ) %>% 
  ggplot(aes(x=created_at, y = retweet_count)) +
  geom_point(aes(fill=slur), size=3,pch=21, color='black', alpha=0.6) +
  scale_fill_brewer(palette = "Set1", type = "seq") +
  labs(
    title = 'Engagement of Tweets Containing Ethnic Slurs and Targeted Groups (excluding retweets)'
    ,
    subtitle = paste("Date Range: ", min(as_date(dat_sorted_count_tot$created_at)) , " to ", max(as_date(dat_sorted_count_tot$created_at)),  sep = "")
    ,
    x = "all times GMT. Ethiopan local time GMT +3"
    ,
    y = "retweet count"
    ,
    caption = ""
  ) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(20, 6, 15, 10), "pt"),
    plot.title    = element_text(size = rel(2.0)),
    plot.subtitle = element_text(size = rel(1.5)),
    plot.caption  = element_text(size = rel(1.3)),
    axis.title.x  = element_text(size = rel(1.3), vjust = -2),
    axis.text.x   = element_text(size = rel(1.7)),
    axis.title.y  = element_text(size = rel(1.7)),
    axis.text.y   = element_text(size = rel(1.7)),
    legend.position = c(.90, .87),
    legend.background = element_rect(fill = 'white'),
    legend.text = element_text(size= rel(1.2)),
    legend.title = element_blank()
  ) 

rtweet::save_as_csv(dat_sorted_count_tot, 'data/ETH_slurs.csv')

d <- str_squish(dat_RT_100K$text)
d <- dat_RT_100K %>% mutate(text, str_squish(str))

rtweet::save_as_csv(dat_am, prepend_ids = FALSE, 'data/dat_am.csv')


top22 <- dat_sorted_count_tot %>% arrange(desc(retweet_count)) %>% head(22)
rtweet::save_as_csv(top22, 'data/top22.csv')
