library(tidyverse)
library(lubridate)
library(rtweet)
library(writexl)
library(ggthemes)

# load("../tokens/twitter_tokens/token_filename.Rdata")
# load("~/.Renviron")

eth_list_id <- "1302196457190109187"
eth_user_ids <- rtweet::lists_members(list_id = eth_list_id)


ethlm <- eth_user_ids %>% mutate(Thumbnail = "") # add empty vec
ethlm %>%
  select(Name = name, Account = screen_name, Description = description, Thumbnail) %>%
  kableExtra::kbl() %>%
  kableExtra::kable_paper() %>%
  kableExtra::kable_styling(full_width = TRUE, position = "center") %>%
  kableExtra::column_spec(
    4,
    extra_css = "display: table-cell; width:100px; text-align: center; font-size:1.1em, border:1px solid #e6e6e6;",
    image = kableExtra::spec_image(eth_user_ids$profile_image_url_https, 150, 150)
  ) %>%
  kableExtra::save_kable(file = "output/eth_twt.pdf", density = 72)



###############################

sn <- tw_user_ids$screen_name

hopper <- vector("list", length(nrow(eth_user_ids)))

# seems to work. takes a few hours.
for(i in 1:nrow(eth_user_ids)){
  tmp <- rtweet::get_timeline(eth_user_ids[i]$user_id, n=3200)
  # tmp <- rtweet::search_tweets(sn[i], n=45000, include_rts = TRUE, retryonratelimit = TRUE)
  hopper[[i]] <- tmp
}
eth_ethiopia_231 <- dplyr::bind_rows(hopper)
rtweet::save_as_csv(eth_ethiopia_231,"output/eth_ethiopia_231")

eth_data <- rtweet::read_twitter_csv("output/eth_ethiopia_231.csv")

eth_data_2020 %>%
  separate_rows(mentions_screen_name) %>%
  filter(!is.na(mentions_screen_name)) %>%
  count(mentions_screen_name, sort = T) %>%
  mutate(mentions_screen_name = fct_reorder(tolower(mentions_screen_name), n)) %>%
  head(30) %>%
  ggplot() +
  geom_col(aes(x = reorder(paste("@",mentions_screen_name,sep = ""),n), y=n)) +
  coord_flip() +
  theme_minimal() +
  labs(x="Account Names Mentioned"
       , y="Number of Times Mentioned"
       , title = "Ethiopia Twitter Mentions 2020"
       , subtitle = today()
       , caption = "src: Twitter api")

dateSpan <- 45

# eth_data_2020 <- eth_data %>% filter(created_at >= as_date("2020-01-01"))

eth_data_span <- eth_data %>% filter(created_at >= as_date(today() - dateSpan))

p <- eth_data_span %>%
  filter(followers_count >= 10000) %>% 
  filter((followers_count / friends_count) >= 10) %>% 
  add_count(screen_name) %>%
  filter(n > 1) %>%
  ggplot() + geom_point(aes(x=created_at, y=reorder(screen_name, desc(screen_name)),color=lang))

p + theme_minimal() + labs(x = "Date of Tweet"
         , y = "Tweet Author"
         , title = paste("Twitter Activity by Influential Ethiopians "
                         , min(date(eth_data_span$created_at))
                         , " to "
                         , max(date(eth_data_span$created_at))
                         , sep = "")
         , subtitle = "Accounts with > 10,000 followers and 10x ratio of followers:following"
         , caption = "src: twitter api")

# CHN Emb Addis -----------------------------------------------------------

chn_emb_gha_tl <- rtweet::get_timeline("ChinaEmbinGH", n=3200)
rtweet::save_as_csv(chn_emb_gha_tl,"output/chn_emb_gha")
rtweet::ts_plot(chn_emb_gha_tl)

chn_emb_gha_tl %>%
  unnest(hashtags) %>%
  select(mentions_screen_name, hashtags) %>%
  count(mentions_screen_name ,hashtags, sort = TRUE) %>%
  knitr::kable()

# TIMELINE of ETH Bosses --------------------------------------------------
options(scipen=999) 

for(i in 1:length(dat)){
  tmp <- rtweet::get_timeline(dat[i], n=3200, retryonratelimit = TRUE) # retry may not apply to this method.
  rtweet::save_as_csv(tmp, file_name = paste("data/gha_",i,sep = ""))
}

# https://stackoverflow.com/questions/42025979/avoid-rate-limit-with-rtweet-get-timeline

data_all <- list.files(path = "./data", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(rtweet::read_twitter_csv) %>%  # Store all files in list
  bind_rows

# https://statisticsglobe.com/merge-csv-files-in-r

gha_all_n <- data_all %>% add_count(screen_name)

## REMOVE ALL Whitespaces
gha_no_ws <- gha_all_n %>% 
  mutate(across(where(is.character), str_trim))

out <- gha_no_ws %>% select(screen_name, description) %>% distinct()
View(out)

gha_all_n %>% 
  separate_rows(hashtags, sep=" ") %>% count(hashtags, sort = TRUE) %>% head(n=101) %>% write_xlsx("output/gha_hashtags100.xlsx")

gha_all_n %>% 
  mutate(screen_name = fct_reorder(screen_name, n)) %>%
  ggplot() + geom_bar(aes(x=screen_name)) + coord_flip()

gha_sep <- gha_all_n %>% 
  separate_rows(hashtags)

gha_lc <- gha_sep %>% mutate(hashtags = tolower(hashtags)) 

gha_ht100 <- gha_lc %>% count(hashtags, sort = T) %>% head(n=100)

write_xlsx(gha_ht100,"output/hashtags.xlsx")
