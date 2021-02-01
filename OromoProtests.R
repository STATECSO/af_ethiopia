library(tidyverse)
library(lubridate)
source("../functionPool/influence_calc_functions.R")

ETH <- "1302196457190109187"

mems <- rtweet::lists_members(ETH)

# get influence ranking of top 100 UGA tweeters
# add china and us emb next time

mems$inf_score <-
  mapply(get_inf_score, mems$user_id)

mems_ETH <- mems
rtweet::save_as_csv(mems, paste('data/mems_ETH_',Sys.Date(), sep = "") )


# get all accounts after top 100 to remove from list
#m <- mems %>% arrange(desc(inf_score)) %>% tail(36)

# remove accounts below 100 influence
# rtweet::post_list(users = m$user_id, list_id = "1300910280952741889", destroy = TRUE)

# bobi <-
# rtweet::get_timeline(user='HEBobiwine', n=3200, include_rts = TRUE) %>%  filter(created_at > (today() - 90))


# get timeline
tmls <-
  tryCatch(
    rtweet::get_timelines(mems$screen_name, n = 300),
    error = function(e)
      Sys.sleep(900)
  )


tmls_dat_temp <- paste("ETH_orom_",Sys.Date(),"_",sep = "")


tmls_dat <- tryCatch(rtweet::search_tweets(
  q = 'oromo OR "the flag:" OR ኦሮሞ'  , n= 200000, retryonratelimit = TRUE), error = function(e) Sys.sleep(900))
         
temp <- paste("tmls_dat_",Sys.Date(),sep = "")

tmls_12_18 <- tmls_dat %>% filter(created_at >= lubridate::ymd_hms("2020-12-18 17:00:00"))

rtweet::save_as_csv(tmls_12_18, paste('data/ETH_tmls_',Sys.Date(), sep = ""))

tmls_12_18 %>% rtweet::ts_plot(by='hour')


# rtweet::post_list(users = t23$user_id, list_id = "1302196457190109187", destroy = TRUE)



