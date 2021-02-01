library(tidyverse)
library(lubridate)

source("../functionPool/influence_calc_functions.R")

uniqueID <- unique(dd$user_id)

users_dat <- rtweet::lookup_users(uniqueID)

u100 <- users_dat %>% arrange(desc(retweet_count)) %>% head(100)


u100$inf_score <-
  mapply(get_inf_score, u100$user_id)


twt_list_members <- u100

# number of accounts to plot
count_accts = 50

country_name <- "Ethiopia"

# run plot for blue green barchart
plot <- twt_list_members %>%
  arrange(desc(inf_score)) %>%
  head(count_accts) %>%
  mutate(screen_name = paste("@ ", screen_name, sep = ""))  %>%
  ggplot(aes(
    x = reorder(screen_name, -inf_score),
    y = inf_score,
    fill = followers_count
  )) +
  geom_col(width = 0.85) +
  geom_text(
    aes(label = screen_name),
    hjust = 1.3,
    colour = "white",
    # fontface = "",
    size = rel(5.9),
    angle = 90,
    nudge_x = -0.03
  ) +
  scale_y_continuous(labels = scales::number_format(scale = 0.01),
                     expand = c(0, 0.1)) +
  # scale_fill_gradient(low = '#6690d4',
  #                     high = '#b30600',
  scale_fill_gradient(low = '#8dccb1',
                      high = '#0740f5',
                      labels = scales::label_comma()) +
  labs(
    title = ""
    # title = paste("Twitter Influence by ", count_accts, " Leading Ugandans", sep = "")
    ,
    subtitle = ""
    # subtitle = paste("Date Range: ", max(as_date(
    #   uga_infs_data$created_at
    # ) - 30), " to ", max(as_date(
    #   uga_infs_data$created_at
    # )),  sep = "")
    ,
    x = paste(country_name," Twitter Accounts as of: ", today(), sep = "")
    ,
    y = "< lower         Average Engagement Per Tweet          higher >"
    ,
    caption = "src: twitter api"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = rel(1.8)),
    plot.subtitle = element_text(size = rel(1.2), margin = margin(0, 0, 0, 0, 'pt')),
    plot.caption  = element_text(size = rel(1.1)),
    axis.title.x = element_text(size=rel(1.6), margin = margin(0, 0, 0, 0, 'pt'),hjust = NULL,
                                vjust = -1),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.title.y  = element_text(size = rel(1.6), margin = margin(0, 10, 0, 0, 'pt')),
    axis.text.y   = element_blank(),
    #axis.text.y   = element_text(size = rel(1.5)),
    plot.margin   = margin(0, 10, 6, 10),
    legend.position = c(.90, .88),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.text = element_text(margin = margin(5, 10, 20, 0), size=rel(1.2)),
    legend.title = element_text(margin = margin(0, 0, 10, 0), size = rel(1.2))
  )

plot + labs(fill = "Twitter\nFollowers")
