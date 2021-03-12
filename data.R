library(gtrendsR)

last_year <- gtrends("big bud press", time = "today 12-m", low_search_volume = TRUE)
last_week <- gtrends("big bud press", time = "now 7-d", low_search_volume = TRUE)

saveRDS(last_year, "last_year.rds")
saveRDS(last_week, "last_week.rds")
