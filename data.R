library(gtrendsR)
library(rnaturalearth)

last_year <- gtrends("big bud press", time = "today 12-m", low_search_volume = TRUE)
last_week <- gtrends("big bud press", time = "now 7-d", low_search_volume = TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")

saveRDS(last_year, "data/last_year.rds")
saveRDS(last_week, "data/last_week.rds")
saveRDS(world, "data/world.rds")
