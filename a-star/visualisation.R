
# Optimal path visualisation ----------------------------------------------


# Libraries ---------------------------------------------------------------

library(data.table)
library(ggmap)
library(ggplot2)
library(mapproj)

# Load data ---------------------------------------------------------------

d <- as.data.frame(fread("data/path.csv"))



# Map ---------------------------------------------------------------------

map <- get_map(location = c(lon = mean(-1), lat = mean(40)), zoom = 6,
							 maptype = "roadmap", source = "google")
p <- ggmap(map) +
	scale_x_continuous(limits=c(-6,2.5), expand = c(0.1,0)) +
	scale_y_continuous(limits=c(36,42), expand = c(0,0))
p + geom_path(data=d, aes(x=longitude, y=latitude), colour="dark orange", size=1.5) +
	geom_point(aes(x = d[1,2], y=d[1,1]), colour = "blue", size = 10, alpha = 0.2) +
	geom_point(aes(x = d[1,2], y=d[1,1]), colour = "white", size = 6, alpha = 0.75) +
	geom_point(aes(x = d[1,2], y=d[1,1]), colour = "blue", size = 4) +
	geom_point(aes(x = d[length(d$latitude),2], y=d[length(d$latitude),1]), colour = "red", size = 10, alpha = 0.2)+
	geom_point(aes(x = d[length(d$latitude),2], y=d[length(d$latitude),1]), colour = "white", size = 6, alpha = 0.75)+
	geom_point(aes(x = d[length(d$latitude),2], y=d[length(d$latitude),1]), colour = "red", size = 4) +
	labs(x = "Longitude", y = "Latitude") +
	theme_bw()
