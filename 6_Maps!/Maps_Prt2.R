library(tidyverse)
library(ggmap)

#load in citibike data
citibike <- read_csv('201508-citibike-tripdata.csv')

#get map of new york city- pay attention to zoom level- pick one that makes sense.
ny1 <- get_map("new york city", maptype = "roadmap", zoom=12)

#prepare a new tibble.  Here we want to calculate the number of trips from each station.
#BTW, I would still do data manipulation even if I were only plotting the locations of the bike stations.  Why?
count_citi <- citibike %>%
  group_by(`start station latitude`, `start station longitude`) %>%
  summarize(count=n())

#basic plots- critique
ggmap(ny1) + geom_point(aes(x=`start station longitude`, y=`start station latitude`, color=count), data=count_citi)
ggmap(ny1) + geom_point(aes(x=`start station longitude`, y=`start station latitude`, color=count), data=count_citi) +
  scale_color_continuous(low='blue', high='red') +
  xlab("longitude") +
  ylab("latitude")
ggmap(ny1) + geom_point(aes(x=`start station longitude`, y=`start station latitude`, size=count), data=count_citi) +
  xlab("longitude") +
  ylab("latitude")

#Some (better?) alternatives
ggmap(ny1) + geom_density2d(aes(x=`start station longitude`, y=`start station latitude`), data=count_citi, size=.5) +
  xlab("longitude") +
  ylab("latitude")

#heatmap type plot
ggmap(ny1) + stat_density_2d(aes(x=`start station longitude`, y=`start station latitude`, fill=..level..), data=count_citi, geom = "polygon") +
  xlab("longitude") +
  ylab("latitude")

#can control how coarse-grained the contours are, or the colors represented
ggmap(ny1) + stat_density_2d(aes(x=`start station longitude`, y=`start station latitude`, fill=..level..), data=count_citi, geom = "polygon", bins=10) +
  xlab("longitude") +
  ylab("latitude") +
  scale_fill_continuous(guide_legend(title="Number of rides"), low="green", high="red")

#get rid of the blob look
ggmap(ny1) + stat_density_2d(aes(x=`start station longitude`, y=`start station latitude`, fill=..level..), data=count_citi, geom = "polygon", bins=10, alpha=.2) +
  xlab("longitude") +
  ylab("latitude") +
  scale_fill_continuous(guide_legend(title="Number of rides"))

#make alpha dependent on level as well
ggmap(ny1) + 
  stat_density_2d(aes(x=`start station longitude`, y=`start station latitude`, fill=..level.., alpha=..level..), data=count_citi, geom = "polygon", bins=10) + 
  scale_alpha_continuous(guide = FALSE) +
  xlab("longitude") +
  ylab("latitude") +
  scale_fill_continuous(guide_legend(title="Number of rides"))

#a different way to view
ggmap(ny1) + 
  stat_density_2d(geom = "point", aes(x=`start station longitude`, y=`start station latitude`,size = ..density..), n=20, data=count_citi, contour = FALSE) + 
  xlab("longitude") +
  ylab("latitude") +
  guides(size=guide_legend(title="Number of rides"))

#density over the water?
ggmap(ny1) + 
  stat_density_2d(geom = "point", aes(x=`start station longitude`, y=`start station latitude`,size = ..density..), n=20, data=count_citi, contour = FALSE, h=.005) + 
  scale_alpha_continuous(guide = FALSE) +
  xlab("longitude") +
  ylab("latitude") +
  guides(size=guide_legend(title="Number of rides"))

#effect of h on 'heatmap'
ggmap(ny1) + 
  stat_density_2d(aes(x=`start station longitude`, y=`start station latitude`, fill=..level.., alpha=..level..), data=count_citi, geom = "polygon", bins=10, h=.005) + 
  scale_alpha_continuous(guide = FALSE) +
  xlab("longitude") +
  ylab("latitude") +
  scale_fill_continuous(guide_legend(title="Number of rides"))

#better h for heatmap
ggmap(ny1) + 
  stat_density_2d(aes(x=`start station longitude`, y=`start station latitude`, fill=..level.., alpha=..level..), data=count_citi, geom = "polygon", bins=10, h=.01) + 
  scale_alpha_continuous(guide = FALSE) +
  xlab("longitude") +
  ylab("latitude") +
  scale_fill_continuous(guide_legend(title="Number of rides"))
