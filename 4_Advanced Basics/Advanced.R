library(tidyverse)
sewing <- read_csv('Allstits.txt')

sewing %>% 
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot() + geom_histogram(aes(x=`Stitch Multiple`))


sewing %>% 
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot(aes(x=`Stitch Multiple`)) + 
  geom_histogram(bins=40, fill='red', color='black')

sewing %>% 
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot(aes(x=`Stitch Multiple`)) + 
  geom_histogram(bins=40, fill='red', color='black') +
  geom_rug()
#rug doesn't do us a ton of good here bc of integer values.

#switch to density
sewing %>% 
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot(aes(x=`Stitch Multiple`)) + 
  geom_density(fill='red', color='black', na.rm=TRUE)

#we can do with density what we did with barplots.
sewing %>% 
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot(aes(x=`Stitch Multiple`)) + 
  geom_density(aes(color=Book, fill=Book), alpha=.1, na.rm=TRUE)


sewing %>% 
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot(aes(x=`Stitch Multiple`)) + 
  geom_density(aes(color=Book, fill=Book), alpha=.1, na.rm=TRUE, position="stack")

sewing %>% 
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot(aes(x=`Stitch Multiple`)) + 
  geom_density(aes(color=Book, fill=Book), alpha=.1, na.rm=TRUE, position="fill")

#boxplots

sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot() +
  geom_boxplot(aes(y=`Stitch Multiple`, x=`Stitch Type`)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#boxplots contain limited amount of info.  Violin plots are boxplots with densities.

sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  ggplot() +
  geom_violin(aes(y=`Stitch Multiple`, x=`Stitch Type`)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  filter(`Stitch Type` %in% c('Lace', 'Ribbings')) %>%
  ggplot(aes(y=`Stitch Multiple`, x=`Stitch Type`)) +
  geom_violin() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_jitter(height = 0, width = 0.5)

#here violins have the same area
sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  filter(`Stitch Type` %in% c('Lace', 'Ribbings')) %>%
  ggplot() +
  geom_violin(aes(y=`Stitch Multiple`, x=`Stitch Type`)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#here area is porportional to sample size
sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  filter(`Stitch Type` %in% c('Lace', 'Ribbings')) %>%
  ggplot() +
  geom_violin(aes(y=`Stitch Multiple`, x=`Stitch Type`), scale="count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#here same width
sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  filter(`Stitch Type` %in% c('Lace', 'Ribbings')) %>%
  ggplot() +
  geom_violin(aes(y=`Stitch Multiple`, x=`Stitch Type`), scale="width") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#of course, can color
sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  filter(`Stitch Type` %in% c('Lace', 'Ribbings')) %>%
  ggplot() +
  geom_violin(aes(y=`Stitch Multiple`, x=`Stitch Type`, fill=`Stitch Type`), scale="count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#add quantiles
sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`)) %>%
  filter(`Stitch Type` %in% c('Lace', 'Ribbings')) %>%
  ggplot() +
  geom_violin(aes(y=`Stitch Multiple`, x=`Stitch Type`, fill=`Stitch Type`), scale="count", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`), `Row Multiple`=as.double(`Row Multiple`)) %>%
  ggplot() +
  geom_point(aes(y=`Row Multiple`, x=`Stitch Multiple`))


sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`), `Row Multiple`=as.double(`Row Multiple`)) %>%
  ggplot(aes(y=`Row Multiple`, x=`Stitch Multiple`)) +
  geom_point(aes(y=`Row Multiple`, x=`Stitch Multiple`)) +
  geom_smooth(method=lm, se=TRUE)


sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`), `Row Multiple`=as.double(`Row Multiple`)) %>%
  ggplot(aes(y=`Row Multiple`, x=`Stitch Multiple`)) +
  geom_point(aes(y=`Row Multiple`, x=`Stitch Multiple`)) +
  geom_smooth(method=loess, se=TRUE)


sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`), `Row Multiple`=as.double(`Row Multiple`)) %>%
  ggplot(aes(y=`Row Multiple`, x=`Stitch Multiple`)) +
  geom_smooth(method=loess, se=TRUE)

sewing %>%
  mutate(`Stitch Multiple`=as.double(`Stitch Multiple`), `Row Multiple`=as.double(`Row Multiple`)) %>%
  ggplot(aes(y=`Row Multiple`, x=`Stitch Multiple`)) +
  geom_smooth(method=loess, se=TRUE, fill="blue", color="red", alpha=.3, size=1.5)

