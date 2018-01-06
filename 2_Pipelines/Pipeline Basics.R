library(tidyverse)

load('countries.Rdata')

#countries <- countries[complete.cases(countries),]
countries <- na.omit(countries) # shout out to Katherine Qian for using this function!

# here, note that scale_x_continuous is how we get the log transformation into the
# x axis.  scale_x_continuous keeps track of transformation and allows us to "think"
# in the 'original' variable space, while displaying in the logged space.
baseplot <- ggplot(countries, aes(x=GDPperCapita, y=LifeExpectancy)) +
  xlab('GDP per Capita') +
  ylab('') + 
  labs(title='Life Expectancy') + #(shout out to Christine and Max)
  scale_x_continuous(trans='log', limits=c(500, 128000), breaks=c(500,1000,2000,4000,8000,16000,32000,64000,128000),labels=c(500,1000,2000,4000,8000,'16k','32k','64k','128k')) +
  scale_y_continuous(limits=c(20, 90), breaks=seq(20, 90, 10),labels=seq(20, 90, 10)) +
  theme_minimal()

# here in order to get the black border on the point, we change the point type to pch=21
# which is a fill-in-able circle.  We fill it in with the aesthetic fill.  The default
# points are solid, with no border
baseplot + 
  geom_point(aes(size=Population, fill=Region), pch=21, color='black', alpha=.8) +
  scale_size_area(guide=FALSE, max_size = 30) +
  scale_fill_brewer(palette = 'Paired')


players <- read_csv('Master.csv')    #'biographical' information
pitching <- read_csv('Pitching.csv') #pitching statistics



#filter

# filter on a variable
pitching %>% 
  filter(yearID>1995)

#filter on multiple variables with &
pitching %>% 
  filter(yearID>1995 & teamID=='DET')

#filter with or- what happens?
pitching %>% 
  filter(yearID>1995 | teamID=='DET')

#not
pitching %>% 
  filter(yearID>1995 & teamID!='DET')

#arrange rows according to a column
pitching %>% 
  filter(yearID>1995 & teamID=='DET') %>% 
  arrange(desc(yearID))

#select first 10
pitching %>%
  filter(yearID>1995 & teamID=='DET') %>%
  arrange(desc(yearID)) %>%
  head(10)

pitching %>%
  filter(yearID>1995 & teamID=='DET') %>%
  arrange(yearID, desc(playerID)) %>%
  head(10)

#filter by more than one group
pitching %>% 
  filter(teamID=='DET' | teamID=='BOS')

#using in
pitching %>% 
  filter(teamID %in% c('DET', 'BOS'))

#the power of in (we could hand-code here, but what if alot? or if our
#subsets came from the data itself and we literally couldn't hand code it
#on the fly)
pitching %>% 
  filter(teamID %in% setdiff(unique(teamID), c('DET', 'BOS')))

#group_by (and summarize)
pitching %>% 
  filter(yearID>2000) %>% 
  group_by(teamID) %>%
  summarize(total_wins=sum(W), total_losses=sum(L))

#group by more than one variable
pitching %>% 
  filter(yearID>2000) %>% 
  group_by(teamID, yearID) %>%
  summarize(total_wins=sum(W), total_losses=sum(L))

#top 5
pitching %>% 
  filter(yearID>2000) %>% 
  group_by(playerID) %>%
  summarize(total_wins=sum(W), total_losses=sum(L)) %>%
  arrange(desc(total_wins)) %>%
  head(5)

#joins

#match rows by playerID.  only return rows containing playerIDs
#that show up in both datasets.  There are multiple rows in pitching
#corresponding to the same playerID (because there are multiple years)
#and each one of those rows will get its own row in the new dataset
#(all with the same information from the players database in the appropriate
#columns)
inner_join(players, pitching, "playerID")

#Same as inner_join, except anything appearing in players that doesn't show
#up in pitching gets a row in the new dataset (with just NAs in the columns
#that would otherwise be filled with things in pitching)
left_join(players, pitching, "playerID")

#same as left join but with the roles reversed
right_join(players, pitching, "playerID")


#returns the rows in the players dataset that don't show up
#in the pitching dataset
anti_join(players, pitching, "playerID")



#combines players and pitching ->
#  creates a new column age -> 
#  explodes into multiple tibbles, one corresponding to each player ->
#  for each player tibble, summarizes by computing the oldest age they played,
#     and the corresponding year (now each player has a single row with their
#     max age and the year that happened) ->
#  orders players by max age, oldest first ->
#  returns a tibble of playerIDs, yearIDs (when they were the oldest), and their
#     age that year, sorted oldest to youngest
#  
inner_join(players, pitching, "playerID") %>%
  mutate(age=yearID - birthYear) %>%
  group_by(playerID) %>%
  summarize(max_age=max(age), yearID=max(yearID)) %>%
  arrange(desc(max_age)) %>%
  select(playerID, yearID, max_age)

#has the same result as above, just a different way of getting there.
#see if you can figure out what exactly this is doing.
inner_join(players, pitching, "playerID") %>%
  mutate(age=yearID - birthYear) %>%
  group_by(playerID) %>%
  filter(age==max(age), stint==1) %>%
  select(playerID, yearID, age) %>%
  arrange(desc(age))

#here, after creating multiple tibbles, one for each player, the
#mutate function counts the number of rows in each tibble (think of
#this as length of career) . The function that returns the number of rows
#is just n(), and note that for each row in each player tibble the column
#"count" is the same number (the number of rows in that tibble)
#so in the next step, when we summarize with the mean of count, we just return
#the count itself, or in other words the length of each players career
#... maybe not the most efficient way to do this, but... see the next pipeline
inner_join(players, pitching, "playerID") %>%
  group_by(playerID) %>%
  mutate(count=n()) %>%
  summarize(mean_number_years=mean(count))

#this one is almost the same, but we ungroup before we summarize
#which means we put all the tibbles back together.  The main effect
#here is that the summarize function applies to the big tibble
#not each individual tibble.
inner_join(players, pitching, "playerID") %>%
  group_by(playerID) %>%
  mutate(count=n()) %>%
  ungroup() %>%
  summarize(mean_number_years=mean(count))

#Buuuuuuuuut, the previous pipeline is *not* the average career length!!!!!! Why????
#how might you fix it?