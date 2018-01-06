#Today we want to start to look at some canonical plots.  We will
#go relatively fast through these canonical plots while trying to cover many plots
#and will try to focus on thinking through some of the issues with
#plots in terms of communication about things like variation etc.

#Today we will do so as an exploration of the salaries dataset, in leading
#up to a solution of the daily assignment problem for today.
#One thing about today is that you will see many a pipeline.


salaries <- read_csv('Salaries.csv')

batting <- read_csv('Batting.csv')

head(salaries)
#First we play around with our dataset.  Get to know it.  We:
#ask questions of our data.  we learn from the answers.
#if anything doesn't conform to your intuition of the dataset
#find out why!  When you plot data, if something looks odd
#go figure it out!

#Here's an example of a way we can get to know our data


#number of entries by player (as we will see, not the same as number of years played!)
salaries %>% 
  group_by(playerID) %>% 
  summarize(count=n())

#arranged by largest number
salaries %>% 
  group_by(playerID) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

#look at one player
salaries %>% 
  filter(playerID=="moyerja01") %>% tail()
#tail here looks at the end of his career

#do any players play with more than one team in a single year?
#first check the actual number of years played
#this is not just the number of lines in a players individual database
salaries %>% 
  group_by(playerID) %>% 
  summarize(numYearsPlayed = length(unique(yearID)), count=n())

#create a variable that says if numYearsPlayed not the same as
#the number of lines in the dataset.  If true, a player played
#for more than one team in a year.
salaries %>% 
  group_by(playerID) %>% 
  summarize(numYearsPlayed = length(unique(yearID)), 
            count=n(), multTeamsInYear=numYearsPlayed!=count)

#summarize...
salaries %>% 
  group_by(playerID) %>% 
  summarize(numYearsPlayed = length(unique(yearID)), 
            count=n(), multTeamsInYear=numYearsPlayed!=count) %>%
  summary()
#we see in the summary that there are indeed players such as these.


#do people skip years?
#a lag function works as follows:
#lag(x(t)) = x(t-1)
#in other words, the lag at time t of function x() is equal to the function
#x() at time t-1.  You can also look at lag(x, 2) which is lag(x(t))=x(t-2)
#in tibbles, the rows are ordered, so lag(yearID) says the the value of
#lag(yearID) is equal to the value of yearID in the previous row.
salaries %>% 
  group_by(playerID) %>%
  mutate(yearContinuity = yearID - lag(yearID)) %>% 
  filter(yearID>1985) #this step bc 1985 is just NA (first year of dataset)
#anywhere you see an NA in this table it means they haven't shown up 
#in this dataset prior to that year.

#get summaries of the lag
salaries %>% 
  group_by(playerID) %>%
  mutate(yearContinuity = yearID - lag(yearID)) %>% 
  summary()

salaries %>%
  filter(playerID=="venabma01")

#############################################
############ Now we start plotting ##########
#############################################

#############################################
######## 1-D Categorical Variables ##########
#############################################

#let's plot these gaps in play with a bar plot
salaries %>% 
  group_by(playerID) %>%
  mutate(yearContinuity = yearID - lag(yearID)) %>%
  ggplot(aes(x=factor(yearContinuity))) +
    geom_bar()
#vast majority of the time, players play consecutive years.

#scale in bar plots (and many others) is an issue.  SO MANY
#entries in yearContinuity are 1, that it (possibly) obscures
#the other entries.

#look closer at gaps less than 10 years
salaries %>% 
  group_by(playerID) %>%
  mutate(yearContinuity = yearID - lag(yearID)) %>%
  filter(yearContinuity<=10) %>%
  ggplot(aes(x=as.factor(yearContinuity))) +
  geom_bar()
#if these are factors, why do 6, 7, 8, and 10 show up?  9 didn't...
#again, 1 is dominating the conversation here.  The plot is more
#worried about accommodating 1 than making sure that the other numbers speak.


#look at 6 to 10
salaries %>% 
  group_by(playerID) %>%
  mutate(yearContinuity = yearID - lag(yearID)) %>%
  filter(yearContinuity<=10 & yearContinuity>=6) %>%
  ggplot(aes(x=factor(yearContinuity))) +
  geom_bar()
#moral of the story: sometimes big values (counts here) can
#make small ones (or smaller differences) disappear.

#look at plot of number of years played (number of unique salary years for each player).
#unique(yearID) returns the same vector, but with any duplicates removed.
#length(unique(yearID)) just asks how many unique years there are in a players tibble.
#this is just the years the player was in the MLB
salaries %>% 
  group_by(playerID) %>% 
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar()

#look at number of years played for a given team (remember some players play for multiple teams)
#before we had players years in MLB.  Now we want for individual teams.  So our tibbles now
#need to comprise of one player and one team.
salaries %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar()
#exponential distribution -ish.  I wouldn't call it exponential
#but if you were squinting, or if you were just glancing
#it wouldn't be a bad guess.

#bar chart filtered; JUST Bos, Was, and Phi
#we only filter. Otherwise we do everything the same.
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar()



###################################################
############ 2 Categorical Variables ##############
###################################################

#let's start to deal with 2 categorical variables in our plots
#stacked bar chart
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar(aes(fill=teamID))
#there are various bits of info contained in this chart
#what's easy to see?  What's hard to see?



#relative proportions,
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar(aes(fill=teamID), position="fill")

#side-by-side (by Sondheim!) proportions
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar(aes(fill=teamID), position="dodge")


##### polar coordinates (think pie chart)
################   use with extreme caution!!!
#stacked bar polar, now make teamID the "x" axis
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  filter(numYearsPlayed <= 5) %>%
  ggplot(aes(x=teamID)) +
  geom_bar(width=1, aes(fill=factor(numYearsPlayed))) +
  coord_polar()

#relative polar, back to numYearsPlayed as x
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  filter(numYearsPlayed <= 5) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar(width=1, aes(fill=teamID), position="fill") +
  coord_polar()
  

#side-by-side polar
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  filter(numYearsPlayed <= 5) %>%
  ggplot(aes(x=numYearsPlayed)) +
  geom_bar(width=1, aes(fill=teamID), position="dodge") +
  coord_polar()

#all the angles are equal in the above- what about a more traditional pie chart?
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  filter(numYearsPlayed <= 5) %>%
  ggplot(aes(x=factor(1), fill=teamID)) +
  geom_bar(width=1) +
  coord_polar(theta="y")

#or a 'target' chart
salaries %>%
  filter(teamID %in% c('BOS', 'WAS', 'PHI')) %>%
  group_by(teamID, playerID) %>%
  summarize(numYearsPlayed = length(unique(yearID))) %>%
  filter(numYearsPlayed <= 5) %>%
  ggplot(aes(x=factor(1), fill=teamID)) +
  geom_bar(width=1) +
  coord_polar()

#point is, ggplot2 is happy to do things with you.  It will create many
#pie-flavored charts.  But at the end of the day you need to be really
#cautious with pie-esque charts.  As you can see, they often require us to
#make area comparisons.  Very bad!
#stick to eating pies.  not plotting them.


#################################################
######## One more plot, then to the hw ##########
#################################################
#let's look at batting
batting %>%
  filter(yearID >= 2000 & yearID <=2010)

#let's plot number of games played
batting %>%
  filter(yearID >= 2000 & yearID <=2010) %>%
  ggplot(aes(x=G)) +
  geom_histogram(binwidth=2)
#why the spike?  This is another example of where
#asking questions of your data yields some insight
#into the dataset, and can help you check intuitions.


###################################################
############### HW Solution #######################
###################################################


#and now for the homework solution
inner_join(salaries, batting) #note what this joins by

# filter years
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010)

# let's get rid of a bunch of variables
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010) %>%
  select(yearID, teamID, playerID, salary, G)
  
# calculate salary per game
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010) %>%
  select(yearID, teamID, playerID, salary, G) %>%
  mutate(salaryPerGame=salary/G)
#notice here each year for each player (etc) has a salary per game.
# each player has (possibly) multiple years
# can calculate a median salary/G per player (across years)

# group by playerID and teamID (takes care of multiple teams)
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010) %>%
  select(yearID, teamID, playerID, salary, G) %>%
  mutate(salaryPerGame=salary/G) %>%
  group_by(playerID, teamID)

# calculate median per player per team
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010) %>%
  select(yearID, teamID, playerID, salary, G) %>%
  mutate(salaryPerGame=salary/G) %>%
  group_by(playerID, teamID) %>%
  summarize(median=median(salaryPerGame))
#note that now years are gone (we aggreagated them out through
#the median).  Players have multiple teams, though

# First attemp at calculating the per-team mean of the medians
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010) %>%
  select(yearID, teamID, playerID, salary, G) %>%
  mutate(salaryPerGame=salary/G) %>%
  group_by(playerID, teamID) %>%
  summarize(median=median(salaryPerGame)) %>%
  summarize(mean=mean(median))
#but this didn't do what we wanted!  It aggregated over teams
#we want to aggregate over players

# second attempt: first specify group_by(teamID) so now the
# grouping over playerID is broken
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010) %>%
  select(yearID, teamID, playerID, salary, G) %>%
  mutate(salaryPerGame=salary/G) %>%
  group_by(playerID, teamID) %>%
  summarize(median=median(salaryPerGame)) %>%
  group_by(teamID) %>%
  summarize(mean=mean(median))
  #this does what we want


# plot it
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010) %>%
  select(yearID, teamID, playerID, salary, G) %>%
  mutate(salaryPerGame=salary/G) %>%
  group_by(playerID, teamID) %>%
  summarize(median=median(salaryPerGame)) %>%
  group_by(teamID) %>%
  summarize(mean=mean(median)) %>%
  ggplot() +
  geom_bar(stat="identity", aes(x=teamID, y=mean))

# plot it, ordered, with colors by league! (notice how we had to incorporate lgID)
inner_join(salaries, batting) %>%
  filter(yearID >=2000 & yearID <= 2010)%>%
  select(yearID, teamID, lgID, playerID, salary, G)%>%
  mutate(salaryPerGame=salary/G) %>%
  group_by(playerID, teamID, lgID) %>%
  summarize(median=median(salaryPerGame)) %>%
  group_by(teamID, lgID) %>%
  summarize(mean=mean(median))%>%
  ggplot() +
  geom_bar(stat="identity", aes(x=reorder(teamID, -mean), y=mean, fill=lgID))
