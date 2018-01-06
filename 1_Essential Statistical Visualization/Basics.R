library(tidyverse)
library(reshape2)

#load the salary dataset
salary <- read.csv('Salaries.csv')

#set up basic plot
ggplot(salary, aes(x=yearID, y=salary))

#change axes (we want x label to be "year" not "yearID")
ggplot(salary, aes(x=yearID, y=salary)) + xlab("year")


#plot y variable in millions for readability
ggplot(salary, aes(x=yearID, y=salary/10^6)) + xlab("year")

#change y axis to reflect change in plotting y variable
ggplot(salary, aes(x=yearID, y=salary/10^6)) + xlab("year") + ylab("salary (millions)")+theme_minimal()


# can save plot layers, which helps to 1) really clean things up, and 2) make it easier to type

salaryplot <- ggplot(salary, aes(x=yearID, y=salary/10^6)) + xlab("year") + ylab("salary (millions)")+theme_minimal()

#and it plots what we expect
plot(salaryplot)


#add data (we want to add points to our graph, hence geom_point)
salaryplot + geom_point()

#make bigger dots (this was a better size when we zoomed the plot)
salaryplot + geom_point(size=2)


#can color with any variable we want, like timeID (which would make sense to try)
salaryplot + geom_point(aes(color=teamID), size=3)

#or make all points the same color, just not black.  Note since the color doesn't depend on the data, this is outside of aes()
salaryplot + geom_point(color='red', size=3)

#if we color by a continuous variable, we get a continuous color pallete (can make it discrete with as.factor())
salaryplot + geom_point(aes(color=yearID))

#Coloring the points by team in the plot above didn't really work.  So we can give each team it's own plot with facets

#make a smaller data set, with just a few teams
salaryslim <- salary %>% filter(teamID %in% c("ATL", "CHN", "CIN", "HOU", "LAN", "MON", "PHI", "PIT", "BOS", "BAL", "CAL", "CHA", "CLE", "DET", "KCA", "MIN"))

#A new base plot with the 'slim' dataset
salarySlimPlot <- ggplot(salaryslim, aes(x=yearID, y=salary/10^6)) + xlab("year") + ylab("salary (millions)")+theme_minimal()

#and here we add in the facets.  facet_wrap is where we tell the plot we want each team to get its own plot
salarySlimPlot + geom_point(size=3) + facet_wrap(~teamID, ncol=4)


#We can color by league ID, and we find somethig interesting.
salarySlimPlot + geom_point(size=3, aes(color=lgID)) + facet_wrap(~teamID, ncol=4)

#facet_grid lays out the plots, but doesn't really work in our case bc teamID and league ID aren't independent.
salarySlimPlot + geom_point(size=3) + facet_grid(teamID~lgID)


## some more things we can do...
# let's only look at cleveland
justCLE <- ggplot(filter(salary, teamID=="CLE"), aes(x=yearID, y=salary/10^6)) + xlab("year") + ylab("salary (millions)")+theme_minimal()

justCLE + geom_point(size=3)

#and add a smoothed "trend line"
justCLE + geom_point(size=3) + geom_smooth()

#do the same with Boston
# just Red Sox
justBOS <- ggplot(filter(salary, teamID=="BOS"), aes(x=yearID, y=salary/10^6)) + xlab("year") + ylab("salary (millions)") + theme_minimal()

justBOS + geom_point(size=3)

justBOS + geom_point(size=3) + geom_smooth()

#If we want to easily compare them, we can put them on the same graph!
# CLE and BOS
CLEBOS <- salary %>% filter(teamID %in% c("CLE", "BOS"))

CLEvBOSplot <- ggplot(CLEBOS, aes(x=yearID, y=salary/10^6)) + xlab("year") + ylab("salary (millions)") + theme_minimal()

CLEvBOSplot + geom_point(size=3)

#here we just have the scatter plot
CLEvBOSplot + geom_point(size=3, aes(color=teamID))


#add regression line instead of a local smoother, just for fun.
#first one regression line for all the data
CLEvBOSplot + geom_point(size=3, aes(color=teamID)) + geom_smooth(method = lm, se  =FALSE, color = "black", linetype = "dotdash", size = 2)

#how do we plot a separate regression line for each team?

#the key is to put color as an aesthetic of the base graph.
CLEvBOSplotColor <- ggplot(CLEBOS, aes(x=yearID, y=salary/10^6, color=teamID)) + xlab("year") + ylab("salary (millions)") + theme_minimal()

#can plot without confidence intervals
CLEvBOSplotColor + geom_point(size=3) + geom_smooth(method = lm, se  =FALSE, size = 2)
#or with (but here doesn't make a difference, really)
CLEvBOSplotColor + geom_point(size=3) + geom_smooth(method = lm, size = 2)


#What about individual players?  To do that we want to connect the dots from year to year
#we do that with geom_line... but just calling that fails
CLEvBOSplotColor + geom_line()
#why?  We never specified that the lines should correspond to players!  Easy fix
CLEvBOSplotColor + geom_line(aes(group=playerID))
#incidentally, if we want to keep the points in there, just add lines, it's totally easy to do with layers!
CLEvBOSplotColor + geom_point()+ geom_line(aes(group=playerID))


#look at means.  
meanSalaryDat <- salary %>% filter(teamID %in% c("BOS", "CLE", "WAS", "FLO")) %>% group_by(teamID, yearID) %>% summarize(meanSalary=mean(salary), numPlayer=length(salary))

meanBase <- ggplot(meanSalaryDat, aes(x=yearID, y=meanSalary/10^6)) + xlab("year") + ylab("Average Salary (millions)") + theme_minimal()


#scale points by numplayers.  We are scaling points, so this happens in geom_point.  And, each point is different based on the data, so it is inside aes()
meanBase + geom_point(alpha=.7, aes(size=numPlayer, color=teamID)) + scale_size_area(guide=FALSE, max_size = 10)
#the scale_size_area does two things.  It sets a max scale size, for one, but it also turns off the legend for size (guide=FALSE).  Otherwise this graph would have two legends

#scale by mean Salary (just for fun- this is a totally redundant scaling since it is the y-axis, but the effect wasn't very big in the previous one.)
meanBase + geom_point(alpha=.7, aes(size=meanSalary, color=teamID)) + scale_size_area(guide=FALSE, max_size = 10)
