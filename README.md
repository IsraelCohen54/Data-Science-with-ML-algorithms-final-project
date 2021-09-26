# ML with biology data project

begginer tips:
to clear Console do: control + L
to run the prog, do control + enter

happy_data=world.happiness.report
names(happy_data) # see columns names
#head(happy_data) #tail..
#summary(happy_data) #mix max avg etc
#str(happy_data) #happy_data in format, we can see we should work with double for example..
#summary(happy_data$year) #summery of year column

#boxplot(happy_data$year) # plot boxes type of year column

#hist(happy_data$Generosity) # plot histograma type
#boxplot(happy_data$Generosity)


#dim(happy_data) #happy_data dimension, 1949 rows, 11 columns

#happy_data[,3] #get spicific happy_data (here it's life ladder column, all column)

#version #get r version (to 4.0.5)
#install.packages("installr")
#library(installr)
#updateR()

#install.packages("ggplot2")
library(ggplot2)
ggplot(data = world.happiness.report) + geom_point(mapping = aes(x = Life.Ladder, y=Social.support)) # other combinations as well...
