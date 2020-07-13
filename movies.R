getwd()
setwd("C:\\Users\\Nordin\\Desktop\\Stat.Comp")

#Importing Data, fileEncoding argument was added to prevent error in reading the data
blockbuster <- read.csv("movies.csv", fileEncoding = "UTF-8-BOM")
str(blockbuster)
attach(blockbuster)

#-------------------DATA EXPLORATION-----------------------------
library(ggplot2) #activate ggplot library for ggplot graph 

# Number Of Movies Produced by Main Genre of the Movies
# use bar graph
ggplot(blockbuster, aes(x=Main_Genre, fill=Main_Genre))+geom_bar()+
  
  # set the label for x and y axis & title
  labs( x="Main Genre of the Movie",y="Number of Movies",title="Number of Movies by Main Genre")+
  
  #show the count label at the top of the bar
  geom_text(stat="count", aes(label=..count..), vjust =-0.2)+
  
  #Change the colour, size, font of the labels
  theme(
    #change x axis values view, angle at 90 to show label vertically
    axis.text.x =element_text(colour="black", size=9, face="bold",angle = 90),
    #set title colour, font size, bold and horizontal adjustment
    plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5),
    #set of x and y labels axis colour, font size, bold and italic
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
    #disable legend
    legend.position = "none"
  )

#Number Of Movies Produced by Studio Film
# use bar graph
ggplot(blockbuster, aes(x=studio, fill=studio))+geom_bar()+
  
  # set the label for x and y axis & title
  labs(fill="Studio",x="Studio", y="Number of Movies",title="Number of Movies by Studio Film")+
  
  #show the count label at the top of the bar
  geom_text(stat="count", aes(label=..count..), vjust =-0.2)+
  
  #Change the colour, size, font of the labels
  theme(
    #set title colour, font size, bold and horizontal adjustment
    plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5),
    #set of x and y labels axis colour, font size, bold and italic
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
    #remove x axis labels
    axis.text.x = element_blank(),
    #remove x axis ticks
    axis.ticks.x = element_blank(),
    #set legend text size
    legend.text = element_text(size = 8),
    #set legend box size
    legend.key.size = unit(0.2,"cm")
  )

#Number Of Movies by MPAA Rating
# use bar graph
ggplot(blockbuster, aes(x=factor(rating), fill=rating))+geom_bar()+
  
  # set the label for x and y axis & title
  labs(fill="MPAA Rating", x="MPAA Rating",y="Number of Movies",title="Number of Movies by MPAA rating")+
  
  #show the count label at the top of the bar
  geom_text(stat="count", aes(label=..count..), vjust =-0.2)+
  
  #Change the colour, size, font of the labels
  theme(
    #set title colour, font size, bold and horizontal adjustment
    plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5),
    #set of x and y axis labels colour, font size, bold and italic
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
    #change x and y axis values colour, font size, bold
    axis.text = element_text(colour="black", size=12, face="bold"), 
  )+
  #rename x axis values label
  scale_x_discrete(labels=c("G","PG","PG-13","R"))

#----------------RESULT AND DISCUSSION---------------------

#Command for Multiple Regression Model
Mlm.fit <- lm(worldwide_gross~rank_in_year+rating+year+imdb_rating+length, data = blockbuster)
summary(Mlm.fit)

#Plotting graph of independent variables against dependent variable using GGPLOT
library(ggplot2)

#1st independent variable (Rank_in_year)

#compute mean of value worldwide_gross by ranking of the movie using tapply function
mean_ranking=data.frame(value=tapply(worldwide_gross,rank_in_year,mean))
mean_ranking <- mean_ranking[,1]
mean_ranking
class(mean_ranking)

#when we plot "mean_ranking" variable on y-axis, the values are in scientific notation in the graph
#remove scientific notation and change it to decimal separator for more clear visualization
mean_ranking=format(mean_ranking, big.mark = ",",scientific = F)
mean_ranking
class(mean_ranking)

#storing the label for x axis(ranking) into new variable
names_ranking<- names(tapply(worldwide_gross,rank_in_year,mean))
names_ranking
class(names_ranking)

#change to integer type since rank is a ordinal variable
names_ranking <- as.integer(names_ranking)
names_ranking
class(names_ranking)

#store "mean_ranking" and "names_ranking" into a new dataframe
df_ranking <- data.frame(names_ranking, mean_ranking)
df_ranking
str(df_ranking)

#plotting graph
#plotting line graph with size of width equal to "1" and colour of the line set to "red"
#add points on lines

ggplot(df_ranking,aes(x=factor(names_ranking), y=mean_ranking, group=1)) + geom_line(size=1.5,col="red")+geom_point(size=2)+  
  
  #set the label for x, y axis and title 
  labs(x="Rank In Year",y="World Wide Gross($)",title="Mean Value Of Gross Film By Ranking Of The Movie")+
  
  #set theme
  theme_gray()+
  
  #label the values inside the graph
  geom_text(aes(label=mean_ranking),vjust=-1,hjust=0.2,size=3.0)+
  
  #Change colour, size, font of the labels
  theme(
    #change x and y axis values colour, font size, bold
    axis.text = element_text(colour="black", size=12, face="bold"), 
    #set title colour, font size, bold and horizontal adjustment
    plot.title = element_text(color="blue", size=14, face="bold"),
    #set of x and y labels axis colour, font size, bold and italic
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
  )


#These are the steps we used to plot all the graphs using other independent variables. The only changes in commands occurs when the graph ploting differs which is changing the command from (geom_line to geom_bar).
#2nd independent variable (Rating)
mean_rating=data.frame(value=tapply(worldwide_gross,rating,mean))
mean_rating <- mean_rating[,1]
mean_rating
class(mean_rating)

mean_rating=format(mean_rating, big.mark = ",",scientific = F)
mean_rating
class(mean_rating)

names_rating<- names(tapply(worldwide_gross,rating,mean))
names_rating
class(names_rating)

df_rating <- data.frame(names_rating, mean_rating)
df_rating
str(df_rating)

ggplot(df_rating, aes(x=factor(names_rating), y=mean_rating, fill=factor(names_rating))) + geom_bar(stat="identity")+
  geom_text(aes(label=mean_rating),vjust=-0.5,size=3.0)+
  labs(fill="Rating",x="Rating",y="World Wide Gross($)",title="Mean value of gross film by MPAA Rating")+
  theme(
    axis.text = element_text(colour="black", size=12, face="bold"), 
    plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5),
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
  )+ 
  #rename legend values label with category name instead of integer
  scale_fill_discrete(labels=c("G","PG","PG-13","R"))+
  #rename x axis values label 
  scale_x_discrete(labels=c("G","PG","PG-13","R"))

#3rd independent variable(Year)

mean_year=data.frame(value=tapply(worldwide_gross,year,mean))
mean_year <- mean_year[,1]
mean_year
class(mean_year)

names_year<- names(tapply(worldwide_gross,year,mean))
names_year
class(names_year)

names_year <- as.integer(names_year)
names_year
class(names_year)

df_year <- data.frame(names_year, mean_year)
df_year
str(df_year)

ggplot(df_year,aes(x=names_year, y=mean_year, group=1)) +geom_line(size=1.5)+  
  labs(x="Year",y="World Wide Gross($)",title="Mean Value Of Gross Film By Year Of The Movie")+
  theme_gray()+
  theme(
    axis.text = element_text(colour="black", size=10, face="bold"), 
    plot.title = element_text(color="blue", size=14, face="bold", hjust=1),
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
  )+
  #set the breaks for x axis 
  scale_x_continuous(limits=c(1975, 2018), breaks=c(1975, 1985, 1995, 2005,2015))+
  #set the breaks for y axis according to the quantile
  scale_y_continuous(limits=c(min(mean_year),max(mean_year)), breaks=c(40966740,112308666,376785220,577977625,925565523)) #quantiles

#for y axis, the breaks set according to the quantile below
quantile(sort(mean_year))

#4th independent variable(Imdb_rating)

mean(imdb_rating)# find mean of imdb rating
summary(imdb_rating)#find min and max of imdb rating

#bining the continuous variable "imdb_rating" into certain values and set labels
disImdb <- cut(imdb_rating, breaks = c(0,5,6,7,8,Inf), labels = c("< 5","5-5.99","6-6.99","7-7.99","8 =>"))
disImdb
table(disImdb) #frequency table for imdb_rating

mean_imdb_rating=data.frame(value=tapply(worldwide_gross,disImdb,mean))
mean_imdb_rating <- mean_imdb_rating[,1]
mean_imdb_rating
class(mean_imdb_rating)

mean_imdb_rating=format(mean_imdb_rating, big.mark = ",",scientific = F)
mean_imdb_rating
class(mean_imdb_rating)

names_imdb_rating<- c("< 5","5-5.99","6-6.99","7-7.99","8 =>")
names_imdb_rating
class(names_imdb_rating)

df_imdb_rating <- data.frame(names_imdb_rating, mean_imdb_rating)
df_imdb_rating
str(df_imdb_rating)

ggplot(df_imdb_rating, aes(x=factor(names_imdb_rating), y=mean_imdb_rating, fill=names_imdb_rating)) + geom_bar(stat="identity")+
  geom_text(aes(label=mean_imdb_rating),vjust=-0.5,size=3.0)+
  labs(fill="Imdb Rating",x="Imdb_rating",y="World Wide Gross($)",title="Mean value of gross film by imdb_rating of the movie")+
  theme(
    axis.text = element_text(colour="black", size=12, face="bold"), 
    plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5),
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
  )

#5th independent variable(Length)

mean(length) #find mean length of movies
summary(length) #find min and max of imdb rating

#bin the continuous variable "length" and set labels
dislength <- cut(length, breaks = c(0,60,120,180,Inf), labels = c("< 60","60-120","121-180","180 >"))
dislength
table(dislength) #frequency table for length

mean_length=data.frame(value=tapply(worldwide_gross,dislength,mean))
mean_length <- mean_length[,1]
mean_length
class(mean_length)

mean_length=format(mean_length, big.mark = ",",scientific = F)
mean_length
class(mean_length)
names_length<- c("< 60","60-120","121-180","180 >")
names_length
class(names_length)

df_length <- data.frame(names_length, mean_length)
df_length
str(df_length)

ggplot(df_length, aes(x=names_length, y=mean_length, fill=names_length)) + geom_bar(stat="identity")+
  geom_text(aes(label=mean_length),vjust=-0.5,size=3.0)+
  labs(fill="Length",x="Length(in minutes)",y="World Wide Gross($)",title="Mean Value Of Gross Film By Length Of The Movie")+
  theme(
    axis.text = element_text(colour="black", size=12, face="bold"), 
    plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5),
    axis.title = element_text(color="blue", size=12, face="bold.italic"),
  )+ 
  #rename x axis values label
  scale_x_discrete(limits=c("< 60","60-120","121-180","180 >"))+
  #rename legend values label with category name
  scale_fill_discrete(breaks=c("< 60","60-120","121-180","180 >"))

#-----------------------------END---------------------------






