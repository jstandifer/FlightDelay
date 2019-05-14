#Title: Has your flight been delayed? Canceled? Mine too.
# Flight delays have affected each of us. More than once I have, while crying, yelled at (equally helpless) airline staff. If that doesn't convince you of the importance of flight delays and cancelations, the Department of Transportation published that in 2017, 965 million passengers flew on U.S. carriers and foriegn carriers which operate in the United States. This is a record high for air travel. In 2017, U.S. flight cancelations affected nearly 18 million customers. That does not include passengers affected by delay - those numbers are significantly higher. Given such high rates of delay, this project hopes to identify annual trends in delay and the geography of delay. 


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tidyverse)

#First, I created a wordcloud of airport codes!

text<-("ATL
       LAX
       ORD
       DFW
       DEN
       JFK
       LAS
       CLT
       EWR
       MIA
       IAH
       BOS
       DTW
       FLL
       BWI
       DCA
       IAD
       HNL
       DAL
       STL
       BNA
       AUS
       HOU
       MCI
       CLE
       ")
docs <- Corpus(VectorSource(text))


set.seed(1234)
wordcloud(words = docs,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8,"Pastel2"))


#Next, before talking about delays in 2017, how have on-time, delay, and cancelation rates changed over the years? 

library(ggplot2)
ggdelays<-read.csv("/Users/MotherBoard2.0/208 Final Project/Final.csv")
head(ggdelays)
library(hrbrthemes)


ggplot() + geom_bar(aes(y = Percent, x = Year, fill = Type), data = ggdelays,
                    stat="identity")+
  scale_fill_brewer(palette="Set2")+
  theme_ipsum()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="right",
        legend.title=element_blank())+
  scale_x_continuous(breaks = c(2009:2018), 
                     labels = factor(2009:2018), 
                     limits = c(2008,2019)) +
  ggtitle("Changes in Delays and Cancelations 2009-2018")


# So more people each year are traveling, and delay/cancelation rates aren't improving. Given that so many of us have issues while flying during the holidays, how do holiday delays compare to the average annual rate of delay/cancelation? (In this instance, "holidays" are defined by December 25 - January 1st)

comp<-read.csv("/Users/MotherBoard2.0/208 Final Project/annualcomp.csv")
head(comp)

comp1<-comp%>%
  mutate(count=1)

ggplot(comp1, aes(Year,On.Time, fill = Delay.Type)) + 
  geom_bar(stat="identity",position = "dodge")+
  theme_ipsum()+ 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="right",
        legend.title=element_blank())+
  scale_x_continuous(breaks = c(2009:2017), 
                     labels = factor(2009:2017), 
                     limits = c(2008,2018))+
  ggtitle("Comparing Holiday and Regular On-time Flights")+theme(plot.title = element_text(hjust = 0.5))

# Holiday delays are even worse! Is that surprising though? Now that we know delay/cancelation rates are fairly high, lets dig into the geography of delay. Where are major airports in the U.S. located?

library(plotly)
library(hrbrthemes)
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')

head(air)

g <- list(
  scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 50,
  projection = list(
    type = 'conic conformal',
    rotation = list(lon = -100)
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-140, -55),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(20, 60),
    dtick = 5
  )
)

MajorAirports <- plot_geo(air, lat = ~lat, lon = ~long)%>%
  add_markers(
    text = ~paste(airport, city, state, sep = "<br />"),
    color = "#8c045a", hoverinfo = "text"
  )%>%
  colorbar(title = "") %>%
  layout(
    title = 'Major U.S. Aiports', geo = g
  )

MajorAirports
htmlwidgets::saveWidget(MajorAirports, "index.html")

# Which airports are the busiest? This uses 2017 passenger emboardings as a measure of customer numbers, capturing numbers of passengers boarding flights, but not including passenger arrival numbers.

library("devtools")
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggmap)
library(ggplot2)
library(png)
library(hrbrthemes)
BAirport<-read.csv("/Users/MotherBoard2.0/BusiestAirports_clean.csv")
Busy_Airports<-BAirport%>%
  mutate(count=1)


ggplot(BAirport,aes(x=reorder(Airport.Code, Enplanements17),y=Enplanements17))+
  geom_bar(stat = "identity",width = 0.7,fill="#69b3a2")+
  coord_flip()+
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none")+xlab("")+ylab("in millions")+ggtitle("Which U.S. airports have the most passenger boardings?")

# Ok, so which airports have the highest/lowest rates of delay? Do high rates of delay occur at busy airports?

Airportdelays<-read.csv("/Users/MotherBoard2.0/208 Final Project/AirportDelays.csv")
head(Airportdelays)

ggplot(Airportdelays,aes(x=reorder(codes, Ontime),y=Ontime))+
  geom_bar(stat = "identity",width = 0.5,fill="#69b3a2")+
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none",axis.text.x = element_text(angle=65, vjust=0.6))+xlab("")+ylab("")+ggtitle("Which Major Airports have the Highest Rate of On-time Flights?")+
  theme(plot.title = element_text(hjust = 0.5))

#Yup, Newark continues to take the cake for most delayed U.S. airport.


#Let's explore the causes for delay between the most delayed and least delayed airports.

library(ggplot2)
library(hrbrthemes)
library(plotly)
delaycause<-read.csv("/Users/MotherBoard2.0/208 Final Project/delaycomp.csv")
head(delaycause)

delaycause1<-delaycause%>%
  mutate(count=1)
del<-ggplot(delaycause1, aes(Issue,Amount, fill = Airport)) + 
  geom_bar(stat="identity",position = "dodge")+
  theme_ipsum()+
  scale_y_discrete("")+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="right",axis.text.x = element_text(angle=65, vjust=0.6))+xlab("")+ylab("")+ggtitle("Causes of Delay")+
  theme(plot.title = element_text(hjust = 0.5))

de<-ggplotly(del)
de
api_create(de, filename = "Delay_Causes_EWR_MSP")

#Airports, and specifically an airport's location, plays a major role in delay. What other factors could contribute to delay? Average ticket prices?


cost<-read.csv("/Users/MotherBoard2.0/208 Final Project/AverageFare.csv")

colnames(cost)<-c("PassangerRank","AirportCode","Airport","City","State","Fare","AFare", "Passengers")

head(cost)

library(ggplot2)
library(hrbrthemes)
library(plotly)

p <- ggplot(data=cost,aes(x=State, y=Fare,size=Fare))+
  geom_point(aes(text = paste(Airport,'<br>',Passengers)),shape=1)+
  geom_smooth(aes(colour = "#333333", fill = "#333333"))+theme_ipsum()+
  scale_y_discrete("Average Fare (Q4 2017)", 20000, 100000)+scale_x_discrete("State", 20000, 100000)+
  ggtitle("Average Fare by Airport")+
  theme(plot.title = element_text(hjust = 0.5))

gg<-ggplotly(p)
gg

api_create(gg, filename = "Ticket_Cost")

#We can see that customers have a limit, and won't pay much over $500 for a ticket. Price averages are similar throughout the country at small and large airports.

#Just a fun addition: What are the most delayed routes? This data is released monthly, the most up to date data came from September 2018.


arc<- read.csv("/Users/Motherboard2.0/208 Final Project/Mdelayed.csv")
head(arc)

library(threejs)

(earth <- system.file("images/world.jpg",  package="threejs")) 


arc_df <- data.frame(origin_lat = arc[,2], origin_long = arc[,3], dest_lat = arc[,4], dest_long = arc[,5])

globe<- globejs(img=earth, arcs=arc_df,
                arcsHeight=0.4, arcsLwd=2, arcsColor="#FF0000", arcsOpacity=0.18,
                atmosphere=TRUE, height=1000, width = 1000)


#To wrap up - this study has a number of limitations. While airports hold a significant share of blame for delay, this study overlooks the airline's role in delay. Furthermore, there are numerous other factors which were not considered in the scope of this study.

# Next steps: I had hoped that I could create an interactive visual or gif showing a memorable image of most delayed routes. I plan to keep working the interactive delayed flight route globe to open my portfolio page with a show stopper!
