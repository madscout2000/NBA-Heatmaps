##Set GMaps API Key and Working Directory
##Remove previous items and set working directory
rm(list = ls())
##Install and Load Required PAckages
install.packages("gmapsdistance")
library(gmapsdistance)
library(tidyverse)
library(ggplot2)
#########Atlantic Division###########
##Arenas in NBA Atlantic Division
Atlantic <- c("TD+Garden+Boston+MA", 
              "Barclays+Center+Brooklyn+NY", 
              "Madison+Square+Garden+New+York+NY", 
              "Wells+Fargo+Center+Philadelphia+PA", 
              "Scotiabank+Arena+Toronto+Canada")
##Load Driving distances between arenas
AtlanticDiv <- gmapsdistance(origin = Atlantic, destination = Atlantic, mode = "driving", key = GMapsKey)
##Convert to miles and round
AtlanticDiv$Distance <- (AtlanticDiv$Distance)/1609.34
ATLDis <- round(AtlanticDiv$Distance[,-1], 2)
ATLDis$or <- AtlanticDiv$Distance$or
##Reformat and rearrange Dataframe to make plotting easy
ATLDis <- ATLDis %>% 
  rename(Origin = or, 
         Boston = `Distance.TD+Garden+Boston+MA`, 
         `Brooklyn - NY` = `Distance.Barclays+Center+Brooklyn+NY`, 
         Manhattan = `Distance.Madison+Square+Garden+New+York+NY`,
         Philadelphia = `Distance.Wells+Fargo+Center+Philadelphia+PA`,
         Toronto = `Distance.Scotiabank+Arena+Toronto+Canada`)
ATLDis$Origin = as.factor(c("Boston Celtics","Brooklyn Nets","New York Knicks","Philadelphia 76ers", "Toronto Raptors"))
ATLDis <- ATLDis %>% gather(Destination, Distance, -Origin)
ATLDis$Distance2 <- NA
##Separate distances for labels
Additions <- c(2, 3, 4, 5, 8, 9, 10, 14, 15, 20)
ATLDis[Additions, 4] <- ATLDis[Additions,3]
##Plot
ATLDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
    geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
    geom_text(aes(label = Distance2), fontface = "bold") +
    scale_fill_gradient(low = "White", high = "red") +
    theme(panel.grid = element_blank(),
          legend.title = element_text(face = "bold"),
          axis.text.y = element_text(size = 10, colour = "black"), 
          axis.text.x = element_text(size = 10, colour = "black"), 
          axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
          axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
          plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
          panel.background = element_rect(fill='white', colour='black'), 
          plot.background = element_rect(fill = 'white', colour = 'black'),
          axis.line = element_line(colour = "black")) +
    scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
    scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
    ggtitle("Intercity Driving Distances (Miles) - NBA Atlantic Division")

##Write dataframe and calculate averages
write_csv(ATLDis, "Atlantic Division Distances.csv")
ATLSum <- ATLDis %>% 
        group_by(Destination) %>% 
        summarise(AverageDivisionalDistance = round(mean(Distance)*5/4, 0))

#########Central Division###########
##Arenas in NBA Central Division
Central <- c("United+Center+Chicago+IL", 
              "Quicken+Loans+Arena+Cleveland+OH", 
              "Little+Caesars+Arena+Detroit+MI", 
              "Bankers+Life+Fieldhouse+Indianapolis+Indiana", 
              "Fiserv+Forum+Milwaukee+WI")
##Load Driving distances between arenas
CentralDiv <- gmapsdistance(origin = Central, destination = Central, mode = "driving", key = GMapsKey)
##Convert to miles and round
CentralDiv$Distance <- (CentralDiv$Distance)/1609.34
CENDis <- round(CentralDiv$Distance[,-1], 2)
CENDis$or <- CentralDiv$Distance$or
##Reformat and rearrange Dataframe to make plotting easy
CENDis <- CENDis %>% 
  rename(Origin = or, 
         Chicago = `Distance.United+Center+Chicago+IL`, 
         Cleveland = `Distance.Quicken+Loans+Arena+Cleveland+OH`, 
         Detroit = `Distance.Little+Caesars+Arena+Detroit+MI`,
         Indianapolis = `Distance.Bankers+Life+Fieldhouse+Indianapolis+Indiana`,
         Milwaukee = `Distance.Fiserv+Forum+Milwaukee+WI`)
CENDis$Origin = as.factor(c("Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers", "Milwaukee Bucks"))
CENDis <- CENDis %>% gather(Destination, Distance, -Origin)
CENDis$Distance2 <- NA
##Separate distances for labels
CENDis[Additions, 4] <- CENDis[Additions,3]
##Plot
CENDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2), fontface = "bold") +
  scale_fill_gradient(low = "White", high = "red") +
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA Central Division")

##Write dataframe and calculate averages
write_csv(CENDis, "Central Division Distances.csv")
CENSum <- CENDis %>% 
  group_by(Destination) %>% 
  summarise(AverageDivisionalDistance = round(mean(Distance)*5/4, 0))

#########Southeast Division###########
##Arenas in NBA Southeast Division
Southeast <- c("State+Farm+Arena+Atlanta+GA",
             "Spectrum+Center+Charlotte+NC",
             "American+Airlines+Arena+Miami+FL", 
             "Amway+Center+Orlando+FL",
             "Capital+One+Arena+Washington+DC")
##Load Driving distances between arenas
SoutheastDiv <- gmapsdistance(origin = Southeast, destination = Southeast, mode = "driving", key = GMapsKey)
##Convert to miles and round
SoutheastDiv$Distance <- (SoutheastDiv$Distance)/1609.34
SEDis <- round(SoutheastDiv$Distance[,-1], 2)
SEDis$or <- SoutheastDiv$Distance$or
##Reformat and rearrange Dataframe to make plotting easy
SEDis <- SEDis %>% 
  rename(Origin = or, 
         Miami = `Distance.American+Airlines+Arena+Miami+FL`, 
         Charlotte = `Distance.Spectrum+Center+Charlotte+NC`, 
         Washington = `Distance.Capital+One+Arena+Washington+DC`,
         Atlanta = `Distance.State+Farm+Arena+Atlanta+GA`,
         Orlando = `Distance.Amway+Center+Orlando+FL`)
SEDis$Origin = as.factor(c("Atlanta Hawks","Charlotte Hornets","Miami Heat", "Orlando Magic", "Washington Wizards"))
SEDis <- SEDis %>% gather(Destination, Distance, -Origin)
SEDis$Distance2 <- NA
##Separate distances for labels
SEDis[Additions, 4] <- SEDis[Additions,3]
##Plot
SEDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2), fontface = "bold") +
  scale_fill_gradient(low = "White", high = "red") +
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA Southeast Division")


##Write dataframe and calculate averages
write_csv(SEDis, "Southeast Division Distances.csv")
SESum <- SEDis %>% 
  group_by(Destination) %>% 
  summarise(AverageDivisionalDistance = round(mean(Distance)*5/4, 0))


#########Pacific Division###########
##Arenas in NBA Pacific Division
Pacific <- c("Staples+Center+Los+Angeles+CA",
             "Staples+Center+Los+Angeles+CA",
             "Oracle+Arena+Oakland+CA", 
             "Talking+Stick+Resort+Arena+Phoenix+AZ",
             "Golden+1+Center+Sacramento+CA")
##Load Driving distances between arenas
PacificDiv <- gmapsdistance(origin = Pacific, destination = Pacific, mode = "driving", key = GMapsKey)
##Convert to miles and round
PacificDiv$Distance <- (PacificDiv$Distance)/1609.34
PACDis <- round(PacificDiv$Distance[,-1], 2)
PACDis$or <- PacificDiv$Distance$or
##Reformat and rearrange Dataframe to make plotting easy
PACDis <- PACDis %>% 
  rename(Origin = or, 
         `Los Angeles` = `Distance.Staples+Center+Los+Angeles+CA`, 
         Oakland = `Distance.Oracle+Arena+Oakland+CA`, 
         Phoenix = `Distance.Talking+Stick+Resort+Arena+Phoenix+AZ`,
         Sacramento = `Distance.Golden+1+Center+Sacramento+CA`)
PACDis$Origin = factor(c("Los Angeles Lakers/Clippers", "Golden State Warriors", "Phoenix Suns", "Sacramento Kings"), levels = c("Los Angeles Lakers/Clippers", "Golden State Warriors", "Phoenix Suns", "Sacramento Kings"))
PACDis <- PACDis %>% gather(Destination, Distance, -Origin)
PACDis$Distance2 <- NA
##Separate distances for labels
Additions2 <- c(2, 3, 4, 7, 8, 12)
PACDis[Additions2, 4] <- PACDis[Additions2,3]
##Plot
PACDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2),fontface = "bold") +
  scale_fill_gradient(low = "White", high = "red") +
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA Pacific Division")

##Write dataframe and calculate averages
write_csv(PACDis, "Pacific Division Distances.csv")
PACSum <- PACDis %>% 
  group_by(Destination) %>% 
  summarise(AverageDivisionalDistance = round(mean(Distance)*4/3, 0))

#########Southwest Division###########
##Arenas in NBA Southwest Division
Southwest <- c("American+Airlines+Center+Dallas+TX",
               "Toyota+Center+Houston+TX",
               "Fedex+Forum+Memphis+TN", 
               "Smoothie+King+Center+New+Orleans+LA",
               "Freeman+Coliseum+San+Antonio+TX")
##Load Driving distances between arenas
SouthwestDiv <- gmapsdistance(origin = Southwest, destination = Southwest, mode = "driving", key = GMapsKey)
##Convert to miles and round
SouthwestDiv$Distance <- (SouthwestDiv$Distance)/1609.34
SWDis <- round(SouthwestDiv$Distance[,-1], 2)
SWDis$or <- SouthwestDiv$Distance$or
##Reformat and rearrange Dataframe to make plotting easy
SWDis <- SWDis %>% 
  rename(Origin = or, 
         Dallas = `Distance.American+Airlines+Center+Dallas+TX`, 
         Houston = `Distance.Toyota+Center+Houston+TX`, 
         Memphis = `Distance.Fedex+Forum+Memphis+TN`,
         `New Orleans` = `Distance.Smoothie+King+Center+New+Orleans+LA`,
         `San Antonio` = `Distance.Freeman+Coliseum+San+Antonio+TX`)
SWDis$Origin = as.factor(c("Dallas Mavericks","Houston Rockets","Memphis Grizzlies", "New Orleans Pelicans", "San Antonio Spurs"))
SWDis <- SWDis %>% gather(Destination, Distance, -Origin)
SWDis$Distance2 <- NA
##Separate distances for labels
SWDis[Additions, 4] <- SWDis[Additions,3]
##Plot
SWDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2), fontface = "bold") +
  scale_fill_gradient(low = "White", high = "red") +
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA Southwest Division")

##Write dataframe and calculate averages
write_csv(SWDis, "Southwest Division Distances.csv")
SWSum <- SWDis %>% 
  group_by(Destination) %>% 
  summarise(AverageDivisionalDistance = round(mean(Distance)*5/4, 0))

#########Northwest Division###########
##Arenas in NBA Northwest Division
Northwest <- c("Pepsi+Center+Denver+CO",
               "Target+Center+Minneapolis+MN",
               "Chesapeake+Energy+Arena+Oklahoma+City+OK", 
               "Moda+Center+Portland+OR",
               "Vivint+Smart+Home+Arena+Salt+Lake+City+UT")
##Load Driving distances between arenas
NorthwestDiv <- gmapsdistance(origin = Northwest, destination = Northwest, mode = "driving", key = GMapsKey)
##Convert to miles and round
NorthwestDiv$Distance <- (NorthwestDiv$Distance)/1609.34
NWDis <- round(NorthwestDiv$Distance[,-1], 2)
NWDis$or <- NorthwestDiv$Distance$or
##Reformat and rearrange Dataframe to make plotting easy
NWDis <- NWDis %>% 
  rename(Origin = or, 
         Denver = `Distance.Pepsi+Center+Denver+CO`, 
         Minneapolis = `Distance.Target+Center+Minneapolis+MN`, 
         `Oklahoma City` = `Distance.Chesapeake+Energy+Arena+Oklahoma+City+OK`,
         Portland = `Distance.Moda+Center+Portland+OR`,
         `Salt Lake City` = `Distance.Vivint+Smart+Home+Arena+Salt+Lake+City+UT`)
NWDis$Origin = as.factor(c("Denver Nuggets","Minnesota Timberwolves","Oklahoma City Thunder", "Portland Trailblazers", "Utah Jazz"))
NWDis <- NWDis %>% gather(Destination, Distance, -Origin)
NWDis$Distance2 <- NA
##Separate distances for labels
NWDis[Additions, 4] <- NWDis[Additions,3]
##Plot
NWDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2), fontface = "bold") +
  scale_fill_gradient(low = "White", high = "red") +
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA Northwest Division")

##Write dataframe and calculate averages
write_csv(NWDis, "Northwest Division Distances.csv")
NWSum <- NWDis %>% 
  group_by(Destination) %>% 
  summarise(AverageDivisionalDistance = round(mean(Distance)*5/4, 0))

#########Eastern Conference###########
##Arenas in NBA Eastern Conference
Eastern1 <- c("State+Farm+Arena+Atlanta+GA",
             "TD+Garden+Boston+MA", 
             "Barclays+Center+Brooklyn+NY",
             "Spectrum+Center+Charlotte+NC",
             "United+Center+Chicago+IL", 
             "Quicken+Loans+Arena+Cleveland+OH", 
             "Little+Caesars+Arena+Detroit+MI")
Eastern2 <- c("Bankers+Life+Fieldhouse+Indianapolis+Indiana",
             "American+Airlines+Arena+Miami+FL", 
             "Fiserv+Forum+Milwaukee+WI",
             "Madison+Square+Garden+New+York+NY",
             "Amway+Center+Orlando+FL",
             "Wells+Fargo+Center+Philadelphia+PA", 
             "Scotiabank+Arena+Toronto+Canada",
             "Capital+One+Arena+Washington+DC")
##Load Driving distances between arenas
EasternDiv1 <- gmapsdistance(origin = Eastern1, destination = Eastern1, mode = "driving", key = GMapsKey)
EasternDiv2 <- gmapsdistance(origin = Eastern1, destination = Eastern2, mode = "driving", key = GMapsKey)
EasternDiv3 <- gmapsdistance(origin = Eastern2, destination = Eastern1, mode = "driving", key = GMapsKey)
EasternDiv4 <- gmapsdistance(origin = Eastern2, destination = Eastern2, mode = "driving", key = GMapsKey)

EastHalf1 <- EasternDiv1$Distance %>% inner_join(EasternDiv2$Distance, by = "or")
EastHalf2 <- EasternDiv3$Distance %>% inner_join(EasternDiv4$Distance, by = "or")

EasternDiv <- EastHalf1 %>% bind_rows(EastHalf2)

##Convert to miles and round
EasternDiv[,2:16] <- round(EasternDiv[,2:16]/1609.34,2)
EASDis <- EasternDiv

##Reformat and rearrange Dataframe to make plotting easy
EASDis <- EASDis %>% 
  rename(Origin = or, 
         Atlanta = `Distance.State+Farm+Arena+Atlanta+GA`, 
         Boston = `Distance.TD+Garden+Boston+MA`, 
         `Brooklyn - NY` = `Distance.Barclays+Center+Brooklyn+NY`,
         Charlotte = `Distance.Spectrum+Center+Charlotte+NC`,
         Chicago = `Distance.United+Center+Chicago+IL`,
         Cleveland = `Distance.Quicken+Loans+Arena+Cleveland+OH`, 
         Detroit = `Distance.Little+Caesars+Arena+Detroit+MI`,
         Indianapolis = `Distance.Bankers+Life+Fieldhouse+Indianapolis+Indiana`,
         Miami = `Distance.American+Airlines+Arena+Miami+FL`,
         Milwaukee = `Distance.Fiserv+Forum+Milwaukee+WI`,
         `New York (Manhattan)` = `Distance.Madison+Square+Garden+New+York+NY`,
         Orlando = `Distance.Amway+Center+Orlando+FL`, 
         Philadelphia = `Distance.Wells+Fargo+Center+Philadelphia+PA`, 
         Toronto = `Distance.Scotiabank+Arena+Toronto+Canada`,
         `Washington DC` = `Distance.Capital+One+Arena+Washington+DC`)
EASDis$Origin = factor(c("Atlanta Hawks","Boston Celtics","Brooklyn Nets", "Charlotte Hornets",
                         "Chicago Bulls", "Cleveland Cavaliers", "Detroit Pistons",
                         "Indiana Pacers", "Miami Heat", "Milwaukee Bucks", "New York Knicks",
                         "Orlando Magic", "Philadelphia 76ers", "Toronto Raptors", "Washington Wizards"),
                       levels = c("Atlanta Hawks","Boston Celtics","Brooklyn Nets", "Charlotte Hornets",
                                  "Chicago Bulls", "Cleveland Cavaliers", "Detroit Pistons",
                                  "Indiana Pacers", "Miami Heat", "Milwaukee Bucks", "New York Knicks",
                                  "Orlando Magic", "Philadelphia 76ers", "Toronto Raptors", "Washington Wizards"))
EASDis <- EASDis %>% gather(Destination, Distance, -Origin)
EASDis$Distance2 <- NA
##Separate distances for labels
for (i in 1:15){
  for(j in i:15){
    EASDis[((i-1)*15)+j,4] <- EASDis[((i-1)*15)+j,3] 
  }
  EASDis[(16*(i-1)+1),4] <- NA
}

##Plot
EASDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2), fontface = "bold") +
  scale_fill_gradient(low = "White", high = "red") +
  theme(legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA Eastern Conference")

##Write dataframe and calculate averages
write_csv(EASDis, "Eastern Conference Distances.csv")
EASSum <- EASDis %>% 
  group_by(Destination) %>% 
  summarise(AverageConferenceDistance = round(mean(Distance)*15/14, 0))

#########Western Conference###########
##Arenas in NBA Western Conference
Western1 <- c("American+Airlines+Center+Dallas+TX",
              "Pepsi+Center+Denver+CO",
              "Oracle+Arena+Oakland+CA", 
              "Toyota+Center+Houston+TX",
              "Staples+Center+Los+Angeles+CA",
              "Fedex+Forum+Memphis+TN", 
              "Target+Center+Minneapolis+MN")
Western2 <- c("Smoothie+King+Center+New+Orleans+LA",
              "Chesapeake+Energy+Arena+Oklahoma+City+OK", 
              "Talking+Stick+Resort+Arena+Phoenix+AZ",
              "Moda+Center+Portland+OR",
              "Golden+1+Center+Sacramento+CA",
              "Freeman+Coliseum+San+Antonio+TX",
              "Vivint+Smart+Home+Arena+Salt+Lake+City+UT")

##Load Driving distances between arenas
WesternDiv1 <- gmapsdistance(origin = Western1, destination = Western1, mode = "driving", key = GMapsKey)
WesternDiv2 <- gmapsdistance(origin = Western1, destination = Western2, mode = "driving", key = GMapsKey)
WesternDiv3 <- gmapsdistance(origin = Western2, destination = Western1, mode = "driving", key = GMapsKey)
WesternDiv4 <- gmapsdistance(origin = Western2, destination = Western2, mode = "driving", key = GMapsKey)

WestHalf1 <- WesternDiv1$Distance %>% inner_join(WesternDiv2$Distance, by = "or")
WestHalf2 <- WesternDiv3$Distance %>% inner_join(WesternDiv4$Distance, by = "or")

WesternDiv <- WestHalf1 %>% bind_rows(WestHalf2)

##Convert to miles and round
WesternDiv[,2:15] <- round(WesternDiv[,2:15]/1609.34,2)
WESDis <- WesternDiv

##Reformat and rearrange Dataframe to make plotting easy
WESDis <- WESDis %>% 
  rename(Origin = or, 
         Dallas = `Distance.American+Airlines+Center+Dallas+TX`, 
         Denver = `Distance.Pepsi+Center+Denver+CO`, 
         Oakland = `Distance.Oracle+Arena+Oakland+CA`,
         Houston = `Distance.Toyota+Center+Houston+TX`,
         `Los Angeles` = `Distance.Staples+Center+Los+Angeles+CA`,
         Memphis = `Distance.Fedex+Forum+Memphis+TN`, 
         Minneapolis = `Distance.Target+Center+Minneapolis+MN`,
         `New Orleans` = `Distance.Smoothie+King+Center+New+Orleans+LA`,
         `Oklahoma City` = `Distance.Chesapeake+Energy+Arena+Oklahoma+City+OK`,
         Phoenix = `Distance.Talking+Stick+Resort+Arena+Phoenix+AZ`,
         Portland = `Distance.Moda+Center+Portland+OR`,
         Sacramento = `Distance.Golden+1+Center+Sacramento+CA`, 
         `San Antonio` = `Distance.Freeman+Coliseum+San+Antonio+TX`, 
         `Salt Lake City` = `Distance.Vivint+Smart+Home+Arena+Salt+Lake+City+UT`)
WESDis$Origin = factor(c("Dallas Mavericks","Denver Nuggets","Golden State Warriors", "Houston Rockets",
                       "LA Lakers/Clippers", "Memphis Grizzlies", "Minnesota Timberwolves",
                         "Pelicans", "OKC Thunder", "Phoenix Suns",
                         "Portland Trailblazers", "Sacramento Kings", "San Antonio Spurs", "Utah Jazz"),
                          levels = c("Dallas Mavericks","Denver Nuggets","Golden State Warriors", "Houston Rockets",
                       "LA Lakers/Clippers", "Memphis Grizzlies", "Minnesota Timberwolves",
                       "Pelicans", "OKC Thunder", "Phoenix Suns",
                       "Portland Trailblazers", "Sacramento Kings", "San Antonio Spurs", "Utah Jazz"))
WESDis <- WESDis %>% gather(Destination, Distance, -Origin)
WESDis$Distance2 <- NA
##Separate distances for labels
i <- 1
for (i in 1:14){
    for(j in i:14){
     WESDis[((i-1)*14)+j,4] <- WESDis[((i-1)*14)+j,3] 
    }
  WESDis[(15*(i-1)+1),4] <- NA
}
WESDis$Destination <- factor(WESDis$Destination, levels = c("Dallas", "Denver", "Oakland", "Houston", "Los Angeles",
                                                            "Memphis","Minneapolis","New Orleans","Oklahoma City",
                                                            "Phoenix","Portland","Sacramento","San Antonio",
                                                            "Salt Lake City")) 
##Plot
WESDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
    geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
    geom_text(aes(label = Distance2), fontface = "bold") +
    scale_fill_gradient(low = "white", high = "red") +
  theme(legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
      scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
      scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
      ggtitle("Intercity Driving Distances (Miles) - NBA Western Conference")

##Write dataframe and calculate averages
write_csv(WESDis, "Western Conference Distances.csv")
WESSum <- WESDis %>% 
  group_by(Destination) %>% 
  summarise(AverageConferenceDistance = round(mean(Distance)*14/13, 0))

####League!!!#######
##Define order of arenas
League1 <- c("State+Farm+Arena+Atlanta+GA",
             "TD+Garden+Boston+MA", 
             "Barclays+Center+Brooklyn+NY",
             "Spectrum+Center+Charlotte+NC",
             "United+Center+Chicago+IL", 
             "Quicken+Loans+Arena+Cleveland+OH",
             "American+Airlines+Center+Dallas+TX",
             "Pepsi+Center+Denver+CO",
             "Little+Caesars+Arena+Detroit+MI",
             "Oracle+Arena+Oakland+CA")
League2 <- c("Toyota+Center+Houston+TX",
             "Bankers+Life+Fieldhouse+Indianapolis+Indiana",
             "Staples+Center+Los+Angeles+CA",
             "Fedex+Forum+Memphis+TN",
             "American+Airlines+Arena+Miami+FL", 
             "Fiserv+Forum+Milwaukee+WI",
             "Target+Center+Minneapolis+MN",
             "Smoothie+King+Center+New+Orleans+LA",
             "Madison+Square+Garden+New+York+NY")
League3 <- c("Chesapeake+Energy+Arena+Oklahoma+City+OK",
             "Amway+Center+Orlando+FL",
             "Wells+Fargo+Center+Philadelphia+PA",
             "Talking+Stick+Resort+Arena+Phoenix+AZ",
             "Moda+Center+Portland+OR",
             "Golden+1+Center+Sacramento+CA",
             "Freeman+Coliseum+San+Antonio+TX",
             "Scotiabank+Arena+Toronto+Canada",
             "Vivint+Smart+Home+Arena+Salt+Lake+City+UT",
             "Capital+One+Arena+Washington+DC")
##obtain distances piecewise
L1 <- gmapsdistance(origin = League1, destination = League2, mode = "driving", key = GMapsKey)
L2 <- gmapsdistance(origin = League1, destination = League1, mode = "driving", key = GMapsKey)
L3 <- gmapsdistance(origin = League1, destination = League3, mode = "driving", key = GMapsKey)
L4 <- gmapsdistance(origin = League2, destination = League1, mode = "driving", key = GMapsKey)
L5 <- gmapsdistance(origin = League2, destination = League2, mode = "driving", key = GMapsKey)
L6 <- gmapsdistance(origin = League2, destination = League3, mode = "driving", key = GMapsKey)
L7 <- gmapsdistance(origin = League3, destination = League1, mode = "driving", key = GMapsKey)
L8 <- gmapsdistance(origin = League3, destination = League2, mode = "driving", key = GMapsKey)
L9 <- gmapsdistance(origin = League3, destination = League3, mode = "driving", key = GMapsKey)
##Piece together distance matrix
LP1 <- L2$Distance %>% inner_join(L1$Distance, by = "or") %>% 
          inner_join(L3$Distance, by = "or")
LP2 <- L4$Distance %>% inner_join(L5$Distance, by = "or") %>% 
          inner_join(L6$Distance, by = "or")
LP3 <- L7$Distance %>% inner_join(L8$Distance, by = "or") %>% 
          inner_join(L9$Distance, by = "or")

##Convert to miles
League <- LP1 %>% bind_rows(LP2) %>% bind_rows(LP3)
League[,2:30] <- round(League[,2:30]/1609.34, 2)
LeagueDis <- League

##Reformat and rearrange Dataframe to make plotting easy
LeagueDis <- LeagueDis %>% 
  rename(Origin = or, 
         Atlanta = `Distance.State+Farm+Arena+Atlanta+GA`, 
         Boston = `Distance.TD+Garden+Boston+MA`, 
         `Brooklyn - NY` = `Distance.Barclays+Center+Brooklyn+NY`,
         Charlotte = `Distance.Spectrum+Center+Charlotte+NC`,
         Chicago = `Distance.United+Center+Chicago+IL`,
         Cleveland = `Distance.Quicken+Loans+Arena+Cleveland+OH`, 
         Dallas = `Distance.American+Airlines+Center+Dallas+TX`, 
         Denver = `Distance.Pepsi+Center+Denver+CO`, 
         Detroit = `Distance.Little+Caesars+Arena+Detroit+MI`,
         Oakland = `Distance.Oracle+Arena+Oakland+CA`,
         Houston = `Distance.Toyota+Center+Houston+TX`,
         Indianapolis = `Distance.Bankers+Life+Fieldhouse+Indianapolis+Indiana`,
         `Los Angeles` = `Distance.Staples+Center+Los+Angeles+CA`,
         Memphis = `Distance.Fedex+Forum+Memphis+TN`, 
         Miami = `Distance.American+Airlines+Arena+Miami+FL`,
         Milwaukee = `Distance.Fiserv+Forum+Milwaukee+WI`,
         Minneapolis = `Distance.Target+Center+Minneapolis+MN`,
         `New Orleans` = `Distance.Smoothie+King+Center+New+Orleans+LA`,
         `New York (Manhattan)` = `Distance.Madison+Square+Garden+New+York+NY`,
         `Oklahoma City` = `Distance.Chesapeake+Energy+Arena+Oklahoma+City+OK`,
         Orlando = `Distance.Amway+Center+Orlando+FL`,
         Philadelphia = `Distance.Wells+Fargo+Center+Philadelphia+PA`, 
         Phoenix = `Distance.Talking+Stick+Resort+Arena+Phoenix+AZ`,
         Portland = `Distance.Moda+Center+Portland+OR`,
         Sacramento = `Distance.Golden+1+Center+Sacramento+CA`, 
         `San Antonio` = `Distance.Freeman+Coliseum+San+Antonio+TX`,
         Toronto = `Distance.Scotiabank+Arena+Toronto+Canada`,
         `Salt Lake City` = `Distance.Vivint+Smart+Home+Arena+Salt+Lake+City+UT`,
         `Washington DC` = `Distance.Capital+One+Arena+Washington+DC`)
LeagueDis$Origin = factor(c("Atlanta Hawks","Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
                            "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks","Denver Nuggets",
                            "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
                            "Los Angeles Lakers/Clippers", "Memphis Grizzlies", "Miami Heat",
                            "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", 
                            "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
                            "Portland Trailblazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
                            "Utah Jazz", "Washington Wizards"),
                 levels = (c("Atlanta Hawks","Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
                            "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks","Denver Nuggets",
                            "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
                            "Los Angeles Lakers/Clippers", "Memphis Grizzlies", "Miami Heat",
                            "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", 
                            "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
                            "Portland Trailblazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
                            "Utah Jazz", "Washington Wizards")))
LeagueDis <- LeagueDis %>% gather(Destination, Distance, -Origin)
LeagueDis$Distance2 <- NA
##Separate distances for labels and space team names for plotting
for (i in 1:29){
  for(j in i:29){
    LeagueDis[((i-1)*29)+j,4] <- LeagueDis[((i-1)*29)+j,3] 
  }
  LeagueDis[(30*(i-1)+1),4] <- NA
}
LeagueDis$Destination <- factor(LeagueDis$Destination, 
                                levels = c("Atlanta", "Boston", "Brooklyn - NY", "Charlotte", "Chicago", "Cleveland",
                                           "Dallas", "Denver", "Detroit", "Oakland", "Houston", "Indianapolis",
                                           "Los Angeles", "Memphis", "Miami", "Milwaukee",  "Minneapolis",
                                           "New Orleans", "New York (Manhattan)", "Oklahoma City", "Orlando", 
                                           "Philadelphia", "Phoenix", "Portland","Sacramento","San Antonio",
                                           "Toronto", "Salt Lake City", "Washington DC")) 
levels(LeagueDis$Origin) <- gsub(" ", "\n", levels(LeagueDis$Origin))
##Plot
LeagueDis %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2), size = 3, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA")

##Write dataframe and calculate averages
write_csv(LeagueDis, "League Distances.csv")
LeagueSum <- LeagueDis %>% 
  group_by(Destination) %>% 
  summarise(AverageLeagueDistance = round(mean(Distance)*29/28, 0))

###ALTERNATE PLOT###########
LeagueDis2 <- League
LeagueDis2 <- LeagueDis2 %>% 
  rename(Origin = or, 
         Atlanta = `Distance.State+Farm+Arena+Atlanta+GA`, 
         Boston = `Distance.TD+Garden+Boston+MA`, 
         `Brooklyn - NY` = `Distance.Barclays+Center+Brooklyn+NY`,
         Charlotte = `Distance.Spectrum+Center+Charlotte+NC`,
         Chicago = `Distance.United+Center+Chicago+IL`,
         Cleveland = `Distance.Quicken+Loans+Arena+Cleveland+OH`, 
         Dallas = `Distance.American+Airlines+Center+Dallas+TX`, 
         Denver = `Distance.Pepsi+Center+Denver+CO`, 
         Detroit = `Distance.Little+Caesars+Arena+Detroit+MI`,
         Oakland = `Distance.Oracle+Arena+Oakland+CA`,
         Houston = `Distance.Toyota+Center+Houston+TX`,
         Indianapolis = `Distance.Bankers+Life+Fieldhouse+Indianapolis+Indiana`,
         `Los Angeles` = `Distance.Staples+Center+Los+Angeles+CA`,
         Memphis = `Distance.Fedex+Forum+Memphis+TN`, 
         Miami = `Distance.American+Airlines+Arena+Miami+FL`,
         Milwaukee = `Distance.Fiserv+Forum+Milwaukee+WI`,
         Minneapolis = `Distance.Target+Center+Minneapolis+MN`,
         `New Orleans` = `Distance.Smoothie+King+Center+New+Orleans+LA`,
         `New York (Manhattan)` = `Distance.Madison+Square+Garden+New+York+NY`,
         `Oklahoma City` = `Distance.Chesapeake+Energy+Arena+Oklahoma+City+OK`,
         Orlando = `Distance.Amway+Center+Orlando+FL`,
         Philadelphia = `Distance.Wells+Fargo+Center+Philadelphia+PA`, 
         Phoenix = `Distance.Talking+Stick+Resort+Arena+Phoenix+AZ`,
         Portland = `Distance.Moda+Center+Portland+OR`,
         Sacramento = `Distance.Golden+1+Center+Sacramento+CA`, 
         `San Antonio` = `Distance.Freeman+Coliseum+San+Antonio+TX`,
         Toronto = `Distance.Scotiabank+Arena+Toronto+Canada`,
         `Salt Lake City` = `Distance.Vivint+Smart+Home+Arena+Salt+Lake+City+UT`,
         `Washington DC` = `Distance.Capital+One+Arena+Washington+DC`)
LeagueDis2 <- LeagueDis2 %>% select(Origin, Boston, `Brooklyn - NY`, `New York (Manhattan)`, Philadelphia, Toronto, 
                      Chicago, Cleveland, Detroit, Indianapolis, Milwaukee, Atlanta, Charlotte, Miami,
                      Orlando, `Washington DC`, Denver, Minneapolis, `Oklahoma City`, Portland, `Salt Lake City`,
                      Oakland, `Los Angeles`, Phoenix, Sacramento, Dallas, Houston, Memphis, `New Orleans`, `San Antonio`)
LeagueDis2$Origin = factor(c("Atlanta Hawks","Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
                            "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks","Denver Nuggets",
                            "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
                            "Los Angeles Lakers/Clippers", "Memphis Grizzlies", "Miami Heat",
                            "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", 
                            "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
                            "Portland Trailblazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
                            "Utah Jazz", "Washington Wizards"),
                  levels = (c("Boston Celtics","Brooklyn Nets", "New York Knicks", "Philadelphia 76ers",
                              "Toronto Raptors", "Chicago Bulls", "Cleveland Cavaliers","Detroit Pistons",
                              "Indiana Pacers", "Milwaukee Bucks", "Atlanta Hawks", "Charlotte Hornets",
                              "Miami Heat", "Orlando Magic", "Washington Wizards", "Denver Nuggets", "Minnesota Timberwolves",
                              "Oklahoma City Thunder", "Portland Trailblazers", "Utah Jazz", "Golden State Warriors",
                              "Los Angeles Lakers/Clippers", "Phoenix Suns", "Sacramento Kings", "Dallas Mavericks",
                              "Houston Rockets", "Memphis Grizzlies", "New Orleans Pelicans", "San Antonio Spurs")))
LeagueDis2 <- LeagueDis2 %>% gather(Destination, Distance, -Origin)
LeagueDis2$Distance2 <- NA
##Separate distances for labels and space team names for plotting
LeagueDis2$Destination <- factor(LeagueDis2$Destination, 
                                 levels = c("Boston", "Brooklyn - NY", "New York (Manhattan)", "Philadelphia",
                                            "Toronto", "Chicago", "Cleveland", "Detroit", "Indianapolis", "Milwaukee",
                                            "Atlanta", "Charlotte", "Miami", "Orlando", "Washington DC", "Denver",
                                            "Minneapolis", "Oklahoma City", "Portland", "Salt Lake City",  "Oakland",
                                            "Los Angeles", "Phoenix", "Sacramento", "Dallas", "Houston", "Memphis",
                                            "New Orleans", "San Antonio")) 
levels(LeagueDis2$Origin) <- gsub(" ", "\n", levels(LeagueDis2$Origin))
LeagueDis3 <- LeagueDis2 %>% arrange(Origin, Destination)
for (i in 1:29){
  for(j in i:29){
    LeagueDis3[((i-1)*29)+j,4] <- LeagueDis3[((i-1)*29)+j,3] 
  }
  LeagueDis3[(30*(i-1)+1),4] <- NA
}
LeagueDis3 %>% 
  ggplot(aes(x = Origin, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance2), size = 3, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Intercity Driving Distances (Miles) - NBA")

###Final Summary Dataframe Creation and Plot####
DivSum <- ATLSum %>% bind_rows(CENSum, SESum, NWSum, PACSum, SWSum)
DivSum[3,1] <- "New York (Manhattan)"
DivSum[15,1] <- "Washington DC"
ConSum <- EASSum %>% bind_rows(WESSum)
LeagueSum2 <-  DivSum %>% 
  inner_join(ConSum, by = "Destination") %>% 
  inner_join(LeagueSum, by = "Destination")
LeagueSum2$Destination <- factor(LeagueSum2$Destination, 
                                 levels = c("Boston", "Brooklyn - NY", "New York (Manhattan)", "Philadelphia",
                                            "Toronto", "Chicago", "Cleveland", "Detroit", "Indianapolis", "Milwaukee",
                                            "Atlanta", "Charlotte", "Miami", "Orlando", "Washington DC", "Denver",
                                            "Minneapolis", "Oklahoma City", "Portland", "Salt Lake City",  "Oakland",
                                            "Los Angeles", "Phoenix", "Sacramento", "Dallas", "Houston", "Memphis",
                                            "New Orleans", "San Antonio")) 
LeagueSum2 <- LeagueSum2 %>% 
  gather(Parameter, Distance, -Destination)
write_csv(LeagueSum2,"Average Distances.csv")

LeagueSum2 %>% 
  ggplot(aes(x = Parameter, y = Destination)) +
  geom_tile(aes(fill = Distance), colour = "black",alpha = 0.8) +
  geom_text(aes(label = Distance), fontface = "bold") +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 12, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5, colour = "black"), 
        panel.background = element_rect(fill='white', colour='black'), 
        plot.background = element_rect(fill = 'white', colour = 'black'),
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Origin City", expand = c(0, 0)) +
  scale_y_discrete(name = "Destination City", expand = c(0, 0)) +
  ggtitle("Average Distances Per City - NBA")
