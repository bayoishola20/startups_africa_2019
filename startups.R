require(tidyr)
require(dplyr)
require(ggplot2)
require(RColorBrewer)
require(reshape2)
require(extrafont)
font_import() # to fetch font from OS


setwd("C:/Users/Adebayo/Documents/Data-Science-All")
data <- read.csv("startups.csv", sep = ",", header=T) # read data with header

View(data) #View data's dataframe
class(data) # view data type
str(data) # view data structure and properties

levels(data$Country.HQ) # examine countries where HQs are located.
levels(data$Sector) # examine sectors.


summary(data) # basic statistics of the data

#=====================#
##     By Country   ##
#===================#

by_country <- data %>%
              group_by(Country.HQ) %>%
              summarise( country_count = n() ) %>%
              arrange(desc(Country.HQ))

by_country <- as.data.frame(by_country)

#===================#
##     By Sector  ##
#=================#

by_sector <- data %>%
  group_by(Sector) %>%
  summarise( sector_count = n() ) %>%
  arrange(desc(Sector))

by_sector <- as.data.frame(by_sector)


#======================#
##     By Launch Year ##
#=====================#

by_launch <- data %>%
  group_by(Launch) %>%
  summarise( launch_count = n() ) %>%
  arrange(desc(Launch))

by_launch <- as.data.frame(by_launch)

#======================#
##     By Amount ##
#=====================#

by_amount <- data %>%
  group_by(Country.HQ) %>%
  summarise( Amount = sum(Amount, na.rm = TRUE )  ) %>%
  arrange(desc(Country.HQ))

by_amount <- as.data.frame(by_amount)

#===================================PLOTS=============================#

#===============================#
##     LOCATION OF STARTUP HQs ##
#===============================#

ggplot(by_country, aes(x=by_country$Country.HQ, y=by_country$country_count)) +
  geom_bar(stat="identity", fill = "darkgray") +
  labs(x = "Country HQ", y = "Count") +
  geom_col() +
  coord_flip() +
  theme_classic() +
  ggtitle("LOCATION OF STARTUP HQs") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90, hjust=1),
        text=element_text(size=14,  family="Garamond", face="bold"),
        axis.line = element_line(colour = "black")) +
  ggsave(file = "Location.png",width = 20, height = 15, units = "cm", dpi = 1200)

#===========================#
##     SECTORS OF STARTUPS ##
#===========================#

ggplot(by_sector, aes(x=by_sector$Sector, y=by_sector$sector_count)) +
  geom_bar(stat="identity", fill = "darkgray") +
  labs(x = "Sector", y = "Count") +
  geom_col() +
  coord_flip() +
  theme_classic() +
  ggtitle("SECTORS OF STARTUPS") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90, hjust=1),
        text=element_text(size=14,  family="Garamond", face="bold"),
        axis.line = element_line(colour = "black")) +
  ggsave(file = "Sectors.png",width = 20, height = 15, units = "cm", dpi = 1200)


#===========================#
##   STARTUP LAUNCH YEAR   ##
#==========================#

ggplot(by_launch, aes(x=as.character(by_launch$Launch), y=by_launch$launch_count)) +
  geom_bar(stat="identity", fill = "darkgray") +
  labs(x = "Year", y = "Count") +
  # geom_col() +
  # coord_flip() +
  theme_classic() +
  ggtitle("LAUNCH YEARS OF STARTUPS") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90, hjust=1),
        text=element_text(size=14,  family="Garamond", face="bold"),
        axis.line = element_line(colour = "black")) +
  ggsave(file = "Launch.png",width = 20, height = 15, units = "cm", dpi = 1200)

#=============================================#
##   STARTUP HQ LOCATION AND FUNDING RAISED  ##
#============================================#

ggplot(by_amount, aes(x=as.character(by_amount$Country.HQ), y=by_amount$Amount)) +
  geom_bar(stat="identity", fill = "darkgray") +
  labs(x = "Country", y = "Amount(million dollars)") +
  # geom_col() +
  # coord_flip() +
  theme_classic() +
  ggtitle("STARTUP HQ LOCATION AND FUNDING RAISED") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90, hjust=1),
        text=element_text(size=14,  family="Garamond", face="bold"),
        axis.line = element_line(colour = "black")) +
  ggsave(file = "Amount.png",width = 20, height = 15, units = "cm", dpi = 1200)




