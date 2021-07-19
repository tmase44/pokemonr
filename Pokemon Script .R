# INSTALL PACKAGES-------------------------------------------------------

install.packages("tmap")
install.packages("tmaptools") # run in console not script
install.packages("tidyverse")
install.packages("tidyr")
install.packages("sf")

# LOAD PACKAGES---------------------!!!!!!!!!!!!!!!!

library(pacman) # stats
library(tidyverse) # tidyr, ggplot2, dplyr, tibble, readr
library(tmap) # plot and visualize maps
  # includes leaflet (interactive javascript maps)
  # includes sf simple features for GEOS geometric operations
library(tmaptools) # bonus tmaps features
library(sf) # this is a pain to install..

# prep dataset----------------------

read.csv("pokemon_data.csv")
  # I actually want to FILE>IMPORT DATASET>FROM TEXT READR 
    # this will provide a better import of data than x<-read.csv
master<-pokemon_data # rename the dataset
rm(pokemon_data)
view(master)

# I am also importing a file with locations pokemon_data_map
map<-pokemon_data_map
rm(pokemon_data_map)

# data first look------------------

class(master) 
ls(master) #variable list
dim(master) # rows, cols
summary(master$`Type 1`) 

# MODIFICATIONS------------------

# first make a copy of original data
master2<-master 

#rename columns because the spaces or dots might cause issues later 
master2<-rename(master2, Type1 = 'Type 1')
master2<-rename(master2, Type2 = 'Type 2')
master2<-rename(master2, SpAtk = 'Sp. Atk')
master2<-rename(master2, SpDef = 'Sp. Def')

# Type 1 is just a character vector. I want to make it a factor
master2$Type1<-factor(master2$Type1) # convert
  summary (master2$Type1) # confirm. Success!
master2$Type2<-factor(master2$Type2) # repeat for Type 2
  summary(master2$Type2) 

# Generation should also be a factor but it's not a character 
class(master22$Generation) # integer
  master2$Generation<-as.character(master2$Generation) # convert to character
  master2$Generation<-factor(master2$Generation) # character to factor
  summary(master2$Generation) # SUCCESS
  
# check NAs
colSums(is.na(master2)) # sum of NA per column, only Type 2 is an issue
    # 386 NA in Type 2

# I want a new column "Total" which sums up HP thru to Speed
master2$Total<-as.numeric(apply(master[,5:10],1,sum))
  summary(master2$Total) # I want to label Pokemon based on total power
  # summary shows me the power ranges and I could decide that
    # <329 is Weak
    # 330-434 is Good
    # 435-514 is Strong
    # >515 is Ultra
        # I might re-categorize this later on
master2$Rating<-ifelse(master2$Total <=329, "Weak",
                 ifelse(master2$Total >=330 & master2$Total <=434, "Good",
                     ifelse(master2$Total >=435 & master2$Total <=514, "Strong",
                         ifelse(master2$Total >=515, "Ultra"))))
# doesn't work because IFELSE only accepts 3 conditions!!!

# MUTATE and CASE_WHEN solves this issue!! 

master2<-master2 %>% 
  mutate(Rating=case_when(
    Total <=329 ~ "Weak",
    Total >=330 & Total <=434 ~ "Good",
    Total >=435 & Total <=514 ~ "Strong",
    Total >=515 ~ "Ultra",
  )) 
# adding  %>% select(-Total) replaces total with the new conditional column

# MERGING DATA SETS
  # I already made sure the data is aligned in Excel beforehand
    # spatial data and master 3 will be merged

map2<-map %>% 
  select('Location','Longitude','Latitude') # extract desired cols - map data
summary(map2)
# merge
master3$Location<-map2$Location 
master3$Longitude<-map$Longitude
master3$Latitude<-map$Latitude
# clean up
rm(master2)
rm(map)
rm(map2)

# FIRST ROUND CHARTING---------------------

# looking at HP
# by default here I get an error - >30 bins (big dataset) so bins are reduced
ggplot(master3,aes(x=HP))+
  geom_histogram(bins=20)+
  geom_vline(aes(xintercept=mean(HP)))

# I might also want to see density
ggplot(master3,aes(x=HP))+
  geom_histogram(aes(y=..density..),bins=20)+
  geom_density(alpha=.2, fill="pink")

# Or using color to split by type
ggplot(master3,aes(x=HP, color=Type1))+
  geom_histogram(bins=10,fill="white")

# obviously it is simply too much data so I should make some summaries

# average HP only?
typesavg<-master3 %>% 
  group_by(Type1) %>% 
  summarize(avgHP=mean(HP))
rm(typesavg) # REMOVE

# how about average of every column? summarize_if allows me to run the desired
  # calculation on each column if it meets the criteria = IS.NUMERIC!
typesmean<-master3 %>% 
  group_by(Type1) %>% 
  summarise_if(is.numeric,mean,na.rm=TRUE) 

typesmean<-select(typesmean,-'#') # remove that "#" column (pokedex no.)

# looks better. but maybe a bit vague
ggplot(typesmean,aes(x=HP, color=Type1))+
  geom_histogram(bins=10, fill="white")

# back to the full dataset, this might give a better category breakdown
ggplot(master3,aes(x=HP, y=Type1))+
  geom_boxplot()

# or this, with a Y axis
ggplot(master3,aes(x=HP,y=Attack, color=Type1))+
  geom_boxplot()

  # or this violin plot
ggplot(master3,aes(x=HP,y=Attack, color=Type1))+
  geom_violin()

### I can see from these charts that the total data is too messy
  # let's try a regression with everything, then some random samples to see.

# REGRESSION - HP as a predictor of all stats among whole population

lmhp<-lm(HP~Attack+Defense+SpAtk+SpDef+Speed,
         data=master3)
lmhp
summary(lmhp)

# results----
  # Attack & SpDef have extremely high correlations in the total population

# but this was on the total population, how about a random sample?

sample<-master3[sample(nrow(master3),50),] # re-run for a new sample

lmhp2<-lm(HP~Attack+Defense+SpAtk+SpDef+Speed,
         data=sample)
lmhp2
summary(lmhp2)  
# im getting different results each time which means I need to narrow down
rm(lmhp)
rm(lmhp2)
rm(sample)
# BY TYPE - let's focus on Water types, the largest population
  # within water are various type 2's, so let's explore Water pokemon

water<-master3 %>% # make the subset data
  subset(Type1=='Water') %>% 
  select(-'#',-Longitude,-Latitude,-Location) # not useful in this analysis

watermeans<-water %>% # group by Type 2
  group_by(Type2) %>% 
  summarise_if(is.numeric,mean,na.rm=FALSE)
  #1st
lmw<-lm(HP~Attack+Defense+SpAtk+SpDef+Speed,
        data=water)
summary(lmw) # regression of water pokemon - Attack & SpDef correlation
  #2nd
lmwm<-lm(HP~Attack+Defense+SpAtk+SpDef+Speed,
        data=watermeans)
summary(lmwm) # regression of means by group - poor correlation

# then regression should be checked for each, lets chart and look
water %>% 
  ggplot(aes(HP,color=Type2))+
  geom_histogram(bins=20,fill="white")
  # let's try another view
water %>% 
  ggplot(aes(HP,Type2,color=Type2))+
  geom_point()+
  theme(legend.position = "none")


# BY GENERATION



  # count of each type
poptype<-count(master3,Type1) # good practice to separate the charting data

poptype %>% 
ggplot(aes(x=reorder(Type1,-n),y=n))+ # (x=reorder(Type1,-n) allows for hi-low sorting
  geom_bar(stat='identity')+ # without 'identity' it will not chart
  labs(title="Species count by Type",
       x="Type",
       y="Species count")+
  theme(axis.text.x = element_text(colour = "grey20", size = 6, angle = 90, 
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 6),
        text = element_text(size = 12))
  
# 1. Stats by type-----------------
# Total by type
ggplot(master3,aes(x=Type1, y=Total))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))

# total scatter Attack and Defense
ggplot(master3,aes(x=))