# 
# 
# LOAD PACKAGES---------------------!!!!!!!!!!!!!!!!
library(pacman)
library(tidyverse)

# prep dataset----------------------
read.csv("pokemon_data.csv")
  # I actually want to FILE>IMPORT DATASET>FROM TEXT READR 
    # this will provide a better import of data than x<-read.csv
master<-pokemon_data # rename the dataset
rm(pokemon_data)
view(master)

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
# based on statistical analysis I might even recategorize this later on!
master2$Rating<-ifelse(master2$Total <=329, "Weak",
                 ifelse(master2$Total >=330 & master2$Total <=434, "Good",
                     ifelse(master2$Total >=435 & master2$Total <=514, "Strong",
                         ifelse(master2$Total >=515, "Ultra"))))
# doesn't work because IFELSE only accepts 3 conditions!!!

# MUTATE and CASE_WHEN solves this issue!! SUCCESS
master2<-master2 %>% 
  mutate(Rating=case_when(
    Total <=329 ~ "Weak",
    Total >=330 & Total <=434 ~ "Good",
    Total >=435 & Total <=514 ~ "Strong",
    Total >=515 ~ "Ultra",
  )) # %>% 
  # select(-Total)
# %>% select(-Total) would replace total with the new conditional column!


# FIRST ROUND CHARTING---------------------

# looking at HP
# by default here I get an error - >30 bins (big dataset) so bins are reduced
ggplot(master2,aes(x=HP))+
  geom_histogram(bins=20)+
  geom_vline(aes(xintercept=mean(HP)))

# I might also want to see density
ggplot(master2,aes(x=HP))+
  geom_histogram(aes(y=..density..),bins=20)+
  geom_density(alpha=.2, fill="pink")

# Or using color to split by type
ggplot(master2,aes(x=HP, color=Type1))+
  geom_histogram(bins=10,fill="white")

# obviously it is simply too much data so I should make some summaries

# average HP only?
typesavg<-master2 %>% 
  group_by(Type1) %>% 
  summarize(avgHP=mean(HP))
rm(typesavg) # REMOVE

# how about average of every column? summarize_if allows me to run the desired
  # calculation on each column if it meets the criteria = IS.NUMERIC!
typesmean<-master2 %>% 
  group_by(Type1) %>% 
  summarise_if(is.numeric,mean,na.rm=TRUE) 

typesmean<-select(typesmean,-'#') # remove that "#" column (pokedex no.)

# looks better. but maybe a bit vague
ggplot(typesmean,aes(x=HP, color=Type1))+
  geom_histogram(bins=10, fill="white")

# back to the full dataset, this might give a better category breakdown
ggplot(master2,aes(x=HP, color=Type1))+
  geom_boxplot()

# or this, with a Y axis
ggplot(master2,aes(x=HP,y=Attack, color=Type1))+
  geom_boxplot()

  # or this violin plot
ggplot(master2,aes(x=HP,y=Attack, color=Type1))+
  geom_violin()