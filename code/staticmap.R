#Generates map of the density of jobs in the US by state

library(readxl)
library(tidyverse)
library(tigris)
library(viridis)
#Read in data


joejobs <- read_excel(file.path(datapath, "joe_resultset.xls", fsep = .Platform$file.sep))

#just include US jobs
joejobs<- joejobs %>% 
  filter(grepl("UNITED STATES", joejobs$locations))

#This file provides us with names, fips codes for states and counties
fipsandnames <- read_excel(file.path(datapath, "fipsandnames.xlsx", fsep = .Platform$file.sep))
#for this we only want states. 040 is code for states
fipsandnames <- fipsandnames %>%
  filter(region_type=="040")

#initialize variable
joejobs$state<-""

#Loop to match names within string
for (i in 1:nrow(fipsandnames)){
  joejobs$state[grepl(fipsandnames$name[i], joejobs$locations)]<-
    fipsandnames$name[i]
}
#district of columbia
joejobs$state[grepl("District", joejobs$locations)]<-"District of Columbia"


#summary stats by state
statesum<-joejobs %>% 
  group_by(state) %>% 
  summarize(n=n())

#consolidate fips codes 
fipsandnames <- fipsandnames %>%
  select(State_fips, name)
colnames(fipsandnames)<-c("GEOID", "state")

#add in fips codes to the main data
statesum<-statesum %>%
  left_join(fipsandnames)

#now prep the map - using tigris pull out map data
state_df<-states(cb=TRUE) %>%
  shift_geometry()

#join the data we are going to plot and get rid of US territories
state_df <- state_df %>%
  left_join(statesum) %>%
  filter(!(GEOID %in% c(60,66,69,72,78)))

#plot map
ggplot(data=state_df)+
  geom_sf(aes(fill=n), color=NA)+
  coord_sf(datum=NA) +
  labs(caption="Source: Job Openings for Economists")+
  scale_fill_viridis(direction = -1, name="Number of Postings")

#save plot
ggsave(filename=file.path(figurespath, "joemap.png", fsep = .Platform$file.sep))
