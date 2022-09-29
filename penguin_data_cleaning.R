#PROJECTS NAME: DATA CLEANING AND EXPLORATION USING 'penguins_raw' data.
## preparing environment by loading needed packages:

View(penguins_raw)

colnames(penguins_raw)

skim_without_charts(penguins_raw)

peng <- penguins_raw %>%
  clean_names()

colnames(peng)

peng_data<-peng %>% 
  select(c(species,region,island,culmen_length_mm,
           culmen_depth_mm,flipper_length_mm,body_mass_g,sex))
peng_data

peng_cleaned<-peng_data %>% 
  drop_na(sex, culmen_length_mm,culmen_depth_mm,flipper_length_mm,body_mass_g)

sep<- peng_cleaned %>% 
  separate(species, into = c("a","b"))

peng_cleaned_data <- sep %>% 
  unite("species", c(a,b), sep = " " )

skim_without_charts(peng_cleaned_data)

# converting Body mass to kg & Formatting values in sex col.
peng_cleaned_data <-peng_cleaned_data %>% 
  mutate(body_mass_kg = body_mass_g/1000, sex= tolower(sex)) %>% 
  select(-body_mass_g)


#DATA EXPLORATION
#1. Which specie of penguins is dominant.
peng_cleaned_data %>% 
  group_by(species) %>% 
  count(species) %>% 
  mutate(percent_species= n*100/333)

peng_cleaned_data %>% 
  ggplot(aes(x=species, fill= species)) + geom_bar(show.legend= FALSE)+
  labs(title = "Dorminant Penguins Species", y= "population",
       subtitle = "Adelie Penguin spcies are the largest with 43.8% of the population") 


#2. Which gender dominates the population
peng_cleaned_data %>% 
  group_by(sex) %>% 
  count(sex) %>% 
  mutate(percent_sex = n*100/333)
 
peng_cleaned_data %>% 
  ggplot() + geom_bar(mapping = aes(x= sex, fill= sex), show.legend = FALSE) + 
  labs(title ="Population By Gender",y= "population", 
       subtitle = "There is 168 males penguins and 165 females penguins in the population.")

#3.What's the correlation between flipper_length and body_mass_kg
peng_cleaned_data %>% 
  select(flipper_length_mm,body_mass_kg)

peng_cleaned_data %>% 
  ggplot(aes(x= body_mass_kg,y =flipper_length_mm)) + geom_point()+
  geom_smooth(method = "loess") +
  labs(title = "Relationship between Flipper Length and Body Mass", subtitle =
         "Positive correlation i.e filpper length increases with the body mass ")

#b.
peng_cleaned_data %>% 
  ggplot(aes(x= body_mass_kg,y =flipper_length_mm, color= species)) + geom_point()+
  geom_smooth(method = "loess") +  facet_wrap(~species) +
  labs(title = "Flipper Length Vs Body Mass Relationship in Each Species",
       subtitle= " The lowest positive correlation is observed in Adellie species")

#4.Which species has the highest average body_mass_kg
peng_cleaned_data %>% 
  group_by(species) %>% 
  summarise(avg_body_mass = mean(body_mass_kg)) %>% 
  ggplot(aes(x= species, y= avg_body_mass, fill= species)) + geom_col(show.legend = FALSE) +
  labs(title = "Average Body Mass(kg) Of Each Species",
       subtitle = "Gentoo penguins species has the highest avaerage mass of 5.09kg.
       Adelie and chinstrap penguins has 3.71kg and 3.73kg respectively.")

#5.Which island has the highest population of penguins and why?

peng_cleaned_data %>% 
  group_by(island) %>% 
  count(island) %>% 
  arrange(-n, .by_group = TRUE)


peng_cleaned_data %>% 
  group_by(island) %>% 
  count(island) %>% 
  arrange(-n, .by_group = TRUE) %>% 
  ggplot(aes(x= island, y= n, fill = island)) + geom_col() + 
  labs(title = "Island with the Highest Population of penguins")

peng_cleaned_data %>% 
  group_by(island,species) %>% 
  count(species) %>% 
  arrange(-n, .by_group = TRUE) %>% 
  ggplot(aes(x= island, y= n, fill = species)) + geom_col() + 
  labs(title = "Island with the Highest Population of penguins",
  caption ="Biscoe island contains all the gentoo penguins (119) used in this dataset and 30% (44) of total adelie speceis.
  Dream island contains all Chinstrap penguin (68) and 38% i.e(55) of gentoo penguin.
  Torgersen island contains only gentoo penguins which is 32%, i.e (47), of the total gentoo population in this dataset")

#6. What is the correlation between the body_mass_kg and culmen_length_mm in general
peng_cleaned_data %>% 
  select(culmen_length_mm,body_mass_kg)

peng_cleaned_data %>% 
  ggplot(aes(x=body_mass_kg,y=culmen_length_mm)) + geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Relation Between Body mass and culmen length")

#6b. What is the correlation between the body_mass_kg and culmen_length_mm (faceting by species)
peng_cleaned_data %>% 
  ggplot(aes(x=body_mass_kg,y=culmen_length_mm, line = species, shape= species, color=species)) + 
  geom_jitter() + geom_smooth(method = "loess") +facet_wrap(~species) +
  labs(title= "Relation Between Body mass and culmen length in each species",
       subtitle ="The two variables show strong positive relation in Chinstrap penguins.
       But less strong positive relationship in the other two species")


#7. What is the correlation between the culmen_depth_mm and culmen_length_mm in general
peng_cleaned_data %>% 
  select(culmen_length_mm,culmen_depth_mm)

peng_cleaned_data %>% 
  ggplot(aes(x=culmen_length_mm,y=culmen_depth_mm)) + 
  geom_jitter() + geom_smooth(method = "loess") +
  labs(title = "Relation between Culmen Depth and Culmen Length",
       subtitle = "There is no any any relationship between the two variable.Hence,the relationship is non-linear")

