##### Assignment 1 - BINF 6210 Software Tools #####
##### Thomas Papp-Simon (1219370)
##### 07/10/2022

##### Introduction
# I believe that dolphins are some of the most fascinating animals on this planet. They are one of the few groups of animals capable of forming and maintaining complex social structures, and also have one of the largest brain to body mass ratios (Connor, 2007; Marino et al., 2006). The Delphinidae family (more commonly known as "oceanic dolphins" or "delphinids") is said to be comprised of 37 species (McGowen et al., 2019). However, it is noted that the phylogeny and classification of this taxonomic group is not completely understood due to several factors such as recent adaptive radiation and hybridization (McGowen et al., 2019). 
# The geographic distribution of delphinids varies widely as they can be found in every ocean and nearly every sea in the world (Forcada, 2009). As a result, I am interested in analyzing the geographic distribution of this family. Specifically, I want to know the number of species and BINs within the Atlantic and Pacific oceans. Furthermore, I want to also compare the BIN composition of this family between the two oceans.


# Installing packages that are going to be used in this script. Uncomment any of these lines if you need to.
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("mapview")
# install.packages("vegan")


# Load packages that were installed and used for this assignment
library(tidyverse)
library(sf)
library(mapview)
library(vegan)

# Downloading the dataset for my taxonomic group of choice "Delphinidae" from the API tool from the BOLD database (06/10/2022)

df_Delphinidae <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Delphinidae&format=tsv") # 1100 observations as of 06/10/2022

# Some functions to refer to when I want to see what columns to analyze from my data frame.
#summary(df_Delphinidae) # 80 variables
#names(df_Delphinidae)

##### Part 1 - Data Exploration

# BIN and species data
# To start off, I will look into how many unique BINs are found within the dataset. 
# Then I will also look at how many unique species are identified in our dataset and compare the results.

# How many missing BINs are there?
sum(is.na(df_Delphinidae$bin_uri)) # 177 missing BINs in our data, which means that most of the records have a BIN associated with them.

# It looks to me like there are 106 missing BINs in the data but this could just be due to a more recent download of the data from BOLD.

# How many unique BINs are there?
length(unique(na.omit(df_Delphinidae$bin_uri))) # There are 24 unique BINs in the Delphinidae data frame (excluding missing values)

# How many records don't have a species name associated with them?
sum(is.na(df_Delphinidae$species_name)) # 14 records don't have a species name associated with them (NA)

# How many unique species are found in this dataset? How does this compare to the amount of species found in the literature?
length(unique(na.omit(df_Delphinidae$species_name))) # At least 29 unique species in this data frame, whereas the literature states there are 37 species (McGowen et al., 2019)

#What is the ratio of BINs to species? 
length(unique(na.omit(df_Delphinidae$bin_uri))) / length(unique(na.omit(df_Delphinidae$species_name)))
# We get a ratio of 0.828, which means...
# Mention this in discussion             

# Now I will be taking a closer look into the records that don't have a species name associated with them.
# How many of these records without a species name have a BIN associated with them?
df_BINspeciesname <- select(df_Delphinidae, "bin_uri", "species_name") # Used the select() function from the dplyr package to subset the 2 columns from the df_Delphinidae data frame and put them into a new data frame.

# filter function is used to filter every record that has a BIN, but does not have a species name associated with it.

# We can do this a little easier making use of the distinct function, so we filter out any non-unique values and this way you don't  have to manually pick out unique values.
BINs_nospecies <- df_BINspeciesname %>%
  filter(!is.na(bin_uri)) %>%
  filter(is.na(species_name))%>%
  distinct(bin_uri, species_name) # This is a new line that I inserted - adding both bin_uri and species_name makes sure both columns are retained.
BINs_nospecies
# We get 3 records that have BINs that don't have a species name associated with them, but only 2 of them are unique: BOLD:ABY4191 and BOLD:AAD4421


# We can search for both of these BINs within the dataframe to see if there any matches:
# BOLD:ABY4181
# Here again, we can add a single addition to our filtering to only retain unique values and ensure you're not missing anything by manually checking the species name columns
df_ABY4181 <- df_Delphinidae %>%
  select(species_name, bin_uri) %>%
  filter(bin_uri == "BOLD:ABY4181")%>%
  distinct(species_name, bin_uri) # same as above
df_ABY4181
# It appears that this BIN can be associated with the Stenella longirostris species.

# Here again, we can add a single addition to our filtering to only retain unique values and ensure you're not missing anything by manually checking the species name columns

# BOLD:AAD4421
df_AAD4421 <- df_Delphinidae %>%
  select(species_name, bin_uri) %>%
  filter(bin_uri == "BOLD:AAD4421")%>%
  distinct(species_name,bin_uri) %>% # same as above
  filter(!is.na(species_name)) # I built this into your original dplyr call as well. As such, you can delete the check for number of species below.
count(df_AAD4421) # now that we used the distinct function in our original dplyr call, we can just count the entries in the df_AAD4421 data frame - it looks like there are 7, the same as in your original code.

# Interestingly, there are many different species associated with this particular BIN.

# Specifically, there are at least 7 different species associated with this BIN!

# Let's check if the members of this BIN are within the same genus.
df_AAD4421_genus <- df_Delphinidae %>%
  select(species_name, bin_uri, genus_name) %>%
  group_by(genus_name) %>%
  filter(bin_uri == "BOLD:AAD4421")
length(unique(df_AAD4421_genus$genus_name)) 
# Specimens with this BIN are associated with 4 different genera.


# What are the 3 most common species in the dataset? 
# Make a variable for the table of the frequency of each species in the dataset.
spec_count <- table(df_Delphinidae$species_name)%>%
  sort(df_Delphinidae$species_name, decreasing = TRUE) # This a bit nitpicky, but I added it into your dplyr call so that the new data frame includes the sort. This way, you could hypothetically sort from here and manipulate in other ways if need be.
spec_count
# Then we can sort that table in ascending order, and return the names of the last 3 species with the names() function and tail() function
tail(names(sort(spec_count, decreasing = FALSE)), 3) # Gives the top 3 most common species: "Orcinus orca", "Tursiops truncatus", and "Stenella longirostris"


# Check the distribution of the institution_storing variable to see where most of the data is from. 
sum(is.na(df_Delphinidae$institution_storing)) # There are no NA's in this variable
length(unique(df_Delphinidae$institution_storing)) # There are 6 unique values for the institution_storing variable


# Looking at the distribution of the source of the sequence data. 
# I will make a table of all the different values for the institution_storing variable, as well as the count for each one of them.
inst_count_table <- df_Delphinidae %>% 
  group_by(institution_storing) %>% 
  count() %>%
  arrange(-n)
inst_count_table
# Overwhelmingly mined from GenBank, not very interesting. 



##### Part 2 - Analysis to address my main research question(s): What is the geographical distribution of the Delphinidae family between the Atlantic and Pacific Ocean? Also, how does the BIN composition of the Delphinidae family compare between the Atlantic and Pacific ocean?

##### Geographical data analysis

# How many missing values for the country variable?
sum(is.na(df_Delphinidae$country)) # 867 missing values for the country variable, which isn't very good for analyzing geographic distribution, but we will work with what we have.
# Number of unique countries in our dataset?
length(unique(na.omit(df_Delphinidae$country))) # 20 unique countries

# Which country is the origin of the most records in our dataset?
df_CountryRecords <- df_Delphinidae %>%
  group_by(country) %>%
  filter(!is.na(country)) %>%
  count()
  tail(arrange(df_CountryRecords, n), 1) # Brazil has the most records (86)
  

# Looking at records that don't have a specific country name, but were instead associated with the ocean they were collected from
nonspecific_records <- grep("Ocean", df_Delphinidae$country) # Use the grep() function to match the word "ocean" in my dataset and then getting the process IDs of these records
df_Delphinidae$processid[nonspecific_records] # We have 8 records that don't have a specific country associated with them (that aren't missing values)


# How many missing values for the longitude and latitude variables?
sum(is.na(df_Delphinidae$lon)) 
sum(is.na(df_Delphinidae$lat)) 
# 988 missing values for both longitude and latitude, which is unfortunate, but we will work with what we have.

# The goal is to look at the geographical distribution of the Delphinidae family
# In particular, I'd like to analyze and compare their distribution/composition between the Atlantic and Pacific oceans.

# I need to organize records with longitude/latitude data into separate groups that correspond with the specimen's ocean of origin.

# Making a subset dateframe of the columns we're going to use for this process:
df_Delphinidae_subset1 <- select(df_Delphinidae, "species_name", "bin_uri", "lat", "lon", "country")

# Filter for records that have longitude and latitude data
df_Delphinidae_subset1 <- df_Delphinidae_subset1 %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lon))

# Defining Atlantic ocean boundaries 
# The 20° meridian east longitude is considered to be the eastern boundary of the Atlantic ocean (where it meets the Indian ocean)
# The -67° longitude is the western boundary of the Atlantic ocean (where it meets the Pacific ocean)
# Source: Limits of Oceans and Seas by International Hydrographic Organization (1953) https://web.archive.org/web/20161013224338/http://www.iho.int/iho_pubs/standard/S-23/S-23_Ed3_1953_EN.pdf
df_Delphinidae_Atlantic <- df_Delphinidae_subset1 %>% 
  filter(20 >= lon) %>%
  filter(-67 <= lon)
# There are 99 records that meet these criteria, which suggests that most of our specimens are collected from the Atlantic ocean

# Defining Pacific ocean boundaries (same source)
# The 146° longitude corresponds to the western border of the Pacific, all the way to the -67° longitude where it meets with the Atlantic Ocean
df_Delphinidae_Pacific <- df_Delphinidae_subset1 %>%
  filter(146 <= lon | -67 >= lon)
# There are 9 records that were collected from the Pacific ocean.

# Defining Indian ocean boundaries
# For the Indian Ocean, we will take latitude into consideration (30° North).
df_Delphinidae_Indian <- df_Delphinidae_subset1 %>%
  filter(146 >= lon) %>%
  filter(20 <= lon) %>%
  filter(30 >= lat)
# There are 3 records that were collected from the Indian ocean.

# There is one more record from Greece that does not fall into any of these groups.
# We can assume this sample was taken from the Mediterranean Sea based off of its coordinates.
df_Delphinidae_Medi <- df_Delphinidae_subset1 %>%
  filter(6 <= lon) %>%
  filter(36 >= lon) %>%
  filter(30 <= lat) %>%
  filter(46 >= lat)

# Now that every record with longitude/latitude data has been grouped into their appropriate groups, we can analyze each group separately.

# Figure 1:
# I wanted to visualize the data points that had a GPS coordinate associated with them on a world map.
# I searched online and found out that I can use the mapview and sf packages together to do this.
# Sources I used for this: 
# https://map-rfun.library.duke.edu/01_georeference.html
# https://r-spatial.github.io/mapview/articles/mapview_01-basics.html

# The st_as_sf() function converts a foreign object to an sf (simple features) object that is compatible with other functions (like mapview)
# I separated the groups into their own sf objects so that I can make their data points on the map different colors. Each group will be considered a layer.
my_sf_Atlantic <- st_as_sf(df_Delphinidae_Atlantic, coords = c('lon','lat'), crs=4326)
my_sf_Pacific <- st_as_sf(df_Delphinidae_Pacific, coords = c('lon','lat'), crs=4326) # The crs argument refers to the coordinate reference system that is assigned to this object. 4326 refers to the WGS84 system, which is a common standard in map projection.
my_sf_Indian <- st_as_sf(df_Delphinidae_Indian, coords = c('lon','lat'), crs=4326)
my_sf_Medi <- st_as_sf(df_Delphinidae_Medi, coords = c('lon','lat'), crs=4326)

# Then we can combine each layer to showcase all data points on one map with different colors!
fig1_map <- mapview(my_sf_Atlantic, col.regions = "Cyan", map.types = "Esri.WorldImagery") +
    mapview(my_sf_Pacific, col.regions = "Red", map.types = "Esri.WorldImagery") +
    mapview(my_sf_Indian, col.regions = "Purple", map.types = "Esri.WorldImagery") +
    mapview(my_sf_Medi, col.regions = "Yellow", map.types = "Esri.WorldImagery")
# Generate the map
fig1_map

##### Figure 1: World map with specimen data points with coordinate data from the Delphinidae family (n = 112). Data points are colour-coded based on their latitude (, Red = Pacific Ocean, Cyan = Atlantic Ocean, Yellow = Mediterranean, Purple = Indian Ocean).  

# By looking at this map, we can see that there are many data points grouped in the Atlantic Ocean near South America (particularly Brazil), as well as a few data points in the North-Eastern region of the Atlantic Ocean, which corresponds with our data.


# How many unique BINs are there in the Atlantic and Pacific oceans?

df_Delphinidae_BINcount_Atlantic <- df_Delphinidae_Atlantic %>%
  group_by(bin_uri) %>%
  filter(!is.na(bin_uri)) %>%
  count(bin_uri)

df_Delphinidae_BINcount_Pacific <- df_Delphinidae_Pacific %>%
  group_by(bin_uri) %>%
  filter(!is.na(bin_uri)) %>%
  count(bin_uri)

Atl_uniqBIN <- nrow(df_Delphinidae_BINcount_Atlantic) # 5 Unique BINs in the Atlantic Ocean
Pcf_uniqBIN <- nrow(df_Delphinidae_BINcount_Pacific) #  4 Unique BINs in the Pacific Ocean
# This is interesting considering there are far less records from the Pacific relative to the Atlantic, yet there is only one less unique BIN.


# I will make a grouped bar plot to showcase and compare the number of specimens sampled from the Atlantic Ocean and Pacific Ocean, as well as the number of unique BINs from each ocean.
# Setting up the variables for the grouped bar plot
Atl_Spcmn_Count <- nrow(df_Delphinidae_Atlantic) 
Pcfc_Spcmn_Count <- nrow(df_Delphinidae_Pacific) 

subgroup <- c("Atlantic Ocean", "Pacific Ocean")
value <- c(Atl_Spcmn_Count,Pcfc_Spcmn_Count,Atl_uniqBIN,Pcf_uniqBIN)
counting <- c(rep("Number of Specimens",2), rep("Number of Unique BINs",2))

df_Spcmn_BIN_count <- data.frame(counting,subgroup,value)

# Figure 2: 
# Grouped bar plot using the ggplot package
fig2_barplot <- ggplot(df_Spcmn_BIN_count, aes(fill=subgroup, y=value, x=counting)) + 
  geom_bar(position="dodge", stat="identity") + # In order to get the grouped bars, need to specify the position argument to dodge.
  geom_text(aes(label = value), vjust = -0.5, position = position_dodge(0.9)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 110)) +
  ggtitle("Specimen and Unique BIN Counts in Atlantic and Pacific") +
  labs(y = "", x = "", fill = "") + # Fixing the labels of my axes
  scale_fill_brewer(palette = "Dark2") + # Colour-blind-friendly palette 
  theme_minimal() + # Chose a minimalist theme to remove background/grid of the default bar plot
  # Fixing some of the aesthetics of my plot (bolded title and labels, thicker axes lines)
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.line.y = element_line(size = 1), 
    axis.line.x = element_line(size = 1), 
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    plot.title = element_text(family = "", face = "bold", colour = "black", size = 14, hjust = 0.2))

# Generating Figure 2
fig2_barplot

##### Figure 2: Grouped bar plot comparing the number of specimens sampled and the number of unique BINs in the Atlantic and Pacific Ocean (n = 112).


# How similar is the BIN composition between the Atlantic and Pacific ocean groups?

# I will use the vegdist() function from the vegan package to compare the BIN composition between Atlantic and Pacific ocean records
# The output of the vegdist() function is a distance matrix with dissimilarity indices.
# There are many algorithms to choose from for the vegdist() function. I chose the bray-curtis algorithm for the purposes of this assignment because it is known to be sufficient for detecting underlying ecological gradients. (Source: https://peat-clark.github.io/BIO381/veganTutorial.html )

df_Delphinidae_BINcount_Atlantic$ocean <- c("Atlantic")
df_Delphinidae_BINcount_Pacific$ocean <- c("Pacific")

# Setting up our data for BIN composition comparison
df_Ocean_BINcounts <- as.data.frame(rbind(df_Delphinidae_BINcount_Atlantic, df_Delphinidae_BINcount_Pacific))
# Converting my dataframe into a community object, which is required for using functions from the vegan package.
df_Ocean_BINcounts_spread <- pivot_wider(data = df_Ocean_BINcounts, names_from = bin_uri, values_from = n)

#Removing ocean origin from column and putting it as row names
df_Ocean_BINcounts_spread <- df_Ocean_BINcounts_spread %>%
  remove_rownames %>%
  column_to_rownames(var="ocean")
# Setting all NAs to 0
df_Ocean_BINcounts_spread[is.na(df_Ocean_BINcounts_spread)] <- 0


bray_BINs = vegdist(df_Ocean_BINcounts_spread, "bray") 

# Figure 3: 
# Histogram to look at the distribution of dissimilarity scores between Atlantic and Pacific BINs.

fig3_histogram <- hist(bray_BINs, 
     main = "Distribution of BIN Dissimilarity Scores", 
     col = "#56B4E9",
     breaks = 2,
     xlab = "Dissimilarity score",
     xlim = range(0,1)) # Dissimilarity score ranges from 0 to 1, where 1 indicates maximum dissimilarity (https://peat-clark.github.io/BIO381/veganTutorial.html)

##### Figure 3: Histogram depicting the distribution of BIN dissimilarity indices between BINs from the Atlantic and Pacific oceans. This was calculated using the vegdist() function from the vegan package using the Bray-Curtis algorithm.


##### Part 3 - Results & Discussion
# As mentioned before, the literature states that there are 37 known species that make up the Delphinidae family (McGowen et al., 2019). While exploring the Delphinidae dataset (n = 1100 samples) from the BOLD database, only 29 unique species were able to be identified. Additionally, there were only 24 unique BINs. With this information I was able to calculate a ratio of unique BIN to unique species: 0.828. While exploring the dataset, I also found some samples with BINs that did not have a species name associated with them. Specifically, one of the BINs (BOLD:AAD4421) was found to be associated with 7 different species of the Delphinidae family. These results indicate that different species were assigned to the same BIN. As mentioned before, the Delphinidae family has undergone recent adaptive radiation and hybridization, which might result in some ambiguity when looking at the phylogenetic relationships within this family (McGowen et al., 2019). The adaptive radiation of this family might be a result of its wide geographic distribution, leading to increased intraspecific divergence as a result (Gaytan et al., 2020). I then went on to look into where the specimen data was obtained from. Disappointingly, most of the data was mined from GenBank which was not very interesting. Only a small fraction of the data was actually obtained from institutions (43 out of 1100 records). 
# To answer my research question, I needed to organize my data into separate groups using GPS coordinate data. Unfortunately, only 112 records from the dataset had this data available so it was not totally representative of the entire dataset. Nonetheless, these records were organized into groups based on their coordinate data. Then, I used this to create a map showcasing the location each data point was taken from (Figure 1). There was a notable difference in the amount of samples taken from the Atlantic Ocean (n = 99) compared to the Pacific Ocean (n = 9). Despite this difference, they had nearly the same number of unique BINs (Figure 2). I thought that this was very interesting, as this might indicate increased diversity in the Pacific relative to the Atlantic. However, a larger sample size and more research would have to be done to arrive to that conclusion. Finally, I went on to compare the BIN composition of this family between the Atlantic and Pacific Oceans. This was done by calculating dissimilarity indices between the unique BINs for each group (Atlantic Ocean and Pacific Ocean) and then displaying the scores on a histogram (Figure 3). The calculation of these indices was done by using the Bray-Curtis dissimilarity algorithm, which is regularly used in ecological analyses (Clark, 2017). The results of this algorithm will return a distance matrix between values 0 to 1, where 1 indicates a high degree of dissimilarity. Because I am only comparing two geographic regions (Atlantic vs Pacific), there is only one dissimilarity index that is returned (0.926). This score indicates a large dissimilarity between the BIN compositions of the Atlantic and Pacific oceans. This is indicative of a high level of genetic divergence among this family probably due to their geographic separation. Again, an increased sample size would have given us more accurate and representative results for this analysis. Future research in this topic could involve a deeper look into the effect of geographic distribution on the genetic divergence of a taxonomic group. Specifically, looking at their distribution across different latitudes could grant more insight into the ambiguous phylogeny of this family.



##### Acknowledgments
# I would like to thank Jesse Wolf and Omar Amin for their feedback on the written code and the figures presented in this project.

##### References

# Adding Labels to a Bar Graph. https://r-graphics.org/recipe-bar-graph-labels

# Clark, Peter. 2017. Vegan tutorial. https://peat-clark.github.io/BIO381/veganTutorial.html

# Connor, Richard C. 2007. “Dolphin Social Intelligence: Complex Alliance Relationships in Bottlenose Dolphins and a Consideration of Selective Environments for Extreme Brain Size Evolution in Mammals.” Phil. Trans. R. Soc. B 362, 587–602.

# Forcada, Jaume. 2009. “Distribution.” In Encyclopedia of Marine Mammals (Second Edition), edited by William F. Perrin, Bernd Würsig, and J. G. M. Thewissen, 316–21. London: Academic Press. 

# Gaytán, Álvaro, Johannes Bergsten, Tara Canelo, Carlos Pérez-Izquierdo, Maria Santoro, and Raul Bonal. 2020. “DNA Barcoding and Geographical Scale Effect: The Problems of Undersampling Genetic Diversity Hotspots.” Ecology and Evolution 10 (19): 10754–72. 

# Grouped barchart. https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2

# Latitude and Longitude Coordinates. https://map-rfun.library.duke.edu/01_georeference.html

# Mapview basics. https://r-spatial.github.io/mapview/articles/mapview_01-basics.html

# Marino, Lori, Daniel Sol, Kristen Toren, and Louis Lefebvre. 2006. “DOES DIVING LIMIT BRAIN SIZE IN CETACEANS?” Marine Mammal Science 22 (2): 413–25.

# McGowen, Michael R, Georgia Tsagkogeorga, Sandra Álvarez-Carretero, Mario dos Reis, Monika Struebig, Robert Deaville, Paul D Jepson, et al. 2020. “Phylogenomic Resolution of the Cetacean Tree of Life Using Target Sequence Capture.” Systematic Biology 69 (3): 479–501. 

# Number of unique elements excluding NA in R. https://stackoverflow.com/questions/33575740/number-of-unique-elements-excluding-na-in-r

# Patrick, Cameron. 2020. Making beautiful bar charts with ggplot. https://cameronpatrick.com/post/2020/03/beautiful-bar-charts-ggplot/

# R Plot Color Combinations that are Colorblind Accessible. https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible

# Subset rows using column values. https://dplyr.tidyverse.org/reference/filter.html

# THE A – Z OF RCOLORBREWER PALETTE. https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/#show-only-colorblind-friendly-brewer-palettes

# Title manipulation with R and ggplot2. https://r-graph-gallery.com/289-control-ggplot2-title.html