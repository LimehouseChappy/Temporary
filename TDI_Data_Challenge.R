# PROGRAM: TDI_Test.R
# AUTHOR: Jamie Daubenspeck

# load in packages
library(tidyverse)
library(sp)
library(geosphere)

rm(list = ls())
# Read in Data
path <- "C:/Users/jamie/OneDrive/Documents/Data Science/DataIncubator/Street_Trees.csv"

df <- read_csv(path)

##### Question 1 - How many rows in data set? #####
print(paste("Number of rows: ", nrow(df)))

##### Question 2 - What Fraction of Trees are Oak or Elm?
# Ignore other, unknown, various, and stump trees
colnames(df) <- tolower(colnames(df))
df <- mutate(df, spp_com = tolower(spp_com))

# flag trees to exclude from analysis
df <- mutate(df, spp_com = if_else(is.na(spp_com), "not found", spp_com))
drop_tree_strings <- c("other, unknown", "various", "stump", " ", "repl to be determined", "not found")
df <- mutate(df, type_unknown_flag = if_else(spp_com %in% drop_tree_strings, 1, 0))
df_drop_trees <- filter(df, type_unknown_flag == 1)

# count fraction of trees are oak or elm 
df <- mutate(df, elm_oak_flag = if_else(grepl("elm|oak", spp_com), 1, 0))
df_oak_elm <- filter(df, type_unknown_flag == 0) %>% group_by(elm_oak_flag) %>% summarize(n = n())
df_oak_elm <- mutate(df_oak_elm, pct = n/sum(df_oak_elm$n))

# calculate tree counts to verify number makes sense
df_tree_counts <- filter(df, type_unknown_flag == 0) %>% group_by(elm_oak_flag, spp_com) %>% summarize(n = n())


##### Question 3 - Standard Deviation of Average Growth Space Size #####
# calculate average of range, or minimum if only one entry
df3 <- df %>%
  mutate(
    min_gssize = as.numeric(str_extract(gssize, "^[0-9]+")),    # Extract the first number
    max_gssize = as.numeric(str_extract(gssize, "(?<=-)[0-9]+"))  # Extract the second number after the dash
  )

# ignore trees with non-numerical gssize
df3 <- filter(df3, !is.na(min_gssize))

# calculate mid point of range
df3 <- mutate(df3, mid_gssize = (min_gssize + max_gssize)/2)
df3 <- mutate(df3, mid_gssize = if_else(is.na(max_gssize) & !is.na(min_gssize), min_gssize, mid_gssize))

gssize_check <- group_by(df3, gssize, min_gssize, max_gssize, mid_gssize) %>% summarize(n = n())

# flag species with under 50 speciments
species_count <- group_by(df3, spp_com) %>% summarize(n = n())
species_count <- mutate(species_count, species_under_50_flag = if_else(n < 50, 1, 0))

df3 <- left_join(df3, select(species_count, spp_com, species_under_50_flag), by = c("spp_com"))

# calculate average of mid_gssize for each species
df_tree_size <- filter(df3, species_under_50_flag == 0) %>% group_by(spp_com) %>% summarize(n = n(), avg_gssize = mean(mid_gssize))

print(paste("Standard deviation: ", sd(df_tree_size$avg_gssize)))

###### Question 4 - Median Distance Between Norway Maple and Closest Red maple ######
# convert lat and long to radians

geo_dist <- function(p1, p2) {
  # given two points in the format (lat,long) in degrees, 
  # calculates flat-surface distance between two points in km
  rx1 <- p1[1]*pi/180
  rx2 <- p2[1]*pi/180
  ry1 <- p1[2]*pi/180
  ry2 <- p2[2]*pi/180
  
  R = 6371 # kilometers
  
  Dx = rx2 - rx1
  Dy = ry2 - ry1
  avgX = (rx1 + rx2)/2
  avgY = (ry1 + ry2)/2
  
  dist <- R*sqrt(Dy^2 + (cos(avgY)*Dx)^2)
  
  return(dist)
  
}

min_geo_dist <- function(point, vec_points) {
  # given a point and a vector of points, return the minimum distance
  distances <- apply(vec_points, MARGIN = 1, FUN = geo_dist, p2 = point)
 
  return(min(distances))
}

# create vector of points
norway <- filter(df, spp_com == "norway maple") %>% select(x, y)
red <- filter(df, spp_com == "red maple") %>% select(x, y)


# test distance formula
km_to_feet = 1/1.60934*5280

norway_1 <- unlist(norway[1,])
names(norway_1) <- NULL

red_1 <- unlist(red[1,])
names(red_1) <- NULL

dist_flat_test <- geo_dist(norway_1, red_1)*km_to_feet
dist_flat_package <- distCosine(norway_1, red_1, r=6371)*km_to_feet
dist_hav_test <- distHaversine(norway_1, red_1)/1000*km_to_feet

min_dist_test <- min_geo_dist(norway_1, red)*km_to_feet

# calculate minimum distance for each observation

# Unbracket to calculate distances
calc_distance <- TRUE
if(calc_distance) {
  norway$dist_to_red_maple_feet <- apply(norway, 1, min_geo_dist, vec_points = red)*km_to_feet
  
  print(paste("Median Distance: ", median(norway$dist_to_red_maple_feet)))
  } else {
  print("Median distance not calculated") 
}

##### Question 5 - Fraction of Last Inspections Occur on 2nd Most Frequent Day of Week #####

# assign weekdays
df <- mutate(df, last_date = as.Date(str_extract(inspect_dt, "\\d{4}/\\d{2}/\\d{2}")))
df <- mutate(df, weekday = weekdays(last_date))

weekday_count <- group_by(df, weekday) %>% summarize(n = n())
weekday_count <- mutate(weekday_count, frac = n/sum(weekday_count$n))


###### Question 6 - Trend Between Tree Diameter and Tree Height ######
path2 <- "C:/Users/jamie/OneDrive/Documents/Data Science/DataIncubator/TS3_Raw_tree_data.csv"

df_h <- read_csv(path2)
colnames(df_h) <- tolower(colnames(df_h))

df_h <- mutate(df_h, height_inches = `treeht (m)`*39.3701)

# calculate average height for each species
df_h <- mutate(df_h, scientificname = tolower(scientificname))
df_h <- mutate(df_h, sci_name_clean = str_extract(scientificname, "^[^\\s]+\\s+[^\\s]+"))

tree_heights <- group_by(df_h, sci_name_clean) %>% summarize(num_heights = n(), avg_height = mean(height_inches))


# calculate average diameters for each species
df <- mutate(df, spp_bot = tolower(spp_bot))
df <- mutate(df, sci_name_clean = str_extract(spp_bot, "^[^\\s]+\\s+[^\\s]+"))

tree_diam <- group_by(df, sci_name_clean) %>% summarize(num_diam = n(), avg_diam = mean(diameter))

# combine
tree_df <- left_join(tree_heights, tree_diam, by = c("sci_name_clean"))
tree_df_clean <- filter(tree_df, !is.na(avg_diam))

# run regression
lm_tree <- lm(avg_height ~ avg_diam, data = tree_df_clean)
print(summary(lm_tree))
