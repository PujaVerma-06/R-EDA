install.packages('pacman')

library('sqldf')
library('pacman')
library('reshape2')
library('ggplot2')
library('dplyr')
library('ggmap')

## To collect data from API:

pacman::p_load(httr, jsonlite, dplyr)

restro_df <- NULL
city_id <- as.list(c(1:50))
for (i in 1:length(city_id))
{
  url <- paste0("https://developers.zomato.com/api/v2.1/location_details?apikey=bb2eaa9d306c89d2441e7b17f98d01c7&entity_id=",city_id[i],"&entity_type=city")
  print(url)
  restro_result <- httr::GET(url)
  content <- httr::content(restro_result, as = 'text')
  content_from_json <- jsonlite::fromJSON(content)
  tryCatch({restro_details_rest <- content_from_json$best_rated_restaurant$restaurant %>% select(id, name, cuisines, average_cost_for_two, all_reviews_count,establishment)
  restro_details_loc <- content_from_json$best_rated_restaurant$restaurant$location %>% select(locality,city, latitude, longitude)
  restro_details_user <- content_from_json$best_rated_restaurant$restaurant$user_rating %>% select(aggregate_rating,rating_text, votes)},
  error=function(e){})
  restro_details <- cbind(restro_details_rest,restro_details_loc,restro_details_user)
  restro_df <- rbind(restro_df,restro_details)
  restro_details <- NULL
}

View(restro_df)

str(restro_df)
unique(restro_df$city)

# To see no. of best rated restro per city:
restro_per_city <- restro_df %>% select(city) %>% group_by(city) %>% summarise(No._of_best_rated_restro =n()) 
View(restro_per_city)


# Get details of the highest rated restro:
best_rating <- max(restro_df$aggregate_rating)
top_rated_restro <- restro_df %>% select(name,cuisines,city,aggregate_rating) %>% 
  filter(top_rated_restro == best_rating)
View(top_rated_restro)


# To see costly restro:
top_20_costly_restro <- restro_df[order(-restro_df$average_cost_for_two),][1:20,] %>% 
  select(name,city,locality,average_cost_for_two,cuisines,aggregate_rating)
View(top_20_costly_restro)


#To see avg rating and avg cost for two people for every city:
restro_df$aggregate_rating <- as.numeric(restro_df$aggregate_rating)

restro_city <- restro_df %>% select(city,aggregate_rating,average_cost_for_two) %>% 
  group_by(city) %>%
  summarise(Avg_Rating = mean(aggregate_rating), Avg_cost = mean(average_cost_for_two)) 

qplot(data = restro_city, Avg_cost, Avg_Rating, colour = city) + labs(x="Average Cost for 2", y = "Average Rating")
