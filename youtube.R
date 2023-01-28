library(httr)
library(jsonlite)

api_key = "AIzaSyAYurm6BXPlkA6muf_sd3pjcjWNhBbolLo"

"https://www.googleapis.com/youtube/v3/videos?part=statistics&id=fdixQDPA2h0&key=[YOUR_API_KEY] HTTP/1.1"
"https://www.googleapis.com/youtube/v3/search?part=snippet&q=moon&key=[YOUR_API_KEY]"


youtube_url <- "https://www.googleapis.com/youtube/v3/search"

movie_title <- "End game"

query_params <- list(part = "snippet", q = movie_title, type = "video", order="relevance", key = api_key)

search_resp <- GET(url = youtube_url, query = query_params)

search_resp <- content(search_resp, as="text")

searched_videos <- jsonlite::fromJSON(search_resp, simplifyDataFrame = TRUE)

searched_videos$items$id[[2]][1]

############################# Get video

youtube_url <- "https://www.googleapis.com/youtube/v3/videos"

movie_id <- searched_videos$items$id[[2]][1]

query_params <- list(part = "statistics", id = movie_id, key = api_key)

video_resp <- GET(url = youtube_url, query = query_params)

video_resp <- content(video_resp, as="text")

videos <- jsonlite::fromJSON(video_resp, simplifyDataFrame = TRUE)

videos$items$statistics

