# We will use the httr package to build the query
library(httr)
library(rvest)
require(svMisc)



# Scrape yearly boxoffice table from TheNumbers website
#getYearlyBoxOffice <- function(theYear) {
  if (theYear <= year(now()) && theYear >= 2002) {
    tryCatch({
      
      base_url = paste0("https://www.the-numbers.com/United-States/movies/year/", theYear)
      
      my_resp = GET(url = base_url)
      
      page_html = read_html(my_resp)
      
      # Extract first table from Html file remove columns of useless data
      yearly_boxoffice = page_html %>%
        html_nodes("table") %>%
        extract2(1) %>%
        html_table()
      
      # Remove columns 
      yearly_boxoffice$`Co-ProducingCountries` <- NULL
      yearly_boxoffice$Trailer <- NULL
      
      names(yearly_boxoffice) <- c("MoiveTitle", "Genre", "Budget", "Worldwide")
      
      return(yearly_boxoffice)
      
    },
    error = function(e)
      return(NULL))
  }
  else {
    return(NULL)
  }
}


# Scrape yearly boxoffice table from Boxoffice Mojo website
GetYearlyBoxOffice <- function(theYear){

  if (theYear <= year(now()) && theYear >= 2002) {
    tryCatch({
      
      # Make a connection and get a response
      base_url <- paste0("https://www.boxofficemojo.com/year/", theYear, "/")
      
      query_params <- list(grossesOption = "totalGrosses")
      
      my_resp <- GET(url = base_url, query = query_params)
      
      page_html <- read_html(my_resp)
      
      # Extract the boxoffice table from Html file
      # and convert it to a table data type
      yearly_boxOffice <- page_html %>%
        html_nodes("table") %>%
        extract2(1) %>%
        html_table()
      
      yearly_boxOffice$Rank <- NULL
      yearly_boxOffice$Genre <- NULL
      yearly_boxOffice$Budget <- NULL
      yearly_boxOffice$`Running Time` <- NULL
      yearly_boxOffice$`% of Total` <- NULL
      yearly_boxOffice$Estimated <- NULL
      
      names(yearly_boxOffice) <- c("MoiveTitle", 
                                   "Distributor", 
                                   "Gross", 
                                   "MaxTheathers", 
                                   "Opening", 
                                   "OpeningTheathers", 
                                   "OpenDate", 
                                   "CloseDate")
      
      yearly_boxOffice = cbind(yearly_boxOffice, year = theYear)
      
      return(yearly_boxOffice)
    },
    error = function(e)
      return(NULL))
  }
  else {
    return(NULL)
  }
}


GetAllYearsMovies <- function(){
  
  # An empty table with 8 columns that will contain all movies from 2008 to 2019
  all_movies <- data.frame(matrix(ncol = 8, nrow = 0))
  
  for (theYear in 2002:2019) {
    
    movie_table <- GetYearlyBoxOffice(theYear)
    
    all_movies <- rbind(all_movies, movie_table)
  }
  
  return(all_movies)
}


# Scrape Movie profile links
GetMovieLinks <- function(theYear){
  
  tryCatch({
    
    # Make a connection and get a response
    base_url <- paste0("https://www.boxofficemojo.com/year/", theYear, "/")
    
    query_params <- list(grossesOption = "totalGrosses")
    
    my_resp <- GET(url = base_url, query = query_params)
    
    page_html <- read_html(my_resp)
    
    links = page_html %>%
      html_nodes("table") %>%
      extract2(1) %>%
      html_nodes("a") %>%
      html_attr("href")
    
    links <- links[which(str_detect(links, "/release/"))]
    links <- paste0("https://www.boxofficemojo.com", links)
    
    return(links)
    
  },
  error = function(e)
    return(NULL))


}


GetAllMovieLinks <- function(){
  
  all_links <- NULL
  
  for (y in 2002:2019) {
    l <- GetMovieLinks(y)
    
    if (!is.null(all_links)){
      all_links <- c(all_links, l)
    } else{
      all_links <- l
    }
  }
  
  return(all_links)
}


GetMovieProfile <- function(movie_links){
  
  result <- data.frame(matrix(ncol = 5, nrow = 0))
  
  for (link in movie_links){
    print(link)
    my_resp <- GET(url = link)
    
    page_html <- read_html(my_resp)
    
    imdb_id <- NA
    runtime <- NA
    genres <- NA
    weekend <- NA
    gross <- NA
    info <- NA
    
    tryCatch({
      
      imdb_id <- page_html %>%
        html_nodes("[class='a-link-normal']") %>%
        html_attr("href")

      for (l in imdb_id){
        if (str_detect(l, regex("/tt[0-9]{7}/"))) {
          imdb_id <- str_extract(l, regex("/tt[0-9]{7}/"))
          break()
        }
      }
      
      
      gross <- page_html %>%
        html_nodes("[class='a-size-medium a-text-bold']") %>%
        html_text() %>% 
        # Trim additional white space
        str_trim()
      
      info <- page_html %>%
        html_nodes("[class='a-section a-spacing-none']") %>%
        html_nodes("span") %>%
        html_text() %>% 
        str_trim()
      
      for (i in 1:length(info)) {
        if (info[i] == "Runtime") {
          
          runtime <- info[i+1]
          
        } else if (info[i] == "Genres") {
          
          genres <- info[i+1]
          genres <- gsub(" ", "", genres)
          genres <- str_split(genres, "\n\n")
          
        } else if (info[i] == "Opening Weekend") {
          
          # `Opening weekend` is in double `span` tag
          weekend <- page_html %>%
            html_nodes("[class='a-section a-spacing-none']") %>%
            html_nodes("span") %>%
            html_nodes("span") %>%
            html_text() %>% 
            str_trim()
          
          # Assign the last element of list to list
          weekend <- tail(weekend, n=1)
          
        } 
      }
      
    result <- rbind(result, data.frame(International = gross[2], 
                                       Runtime = runtime, 
                                       Genres = genres[[1]][1], 
                                       OpeningWeekend = weekend,
                                       imdbID = imdb_id))  

    },
    error = function(e)
      return(NULL))
  }
  
  return(result)
 
}


# Scrape the all american movies that have wikipedia page
GetWikipediaMovieLinks <- function(theYear) {
  if (theYear <= year(now()) && theYear >= 2002) {
    tryCatch({
      wikiURL <-
        paste("https://en.wikipedia.org/wiki/List_of_American_films_of_",
              theYear,
              sep = "")
      
      wikiResp = GET(url = wikiURL)
      
      wikiHtml = read_html(wikiResp)
      
      wikiMovieLinks = wikiHtml %>%
        html_nodes(".wikitable") %>%
        html_nodes("i") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        data.frame()
      
      wikiMovieNames = wikiHtml %>%
        html_nodes(".wikitable") %>%
        html_nodes("i") %>%
        html_nodes("a") %>%
        html_text()
      
      # Remove `/wiki/` from all links to get movie names
      # And add main link to all of links to make Urls
      for (movie in wikiMovieLinks) {
        newboxOffice <- str_replace(movie, "/wiki/", "")
        wikiMoviesUrls <-
          paste("https://en.wikipedia.org", movie, sep = "")
      }
      
      # Make new tabel
      wikiMovieLinks[1] <- newboxOffice
      names(wikiMovieLinks)[1] <- "ID"
      wikiMovieLinks[2] <- wikiMovieNames
      names(wikiMovieLinks)[2] <- "title"
      wikiMovieLinks[3] <- wikiMoviesUrls
      names(wikiMovieLinks)[3] <- "wikiLinks"
      wikiMovieLinks[4] <- theYear
      names(wikiMovieLinks)[4] <- "wikiYear"
      
      return(wikiMovieLinks)
    },
    error = function(e)
      return(NULL))
  }
  else {
    return(NULL)
  }
  
}


GetAllWikipediaLinks <- function(){
  
  all_links <- data.frame(matrix(ncol = 4, nrow = 0))
  
  for (y in 2002:2019){
    l <- GetWikipediaMovieLinks(y)
    if (!is.null(all_links)){
      all_links <- rbind(all_links, l)
    }else{
      all_links <- l
    }
  }
  return(all_links)
  
}


# Formatting realese dates
DateFormatting <- function(Open.Date) {
  # date format month/day
  
  openDates <- as.Date(Open.Date, format = "%Y %m %d")
  
  monthOfRelease <- month(openDates)
  dayOfRelease <- wday(openDates)
  seasonOfRelease <- time2season(openDates, out.fmt = "seasons")
  
  allDates <- tibble(
    ReleaseDate = openDates,
    MonthOfRelease = monthOfRelease,
    DayOfRelease = dayOfRelease,
    SeasonOfRelease = seasonOfRelease
  )
  
  return(allDates)
}


# Get number of wikipedia movie page views
GetWikiViews <- function(movieID, startDate, endDate) {
  tryCatch({
    #startDate <- "2017050100"
    #endDate <- "2018043000"
    
    apiURL <-
      paste0(
        "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/user/",
        movieID,
        "/daily/",
        startDate,
        "/",
        endDate
      )
    
    data <- jsonlite::fromJSON(apiURL, simplifyDataFrame = TRUE)
    wiki_views <- data$items$views
    
    return(sum(wiki_views))
  },
  error = function(e)
    return(NULL))
}


GetWikiViewsTable <- function(movie.dates, wiki.id, number) {
  #wiki.dates <- paste(theYear, movie.dates, sep = "/")
  if (number > 0) {
    start.date <- as.Date(movie.dates, format = "%Y-%m-%d")
    end.date <- start.date + number
    
    end.date <- gsub("-", "", end.date)
    start.date <- gsub("-", "", start.date)
    
    end.date <- paste0(end.date, "00")
    start.date <- paste0(start.date, "00")
    
  } else {
    # `number` is negative
    end.date <- as.Date(movie.dates, format = "%Y-%m-%d")
    start.date <- end.date + number
    
    end.date <- gsub("-", "", end.date)
    start.date <- gsub("-", "", start.date)
    
    end.date <- paste0(end.date, "00")
    start.date <- paste0(start.date, "00")
    
  }

  wiki.views <- NULL
  for (i in 1:length(wiki.id)) {
    print(i)
    id <- wiki.id[i]
    if (!is.na(id)) {
      myview <- GetWikiViews(id, start.date[i], end.date[i])
      wiki.views[[i]] <- if (!is.null(myview))
        myview
      else
        NA
    } else {
      wiki.views[[i]] <- NA
    }
    
  }
  
  return(wiki.views)
}













all_movies <- GetAllYearsMovies()

all_links <- GetAllMovieLinks() 

#all_movies_table <- cbind(all_movies, mojoLinks = all_links)

movie_profiles <- GetMovieProfile(all_links)

all_wiki <- GetAllWikipediaLinks()

dates_temp <- paste(all_movies_table$year, all_movies_table$OpenDate, sep = " ") 
dates_temp <- DateFormatting(dates_temp)
all_movies_table <- cbind(all_movies_table, dates_temp)


names(all_movies_table)
length(all_movies_table$reviewNumbers[which(all_movies_table$reviewNumbers > 23)] )
length(which(complete.cases(ttt)))
ttt <- all_movies_table[,c(2:7, 11:16, 19:22, 27:44)]


# Trends
##############################
trends_table <- data.frame(matrix(ncol = 5, nrow = 0))
for (i in 1:13){
  ttt <- read.csv(file = paste0("C:/Users/Amir/Documents/R/New Boxoffice/trends/trend_result", i, ".csv"))
  trends_table <- rbind(trends_table, ttt)
}
trends_table$X <- NULL


length(all_movies_table$MoiveTitle[1538:11444])

all_movies_table$TrendWeek[1538:11444] <- trends_table$TrendWeek
all_movies_table$Trend2Week[1538:11444] <- trends_table$Trend2Week
all_movies_table$TrendMonth[1538:11444] <- trends_table$TrendMonth

length(which(!complete.cases(trends_table$TrendWeek)))


# wikipedia views
######################################################################
names(all_movies_table)
wiki_temp <- all_movies_table[7657:11444,c(1,16,18)]

wiki_views_31 <- GetWikiViewsTable(wiki_temp$ReleaseDate, wiki_temp$ID, -31)
wiki_views_14 <- GetWikiViewsTable(wiki_temp$ReleaseDate, wiki_temp$ID, -14)
wiki_views_7 <- GetWikiViewsTable(wiki_temp$ReleaseDate, wiki_temp$ID, -7)
wiki_views_after_7 <- GetWikiViewsTable(wiki_temp$ReleaseDate, wiki_temp$ID, +7)
wiki_views_after_14 <- GetWikiViewsTable(wiki_temp$ReleaseDate, wiki_temp$ID, +14)

length(wiki_views_31[which(complete.cases(wiki_views_31))])

length(wiki_movies)
all_movies_table$WV_31[7657:11444] <- wiki_views_31
all_movies_table$WV_14 <- NA
all_movies_table$WV_7 <- NA
all_movies_table$WV7 <- NA
all_movies_table$WV14 <- NA


temp_movies <- cbind(wiki_movies, Wikiviews = wiki_views)
#all_movies_table <- cbind(all_movies_table, movie_profiles)
#str_extract("https://pro.imdb.com/title/tt0259446/cast?ref_=mojo_rl_cta_cast&rf=mojo_rl_cta_cast", regex("/tt[0-9]{7}/"))




# Imdb id

#######################
names(all_movies_table)
tempId <- all_movies_table[,c(10,15)]
index_na2 <- which(!complete.cases(all_movies_table$imdbID))
length(index_na2)
length(which(!complete.cases(tempId$imdbID)))
index_na[1]
all_movies_table$imdbID[102]
all_movies_table$MoiveTitle[102]
all_movies_table$mojoLinks[102]
all_movies_table$movieRate[102]
#+++++++++++++++++++++++++++++++ index_na reviews is NA



for (i in index_f){
  if(is.na(tempId$imdbID[i])){
    print(i)
    my_resp <- GET(url = tempId$mojoLinks[i])
    
    page_html <- read_html(my_resp)
    
    imdb_id <- page_html %>%
      html_nodes("[class='a-link-normal']") %>%
      html_attr("href")
    
    for (l in imdb_id){
      if (str_detect(l, regex("/tt[0-9]+/"))) {
        imdb_id <- str_extract(l, regex("/tt[0-9]+/"))
        break()
      }
    }
    
    tempId$imdbID[i] <- imdb_id[1]
    
  }
}

all_movies_table$imdbID <- tempId$imdbID




##############################################################
write.table(crew_table , file = "crew_table.csv",
            col.names = TRUE, row.names=FALSE )
write.csv(all_movies_table, file = "all_movies_table_trends.csv", fileEncoding = "UTF-8")

movie_table_for_python <- all_movies_table[which(all_movies_table$year > 2004),c(1,18)]
movie_table_for_python$OneMonthBeforeRelease <- movie_table_for_python$ReleaseDate - 31

#Bind Tables
###################### 

BindTables <- function() {
  
  all_movies_temp <- data.frame(matrix(ncol = 4, nrow = 0))
  
  for (movie_title in all_movies_table$MoiveTitle) {
    
    movie_title <- gsub(regex("\\(([^\\)]+)\\)"), "", movie_title)
    
    title_temp <- all_wiki[which(adist(all_wiki$title, movie_title) < 1),]
    
    all_movies_temp <- rbind(all_movies_temp, if (nrow(title_temp) > 0) title_temp[1,] else NA)
  }
  
  return(all_movies_temp)
}


all_movies_table <- cbind(all_movies_table, test[,c(1,3)])



# Date Formatting
####################
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Jan", "01")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Feb", "02")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Mar", "03")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Apr", "04")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "May", "05")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Jun", "06")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Jul", "07")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Aug", "08")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Sep", "09")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Oct", "10")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Nov", "11")
all_movies_table$OpenDate <- str_replace_all(all_movies_table$OpenDate, "Dec", "12")



length(dates_temp)


