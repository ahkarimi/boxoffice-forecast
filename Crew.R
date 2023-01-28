


GetCrew <- function(movie_links){
  
  result <- list()
  id <- 1
  for (link in movie_links){
    temp_results <- list()
    print(link)
    
    my_resp <- GET(url = link)
    
    page_html <- read_html(my_resp)
    
    crew <- NA
    actors <- NA
    director <- NA
    writer <- NA
    producer <- NA
    
    crew <- page_html %>%
      html_nodes("table") %>%
      html_table()
    
    tryCatch({
      actors <- crew[[2]][["Actor"]]
    },
    error = function(e)
      return(NULL))
    
    tryCatch({
      len <- length(crew[[1]][[1]])
      for (i in 1:len){
        if (crew[[1]][[2]][i] == "Director") {
          if (is.na(director)){
            director <- crew[[1]][[1]][i]
          } else {
            director <- c(director, crew[[1]][[1]][i])
          }
        } else if (crew[[1]][[2]][i] == "Writer") {
          if (is.na(writer)){
            writer <- crew[[1]][[1]][i]
          } else {
            writer <- c(writer, crew[[1]][[1]][i])
          }
        } else if (crew[[1]][[2]][i] == "Producer") {
          if (is.na(producer)){
            producer <- crew[[1]][[1]][i]
          } else {
            producer <- c(producer, crew[[1]][[1]][i])
          }
        }
      }
      #sapply(crew[[1]], function(x) sapply(x, function(y) y[2]) )
    },
    error = function(e)
      return(NULL))
    
    temp_results$actor <- actors
    temp_results$director <- director
    temp_results$writer <- writer
    temp_results$producer <- producer
    
    result[[id]] <- temp_results
    
    id = id + 1
  }
  
  return(result)
}


# Get top actors and actresses names from Ranker website
GetTopActorsList <- function() {
  json_file_actresses <-
    "https://cache-api.ranker.com/lists/841563/items?limit=100&offset=0&include=votes,wikiText,rankings,openListItemContributors&propertyFetchType=ALL&liCacheKey=null"
  data1 <-
    jsonlite::fromJSON(json_file_actresses, simplifyDataFrame = TRUE)
  top_actresses <- data1$listItems$node$name
  
  json_file_actors <-
    "https://cache-api.ranker.com/lists/679173/items?limit=100&offset=0&include=votes,wikiText,rankings,openListItemContributors&propertyFetchType=ALL&liCacheKey=null"
  data2 <-
    jsonlite::fromJSON(json_file_actors, simplifyDataFrame = TRUE)
  top_actors <- data2$listItems$node$name
  
  top_actors <- c(top_actors, top_actresses)
  
  return(top_actors)
  
}


GetNumActors <- function(crew){
  
  top_actors_list <- GetTopActorsList()
  
  actors_number <- NULL
  
  for (i in 1:length(crew)){
    print(i)
    actors <- NA
    count <- 0
    
    tryCatch({
      actors <- crew[[i]]$actor
    },
    error = function(e)
      return(NULL))
    
    # Count how many movies actors is in top list
    if (length(actors) > 0 && !is.na(actors)){
      for (actor in actors) {
        if (is.element(actor, top_actors_list)) {
          count <- count + 1
        }
      }
    }
    
    actors_number <- c(actors_number, count)
  }
  
  return(actors_number)
  
}






all_id <- paste0("https://www.boxofficemojo.com/title", all_movies_table$imdbID, "credits/")
all_crews <- GetCrew(all_id)

top_actors <- GetNumActors(all_crews)

all_movies_table$NumberOfTopActors <- NA
all_movies_table$NumberOfTopActors <- top_actors


#Director
#############################
all_gross <- all_movies_table[,c(1,3,11)]
all_gross$International <- as.character(all_gross$International)

str(all_gross)

all_crews2 <- result

actor_gross_table = list()
director_gross_table = list()
writer_gross_table = list()
producer_gross_table = list() 

for (i in 1:length(all_crews)){
  print(i)
  for(a in all_crews[[i]]$actor){
    if(!is.na(a)){
      gross_len <- length(actor_gross_table[[a]]$gross)
      inter_len <- length(actor_gross_table[[a]]$international)
      
      actor_gross_table[[a]]$gross[gross_len + 1] <- all_gross$Gross[i]
      actor_gross_table[[a]]$international[inter_len + 1] <- all_gross$International[i]
    }
  }
  
  for(d in all_crews[[i]]$director){
    if(!is.na(d)){
      gross_len <- length(director_gross_table[[d]]$gross)
      inter_len <- length(director_gross_table[[d]]$international)
      
      director_gross_table[[d]]$gross[gross_len + 1] <- all_gross$Gross[i]
      director_gross_table[[d]]$international[inter_len + 1] <- all_gross$International[i] 
    }
  }
  
  for(w in all_crews[[i]]$writer){
    if (!is.na(w)) {
      gross_len <- length(writer_gross_table[[w]]$gross)
      inter_len <- length(writer_gross_table[[w]]$international)
      
      writer_gross_table[[w]]$gross[gross_len + 1] <- all_gross$Gross[i]
      writer_gross_table[[w]]$international[inter_len + 1] <- all_gross$International[i]
    }
  }
  
  for(p in all_crews[[i]]$producer){
    if (!is.na(p)) {
      gross_len <- length(producer_gross_table[[p]]$gross)
      inter_len <- length(producer_gross_table[[p]]$international)
      
      producer_gross_table[[p]]$gross[gross_len + 1] <- all_gross$Gross[i]
      producer_gross_table[[p]]$international[inter_len + 1] <- all_gross$International[i]
    } 
  }
  
}

g <- NA
for (ind in 1:length(all_crews)){
  for(x in all_crews[[ind]]$actor){
    if (!is.na(x)) {
      if(x == "Bruce Boxleitner"){
        print(ind)
        print(all_gross$MoiveTitle[ind])
        g <- c(g,all_gross$Gross[ind] )
        print(all_gross$Gross[ind])
        print(all_gross$International[ind])
      }
    } 
  }
}
g <- gsub("\\$", "", g)
g <- gsub(",", "", g)
g <- as.numeric(g)
mean(g, na.rm = TRUE)


(50091069 + 44121967 + 28084361 + 170669481) / 4

for (i in 1:length(actor_gross_table)){
  actor_gross_table[[i]]$gross <- gsub("\\$", "",actor_gross_table[[i]]$gross)
  actor_gross_table[[i]]$gross <- gsub(",", "",actor_gross_table[[i]]$gross)
  actor_gross_table[[i]]$gross <- as.numeric(actor_gross_table[[i]]$gross)
  
  actor_gross_table[[i]]$international <- gsub("\\$", "",actor_gross_table[[i]]$international)
  actor_gross_table[[i]]$international <- gsub(",", "",actor_gross_table[[i]]$international)
  actor_gross_table[[i]]$international <- as.numeric(actor_gross_table[[i]]$international)
}

for (i in 1:length(director_gross_table)){
  director_gross_table[[i]]$gross <- gsub("\\$", "",director_gross_table[[i]]$gross)
  director_gross_table[[i]]$gross <- gsub(",", "",director_gross_table[[i]]$gross)
  director_gross_table[[i]]$gross <- as.numeric(director_gross_table[[i]]$gross)
  
  director_gross_table[[i]]$international <- gsub("\\$", "",director_gross_table[[i]]$international)
  director_gross_table[[i]]$international <- gsub(",", "",director_gross_table[[i]]$international)
  director_gross_table[[i]]$international <- as.numeric(director_gross_table[[i]]$international)
}

for (i in 1:length(writer_gross_table)){
  writer_gross_table[[i]]$gross <- gsub("\\$", "",writer_gross_table[[i]]$gross)
  writer_gross_table[[i]]$gross <- gsub(",", "",writer_gross_table[[i]]$gross)
  writer_gross_table[[i]]$gross <- as.numeric(writer_gross_table[[i]]$gross)
  
  writer_gross_table[[i]]$international <- gsub("\\$", "",writer_gross_table[[i]]$international)
  writer_gross_table[[i]]$international <- gsub(",", "",writer_gross_table[[i]]$international)
  writer_gross_table[[i]]$international <- as.numeric(writer_gross_table[[i]]$international)
}

for (i in 1:length(producer_gross_table)){
  producer_gross_table[[i]]$gross <- gsub("\\$", "",producer_gross_table[[i]]$gross)
  producer_gross_table[[i]]$gross <- gsub(",", "",producer_gross_table[[i]]$gross)
  producer_gross_table[[i]]$gross <- as.numeric(producer_gross_table[[i]]$gross)
  
  producer_gross_table[[i]]$international <- gsub("\\$", "",producer_gross_table[[i]]$international)
  producer_gross_table[[i]]$international <- gsub(",", "",producer_gross_table[[i]]$international)
  producer_gross_table[[i]]$international <- as.numeric(producer_gross_table[[i]]$international)
}


crew_table <- all_movies_table[,c(1,9,15)]
crew_table$MeanActors <- NA
crew_table$MaxActors <- NA
crew_table$MedianActors <- NA
crew_table$MeanActorsInt <- NA
crew_table$MaxActorsInt <- NA
crew_table$MedianActorsInt <- NA

crew_table$MeanDirector <- NA
crew_table$MeanDirectorInt <- NA

crew_table$MeanWriter <- NA
crew_table$MaxWriter <- NA
crew_table$MeanWriterInt <- NA
crew_table$MaxWriterInt <- NA

crew_table$MeanProducer <- NA
crew_table$MaxProducer <- NA
crew_table$MeanProducerInt <- NA
crew_table$MaxProducerInt <- NA


for (i in 1:length(crew_table$MoiveTitle)) {
  print(i)
  
  meanActors <- NA
  #medianActors <- NA
  #maxActors <- NA
  
  meanActorsInt <- NA
  #medianActorsInt <- NA
  #maxActorsInt <- NA
  
  for(a in all_crews[[i]]$actor){
    if(!is.na(a)){
      meanActors <- c(meanActors, mean(actor_gross_table[[a]]$gross, na.rm = TRUE))
      #medianActors <- c(medianActors, median(actor_gross_table[[a]]$gross, na.rm = TRUE))
      #maxActors <- c(maxActors, max(actor_gross_table[[a]]$gross, na.rm = TRUE))
      
      meanActorsInt <- c(meanActorsInt, mean(actor_gross_table[[a]]$international, na.rm = TRUE))
      #medianActorsInt <- c(medianActorsInt, median(actor_gross_table[[a]]$international, na.rm = TRUE))
      #maxActorsInt <- c(maxActorsInt, max(actor_gross_table[[a]]$international, na.rm = TRUE))
    }
  }
  crew_table$MeanActors[i] <- mean(meanActors, na.rm = TRUE)
  crew_table$MedianActors[i] <- median(meanActors, na.rm = TRUE)
  crew_table$MaxActors[i] <- max(meanActors, na.rm = TRUE)
  
  crew_table$MeanActorsInt[i] <- mean(meanActorsInt, na.rm = TRUE)
  crew_table$MedianActorsInt[i] <- median(meanActorsInt, na.rm = TRUE)
  crew_table$MaxActorsInt[i] <- max(meanActorsInt, na.rm = TRUE)
  
  
  meanDirector <- NA
  meanDirectorInt <- NA
  
  for(d in all_crews[[i]]$director){
    if(!is.na(d)){
      meanDirector <- mean(director_gross_table[[d]]$gross, na.rm = TRUE)
      meanDirectorInt <- mean(director_gross_table[[d]]$international, na.rm = TRUE)
    }
  }
  crew_table$MeanDirector[i] <- mean(meanDirector, na.rm = TRUE)
  crew_table$MeanDirectorInt[i] <- mean(meanDirectorInt, na.rm = TRUE)
  
  
  meanWriter <- NA
  #maxWriter <- NA
  meanWriterInt <- NA
  #maxWriterInt <- NA
  
  for(w in all_crews[[i]]$writer){
    if(!is.na(w)){
      meanWriter <- c(meanWriter, mean(writer_gross_table[[w]]$gross, na.rm = TRUE))
      #maxWriter <- c(maxWriter, max(writer_gross_table[[w]]$gross, na.rm = TRUE))
      meanWriterInt <- c(meanWriterInt, mean(writer_gross_table[[w]]$international, na.rm = TRUE))
      #maxWriterInt <- c(maxWriterInt, max(writer_gross_table[[w]]$international, na.rm = TRUE))
    }
  }
  crew_table$MeanWriter[i] <- mean(meanWriter, na.rm = TRUE)
  crew_table$MaxWriter[i] <- max(meanWriter, na.rm = TRUE)
  crew_table$MeanWriterInt[i] <- mean(meanWriterInt, na.rm = TRUE)
  crew_table$MaxWriterInt[i] <- max(meanWriterInt, na.rm = TRUE)
  
  
  meanProducer <- NA
  #maxProducer <- NA
  meanProducerInt <- NA
  #maxProducerInt <- NA
  
  for(p in all_crews[[i]]$producer){
    if(!is.na(p)){
      meanProducer <- c(meanProducer, mean(producer_gross_table[[p]]$gross, na.rm = TRUE))
      #maxProducer <- c(maxProducer, max(producer_gross_table[[p]]$gross, na.rm = TRUE))
      meanProducerInt <-  c(meanProducerInt, mean(producer_gross_table[[p]]$international, na.rm = TRUE))
      #maxProducerInt <- c(maxProducerInt, max(producer_gross_table[[p]]$international, na.rm = TRUE))
    }
  }
  crew_table$MeanProducer[i] <- mean(meanProducer, na.rm = TRUE)
  crew_table$MaxProducer[i] <- max(meanProducer, na.rm = TRUE)
  crew_table$MeanProducerInt[i] <-  mean(meanProducerInt, na.rm = TRUE)
  crew_table$MaxProducerInt[i] <- max(meanProducerInt, na.rm = TRUE)
  
}

#all_movies_table <- cbind(all_movies_table, crew_table[,c(4:19)])


write.table(all_movies_table_t , file = "all_movies_table_actors_gross.csv",
            col.names = TRUE, row.names=FALSE )
