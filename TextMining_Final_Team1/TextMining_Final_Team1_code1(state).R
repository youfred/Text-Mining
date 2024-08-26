#install packages and run the library 
install.packages("ldatuning")
install.packages("maps")
install.packages("ggplot2")
library(dplyr)
library(textstem)
library(tm)
library(stringr)
library(tm)
library(magrittr)
library(ldatuning)
library(tidytext)
library(maps)
library(ggplot2)


##read the complaint file 
complaint <- read.csv("./Data/complaints.csv")
View(complaint)

# group by the State and combine all comments by the State 
State.complaint <- complaint %>% filter(Consumer.complaint.narrative != "" & State != "None") %>%
  group_by(State) %>%
  summarise(comments = paste(Consumer.complaint.narrative, collapse = " ")) 

# Assuming State.complaint$comments is your character vector
State.complaint %<>% rename(doc_id = State, text = comments)

# make the corpus 
text_corpus <- VCorpus(DataframeSource(State.complaint))


# Function that removes the input pattern
myRemove <- content_transformer(function(x, pattern){
  return(gsub(pattern, "" ,x))})

remove_patterns <- function(text) {
  text %>% 
    str_remove_all("\\n|\\$") %>%  # Remove patterns of "\\n", "$"
    str_remove_all("#|>|\\+|=")           # Remove characters "#", ">", "+", "="
}

# Remove 'XXXXX'
# Assuming 'myRemove' is a custom function you've created for preprocessing
# Assuming text_corpus is your existing text corpus

# First, apply myRemove to remove 'X+' pattern
text_corpus <- tm_map(text_corpus, myRemove, pattern = "X+")

# Then, apply remove_patterns to further clean the text
text_corpus <- tm_map(text_corpus, content_transformer(remove_patterns))


text_corpus <- text_corpus %>%
  tm_map(removePunctuation) %>% # remove punctuation
  tm_map(removeNumbers) %>% # remove numbers
  tm_map(stripWhitespace) %>% # strip spaces
  tm_map(content_transformer(lemmatize_strings)) %>% # Lemmatize
  tm_map(content_transformer(tolower)) %>% # To lower
  tm_map(content_transformer(trimws)) %>% # Remove margin spaces
  tm_map(content_transformer(removeWords),
         stopwords("en", source = "stopwords-iso")) %>% # Remove stopwords
  tm_map(content_transformer(str_squish)) # Every space into single sapce


# Extract document identifiers and content from VCorpus objects and save them as lists
docs_list <- lapply(names(text_corpus), function(doc_id) {
  list(id = doc_id, content = text_corpus[[doc_id]][["content"]])
})


# convert the list to dataframe 
State.complaint2  <- do.call(rbind, lapply(docs_list, as.data.frame, stringsAsFactors = FALSE))

# rename the colum id and content to State and comments 
State.complaint2 %<>% rename(State = id , comments = content)

###### sentiment 
#make the all state names to dataframe 
state_names <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  full = c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", 
           "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", 
           "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", 
           "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", 
           "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", 
           "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", 
           "virginia", "washington", "west virginia", "wisconsin", "wyoming")
)

# use the State.complaint2 and use the library and use the unnest tokens and make it
State.complaint2 <- State.complaint2 %>% 
  unnest_tokens(word,comments)

# group by the State and count it 
toal_State.word <- State.complaint2 %>% group_by(State) %>% count()


# inner join the sentiments the afinn and filter none word 
State.affin <- State.complaint2 %>%  filter(word != "") %>%
  inner_join(get_sentiments("afinn"))
# show the result
State.affin %>% head()

# grup by the State and sum the value the each group 
total_State_sum <- State.affin %>% group_by(State) %>% 
  summarise(value = sum(value)) 
# show the result
total_State_sum %>% head()
# combine the state_names and total_State_sum and make the new State_map 
State_map <- total_State_sum %>% inner_join(state_names)
State_map %>% head()

# make the unique State list 
State_list <- complaint %>% select(State) %>% 
  filter(State != 'None') %>% unique %>% as.list()
# unlist the liste 
state_vector <- unlist(State_list)

# make the dataframe named df 
df <- data.frame(State = character(), count = numeric(), stringsAsFactors = FALSE)


#make the each State group all unique comments count and combine it 
for (i in 1:length(state_vector)) {
  state_name <- state_vector[i]
  
  count_result <- complaint %>% 
    filter(State == state_name) %>% 
    select(Consumer.complaint.narrative) %>%
    unique() %>%
    count() 
  
  count_value <- count_result$n
  
  df <- rbind(df, data.frame(State = state_name, count = count_value))
}

# combine the df dataframe from the total_State_sum 
total_State_sum <- total_State_sum %>% 
  inner_join(toal_State.word)
total_State_sum %>% filter()
total_State_sum
# show the result
total_State_sum %>% head()
# and make the pro column that is divide to the state total comments count and all sentiment value 
total_State_sum$pro <- (total_State_sum$value/total_State_sum$n)
# show the result
total_State_sum %>% arrange(pro)

# combine the two data frame 
State_map <- total_State_sum %>% inner_join(state_names)

# import the USA State map data 
states_map <- map_data("state")

# and combine two dataframe State_map and states map 
states_map<- states_map %>% 
  inner_join(State_map, full, by = c("region" = "full"))



# make the graph which the pro value is high hilight the color 
ggplot(data = states_map, aes(x = long, y = lat, group = group, fill = pro)) +
  geom_polygon(color = "white") +
  scale_fill_gradient2(low = "maroon", mid = "red", high = "pink", midpoint = -0.0960764, limits = c(-0.2, 0.01219512)) +
  coord_fixed(1.3) +
  labs(fill = "Sentiment Score", title = "Sentiment Analysis by State") +
  theme_void()


















