complaint.data <- read.csv("./Data/complaints_add_label.csv") # Read data
View(complaint.data) # Check the data

complaint <- complaint.data %>% filter(Complaint != "") %>% # Filter out blank complaints 
  select(Label, Complaint) # Select label and complaint columns

View(complaint) # Check the data
nrow(complaint) # Check the number of data

colnames(complaint) <- c("type", "text") # Rename columns

##################################################################################################

library(dplyr)
library(tibble)

complaint.bylabel <- complaint %>% 
  select(text, type) %>% 
  group_by(type) %>% 
  summarise(text = paste(text, collapse = " ")) %>% 
  add_column(doc_id=1:nrow(.), .before=1) %>% # Add doc_id column
  mutate(text=iconv(text, to="ascii", sub="")) # Change to ascii code, and delete unchanged words

#View(complaint.bylabel)

library(tm)

docs.bylabel <- VCorpus(DataframeSource(complaint.bylabel)) # Make a corpus
docs.bylabel # Check the corpus

myRemove <- content_transformer(function(x, pattern) # Function that removes the input pattern
{return(gsub(pattern, "", x))})

docs.bylabel <- tm_map(docs.bylabel, myRemove, "X+") # Remove 'XXXX'

library(stopwords)
library(textstem)
library(stringr)

## !!!Takes some time to run this code!!!
docs.bylabel <- docs.bylabel %>% 
  tm_map(removePunctuation) %>% # Remove punctuation
  tm_map(removeNumbers) %>% # Remove numbers
  tm_map(stripWhitespace) %>% # Strip spaces
  tm_map(content_transformer(lemmatize_strings)) %>% # Lemmatize
  tm_map(content_transformer(tolower)) %>% # To lower
  tm_map(content_transformer(trimws)) %>% # Remove margin spaces
  tm_map(content_transformer(removeWords), 
         stopwords('en', source='stopwords-iso')) %>% # Remove stopwords
  tm_map(content_transformer(str_squish)) # Every spaces into single space

dtm.bylabel <- DocumentTermMatrix(
  docs.bylabel,
  control=list(wordLengths=c(3,Inf))) # Limit the word length

dtm.bylabel.tfidf <- DocumentTermMatrix(
  docs.bylabel,
  control=list(wordLengths=c(3,Inf), # Limit the word length
               weighting=function(x)
                 weightTfIdf(x, normalize = FALSE))) # Calculate TF-IDF

dtm.bylabel.tfidf.w <- DocumentTermMatrix(
  docs.bylabel,
  control=list(wordLengths=c(3,Inf), # Limit the word length
               weighting=function(x)
                 weightTfIdf(x, normalize = TRUE))) # Calculate TF-IDF

inspect(dtm.bylabel) # Check the DTM

comp.table <- data.frame(
  # Creating a 'doc' column by repeating row names of dtm.bylabel
  doc = rep(rownames(dtm.bylabel), dim(dtm.bylabel)[2]),
  # Creating a 'term' column by repeating and sorting column names of dtm.bylabel
  term = rep(colnames(dtm.bylabel) %>% sort(decreasing = FALSE),
             each = dim(dtm.bylabel)[1]),
  # Adding a 'TF' column with the vectorized values of dtm.bylabel
  TF = as.vector(dtm.bylabel),
  # Adding a 'TF_IDF' column with the vectorized values of dtm.bylabel.tfidf
  TF_IDF = as.vector(dtm.bylabel.tfidf),
  # Adding a 'W_TF_IDF' column with the rounded vectorized values of dtm.bylabel.tfidf.w
  W_TF_IDF = as.vector(dtm.bylabel.tfidf.w) %>% round(3)
)

# Sorting comp.table based on the 'TF' column in descending order and displaying the top 10 rows
comp.table %>% arrange(desc(TF)) %>% head(10)

# Sorting comp.table based on the 'TF_IDF' column in descending order and displaying the top 10 rows
comp.table %>% arrange(desc(TF_IDF)) %>% head(10)

# Check top 5 words in each document
top_terms_per_doc <- apply(dtm.bylabel[, -1], 1, function(x) {
  sorted_indices <- order(x, decreasing = TRUE)
  names(x)[sorted_indices][1:5]
})

# Print the result with a data frame
top_terms_df <- data.frame(Docs = 1:nrow(dtm.bylabel), t(top_terms_per_doc))
rownames(top_terms_df) <- c("Card&Account", "Credit_management", "Loan") 
colnames(top_terms_df) <- c("Doc_id", "Term1", "Term2", "Term3", "Term4", "Term5")

top_terms_df # Print the result (Top 5 TF-IDF words in each document)

##################################################################################################

complaint <- complaint %>% 
  select(text, type) %>% # Column order switch
  add_column(doc_id=1:nrow(.), .before=1) %>% # Add doc_id column
  mutate(text=iconv(text, to="ascii", sub="")) # Change to ascii code, and delete unchanged words

docs <- VCorpus(DataframeSource(complaint)) # Make a corpus
docs # Check the corpus

lapply(docs, content)[c(10, 100, 1000)] # Check the content
meta(docs)$type[c(10, 100, 1000)] # Check the meta variable

docs <- tm_map(docs, myRemove, "X+") # Remove 'XXXX'

## !!!Takes some time to run this code!!!
docs <- docs %>% 
  tm_map(removePunctuation) %>% # Remove punctuation
  tm_map(removeNumbers) %>% # Remove numbers
  tm_map(stripWhitespace) %>% # Strip spaces
  tm_map(content_transformer(lemmatize_strings)) %>% # Lemmatize
  tm_map(content_transformer(tolower)) %>% # To lower
  tm_map(content_transformer(trimws)) %>% # Remove margin spaces
  tm_map(content_transformer(removeWords), 
         stopwords('en', source='stopwords-iso')) %>% # Remove stopwords
  tm_map(content_transformer(str_squish)) # Every spaces into single space

## doc_save <- docs # Save docs just in case

lapply(docs, content)[c(10, 100, 1000)] # Check the content

dtm <- DocumentTermMatrix(docs) # Make DTM
inspect(dtm) # Check the DTM -> There are too many words included

dtm <- DocumentTermMatrix(
  docs,
  control=list(wordLengths=c(3,Inf), # Limit the word length
               bounds=list(global=c(13,26159)))) # Lower and upper limits for the number of documents that appear
                                                 # Except for words that appear in less than 0.05% of all documents and words that appear in more than 95% of all documents  

# Remove rows with sparsity over 0.99
dtm <- removeSparseTerms(x =  dtm, sparse = as.numeric(x = 0.99))

inspect(dtm) # Check the DTM

##################################################################################################

termfreq <- colSums(as.matrix(dtm)) # Frequency of each word
termfreq[head(order(termfreq, decreasing = TRUE), 10)] # Top 10 words
termfreq[tail(order(termfreq, decreasing = TRUE), 50)] # Bottom 10 words

# Creating a Bar Graph with the Top 10 Words and Adjusting Axis Labels
barplot(termfreq[head(order(termfreq, decreasing = TRUE), 20)],
        main = "Top 20 Words",
        xlab = "Words",
        ylab = "Frequency",
        col = "skyblue",
        cex.names = 0.7,
        las = 2,  # Rotate labels for better readability
        cex.axis = 0.6,  # Adjust x and y axis label size
        cex.lab = 0.6)  # Adjust main title, xlab, and ylab size

findFreqTerms(dtm, # Words with high frequencies
              lowfreq = 500) # Minimum frequency number

# Check which words are used with other words that appear frequently
findAssocs(dtm, c("bank", "account", "america", "credit", "card"), 0.25) # Check the high frequency words with correlation

##################################################################################################

# Network
#install.packages("network")
#install.packages("GGally")
#install.packages("sna")
library(network)
library(GGally)
library(sna)

# Create a document-term matrix (dtm), convert it to a matrix, and compute the correlation
corTerms <- dtm %>% as.matrix() %>% cor()

# Display the top-left 10x10 part of the correlation matrix
corTerms[1:10, 1:10]

# Set correlations below 0.25 to 0
corTerms[corTerms <= 0.25] <- 0

# Create a network using the correlation matrix
netTerms <- network(x = corTerms, directed = FALSE)

# Compute betweenness centrality for each term in the network
btnTerms <- betweenness(netTerms)
btnTerms[1:10]

# Assign 'Top' or 'Rest' label to vertices based on betweenness centrality
netTerms %v% 'mode' <-
  ifelse(
    test = btnTerms >= quantile(x = btnTerms, probs = 0.95, na.rm = TRUE), 
    yes = 'Top', 
    no = 'Rest'
  )

# Define node colors
nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')

# Set edge sizes based on the correlation values
set.edge.value(netTerms, attrname = 'edgeSize', value = corTerms * 3)

# Visualize the network using ggnet2
ggnet2(
  net = netTerms,
  mode = 'fruchtermanreingold',
  layout.par = list(cell.jitter = 0.001),
  size.min = 5,
  label = TRUE,
  label.size = 3,
  node.color = 'mode',
  palette = nodeColors,
  node.size = sna::degree(dat = netTerms),
  edge.size = 'edgeSize',
  family = 'AppleGothic'
) +
  theme_gray(base_family = 'NanumGothic') # Only for Mac users!

##################################################################################################

# Wordcloud
library(wordcloud)
library(RColorBrewer)
set.seed(123)
wordcloud(words=names(termfreq), freq=termfreq,
          scale=c(4, 0.5), # Range of word sizes
          min.freq = 30, # Minimum word frequency
          max.words = 200, # Maximum number of words
          rot.per = 0, # Rotation percentage (0 for horizontal)
          random.order = FALSE, # Order words by frequency (not random)
          random.color = FALSE, # Use consistent color for each word
          colors = brewer.pal(6, "Set2") # Color palette from Set2 with 6 colors
)

# To create a comparison cloud, you need a matrix where rows represent words, columns represent categories,
# and cell values indicate the frequency of occurrence.
comparision.label <- as.matrix(dtm)
rownames(comparision.label) <- complaint$type # Set row names to words
comparision.label[1:5, 1:5] # Displaying a subset of the matrix for illustration

# Calculate the sum of each row
comparision.label <- rowsum(comparision.label, 
                            group = rownames(comparision.label)) # Vector specifying the categories to include
comparision.label[, 1:10] # Displaying a subset of the matrix where each cell represents the frequency of each word

set.seed(123)
# Create a comparison cloud by transposing the matrix (rows become words, columns become categories)
comparison.cloud(t(comparision.label), 
                 title.size = 0.7, # Title size
                 colors = brewer.pal(3, "Set2"), # Color palette from Set2 with 3 colors
                 title.colors = brewer.pal(3, "Set2"), # Title color palette
                 random.order=FALSE, # Random order False
                 title.bg.colors = "wheat", # Title background color
                 rot.per = 0, # Rotation percentage (0 for horizontal)
                 scale = c(3, 0.5),  # Scale for word sizes
                 max.words = 200) # Maximum number of words

##################################################################################################

### Creating a Predictive Model Using Naive Bayes
# Predictive variable: Words extracted from documents
# Label variable: Labels of documents
# To perform Naive Bayes analysis:
#   Predictive variable should be in matrix or data frame format
#   Label variable should be in factor format
# Predictive variable is stored in dtm, as seen in inspect(dtm)
# Label variable: complaint$type

# 70:30 = train:test
#set.seed(123)
train <- sample(nrow(complaint), 0.7 * nrow(complaint)) # Splitting the data into training and testing tets
y.train <- complaint[train, ]$type # Training response variable
y.test <- complaint[-train, ]$type # Testing response variable

table(y.train) # Number of labels in train data
table(y.test) # Number of labels in test data

# Checking if the split datasets reflect the characteristics of the entire dataset
prop.table(table(complaint$type)) # Entire dataset
prop.table(table(y.train)) # Train dataset
prop.table(table(y.test)) # Test dataset
# Both reflect the proportions of the entire dataset

# Naive Bayes works based on categorical predictive variables
# Currently, the predictive variable is the frequency of word occurrences,
# so the predictive variable needs to be converted to categorical format (1 if the word appears, 0 otherwise)
toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0) # If the cell value is greater than 0, set it to 1; otherwise, set it to 0
  x <- factor(x, level = c(0, 1), labels = c("no", "yes")) # Convert 0 to "no" and 1 to "yes"
  return(x)
}

## !!!Takes some time to run this code!!!
complaint.dtm <- apply(dtm, MARGIN = 2, toFactor) # Execute the function in the column direction
str(complaint.dtm) # Display the structure of the resulting matrix

complaint.dtm[1:10, 1:10] # Check the matrix

x.train <- complaint.dtm[train, ] # Training set for predictive variable
x.test <- complaint.dtm[-train, ] # Testing set for predictive variable

library(e1071)
complaint.nb <- naiveBayes(x=as.matrix(x.train), y=y.train) # Train the model
## !!!Takes some time to run this code!!!
complaint.nb.pred <- predict(complaint.nb, newdata=as.matrix(x.test)) # Predict the test data
head(complaint.nb.pred) # Check the predicted result

# Model evaluation
evaluate_model <- function(actual, predicted) {
  # Calculate the evaluation metrics 
  confusion_mat <- table(Actual = actual, Predicted = predicted, dnn=c("Actual", "Predicted"))
  accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat) # Accuracy
  precision <- confusion_mat[2, 2] / sum(confusion_mat[, 2]) # Precision
  recall <- confusion_mat[2, 2] / sum(confusion_mat[2, ]) # Recall
  f1_score <- 2 * (precision * recall) / (precision + recall) # F1 Score
  
  # round up
  accuracy <- accuracy %>% round(2)
  precision <- precision %>% round(2)
  recall <- recall %>% round(2)
  f1_score <- f1_score %>% round(2)
  
  # Print the result
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1_score, "\n")
}

library(caret)
# Confusion matrix
cm <- confusionMatrix(factor(y.test), factor(complaint.nb.pred), dnn = c("Prediction", "Actual"))
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Actual,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Actual") +
  scale_x_discrete(labels=c("Card&Account","Credit_Management","Loan")) +
  scale_y_discrete(labels=c("Loan","Credit_Management","Card&Account"))

evaluate_model(y.test, complaint.nb.pred) # Print the calculated evaluation metrics
