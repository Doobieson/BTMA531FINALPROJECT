###### LOAD NECESSARY LIBRARIES ######
library(data.table)
library(tm)
library(SnowballC)
library(wordcloud)


###### DATA PREP ######
bnb = fread("Airbnb_Data.csv") # Read data into r

bnb$log_price <- exp(bnb$log_price) # Un-log the price to get the actual price - some minor data cleaning that we kept as standard
                                    # across the entire group to make sure that we are all working with the the same dataset


###### LISTING DESCRIPTIONS ######
desc_bnb <- data.frame(doc_id = seq.int(nrow(bnb)), text = bnb$description) # Create subset of data for analysis

descriptions <- VCorpus(DataframeSource(desc_bnb)) # Convert csv to something usable

descriptions <- tm_map(descriptions, content_transformer(tolower)) # Make it all lowercase

descriptions <- tm_map(descriptions, removePunctuation) # Remove all punctuation

descriptions <- tm_map(descriptions, stripWhitespace) # Remove all white spaces

descriptions <- tm_map(descriptions, removeNumbers) # Remove all numbers

descriptions <- tm_map(descriptions, removeWords, stopwords("english")) # Remove all stopwords (this step takes a little longer)

descriptions <- tm_map(descriptions, stemDocument) # Stem document

dtm <- DocumentTermMatrix(descriptions) # Create document term matrix (this step also takes a little longer)

findFreqTerms(dtm, 250) # Displays a list of words in alphabetical order used at least 250 times.
                        # There seems to be nearly 2000 words here so this number could be higher - this is for demonstration purposes only.

tdm <- TermDocumentMatrix(descriptions) # Create term document matrix (this step also takes a little longer)

tdm <- removeSparseTerms(tdm, sparse = 0.98) # If we try to perform the next step, it will fail since the matrix would be too large for R to handle.
                                             # Therefore, we must cut out the sparse words now, 98% of them.

wordcloud_matrix <- as.matrix(tdm) # Then we can create the wordcloud matrix with this smaller matrix

wordcloud_frequency <- sort(rowSums(wordcloud_matrix), decreasing = T) # Turn this matrix into a word frequency list

set.seed(1) # Set seed for replicability before generating the wordcloud

wordcloud(words = names(wordcloud_frequency), freq = wordcloud_frequency, min.freq = 350, random.order = F)  # Create the wordcloud with a minimum frequency
                                                                                                             # of 350

wordcloud_frequency # Displays which words are used the most

dist_matrix <- dist(scale(wordcloud_matrix)) # Get the distances between observations for clustering - need to scale the data first
                                             # (this step also takes a little while)

word_hier <- hclust(dist_matrix) # Cluster terms

plot(word_hier) # Not the greatest looking dendrogram since there are a large number of terms

# We can also perform the same analysis on the TITLES of the listings, since this is most likely what customers will see first - their first impression


###### LISTING TITLES ######
# Essentially repeat the same steps above but with the titles instead of descriptions
title_bnb <- data.frame(doc_id = seq.int(nrow(bnb)), text = bnb$name)

titles <- VCorpus(DataframeSource(title_bnb))

titles <- tm_map(titles, content_transformer(tolower))

titles <- tm_map(titles, removePunctuation)

titles <- tm_map(titles, stripWhitespace)

titles <- tm_map(titles, removeNumbers)

titles <- tm_map(titles, removeWords, stopwords("english"))

titles <- tm_map(titles, stemDocument)

titles_dtm <- DocumentTermMatrix(titles)

findFreqTerms(titles_dtm, 250)

titles_tdm <- TermDocumentMatrix(titles)

titles_tdm <- removeSparseTerms(titles_tdm, sparse = 0.99)

titles_wordcloud_matrix <- as.matrix(titles_tdm)

titles_wordcloud_frequency <- sort(rowSums(titles_wordcloud_matrix), decreasing = T)

set.seed(1)

wordcloud(words = names(titles_wordcloud_frequency), freq = titles_wordcloud_frequency, min.freq = 250, random.order = F)

titles_wordcloud_frequency