# Quinn text visualization assignment

# Load packages
library(tm)
library(SnowballC)
library(som)
library(tau)
library(wordcloud)
library(dplyr)
library(ggplot2)

#Function to find the top ten words
Top10Words = function(df){
  sort(colSums(df), decreasing=TRUE)[c(1:10)]
}

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

bg_stopwords <- c("players", "player", "game", "card", "cards", "can", "will")

bg_corpus <- Corpus(VectorSource(board_games$description))

bg_corpus <- bg_corpus %>% tm_map(removeNumbers) %>% tm_map(tolower) %>% tm_map(removePunctuation) %>% tm_map(removeWords, c(stopwords("english"),bg_stopwords)) #%>% tm_map(PlainTextDocument)

bg_frequencies <- DocumentTermMatrix(bg_corpus)
bg_df <- as.data.frame(as.matrix(bg_frequencies))

# Remove sparse terms
bg_sparse <- removeSparseTerms(bg_frequencies, 0.98)
bg_sparse <- as.data.frame(as.matrix(bg_sparse))
colnames(bg_sparse) = make.names(colnames(bg_sparse))

# Sort the sparsed dataframe by decreasing frequency and set the color levels for the word cloud
bg_wordFreqsort <- sort(colSums(bg_sparse),decreasing=T)
bg_grayLevels <- gray((bg_wordFreqsort + 10)/(max(bg_wordFreqsort)+10))
bg_wordCloud <- wordcloud(words=names(bg_wordFreqsort),freq=bg_wordFreqsort,min.freq=50,random.order=T,colors=bg_grayLevels)

# Cluster analysis
bg2 <- bg_sparse
bg2[bg2>1]=1

bgNorm <- bg2
bgNorm_Dist <- dist(bgNorm, method="manhattan") # compute distances
bgNorm_HC <- hclust(bgNorm_Dist, method="ward.D") # make model
plot(bgNorm_HC) # plot the dendogram
bgNorm_HC_Groups <- cutree(bgNorm_HC, k = 5) # split data into clusters

#Visualize the clusters
lapply(split(bg_sparse, bgNorm_HC_Groups), Top10Words)



lapply(split(bs_sparse, bsNorm_HC_Groups), Top10Words)