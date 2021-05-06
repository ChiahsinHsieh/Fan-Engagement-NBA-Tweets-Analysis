# Set the working directory
setwd("~/Desktop/hult_NLP_student/cases/NBA Fan Engagement/personal")

# Libs
library(tm)
library(ggplot2)
library(ggthemes)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

#'Stops words
stops <- c(stopwords('SMART'),'us','won','win','lose','today','tonight','twitter','season'
           ,'nba','score','lol', 'amp', 'hawks','cavs','Atlanta Hawks','love')

# Read in Data, clean & organize
text      <- read.csv('M_Oct2020.csv',nrows = 10000)

txtCorpus <- VCorpus(VectorSource(text$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

# Frequency Data Frame
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
tweetFreq[50:55,]

# Simple barplot; values greater than 500
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 500) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# qdap version, slightly different results based on params but faster
# plot(freq_terms(text$text, top=35, at.least=2, stopwords = stops))

#'Stops words
stops <- c(stopwords('SMART'), 'nba', 'amp', 'hawks','cavs','Atlanta Hawks','love')

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Data
text <- read.csv('M_Oct2020.csv',nrows = 10000)

# As of tm version 0.7-3 tabular was deprecated
names(text)[1] <-'doc_id' 

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
OctTDM  <- TermDocumentMatrix(txtCorpus, 
                              control=list(tokenize=bigramTokens))
OctTDMm <- as.matrix(OctTDM)

# Get Row Sums & organize
OctTDMv <- sort(rowSums(OctTDMm), decreasing = TRUE)
text   <- data.frame(word = names(OctTDMv), freq = OctTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(topWords$word,   
          topWords$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

#Zoom in the top 5 popular words
# Simple barplot; values greater than 745
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 745) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# qdap version, slightly different results based on params but faster
# plot(freq_terms(text$text, top=35, at.least=2, stopwords = stops))

##
# Read in Data, clean & organize
text      <- read.csv('L_Sep2020.csv',nrows = 10000)
txtCorpus <- VCorpus(VectorSource(text$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

# Frequency Data Frame
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
tweetFreq[50:55,]

# Simple barplot; values greater than 500
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 500) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# qdap version, slightly different results based on params but faster
# plot(freq_terms(text$text, top=35, at.least=2, stopwords = stops))


#Zoom in the top 5 popular words
# Simple barplot; values greater than 745
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 1850) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# qdap version, slightly different results based on params but faster
# plot(freq_terms(text$text, top=35, at.least=2, stopwords = stops))

##
# Read in Data, clean & organize
text      <- read.csv('K_Aug2020.csv',nrows = 10000)
txtCorpus <- VCorpus(VectorSource(text$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

# Frequency Data Frame
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
tweetFreq[50:55,]

# Simple barplot; values greater than 500
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 500) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# qdap version, slightly different results based on params but faster
# plot(freq_terms(text$text, top=35, at.least=2, stopwords = stops))


#Zoom in the top 5 popular words
# Simple barplot; values greater than 745
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 953) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# qdap version, slightly different results based on params but faster
# plot(freq_terms(text$text, top=35, at.least=2, stopwords = stops))


#END
