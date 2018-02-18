#setwd("~/text_as_data")
rm(list=ls())
# xd <- read.csv('dataset_clean_1.csv')
# save(xd, file="xd.RData")
load("xd.RData")
library(RTextTools)
library(tm)
library(topicmodels)
library(SnowballC)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(tidyverse)


sample <- xd %>% sample_n(2000)
corpus <- Corpus(VectorSource(sample$text))

docs <-tm_map(corpus ,content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "\\'")
docs <- tm_map(docs, toSpace, "\\'")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, "\\'")
docs <- tm_map(docs, toSpace, "\"")


#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)


# Define additional stopwords here
myStopwords <- c("can", "say","one","way","use",
                  "also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                  "might","see","someth","thing","point",
                  "post","look","right","now","think","'ve ",
                  "'re ","anoth","put","set","new","good",
                  "want","sure","kind","larg","yes,","day","etc",
                  "quit","sinc","attempt","lack","seen","awar",
                  "littl","ever","moreov","though","found","abl",
                  "enough","far","earli","away","achiev","draw",
                  "last","never","brief","bit","entir","brief",
                  "great","lot", "new", "york", "yesterday", "monday", "tuesday",
                 "wednesday", "thursday", "friday", "saturday", "sunday", "type",
                 "time", "global", "january", "february", "march", "april", "may", "june", "july", "august", "september",
                  "october", "november", "december", "year","date", "people", "page", "global", "week", "mrs", "mr", "climate", "change", "state",
                 "store", "book", "film", "free", "theater", "museum", "center", "art", "street", "group", "show", "jan", "feb", "mar", "apr", "jun", "jul", "aug",
                 "sept", "oct", "nov", "dec", "colleg", "college", "perform", "music", "work", "exhibit", "avenue", "avenu", "park", "photograph", "percent", "chang", 
                 "people", "peopl", "unit", "student", "world", "museum", "news", "public", "climat", "spring","artist",
                 "copyright", "english", "language", "document", "language", "languag", "author", "school", "includ", "school","play", "stori",
                 "open", "hour", "nation", "editor", "hightlight", "movi", "minut", "director", "direct", "love", "star", "rate", "documentari", "presid", "minut", "movi",
                 "galleri", "featur", "cultur", "museum", "yea", "just", "issu", "issue", "don", "write", "paint", "climatechang", "newspap", "url",
                 "song", "wine", "festiv", "univers", "west", "load", "web", "seem", "back", "blog" )
docs <- tm_map(docs, removeWords, myStopwords)

climatechange_dtm <- DocumentTermMatrix(docs)

# Print out climatechange_dtm
climatechange_dtm

ui = unique(climatechange_dtm$i)
dtm.new = climatechange_dtm[ui,]
k <- 20 # Number of topics
ldaOut <- LDA(dtm.new, k, method="VEM")

#save(ldaOut, file="ldaOut_2000_20.RData")
#------------------------------------
# Continue from here if using ldaOut file
 #load("ldaOut_2000_15_b.RData")
#-----------------------------------

#List topics
ldaOut.topics <- as.matrix(topics(ldaOut))

# Pullt out top 10 terms for each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))

# Produce a yearly count of terms
# Takes a given year of article text, counts number of times
# that any words in a given set of terms occur.
find_terms <- function(content, terms){
  matches <- sum(unlist(lapply(content, function(x) sum(str_count(x, pattern=terms), na.rm=TRUE))))
  return(matches)
}

# Removes data with year==NA
xd <- xd[is.na(xd$date)==FALSE,]

# Gets number of years in data
years <- unique(xd$date[is.na(xd$date)==FALSE])

# Generate an empty matrix to populate with term counts by year
# 5 rows for 5 topics, column for each year
lda_count.out <- matrix(NA, nrow=15, ncol=length(years))

# DOuble nested loop that runs over number of years and number of topics
# This gets the count of terms occurring in every article in a given year
# for a given topic, and assigns it to a row=topic# and col=year.
for(i in 1:length(years)){
  for(j in 1:dim(ldaOut.terms)[2]){
    lda_count.out[j,i] <- find_terms(xd$text[xd$date==years[i]], ldaOut.terms[,j])
  }
}
# Convert this output to a data frame; give it labels
lda_count <- as.data.frame(lda_count.out)
names(lda_count) <- years 
row.names(lda_count) <- paste0("Topic", 1:15)
lda_count # Display results

# Convert to yearly proportions
lda_proportions <- prop.table(as.matrix(lda_count), margin=2)
 rownames(lda_proportions) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")

# Plot as barplot and line plot
 par(mar=c(5,4,4,3))
 barplot(lda_proportions[nrow(lda_proportions):1,], legend.text=TRUE,
        args.legend=list(x="topright", inset=c(-0.05,0)),
        col=brewer.pal(8,"Dark2"), main="Article Share of Topics (k=15)")
 
layout(matrix(c(1,2),nrow=1), width=c(4,1))
par(mar=c(5,4,4,0))
matplot(t(lda_proportions), type="l", lty=1, ylim=c(0,.2), ylab="Proportion", xaxt="n")
axis(1, at=1:length(years), labels=years)
par(mar=c(5,0,4,2))
plot(c(0,1), type="n", axes=FALSE, xlab="", ylab="")
legend("center", rownames(lda_proportions), col=seq_len(ncol(t(lda_proportions))),
       cex=1, fill=seq_len(ncol(t(lda_proportions))), y.intersp=1.2)

