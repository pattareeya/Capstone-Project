setwd("C:/Capstone Project/")
#set file as binary
con <- file("blogsample.txt", open = "rb")
#read file and encoding as UTF-8
btxt1 <- readLines(con, encoding = "UTF-8")
close(con)
con <- file("newssample.txt", open = "rb")
ntxt1 <- readLines(con,encoding = "UTF-8")
close(con)
con <- file("tweetsample.txt", open = "rb")
ttxt1 <- readLines(con, encoding = "UTF-8")
close(con)

btxt1 <- iconv(btxt1, from="UTF-8", to="latin1", sub=" ") 
ntxt1 <- iconv(ntxt1, from="UTF-8", to="latin1", sub=" ") 
ttxt1 <- iconv(ttxt1, from="UTF-8", to="latin1", sub=" ") 


#text mining package "tm"
library(NLP)
library(tm)
#Data Import in to Corpus
btxt1.corpus <- Corpus(VectorSource(btxt1))
ntxt1.corpus <- Corpus(VectorSource(ntxt1))
ttxt1.corpus <- Corpus(VectorSource(ttxt1))

#Convert to lowercase
btxt1.corpus <- tm_map(btxt1.corpus, content_transformer(tolower))
ntxt1.corpus <- tm_map(ntxt1.corpus, content_transformer(tolower))
ttxt1.corpus <- tm_map(ttxt1.corpus, content_transformer(tolower))

#Removing stop words SMART for english stopwords from the SMART information retreival system
btxt1.corpus <- tm_map(btxt1.corpus, removeWords, stopwords(kind = "SMART")) 
ntxt1.corpus <- tm_map(ntxt1.corpus, removeWords, stopwords(kind = "SMART")) 
ttxt1.corpus <- tm_map(ttxt1.corpus, removeWords, stopwords(kind = "SMART")) 

inspect(btxt1.corpus[20:25])
inspect(ntxt1.corpus[20:25])
inspect(ttxt1.corpus[20:25])

#Remove URL
#removeURLs <- function(x) gsub("http[[:alnum:]]*.* ", "",x)
#btxt1.corpus = tm_map(btxt1.corpus, removeURLs)
#ntxt1.corpus = tm_map(ntxt1.corpus, removeURLs)
#ttxt1.corpus = tm_map(ttxt1.corpus, removeURLs)


#Remove profane words
#list of profane words. Source is from http://www.bannedwordlist.com/lists/swearWords.txt
pwords <- c("anal","anus","arse","ass","asshole","ballsack","balls","bastard","bitch","biatch","bloody",
            "blowjob","blow job","bollock","bollok","boner","boob","bugger","bum",
            "butt","buttplug","clitoris","cock","coon","crap","cunt","damn","dick",
            "dildo","dumbass","dyke","fag","feck","fellate","fellatio","felching","fuck","f u c k",
            "fudgepacker","fudge packer","flange","Goddamn","God damn","hell","homo",
            "jerk","jizz","knobend","knob end","labia","lmao","lmfao","muff","nigger","
            nigga","omg","penis","piss","poop","prick","pube","pussy","queer","scrotum",
            "sex","shit","s hit","sh1t","slut","smegma","spunk","tit","tosser","turd",
            "twat","vagina","wank","whore","wtf"
)

btxt1.corpus <- tm_map(btxt1.corpus, removeWords, pwords)
ntxt1.corpus <- tm_map(ntxt1.corpus, removeWords, pwords)
ttxt1.corpus <- tm_map(ttxt1.corpus, removeWords, pwords)



#Remove punctuations
btxt1.corpus = tm_map(btxt1.corpus, removePunctuation)
ntxt1.corpus = tm_map(ntxt1.corpus, removePunctuation)
ttxt1.corpus = tm_map(ttxt1.corpus, removePunctuation)

#Remove numbers

btxt1.corpus = tm_map(btxt1.corpus, removeNumbers)
ntxt1.corpus = tm_map(ntxt1.corpus, removeNumbers)
ttxt1.corpus = tm_map(ttxt1.corpus, removeNumbers)


#Strip extra white space
btxt1.corpus = tm_map(btxt1.corpus, stripWhitespace)
ntxt1.corpus = tm_map(ntxt1.corpus, stripWhitespace)
ttxt1.corpus = tm_map(ttxt1.corpus, stripWhitespace)

#want to predict simple texts such as "At the end of the day...", so I did not remove stopwords

#Tokenization
#ttoken1 <-scan_tokenizer(ttxt1.corpus)

#Next we perform stemming, which removes affixes from words 
#(so, for example, "run", "runs" and "running" all become "run").

library(SnowballC)

btxt1.corpus <- tm_map(btxt1.corpus, stemDocument)  
ntxt1.corpus <- tm_map(ntxt1.corpus, stemDocument)  
ttxt1.corpus <- tm_map(ttxt1.corpus, stemDocument)

btxt1.corpus <- tm_map(btxt1.corpus, PlainTextDocument)
ntxt1.corpus <- tm_map(ntxt1.corpus, PlainTextDocument)
ttxt1.corpus <- tm_map(ttxt1.corpus, PlainTextDocument)


#Conver to Term Document Matrix
btxt1.tdm <- TermDocumentMatrix(btxt1.corpus)
ntxt1.tdm <- TermDocumentMatrix(ntxt1.corpus)
ttxt1.tdm <- TermDocumentMatrix(ttxt1.corpus)

btxt1.tdm
ntxt1.tdm
ttxt1.tdm

inspect(btxt1.tdm[1:10,1:10])

#show the words which are frequently use
findFreqTerms(btxt1.tdm, 1000)
findAssocs(btxt1.tdm, "good", 0.10)
findAssocs(btxt1.tdm, "like", 0.10)

#plot frequecy words using historgram
library(ggplot2)
library(slam)

bRowTotal <- row_sums(btxt1.tdm)
btxt2.tdm <- btxt1.tdm[which(bRowTotal > 1000),]
btxt2.tdm
bFreq <- as.data.frame(rowSums(as.matrix(btxt2.tdm)))
colnames(bFreq) = c("Freq")
ggplot(bFreq) + geom_bar(aes(x=row.names(bFreq), y = Freq), stat = "identity") + labs(x = "Terms", y = "Frequency", title = "Term Frequency of Samples from Blogs") +coord_flip()

nRowTotal <- row_sums(ntxt1.tdm)
ntxt2.tdm <- ntxt1.tdm[which(nRowTotal > 1600),]
ntxt2.tdm
nFreq <- as.data.frame(rowSums(as.matrix(ntxt2.tdm)))
colnames(nFreq) = c("Freq")
ggplot(nFreq) + geom_bar(aes(x=row.names(nFreq), y = Freq), stat = "identity") + labs(x = "Terms", y = "Frequency", title = "Term Frequency of Samples from News") +coord_flip()

tRowTotal <- row_sums(ttxt1.tdm)
ttxt2.tdm <- ttxt1.tdm[which(tRowTotal > 4000),]
ttxt2.tdm
tFreq <- as.data.frame(rowSums(as.matrix(ttxt2.tdm)))
colnames(tFreq) = c("Freq")
ggplot(tFreq) + geom_bar(aes(x=row.names(tFreq), y = Freq), stat = "identity") + labs(x = "Terms", y = "Frequency", title = "Term Frequency of Samples from Tweet") +coord_flip()

#Merge samples from blogs, news and tweet
txt.corpus <-c(btxt1.corpus, ntxt1.corpus,ttxt1.corpus)
txt.corpus <- sample(txt.corpus, size = length(txt.corpus)/3)

library(RWeka)
#2-gram
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram.tdm <- TermDocumentMatrix(txt.corpus, control = list(tokenize = bigram))
findFreqTerms(bigram.tdm, 100)


#3-gram
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram.tdm <- TermDocumentMatrix(txt.corpus, control = list(tokenize = trigram))
findFreqTerms(trigram.tdm, 5)

#4-gram
quadgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgram.tdm <- TermDocumentMatrix(txt.corpus, control = list(tokenize = quadgram))
findFreqTerms(quadgram.tdm, 100)


greptdm <- function(x,input = character) grep(input,x, value = TRUE)
d

#plot historgram of 2-gram and 3-grams
