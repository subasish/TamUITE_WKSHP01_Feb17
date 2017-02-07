##### TEXT MINING CODES
setwd("/Users/subasishdas1/Dropbox/TWT_FB/aircrash")
library(tm)

list.files()
bt_tw1 <- read.csv("ac.csv")
dim(bt_tw1)
names(bt_tw1)
#### bt_tw2 <- bt_tw1[!duplicated(bt_tw1[,2]),]

all2<- data.frame(Year = unique(bt_tw1$Year), 
Summary = tapply(bt_tw1$Summary, bt_tw1$Year, 
               paste, collapse = ' '))
head(all2)
dim(all2)
names(all2)

library(tm)

mydata.corpus <- Corpus(VectorSource(all2$Summary))
mydata.corpus <- tm_map(mydata.corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
mydata.corpus <- tm_map(mydata.corpus, content_transformer(tolower), mc.cores=1) 
mydata.corpus <- tm_map(mydata.corpus, removePunctuation, preserve_intra_word_dashes=TRUE, mc.cores=1) 
my_stopwords <- c(stopwords('german'),"the", "due", "are", "not", "for", "this", "and", 
"that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than", 
"set","has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", 
"ccc", "end", "have", "avenue", "before", "i-us", "i-e", "i-i-", "ames", "belle", "gen", 
"okeefe", "one", "just", "mac", "being", "i-i-", "left", "right", "west",  "when",
"levels","remaining","based", "issues",  "still", "off", "over", "only", "north", 
"past", "twin", "while",  "i-w" ,"air","aircraft", "flight", "plane", "accident","airplane",  "general" , "harvey", "i-e","i-i-","i-us" , 
"must", "more", "work","read",  "reached", "morrison",  "mph", "three","info", 
"canal", "camp", "la-", "approximately",  "amp", "access", "approaching",  "forest", 
"friday",  "its", "affect", "after", "within", "what", "various", "under", "toward", 
"san", "other" , "city", "into", "by", "for", "is", "are", "their", "he", "she", 
"research", "through", "between", "under", "below", "over", "with", "an", "affect", 
"nowadays", "present", "important", "significant", "then", "using", "having", 
"via", "vermont", "some", "rap", "how", "can","task","type","next","limit","areas",
"real-","samll","method","case", "california","canada","used","effect","variable",
"state","factors","zone","analysis","crashed", "miles", "flying", "test","were","methods", "had", "his", "which", "but")

mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords, mc.cores=1)
mydata.corpus <- tm_map(mydata.corpus, removeNumbers, mc.cores=1) 
mydata.dtm2 <- TermDocumentMatrix(mydata.corpus) 
library(slam)
TDM.dense <- as.matrix(mydata.dtm2)
head(TDM.dense)
dim(TDM.dense)


mydata.dtm3 <- removeSparseTerms(mydata.dtm2, sparse=0.25)
dim(mydata.dtm3)

### write.csv(TDM.dense, "new1.csv")
termDocMatrix <- as.matrix(mydata.dtm3)
termMatrix <- termDocMatrix  %*% t(termDocMatrix)

library(igraph)
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


mydata.dtm3 <- removeSparseTerms(mydata.dtm2, sparse=0.18)
dim(mydata.dtm3)

### write.csv(TDM.dense, "new1.csv")
termDocMatrix <- as.matrix(mydata.dtm3)
termMatrix <- termDocMatrix  %*% t(termDocMatrix)

library(igraph)
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


mydata.dtm3 <- removeSparseTerms(mydata.dtm2, sparse=0.25)
dim(mydata.dtm3)
net2 <- graph_from_incidence_matrix(mydata.dtm3)

table(V(net2)$type)

net2.bp <- bipartite.projection(net2)
as_incidence_matrix(net2)  %*% t(as_incidence_matrix(net2)) 

t(as_incidence_matrix(net2)) %*%   as_incidence_matrix(net2)
plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     vertex.size=7)

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
vertex.size=7)
plot(net2, edge.arrow.size=.4, edge.curved=.1)


plot(net2, edge.arrow.size=.2, edge.curved=0,
vertex.color="orange", vertex.frame.color="#555555", vertex.label.color="black",
vertex.label.cex=.7) 

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g, layout=layout1)





#####

setwd("/Users/subasishdas1/Dropbox/TWT_FB/aircrash")
library(tm)

list.files()
bt_tw1 <- read.csv("ac_5.csv")
dim(bt_tw1)
table(bt_tw1$Year1)
names(bt_tw1)
#### bt_tw2 <- bt_tw1[!duplicated(bt_tw1[,2]),]

all2<- data.frame(Year = unique(bt_tw1$Year1), 
                  Summary = tapply(bt_tw1$Summary, bt_tw1$Year1, 
                                   paste, collapse = ' '))
head(all2)
dim(all2)
names(all2)





library(tm)
mydata.corpus <- Corpus(VectorSource(all2$Summary))
mydata.corpus <- tm_map(mydata.corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
mydata.corpus <- tm_map(mydata.corpus, content_transformer(tolower), mc.cores=1) 
mydata.corpus <- tm_map(mydata.corpus, removePunctuation, preserve_intra_word_dashes=TRUE, mc.cores=1) 
my_stopwords <- c(stopwords('german'),"the", "due", "are", "not", "for", "this", "and", 
                  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than", "set",
                  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "ccc", "end", "have", "avenue", "before", "i-us", "i-e", "i-i-", "ames", "belle", "gen", "okeefe", "one", "just", "mac", "being", "i-i-", "left", "right", "west",  "when","levels","remaining","based", "issues",  "still", "off", "over", "only", "north", "past", "twin", "while",  "i-w" ,  "general" , "harvey", "i-e","i-i-","i-us" , "must", "more", "work","read",  "reached", "morrison",  "mph", "three","info", "canal", "camp", "la-", "approximately",  "amp", "access", "approaching",  "forest", "friday",  "its", "affect", "after", "within", "what", "various", "under", "toward", "san", "other" , "city", "into", "by", "for", "is", "are", "their", "he", "she", "research", "through", "between", "under", "below", "over", "with", "an", "affect", "nowadays", "present", "important", "significant", "then", "using", "having", "via", "vermont", "some", "rap", "how", "can","task","type","next","limit","areas","real-","samll","method","case", "california","canada","used","effect","variable","state","factors","zone","analysis","test","methods")
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords, mc.cores=1)
mydata.corpus <- tm_map(mydata.corpus, removeNumbers, mc.cores=1) 



mydata.dtm <- TermDocumentMatrix(mydata.corpus) 
dim(mydata.dtm)



DTM <- DocumentTermMatrix(mydata.corpus)



library(wordcloud)
term.matrix <- TermDocumentMatrix(mydata.corpus)
term.matrix <- as.matrix(term.matrix)
head(term.matrix)
colnames(term.matrix) <- c("1908-1933", "1934-1959","1960-1984", "1985-2009")
comparison.cloud(term.matrix,max.words=1000, random.order=FALSE)



####

setwd("/Users/subasishdas1/Copy/Rpubs/rpubs/tm_rpubs")
data <- read.csv("ac.csv")
dim(data)
names(data)
## [1] 45933     7
bt_tw2 <- data[!duplicated(data[,2]),]
dim(bt_tw2)
## [1] 15361     7
bt_tw3 <- bt_tw2
## [1] 15357     7
### stm for all documents TITLE
### stm
library(stm)
processed <- textProcessor(bt_tw3$Summary, metadata = bt_tw3, stem=FALSE, customstopwords = c("the", "due","traffic", "crashed", "model","next", "are", "not", "for", "this", "and", "papers","reveiw", "that",  "there", "new", "near", "beyond", "time", "from", "been", "both", "than", "review","subcommittee", "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "ccc", "end", "have",  "avenue", "before", "i-us", "i-e", "i-i-", "ames", "belle", "gen", "okeefe", "one", "just", "mac", "being", "i-i-", "left", "right", "west",  "when","levels","remaining","based", "issues",  "still", "off", "over", "only", "north", "past", "twin", "while",  "i-w" ,  "general" , "harvey", "i-e","i-i-","i-us" , "must", "more", "work","read",  "reached", "morrison",  "mph", "three","info", "canal", "camp", "la-", "approximately",  "amp", 
                                                                                                     "access", "approaching",  "forest", "friday",  "its", "affect", "after", "within", "what", "various", "under", "toward", "san", "other" , "city", "into", "by", "for", "is", "are", "their", "he", "she", "research", "through", "between", "under", "below", "over", "with", "an", "affect", "nowadays", "present", "important", "significant", 
                                                                                                     "then", "using", "having", "via", "vermont", "some", "rap", "how", "can", "inc", "transportation",  "advanced", "applied" , "asphalt", "associates", "association", "authority", "center", "central", "cities", "college",   "commission","company","construction","consultant"  ,    "consultants"  ,    "consulting", "corporation",      "council" ,"county" ,"department","development" , "engineering" ,"group",     "highway", "icf" , "imperial", "inc", "innovation","institute","international",   "kth" , "laboratory" ,"llc" ,"los" ,"ltd" , "metropolitan","ministry","national" ,"old"  , "park"  ,"parsons","planning"   ,"polytechnic" ,"polytechnique"  ,"resource","road" ,"royal", "safety" , "santa","science","state", "systematics","systems","tech","technical","technological",
                                                                                                     "technology"   , "toronto","transit","transport" , "transportation","united" ,"aircraft", "flight", "plane", "airplane", 
                                                                                                     "universidad", "universitat","university","ahd","paper", "ahn", "special","call","ahd",
                                                                                                     "activities","cfp","toledo","geroliminis", "study", "case", "assessment", "analysis", "approach"))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta) 
## Removing 19270 of 40044 terms (19270 of 1221726 tokens) due to frequency 
## Removing 1 Documents with No Words 
## Your corpus now has 15356 documents, 20774 terms and 1202456 tokens.
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

#### 10 topics
docs <- out$documents
poliblogPrevFit <- stm(out$documents, out$vocab, K =10,
                       prevalence=~ Location + Route+ Operator,
                       data = out$meta, init.type = "Spectral")



labelTopics(poliblogPrevFit, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
plot.STM(poliblogPrevFit, type = "summary")




setwd("/Users/michellezupancich/Desktop/R_codes/TRB_TopicModel")

############################################################### ALL YEARS
############ Load packages
library(ggplot2)
library(reshape2)

############ Read file
setwd("/Users/subasishdas1/Dropbox/TWT_FB/aircrash")
a1 <- read.csv("aircrash_words2.csv")
head(a1)

library(reshape2)
a2 <- melt(a1)
### write.csv(a2, "a2.csv")

a2 <- read.csv("a2.csv")
head(a2, 45)

gg <- gg + coord_equal()+
  + scale_x_continuous(breaks = c(1908, 1918, 1928, 1938, 1948,
                                  1958, 1968, 1978, 1988, 1998,
                                  2009))


library(ggplot2)
gg <- ggplot(a2, aes(x=variable, y=Pairword, fill=value))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + labs(x="Committee Name", y=NULL, title=" ")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=12))
gg <- gg + theme(legend.title=element_text(size=12))
gg1 <- gg + theme(legend.text=element_text(size=12))+theme_bw()
gg1