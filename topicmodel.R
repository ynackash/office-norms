---
  title: "Topic Modeling"
output: html_notebook
---
  
  ```{r}

#install.packages('stm')
#install.packages('stmBrowser')
#install.packages('tm')

library(stm)
#library(stmBrowser)
library(tm)

#Import dataset
filename = ""
data = read.csv(filename,header=TRUE,sep=",")

#SAMPLE DATA IF NEEDED
#data <- data[sample(1:nrow(data), 300,replace=FALSE),]

data = data[!duplicated(data$Response), ]
#get rid of columns where for ALL rows the value is NA:
data = data[,colSums(is.na(data))<nrow(data)]

#when you load data, first column name is pre-pended with i
#data$Author <- data$ï..Author

#Company, Date, Pros, Cons
names(data)
str(data)

data$text = data$Response
data$text = as.character(data$text)
data$text=sapply(data$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

# ------------------------------------------------------------------------------
#  Prepare documents
# ------------------------------------------------------------------------------
# Stem, tokenize, stopwords, ...


# custom stopword list

filename2 = ""
custom_stopwords = read.csv(filename2,header=TRUE,sep=",")

#View(custom_stopwords)

custom_removal_words = custom_stopwords
custom_removal_words$stopwords = as.character(custom_removal_words$stopwords) 


processed = textProcessor(data$text,
                          lowercase = TRUE, 
                          removestopwords = TRUE,
                          customstopwords = custom_removal_words$stopwords,
                          removenumbers = TRUE,
                          removepunctuation = TRUE, 
                          wordLengths = c(3,Inf),
                          stem = TRUE,
                          metadata = data)


#Remove words that are too frequent if needed
plotRemoved(processed$documents, lower.thresh=seq(from = 10, to = 1000, by = 10))
#plotRemoved(processed$documents, lower.thresh=seq(from = 1, to = 1, by = 1))


# Prepare documents for analysis
out = prepDocuments(processed$documents, processed$vocab, processed$meta)
docs  = out$documents
vocab = out$vocab
meta = out$meta


# ------------------------------------------------------------------------------
#  Find optimum number of topics
# ------------------------------------------------------------------------------

# Grid search
n_topics = seq(from = 2, to = 3, by = 1)

storage = searchK(out$documents, out$vocab, K = n_topics, #prevalence =~ state + s(day), max.em.its = 75,
                  data = meta)


plot(storage)
print(storage)

# Select the best number of topics that maximizes both exclusivity and semantic coherence
# Top right quadrant represents balance of this two metrics - ideal number of topics should exist close to here
{plot(storage$results$semcoh, storage$results$exclus, main='Choosing the Number of Topics', bty="n", bg = storage$results$K, pch = 21, type = "p", xlab = "Semantic Coherence", ylab = "Exclusivity")
  text( storage$results$semcoh, storage$results$exclus, labels=storage$results$K, cex= 0.9, pos = 2)}

# SET # OF TOPICS
n_topics = 2

# ------------------------------------------------------------------------------
#  Model selection
# ------------------------------------------------------------------------------

choose_model = selectModel(out$documents, out$vocab,
                           K           = n_topics,
                           #prevalence  = ~ out$meta$state + s(as.numeric(out$meta$day)),
                           data        = meta,
                           runs        = 20,
                           max.em.its = 500,
                           seed        = 2)


choose_model
plotModels(choose_model)

#SET THE MODEL #
model_fit = choose_model$runout[[2]]


# ------------------------------------------------------------------------------
#  Prevalance
# ------------------------------------------------------------------------------
sageLabels(model_fit, n=10)
labelTopics(model_fit, n=10)

# ------------------------------------------------------------------------------
# Visualizing Topic Proportions in Aggregate and at the Document Level
# ------------------------------------------------------------------------------

# Document Level Topic Proportions [associated with all metadata] (visualizations?)
#---------------------------------------------------------------------------------------------------------
#model_fit$theta provides document-topic loadings
#make.dt function combines the document-topic loadings (theta) with metadata to create a data.table object 
doc_top_loadings = make.dt(model_fit, meta = meta)
doc_top_loadings

#create factor variable that assigns a single topic to each row based on the proportions provided
doc_top_loadings$Topic_Assgn = as.factor(max.col(doc_top_loadings[, 2:(n_topics+1)]))

write.csv(doc_top_loadings,"", row.names=FALSE)


#visualize distribution of document-topic proportions
#custom topic names should be provided using "labeltype = custom" and "custom.labels = c("lack of innovation", "culture", etc.)"
plot(model_fit, type = "hist")

# Aggregate Topic Proportions (visualizations - alternative to a pie chart?)
#---------------------------------------------------------------------------------------------------------
#calculate aggregate topic proportions using table function - compare to plot above (approx. line 122)
aggregate_topic_proportions = round(prop.table(table(doc_top_loadings$Topic_Assgn)), 3)
sum(aggregate_topic_proportions) # should equal 1

#the gephi visualization proportions will be slightly off b/c we're taking a 1K sample of documents
aggregate_topic_proportions

#ESTIMATED topic proportion plot
#should replace with ACTUAL topic proportion plot constructured from aggregate_topic_proportion table
plot(model_fit, type = "summary")


# ------------------------------------------------------------------------------
# Topic Quality
# ------------------------------------------------------------------------------

#plot quality of topic within each model
#top right quadrant representative of high quality topics
topicQuality(model=model_fit, documents=docs, main = "Quality of Each Topic Within Model")

# ------------------------------------------------------------------------------
# Topic Labels & Word Clouds per Topic
# ------------------------------------------------------------------------------

# List of words associated with the topic
sageLabels(model_fit, n=10)
labelTopics(model_fit, n=10)

#top words associated with each topic
plot(model_fit, type = "labels", main = "Sage Labels Alternative", width = 90)


#write.csv(doc_top_loadings, "doc_top_loadings.csv", row.names = FALSE)


#ADD ROW NAMES, WHICH ARE TOPIC ASSIGNMENTS (can color graph by topic using this later)
topic_docs = doc_top_loadings[,2:(n_topics+1)]
#topic_docs = topic_docs[1:1218,]
row.names(topic_docs) = paste0("doc_", doc_top_loadings$docnum) 

#calculate Euclidean distances between documents based on topic proportions
library(cluster)
# DAISY FUNCTION - Compute all the pairwise dissimilarities (distances) between observations in the dataset
# 996 rows by 996 columns in 6 company ex
topic_df_dist = as.matrix(daisy((topic_docs), metric = "euclidean", stand = TRUE))
#colnames and rownames are the same as expected
#length(intersect(rownames(topic_df_dist), colnames(topic_df_dist)))

#subset the distances to keep only ones below a certain threshold to avoid giant dense blob network of documents
topic_df_dist[sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] = 0




#### network diagram using Fruchterman & Reingold algorithm (Jockers uses the ForceAtlas2 algorithm which is unique to Gephi)
install.packages("igraph")
library(igraph)

g = as.undirected(graph.adjacency(topic_df_dist, weighted = TRUE))

vertex_attr(g, "topic_assign", index = V(g)) <- doc_top_loadings$Topic_Assgn

layout1 = layout.fruchterman.reingold(g, niter=500)

plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1,  vertex.color= "grey", edge.arrow.size = 0.1, vertex.label.dist=0.5, vertex.label = NA)

#write file for visualization in gephi
write.graph(g, file="", format="graphml")

dir()

COMBINED_node_file = COMBINED.Nodes#read.csv("COMBINED-Nodes.csv", header=TRUE, sep=",") 

library(sqldf)

#change docnum ROW names in top_n_reviews
doc_top_loadings$docnum_revised = paste0("doc_", doc_top_loadings$docnum)

#join top_n_reviews and gephi node file
topic_assgn_joined = sqldf("SELECT * FROM COMBINED_node_file INNER JOIN doc_top_loadings ON v_name == docnum_revised")

#topic_assgn_joined = sqldf("SELECT * FROM topic_assgn_joined WHERE Topic_Assgn != 4 AND Topic_Assgn != 5 ")

#write new node file with metadata for enhanced visualization in Gephi
write.csv(topic_assgn_joined, "", row.names = FALSE)


```

