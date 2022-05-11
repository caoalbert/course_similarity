# would not recommend running (might try to retrain models)
# just for reference

library(reticulate)

py_install("pandas")
py_install("numpy")
py_install("re")
py_install("gensim")
py_install("nltk")

pd <- import("pandas")
np <- import("numpy")
re <- import("re")
gensim <- import("gensim")
Doc2Vec <- gensim$models$doc2vec$Doc2Vec
TaggedDocument <- gensim$models$doc2vec$TaggedDocument
Phrases <- gensim$models$phrases$Phrases
Phraser <- gensim$models$phrases$Phraser
nltk <- import("nltk")
WordNetLemmatizer <- nltk$stem$WordNetLemmatizer
SnowballStemmer <- nltk$stem$SnowballStemmer
random <- import("random")
sample <- random$sample

source_python("similarity.py")