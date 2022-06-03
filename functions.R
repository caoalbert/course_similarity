library(reticulate)


#install and import required python packages
conda_create("r-reticulate")
conda_install("r-reticulate", "pandas")
conda_install("r-reticulate", "numpy")
conda_install("r-reticulate","gensim")
conda_install("r-reticulate","Levenshtein",pip = TRUE)

pd <- import("pandas")
np <- import("numpy")
re <- import("re")
gensim <- import("gensim")
Doc2Vec <- gensim$models$doc2vec$Doc2Vec
Phrases <- gensim$models$phrases$Phrases
Phraser <- gensim$models$phrases$Phraser
lev <- import("Levenshtein")

# use python source code
source_python("functions.py")
parsed$lecture <- as.integer(parsed$lecture)

# test case using no filter different spellings (same results for all)
course_find_similar("POL SCI 0251")#, num_show = 4L)
course_find_similar("POLITICAL SCIENCE 251", num_show = 4L)
course_find_similar("POLT SCI 251", num_show = 4L)

# test cases using different filters
course_find_similar("POL SCI 0251", class_type= "lecture", num_show = 4L)
course_find_similar("POL SCI 0251", num_show = 4L)
course_find_similar("POL SCI 0251", class_type= "lecture",career_lvl= "Undergraduate",hrs=TRUE)
course_find_similar("POL SCI 0251", career_lvl= "Undergraduate", impacted = "True")

# test case using phrase function
#phrase_find_similar("epidemiology")

setwd( "/Users/huqinhan/Desktop/shiny")
