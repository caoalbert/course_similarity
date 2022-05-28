##### Functions for user input

import pandas as pd
import numpy as np
import re
import gensim
from gensim.models.doc2vec import Doc2Vec
from gensim.models.phrases import Phrases, Phraser
import Levenshtein as lev


# import preprocessed data set, add column with collapsed subject area and course number
parsed = pd.read_csv("parsed_revisedv2.csv")
parsed_coursenum = pd.read_csv("parsed_coursenum.csv")
parsed_coursenum = parsed_coursenum[~parsed_coursenum["extra_clean"].isnull()]
parsed_coursenum = parsed_coursenum.reset_index(drop= True)
parsed["subj_cat"] = parsed_coursenum["course_num"]

# import department similarity matrix, add column that specifies which department each is compared to vertically
dep = pd.read_csv("dep.csv")
#prereq = pd.read_csv("parsed_prereq.csv")
uniq_dep = []
for i in range(len(dep)):
    uniq_dep.append(dep.columns[i])
dep["dept"] = uniq_dep
dep.to_csv("dep.csv")

# load model
model = Doc2Vec.load("final_similarity.model")
phrases = Phrases(parsed["processed_desc"])
bigram = Phraser(phrases)

### Function 1

def course_find_similar(subj_cat, prereq = None, prereq2 = None, prereq3 = None, class_type = None,grade_type=None, 
  hrs = False, dept = False, career_lvl = None, impacted = "False", num_show = 10):
    # Function inputs a subject area code and catalog number, and optional filters
    # Outputs top (10, or less if length after filters is less than 10) courses and catalog number, along with similarity scores in form of list

    # Filter options:
    # class_type: lecture, discussion...etc (shows classes with specified class type), None: no filter on class type applied
    # hrs: True (only show class with same number of hours as input class), False: no filter on hours applied
    # dept: True (re order output according to department relevance), False: show natural order
    # career_lvl: "Undergraduate", "Graduate", "Law", "Dentistry" (shows classes with specified career level), None: no filter on career level applied
    # impacted: "True" (Only show non-impacted course), "False": no filter on impacted applied
    # Default: no filters applied
    # num_show: how many top similarity courses to show, default is 10

    # find the document id corresponding to input subj code and catalog number
    input_sim = []
    for i in range(len(parsed)):
        input_sim.append(lev.ratio(subj_cat.lower(), parsed.loc[i, "subj_cat"].lower()))
    id = input_sim.index(np.max(input_sim))

    # rank all documents from most similar to least
    similar = model.dv.most_similar(id, topn=len(parsed))
    # get list of just indexes in order of most similar
    rank = []
    scores = []
    for i in range(len(parsed) - 1):
        rank.append(similar[i][0])
        scores.append(similar[i][1])

    # create new df where rows are re-ordered according to similarity, re index after
    ranked_parsed = parsed.reindex(rank)
    ranked_parsed["Similarity Score"] = scores
    ranked_parsed = ranked_parsed.reset_index(drop = True)
    
    

    # filter class type
    if class_type is not None:
    
      if isinstance(class_type, str):
        
        ranked_parsed = ranked_parsed[ranked_parsed[class_type] == 1]
      
      elif len(class_type) > 1:
        class_type = list(class_type)
        temp = ranked_parsed[class_type].sum(axis = 1)
        ranked_parsed = ranked_parsed[temp>=1]
        
        
        

    # filter by grade  type
    if grade_type is not None:
            
        if isinstance(grade_type, str):
          ranked_parsed = ranked_parsed[ranked_parsed["crs_grd_typ_cd"] == grade_type]
              
        elif len(grade_type) > 1:
          grade_type = list(grade_type)
          ranked_parsed=ranked_parsed [ranked_parsed["crs_grd_typ_cd"].isin(grade_type)]


      
    # filter by prereq
    if prereq is not None:
       
    
        if prereq2 is not None and prereq3 is None:
          #prereq = "&".join([prereq, prereq2])
          prereq = "(?=.*" + prereq + ")(?=.*" + prereq2 +")"
        elif prereq2 is not None and prereq3 is not None:
          #prereq = "&".join([prereq, prereq2, prereq3])
          prereq = "(?=.*" + prereq + ")(?=.*" + prereq2 +")" + "(?=.*" + prereq3 +")"
        
        test = ranked_parsed[ranked_parsed['pre_req'].str.contains(prereq, na=False)]
        if not test.empty:
            ranked_parsed = ranked_parsed[ranked_parsed['pre_req'].str.contains(prereq, na=False)]
            
    # filter hours
    if hrs == True:
        course_hrs = parsed.loc[id, "hours"]
        ranked_parsed = ranked_parsed[ranked_parsed["hours"] == course_hrs]

    # filter career levels
    if career_lvl is not None:
        # create dictionary to map user input to career level code
        career_dic = {"Undergraduate" : "U",
               "Graduate" : "G",
               "Law" : "L",
               "Medicine" : "M",
               "Dentist": "D"}
        if isinstance(career_lvl, str):
          ranked_parsed = ranked_parsed[ranked_parsed["crs_career_lvl_cd"] == career_dic[career_lvl]]
        elif len(career_lvl) > 1:
          newlist = []
          for i in career_lvl:
            newlist.append(career_dic[i])
          ranked_parsed = ranked_parsed[ranked_parsed["crs_career_lvl_cd"].isin(newlist)]

    # filter out impacted courses
    if impacted == True:
        ranked_parsed = ranked_parsed[ranked_parsed["impacted_crs_fl"] == "N"]

    # reindex after filtered out courses are dropped from data frame
    ranked_parsed = ranked_parsed.reset_index(drop = True)

    # create final output list of top ten courses (or all top course if less than ten) after filtering
    sim_courses = []
  

    # reorder according to similarity if specified
    if dept == True:
        course_dept = parsed.loc[id, "sr_dept_cd"]
        sim_col = dep.loc[:, [course_dept, "dept"]]
        if num_show != 14499:
            if len(ranked_parsed) > num_show:
                ranked_parsed = ranked_parsed.loc[0:(num_show - 1), ]
            ranked_parsed = ranked_parsed.merge(sim_col, left_on = "sr_dept_cd", right_on = "dept", how = "left")
            ranked_parsed = ranked_parsed.sort_values(course_dept, ascending = False)
            ranked_parsed = ranked_parsed.reset_index(drop = True)
            for i in range(len(ranked_parsed)):
                sim_courses.append(ranked_parsed.loc[i, "subj_cat"])
        else:
            ranked_parsed = ranked_parsed.merge(sim_col, left_on="sr_dept_cd", right_on="dept", how="left")
            ranked_parsed = ranked_parsed.sort_values(course_dept, ascending=False)
            ranked_parsed = ranked_parsed.reset_index(drop=True)
            for i in range(len(ranked_parsed)):
                sim_courses.append(ranked_parsed.loc[i, "subj_cat"])
    if dept == False:
        if len(ranked_parsed) < num_show:
            for i in range(len(ranked_parsed)):
                sim_courses.append(ranked_parsed.loc[i, "subj_cat"])
        else:
            for i in range(num_show):
                sim_courses.append(ranked_parsed.loc[i, "subj_cat"])


    return sim_courses

# test case using only num_show filter (all gives same results)
course_find_similar("POL SCI 251", prereq = "STATS 100A", prereq2 = "STATS 102A", prereq3 = "STATS 100C", num_show = 5)
course_find_similar("POL SCI 251", prereq = "STATS 100A", num_show = 5)
course_find_similar("POLT SCI 251", num_show = 5)

course_find_similar("POLT SCI 251", class_type= ["lecture", "discussion"], num_show = 5)
course_find_similar("POLT SCI 251", class_type= "lecture", num_show = 5)

# test cases using different filters
course_find_similar("POL SCI 251",  hrs = True)
course_find_similar("POL SCI 251", dept=True)
course_find_similar("POL SCI 251",  dept=True, hrs = True, career_lvl= ["Law"], impacted = "False", num_show=36)
#grade_type=None
course_find_similar("POL SCI 251",  dept=True, hrs = True, career_lvl= ["Law"], impacted = "False", num_show=36,grade_type="SO")
course_find_similar("POL SCI 251",  dept=True, hrs = True, career_lvl= ["Law"], impacted = "False", num_show=36,grade_type=["SO","LG"])

### Function 2
def phrase_find_similar(phrase):
  
    # # Function input phrase (NO filters)
    # # Outputs top ten courses most similar to phrase (subject area code, catalog number, similarity sore) in form of list
    # 
    # #tokenize input phrase and remove stop words
    # phrases = []
    # for token in gensim.utils.simple_preprocess(phrase):
    #     if token not in gensim.parsing.preprocessing.STOPWORDS:
    #         phrases.append(token)
    # bigram_doc = bigram[phrases]
    # 
    # #find most similar courses
    # similar = model.dv.most_similar(positive=[model.infer_vector(bigram_doc)], topn=len(parsed))
    # rank = []
    # scores = []
    # for i in range(len(parsed) - 1):
    #     rank.append(similar[i][0])
    #     scores.append(similar[i][1])
    # 
    # # create new df where rows are re-ordered according to similarity, re index after
    # ranked_parsed = parsed.reindex(rank)
    # ranked_parsed["Similarity Score"] = scores
    # ranked_parsed = ranked_parsed.reset_index(drop=True)
    # 
    # # create final output list of top ten courses
    # sim_courses = []
    # for i in range(10):
    #     sim_courses.append(ranked_parsed.loc[i, "subj_cat"])
    #     #+ "; Similarity Score: " +
    #                 #str(ranked_parsed.loc[i, "Similarity Score"])
    
    
    phrase = phrase + " "
    descriptions = parsed_coursenum['clean']
    boolean_findings = descriptions.str.contains(phrase, flags = re.IGNORECASE)
    sim_courses = parsed_coursenum[boolean_findings]['course_num'].tolist()
    
    
                          
    return sim_courses

# test case
phrase_find_similar("linear model")



