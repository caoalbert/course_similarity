# -*- coding: utf-8 -*-
"""
Calculate department similarity scores
"""

import spacy
import wmd
from spacy.language import Language
import pandas as pd
import numpy as np

nlp = spacy.load('en_core_web_md')

@Language.component('my_component')
def my_component(doc):
    return wmd.WMD.SpacySimilarityHook(nlp)

nlp.add_pipe('my_component', last = True)
nlp = spacy.load('en_core_web_md')

doc1 = nlp("Politician speaks to the media in Illinois.")
doc2 = nlp("The president greets the press in Chicago.")
print(doc1.similarity(doc2))

dpt_descrip = pd.read_csv("dpt_descrip.csv", index_col=[0])

mat_sim = np.repeat(1, dpt_descrip.shape[0])
mat_sim = np.diag(mat_sim)
mat_sim = pd.DataFrame(mat_sim)

dept_names = dpt_descrip['departments'].tolist()
mat_sim.columns = dept_names

for i in dpt_descrip['num']:
    ref = dpt_descrip['fields_study'][i]
    checks = dpt_descrip['fields_study']
    
    for j in np.array(range(1,mat_sim.shape[0]+1)):
        try:
            doc1 = nlp(ref)
            doc2 = nlp(checks[j])
            mat_sim.iat[j-1, i-1] = doc1.similarity(doc2)
        except:
            mat_sim.iat[j-1, i-1] = 0

mat_sim.to_csv("dep.csv", encoding='utf-8', index=False)

