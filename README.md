# Case_Study_Presentation
Materials from case study presentation. 

<b>demographic_maps.R </b>

R script that creates both static and interactive maps of US and China showing the origins of speakers recruited for the deception experiment. 

Output of demographic_maps.R
- USmap.png
- Chinamap.png
- speaker_map.html : interactive map widget, open in web browser

<b>deception_classification.py</b>
- Example call: python3 deception_classification.py --seg question_chunk --features acoustic --clf lr --k 200 --sensitivity false --qnum false --interactions false --individual false --out classifier_test.csv

Python script that takes in arguments for classification experiment and performs 10-fold cross-validation classification experiments with the choice of Naive Bayes, SVM, Random Forest, and Logistic Regression models from scikit-learn library. 
It retrieves pre-extracted features (acoustic, lexical, syntactic) and labels (Statement was True/False)  from answers to questions in the deception experiment. Prior to classification, it performs feature selection of up to k features specified on input to script (--k) to avoid curse of dimensionality and overfitting. Classification model is evaluated using 10-fold cross validation, with unique
speakers in each fold. The script retrieves a pre-divided folds of data to ensure consistency.

Output of deception_classification.py:
- Ongoing progress and results printed to screen
- Saves output to csv file specified on input to script (--out)
- classifier_test.csv : example output file 


