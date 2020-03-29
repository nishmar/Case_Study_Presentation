# This script is used for deception classification.  
# -> compares feature sets
# -> compares all segmentations - IPU, turn, question response, question chunk
# -> compares RF, LR, SVM, NB classifiers
# -> saves results to csv for easy analysis

from sklearn import metrics
from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import MultinomialNB
from sklearn.naive_bayes import GaussianNB
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import Normalizer
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import VotingClassifier
from sklearn.model_selection import KFold,StratifiedKFold
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import Imputer
from sklearn.feature_selection import SelectKBest, f_classif
import os
import numpy as np
import csv
import pickle
import argparse
from sklearn.svm import LinearSVC
from sklearn import preprocessing
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)




FOLDS_PATH = "/local/users/deception/full_corpus/classification/folds"

segmentation_options = ['ipu','turn','question_response','question_chunk']
#feature_options = ['IS09','liwc','lexical','IS09_aggregated','complexity','embedding','praat15']
feature_options = ['acoustic', 'lexical', 'syntactic']
clf_options = ['rf','lr','svc','nb', 'svc_nonlinear']
pickled_features_path = "/local/users/deception/full_corpus/classification/pickled_features"

sensitivity_options= ['sensitive','qnum','all', 'none']
interaction_options= ['sensitivity','qnum','all', 'none']
individual_options= ['gender', 'language', 'personality', 'all', 'none']


def process_args():
    parser = argparse.ArgumentParser(description="Run deception classification experiments.")
    parser.add_argument('--seg',help='segmentation unit for classification.'  ,required=True, choices=segmentation_options )
    parser.add_argument('--features',nargs='+',help='which feature(s) to use for classification.',required=True, choices=feature_options )
    parser.add_argument('--clf',help='which classification model to use for classification.',required=True, choices=clf_options )
    parser.add_argument('--k',help='how many top features to select. use "all" to select all features.',required=True )
    parser.add_argument('--out',help='file to write results to - if exists will append to file.',required=True )
    parser.add_argument('--note',help='add note about exper - will be saved to output.' )

    parser.add_argument('--sensitivity', help='include binary sensitivity feature and question number feature', choices=sensitivity_options)
    parser.add_argument('--interactions', help='compute sensitivity interactions', choices=interaction_options)
    parser.add_argument('--individual', help='include individual features: gender, language, personality', choices=individual_options)
    
    args = parser.parse_args()
    return args.seg, args.features, args.clf, args.k, args.out, args.note, args.sensitivity, args.interactions, args.individual

# read 10 folds for cross-validation
def read_folds(segmentation):
    split_type='speaker'
    folds_files = os.listdir(os.path.join(*[FOLDS_PATH,segmentation,split_type]))
    folds = []
    for ff in folds_files:
        lines = [line.strip() for line in open(os.path.join(*[FOLDS_PATH,segmentation,split_type,ff]))]
        folds.append(lines)
    return(folds)

def aggregate_folds(fold_features, i):
    train_folds = fold_features[:i] + fold_features[i+1:]
    train_x = np.concatenate([tf[0] for tf in train_folds])
    train_y = np.concatenate([tf[1] for tf in train_folds])
    return train_x, train_y

def get_metrics(gold, predicted):
    acc=np.mean(predicted==gold)
    all_metrics = metrics.precision_recall_fscore_support(gold, predicted)
    precision_f = all_metrics[0][0]
    precision_t = all_metrics[0][1]
    recall_f = all_metrics[1][0]
    recall_t = all_metrics[1][1]
    f1_f = all_metrics[2][0]
    f1_t = all_metrics[2][1]
    avg_p=np.mean([precision_f, precision_t])
    avg_r=np.mean([recall_t, recall_f])
    avg_f1=np.mean([f1_t, f1_f])

    metrics_labels = ['acc', 'precision_f','precision_t', 'recall_f', 'recall_t', 'f1_f', 'f1_t','avg_p', 'avg_r', 'avg_f']
    metrics_values = [acc, precision_f, precision_t, recall_f, recall_t, f1_f, f1_t, avg_p, avg_r, avg_f1]

    return metrics_labels, metrics_values

def write_results(exper_settings, out, metrics_labels, metrics_mean, metrics_std):
    header = ['seg','features','clf','k']
    for ml in metrics_labels:
        header.append('%s_mean'%ml)
        header.append('%s_std'%ml)
    header.append('note')
    write_header = True
    if os.path.exists(out): #don't need header
        write_header = False
    with open(out, 'a+') as f:
        if write_header:
            f.write(','.join(header)+'\n')
        out_line = []
        for h in header[:4]:
            if h == 'features':
                out_line.append(':'.join(exper_settings[h]))

            else:
                out_line.append(exper_settings[h])
        for m,s in zip(metrics_mean, metrics_std):
            out_line.append(m)
            out_line.append(s)
        note = exper_settings['note']
        if note is not None:
            out_line.append(note)
        out_line = [str(s) for s in out_line]
        f.write(','.join(out_line)+'\n')
 
        
        
        
        
        


def cross_validation(fold_features, clf_name, idx_selected):
    if clf_name == 'rf':
        clf = RandomForestClassifier(n_estimators=100, random_state=0)
    elif clf_name == 'lr':
        clf = LogisticRegression(random_state=45)
    elif clf_name == 'svc':
        clf = LinearSVC(random_state=45, dual=False)
    elif clf_name == 'nb':
        clf = GaussianNB()
    elif clf_name == 'svc_nonlinear':
    	clf = SVC()


    #force sensitivity into idx selected
    sensitivity=False
    if sensitivity:
    	last_qnum_idx = fold_features[0][0].shape[1]-1
    	sensitivity_idx = last_qnum_idx-25

    	#print(fold_features[0][0][:,qnum_idx])
    	#print(fold_features[0][0][:,sensitivity_idx])

    	for i in range(sensitivity_idx,last_qnum_idx):
    		idx_selected= np.append(idx_selected, i)
    
    

    all_folds_metrics = []
    print("->training model - 10 fold cross validation")
    for i in range(len(fold_features)):
        test_x, test_y = fold_features[i]
        train_x, train_y = aggregate_folds(fold_features, i)
        test_x = test_x[:,idx_selected]
        train_x = train_x[:,idx_selected]

        print(train_x.shape)
        clf.fit(train_x, train_y)
        predicted = clf.predict(test_x)
        metrics_labels, metrics_values = get_metrics(test_y, predicted)
        all_folds_metrics.append(metrics_values)

    all_metrics_np = np.asarray(all_folds_metrics)
    mean_metrics = np.mean(all_metrics_np, axis=0)
    std_metrics = np.std(all_metrics_np, axis=0)

    return metrics_labels, mean_metrics, std_metrics


def get_single_fold_single_feature_np(feature, fd, fold):
    unique_speakers = set(fname.split(':')[0] for fname in fold)
    fold_features = []
    fold_labels = []

    qnum_feature_np=[]

    for i,speaker in enumerate(unique_speakers):
        speaker_fnames = [f for f in fold if f.startswith(speaker)]
        speaker_labels = [f.split(':')[-1].rstrip('.txt') for f in speaker_fnames] #T/F labels
        speaker_features = [fd[f][feature] if f in fd.keys() else None for f in speaker_fnames]

        #get question number
        question_num= [f.split(':')[3] for f in speaker_fnames]
        sensitive= ['q5','q12','q13','q14','q15','q16','q23','q24']
        sensitivity_feature = [1 if q in sensitive else 0 for q in question_num]
        

        #One-hot encode question num feature
        
        #qnum_feature = [float(q.lstrip('q')) for q in question_num] 
        le= preprocessing.LabelEncoder()

        #not all question chunks present for all speakers, but encoding must contain all 24
        qlist= ['q1','q2','q3','q4','q5','q6','q7','q8','q9','q10','q11','q12','q13','q14','q15','q16','q17','q18','q19','q20','q21','q22','q23','q24']
        le.fit(qlist) 
        qnum_feature = np.array(question_num)
        qnum_feature= le.transform(qnum_feature)
        enc = preprocessing.OneHotEncoder(sparse=False, n_values=24)
        
        qnum_feature = qnum_feature.reshape(len(qnum_feature), 1)

        #print(qnum_feature)
        #print(len(qnum_feature))
        #print('le.classes_')
        #print(le.classes_)

        enc.fit(qnum_feature)
        qnum_feature = enc.fit_transform(qnum_feature)

        assert(len(speaker_features) == len(speaker_labels) == len(speaker_fnames) == len(sensitivity_feature))

        # remove any None elements from both features and labels arrays
        speaker_features_labels = [(a,b, c,d) for a,b,c,d in zip(speaker_features, speaker_labels, sensitivity_feature, qnum_feature) if a is not None]
        speaker_features = [a for a,b,c,d in speaker_features_labels]
        speaker_labels = [b for a,b,c,d in speaker_features_labels]
        sensitivity_feature = [c for a,b,c,d in speaker_features_labels]
        qnum_feature= [d for a,b,c,d in speaker_features_labels]

        speaker_features_np = np.asarray(speaker_features, dtype=float)

        #impute nan features per speaker
        #first prevent col of all NaNs from being removed - replace with zeroes
        for i in range(speaker_features_np.shape[1]): #for each col
            if np.isnan(speaker_features_np[:,i]).all():
                speaker_features_np[:,i] = 0.
        imp = Imputer(missing_values = "NaN", strategy='mean',axis=0)
        speaker_features_np = imp.fit_transform(speaker_features_np)

        #Normalizing features by speaker
        if feature in ['lexical','liwc','IS09','praat15','complexity']: #do not scale ngram features - txt, pos, prod_rule
            scaled_features_np = (speaker_features_np - np.mean(speaker_features_np,axis=0))/np.std(speaker_features_np,axis=0)
            #replace nan with 0 - nans are a result of divide by zero
            scaled_features_np = np.nan_to_num(scaled_features_np)
            speaker_features_np = scaled_features_np

        
        ## FIX : ONLY ADD IN ONCE , at the end 
        # if feature is the last feature type in set 
        #'txt' for lexical
        #'praat15' for acoustic
        #'grand_prod_rules_unlex' for syntactic
        #
        #Adding sensitivity features after normalization
        sensitivity_feature_np = np.asarray(sensitivity_feature, dtype=float)
        sensitivity_feature_np = np.reshape(sensitivity_feature_np, (len(sensitivity_feature_np),1)) #make col vector
        
        #qnum_feature_np= np.asarray(qnum_feature, dtype=float)
        #qnum_feature_np = np.reshape(qnum_feature_np, (len(qnum_feature_np),1)) #make col vector
        qnum_feature_np = np.asarray(qnum_feature, dtype=float)

        # print('speaker_features_np.shape:')
        # print(speaker_features_np.shape)

        # print('sensitivity_features_np.shape:')
        # print(sensitivity_feature_np.shape)

        add_sens = False
        if add_sens:
        	speaker_features_np = np.append(speaker_features_np, sensitivity_feature_np, 1) #add sensitivity to features
        

        add_qnum= False
        if add_qnum:
        	speaker_features_np = np.append(speaker_features_np, qnum_feature_np, 1) #add qnum to features
        

        #manually add interactions
        add_interactions = True
        if add_interactions:
        	#compute interactions with all features- multiply sensitivity col vector with speaker_features
        	interactions_np = sensitivity_feature_np*speaker_features_np
        	#concat interactions
        	speaker_features_np = np.append(speaker_features_np, interactions_np)
        
        fold_features.append(speaker_features_np) #list of np arrays
        fold_labels.append(speaker_labels)


    features_np = np.concatenate(fold_features) #concat list of arrays into matrix
    labels_np = np.concatenate(fold_labels)

    return features_np, labels_np


# given a list of features, feature dict, and a fold with a list of speakers,
# retrieve the features for that list of speakers
# optional scale - speaker normalization of the features
def get_single_fold_features_np(features_list, features_dict, fold):
    fold_all_features = []
    for feature in features_list:
        fold_feature, fold_labels = get_single_fold_single_feature_np(feature, features_dict, fold)
        fold_all_features.append(fold_feature)
    # can explore other methods of combining. here we do simple concatenation.
    fold_all_features_combined = np.concatenate(fold_all_features, axis=1)

    #print('fold_all_features_combined: ')
    #print(fold_all_features_combined.shape)

    return fold_all_features_combined, fold_labels 


def get_folds_features(features_list, features_dict, folds):
    folds_features = []
    for i in range(len(folds)):
        fold_i_features, fold_i_labels  = get_single_fold_features_np(features_list, features_dict, folds[i])
        folds_features.append((fold_i_features, fold_i_labels))
    return folds_features



def load_features(seg):
    pickled_features_file = os.path.join(pickled_features_path, seg+"_features.p")   
    print("loading features for %s segmentation from file %s"%(seg, pickled_features_file ))
    features_dict = pickle.load(open(pickled_features_file,'rb'))
    print("loaded features dictionary with %s entries"%len(features_dict.keys()))
    return features_dict

# given a feature set, return the feature names to be used
def get_features_from_set(feature_set, seg):
    all_features = []
    for fs in feature_set:
        if fs == 'acoustic':
            all_features += ['IS09', 'praat15']
        elif fs == 'lexical':
            all_features += ['lexical', 'liwc', 'txt']
        elif fs == 'syntactic':
            all_features += ['complexity']
            if seg == 'question_response' or seg == 'question_chunk':
                all_features += ['pos', 'uni_pos', 'prod_rules_lex', 'prod_rules_unlex', 'grand_prod_rules_lex', 'grand_prod_rules_unlex']
    return all_features

def select_features(folds_features, num_features):
    # get X and y for full corpus
    X = np.concatenate([tf[0] for tf in folds_features])
    y = np.concatenate([tf[1] for tf in folds_features])

    # select k best features
    if num_features=='all':
        num_features = X.shape[1]
    else:
        num_features = int(num_features)
    print("selecting %s best features"%num_features)
    selector = SelectKBest(f_classif, k=num_features) #TODO: tune this param?
    selector.fit(X, y)
    idx_selected = selector.get_support(indices = True)


    return idx_selected    


def main():
    seg, features, clf, k, out, note, sensitivity, interactions, individual = process_args()
    exper_settings = {'seg':seg, 'features':features,'clf':clf, 'k':k, 'note':note, 'sensitivity':sensitivity, 'interactions':interactions, 'individual':individual}

    all_features = get_features_from_set(features, seg)
    print("using the following features: %s"%all_features)

    features_dict = load_features(seg)
    folds = read_folds(seg)
    folds_features = get_folds_features(all_features, features_dict, folds)


    print(folds_features[0][0].shape)


    #folds_features is a nested list of numpy matrices 
    #print(folds_features[0][0][0])


    # free up memory
    del features_dict
    del folds

    # feature selection
    idx_selected = select_features(folds_features, k)
    print(idx_selected.shape)

    metrics_labels, mean_metrics, std_metrics = cross_validation(folds_features, clf, idx_selected)   
    for i,j,k in zip(metrics_labels, mean_metrics, std_metrics):
        print(i,j*100,k)
        
    write_results(exper_settings,out, metrics_labels, mean_metrics, std_metrics)

if __name__ == "__main__":
        main()

