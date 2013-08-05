import pandas as pd
import numpy as np

# first import the data
train = pd.read_csv('train.csv')
test = pd.read_csv('test.csv')

# then import a bunch of sklearn classifiers and methoda
from sklearn import svm
from sklearn.cross_validation import cross_val_score
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn import preprocessing
from sklearn.ensemble.forest import RandomForestClassifier, ExtraTreesClassifier


# convert gender labels to binary
enc = preprocessing.LabelEncoder()
enc.fit(train.Sex)

train['Sex01'] = enc.transform(train.Sex)
test['Sex01'] = enc.transform(test.Sex)

# define features and dropna. could do better by substituting them with median.
X = train[['Sex01','Fare','SibSp','Parch','Pclass','Survived']].dropna(axis=0, how='any', thresh=None, subset=None)

y = X.Survived.values
X = X[['Sex01','Fare','SibSp','Parch','Pclass']]

#cross validation folds
cv = 10

#classifiers to test
classifiers = [KNeighborsClassifier(70), DecisionTreeClassifier(), ExtraTreesClassifier(),RandomForestClassifier()]
# also tested this:
# svm.SVC(kernel='linear', C=1.0), GaussianNB()
# doesn't improve and takes long

#running crossvalidation score on all classifiers
for clf in classifiers:
    score = cross_val_score(clf, X, y, cv=cv)
    print "%s \n Accuracy: %0.2f (+/- %0.2f)\n" % (clf, score.mean(), score.std() / 2)

#now let's go to OOS test
testX = test[['Sex01','Fare','SibSp','Parch','Pclass']]
medianFare = testX.Fare.median()
testX.Fare = testX.Fare.fillna(medianFare)

#print results to CSV files for Kaggle submission
clf = ExtraTreesClassifier()
clf.fit(X, y)
test['Survived'] = pd.Series(clf.predict(testX))
test[['PassengerId','Survived']].to_csv('ETClf.csv',index=False)

clf = RandomForestClassifier()
clf.fit(X, y)
test['Survived'] = pd.Series(clf.predict(testX))
test[['PassengerId','Survived']].to_csv('RFClf.csv',index=False)

clf = DecisionTreeClassifier()
clf.fit(X, y)
test['Survived'] = pd.Series(clf.predict(testX))
test[['PassengerId','Survived']].to_csv('DTClf.csv',index=False)

