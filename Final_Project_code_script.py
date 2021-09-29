# python code:
#1.	
#Decision Tree algorithm: 

#py_install("pandas")
#py_install("matplotlib")
#py_install("seaborn")
#py_install("scikit-learn")
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import tree
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder

df = pd.read_csv("C:/Users/Israel/Desktop/ML & application to biological data analysis/Final project/archive/timesData - UsedWithLabels.csv")
df = df.dropna() #drop nans rows
LE = LabelEncoder()
df['country'] = LE.fit_transform(df['country'])
df['university_name'] = LE.fit_transform(df['university_name'])
features0 = df.drop(['world_rank','female_male_ratio','year'], axis=1 )
features = np.array([features0])
labels0 = df.pop("world_rank")
labels = np.array([labels0])
X_train, X_test, y_train, y_test = train_test_split(
    features0, labels0,
    test_size=0.3,
    random_state=42,
)
max_pred = 0.0001
best_depth = 0
pred_array = []
i_lst = []
pred_lst = []
i_for_depth = 0.0
accuracy = 0.0
j=0
for i in range(300):
  j=i+1
  print (j)
  clf = tree.DecisionTreeClassifier(criterion='gini', splitter='best', max_depth=j, min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features=None, random_state=None, max_leaf_nodes=None, min_impurity_decrease=0.0, min_impurity_split=None, class_weight=None, ccp_alpha=0.0)
  
  clf.fit(X = X_train, y = y_train)
  clf.feature_importances_ # [ 1.,  0.,  0.]
  accuracy = clf.score(X=X_test, y=y_test)

  if (max_pred <= accuracy):
    best_depth = clf.get_depth()
    pred_array =  clf.predict(X_test)
    max_pred = accuracy # 1.0
    print("better accuracy: ",max_pred)
    best_wight = clf.feature_importances_ # [ 1.,  0.,  0.]
  pred_lst.append(accuracy)
  i_lst.append(j)

print("best_depth:",best_depth, "pred_array:",pred_array,"max_pred:",max_pred,"best_wight:",best_wight)

fig = plt.figure(figsize=(10,5))
ax = fig.add_subplot(111)
plt.plot(i_lst,pred_lst,"-b");

ax.set_title('accuracy prediction')
ax.set_xlabel("index")
ax.set_ylabel("accuracy")
plt.show()
plt.close()

