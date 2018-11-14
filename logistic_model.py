import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score



# Load Data
cleaned_data = pd.read_csv("data/nfl_filtered.csv", header=0)
headers = cleaned_data.columns


# set up X and Y
y = cleaned_data[["home_wn"]]
X = cleaned_data[['Spread Favorite', 'Over Under','hmhalfsc', 'hpyd',
                          'hpatt', 'hcomp', 'hypa', 'hcomppct', 'hint',
                          'hryd', 'hratt', 'hypr', 'awhalfsc', 'apyd',
                          'apatt', 'acomp', 'aypa', 'acomppct', 'aint',
                          'aryd', 'aratt', 'aypr']]

# split data into training and testing

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33, random_state=42)


# fit model to training data

log_reg = LogisticRegression().fit(X_train,y_train.values.ravel())

# predict on testing data
pred = log_reg.predict(X_test)
print(accuracy_score(y_test, pred))

