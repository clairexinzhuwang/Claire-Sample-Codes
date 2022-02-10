from typing import List, Any, Union

import numpy as np
import pandas as pd
import pylab
from matplotlib import pyplot as plt
from seaborn import countplot
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, accuracy_score

# read data
filepath = 'data.csv'
data = pd.read_csv(filepath)
print(data.head(10))
print(data.dtypes)

# convert year variable from integer to category and time variable to datetime
data['year'] = pd.Categorical(data.year)
data['time'] = pd.to_datetime(data['time'])
print(data.dtypes)

# summary statistics
data.describe(include='all')
summary_list = [data.describe()] + \
               [data.groupby([c])[data.columns[0]].count() \
                for c in data.columns if data[c].dtype == 'object' or 'category']
for i in summary_list:
    print(i)
    print()

# frequency plot for product summary
pylab.interactive(True)
pylab.rcParams['figure.figsize'] = (10, 6)
ax = countplot(x='productSum', data=data)
ax.set_xticklabels(ax.get_xticklabels(), rotation=90, ha="right")
plt.tight_layout()
plt.show()

# frequency plot for brand
countplot(x='brandGeneric', data=data)
plt.show()

# frequency plot for combined molecule
pylab.interactive(True)
pylab.rcParams['figure.figsize'] = (10, 6)
ax = countplot(x='combinedMolecule', data=data)
ax.set_xticklabels(ax.get_xticklabels(), rotation=90, ha="right")
plt.tight_layout()
plt.show()

# frequency plot for year
countplot(x='year', data=data)
plt.show()

# trendline of sales from Aprin 2015 to December 2019
data.plot(x="time", y="sales", kind="line")
plt.show()

# check if any variable has any missing value
data['productSum'].isnull().values.any()
data['brandGeneric'].isnull().values.any()
data['combinedMolecule'].isnull().values.any()
data['time'].isnull().values.any()
data['sales'].isnull().values.any()

# sort data by time and check anomalies
print(data.sort_values("time"))
print(data.sort_values("sales"))
# dummy variable codings
from sklearn import preprocessing

le = preprocessing.LabelEncoder()
le.fit(data.productSum)
dum_productSum = le.transform(data.productSum)

le = preprocessing.LabelEncoder()
le.fit(data.brandGeneric)
dum_brandGeneric = le.transform(data.brandGeneric)

le = preprocessing.LabelEncoder()
le.fit(data.combinedMolecule)
dum_combinedMolecule = le.transform(data.combinedMolecule)

s = pd.Series(data.time)
dum_time = pd.to_datetime(s)
for i in range(4313):
    dum_time[i] = pd.to_datetime(data.time[i]).value

d = {'January': 1, 'February': 2, 'March': 3, 'April': 4, 'May': 5, 'June': 6, 'July': 7, 'August': 8, 'September': 9,
     'October': 10, 'November': 11, 'December': 12}
data.month = data.month.map(d)

data["dum_productSum"] = dum_productSum
data["dum_brandGeneric"] = dum_brandGeneric
data["dum_combinedMolecule"] = dum_combinedMolecule
data["dum_time"] = dum_time
# data[1:53]

# fitting data = X
features = ['month', 'year']
X = data[features]
X.describe()
Y = data.sales

import csv

# create input data for testing
header = ['month', 'year', 'time']
with open('test.csv', 'w', encoding='UTF8') as f:
    writer = csv.writer(f)
    # write the header
    writer.writerow(header)
    # write the data
    for i in range(1, 13):
        time = str(i) + '/1/2020'
        writer.writerow([i, 2020, pd.to_datetime(time).value])

test = pd.read_csv('test.csv')
input_data = test[features]

# find transition position in the data where the drug type changes
transition = [0]
for i in range(4312):
    if data.productSum[i] != data.productSum[i + 1] or data.brandGeneric[i] != data.brandGeneric[i + 1] or \
            data.combinedMolecule[i] != data.combinedMolecule[i + 1]:
        transition.append(i + 1)

# do prediction for each unique type of drug
predict_list: List[Union[str, Any]] = []
for i in range(len(transition)):
    predict_list.append('predict_array_' + str(i))

for i in range(len(transition)):
    model = RandomForestRegressor(random_state=1)
    if i == len(transition) - 1:
        model.fit(X[transition[i]:], Y[transition[i]:])
    else:
        model.fit(X[transition[i]:transition[i + 1]], Y[transition[i]:transition[i + 1]])
    predict_list[i] = model.predict(input_data)
np.savetxt('result0.csv', predict_list, delimiter=',')



# model validation
from sklearn.model_selection import train_test_split

# calculate mean absolute error
def get_mae(max_leaf_nodes, train_X, val_X, train_y, val_y):
    model = RandomForestRegressor(max_leaf_nodes=max_leaf_nodes, random_state=0)
    model.fit(train_X, train_y)
    preds_val = model.predict(val_X)
    mae = mean_absolute_error(val_y, preds_val)
    return (mae)


# obtain optimal leaf nodes that doesn't overfit or underfit
from sklearn.utils.validation import check_array as check_arrays

MAE = []
val_predictions = []
leaf_index = []
for i in range(len(transition)):
    MAE.append(0)
    val_predictions.append(0)
    leaf_index.append(0)


def mean_absolute_percentage_error(y_true, y_pred):
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true))

# accuracy result of preliminary model
for i in range(len(transition)):
    print(i)
    ### this is to account for the one array that only has size of 1
    if i == len(transition) - 1:
        if 4312 - transition[i] > 1:
            train_X, val_X, train_y, val_y = train_test_split(X[transition[i]:], Y[transition[i]:], random_state=i)

            model = RandomForestRegressor(random_state=1)
            model.fit(train_X, train_y)
            val_predictions[i] = model.predict(val_X)

            MAE[i] = mean_absolute_percentage_error(val_y, val_predictions[i])

        else:
            MAE[i] == 'N/A'
    else:
        if transition[i + 1] - transition[i] > 1:
            train_X, val_X, train_y, val_y = train_test_split(X[transition[i]:transition[i + 1]],
                                                              Y[transition[i]:transition[i + 1]], random_state=i)

            model = RandomForestRegressor(random_state=1)
            model.fit(train_X, train_y)
            val_predictions[i] = model.predict(val_X)

            MAE[i] = mean_absolute_percentage_error(val_y, val_predictions[i])

        else:
            MAE[i] == 'N/A'

for i in range(len(transition)):
    # Calculate and display accuracy
    accuracy = 100 - np.mean(MAE[i])
    print('Accuracy_raw:', round(accuracy, 2), '%.')

# result plot of preliminary model
import matplotlib.pyplot as plot
for i in range(len(transition)):
    plot.figure()
    if i == len(transition) - 1:
        title_string = data.productSum[transition[i]] + ', ' + data.brandGeneric[transition[i]] + ', ' + \
                       data.combinedMolecule[transition[i]]
        plot.title(title_string)
        plot.scatter(pd.to_datetime(data.dum_time[transition[i]:]), Y[transition[i]:])
        plot.scatter(pd.to_datetime(test.time), predict_list[i])
        plot.plot(pd.to_datetime(data.dum_time[transition[i]:]), Y[transition[i]:], label='original data')
        plot.plot(pd.to_datetime(test.time), predict_list[i], label='2020 prediction_preliminary')
        plot.xlabel('Time (years)')
        plot.ylabel('Sales')
    else:
        title_string = data.productSum[transition[i]] + ', ' + data.brandGeneric[transition[i]] + ', ' + \
                       data.combinedMolecule[transition[i]]
        plot.title(title_string)
        plot.scatter(pd.to_datetime(data.dum_time[transition[i]:transition[i + 1]]), Y[transition[i]:transition[i + 1]])
        plot.scatter(pd.to_datetime(test.time), predict_list[i])
        plot.plot(pd.to_datetime(data.dum_time[transition[i]:transition[i + 1]]), Y[transition[i]:transition[i + 1]],
                  label='original data')
        plot.plot(pd.to_datetime(test.time), predict_list[i], label='2020 prediction_preliminary')
        plot.xlabel('Time (years)')
        plot.ylabel('Sales')

    plot.legend()
    plot.show()





# obtain optimized leaf nodes and MAE
for i in range(len(transition)):
    print(i)
    ### this is to account for the one array that only has size of 1
    if i == len(transition) - 1:
        if 4312 - transition[i] > 1:
            train_X, val_X, train_y, val_y = train_test_split(X[transition[i]:], Y[transition[i]:], random_state=i)

            my_mae = [0, 0]
            for max_leaf_nodes in range(2, 10):
                my_mae.append(0)
            for max_leaf_nodes in range(2, 10):
                my_mae[max_leaf_nodes] = get_mae(max_leaf_nodes, train_X, val_X, train_y, val_y)

            leaf_index[i] = my_mae.index(min(my_mae[2:]))
            model = RandomForestRegressor(max_leaf_nodes=leaf_index[i])
            model.fit(train_X, train_y)
            val_predictions[i] = model.predict(val_X)

            MAE[i] = mean_absolute_percentage_error(val_y, val_predictions[i])

        else:
            MAE[i] == 'N/A'
    else:
        if transition[i + 1] - transition[i] > 1:
            train_X, val_X, train_y, val_y = train_test_split(X[transition[i]:transition[i + 1]],
                                                              Y[transition[i]:transition[i + 1]], random_state=i)

            my_mae = [0, 0]
            for max_leaf_nodes in range(2, 100):
                my_mae.append(0)
            for max_leaf_nodes in range(2, 100):
                my_mae[max_leaf_nodes] = get_mae(max_leaf_nodes, train_X, val_X, train_y, val_y)

            leaf_index[i] = my_mae.index(min(my_mae[2:]))
            model = RandomForestRegressor(max_leaf_nodes=leaf_index[i])
            model.fit(train_X, train_y)
            val_predictions[i] = model.predict(val_X)

            MAE[i] = mean_absolute_percentage_error(val_y, val_predictions[i])

        else:
            MAE[i] == 'N/A'

# refit with optimized leaf nodes
for i in range(len(leaf_index)):
    if leaf_index[i] == 0:
        leaf_index[i] = leaf_index[i - 1]

for i in range(len(transition)):
    model = RandomForestRegressor(max_leaf_nodes=leaf_index[i], random_state=1)
    if i == len(transition) - 1:
        model.fit(X[transition[i]:], Y[transition[i]:])
    else:
        model.fit(X[transition[i]:transition[i + 1]], Y[transition[i]:transition[i + 1]])
    predict_list[i] = model.predict(input_data)

# print prediction values
print(predict_list)
np.savetxt('result.csv', predict_list, delimiter=',')

#accuracy for refitted model
for i in range(len(transition)):
    # Calculate and display accuracy
    accuracy = 100 - np.mean(MAE[i])
    print('Accuracy:', round(accuracy, 2), '%.')


# final result plot
import matplotlib.pyplot as plot

# import matplotlib.dates as mdates
"""
plot.gca().xaxis.set_major_formatter(mdates.DateFormatter('%m/%d/%Y'))
plot.gca().xaxis.set_major_locator(mdates.DayLocator(interval=10))
plot.plot(data.time[0:53],Y[0:53])
plot.gcf().autofmt_xdate()
"""

for i in range(len(transition)):
    plot.figure()
    if i == len(transition) - 1:
        title_string = data.productSum[transition[i]] + ', ' + data.brandGeneric[transition[i]] + ', ' + \
                       data.combinedMolecule[transition[i]]
        plot.title(title_string)
        plot.scatter(pd.to_datetime(data.dum_time[transition[i]:]), Y[transition[i]:])
        plot.scatter(pd.to_datetime(test.time), predict_list[i])
        plot.plot(pd.to_datetime(data.dum_time[transition[i]:]), Y[transition[i]:], label='original data')
        plot.plot(pd.to_datetime(test.time), predict_list[i], label='2020 prediction')
        plot.xlabel('Time (years)')
        plot.ylabel('Sales')
    else:
        title_string = data.productSum[transition[i]] + ', ' + data.brandGeneric[transition[i]] + ', ' + \
                       data.combinedMolecule[transition[i]]
        plot.title(title_string)
        plot.scatter(pd.to_datetime(data.dum_time[transition[i]:transition[i + 1]]), Y[transition[i]:transition[i + 1]])
        plot.scatter(pd.to_datetime(test.time), predict_list[i])
        plot.plot(pd.to_datetime(data.dum_time[transition[i]:transition[i + 1]]), Y[transition[i]:transition[i + 1]],
                  label='original data')
        plot.plot(pd.to_datetime(test.time), predict_list[i], label='2020 prediction')
        plot.xlabel('Time (years)')
        plot.ylabel('Sales')

    plot.legend()
    plot.show()

