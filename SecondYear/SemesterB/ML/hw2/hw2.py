import numpy as np
import matplotlib.pyplot as plt

### Chi square table values ###
# The first key is the degree of freedom 
# The second key is the p-value cut-off
# The values are the chi-statistic that you need to use in the pruning

chi_table = {1: {0.5: 0.45,
                 0.25: 1.32,
                 0.1: 2.71,
                 0.05: 3.84,
                 0.0001: 100000},
             2: {0.5: 1.39,
                 0.25: 2.77,
                 0.1: 4.60,
                 0.05: 5.99,
                 0.0001: 100000},
             3: {0.5: 2.37,
                 0.25: 4.11,
                 0.1: 6.25,
                 0.05: 7.82,
                 0.0001: 100000},
             4: {0.5: 3.36,
                 0.25: 5.38,
                 0.1: 7.78,
                 0.05: 9.49,
                 0.0001: 100000},
             5: {0.5: 4.35,
                 0.25: 6.63,
                 0.1: 9.24,
                 0.05: 11.07,
                 0.0001: 100000},
             6: {0.5: 5.35,
                 0.25: 7.84,
                 0.1: 10.64,
                 0.05: 12.59,
                 0.0001: 100000},
             7: {0.5: 6.35,
                 0.25: 9.04,
                 0.1: 12.01,
                 0.05: 14.07,
                 0.0001: 100000},
             8: {0.5: 7.34,
                 0.25: 10.22,
                 0.1: 13.36,
                 0.05: 15.51,
                 0.0001: 100000},
             9: {0.5: 8.34,
                 0.25: 11.39,
                 0.1: 14.68,
                 0.05: 16.92,
                 0.0001: 100000},
             10: {0.5: 9.34,
                  0.25: 12.55,
                  0.1: 15.99,
                  0.05: 18.31,
                  0.0001: 100000},
             11: {0.5: 10.34,
                  0.25: 13.7,
                  0.1: 17.27,
                  0.05: 19.68,
                  0.0001: 100000}}


def calc_gini(data):
    """
    Calculate gini impurity measure of a dataset.
 
    Input:
    - data: any dataset where the last column holds the labels.
 
    Returns:
    - gini: The gini impurity value.
    """
    gini = 0.0
    m, n = data.shape  # m - #instances, n - #features
    unique_labels, counts = np.unique(data[:, -1], return_counts=True)  # getting the labels and their #appearences
    dist_vector = counts / m  # |Si| / |S|
    gini = 1.0 - np.sum(dist_vector ** 2)  # equation of gini
    return gini


def calc_entropy(data):
    """
    Calculate the entropy of a dataset.

    Input:
    - data: any dataset where the last column holds the labels.

    Returns:
    - entropy: The entropy value.
    """
    entropy = 0.0
    m, n = data.shape  # m - #instances, n - #features
    unique_labels, counts = np.unique(data[:, -1], return_counts=True)  # getting the labels and their #appearences
    dist_vector = counts / m  # |Si| / |S|
    entropy = -np.sum(dist_vector * np.log2(dist_vector))  # equation of entropy
    return entropy


class DecisionNode:

    def __init__(self, data, impurity_func, feature=-1, depth=0, chi=1, max_depth=1000, gain_ratio=False):

        self.data = data  # the relevant data for the node
        self.feature = feature  # column index of criteria being tested
        self.pred = self.calc_node_pred()  # the prediction of the node
        self.depth = depth  # the current depth of the node
        self.children = []  # array that holds this node's children
        self.children_values = []
        self.terminal = False  # determines if the node is a leaf
        self.chi = chi
        self.max_depth = max_depth  # the maximum allowed depth of the tree
        self.impurity_func = impurity_func
        self.gain_ratio = gain_ratio
        self.feature_importance = 0

    def calc_node_pred(self):
        """
        Calculate the node prediction.

        Returns:
        - pred: the prediction of the node
        """
        pred = None
        unique_labels, counts = np.unique(self.data[:, -1], return_counts=True)  # getting the labels and their #appearences
        unique = dict(zip(unique_labels, counts))  # creating a dict to easily find the max.
        pred = max(unique, key=unique.get)
        return pred

    def add_child(self, node, val):
        """
        Adds a child node to self.children and updates self.children_values

        This function has no return value
        """
        self.children.append(node)
        self.children_values.append(val)

    def calc_feature_importance(self, n_total_sample):
        """
        Calculate the selected feature importance.
        
        Input:
        - n_total_sample: the number of samples in the dataset.

        This function has no return value - it stores the feature importance in 
        self.feature_importance
        """
        m, n = self.data.shape  # m - #instances, n - #features
        goodness, groups = self.goodness_of_split(self.feature)
        self.feature_importance = (m / n_total_sample) * goodness

    def goodness_of_split(self, feature):
        """
        Calculate the goodness of split of a dataset given a feature and impurity function.

        Input:
        - feature: the feature index the split is being evaluated according to.

        Returns:
        - goodness: the goodness of split
        - groups: a dictionary holding the data after splitting 
                  according to the feature values.
        """
        goodness = 0
        groups = {}  # groups[feature_value] = data_subset
        m, n = self.data.shape  # m - #instances, n - #features
        feature_gain = 0
        split_gain = 0.0
        impurity = self.impurity_func(self.data)  # impurity of the class
        unique_values, counts = np.unique(self.data[:, feature], return_counts=True)

        if not self.gain_ratio:  # regular goodness of split
            for value in unique_values:
                feature_data = self.data
                feature_data = feature_data[feature_data[:, feature] == value]  # keeping rows of the specific value of the feature
                groups.update({value: feature_data})
                feature_gain += (feature_data.shape[0] / m) * self.impurity_func(feature_data)  # calculating Gini_Gain\information_Gain (according to impurity_func)
            goodness = impurity - feature_gain
        else:  # gain ratio
            info_gain = calc_entropy(self.data)
            for idx, value in enumerate(unique_values):
                feature_data = self.data
                feature_data = feature_data[feature_data[:, feature] == value]  # keeping rows of the specific value of the feature
                groups.update({value: feature_data})
                info_gain -= (counts[idx] / m) * calc_entropy(feature_data)  # calculating information_Gain (with entropy)
                split_gain += (counts[idx] / m) * np.log2((counts[idx] / m))
            if split_gain == 0:
                goodness = 0
            else:
                goodness = info_gain / (-split_gain)

        return goodness, groups

    def split(self):
        """
        Splits the current node according to the self.impurity_func. This function finds
        the best feature to split according to and create the corresponding children.
        This function should support pruning according to self.chi and self.max_depth.

        This function has no return value
        """
        m, n = self.data.shape  # m - #instances, n - #features

        # calculating the best_feature index
        best_feature_idx = -1
        max_goodness_of_split = -1
        for feature_idx in range(0, n - 1):  # finding the best feature
            # checking for the maximum goodness of split
            goodness = self.goodness_of_split(feature_idx)[0]
            if max_goodness_of_split < goodness:
                max_goodness_of_split = goodness
                best_feature_idx = feature_idx
        self.feature = best_feature_idx
        if self.feature is None:
            self.terminal = True

        unique_feature_values, _ = np.unique(self.data[:, self.feature], return_counts=True)
        unique_cls_values, _ = np.unique(self.data[:, -1], return_counts=True)

        if len(unique_feature_values) > 1 and len(unique_cls_values) > 1:
            # df = (#attributes - 1) * (#classes - 1)
            degree_of_freedom = (len(unique_feature_values) - 1) * (len(unique_cls_values) - 1)
        else:
            self.terminal = True
            return

        # getting the relevant p_value from the table
        if not self.chi == 1:
            p_value = chi_table[degree_of_freedom][self.chi]  # p_value = table[df][risk]
        else:
            p_value = 0

        # calculating the chi square value
        chi_squared_value = calc_chi_squared(best_feature_idx, self.data)

        # (checking we didn't pass the max_depth) and (chi_squared_value is greater than the p_value)
        if self.depth < self.max_depth and chi_squared_value > p_value:
            for value in unique_feature_values:  # creating the children nodes (value/attributes of that feature)
                new_child = DecisionNode(
                    data=self.data[self.data[:, best_feature_idx] == value],
                    impurity_func=self.impurity_func,
                    depth=self.depth + 1,
                    gain_ratio=self.gain_ratio,
                    max_depth=self.max_depth,
                    chi=self.chi
                )
                self.add_child(new_child, value)
        else:  # no split - current node is a leaf
            self.terminal = True
            return


def calc_chi_squared(best_feature_idx, data):
    m, n = data.shape  # m - #instances, n - #features
    unique_class_values, _ = np.unique(data[:, -1], return_counts=True)
    unique_feature_values, _ = np.unique(data[:, best_feature_idx], return_counts=True)
    chi_squared = 0

    for value in unique_feature_values:
        # filtering the data by current value
        value_data_filter = data[data[:, best_feature_idx] == value]

        value_instances = value_data_filter.shape[0]   # D_f
        for label in unique_class_values:
            # Another filter, to the specific class
            value_data = value_data_filter[value_data_filter[:, -1] == label]
            instance_idx = value_data.shape[0]  # p_f, n_f...
            E_idx = ((data[data[:, -1] == label].shape[0]) / m) * value_instances  # expected (rnd) values
            chi_squared += (instance_idx - E_idx) ** 2 / E_idx  # the formula, per value

    return chi_squared


class DecisionTree:
    def __init__(self, data, impurity_func, feature=-1, chi=1, max_depth=1000, gain_ratio=False):
        self.data = data  # the relevant data for the tree
        self.impurity_func = impurity_func  # the impurity function to be used in the tree
        self.chi = chi  # the risk value
        self.max_depth = max_depth  # the maximum allowed depth of the tree
        self.gain_ratio = gain_ratio  #
        self.root = None  # the root node of the tree

    def build_tree(self):
        """
        Build a tree using the given impurity measure and training dataset. 
        You are required to fully grow the tree until all leaves are pure 
        or the goodness of split is 0.

        This function has no return value
        """
        self.root = None
        self.root = DecisionNode(
            self.data,
            self.impurity_func,
            chi=self.chi,
            max_depth=self.max_depth,
            gain_ratio=self.gain_ratio
        )

        m = self.root.data.shape[0]
        nodes = [self.root]

        while len(nodes) > 0:  # while there are still node to explore (split)
            # in case terminal(max_dept/chi_value) or fully pure node
            if not nodes[0].terminal and self.is_pure(nodes[0]):
                nodes[0].terminal = True
                del nodes[0]
                continue

            nodes[0].split()
            nodes[0].calc_feature_importance(m)

            if nodes[0].terminal or nodes[0].goodness_of_split(nodes[0].children[0].feature)[0] == 0:
                del nodes[0]

            else:  # adding the children of the current node (then continuing to split them)
                nodes.extend(nodes[0].children)
                del nodes[0]

    @staticmethod
    def is_pure(node):
        """
        checking if a node is fully pure or not (monochromatic)
        Args:
            node: TreeNode

        Returns:

        """
        unique_node_values = np.unique(node.data[:, -1])
        if len(unique_node_values) == 1:
            return True
        return False

    def predict(self, instance):
        """
        Predict a given instance
     
        Input:
        - instance: an row vector from the dataset. Note that the last element 
                    of this vector is the label of the instance.
     
        Output: the prediction of the instance.
        """
        pred = None
        node = self.root

        # exploring the tree until finiding the relevant node
        while not node.terminal:
            curr_value = instance[node.feature]  # the value of the instance at the feature column
            if curr_value not in node.children_values:
                break
            curr_value_idx = node.children_values.index(curr_value)  # getting the index of that value inside childre_values list
            node = node.children[curr_value_idx]  # getting the node corresponding to the index above

        return node.pred

    def calc_accuracy(self, dataset):
        """
        Predict a given dataset 
     
        Input:
        - dataset: the dataset on which the accuracy is evaluated
     
        Output: the accuracy of the decision tree on the given dataset (%).
        """
        accuracy = 0
        actual_labels = list(dataset.transpose()[-1])  # getting the labels column

        prediction_labels = []
        for instance in dataset:  # getting the predictions for each instance
            prediction_labels.append(self.predict(instance))

        count_matching = 0
        for idx, actual_label in enumerate(actual_labels):
            # checking if the prediction matches the actual value
            if actual_label == prediction_labels[idx]:
                count_matching += 1

        accuracy = count_matching / len(actual_labels)

        return accuracy

    def depth(self):
        return self.root.depth()


def depth_pruning(X_train, X_validation):
    """
    Calculate the training and validation accuracies for different depths
    using the best impurity function and the gain_ratio flag you got
    previously. On a single plot, draw the training and testing accuracy 
    as a function of the max_depth. 

    Input:
    - X_train: the training data where the last column holds the labels
    - X_validation: the validation data where the last column holds the labels
 
    Output: the training and validation accuracies per max depth
    """
    training = []
    validation = []
    root = None

    tree = DecisionTree(data=X_train, impurity_func=calc_entropy, gain_ratio=True)
    for max_depth in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
        tree.max_depth = max_depth
        tree.build_tree()

        # calculating the accuracies for both the training and validation sets
        training.append(tree.calc_accuracy(X_train))
        validation.append(tree.calc_accuracy(X_validation))

    return training, validation


def chi_pruning(X_train, X_test):
    """
    Calculate the training and validation accuracies for different chi values
    using the best impurity function and the gain_ratio flag you got
    previously. 

    Input:
    - X_train: the training data where the last column holds the labels
    - X_validation: the validation data where the last column holds the labels
 
    Output:
    - chi_training_acc: the training accuracy per chi value
    - chi_validation_acc: the validation accuracy per chi value
    - depth: the tree depth for each chi value
    """
    chi_training_acc = []
    chi_validation_acc = []
    depth = []

    tree = DecisionTree(data=X_train, impurity_func=calc_entropy, gain_ratio=True)
    for chi in [1, 0.5, 0.25, 0.1, 0.05, 0.0001]:
        tree.chi = chi
        tree.build_tree()

        # calculating the accuracies for both the training and validation sets
        chi_training_acc.append(tree.calc_accuracy(X_train))
        chi_validation_acc.append(tree.calc_accuracy(X_test))

        # finding the depth for each chi value
        nodes = [tree.root]
        max_depth = 0
        while len(nodes) > 0:
            nodes.extend(nodes[0].children)
            if max_depth < nodes[0].depth:
                max_depth = nodes[0].depth
            del nodes[0]
        depth.append(max_depth)

    return chi_training_acc, chi_validation_acc, depth


def count_nodes(node):
    """
    Count the number of node in a given tree
 
    Input:
    - node: a node in the decision tree.
 
    Output: the number of node in the tree.
    """
    n_nodes = 0

    nodes = [node]
    while len(nodes) > 0:
        nodes.extend(nodes[0].children)
        del nodes[0]
        n_nodes += 1

    return n_nodes
