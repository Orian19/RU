from matplotlib.colors import ListedColormap
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt


def pearson_correlation(x, y):
    """
    Calculate the Pearson correlation coefficient for two given columns of data.

    Inputs:
    - x: An array containing a column of m numeric values.
    - y: An array containing a column of m numeric values.

    Returns:
    - The Pearson correlation coefficient between the two columns.
    """
    r = 0.0
    std_x = x - np.mean(x)
    std_y = y - np.mean(y)
    numerator = np.sum(np.dot(std_x, std_y))
    denominator = np.sqrt(np.sum(std_x ** 2) * np.sum(std_y ** 2))
    r = numerator / denominator
    return r


def feature_selection(X, y, n_features=5):
    """
    Select the best features using pearson correlation.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).

    Returns:
    - best_features: list of best features (names - list of strings).
    """
    best_features = []
    features_dict = {}
    m, n = X.shape
    # getting rid of non-numeric columns
    X = X.drop(['id', 'date'], axis=1)
    # calculating the pearson correlation per feature
    for feature in X:
        features_dict.update({feature: pearson_correlation(X[feature], y)})
    # sorting the feature by descending pearson score
    features_score = list(sorted(features_dict.items(), key=lambda item: item[1], reverse=True))
    # taking the best n features = features with highest score
    best_features = [feature[0] for feature in features_score[:n_features]]
    return best_features


class LogisticRegressionGD(object):
    """
    Logistic Regression Classifier using gradient descent.

    Parameters
    ------------
    eta : float
      Learning rate (between 0.0 and 1.0)
    n_iter : int
      Passes over the training dataset.
    eps : float
      minimal change in the cost to declare convergence
    random_state : int
      Random number generator seed for random weight
      initialization.
    """

    def __init__(self, eta=0.00005, n_iter=10000, eps=0.000001, random_state=1):
        self.eta = eta
        self.n_iter = n_iter
        self.eps = eps
        self.random_state = random_state

        # model parameters
        self.theta = None

        # iterations history
        self.Js = []
        self.thetas = []

    def fit(self, X, y):
        """
        Fit training data (the learning phase).
        Update the theta vector in each iteration using gradient descent.
        Store the theta vector in self.thetas.
        Stop the function when the difference between the previous cost and the current is less than eps
        or when you reach n_iter.
        The learned parameters must be saved in self.theta.
        This function has no return value.

        Parameters
        ----------
        X : {array-like}, shape = [n_examples, n_features]
          Training vectors, where n_examples is the number of examples and
          n_features is the number of features.
        y : array-like, shape = [n_examples]
          Target values.

        """
        # set random seed
        np.random.seed(self.random_state)
        # initializing n (# features + 1 for theta_0) random theta values
        self.theta = np.random.random(size=X.shape[1] + 1)
        X = self.apply_bias_trick(X)
        self.theta, self.Js = self.efficient_gradient_descent(X, y)

    def predict(self, X):
        """
        Return the predicted class labels for a given instance.
        Parameters
        ----------
        X : {array-like}, shape = [n_examples, n_features]
        """
        preds = None
        X = self.apply_bias_trick(X)
        sigmoid = 1 / (1 + np.exp(-X.dot(self.theta.transpose())))
        # predicting class 1 if the probability is larger than 0.5
        preds = [1 if prob >= 0.5 else 0 for prob in sigmoid]
        return np.asarray(preds)

    def efficient_gradient_descent(self, X, y):
        """
          Input:
          - X: Input data (m instances over n features).
          - y: True labels (m instances).
          - theta: The parameters (weights) of the model being learned.
          - etas: The learning rate of your model.
          - n_iter: The number of updates performed.

          Returns:
          - theta: The learned parameters of your model.
          - J_history: the loss value for every iteration.
        """
        theta = self.theta.copy()
        J_history = []
        current = np.inf

        # sigmoid func, updating theta and js
        h_zero_vector = 1 / (1 + np.exp(-X.dot(theta.transpose())))
        deviation = h_zero_vector - y
        theta = theta + (-self.eta) * np.dot(X.transpose(), deviation)
        J_history.append(self.compute_cost(X, y, theta))
        self.thetas.append(theta)

        i = 0
        while i <= self.n_iter - 1 and (abs(current - J_history[i - 1]) >= self.eps or np.isnan(J_history[i])):
            h_zero_vector = 1 / (1 + np.exp(-X.dot(theta.transpose())))
            deviation = h_zero_vector - y
            theta = theta + (-self.eta) * np.dot(X.transpose(), deviation)
            J_history.append(self.compute_cost(X, y, theta))
            self.thetas.append(theta)
            i += 1
            current = J_history[i]

        return theta, J_history

    @staticmethod
    def compute_cost(X, y, theta):
        """
      Computes the average squared difference between an observation's actual and
      predicted values for LoR

      Input:
      - X: Input data (m instances over n features).
      - y: True labels (m instances).
      - theta: the parameters (weights) of the model being learned.

      Returns:
      - J: the cost associated with the current set of parameters (single number).
        """
        J = 0  # We use J for the cost.

        # creating the variables for the equation
        m = X.shape[0]
        h = 1 / (1 + np.exp(-X.dot(theta.transpose())))

        # calculate the value of the equation
        J = (1 / m) * np.sum((-y * np.log(h)) - (1 - y) * np.log(1 - h))
        return J

    @staticmethod
    def apply_bias_trick(X):
        """
      Applies the bias trick to the input data.

      Input:
      - X: Input data (m instances over n features).

      Returns:
      - X: Input data with an additional column of ones in the
          zeroth position (m instances over n+1 features).
        """
        m = X.shape[0]
        X = np.column_stack((np.ones((m)), X))
        return X


def cross_validation(X, y, folds, algo, random_state):
    """
    This function performs cross validation as seen in class.

    1. shuffle the data and creates folds
    2. train the model on each fold
    3. calculate aggregated metrics

    Parameters
    ----------
    X : {array-like}, shape = [n_examples, n_features]
      Training vectors, where n_examples is the number of examples and
      n_features is the number of features.
    y : array-like, shape = [n_examples]
      Target values.
    folds : number of folds (int)
    algo : an object of the classification algorithm
    random_state : int
      Random number generator seed for random weight
      initialization.

    Returns the cross validation accuracy.
    """

    cv_accuracy = None
    # set random seed
    np.random.seed(random_state)
    data = np.column_stack((X, y))
    # shuffling the data
    np.random.shuffle(data)
    X_copy = data[:, :-1]
    y_copy = data[:, -1]
    # splitting the data to k folds (for validation)
    k_folds = np.split(X_copy, folds)

    accuracies = []
    for k, validation_set in enumerate(k_folds):
        # getting the training set values for the k folds
        train_set_X = np.concatenate(k_folds[:k] + k_folds[k + 1:])
        train_set_y = np.concatenate((y_copy[:k * validation_set.shape[0]],
                                      y_copy[(k + 1) * validation_set.shape[0]:]), axis=0)

        # getting the validation class values
        val_set_y = y_copy[k * validation_set.shape[0]:(k + 1) * validation_set.shape[0]]

        # learning process
        algo.fit(train_set_X, train_set_y)
        # calculating accuracies
        predictions = algo.predict(validation_set)
        accuracies.append(np.count_nonzero((predictions == val_set_y)) / validation_set.shape[0])

    cv_accuracy = sum(accuracies) / folds

    return cv_accuracy


def norm_pdf(data, mu, sigma):
    """
    Calculate normal desnity function for a given data,
    mean and standrad deviation.

    Input:
    - x: A value we want to compute the distribution for.
    - mu: The mean value of the distribution.
    - sigma:  The standard deviation of the distribution.

    Returns the normal distribution pdf according to the given mu and sigma for the given x.
    """
    p = None
    denominator = np.sqrt(2 * np.pi * np.square(sigma))
    numerator = np.exp(np.divide(-np.square((data - mu)), 2 * np.square(sigma)))

    p = numerator / denominator
    return p


class EM(object):
    """
    Naive Bayes Classifier using Gauusian Mixture Model (EM) for calculating the likelihood.

    Parameters
    ------------
    k : int
      Number of gaussians in each dimension
    n_iter : int
      Passes over the training dataset in the EM proccess
    eps: float
      minimal change in the cost to declare convergence
    random_state : int
      Random number generator seed for random params initialization.
    """

    def __init__(self, k=1, n_iter=1000, eps=0.01, random_state=1991):
        self.k = k
        self.n_iter = n_iter
        self.eps = eps
        self.random_state = random_state

        np.random.seed(self.random_state)

        self.responsibilities = None
        self.weights = None
        self.mus = None
        self.sigmas = None
        self.costs = None

    # initial guesses for parameters
    def init_params(self, data):
        """
        Initialize distribution params

        """
        self.sigmas = np.random.random(size=self.k)
        self.weights = np.array([1 / self.k] * self.k)
        self.mus = np.random.choice(data.flatten(), self.k)

    def expectation(self, data):
        """
        E step - This function should calculate and update the responsibilities
        """
        resp_numerator = self.weights * norm_pdf(data, self.mus, self.sigmas)  # dot product
        resp_denominator = np.sum(resp_numerator, axis=1)
        resp_denominator = resp_denominator.reshape(-1, 1)
        self.responsibilities = resp_numerator / resp_denominator

    def maximization(self, data):
        """
        M step - This function should calculate and update the distribution params
        """
        N = data.shape[0]  # #instances
        self.weights = (1 / N) * np.sum(self.responsibilities, axis=0)
        self.mus = (1 / (N * self.weights)) * np.sum(self.responsibilities * data, axis=0)
        self.sigmas = (1 / (N * self.weights)) * np.sum(self.responsibilities * (data - self.mus) ** 2, axis=0)
        self.sigmas = np.sqrt(self.sigmas)

    def fit(self, data):
        """
        Fit training data (the learning phase).
        Use init_params and then expectation and maximization function in order to find params
        for the distribution.
        Store the params in attributes of the EM object.
        Stop the function when the difference between the previous cost and the current is less than eps
        or when you reach n_iter.
        """

        self.init_params(data)
        current = np.inf
        self.costs = []
        self.expectation(data)
        self.maximization(data)
        self.costs.append(self.compute_cost(data))
        i = 0
        # optimizing the learning parameters until reaching limit of iterations or threshold (eps)
        while i <= self.n_iter - 1 and abs(current - self.costs[i - 1]) >= self.eps:
            self.expectation(data)
            self.maximization(data)
            self.costs.append(self.compute_cost(data))
            i += 1
            current = self.costs[i]

    def get_dist_params(self):
        return self.weights, self.mus, self.sigmas

    def compute_cost(self, data):
        """
      Computes the average squared difference between an observation's actual and
      predicted values for EM

      Input:
      - X: Input data (m instances over n features).
      - y: True labels (m instances).
      - theta: the parameters (weights) of the model being learned.

      Returns:
      - J: the cost associated with the current set of parameters (single number).
        """
        J = 0  # we use J for the cost
        # using -log likelihood as the cost function
        J = np.sum(-np.log(np.sum(self.weights * norm_pdf(data, self.mus, self.sigmas), axis=1)))
        return J


def gmm_pdf(data, weights, mus, sigmas):
    """
    Calculate gmm desnity function for a given data,
    mean and standrad deviation.

    Input:
    - data: A value we want to compute the distribution for.
    - weights: The weights for the GMM
    - mus: The mean values of the GMM.
    - sigmas:  The standard deviation of the GMM.

    Returns the GMM distribution pdf according to the given mus, sigmas and weights
    for the given data.
    """
    pdf = None
    pdf = np.sum(weights * norm_pdf(data.reshape(-1, 1), mus, sigmas), axis=1)
    return pdf


class NaiveBayesGaussian(object):
    """
    Naive Bayes Classifier using Gaussian Mixture Model (EM) for calculating the likelihood.

    Parameters
    ------------
    k : int
      Number of gaussians in each dimension
    random_state : int
      Random number generator seed for random params initialization.
    """

    def __init__(self, k=1, random_state=1991):
        self.k = k
        self.random_state = random_state
        self.prior = {}
        self.gmms = {}

    def fit(self, X, y):
        """
        Fit training data.

        Parameters
        ----------
        X : array-like, shape = [n_examples, n_features]
          Training vectors, where n_examples is the number of examples and
          n_features is the number of features.
        y : array-like, shape = [n_examples]
          Target values.
        """
        self.prior = {}
        self.gmms = {}

        # getting unique class values
        unique_class, counts = np.unique(y, return_counts=True)
        # calculating the prior probability for each class value
        for i, value in enumerate(unique_class):
            self.prior.update({value: counts[i] / X.shape[0]})

        # defining EM for each feature in each class. {0: {0: EM, 1: EM}, 1: {0: EM, 1: EM}}
        for class_value in unique_class:
            self.gmms.update({class_value: {feature: EM(k=self.k) for feature in range(X.shape[1])}})

        # training all EM's
        for class_value in self.gmms.keys():
            for feature in self.gmms[class_value].keys():
                self.gmms[class_value][feature].fit(X[y == class_value][:, feature].reshape(-1, 1))

    def get_likelihood(self, X, class_value):
        """
        Returns the likelihhod porbability of the instance under the class according to the dataset distribution.
        """
        likelihood = np.ones(X.shape[0])

        # calculating the likelihood per feature, using GMM pdf as the likelihood function
        for feature in range(X.shape[1]):
            weights, mus, sigmas = self.gmms[class_value][feature].get_dist_params()
            gmm = gmm_pdf(X[:, feature], weights, mus, sigmas)
            likelihood *= gmm

        return likelihood

    def get_prior(self, class_label):
        return self.prior[class_label]

    def calc_posterior(self, X, class_value):
        # calculating posterior = prior * likelihood
        return self.get_prior(class_value) * self.get_likelihood(X, class_value)

    def predict(self, X):
        """
        Return the predicted class labels for a given instance.
        Parameters
        ----------
        X : {array-like}, shape = [n_examples, n_features]
        """
        preds = None
        preds = []

        posteriors = np.zeros((X.shape[0], len(self.prior)))

        # calculating posteriors feature by feature and predicting the class according to the max
        for i, class_value in enumerate(self.prior.keys()):
            posteriors[:, i] = self.calc_posterior(X, class_value)
        return np.array(list(self.prior.keys()))[np.argmax(posteriors, axis=1)]


def model_evaluation(x_train, y_train, x_test, y_test, k, best_eta, best_eps):
    '''
    Read the full description of this function in the notebook.

    You should use visualization for self debugging using the provided
    visualization functions in the notebook.
    Make sure you return the accuracies according to the return dict.

    Parameters
    ----------
    x_train : array-like, shape = [n_train_examples, n_features]
      Training vectors, where n_examples is the number of examples and
      n_features is the number of features.
    y_train : array-like, shape = [n_train_examples]
      Target values.
    x_test : array-like, shape = [n_test_examples, n_features]
      Training vectors, where n_examples is the number of examples and
      n_features is the number of features.
    y_test : array-like, shape = [n_test_examples]
      Target values.
    k : Number of gaussians in each dimension
    best_eta : best eta from cv
    best_eps : best eta from cv
    '''

    lor_train_acc = None
    lor_test_acc = None
    bayes_train_acc = None
    bayes_test_acc = None

    # training with LoR
    LoR_train = LogisticRegressionGD(eta=best_eta, eps=best_eps)
    LoR_train.fit(x_train, y_train)

    # training with naive bayes
    gmms_train = NaiveBayesGaussian(k=k)
    gmms_train.fit(x_train, y_train)

    # predicting by training data and test data for LoR
    predictions_LoRx_train = LoR_train.predict(x_train)
    predictions_LoRx_test = LoR_train.predict(x_test)

    # predicting by training data and test data for naive bayes
    predictions_Gmmx_train = gmms_train.predict(x_train)
    predictions_Gmmx_test = gmms_train.predict(x_test)

    # accuracies for LoR
    lor_train_acc = np.count_nonzero(predictions_LoRx_train.reshape(-1, 1) == y_train.reshape(-1, 1)) / len(y_train)
    lor_test_acc = np.count_nonzero(predictions_LoRx_test.reshape(-1, 1) == y_test.reshape(-1, 1)) / len(y_test)

    # accuracies for naive bayes
    bayes_train_acc = np.count_nonzero(predictions_Gmmx_train.reshape(-1, 1) == y_train.reshape(-1, 1)) / len(y_train)
    bayes_test_acc = np.count_nonzero(predictions_Gmmx_test.reshape(-1, 1) == y_test.reshape(-1, 1)) / len(y_test)

    # plotting the results with decision regions
    plot_decision_regions(x_train, y_train, LoR_train)
    plot_decision_regions(x_train, y_train, gmms_train)

    # explaining the results
    print("We can see in the graph that data is not linearly separable and Naive Bayes Algorithm works better and "
          "produces more accurate results") # naive bayes

    plt.plot(np.arange(len(LoR_train.Js)), LoR_train.Js)
    plt.ylabel('cost')
    plt.xlabel('iteration')
    plt.title('cost Vs iteration of LoR model')
    plt.show()

    # explaining the results
    print("We can see that LoR converges quickly in regrades to the cost vs the number of iterations")  # LoR

    return {'lor_train_acc': lor_train_acc,
            'lor_test_acc': lor_test_acc,
            'bayes_train_acc': bayes_train_acc,
            'bayes_test_acc': bayes_test_acc}


def generate_datasets():
    from scipy.stats import multivariate_normal
    '''
    This function should have no input.
    It should generate the two dataset as described in the jupyter notebook,
    and return them according to the provided return dict.
    '''
    dataset_a_features = None
    dataset_a_labels = None
    dataset_b_features = None
    dataset_b_labels = None

    # without correlation -> best for naive (features are independent)
    # 1th gaussian
    mean_class0_a1 = [6, 6, 6]
    cov_class0_a1 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    class0_a1 = multivariate_normal.rvs(mean=mean_class0_a1, cov=cov_class0_a1, size=300)

    mean_class1_a1 = [8, 8, 8]
    cov_class1_a1 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]  # identity covariance matrix
    class1_a1 = multivariate_normal.rvs(mean=mean_class1_a1, cov=cov_class1_a1, size=300)

    # 2nd gaussian
    mean_class0_a2 = [7, 7, 7]
    cov_class0_a2 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    class0_a2 = multivariate_normal.rvs(mean=mean_class0_a2, cov=cov_class0_a2, size=300)

    mean_class1_a2 = [9, 9, 9]
    cov_class1_a2 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]  # identity covariance matrix
    class1_a2 = multivariate_normal.rvs(mean=mean_class1_a2, cov=cov_class1_a2, size=300)

    # vertically stacking classes to create one big dataset
    class0_a = np.vstack((class0_a1, class0_a2))
    class1_a = np.vstack((class1_a1, class1_a2))
    dataset_a_features = np.vstack((class0_a, class1_a))

    # using 2 gaussians
    dataset_a_labels = np.hstack((np.zeros(600), np.ones(600)))

    # With correlation -> linear relationship between features (features are dependent)
    mean_class0_b = [3, 3, 3]
    cov_class0_b = [[1, 0.8, 0.8], [0.8, 1, 0.8], [0.8, 0.8, 1]]  # Correlated features
    class0_b = multivariate_normal.rvs(mean=mean_class0_b, cov=cov_class0_b, size=300)

    mean_class1_b = [8, 8, 8]
    cov_class1_b = [[1, 0.8, 0.8], [0.8, 1, 0.8], [0.8, 0.8, 1]]  # Correlated features
    class1_b = multivariate_normal.rvs(mean=mean_class1_b, cov=cov_class1_b, size=300)

    # vertically stacking classes to create one big dataset
    dataset_b_features = np.vstack((class0_b, class1_b))

    dataset_b_labels = np.hstack((np.zeros(300), np.ones(300)))

    visualize_datasets({'dataset_a_features': dataset_a_features,
                        'dataset_a_labels': dataset_a_labels,
                        'dataset_b_features': dataset_b_features,
                        'dataset_b_labels': dataset_b_labels
                        })

    return {'dataset_a_features': dataset_a_features,
            'dataset_a_labels': dataset_a_labels,
            'dataset_b_features': dataset_b_features,
            'dataset_b_labels': dataset_b_labels
            }


def plot_decision_regions(X, y, classifier, resolution=0.01, title=""):
    """
    Function for ploting the decision boundaries of a model
    """
    # setup marker generator and color map
    markers = ('.', '.')
    colors = ['blue', 'red']
    cmap = ListedColormap(colors[:len(np.unique(y))])
    # plot the decision surface
    x1_min, x1_max = X[:, 0].min() - 1, X[:, 0].max() + 1
    x2_min, x2_max = X[:, 1].min() - 1, X[:, 1].max() + 1
    xx1, xx2 = np.meshgrid(np.arange(x1_min, x1_max, resolution),
                           np.arange(x2_min, x2_max, resolution))
    Z = classifier.predict(np.array([xx1.ravel(), xx2.ravel()]).T)
    Z = Z.reshape(xx1.shape)
    plt.contourf(xx1, xx2, Z, alpha=0.3, cmap=cmap)
    plt.xlim(xx1.min(), xx1.max())
    plt.ylim(xx2.min(), xx2.max())

    for idx, cl in enumerate(np.unique(y)):
        plt.title(title)
        plt.scatter(x=X[y == cl, 0],
                    y=X[y == cl, 1],
                    alpha=0.8,
                    c=colors[idx],
                    marker=markers[idx],
                    label=cl,
                    edgecolor='black')
    plt.show()


def visualize_datasets(data):
    dataset_a_features = data['dataset_a_features']
    dataset_a_labels = data['dataset_a_labels']
    dataset_b_features = data['dataset_b_features']
    dataset_b_labels = data['dataset_b_labels']

    # figure will show each feature against all other features
    fig, axs = plt.subplots(2, 3, figsize=(18, 12))

    # dataset a

    # feature 1 vs feature 2
    axs[0, 0].scatter(dataset_a_features[dataset_a_labels == 0][:, 0], dataset_a_features[dataset_a_labels == 0][:, 1],
                      color='r', label='Class 0')
    axs[0, 0].scatter(dataset_a_features[dataset_a_labels == 1][:, 0], dataset_a_features[dataset_a_labels == 1][:, 1],
                      color='b', label='Class 1')
    axs[0, 0].set_title('Dataset A: Feature 1 vs Feature 2')
    axs[0, 0].set_xlabel('Feature 1')
    axs[0, 0].set_ylabel('Feature 2')
    axs[0, 0].legend()

    # feature 1 vs feature 3
    axs[0, 1].scatter(dataset_a_features[dataset_a_labels == 0][:, 0], dataset_a_features[dataset_a_labels == 0][:, 2],
                      color='r', label='Class 0')
    axs[0, 1].scatter(dataset_a_features[dataset_a_labels == 1][:, 0], dataset_a_features[dataset_a_labels == 1][:, 2],
                      color='b', label='Class 1')
    axs[0, 1].set_title('Dataset A: Feature 1 vs Feature 3')
    axs[0, 1].set_xlabel('Feature 1')
    axs[0, 1].set_ylabel('Feature 3')
    axs[0, 1].legend()

    # feature 2 vs feature 3
    axs[0, 2].scatter(dataset_a_features[dataset_a_labels == 0][:, 1], dataset_a_features[dataset_a_labels == 0][:, 2],
                      color='r', label='Class 0')
    axs[0, 2].scatter(dataset_a_features[dataset_a_labels == 1][:, 1], dataset_a_features[dataset_a_labels == 1][:, 2],
                      color='b', label='Class 1')
    axs[0, 2].set_title('Dataset A: Feature 2 vs Feature 3')
    axs[0, 2].set_xlabel('Feature 2')
    axs[0, 2].set_ylabel('Feature 3')
    axs[0, 2].legend()

    # dataset b

    # feature 1 vs feature 2
    axs[1, 0].scatter(dataset_b_features[dataset_b_labels == 0][:, 0], dataset_b_features[dataset_b_labels == 0][:, 1],
                      color='r', label='Class 0')
    axs[1, 0].scatter(dataset_b_features[dataset_b_labels == 1][:, 0], dataset_b_features[dataset_b_labels == 1][:, 1],
                      color='b', label='Class 1')
    axs[1, 0].set_title('Dataset B: Feature 1 vs Feature 2')
    axs[1, 0].set_xlabel('Feature 1')
    axs[1, 0].set_ylabel('Feature 2')
    axs[1, 0].legend()

    # feature 1 vs feature 3
    axs[1, 1].scatter(dataset_b_features[dataset_b_labels == 0][:, 0], dataset_b_features[dataset_b_labels == 0][:, 2],
                      color='r', label='Class 0')
    axs[1, 1].scatter(dataset_b_features[dataset_b_labels == 1][:, 0], dataset_b_features[dataset_b_labels == 1][:, 2],
                      color='b', label='Class 1')
    axs[1, 1].set_title('Dataset B: Feature 1 vs Feature 3')
    axs[1, 1].set_xlabel('Feature 1')
    axs[1, 1].set_ylabel('Feature 3')
    axs[1, 1].legend()

    # feature 2 vs feature 3
    axs[1, 2].scatter(dataset_b_features[dataset_b_labels == 0][:, 1], dataset_b_features[dataset_b_labels == 0][:, 2],
                      color='r', label='Class 0')
    axs[1, 2].scatter(dataset_b_features[dataset_b_labels == 1][:, 1], dataset_b_features[dataset_b_labels == 1][:, 2],
                      color='b', label='Class 1')
    axs[1, 2].set_title('Dataset B: Feature 2 vs Feature 3')
    axs[1, 2].set_xlabel('Feature 2')
    axs[1, 2].set_ylabel('Feature 3')
    axs[1, 2].legend()

    plt.tight_layout()
    plt.show()
