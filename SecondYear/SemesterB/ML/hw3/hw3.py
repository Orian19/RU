import numpy as np
from scipy.special import factorial

class conditional_independence():

    def __init__(self):

        # You need to fill the None value with *valid* probabilities
        self.X = {0: 0.3, 1: 0.7}  # P(X=x)
        self.Y = {0: 0.3, 1: 0.7}  # P(Y=y)
        self.C = {0: 0.5, 1: 0.5}  # P(C=c)

        self.X_Y = {
            (0, 0): 0.2,
            (0, 1): 0.1,
            (1, 0): 0.25,
            (1, 1): 0.45
        }  # P(X=x, Y=y)

        self.X_C = {
            (0, 0): 0.3,
            (0, 1): 0.1,
            (1, 0): 0.2,
            (1, 1): 0.4
        }  # P(X=x, C=c)

        self.Y_C = {
            (0, 0): 0.15,
            (0, 1): 0.2,
            (1, 0): 0.35,
            (1, 1): 0.3
        }  # P(Y=y, C=c)

        self.X_Y_C = {
            (0, 0, 0): 0.09,
            (0, 0, 1): 0.04,
            (0, 1, 0): 0.21,
            (0, 1, 1): 0.06,
            (1, 0, 0): 0.06,
            (1, 0, 1): 0.16,
            (1, 1, 0): 0.14,
            (1, 1, 1): 0.24,
        }  # P(X=x, Y=y, C=c)

    def is_X_Y_dependent(self):
        """
        return True iff X and Y are depndendent
        """
        X = self.X
        Y = self.Y
        X_Y = self.X_Y

        X_Y_mult = []
        X_Y_joint = []
        for key in X_Y.keys():
            X_Y_mult.append(X.get(key[0]) * Y.get(key[1]))  # P(X=x) * P(Y=y)
            X_Y_joint.append(X_Y.get(key))

        return False in np.isclose(X_Y_mult, X_Y_joint)  # if there is one mismatch => X,Y dependent

    def is_X_Y_given_C_independent(self):
        """
        return True iff X_given_C and Y_given_C are indepndendent
        """
        X = self.X
        Y = self.Y
        C = self.C
        X_C = self.X_C
        Y_C = self.Y_C
        X_Y_C = self.X_Y_C

        XC_YC_mult = []
        X_Y_C_joint = []
        for key in X_Y_C.keys():
            # P(X=x|C=c) * P(Y=y|C=c) / P(C=c)
            XC_YC_mult.append(X_C.get((key[0], key[2])) * Y_C.get((key[1], key[2])) / C.get(key[2]))
            X_Y_C_joint.append(X_Y_C.get(key))

        return False not in np.isclose(XC_YC_mult, X_Y_C_joint)  # if there is one mismatch => X,Y dependent


def poisson_log_pmf(k, rate):
    """
    k: A discrete instance
    rate: poisson rate parameter (lambda)

    return the log pmf value for instance k given the rate
    """
    log_p = None

    pmf = (rate ** k) * np.exp(-rate) / factorial(k)  # TODO: make sure we can use scipy for factorial (piazza)
    log_p = np.log(pmf)

    return log_p


def get_poisson_log_likelihoods(samples, rates):
    """
    samples: set of univariate discrete observations
    rates: an iterable of rates to calculate log-likelihood by.

    return: 1d numpy array, where each value represent that log-likelihood value of rates[i]
    """
    likelihoods = None

    p_logs = []
    for rate in rates:
        p_logs.append(np.sum(poisson_log_pmf(samples, rate)))  # calculating the log-likelihood value of rates[i]

    likelihoods = np.array(p_logs)
    return likelihoods


def possion_iterative_mle(samples, rates):
    """
    samples: set of univariate discrete observations
    rate: a rate to calculate log-likelihood by.

    return: the rate that maximizes the likelihood
    """
    rate = 0.0
    likelihoods = get_poisson_log_likelihoods(samples, rates)  # might help
    rate = rates[np.argmax(likelihoods)]  # argmax(likelihood) = index of the max value

    return rate


def possion_analytic_mle(samples):
    """
    samples: set of univariate discrete observations

    return: the rate that maximizes the likelihood
    """
    mean = None
    # after doing all the steps we get that MLE for poission dist. is mean, namely: rate = 1/n * sum (samples)
    mean = np.mean(samples)

    return mean


def normal_pdf(x, mean, std):
    """
    Calculate normal desnity function for a given x, mean and standrad deviation.
 
    Input:
    - x: A value we want to compute the distribution for.
    - mean: The mean value of the distribution.
    - std:  The standard deviation of the distribution.

    Returns the normal distribution pdf according to the given mean and std for the given x.
    """
    p = None

    denominator = np.sqrt(2 * np.pi * np.square(std))
    numerator = np.exp(np.divide(-np.square((x - mean)), 2 * np.square(std)))

    p = numerator / denominator
    return p


class NaiveNormalClassDistribution():
    def __init__(self, dataset, class_value):
        """
        A class which encapsulates the relevant parameters(mean, std) for a class conditinoal normal distribution.
        The mean and std are computed from a given data set.
        
        Input
        - dataset: The dataset as a 2d numpy array, assuming the class label is the last column
        - class_value : The class to calculate the parameters for.
        """
        self.class_value = class_value
        self.class_data = dataset[dataset[:, -1] == self.class_value]
        self.mean = np.mean(self.class_data, axis=0)  # axis=0 to calculate each feature independently.
        self.std = np.std(self.class_data, axis=0)
        self.num_instances = dataset.shape[0]

    def get_prior(self):
        """
        Returns the prior porbability of the class according to the dataset distribution.
        """
        prior = None
        prior = self.class_data.shape[0] / self.num_instances
        return prior

    def get_instance_likelihood(self, x):
        """
        Returns the likelihhod porbability of the instance under the class according to the dataset distribution.
        """
        likelihood = None
        likelihood = 1

        for i, feature_value in enumerate(x[:-1]):  # x is a row vector (instance)
            likelihood *= normal_pdf(feature_value, self.mean[i], self.std[i])

        return likelihood

    def get_instance_posterior(self, x):
        """
        Returns the posterior porbability of the instance under the class according to the dataset distribution.
        * Ignoring p(x)
        """
        posterior = None
        posterior = self.get_instance_likelihood(x) * self.get_prior()

        return posterior


class MAPClassifier():
    def __init__(self, ccd0, ccd1):
        """
        A Maximum a posteriori classifier.
        This class will hold 2 class distributions.
        One for class 0 and one for class 1, and will predict an instance
        using the class that outputs the highest posterior probability
        for the given instance.

        Input
            - ccd0 : An object contating the relevant parameters and methods
                     for the distribution of class 0.
            - ccd1 : An object contating the relevant parameters and methods
                     for the distribution of class 1.
        """
        self.ccd0 = ccd0
        self.ccd1 = ccd1

    def predict(self, x):
        """
        Predicts the instance class using the 2 distribution objects given in the object constructor.

        Input
            - An instance to predict.
        Output
            - 0 if the posterior probability of class 0 is higher and 1 otherwise.
        """
        pred = None
        pred = 0 if self.ccd0.get_instance_posterior(x) > self.ccd1.get_instance_posterior(x) else 1

        return pred


def compute_accuracy(test_set, map_classifier):
    """
    Compute the accuracy of a given a test_set using a MAP classifier object.

    Input
        - test_set: The test_set for which to compute the accuracy (Numpy array). where the class label is the last column
        - map_classifier : A MAPClassifier object capable of prediciting the class for each instance in the testset.

    Ouput
        - Accuracy = #Correctly Classified / test_set size
    """
    acc = None
    test_size = test_set.shape[0]

    acc = 0
    for instance in test_set:
        if map_classifier.predict(instance) == instance[-1]:
            acc += 1

    acc /= test_size
    return acc


def multi_normal_pdf(x, mean, cov):
    """
    Calculate multi variable normal desnity function for a given x, mean and covarince matrix.

    Input:
    - x: A value we want to compute the distribution for.
    - mean: The mean vector of the distribution.
    - cov:  The covariance matrix of the distribution.

    Returns the normal distribution pdf according to the given mean and var for the given x.
    """
    pdf = None
    pdf_one = (2 * np.pi) ** (-x.shape[0] - 1 / 2)  # (2pi)^(-d/2)
    pdf_two = np.linalg.det(cov) ** -0.5  # det(Cov)^(-1/2)
    # e^(-1/2 * (x-mean).T * inv(Cov) * (x-mean))
    pdf_three = np.exp(-0.5 * np.dot(np.transpose(x[:-1] - mean[:-1]), np.dot(np.linalg.inv(cov), (x[:-1] - mean[:-1]))))
    pdf = pdf_one * pdf_two * pdf_three

    return pdf


class MultiNormalClassDistribution():

    def __init__(self, dataset, class_value):
        """
        A class which encapsulate the relevant parameters(mean, cov matrix) for a class conditinoal multi normal distribution.
        The mean and cov matrix (You can use np.cov for this!) will be computed from a given data set.
        
        Input
        - dataset: The dataset as a numpy array
        - class_value : The class to calculate the parameters for.
        """
        self.class_value = class_value
        self.class_data = dataset[dataset[:, -1] == self.class_value]
        self.mean = np.mean(self.class_data, axis=0)  # axis=0 to calculate each feature independently.
        self.cov = np.cov(self.class_data[:, :-1].transpose())  # each row is a variable, each column is an instance
        self.num_instances = dataset.shape[0]

    def get_prior(self):
        """
        Returns the prior porbability of the class according to the dataset distribution.
        """
        prior = None
        prior = self.class_data.shape[0] / self.num_instances
        return prior

    def get_instance_likelihood(self, x):
        """
        Returns the likelihood of the instance under the class according to the dataset distribution.
        """
        likelihood = None
        likelihood = multi_normal_pdf(x, self.mean, self.cov)
        return likelihood

    def get_instance_posterior(self, x):
        """
        Returns the posterior porbability of the instance under the class according to the dataset distribution.
        * Ignoring p(x)
        """
        posterior = None
        posterior = self.get_instance_likelihood(x) * self.get_prior()
        return posterior


class MaxPrior():
    def __init__(self, ccd0, ccd1):
        """
        A Maximum prior classifier.
        This class will hold 2 class distributions, one for class 0 and one for class 1, and will predicit an instance
        by the class that outputs the highest prior probability for the given instance.

        Input
            - ccd0 : An object contating the relevant parameters and methods for the distribution of class 0.
            - ccd1 : An object contating the relevant parameters and methods for the distribution of class 1.
        """
        self.ccd0 = ccd0
        self.ccd1 = ccd1

    def predict(self, x):
        """
        Predicts the instance class using the 2 distribution objects given in the object constructor.

        Input
            - An instance to predict.
        Output
            - 0 if the posterior probability of class 0 is higher and 1 otherwise.
        """
        pred = None
        pred = 0 if self.ccd0.get_prior() > self.ccd1.get_prior() else 1

        return pred


class MaxLikelihood():
    def __init__(self, ccd0, ccd1):
        """
        A Maximum Likelihood classifier.
        This class will hold 2 class distributions, one for class 0 and one for class 1, and will predicit an instance
        by the class that outputs the highest likelihood probability for the given instance.

        Input
            - ccd0 : An object contating the relevant parameters and methods for the distribution of class 0.
            - ccd1 : An object contating the relevant parameters and methods for the distribution of class 1.
        """
        self.ccd0 = ccd0
        self.ccd1 = ccd1

    def predict(self, x):
        """
        Predicts the instance class using the 2 distribution objects given in the object constructor.

        Input
            - An instance to predict.
        Output
            - 0 if the posterior probability of class 0 is higher and 1 otherwise.
        """
        pred = None
        pred = 0 if self.ccd0.get_instance_likelihood(x) > self.ccd1.get_instance_likelihood(x) else 1

        return pred


EPSILLON = 1e-6  # if a certain value only occurs in the test set, the probability for that value will be EPSILLON.


class DiscreteNBClassDistribution():
    def __init__(self, dataset, class_value):
        """
        A class which computes and encapsulate the relevant probabilites for a discrete naive bayes 
        distribution for a specific class. The probabilites are computed with laplace smoothing.
        
        Input
        - dataset: The dataset as a numpy array.
        - class_value: Compute the relevant parameters only for instances from the given class.
        """
        self.class_value = class_value
        self.class_data = dataset[dataset[:, -1] == self.class_value]
        self.n_i = self.class_data.shape[0]  # from the discrete formula (#instances of the current class)
        self.num_instances = dataset.shape[0]

    def get_prior(self):
        """
        Returns the prior porbability of the class
        according to the dataset distribution.
        """
        prior = None
        prior = self.class_data.shape[0] / self.num_instances
        return prior

    def get_instance_likelihood(self, x):
        """
        Returns the likelihood of the instance under
        the class according to the dataset distribution.
        """
        likelihood = None
        likelihood = 1
        alpha = 1  # from the Laplace smooth function, we choose alpha=1
        for i, feature_value in enumerate(x[:-1]):
            # case when the probability is zero -> P(x_j|A) = EPSILON
            if feature_value not in np.unique(self.class_data[:, i]):
                likelihood *= EPSILLON  # TODO: check why do we need both alpha and epsilon
                continue
            # when the probability is not zero we use the discrete func
            V_j = len(np.unique(self.class_data[:, i]))
            n_ij = self.class_data[self.class_data[:, i] == feature_value].shape[0]
            likelihood *= (n_ij + alpha) / (self.n_i + V_j)

        return likelihood

    def get_instance_posterior(self, x):
        """
        Returns the posterior porbability of the instance
        under the class according to the dataset distribution.
        * Ignoring p(x)
        """
        posterior = None
        posterior = self.get_instance_likelihood(x) * self.get_prior()
        return posterior


class MAPClassifier_DNB():
    def __init__(self, ccd0, ccd1):
        """
        A Maximum a posteriori classifier.
        This class will hold 2 class distributions, one for class 0 and one for class 1, and will predict an instance
        by the class that outputs the highest posterior probability for the given instance.

        Input
            - ccd0 : An object contating the relevant parameters and methods for the distribution of class 0.
            - ccd1 : An object contating the relevant parameters and methods for the distribution of class 1.
        """
        self.ccd0 = ccd0
        self.ccd1 = ccd1

    def predict(self, x):
        """
        Predicts the instance class using the 2 distribution objects given in the object constructor.
    
        Input
            - An instance to predict.
        Output
            - 0 if the posterior probability of class 0 is higher and 1 otherwise.
        """
        pred = None
        pred = 0 if self.ccd0.get_instance_posterior(x) > self.ccd1.get_instance_posterior(x) else 1

        return pred

    def compute_accuracy(self, test_set):
        """
        Compute the accuracy of a given a testset using a MAP classifier object.

        Input
            - test_set: The test_set for which to compute the accuracy (Numpy array).
        Ouput
            - Accuracy = #Correctly Classified / #test_set size
        """
        acc = None
        test_size = test_set.shape[0]

        acc = 0
        for instance in test_set:
            if self.predict(instance) == instance[-1]:
                acc += 1

        acc /= test_size
        return acc
