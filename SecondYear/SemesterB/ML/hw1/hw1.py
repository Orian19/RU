###### Your ID ######
# ID1: 211676325
# ID2: 208126094
#####################

# imports
import numpy as np
import pandas as pd


def preprocess(X, y):
    """
    Perform mean normalization on the features and true labels.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).

    Returns:
    - X: The mean normalized inputs.
    - y: The mean normalized labels.
    """
    X = (X - X.mean()) / (X.max() - X.min())
    y = (y - y.mean()) / (y.max() - y.min())

    return X, y


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
    X = np.c_[np.ones((m)), X]
    return X


def compute_cost(X, y, theta):
    """
    Computes the average squared difference between an observation's actual and
    predicted values for linear regression.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).
    - theta: the parameters (weights) of the model being learned.

    Returns:
    - J: the cost associated with the current set of parameters (single number).
    """
    J = 0  # We use J for the cost.

    # Creating the variables for the equation.
    m = len(X)
    h = X.dot(theta)

    # Calculate the value of the equation.
    J = np.sum((h - y) ** 2) / (2 * m)
    return J


def gradient_descent(X, y, theta, alpha, num_iters):
    """
    Learn the parameters of the model using gradient descent using
    the training set. Gradient descent is an optimization algorithm
    used to minimize some (loss) function by iteratively moving in
    the direction of steepest descent as defined by the negative of
    the gradient. We use gradient descent to update the parameters
    (weights) of our model.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).
    - theta: The parameters (weights) of the model being learned.
    - alpha: The learning rate of your model.
    - num_iters: The number of updates performed.

    Returns:
    - theta: The learned parameters of your model.
    - J_history: the loss value for every iteration.
    """

    theta = theta.copy()  # optional: theta outside the function will not change
    J_history = []  # Use a python list to save the cost value in every iteration
    m = X.shape[0]  # 4000
    for i in range(0, num_iters):
        J_history.append(compute_cost(X, y, theta))
        h_zero_vector = X.dot(theta)
        deviation = h_zero_vector - y
        theta = theta - (alpha / m) * np.dot(X.transpose(), deviation)

    return theta, J_history


def compute_pinv(X, y):
    """
    Compute the optimal values of the parameters using the pseudoinverse
    approach as you saw in class using the training set.

    #########################################
    #### Note: DO NOT USE np.linalg.pinv ####
    #########################################

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).

    Returns:
    - pinv_theta: The optimal parameters of your model.
    """

    pinv_theta = []
    pinv_X = np.dot(np.linalg.inv((np.dot(X.transpose(), X))), X.transpose())
    pinv_theta = np.dot(pinv_X, y)
    return pinv_theta


def efficient_gradient_descent(X, y, theta, alpha, num_iters):
    """
    Learn the parameters of your model using the training set, but stop
    the learning process once the improvement of the loss value is smaller
    than 1e-8. This function is very similar to the gradient descent
    function you already implemented.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).
    - theta: The parameters (weights) of the model being learned.
    - alpha: The learning rate of your model.
    - num_iters: The number of updates performed.

    Returns:
    - theta: The learned parameters of your model.
    - J_history: the loss value for every iteration.
    """
    m = X.shape[0]  # 4000
    theta = theta.copy()  # optional: theta outside the function will not change
    J_history = []  # Use a python list to save the cost value in every iteration
    current = np.inf
    i = 0
    J_history.append(compute_cost(X, y, theta))
    h_zero_vector = X.dot(theta)
    deviation = h_zero_vector - y
    theta = theta - (alpha / m) * np.dot(X.transpose(), deviation)
    while i <= num_iters - 1 and current - J_history[i] >= 1e-8:
        J_history.append(compute_cost(X, y, theta))
        current = J_history[i]
        i += 1
        h_zero_vector = X.dot(theta)
        deviation = h_zero_vector - y
        theta = theta - (alpha / m) * np.dot(X.transpose(), deviation)

    return theta, J_history


def find_best_alpha(X_train, y_train, X_val, y_val, iterations):
    """
    Iterate over the provided values of alpha and train a model using
    the training dataset. maintain a python dictionary with alpha as the
    key and the loss on the validation set as the value.

    You should use the efficient version of gradient descent for this part.

    Input:
    - X_train, y_train, X_val, y_val: the training and validation data
    - iterations: maximum number of iterations

    Returns:
    - alpha_dict: A python dictionary - {alpha_value : validation_loss}
    """

    alphas = [0.00001, 0.00003, 0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 2, 3]
    alpha_dict = {}  # {alpha_value: validation_loss}

    for alpha in alphas:
        np.random.seed(42)
        theta = np.random.random(size=2)
        theta = efficient_gradient_descent(X_train, y_train, theta, alpha, iterations)[0]
        cost = compute_cost(X_val, y_val, theta)
        alpha_dict.update({alpha: cost})

    return alpha_dict


def forward_feature_selection(X_train, y_train, X_val, y_val, best_alpha, iterations):
    """
    Forward feature selection is a greedy, iterative algorithm used to
    select the most relevant features for a predictive model. The objective
    of this algorithm is to improve the model's performance by identifying
    and using only the most relevant features, potentially reducing overfitting,
    improving accuracy, and reducing computational cost.

    You should use the efficient version of gradient descent for this part.

    Input:
    - X_train, y_train, X_val, y_val: the input data without bias trick
    - best_alpha: the best learning rate previously obtained
    - iterations: maximum number of iterations for gradient descent

    Returns:
    - selected_features: A list of selected top 5 feature indices
    """
    selected_features = []
    n = X_train.shape[1]  # 17

    while len(selected_features) < 5:
        min_cost = []
        for i in range(0, n):
            current_indices = list(set(selected_features + [i]))
            current_feature_train = X_train[:, current_indices]
            current_feature_train = apply_bias_trick(current_feature_train)
            current_feature_val = X_val[:, current_indices]
            current_feature_val = apply_bias_trick(current_feature_val)
            np.random.seed(42)
            theta = np.random.random(size=current_feature_train.shape[1])
            theta = efficient_gradient_descent(current_feature_train, y_train, theta, best_alpha, iterations)[0]
            min_cost.append(compute_cost(current_feature_val, y_val, theta))

        min_idx = min_cost.index(min(min_cost))
        while min_idx in selected_features:
            min_cost[min_idx] = np.inf
            min_idx = min_cost.index(min(min_cost))
        selected_features.append(min_idx)

    return selected_features


def create_square_features(df):
    """
    Create square features for the input data.

    Input:
    - df: Input data (m instances over n features) as a dataframe.

    Returns:
    - df_poly: The input data with polynomial features added as a dataframe
               with appropriate feature names
    """

    df_poly = df.copy()
    n = len(df_poly.columns)
    for i in range(0, n):
        column_i_name = df_poly.columns[i]
        for j in range(i, n):
            column_j_name = df_poly.columns[j]
            if i == j:
                new_column_name = df_poly.columns[i] + "^2"
            else:
                new_column_name = df_poly.columns[i] + "*" + df_poly.columns[j]

            df_poly.insert(len(df_poly.columns), new_column_name, df_poly[column_i_name] * df_poly[column_j_name])

    return df_poly
