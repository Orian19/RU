import numpy as np

def get_random_centroids(X, k):
    '''
    Each centroid is a point in RGB space (color) in the image. 
    This function should uniformly pick `k` centroids from the dataset.
    Input: a single image of shape `(num_pixels, 3)` and `k`, the number of centroids. 
    Notice we are flattening the image to a two dimentional array.
    Output: Randomly chosen centroids of shape `(k,3)` as a numpy array. 
    '''
    centroids = []
    points = np.random.randint(0,X.shape[0], k)
    centroids = X[points, :]
    # make sure you return a numpy array
    return np.asarray(centroids).astype(np.float64) 

def lp_distance(X, centroids, p=2):
    '''
    Inputs: 
    A single image of shape (num_pixels, 3)
    The centroids (k, 3)
    The distance parameter p

    output: numpy array of shape `(k, num_pixels)` thats holds the distances of 
    all points in RGB space from all centroids
    '''
    distances = []
    m, n = X.shape
    k = len(centroids)
    for index in range(k):
        c_sum = np.sum(np.abs(X - centroids[index] * np.ones((m,n))**p),axis=1)
        distances.append(np.power(c_sum, 1 / p))

    distances = np.asarray(distances).astype(np.float64)
    return distances

def kmeans(X, k, p ,max_iter=100):
    """
    Inputs:
    - X: a single image of shape (num_pixels, 3).
    - k: number of centroids.
    - p: the parameter governing the distance measure.
    - max_iter: the maximum number of iterations to perform.

    Outputs:
    - The calculated centroids as a numpy array.
    - The final assignment of all RGB points to the closest centroids as a numpy array.
    """
    classes = []
    centroids = get_random_centroids(X, k)
    i = 0
   
    while i < max_iter:
        distances = lp_distance(X,centroids,p)
        # taking the index of the instance that has the minimum distance to some cluster
        classes = np.argmin(distances,axis=0)
        current_mus = centroids
        new_centroids = np.ones_like(centroids)
        for i in range(k):
            # points of specific cluster
            cluster_points = X[classes == i]
            
            if cluster_points.shape[0] > 0:
                new_centroids[i] = np.mean(cluster_points, axis=0)
            # in the case when we have less then 4 clusters
            else:
                new_centroids[i] = current_mus[i]
        # updating centroids       
        centroids = new_centroids
        if (centroids == current_mus).all():
            break
        i += 1
    return centroids, classes

def kmeans_pp(X, k, p ,max_iter=100):
    """
    Your implenentation of the kmeans++ algorithm.
    Inputs:
    - X: a single image of shape (num_pixels, 3).
    - k: number of centroids.
    - p: the parameter governing the distance measure.
    - max_iter: the maximum number of iterations to perform.

    Outputs:
    - The calculated centroids as a numpy array.
    - The final assignment of all RGB points to the closest centroids as a numpy array.
    """
    X_copy = X.copy()
    classes = []
    centroids = []
    rand = np.random.randint(0,X_copy.shape[0], 1)
    centroid = X_copy[rand]
    # initialize maximal distance to infinity to not affect the min operation for the first iteration
    total_square_distance = np.full(X.shape[0], np.inf) 
    for _ in range(k):
        centroids.append(centroid)
        # taking the minimum distance
        total_square_distance = np.minimum(lp_distance(X.copy(),centroids,p),total_square_distance)[0]
        # calculating the probability distribution weight
        prob_weight = total_square_distance**2 / np.sum(total_square_distance**2)
        centroid = X_copy[np.random.choice(len(X_copy), p=prob_weight)]


    # running k-mean with the init cetroids we found
    i = 0
    centroids = np.vstack(centroids)
    while i < max_iter:
        distances = lp_distance(X,centroids,p)
        # taking the index of the instance that has the minimum distance to some cluster
        classes = np.argmin(distances,axis=0)
        current_mus = centroids
        new_centroids = np.ones_like(centroids)
        for i in range(k):
            # points of specific cluster
            cluster_points = X[classes == i]
            
            if cluster_points.shape[0] > 0:
                new_centroids[i] = np.mean(cluster_points, axis=0)
            # in the case when we have less then 4 clusters
            else:
                new_centroids[i] = current_mus[i]
        # updating centroids       
        centroids = new_centroids
        if (centroids == current_mus).all():
            break
        i += 1
    return centroids, classes
