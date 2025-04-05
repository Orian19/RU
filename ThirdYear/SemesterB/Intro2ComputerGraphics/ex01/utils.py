import numpy as np
from PIL import Image
from numba import jit
from tqdm import tqdm
from abc import abstractmethod, abstractstaticmethod
from os.path import basename
from typing import List
import functools
import copy

    
def NI_decor(fn):
    def wrap_fn(self, *args, **kwargs):
        try:
            return fn(self, *args, **kwargs)
        except NotImplementedError as e:
            print(e)
    return wrap_fn


class SeamImage:
    def __init__(self, img_path: str, vis_seams: bool=True):
        """ SeamImage initialization.

        Parameters:
            img_path (str): image local path
            vis_seams (bool): if true, another version of the original image shall be store, and removed seams should be marked on it
        """
        #################
        # Do not change #
        #################
        self.path = img_path
        
        self.gs_weights = np.array([[0.299, 0.587, 0.114]]).T
        
        self.rgb = self.load_image(img_path)
        self.resized_rgb = self.rgb.copy()

        self.vis_seams = vis_seams
        if vis_seams:
            self.seams_rgb = self.rgb.copy()
        
        self.h, self.w = self.rgb.shape[:2]
        
        try:
            self.gs = self.rgb_to_grayscale(self.rgb)
            self.resized_gs = self.gs.copy()
            # self.cumm_mask = np.ones_like(self.gs, dtype=bool)  # todo
            self.cumm_mask = np.ones(self.gs.shape[:2], dtype=bool)
        except NotImplementedError as e:
            print(e)

        try:
            self.E = self.calc_gradient_magnitude()
        except NotImplementedError as e:
            print(e)
        #################

        # additional attributes you might find useful
        self.seam_history = []
        self.seam_balance = 0

        # This might serve you to keep tracking original pixel indices 
        self.idx_map_h, self.idx_map_v = np.meshgrid(range(self.w), range(self.h))

    @NI_decor
    def rgb_to_grayscale(self, np_img):
        """ Converts a np RGB image into grayscale (using self.gs_weights).
        Parameters
            np_img : ndarray (float32) of shape (h, w, 3) 
        Returns:
            grayscale image (float32) of shape (h, w, 1)

        Guidelines & hints:
            Use NumpyPy vectorized matrix multiplication for high performance.
            To prevent outlier values in the boundaries, we recommend to pad them with 0.5
        """
        # add padding to the image
        np_img_padded = np.pad(np_img, pad_width=((1, 1), (1, 1), (0, 0)), mode='constant', constant_values=0.5)
        gs_weights = self.gs_weights.reshape(1, 1, 3)  # (1,1,3)

        # calculate the grayscale image
        gs_img = np.sum(np_img_padded * gs_weights, axis=2)  # (341, 512)
        gs_img = gs_img[1:-1, 1:-1, np.newaxis]  # (341, 512, 1)

        return gs_img

    @NI_decor
    def calc_gradient_magnitude(self):
        """ Calculate gradient magnitude of a grayscale image

        Returns:
            A gradient magnitude image (float32) of shape (h, w)

        Guidelines & hints:
            - In order to calculate a gradient of a pixel, only its neighborhood is required.
            - keep in mind that values must be in range [0,1]
            - np.gradient or other off-the-shelf tools are NOT allowed, however feel free to compare yourself to them
        """
        gradient_x = self.resized_gs[:, 1:] - self.resized_gs[:, :-1]
        gradient_y = self.resized_gs[1:, :] - self.resized_gs[:-1, :]

        gradient_x = np.pad(gradient_x, ((0, 0), (0, 1), (0, 0)), mode='constant', constant_values=0)
        gradient_y = np.pad(gradient_y, ((0, 1), (0, 0), (0, 0)), mode='constant', constant_values=0)

        # the magnitude of the gradient
        gradient_magnitude = np.squeeze(np.sqrt(gradient_x ** 2 + gradient_y ** 2))
        gradient_magnitude = np.clip(gradient_magnitude, 0.0, 1.0)

        return gradient_magnitude.astype(np.float32)

    def update_ref_mat(self):
        for i, s in enumerate(self.seam_history[-1]):
            # self.idx_map[i, s:] += 1
            self.idx_map[i, s:] = np.roll(self.idx_map[i, s:], -1)

    def reinit(self):
        """
        Re-initiates instance and resets all variables.
        """
        self.__init__(img_path=self.path)

    @staticmethod
    def load_image(img_path, format='RGB'):
        return np.asarray(Image.open(img_path).convert(format)).astype('float32') / 255.0

    # def paint_seams(self):
    #     for s in self.seam_history:
    #         for i, s_i in enumerate(s):
    #             self.cumm_mask[self.idx_map_v[i,s_i], self.idx_map_h[i,s_i]] = False
    #     # cumm_mask_rgb = np.stack([np.squeeze(self.cumm_mask)] * 3, axis=2)  # todo: fix this
    #     cumm_mask_rgb = np.stack([self.cumm_mask] * 3, axis=2)  # todo: fix this
    #     self.seams_rgb = np.where(cumm_mask_rgb, self.seams_rgb, [1, 0, 0])

    def update_cumm_mask(self):  # todo change name
        min_seam = self.seam_history[-1]
        for i, s_i in enumerate(min_seam):
            self.cumm_mask[self.idx_map_v[i, s_i], self.idx_map_h[i, s_i]] = False

    def paint_seams(self, color=[1, 0, 0]):
        cumm_mask_rgb = np.stack([self.cumm_mask] * 3, axis=2)
        self.seams_rgb = np.where(cumm_mask_rgb, self.seams_rgb, color)

    def seams_removal(self, num_remove: int):
        """ Iterates num_remove times and removes num_remove vertical seams

        Parameters:
            num_remove (int): number of vertical seams to be removed

        Guidelines & hints:
        As taught, the energy is calculated from top to bottom.
        You might find the function np.roll useful.

        This step can be divided into a couple of steps:
            i) init/update matrices (E, mask) where:
                - E is the gradient magnitude matrix
                - mask is a boolean matrix for removed seams
            iii) find the best seam to remove and store it
            iv) index update: when a seam is removed, index mapping should be updated in order to keep track indices for next iterations
            v) seam removal: create the carved image with the chosen seam (and update seam visualization if desired)
            Note: the flow described below is a recommendation. You may implement seams_removal as you wish, but it needs to support:
            - removing seams a couple of times (call the function more than once)
            - visualize the original image with removed seams marked in red (for comparison)
        """
        for _ in tqdm(range(num_remove)):
            self.E = self.calc_gradient_magnitude()
            self.mask = np.ones_like(self.E, dtype=bool)

            seam = self.find_minimal_seam()
            self.seam_history.append(seam)
            if self.vis_seams:
                self.update_cumm_mask()  # todo: remove
                self.update_ref_mat()
            self.remove_seam(seam)

    @NI_decor
    def find_minimal_seam(self) -> List[int]:
        """
        Finds the seam with the minimal energy.
        Returns:
            The found seam, represented as a list of indexes
        """
        raise NotImplementedError("TODO: Implement SeamImage.find_minimal_seam in one of the subclasses")


    @NI_decor
    def remove_seam(self, seam: List[int]):
        """ Removes a seam from self.rgb (you may create a resized version, like self.resized_rgb)

        Guidelines & hints:
        In order to apply the removal, you might want to extend the seam mask to support 3 channels (rgb) using:
        3d_mak = np.stack([1d_mask] * 3, axis=2)
        ...and then use it to create a resized version.

        :arg seam: The seam to remove
        """
        self.w -= 1
        self.mask[np.arange(self.h), seam] = False

        mask_3d = np.stack([self.mask] * 3, axis=2)  # (h, w, 3)

        self.resized_rgb = self.resized_rgb[mask_3d].reshape(self.h, self.w, self.resized_rgb.shape[2])
        self.resized_gs = self.resized_gs[self.mask].reshape(self.h, self.w, self.resized_gs.shape[2])

    @NI_decor
    def add_seams(self, nums_add: int):
        self.w += nums_add
        self.resized_rgb = np.copy(self.rgb)
        self.resized_gs = np.copy(self.gs)

        for i in range(nums_add):
            self.resized_rgb = np.stack([
                np.insert(self.resized_rgb[row], self.seam_history[i][row] + 1, self.resized_rgb[row, self.seam_history[i][row]], axis=0)
                for row in range(self.h)
            ], axis=0)  # shape: (h, 3)

            self.resized_gs = np.stack([
                np.insert(self.resized_gs[row], self.seam_history[i][row] + 1,
                          self.resized_gs[row, self.seam_history[i][row]], axis=0)
                for row in range(self.h)
            ], axis=0)  # shape: (h, 1)

        self.w = self.resized_rgb.shape[1]
        self.h = self.resized_rgb.shape[0]

        self.cumm_mask = np.ones(self.resized_gs.shape[:2], dtype=bool)
        self.idx_map_h, self.idx_map_v = np.meshgrid(range(self.w), range(self.h))
        self.seams_rgb = np.copy(self.resized_rgb)

    @NI_decor
    def rotate_mats(self, clockwise: bool):
        """
        Rotates the matrices either clockwise or counter-clockwise.
        """
        self.rgb = np.rot90(self.rgb, k=-1 if clockwise else 1, axes=(0, 1))
        self.gs = np.rot90(self.gs, k=-1 if clockwise else 1, axes=(0, 1))
        self.resized_rgb = np.rot90(self.resized_rgb, k=-1 if clockwise else 1, axes=(0, 1))
        self.resized_gs = np.rot90(self.resized_gs, k=-1 if clockwise else 1, axes=(0, 1))
        self.E = np.rot90(self.E, k=-1 if clockwise else 1)
        self.cumm_mask = np.rot90(self.cumm_mask, k=-1 if clockwise else 1, axes=(0, 1))
        self.idx_map_h = np.rot90(self.idx_map_h, k=-1 if clockwise else 1)
        self.idx_map_v = np.rot90(self.idx_map_v, k=1 if clockwise else -1)
        self.seams_rgb = np.rot90(self.seams_rgb, k=-1 if clockwise else 1, axes=(0, 1))
        self.h, self.w = self.w, self.h
        self.idx_map_v, self.idx_map_h = self.idx_map_h, self.idx_map_v

    @NI_decor
    def seams_removal_vertical(self, num_remove: int):
        """ A wrapper for removing num_remove horizontal seams (just a recommendation)

        Parameters:
            num_remove (int): umber of vertical seam to be removed
        """
        self.idx_map = self.idx_map_h
        self.seams_removal(num_remove)

        # update seam visualization
        if self.vis_seams:
            self.paint_seams()

        self.seam_balance += num_remove

        self.seam_history = []

    @NI_decor
    def seams_removal_horizontal(self, num_remove: int):
        """ Removes num_remove horizontal seams by rotating the image, removing vertical seams, and restoring the original rotation.

        Parameters:
            num_remove (int): number of horizontal seam to be removed
        """
        # self.idx_map = np.copy(self.idx_map_v)  # todo: do this if it works
        # rotate the image
        self.rotate_mats(clockwise=True)
        self.idx_map = self.idx_map_h
        self.seams_removal(num_remove)

        # update seam visualization
        if self.vis_seams:
            self.paint_seams()

        # rotate back
        self.rotate_mats(clockwise=False)

        self.seam_balance += num_remove
        self.seam_history = []

    """
    BONUS SECTION
    """

    # todo: time complexity bonus in the notebook


    @NI_decor
    def seams_addition_horizontal(self, num_add: int):
        """ A wrapper for removing num_add horizontal seams (just a recommendation)

        Parameters:
            num_add (int): number of horizontal seam to be added

        Guidelines & hints:
            You may find np.rot90 function useful

        """
        self.rotate_mats(clockwise=True)
        self.idx_map = self.idx_map_h
        self.seams_removal(num_add)

        # update seam visualization
        if self.vis_seams:
            self.paint_seams(color=[0, 1, 0])

        self.add_seams(num_add)

        # rotate back
        self.rotate_mats(clockwise=False)

        self.seam_balance += num_add
        self.seam_history = []

    @NI_decor
    def seams_addition_vertical(self, num_add: int):
        """ A wrapper for removing num_add vertical seams (just a recommendation)

        Parameters:
            num_add (int): number of vertical seam to be added
        """
        self.idx_map = self.idx_map_h
        self.seams_removal(num_add)

        # update seam visualization
        if self.vis_seams:
            self.paint_seams(color=[0, 1, 0])

        self.seam_balance += num_add

        self.add_seams(num_add)
        self.seam_history = []


class GreedySeamImage(SeamImage):   # todo: compare final image solution
    """Implementation of the Seam Carving algorithm using a greedy approach"""
    @NI_decor
    def find_minimal_seam(self) -> List[int]:
        """
        Finds the minimal seam by using a greedy algorithm.

        Guidelines & hints:
        The first pixel of the seam should be the pixel with the lowest cost.
        Every row chooses the next pixel based on which neighbor has the lowest cost.
        """
        # Get the first pixel of the seam
        min_seam = [np.argmin(self.E[0])]

        # go over all the rows and find the next pixel
        for i in range(1, self.h):
            prev_x = min_seam[-1]
            curr_row = self.E[i]
            left = max(prev_x - 1, 0)
            right = min(prev_x + 1, self.w - 1)
            # add the index of the minimum pixel in the current row
            min_seam.append(np.argmin(curr_row[left:right + 1]) + left)

        return min_seam


class DPSeamImage(SeamImage):
    """
    Implementation of the Seam Carving algorithm using dynamic programming (DP).
    """
    def __init__(self, *args, **kwargs):
        """ DPSeamImage initialization.
        """
        super().__init__(*args, **kwargs)
        try:
            self.backtrack_mat = np.zeros_like(self.E, dtype=int)  # todo
            self.M = self.calc_M()
        except NotImplementedError as e:
            print(e)

    @NI_decor
    def find_minimal_seam(self) -> List[int]:
        """
        Finds the minimal seam by using dynamic programming.

        Guidelines & hints:
        As taught, the energy is calculated from top to bottom.
        You might find the function np.roll useful.

        This step can be divided into a couple of steps:
            i) init/update matrices (M, backtracking matrix) where:
                - M is the cost matrix
                - backtracking matrix is an idx matrix used to track the minimum seam from bottom up
            ii) fill in the backtrack matrix corresponding to M
            iii) seam backtracking: calculates the actual indices of the seam
        """
        self.init_mats()
        min_seam = []
        # find the minimum cost in the last row of M
        min_cost_idx = np.argmin(self.M[-1])

        # backtrack the seam from the last row to the first row and update mask
        for i in range(self.h - 1, -1, -1):
            min_seam.append(min_cost_idx)
            # update the backtrack index (-1 left, 0 middle, +1 right)
            next_direction = self.backtrack_mat[i][min_cost_idx]
            min_cost_idx = min_cost_idx + next_direction

        min_seam.reverse()

        return min_seam

    @NI_decor
    def calc_M(self):
        """ Calculates the matrix M discussed in lecture (with forward-looking cost)

        Returns:
            An energy matrix M (float32) of shape (h, w)

        Guidelines & hints:
            As taught, the energy is calculated from top to bottom.
            You might find the function 'np.roll' useful.
        """
        # C_L = |p_i-1,j, p_i,j-1| + |p_i-1,j - p_i+1,j|
        # C_M = |p_i-1,j, p_i,j-1|
        # C_R = |p_i+1,j, p_i,j-1| + |p_i-1,j - p_i+1,j|
        # calculate the C matrix with the resized rgb image and the resized gs image
        sqz_gs = np.squeeze(self.resized_gs)  # todo: time efficiency
        C_M = np.abs(np.roll(sqz_gs, shift=1, axis=1) - np.roll(sqz_gs, shift=-1, axis=1))
        C_L = C_M + np.abs(np.roll(sqz_gs, shift=1, axis=0) - np.roll(sqz_gs, shift=1, axis=1))
        C_R = C_M + np.abs(np.roll(sqz_gs, shift=1, axis=0) - np.roll(sqz_gs, shift=-1, axis=1))

        # handle the edges of the image
        C_L[:, 0] = np.inf
        C_R[:, -1] = np.inf

        M_ij = np.copy(self.E)
        M_ij[0] = self.E[0]  # first row is the same as E (base case)

        for i in range(1, self.h):
            left_shifted = np.roll(M_ij[i - 1], shift=1) + C_L[i]
            middle = M_ij[i - 1] + C_M[i]
            right_shifted = np.roll(M_ij[i - 1], shift=-1) + C_R[i]

            # stack all costs to find more easily the min
            cost_matrix = np.stack([left_shifted, middle, right_shifted], axis=0)  # (3, w)
            # find the min cost at each pixel
            min_indices = np.argmin(cost_matrix, axis=0)
            # update M_ij with the minimum cost
            M_ij[i] = self.E[i] + np.choose(min_indices, cost_matrix)

            # save backtracking indices (-1 for left, 0 for middle, +1 for right)
            self.backtrack_mat[i] = min_indices - 1   # todo: check if need absoulte indice and not relative
        return M_ij.astype(np.float32)

    def init_mats(self):
        self.backtrack_mat = np.zeros_like(self.E, dtype=int)
        self.M = self.calc_M()  # todo: switched order

    @staticmethod
    @jit(nopython=True)
    def calc_bt_mat(M, E, GS, backtrack_mat):
        """ Fills the BT back-tracking index matrix. This function is static in order to support Numba. To use it, uncomment the decorator above.

        Recommended parameters (member of the class, to be filled):
            M: np.ndarray (float32) of shape (h,w)
            E: np.ndarray (float32) of shape (h,w)
            GS: np.ndarray (float32) of shape (h,w)
            backtrack_mat: np.ndarray (int32) of shape (h,w): to be filled here

        Guidelines & hints:
            np.ndarray is a reference type. Changing it here may affect it on the outside.
        """
        # raise NotImplementedError("TODO: Implement DPSeamImage.calc_bt_mat")
        # h, w = M.shape
        pass
        # this method is implemented as part of the calc_M method


def scale_to_shape(orig_shape: np.ndarray, scale_factors: list):
    """ Converts scale into shape

    Parameters:
        orig_shape (np.ndarray): original shape [y,x]
        scale_factors (list): scale factors for y,x respectively

    Returns
        the new shape
    """
    return (orig_shape * np.array(scale_factors)).astype(int)


def resize_seam_carving(seam_img: SeamImage, shapes: np.ndarray):
    """ Resizes an image using Seam Carving algorithm

    Parameters:
        seam_img (SeamImage) The SeamImage instance to resize
        shapes (np.ndarray): desired shape (y,x)

    Returns
        the resized rgb image
    """
    image = copy.deepcopy(seam_img)
    original_h, original_w = shapes[0]
    new_h, new_w = shapes[1]

    # calculate the number of seams to remove/add
    num_remove_h = original_h - new_h
    num_remove_w = original_w - new_w

    # if num_remove_w > 0:  # todo
    image.seams_removal_vertical(num_remove_w)
    image.seams_removal_horizontal(num_remove_h)

    # if num_remove_h > 0:  # todo

    return image.resized_rgb


def bilinear(image, new_shape):
    """
    Resizes an image to new shape using bilinear interpolation method
    :param image: The original image
    :param new_shape: a (height, width) tuple which is the new shape
    :returns: the image resized to new_shape
    """
    in_height, in_width, _ = image.shape
    out_height, out_width = new_shape
    new_image = np.zeros(new_shape)
    ###Your code here###

    def get_scaled_param(org, size_in, size_out):
        scaled_org = (org * size_in) / size_out
        scaled_org = min(scaled_org, size_in - 1)
        return scaled_org

    scaled_x_grid = [get_scaled_param(x,in_width,out_width) for x in range(out_width)]
    scaled_y_grid = [get_scaled_param(y,in_height,out_height) for y in range(out_height)]
    x1s = np.array(scaled_x_grid, dtype=int)
    y1s = np.array(scaled_y_grid,dtype=int)
    x2s = np.array(scaled_x_grid, dtype=int) + 1
    x2s[x2s > in_width - 1] = in_width - 1
    y2s = np.array(scaled_y_grid,dtype=int) + 1
    y2s[y2s > in_height - 1] = in_height - 1
    dx = np.reshape(scaled_x_grid - x1s, (out_width, 1))
    dy = np.reshape(scaled_y_grid - y1s, (out_height, 1))
    c1 = np.reshape(image[y1s][:,x1s] * dx + (1 - dx) * image[y1s][:,x2s], (out_width, out_height, 3))
    c2 = np.reshape(image[y2s][:,x1s] * dx + (1 - dx) * image[y2s][:,x2s], (out_width, out_height, 3))
    new_image = np.reshape(c1 * dy + (1 - dy) * c2, (out_height, out_width, 3)).astype(int)
    return new_image
