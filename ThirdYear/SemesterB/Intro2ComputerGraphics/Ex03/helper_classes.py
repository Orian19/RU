import numpy as np


# This function gets a vector and returns its normalized form.
def normalize(vector):
    return vector / np.linalg.norm(vector)


# This function gets a vector and the normal of the surface it hit
# This function returns the vector that reflects from the surface
def reflected(vector, axis):
    # TODO:
    v = np.array([0,0,0])
    return v

## Lights


class LightSource:
    def __init__(self, intensity):
        self.intensity = intensity


class DirectionalLight(LightSource):

    def __init__(self, intensity, direction):
        super().__init__(intensity)
        self.direction = normalize(np.array(direction))

    # This function returns the ray that goes from the light source to a point
    def get_light_ray(self,intersection_point):
        # returning the opposite direction of the light source
        return Ray(intersection_point, -self.direction)

    # This function returns the distance from a point to the light source
    def get_distance_from_light(self, intersection):
        # directional light is defined as a source of light that is very far and shouldn't affect different points
        return np.inf

    # This function returns the light intensity at a point
    def get_intensity(self, intersection):
        # the intensity of the light is constant in directional light
        return self.intensity


class PointLight(LightSource):
    def __init__(self, intensity, position, kc, kl, kq):
        super().__init__(intensity)
        self.position = np.array(position)
        self.kc = kc
        self.kl = kl
        self.kq = kq

    # This function returns the ray that goes from a point to the light source
    def get_light_ray(self,intersection):
        # todo: why opposite direction? and not from light source to intersection?
        return Ray(intersection, normalize(self.position - intersection))

    # This function returns the distance from a point to the light source
    def get_distance_from_light(self, intersection):
        #TODO maybe should be intersection - self.position
        return np.linalg.norm(self.position - intersection)

    # This function returns the light intensity at a point
    def get_intensity(self, intersection):
        # calculate distance between light source and intersection 
        # calculate and return the light intensity based on kc, kl, kq
        # I_L = I_0 * 1 / f_att(d)

        d = self.get_distance_from_light(intersection)

        # f_att(d) = kq * d^2 + kl * d + kc
        f_att = self.kq * d**2 + self.kl * d + self.kc

        return self.intensity / f_att


class SpotLight(LightSource):
    def __init__(self, intensity, position, direction, kc, kl, kq):
        super().__init__(intensity)
        self.position = np.array(position)
        self.direction = normalize(np.array(direction))
        self.kc = kc
        self.kl = kl
        self.kq = kq

    # This function returns the ray that goes from the light source to a point
    def get_light_ray(self, intersection):
        return Ray(intersection, normalize(self.position - intersection))

    def get_distance_from_light(self, intersection):
        return np.linalg.norm(self.position - intersection)

    def get_intensity(self, intersection):
        d = self.get_distance_from_light(intersection)
        v = normalize(intersection - self.position)

        # I_L = I_0 * (v . v_d) / f_att(d)
        f_att = self.kq * d ** 2 + self.kl * d + self.kc

        return self.intensity * np.dot(self.direction, v) / f_att


class Ray:
    def __init__(self, origin, direction):
        self.origin = origin
        self.direction = direction

    # The function is getting the collection of objects in the scene and looks for the one with minimum distance.
    # The function should return the nearest object and its distance (in two different arguments)
    def nearest_intersected_object(self, objects):
        intersections = None
        nearest_object = None
        min_distance = np.inf
        #TODO
        return nearest_object, min_distance


class Object3D:
    def set_material(self, ambient, diffuse, specular, shininess, reflection):
        self.ambient = ambient
        self.diffuse = diffuse
        self.specular = specular
        self.shininess = shininess
        self.reflection = reflection


class Plane(Object3D):
    def __init__(self, normal, point):
        self.normal = np.array(normal)
        self.point = np.array(point)

    def intersect(self, ray: Ray):
        v = self.point - ray.origin
        t = np.dot(v, self.normal) / (np.dot(self.normal, ray.direction) + 1e-6)
        if t > 0:
            return t, self
        else:
            return None


class Triangle(Object3D):
    """
        C
        /\
       /  \
    A /____\ B

    The fornt face of the triangle is A -> B -> C.
    
    """
    def __init__(self, a, b, c):
        self.a = np.array(a)
        self.b = np.array(b)
        self.c = np.array(c)
        self.u, self.v = self.compute_plane_vectors()
        self.normal = self.compute_normal()
        self.triangle = Plane(self.normal, self.a)

    def compute_plane_vectors(self):
        # using the triangle vertices to get the plane vectors for parametric representation
        u = self.b - self.a
        v = self.c - self.a

        return u, v

    # computes normal to the triangle surface. Pay attention to its direction!
    def compute_normal(self):
        # using the plane vectors and cross-product to get the normal vector
        return np.cross(self.u, self.v)

    def intersect(self, ray: Ray):
        # finding the t
        # TODO
        return self.triangle.intersect(ray)


class Diamond(Object3D):
    """     
            D
            /\*\
           /==\**\
         /======\***\
       /==========\***\
     /==============\****\
   /==================\*****\
A /&&&&&&&&&&&&&&&&&&&&\ B &&&/ C
   \==================/****/
     \==============/****/
       \==========/****/
         \======/***/
           \==/**/
            \/*/
             E 
    
    Similar to Traingle, every from face of the diamond's faces are:
        A -> B -> D
        B -> C -> D
        A -> C -> B
        E -> B -> A
        E -> C -> B
        C -> E -> A
    """
    def __init__(self, v_list):
        self.v_list = v_list  # list of vertices
        self.triangle_list = self.create_triangle_list()

    def create_triangle_list(self):
        l = []
        t_idx = [
                [0,1,3],  # front face A -> B -> D
                [1,2,3],
                [0,3,2],
                 [4,1,0],
                 [4,2,1],
                 [2,4,0]]

        # go over the triangle indices and create triangles
        for i in t_idx:
            a = self.v_list[i[0]]
            b = self.v_list[i[1]]
            c = self.v_list[i[2]]
            triangle = Triangle(a, b, c)
            l.append(triangle)

        return l

    def apply_materials_to_triangles(self):
        # go over the triangles and apply the materials
        for triangle in self.triangle_list:
            triangle.set_material(self.ambient, self.diffuse, self.specular, self.shininess, self.reflection)

    def intersect(self, ray: Ray):
        # TODO
        pass

class Sphere(Object3D):
    def __init__(self, center, radius: float):
        self.center = center
        self.radius = radius

    def intersect(self, ray: Ray):
        # |P-C|^2 = r^2
        # |O + tD - C|^2 = r^2
        # a = D.D
        # b = 2D.(O-C)
        # c = |O-C|^2 - r^2
        # t = (-b +/- sqrt(b^2 - 4ac)) / 2a

        # if ray.direction is a zero vector, we can't find the intersection
        if np.dot(ray.direction, ray.direction) == 0:
            return None

        O = ray.origin
        C = self.center

        # quadratic coefficients after substituting the ray equation into the sphere equation
        a = np.dot(ray.direction, ray.direction)
        b = 2 * np.dot(ray.direction, O - C)
        c = np.dot(O - C, O - C) - self.radius ** 2
        discriminant = b ** 2 - 4 * a * c

        # if the discriminant is negative, there is no intersection
        if discriminant >= 0:
            t1 = (-b + np.sqrt(discriminant)) / (2 * a)
            t2 = (-b - np.sqrt(discriminant)) / (2 * a)

            # if t1 > 0 and t2 > 0:
            #     return min(t1, t2), self
            # elif t1 > 0:
            #     return t1, self
            # elif t2 > 0:
            #     return t2, self
            
            # if t1 and t2 are both negative, there is no intersection
            valid_ts = [t for t in (t1, t2) if t > 0]
            if valid_ts:
                return min(valid_ts), self

        return None


