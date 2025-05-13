import numpy as np

EPSILON = 1e-5

# This function gets a vector and returns its normalized form.
def normalize(vector):
    return vector / np.linalg.norm(vector)


# This function gets a vector and the normal of the surface it hit
# This function returns the vector that reflects from the surface
def reflected(vector, axis):
    v = np.array([0,0,0])

    # R = L - 2(L*N)N
    v = vector - 2 * np.dot(vector, axis) * axis

    return v

## Lights


class LightSource:
    def __init__(self, intensity):
        self.intensity = intensity

    def get_light_ray(self, intersection_point):
        pass

    def get_distance_from_light(self, intersection):
        pass

    def get_intensity(self, intersection):
        pass
    
    def is_light_source_blocked(self, objects, normal, intersection_point):
        ray = self.get_light_ray(intersection_point)

        # check if the ray intersects with any object
        nearest_object, min_distance = ray.nearest_intersected_object(objects)
        if nearest_object is None:
            return False

        # the light source is not behind the object
        if ray.direction.dot(normal) < 0:
            return True

        # the light source is blocked by the object (the object is between the light source and the intersection point)
        if min_distance >= self.get_distance_from_light(intersection_point):
            return False

        return True

class DirectionalLight(LightSource):

    def __init__(self, intensity, direction):
        super().__init__(intensity)
        self.direction = normalize(np.array(direction))

    # This function returns the ray that goes from a point to a light source
    def get_light_ray(self, intersection_point):
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
        # intersections = [obj.intersect(self) for obj in objects]
        intersections = []
        for obj in objects:
            intersections.append(obj.intersect(self))

        nearest_object = None
        min_distance = np.inf

        for intersection in intersections:
            if intersection is not None:
                distance, obj = intersection
                if distance < min_distance:
                    min_distance = distance
                    nearest_object = obj

        return nearest_object, min_distance


class Object3D:
    def set_material(self, ambient, diffuse, specular, shininess, reflection, refraction=0):
        self.ambient = ambient
        self.diffuse = diffuse
        self.specular = specular
        self.shininess = shininess
        self.reflection = reflection
        self.refraction = refraction

    def compute_normal(self, intersection):
        pass


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

    def compute_normal(self, intersection=None):
        return self.normal


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
        self.ab, self.ac = self.compute_plane_vectors()
        self.normal = self.compute_normal()
        self.triangle = Plane(self.normal, self.a)

    def compute_plane_vectors(self):
        # using the triangle vertices to get the plane vectors for parametric representation
        u = self.b - self.a
        v = self.c - self.a

        return u, v

    # computes normal to the triangle surface. Pay attention to its direction!
    def compute_normal(self, intersection=None):
        # using the plane vectors and cross-product to get the normal vector
        return normalize(np.cross(self.ab, self.ac))

    def intersect(self, ray: Ray):
        # checking if the ray is parallel to the plane (which happens when N . rayD = 0)
        denom = np.dot(self.normal, ray.direction)
        if abs(denom) < EPSILON:
            return None

        # for the point to be on the plane:
        # N * (p - a) = 0, where N - normal, p - O + tD, a - point on the plane
        # opening the equation we get:
        # N * p = N * a = d
        # =>  N * (O + tD) = d  => N * O + t(N*D) = d
        # =>  t = (d - N*O) / (N*D)
        d = np.dot(self.normal, self.a)
        t = (d - np.dot(self.normal, ray.origin)) / denom

        # if the intersection is behind the ray
        if t < 0:
            return None

        # after we checked the point is on the plane and the ray is no parallel,
        # we can use the Barycentric coordinates to check if the point is also on the triangle
        p = ray.origin + t * ray.direction

        abc_total_area = self.compute_triangle_area(self.ab, self.ac)

        pa = self.a - p
        pb = self.b - p
        pc = self.c - p

        # Barycentric coordinate
        alpha = self.compute_triangle_area(pb, pc) / abc_total_area
        beta = self.compute_triangle_area(pc, pa) / abc_total_area
        gamma = self.compute_triangle_area(pa, pb) / abc_total_area
    
        # validating the point is on the triangle
        if 0 <= alpha <= 1 and 0 <= beta <= 1 and 0 <= gamma <= 1 and abs(alpha + beta + gamma - 1) < EPSILON:
            return t, self
        else:
            return None

    @staticmethod
    def compute_triangle_area(u, v):
        return np.linalg.norm(np.cross(u, v)) / 2

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
        nearest_object, min_distance = ray.nearest_intersected_object(self.triangle_list)
        if nearest_object is not None:
            return min_distance, nearest_object

        return None

class Sphere(Object3D):
    def __init__(self, center, radius: float):
        self.center = center
        self.radius = radius

    def compute_normal(self, intersection):
        # normal is the vector from the center to the intersection point
        return normalize(intersection - self.center)

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

            # if t1 and t2 are both negative, there is no intersection
            valid_ts = [t for t in (t1, t2) if t > 0]
            if valid_ts:
                return min(valid_ts), self

        return None
