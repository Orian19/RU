from helper_classes import *
import matplotlib.pyplot as plt

def render_scene(camera, ambient, lights, objects, screen_size, max_depth):
    width, height = screen_size
    ratio = float(width) / height
    screen = (-1, 1 / ratio, 1, -1 / ratio)  # left, top, right, bottom

    image = np.zeros((height, width, 3))

    for i, y in enumerate(np.linspace(screen[1], screen[3], height)):
        for j, x in enumerate(np.linspace(screen[0], screen[2], width)):
            # screen is on origin
            pixel = np.array([x, y, 0])
            origin = camera
            direction = normalize(pixel - origin)
            ray = Ray(origin, direction)

            color = np.zeros(3)

            # This is the main loop where each pixel color is computed.
            # TODO
            nearest_object, min_distance = ray.nearest_intersected_object(objects)
            intersection_point = camera + nearest_object.intersect(ray)[0] * ray.direction

            if intersection_point is not None:
                color = get_color(objects, color, ambient, lights, nearest_object, ray, intersection_point, max_depth, 1)

            # We clip the values between 0 and 1 so all pixel values will make sense.
            image[i, j] = np.clip(color,0,1)

    return image

def get_color(objects, color, ambient, lights, nearest_object, ray, intersection_point, max_depth, depth):
    normal = nearest_object.compute_normal(intersection_point)
    intersection_point += normal * EPSILON  # avoid self-intersection (due to floating point errors)
        
    # I_a = K_A * I_amb
    color += nearest_object.ambient * ambient

    # using only unblocked lights
    lights = [light for light in lights if not light.is_light_source_blocked(objects, normal, intersection_point)]
    
    for light in lights:
        # diffuse reflection
        diffuse = (light.get_intensity(intersection_point) * nearest_object.diffuse *
                   max(0, np.dot(light.get_light_ray(intersection_point).direction, normal)))

        # specular reflection
        specular = (nearest_object.specular * light.get_intensity(intersection_point) *
                    max(0, np.dot(normalize(reflected(-light.get_light_ray(intersection_point).direction, normal)),
                            -ray.direction)) ** nearest_object.shininess)

        color += diffuse + specular

    depth += 1
    if depth > max_depth:
        return color

    # reflected ray from the object
    # todo: maybe need to check reflection value > 0
    r_ray = Ray(intersection_point, reflected(ray.direction, normal))
    nearest_object, _ = ray.nearest_intersected_object(objects)
    r_intersection_point = intersection_point + nearest_object.intersect(r_ray)[0] * r_ray.direction
    
    if r_intersection_point:
        color += nearest_object.reflection * get_color(objects, color, ambient, lights, nearest_object, r_ray,
                                                       r_intersection_point, max_depth, depth)

    # refracted ray from the object
    # todo: maybe need to check refracted value > 0
    # todo: fake snell calc?
    t_ray = Ray(intersection_point, ray.direction)
    nearest_object, _ = ray.nearest_intersected_object(objects)
    t_intersection_point = intersection_point + nearest_object.intersect(t_ray)[0] * t_ray.direction

    if t_intersection_point:
        color += nearest_object.refraction * get_color(objects, color, ambient, lights, nearest_object, t_ray,
                                                       t_intersection_point, max_depth, depth)

    return color


# Write your own objects and lights
# TODO
def your_own_scene():
    camera = np.array([0,0,1])
    lights = []
    objects = []
    return camera, lights, objects
