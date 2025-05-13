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
            nearest_object, _ = ray.nearest_intersected_object(objects)

            if nearest_object is not None:
                intersection_point = camera + nearest_object.intersect(ray)[0] * ray.direction

                if intersection_point is not None:
                    color = get_color(objects, ambient, lights, nearest_object, ray, intersection_point, max_depth, 1)

            # We clip the values between 0 and 1 so all pixel values will make sense.
            image[i, j] = np.clip(color,0,1)

    return image

def get_color(objects, ambient, lights, nearest_object, ray, intersection_point, max_depth, depth):
    normal = nearest_object.compute_normal(intersection_point)
    intersection_point += normal * EPSILON  # avoid self-intersection (due to floating point errors)
    
    # I_a = K_A * I_amb
    color = (nearest_object.ambient * ambient).astype(np.float64)

    # using only unblocked lights
    unblocked_lights = []
    for light in lights:
        if not light.is_light_source_blocked(objects, normal, intersection_point):
            unblocked_lights.append(light)

    for light in lights:
        # diffuse reflection
        # I_d = K_D * I_L * (N . L)
        diffuse = (light.get_intensity(intersection_point) * nearest_object.diffuse *
                   max(0, np.dot(light.get_light_ray(intersection_point).direction, normal)))

        # specular reflection
        # I_s = K_S * I_L * (L' . V) ^ n
        specular = (nearest_object.specular * light.get_intensity(intersection_point) *
                    max(0, np.dot(normalize(reflected(-light.get_light_ray(intersection_point).direction, normal)),
                            -ray.direction)) ** nearest_object.shininess)

        color += diffuse + specular

    # update and check recursion depth
    depth += 1
    if depth > max_depth:
        return color

    # handle reflection
    if nearest_object.reflection > 0:
        r_ray = Ray(intersection_point, reflected(ray.direction, normal))
        r_nearest_object, min_distance = r_ray.nearest_intersected_object(objects)

        if r_nearest_object is not None:
            r_intersection_point = intersection_point + min_distance * r_ray.direction
            # I_r = K_R * I_R
            color += nearest_object.reflection * get_color(objects, ambient, lights, r_nearest_object, r_ray,
                                                     r_intersection_point, max_depth, depth)
    

    # Add diffuse and specular components for each unblocked light
    for light in unblocked_lights:
        light_ray = light.get_light_ray(offset_point)
        
        # Calculate diffuse component: K_D * I_L * (N · L)
        diffuse_factor = max(0, np.dot(normal, light_ray.direction))
        diffuse = light.get_intensity(offset_point) * nearest_object.diffuse * diffuse_factor
        
        # Calculate specular component: K_S * I_L * (R · V)^n
        view_dir = normalize(-ray.direction)
        light_dir = normalize(light_ray.direction)
        reflect_dir = reflected(-light_dir, normal)
        specular_factor = max(0, np.dot(view_dir, reflect_dir)) ** nearest_object.shininess
        specular = light.get_intensity(offset_point) * nearest_object.specular * specular_factor
        
        color += diffuse + specular
    
    # Stop recursion if we've reached max depth
    depth += 1
    if depth > max_depth:
        return color
    
    # Handle reflection
    reflection_color = np.zeros(3)
    if nearest_object.reflection > 0:
        # Calculate reflection ray
        reflect_dir = reflected(ray.direction, normal)
        reflect_ray = Ray(offset_point, reflect_dir, 0)
        
        # Find the object hit by reflection ray
        hit_obj, hit_dist = reflect_ray.nearest_intersected_object(objects)
        if hit_obj is not None:
            hit_point = reflect_ray.origin + hit_dist * reflect_ray.direction
            reflection_color = get_color(objects, ambient, lights, hit_obj, reflect_ray,
                                         hit_point, max_depth, depth)
    
    # Handle refraction/transparency
    refraction_color = np.zeros(3)
    if nearest_object.refraction > 0:
        # Determine refractive indices
        if is_inside:
            # Exiting the object
            n1 = nearest_object.refractive_index
            n2 = 1.0  # Air
        else:
            # Entering the object
            n1 = 1.0  # Air
            n2 = nearest_object.refractive_index
        
        # Calculate refraction direction using Snell's law
        ratio = n1 / n2
        cos_i = np.dot(-normal, ray.direction)
        k = 1 - ratio * ratio * (1 - cos_i * cos_i)
        
        if k >= 0:  # No total internal reflection
            # Calculate refracted direction
            refract_dir = ratio * ray.direction + (ratio * cos_i - np.sqrt(k)) * normal
            refract_dir = normalize(refract_dir)
            
            # Create refracted ray slightly offset along refraction direction
            refract_ray = Ray(intersection_point + refract_dir * EPSILON, refract_dir, n2)
            
            # Find the object hit by refraction ray
            hit_obj, hit_dist = refract_ray.nearest_intersected_object(objects)
            if hit_obj is not None:
                hit_point = refract_ray.origin + hit_dist * refract_ray.direction
                refraction_color = get_color(objects, ambient, lights, hit_obj, refract_ray,
                                           hit_point, max_depth, depth)
    
    # Combine colors according to material properties
    # Fresnel effect could be added here for more realism
    color += nearest_object.reflection * reflection_color + nearest_object.refraction * refraction_color
    
    return color
# def get_color(objects, ambient, lights, nearest_object, ray, intersection_point, max_depth, depth):
#     normal = nearest_object.compute_normal(intersection_point)
#     intersection_point += normal * EPSILON  # avoid self-intersection (due to floating point errors)
    
#     # I_a = K_A * I_amb
#     color = (nearest_object.ambient * ambient).astype(np.float64)

#     # using only unblocked lights
#     lights = [light for light in lights if not light.is_light_source_blocked(objects, normal, intersection_point)]

#     for light in lights:
#         # diffuse reflection
#         # I_d = K_D * I_L * (N . L)
#         diffuse = (light.get_intensity(intersection_point) * nearest_object.diffuse *
#                    max(0, np.dot(light.get_light_ray(intersection_point).direction, normal)))

#         # specular reflection
#         # I_s = K_S * I_L * (L' . V) ^ n
#         specular = (nearest_object.specular * light.get_intensity(intersection_point) *
#                     max(0, np.dot(normalize(reflected(-light.get_light_ray(intersection_point).direction, normal)),
#                             -ray.direction)) ** nearest_object.shininess)

#         color += diffuse + specular

#     # update and check recursion depth
#     depth += 1
#     if depth > max_depth:
#         return color

#     # handle reflection
#     if nearest_object.reflection > 0:
#         r_ray = Ray(intersection_point, reflected(ray.direction, normal))
#         r_nearest_object, min_distance = r_ray.nearest_intersected_object(objects)

#         if r_nearest_object is not None:
#             r_intersection_point = intersection_point + min_distance * r_ray.direction
#             # I_r = K_R * I_R
#             color += nearest_object.reflection * get_color(objects, ambient, lights, r_nearest_object, r_ray,
#                                                      r_intersection_point, max_depth, depth)
    
#     # handle refraction
#     if nearest_object.refraction > 0:
#         t_ray = construct_refracted_ray(ray, intersection_point, normal, nearest_object.refractive_index)
        
#         if t_ray is not None:
#             t_nearest_object, min_distance = t_ray.nearest_intersected_object(objects)
            
#             if t_nearest_object is not None:
#                 t_intersection_point = t_ray.origin + min_distance * t_ray.direction
#                 # I_t = K_T * I_T
#                 color += nearest_object.refraction * get_color(objects, ambient, lights, t_nearest_object, t_ray,
#                                                          t_intersection_point, max_depth, depth)

#     return color
# def get_color(objects, ambient, lights, nearest_object, ray, intersection_point, max_depth, depth):
#     normal = nearest_object.compute_normal(intersection_point)
#     intersection_point += normal * EPSILON  # avoid self-intersection (due to floating point errors)
    
#     # I_a = K_A * I_amb
#     color = (nearest_object.ambient * ambient).astype(np.float64)

#     # using only unblocked lights
#     lights = [light for light in lights if not light.is_light_source_blocked(objects, normal, intersection_point)]

#     for light in lights:
#         # diffuse reflection
#         # I_d = K_D * I_L * (N . L)
#         diffuse = (light.get_intensity(intersection_point) * nearest_object.diffuse *
#                    max(0, np.dot(light.get_light_ray(intersection_point).direction, normal)))

#         # specular reflection
#         # I_s = K_S * I_L * (L' . V) ^ n
#         specular = (nearest_object.specular * light.get_intensity(intersection_point) *
#                     max(0, np.dot(normalize(reflected(-light.get_light_ray(intersection_point).direction, normal)),
#                             -ray.direction)) ** nearest_object.shininess)

#         color += diffuse + specular

#     depth += 1
#     if depth > max_depth:
#         return color

#     # reflected ray from the object
#     r_ray = Ray(intersection_point, reflected(ray.direction, normal))
#     nearest_object, _ = r_ray.nearest_intersected_object(objects)

#     if nearest_object and nearest_object.reflection > 0:
#         if nearest_object and nearest_object.intersect(r_ray):
#             r_intersection_point = intersection_point + nearest_object.intersect(r_ray)[0] * r_ray.direction
            
#             # I_r = K_R * I_R
#             color += nearest_object.reflection * get_color(objects, ambient, lights, nearest_object, r_ray,
#                                                         r_intersection_point, max_depth, depth)
        
#     # refracted ray from the object
#     # todo: fake snell calc?

#     if nearest_object and nearest_object.refraction > 0:
#         t_ray = construct_refracted_ray(ray, intersection_point, normal, nearest_object.refractive_index)
#         if nearest_object and nearest_object.intersect(t_ray):
#             t_intersection = intersection_point + nearest_object.intersect(t_ray)[0] * t_ray.direction
#             if t_intersection is not None:
#                 t_color = get_color(objects, ambient, lights, nearest_object, t_ray, t_intersection, max_depth, depth)
#                 color += t_color * nearest_object.refraction

#         # t_ray = Ray(intersection_point, ray.direction)
#         # nearest_object, _ = t_ray.nearest_intersected_object(objects)
        
#         # if nearest_object and nearest_object.intersect(t_ray):
#         #     t_intersection_point = intersection_point + nearest_object.intersect(t_ray)[0] * t_ray.direction

#         #     # I_t = K_T * I_T
#         #     color += nearest_object.refraction * get_color(objects, ambient, lights, nearest_object, t_ray,
#         #                                                     t_intersection_point, max_depth, depth)

#     return color


# Write your own objects and lights
# TODO
def your_own_scene():
    # materials - (ambient, diffuse, specular, shininess, reflection, transparency, refractive_index)
    glass_material = ([0.1, 0.1, 0.2], [0.1, 0.1, 0.2], [0.5, 0.5, 0.8], 100, 0.1, 0.9, 1.5)
    sphere_material = ([0.1, 0.8, 0.1], [0.1, 0.8, 0.1], [0.2, 0.2, 0.2], 20, 0.3, 0.0)#, 1.0)
    floor_material = ([0.5, 0.5, 0.5], [0.3, 0.3, 0.3], [0.1, 0.1, 0.1], 10, 0.2, 0.0)#, 1.0)
    
    # outer transparent object (sphere)
    outer_sphere = Sphere([0, 0, -1], 0.6)
    outer_sphere.set_material(*glass_material)

    # extra object: sphere behind sphere
    behind_sphere = Sphere([0.7, 0, -1.5], 0.3)
    behind_sphere.set_material(*sphere_material)

    # plane (floor)
    floor = Plane([0, 1, 0], [0, -0.6, 0])
    floor.set_material(*floor_material)

    # lights
    point_light = PointLight(intensity=np.array([1, 0.9, 0.9]), position=np.array([1, 1, 1]), kc=0.1, kl=0.1, kq=0.1)
    dir_light = DirectionalLight(intensity=np.array([0.8, 0.8, 1]), direction=np.array([-1, -1, -1]))

    # create scene
    objects = [outer_sphere, behind_sphere, floor]
    lights = [point_light, dir_light]
    camera = np.array([0, 0, 1])

    return camera, lights, objects
