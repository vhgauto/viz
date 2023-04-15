
# paquetes ----------------------------------------------------------------

library(sp)
library(rgeos)
library(maptools)
library(rgdal)

browseURL("https://pappubahry.com/misc/rectangles/code/")

browseURL("https://pappubahry.com/misc/rectangles/")

tau = 2*pi

pcia_shp = readShapePoly("provincia_rectangulo/Provincias.shp")

make_simple_poly = function(params, area) {
  # Function to return a rectangle as a SpatialPolygons object.
  # params is a 4-element vector.
  
  centroid = c(params[1], params[2])
  angle = params[3]
  aspect = params[4]
  
  lon_side_length = sqrt(area * aspect)
  lat_side_length = area / lon_side_length
  
  vertices = matrix(0, nrow=5, ncol=2)
  vertices[1, ] = c(-lon_side_length/2, -lat_side_length/2)
  vertices[2, ] = c(-lon_side_length/2, +lat_side_length/2)
  vertices[3, ] = c(lon_side_length/2, lat_side_length/2)
  vertices[4, ] = c(lon_side_length/2, -lat_side_length/2)
  vertices[5, ] = vertices[1, ]
  
  # Rotate:
  rot_mat = matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)),
                   nrow=2, ncol=2)
  
  vertices = t(rot_mat %*% t(vertices))
  
  # Centre:
  vertices[ , 1] = vertices[ , 1] + centroid[1]
  vertices[ , 2] = vertices[ , 2] + centroid[2]
  
  # Convert to SpatialPolygons:
  polygon_object = SpatialPolygons(list(Polygons(list(Polygon(vertices)), 0)))
  return(polygon_object)
}

num_features = length(pcia_shp$FNA)
overlaps = numeric(num_features)

# How many iterations before terminating/reducing the increment?
max_iter = 100

# Indices of the countries getting a second, third, ... run:
second_pass_i = c(26,28,31,41,44,45,53,55,59,63,66,72,80,93,96,102,116,118,123,124,135,139,140,
                  141,142,146,147,155,159,161,165,166,174,178,179,184,186,195,197,198,204,205,206)

third_pass_i = c(41,45,96,118,141,142,166)

fourth_pass_i = c(141,166)

fifth_pass_i = 141

second_pass = FALSE
third_pass = FALSE
fourth_pass = FALSE
fifth_pass = FALSE

if (fifth_pass) {
  fourth_pass = FALSE
  third_pass = FALSE
  second_pass = FALSE
}

if (fourth_pass) {
  third_pass = FALSE
  second_pass = FALSE
}

if (third_pass) {
  second_pass = FALSE
}

# ir de 10 en 10
for (j in 1:1) {
  if (fifth_pass) {
    i = fifth_pass_i[j]
  } else if (fourth_pass) {
    i = fourth_pass_i[j]
  } else if (third_pass) {
    i = third_pass_i[j]
  } else if (second_pass) {
    i = second_pass_i[j]
  } else {
    i = j
  }
  
  print(sprintf("Starting country %d: %s", i, pcia_shp$FNA[i]))
  iteration_printouts = character()
  
  # The country under study:
  this_shp = pcia_shp[i, ]
  
  # Get some initial parameters for the rectangle:
  centroid = gCentroid(this_shp)@coords
  bounding_box = gEnvelope(this_shp)
  bounding_box = bounding_box@bbox
  area = gArea(this_shp)
  aspect = (bounding_box[1, 2] - bounding_box[1, 1]) / (bounding_box[2, 2] - bounding_box[2, 1])
  
  angle = 0
  
  params = c(centroid[1],
             centroid[2],
             angle,
             aspect)
  
  # increments:
  inc = c((bounding_box[1, 2] - bounding_box[1, 1]) / 100,
          (bounding_box[2, 2] - bounding_box[2, 1]) / 100,
          1 * tau / 360,
          0.01)
  
  num_params = length(params)
  
  # this_rect will be the current rectangle
  this_rect = make_simple_poly(params, area)
  this_int = gIntersection(this_rect, this_shp)
  
  # In the case of a null intersection, gIntersection() returns NULL,
  # so we need to check !is.null before continuing (can't go ahead
  # and use gArea on a NULL).
  if (!is.null(this_int)) {
    this_overlap = gArea(this_int) / area
    
    # Counter for the iterations:
    iter_ct = 0
    
    # fine_incs will be TRUE after they get divided by 10:
    fine_incs = FALSE
    
    keep_iterating = TRUE
    
    # Initialising a couple of vectors for use in checking
    # if the algorithm is approximately converged:
    prev_prev_params = numeric(4)
    prev_params = numeric(4)
    
    while (keep_iterating) {
      if ((iter_ct == max_iter) && (!fine_incs)) {
        # Time to reduce the increments.
        inc = inc / 10
      }
      
      prev_overlap = this_overlap
      prev_prev_params = prev_params
      prev_params = params
      
      this_inc = numeric(num_params)
      
      for (i_param in 1:num_params) {
        # Increment the i-th parameter, see what the overlap is like:
        test_params = params
        test_params[i_param] = test_params[i_param] + inc[i_param]
        
        test_rect = make_simple_poly(test_params, area)
        test_int = gIntersection(test_rect, this_shp)
        
        if (!is.null(test_int)) {
          test_overlap = gArea(gIntersection(test_int, this_shp)) / area
          this_inc[i_param] = inc[i_param] * 2*(1*(test_overlap > prev_overlap) - 0.5)
        }
      }
      
      # Update the parameters:
      params = params + this_inc
      
      prev_rect = this_rect
      this_rect = make_simple_poly(params, area)
      this_overlap = gArea(gIntersection(this_rect, this_shp)) / area
      
      iteration_printouts = c(iteration_printouts,
                              sprintf("Step %d, overlap = %.3f, params = %.4f, %.4f, %.4f, %.4f",
                                      iter_ct, this_overlap, params[1], params[2], params[3], params[4]))
      
      print(iteration_printouts[iter_ct + 1])
      
      if (identical(params, prev_prev_params)) {
        if (fine_incs) {
          # Convergence!
          keep_iterating = FALSE
        } else {
          # Approximately converged, hopefully.
          inc = inc / 10
          fine_incs = TRUE
        }
      }
      
      
      iter_ct = iter_ct + 1
      if (iter_ct > 2*max_iter) {
        if (abs(this_overlap - prev_overlap) < 0.0005) {
          # I let it keep running a bit if it looks like
          # it's nowhere near converged.
          keep_iterating = FALSE
        } else {
          if (iter_ct > 5*max_iter) {
            keep_iterating = FALSE
          }
        }
      }
    }
    
    out_file = sprintf("provincia_rectangulo/images_no_title/%03d.png", i)
    out_file_iter = sprintf("provincia_rectangulo/iterations/%03d.txt", i)
    
    # For plotting, want to have both the rectangle
    # and the country within the axis bounds.
    bbox_rect = gEnvelope(this_rect)@bbox
    min_x = min(bbox_rect[1, 1], bounding_box[1, 1])
    max_x = max(bbox_rect[1, 2], bounding_box[1, 2])
    min_y = min(bbox_rect[2, 1], bounding_box[2, 1])
    max_y = max(bbox_rect[2, 2], bounding_box[2, 2])
    
    # 303x346 should give approximately a 200x200 plotting area.
    png(filename=out_file, width=303, height=346, units="px")
    plot(this_shp, xlim = c(min_x, max_x), ylim = c(min_y, max_y))
    plot(this_rect, add=TRUE)
    dev.off()
    
    out_conn = file(out_file_iter)
    writeLines(iteration_printouts, out_conn)
    close(out_conn)
    
    # Append the results to file:
    output_line = sprintf("%s: %.3f", pcia_shp$FNA[i], this_overlap)
    write(output_line, file="output.txt", append=TRUE)
    
    print(sprintf("%s: %.3f", pcia_shp$FNA[i], this_overlap))
  } else {
    # To be returned to later....
    print("No overlap!")
    print(pcia_shp$FNA[i])
    write(as.character(pcia_shp$FNA[i]), file="skipped_no_overlap.txt", append=TRUE)
  }
}
