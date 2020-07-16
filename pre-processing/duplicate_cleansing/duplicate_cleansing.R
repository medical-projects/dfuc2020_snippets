################################################################################
# LIBS
################################################################################

# Load libraries
library(sf)
library(foreach)


################################################################################
# CONFIG
################################################################################

# Set working directory
wd = "SET YOUR WORKING DIRECTORY HERE"
setwd(wd)

# Set ground truth here
input_file = "SET YOUR INPUT FILE HERE"

# Set identified duplicates and 2 column csv, see example file "duplicates.csv"
# Columns with headers: 
# name1,name2
# <duplicate file 1.1>,<duplicate file 1.2>
# <duplicate file 2.1>,<duplicate file 2.2>
# ...
duplicates_file = "SET YOU DUPLICATES FILE HERE"

# Set cleansed ground truth here
output_file = "SET OUTPUT FILE HERE" 

# Image sizes (training data has landscape format only)
img_w = 640
img_h = 480


################################################################################
# READING AND PRE-PROCESSING
################################################################################

# Ground truth
data_file = input_file
data = read.csv(file = data_file, sep = ",", header = T)
data.pre = data


# COORDINATE PLAUSIBILITY
################################################################################

# Assure plausibility
data.pre$xmin[data.pre$xmin < 0] = 0
data.pre$ymin[data.pre$ymin < 0] = 0
data.pre$xmax[data.pre$xmax >= img_w] = img_w - 1
data.pre$ymax[data.pre$ymax >= img_h] = img_h - 1


# CREATE POLYGONS
################################################################################

# Add sf polygons to data frame
data.bbs = data.frame(bb = c())
for (img.id in unique(data$name)) { # [1:10]) { # Small batch testing?
  img = subset(data, name == img.id)
  
  # Create BB polygons out of coordinates
  img.bbs = foreach(s.id = img$ymax, .combine = rbind) %do% {
    img.s = subset(img, ymax == s.id)
    img.s.bb = st_polygon(
      x = list(rbind( # Y-coordinates negated for representation of hanging coordinate system
        c(img.s$xmin, img.s$ymin), # Upper left corner
        c(img.s$xmax, img.s$ymin), # Upper right corner
        c(img.s$xmax, img.s$ymax), # Lower right corner
        c(img.s$xmin, img.s$ymax), # Lower left corner
        c(img.s$xmin, img.s$ymin)) # Back to upper right corner, BB complete
      ), dim = "XY")
    list(img.s.bb)
  }
  
  # Wrap in list in case of only one substrate in an image
  if (is.vector(img.bbs)) {
    img.bbs = list(img.bbs)
    #print(paste("Only one wound:", img.id))
  }
  
  data.bbs = rbind(data.bbs, img.bbs)
  rm(img.id, s.id, img.bbs, img.s, img.s.bb)
}

data = cbind(data, data.bbs)
colnames(data)[6] <- "bb"
rownames(data) <- c()
rm(data.bbs)


# FIND DUPLICATES
################################################################################

# Duplicates, identical images on bitlevel
duplicates = read.csv(file = duplicates_file, sep = ",", header = T)

merged_bbs = data.frame()
unmerged_bbs = data.frame() # For comparison
apply(duplicates, 1, function (d) {
  d1_name = d[1]
  d2_name = d[2]
  d1_bbs = subset(data, data$name == d1_name)
  d2_bbs = subset(data, data$name == d2_name)
  
  # Set to name of first instance and append to unmerged BBs, then process
  d2_bbs$name = d1_name
  unmerged_bbs <<- rbind(unmerged_bbs, d1_bbs, d2_bbs)
  
  # Merge intersecting BBs of the same image
  apply(d1_bbs, 1, function (bb1) {
    apply(d2_bbs, 1, function (bb2) {
      sf1 = st_sf(st_sfc(bb1[6]))
      sf2 = st_sf(st_sfc(bb2[6]))
      
      if (length(st_intersects(sf1, sf2)[[1]]) > 0) {
        print("WE GOT AN INTERSECTION HERE")
        print(paste(bb1[1], bb1[2], bb1[3], bb1[4], bb1[5]))
        print(paste(bb2[1], bb2[2], bb2[3], bb2[4], bb2[5]))
        merged_bb = data.frame(
          "name" = bb1[1],
          "xmin" = min(as.numeric(bb1[2]), as.numeric(bb2[2])),
          "ymin" = min(as.numeric(bb1[3]), as.numeric(bb2[3])),
          "xmax" = max(as.numeric(bb1[4]), as.numeric(bb2[4])),
          "ymax" = max(as.numeric(bb1[5]), as.numeric(bb2[5]))
        )
        print(merged_bb)
        merged_bbs <<- rbind(merged_bbs, merged_bb)
        print("---")
      }
    })
  })
})

table(unmerged_bbs$name)
sum(lengths(table(unmerged_bbs$name)))
sum(table(unmerged_bbs$name))

table(merged_bbs$name)
sum(lengths(table(merged_bbs$name)))
sum(table(merged_bbs$name))


# Remove all images in duplicates
data.pre = subset(data.pre, !(name %in% duplicates$name1) & !(name %in% duplicates$name2))

# Restore half of duplicates with merged BBs
data.pre = rbind(data.pre, merged_bbs)
data.pre = data.pre[order(data.pre$name),]
rownames(data.pre) = c()

# Check
sum(lengths(table(data$name)))
length(data$name)
sum(lengths(table(data.pre$name)))
length(data.pre$name)

# Write
data_file.pre = output_file
write.csv(data.pre, file = data_file.pre, quote = F, row.names = F)
