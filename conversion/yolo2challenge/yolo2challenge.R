# Libs
###################################################################################################

library(jpeg)


# Settings
###################################################################################################

wd = "SET WORKING DIRECTORY HERE"
setwd(wd)

# Set directory containing YOLO annotations here
input_yolo_dir = "SET INPUT YOLO ANNOTATIONS DIRECTORY HERE"

# Set a list file of YOLO annotation files here to be converted to the challenge format
# No header information must be set, just a one column list of YOLO annotation file names:
# <first>.txt
# <second>.txt
# ...
input_yolo_files_list = "SET INPUT YOLO FILES LIST HERE"

# Set directory containing images here, most propably the same as the annotation directory
input_image_dir = input_yolo_dir

# Set the desired output file here to write annotations in the challenge data format
output_file = "SET CHALLENGE ANNOTATION OUTPUT FILE HERE"

# Set whether the YOLO annotation files contain the prediction confidence as sixth column or not
# (should always be true as only the final predictions after self-training should be converted back
# to the challenge annotation format)
has_confidence = T


# Functions
###################################################################################################

f.calc_bbs <- function (name) {
  require (jpeg)
  
  img.name = paste0(substr(name, 1, nchar(name) - 4), ".jpg")
  img = paste(input_image_dir, img.name, sep = "/")
  img.dim = dim(readJPEG(img))
  img.w = img.dim[2] # Width
  img.h = img.dim[1] # Height
  
  yolo = paste(input_yolo_dir, name, sep = "/")
  yolo.bbs = read.csv(yolo, sep = " ", header = F)
  if (has_confidence) {
    names(yolo.bbs) = c("class", "x.center.rel", "y.center.rel", "x.size.rel", "y.size.rel", "score")
  } else {
    names(yolo.bbs) = c("class", "x.center.rel", "y.center.rel", "x.size.rel", "y.size.rel")
  }
  
  # Calculate absolute BBs from relative BBs
  x.center.abs = yolo.bbs$x.center.rel * img.w
  y.center.abs = yolo.bbs$y.center.rel * img.h
  bb.w = yolo.bbs$x.size.rel * img.w
  bb.h = yolo.bbs$y.size.rel * img.h
  bb.x.min = ceiling(x.center.abs - (bb.w / 2))
  bb.y.min = ceiling(y.center.abs - (bb.h / 2))
  bb.x.max = floor(bb.x.min + bb.w)
  bb.y.max = floor(bb.y.min + bb.h)
  
  if (has_confidence) {
    bbs = data.frame(
      name = img.name,
      img.w = img.w,
      img.h = img.h,
      class = yolo.bbs$class,
      bb.x.min = bb.x.min + 1,
      bb.y.min = bb.y.min + 1,
      bb.x.max = bb.x.max - 1,
      bb.y.max = bb.y.max - 1,
      score = yolo.bbs$score
    )
  } else {
    bbs = data.frame(
      name = img.name,
      img.w = img.w,
      img.h = img.h,
      class = yolo.bbs$class,
      bb.x.min = bb.x.min + 1,
      bb.y.min = bb.y.min + 1,
      bb.x.max = bb.x.max - 1,
      bb.y.max = bb.y.max - 1
    )
  }
  
  # Order by class
  bbs = bbs[order(bbs$class),]
  rownames(bbs) = c()
  
  return (bbs)
}


# Processing
###################################################################################################

data = read.csv(file = input_yolo_files_list, header = F)
colnames(data) = c("name")

# Calculate for each file in list and append
bbs = data.frame()
for (name in data$name) {
  bbs = rbind(bbs, f.calc_bbs(name))
  print(name)
}

# Check boundaries
min(bbs$img.w - bbs$bb.x.max)
min(bbs$img.h - bbs$bb.y.max)
min(bbs$bb.x.min)
min(bbs$bb.y.min)
summary(bbs$img.w)
summary(bbs$img.h)

table(bbs$name)

if (has_confidence) {
  bbs = subset(bbs, select = c("name", "bb.x.min", "bb.y.min", "bb.x.max", "bb.y.max", "score"))
  colnames(bbs) = c("filename", "xmin", "ymin", "xmax", "ymax", "score")
} else {
  bbs = subset(bbs, select = c("name", "bb.x.min", "bb.y.min", "bb.x.max", "bb.y.max"))
  colnames(bbs) = c("filename", "xmin", "ymin", "xmax", "ymax")
}

# Write output file
write.table(bbs, output_file, row.names = F, quote = F, sep = ",")

