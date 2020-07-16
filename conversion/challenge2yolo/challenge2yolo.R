################################################################################
# CONFIG
################################################################################

# Set working directory
wd = "STATE YOUR WORKING DIRECTORY HERE"
setwd(wd)

# State training ground truth annotation file here as input
input_file = "STATE YOUR CHALLENGE GROUND TRUTH FILE HERE"

# State output directory for converted annotation data to per-image YOLO data
output_dir = "STATE THE OUTPUT DIRECTORY FOR YOLO ANNOTATIONS HERE"

# The training ground truth just has landscape images
img_w = 640
img_h = 480


################################################################################
# READING AND PRE-PROCESSING
################################################################################

# Read all files: ground truth and predictions
data_file = input_file
data = read.csv(file = data_file, sep = ",", header = T)


################################################################################
# CONVERSION
################################################################################

dw = 1 / img_w
dh = 1 / img_h

data.yolo = data.frame()
for (n in unique(data$name)) {
  rows = subset(data, name == n)
  
  rows.yolo = data.frame()
  apply(rows, 1, function (row) {
    xmin = as.numeric(row[2])
    ymin = as.numeric(row[3])
    xmax = as.numeric(row[4])
    ymax = as.numeric(row[5])
    
    xc = ((xmin + xmax) / 2) * dw
    yc = ((ymin + ymax) / 2) * dh
    xw = (xmax - xmin) * dw
    yh = (ymax - ymin) * dh
    
    # Class is always 0 as we just got the "wound" class
    rows.yolo <<- rbind(rows.yolo, data.frame(0, xc, yc, xw, yh))
  })
  
  # Write YOLO annotation file for currently processed image
  file_name = strsplit(rows[1,]$name, split = "[.]")[[1]][1]
  file_path = paste(wd, output_dir, paste0(file_name, ".txt"), sep = "/")
  write.table(rows.yolo, file = file_path, quote = F, sep = " ", row.names = F, col.names = F)
}
