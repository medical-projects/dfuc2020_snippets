import sys
import numpy as np
import cv2 as cv
from matplotlib import pyplot as plt
from glob import glob

# First CLI argument is the source directory
# Second CLI argument is the destination directory
for fn in glob(f"{sys.argv[1]}/*.jpg"):
	print("processing ", fn)
	img = cv.imread(fn)
	
	#void cv::fastNlMeansDenoisingColored 	( 	InputArray  	src,
	#		OutputArray  	dst,
	#		float  	h = 3,
	#		float  	hColor = 3,
	#		int  	templateWindowSize = 7,
	#		int  	searchWindowSize = 21 
	#	)
	dst = cv.fastNlMeansDenoisingColored(img, None, 1, 3, 7, 21)	
	out_path = f"{str(fn).replace(sys.argv[1], sys.argv[2])}"
	print("writing ", f"{str(fn).replace(sys.argv[1], sys.argv[2])}")
	cv.imwrite(out_path, dst)
