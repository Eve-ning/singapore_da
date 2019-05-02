# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from PIL import Image
import os

def stitch(path_dir:str):
    image_names = filter(lambda x: x != 'stitched.jpg', os.listdir(path_dir))
    image_paths = [path_dir + x for x in image_names]
    images = [Image.open(x) for x in image_paths]
    widths, heights = zip(*(i.size for i in images))
    
    width = max(widths)
    height = sum(heights)
    
    image_out = Image.new(mode = 'RGB',
                          size = (width, height))
    
    y_offset = 0
    for i in images:
        image_out.paste(i, (0, y_offset))
        y_offset += i.size[1]
        
    image_out.save(path_dir + 'stitched.jpg')
    
stitch("../img/frags/intake_rate/")