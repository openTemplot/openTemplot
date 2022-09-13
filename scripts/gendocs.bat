# This script will generate documentation under Windows
# For Linux use "scripts/gendocs" 

# RUN THIS FROM THE PROJECT ROOT DIRECTORY

pasdoc -E docs/autodocs --css OT-doc.css  *.pas  model/*.pas
