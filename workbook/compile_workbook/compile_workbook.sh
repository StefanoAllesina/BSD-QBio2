## Cover sheet
pdftk ../other_docs/cover.pdf ../other_docs/blank.pdf cat output tmp.pdf
## Map
pdftk tmp.pdf ../other_docs/map.pdf cat output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Info sheet
pdftk tmp.pdf ../other_docs/info_sheet.pdf cat output tmp2.pdf; mv tmp2.pdf tmp.pdf
## General schedule
pdftk tmp.pdf ../../schedule/GeneralSchedule.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Tutorials cover
pdftk tmp.pdf ../other_docs/cover_tutorials.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Imaging
pdftk tmp.pdf ../../tutorials/imaging/data/ImageProcessingExercises.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Math I
pdftk tmp.pdf ../../tutorials/foundations_1/foundations_1_prob_and_inference.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
# Math II
pdftk tmp.pdf ../../tutorials/foundations_2/foundations_2_dynamical_systems.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Basic I
pdftk tmp.pdf ../../tutorials/basic_computing_1/code/basic_computing_1.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Basic II
pdftk tmp.pdf ../../tutorials/basic_computing_2/code/basic_computing_2.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Advanced I
pdftk tmp.pdf ../../tutorials/advanced_computing_1/code/advanced_computing_1.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Advanced II
pdftk tmp.pdf ../../tutorials/advanced_computing_2/code/advanced_computing_2.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Workshops cover
pdftk tmp.pdf ../other_docs/cover_workshops.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Cobey
pdftk tmp.pdf ../../workshops/cobey/README.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Novembre
pdftk tmp.pdf ../../workshops/novembre/Handouts/MBL_WorkshopJN.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Osborne
pdftk tmp.pdf ../../workshops/osborne/MBL_course2016_infotheory.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Vander Griend
pdftk tmp.pdf ../../workshops/vander_griend/README.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
## Munro/Rust
pdftk tmp.pdf ../../workshops/cell_cycle/CellCycleTutorial.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf

## Redistill to reduce size
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -sOutputFile=../workbook.pdf tmp.pdf
## remove tmp
rm tmp.pdf
