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
# MISSING
## Math II
# MISSING
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
# MISSING
## Osborne
# MISSING
## Vander Griend
# Non-extistent
## Munro/Rust
pdftk tmp.pdf ../../workshops/cell_cycle/CellCycleTutorial.pdf output tmp2.pdf; mv tmp2.pdf tmp.pdf
