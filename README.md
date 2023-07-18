# DC_WAS
Downward Continuation of WAS data to the seafloor
# User Manual: Downward Continuation and redatuming to the seafloor of WAS (OBS) data

This is the first version of the software for the Downward Continuation (DC) of marine OBS field data. The DC is a virtual transformation of the OBS gathers from datum 1 (shots at the sea surface) to datum 2 (shots at seafloor).

As inputs:
- Each OBS gather is required in a file with SU format.
- The bathymetry of the shots in a ascii file.
- Also, a series of parameters have to be provided through an input file which is read in the execution line.
- By default, the p-wave velocity model for the water column is considered constant but also XBT data can be provided to build a specific Vp model.

For more details on the physics implemented in the software, see the reference pre-print [1].

## Installation
Instructions on how to install and use this software are available in the PDF manual located in this directory and named: [UserGuide.pdf](UserGuide.pdf)

## Development
- Development is hosted on GitHub repository:
[github/ejimeneztejero/DC_WAS](https://github.com/ejimeneztejero/DC_WAS).

## Author
- The author is Clara Estela Jiménez Tejero.
- This software was developed at Barcelona Center for Subsurface Imaging, at ICM-CSIC.
