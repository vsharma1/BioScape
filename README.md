BioScape
========
{Note: Implementation of BioScape is based on Stochastic Pi Machine (SPiM). SPiM is developed by Microsoft Research and more information about SPiM can be found at http://research.microsoft.com/en-us/projects/spim/}

Steps Involved to Execute BioScape
1. BioScape implementation is both compatible with Linux and MacOS X. Use Terminal to run BioScape on your machine.
2. Install Objective Caml, Python and VPython.
3. Go to the source repository shared on GitHub
4. Type the following instructions on the terminal
ocamlbuild main.byte && ocamlrun ./_build/main.byte test/spaces/test_declarespaces001.spi
python visualization/graphics.py test/spaces/test_declarespaces001.spi_pos.csv
5. The output of the BioScape generates 2 csv les: *.spi.csv and *.spi pos.csv.
6. *.spi.csv plots populations over time.
7. *.spi pos.csv produces 3D-rendered videos.

