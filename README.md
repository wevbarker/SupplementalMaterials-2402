# Supplemental Materials to arXiv:2402:#####

W. Barker and C. Marzo

## About

These materials are designed to supplement our paper. They provide complete spectral analyses using the PSALTer package, which supplement the results presented in the appendices of arXiv:2402:#####.

## Installation

#### Requirements 
These supplemental materials have been tested in the following environment(s):
- Linux x86 (64-bit), specifically Manjaro, Arch, CentOS, Scientific Linux and Ubuntu
- Mathematica v 14.0.0.0
- xAct v 1.2.0
#### Install 
1. Make sure you have [installed xAct](http://www.xact.es/download.html).
2. Download this repository:
	```bash, git
	git clone https://github.com/wevbarker/SupplementalMaterials-2402
	cd SupplementalMaterials-2402
	```
3. Place the contents of the `SupplementalMaterials-2402/PackagesForMathematica_v14.0.0.0/xAct` directory (i.e. the one new package PSALTer) relative to your xAct install. A global install might have ended up at: 
	```bash
	/usr/share/Mathematica/Applications/xAct
	```
## How to use 

- You may decide to interact with the supplement at various levels. 
- You can read the `SupplementalMaterials.pdf` file. 
- If you wish to reproduce the calculations, or if you find some problem with the formatting of the pdf (the Mathematica pdf export is not flawless), you can explore the `SupplementalMaterials.nb` notebook file. 
- If you wish to re-run the notebook, then you can execute the single executable cell at the top, which reads:
```wolfram
Get@FileNameJoin@{NotebookDirectory[],"SupplementalMaterials.m"};
```
- If there is any ambiguity over the calculation, you can always read the `SupplementalMaterials.m` Wolfram Language file, and any files to which it refers, and by construction these will tell you **exactly** what is being done at each step.
