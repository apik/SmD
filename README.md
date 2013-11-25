SmD
===

SARAH meets DIANA


Project aimed to develop conversion script from 
SARAH - tool for Feynman rules building from lagrangian to DIANA - 
efficient program for multi-loop diagram generation.

This connection can expand set of models available in multi-loop calculations.

Needed tools are available for download:

SARAH: http://sarah.hepforge.org

DIANA: https://github.com/apik/diana


Installation
----------

Place it somewhere to be loadable from Mathematica,
Example file is included in distribution.

Usage
----------
 ```
 <<SmD`
 MakeDIANA["output_file_name",Options]
 ```
Options are:

`Model->"SM"` Model name specification

`State->GaugeES` choose SARAH model eigenstate
