{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipTransformations";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Contains functions handling transformations between different
    types/classes of data, and other types of general
    transformations (e.g., stacking by weight).'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    data_table
    flipU
    plyr
    stringr
    Hmisc
    flipFormat
    stringi
    flipTime
  ];
}
