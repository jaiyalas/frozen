# CSVSquash 

The CSV Squash.

## AUTHOR:
* jaiyalas (Yun-Yan Chi)

## LINK:
* [github](https://github.com/jaiyalas/csvc)

## DESCRIPTION:
A program that squashes csv files in a row.

### NOTICE:
All input csv files MUST have sequential name.
And, all files must have same size and structure.
In other words, all files MUST have same number of lines that will be ignored,
and, all files also have same number of  lines of data.

#### USAGE EXAMPLE:
For Example: 84.csv, 85.csv, ... 89.csv. Each file has 3 redundant lines in the top.
    csvs 3 84 89

#### FORMAT EXAMPLE:

Support there are two files:
file1 is:

    1, a
    2, b
    3, c

and, file2 is:

    4, d
    5, e
    6, f

after executing csvs, the result file will be:

    1, a, 4, d
    2, b, 5, e
    3, c, 6, f

## REQUIREMENTS:
* Tested with GHC v7.0.3, cabal-install v0.10.2 and cabal v1.10.1.0.

* (must) ghc-7.0 and cabal-1.2
  - [Haskell Platform](http://hackage.haskell.org/platform/)

* (optional) cmdargs: a haskell package.
  - cabal install cmdargs
  - [cmdargs](http://hackage.haskell.org/package/cmdargs)
  - this package will be installed automatically during the build-stage of scvs.

## INSTALLATION:
configure csvs

    cabal configure

build csvs

    cabal build

executable file will be in ./dist/build/csvs/ .

(optional)

    cabal install

## LICENSE:

  Apache License 2.0

  Copyright (c) 2010, Cardinal Blue

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     <http://www.apache.org/licenses/LICENSE-2.0>

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
