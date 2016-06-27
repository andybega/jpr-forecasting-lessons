**Replication materials for:<br/> 
[title](link)**
***

For questions contact the corresponding author [Michael Ward](mailto:michael.d.ward@duke.edu) or [Andreas Beger](adbeger@gmail.com).

**Citation:**

Beger, Andreas and Michael D. Ward, 2014, "title," journal.

```bibtex
@article{beger2014ensemble,
  title={Ensemble Forecasting of Irregular Leadership Changes},
  author={Beger, Andreas, Dorff, Cassy L., Ward, Michael D.},
  journal={Research \& Politics},
  year={2014},
  volume={1},
  issue={3},
  pages={1-7}
}    
```

Getting the code and data
-----

The easiest way to get the replication code is to [download a zip](https://github.com/andybega/rap-ensemble-forecasting/archive/master.zip). Alternatively, you can clone the repository through the Github GUI client ([OS X](https://mac.github.com/), [Windows](https://windows.github.com/)).

The data, including several intermediate results, are available on dataverse: [http://dx.doi.org/10.7910/DVN/27482](http://dx.doi.org/10.7910/DVN/27482).


Running the replication
-----

1. [Download](https://github.com/andybega/rap-ensemble-forecasting/archive/master.zip) or [clone](github-mac://openRepo/https://github.com/andybega/rap-ensemble-forecasting) this repository. 

2. Download the 3 data sets on [Dataverse](http://dx.doi.org/10.7910/DVN/27482) and place them in `replication/data`.

3. In `runme.R`, change the working directory path on line 33.

4. Source or run the code in `runme.R`. We recommend running through the code block by block rather than sourcing. The original analysis was run on OS X using R 3.0.2 and 3.1.1.

The script relies on two packages, `EBMAforecastbeta` and `spduration` that are not available on CRAN. They are included in `replication/R/packages` with both OS X and Windows versions. The replication script will attempt to install them if they are not already present, but you may have to do so manually if this fails.

See `replication.pdf` for a list of included files and scripts.