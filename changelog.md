## 2018-12-06

**Version 0.1.2**

- Column labels now show at the top of the plate.
- Now have access to stats::med_smooth arguments to control iterations
  when using two-way median polish and b_score functions.
- All plots now have additional arguments to change the `size` and `shape` of
  the wells using the same arguments as `geom_point`.
- b-score functions now have `normalise` argument to ficide residuals by the
  plate median-absolute-deviation (MAD) as per the original paper.
- Deprecation warning as `each` argument changed to `scale_each`.


## 2018-06-25

**Version 0.1.1**

- Fix failures caused by new ggplot2 version

## 2018-03-03

**Version 0.0.2 to 0.1.0**

- Add set_block function (contributed by @charles-plessy)
- Fix deprecatation warning for `panel.margin`


## 2016-10-05

**Version 0.0.1 to 0.0.2**

- Fix bug in `R/missing_wells.R`
- Fix CRAN NOTE for unused Imports
    - Remove MASS, lazyeval, raster from Imports
    - Move viridis to Suggests
