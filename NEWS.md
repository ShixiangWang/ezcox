# ezcox 0.8.1

* Tried fixing CRAN checking issue.

# ezcox 0.8.0

* Added more options in `show_forest()` for better visualization.

# ezcox 0.7.0

* Added `ezgrp()` for group cox analysis.
* Added headings control.

# ezcox 0.6.0

* Fixed bug about showing model with unvalid R variable names.

# ezcox 0.5.0

* Supported other parameters in `coxph`.


# ezcox 0.4.0

* Add function `show_forest`, which is a wrapper of
function `ezcox`, `get_models` and `show_models`.

# ezcox 0.3.1

* Fix the bug that a `NA` is added to model names when no control

# ezcox 0.3.0

* Remove `parallel` option in `ezcox()`
* Add `ezcox_parallel()` for better parallel computation (#10)

# ezcox 0.2.5

* Add 'auto' option for filtering

# ezcox 0.2.4

* Fix bug: cannot get model result from parallel model
* Add `clean_model_dir()`

# ezcox 0.2.3

* Add `show_models()`

# ezcox 0.2.2

* Add `filter_ezcox()` and `get_models()`

# ezcox 0.2.1

* Add `verbose` option. This may speed up parallel computation.

# ezcox 0.2.0

* Parallel computation is implemented in `parallel` option by **furrr** package.

# ezcox 0.1.0

* The first experimental version submitted to CRAN.
* Added a `NEWS.md` file to track changes to the package.
