# lvsemEnterpriseData

Enterprise and organizational datasets for latent variable SEM examples.

This data package is intended to accompany `lvsem`. It collects public
PLS-SEM-ready or PLS-SEM-related enterprise datasets from Mendeley Data and
Figshare where the data license is compatible with redistribution under
Creative Commons Attribution 4.0.

## Included objects

- `enterprise_metadata`: dataset-level metadata, source URLs, licenses, and scope tags.
- `enterprise_datasets`: a named list. Each element contains metadata, raw file paths,
  and tabular data imported from CSV/XLS/XLSX files where possible.

## Rebuilding the data

Run:

```r
source("data-raw/download_and_prepare.R")
```

The script downloads source files into `inst/extdata/raw/` and writes `.rda`
objects into `data/`.

## License

Package metadata and included converted datasets are distributed under CC BY
4.0, following the licenses of the included source datasets. See `inst/NOTICE`
and `inst/licenses/` for attribution and source links.
