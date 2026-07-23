# biocPropagationPipeline

A Bioconductor propagation pipeline that monitors R-universe for newly built
packages, identifies packages meeting propagation criteria, updates package view
and branch VIEWS files accordingly, and generates a report of artifacts to
update the repository.

## Features

- **Monitors** R-universe for newly built packages
- **Identifies** packages meeting propagation criteria
- **Updates** individual `{branch}/{package}.json` files with new package metadata
- **Rebuilds** `{branch}/VIEWS.json` by aggregating all package JSONs with
  additional metadata
- **Automated** via GitHub Actions

## Repository Structure

```
biocPropagationPipeline/
├── R/                          # Pipeline code
├── devel/                      # Devel package.view files + VIEWS.json
├── release/                    # Release package.view filess + VIEWS.json
├── .github/workflows/          # GitHub Actions workflows
├── tests/                      # Test scripts
└── README.md
```

## Bioconductor Release

TODO
