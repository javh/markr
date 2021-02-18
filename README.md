# Markr

This is a fork of [staticdocs](https://github.com/hadley/staticdocs) with some bits from [readthedocs](https://github.com/sahilseth/readthedocs) to render output suitable for 
input to [MkDocs](http://www.mkdocs.org) or [Sphinx](https://www.sphinx-doc.org).

## Installation

```
devtools::install_github("javh/markr")
```

## What it does

* Parses the README.md, DESCRIPTION, and inst/CITATION files in your package to generate
  the **Introduction** page (index.md).
* Knits R Markdown (Rmd) files from the package into markdown files, places them in
  `docs/vignettes`, and assigns them to the **Vignettes** section of the contents.
* Converts Rd (man page) files of the package into markdown files, runs examples,  
  places the output into `docs/topics`, and assigns them to the **Help Topics** 
  section of the contents.
* Assigns the package  help file (the man page named after your package) to
  the **Package Overview** page.
* Generates a `mkdocs.yml` config file.

## How to use it

Run this from your package source directory, where `package` is the name of your package:

```
library(markr)
library(package)
build_mkdocs(yaml=TRUE)
```

This will generate a folder called `docs` in the package root and a default `mkdocs.yml` file.
Note that the default for `build_mkdocs()` is `yaml=FALSE`, so that you (hopefully) do not 
accidentally overwrite your mkdocs config. The first time you run `build_mkdocs()` you should 
use the argument `yaml=TRUE` to populate the `mkdocs.yml` with the defaults. Then, edit the
`mkdocs.yml` config to taste according to the [MkDocs](http://www.mkdocs.org) documentation.

You can alter the input and output directories like so:

```
build_mkdocs(pkg="package/source/directory", doc_path="doc/output/directory")
```

If you put the `mkdocs.yml` and `docs` directory in the root of your repo, then 
[Read the Docs](http://readthedocs.org) will build your docs automatically, assuming you 
select "MkDocs" as the documentation type within settings

## Examples

These [docs](http://alakazam.readthedocs.org)  
Are built from this [repo](https://bitbucket.org/kleinstein/alakazam)  
Whose docs are built from [these commands](https://bitbucket.org/kleinstein/alakazam/src/master/docs/build.R)  
Supplemented with one manually created [file](https://bitbucket.org/kleinstein/alakazam/src/master/docs/install.md)

## Limitations

This is still a work in progress. At this point, everything *should* render correctly, 
with a few exceptions:

* `devtools::load_all()` and `knitr::knit()` don't seem to be friends. For now, you
  *must* `library()` your package before building the docs so the examples work.
* Does not currently support the `\code{\link{}}` syntax.  Just use `\link{}` instead. 
  Note, this also means roxygen `@family` tags won't render correctly, nor will linking to
  an external package in roxygen via `\link[package]{topic}`.
* Does not currently support demos.
* MathJax is currently disabled, as it requires the 
  [python markdown math extension](https://github.com/mitya57/python-markdown-math).
