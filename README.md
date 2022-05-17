
# Awesome CV in R

This CV was made using the
[`vitae`](https://pkg.mitchelloharawild.com/vitae/) package in R, and
using the template from [Dominique
Makowski](https://github.com/DominiqueMakowski/CV) (which includes the
code for the Google Scholar figure and table, and a lot more). Please
give them due credit if using this template. I also brought some
optimization and documentation to the current template, so acknowledging
me would be appreciated, though totally optional.

## Instructions

-   The main file to edit is `cv.Rmd`. Use it to add your name, contact
    information, and description. Change the template colour using the
    `headcolor` parameter.
-   Request a short version by setting `short` to `TRUE`.
-   Make sure to install all required packages.
-   Specify your Google Scholar profile number and name if using this
    feature.
-   Change the order of sections by changing the order of code chunks.
-   Edit the individual files (“childs”) to edit your sections’ content
    as desired (e.g., `publications.Rmd`).
-   Add `eval = !short` to code chunk options to make them optional for
    the short version.
-   To output the CV to PDF, simply Knit it (through RStudio’s button or
    the Ctr+Shift+K shortcut)

## Notes

-   This template uses the North-American `8.5in x 11in` (letter) size
    instead of the original European `8.27 x 11.69` (A4) size.
-   Special characters (e.g., `$`, `&`, `%`, need to be escaped with a
    backslash or a double backslash)
-   The template uses some LaTeX language at times for specific
    customization (commands starting with backslashes `\`). Some useful
    commands include:
    -   `\pagebreak` to create a page break between sections
    -   `\scriptsize` to make text smaller (typically for publications
        sections)
    -   `\normalsize` to make text normal (for regular sections)
    -   `\\textit{yourtext}` to italicize `yourtext` (within dataframes,
        outside of regular rmarkdown)
    -   `\\hspace{0.5cm}` to create some left space (e.g., between
        columns in tables)
    -   `\\dotfill` to create dotted lines (e.g., for the award section)
    -   `\setlength{\parindent}{-0.2in}` and
        `\setlength{\leftskip}{0.2in}` to indent publications, “hanging”
        style.
-   For questions or difficulties, feel free to open a GitHub issue
    here.

My long CV is available for demonstration here:
<https://remi-theriault.com/cv>

My short CV is available for demonstration here:
<https://remi-theriault.com/cv_short.pdf>
