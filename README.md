
# Awesome CV in R

This CV was made using the
[`vitae`](https://pkg.mitchelloharawild.com/vitae/) package in R, and
using the template from [Dominique
Makowski](https://github.com/DominiqueMakowski/CV) (which includes the
code for the Google Scholar figure and table, and a lot more). Please
give them due credit if using this template. I also brought some minor
optimization and documentation to the current template.

## Instructions

1.  First, you will need to clone/download this entire repository and
    all it’s files to your computer so you can run the code in R.
2.  The main file to edit is `cv.Rmd`. Use it to add your name, contact
    information, and description. Change the template colour using the
    `headcolor` parameter (in the YAML header). Also replace the photo
    in the `img` folder.
3.  Make sure to install all required packages (LaTeX is required as
    well).
4.  Specify your Google Scholar profile number and name if using this
    feature.
5.  Change the order of sections by changing the order of code chunks.
6.  Edit the individual `.Rmd` files in the `sections` subfolder to edit
    your sections’ content as desired (e.g., `publications.Rmd`).
7.  To output the CV to PDF, simply knit `cv.Rmd` (shortcut is
    `Ctrl+Shift+K`)

## Tips

-   Request a short version by setting `short` to `TRUE` in `cv.Rmd`.
-   Add `eval = !short` to code chunk options to make them optional for
    the short version.
-   If you are using a word processor as well as RStudio, one tip to
    save time is to change the relevant content section (e.g.,
    `publications.Rmd`) View from `Source` to `Visual` (top left in
    RStudio) before copy-pasting your publications and other formatted
    content. This way, all the existing formatting (bold, italic, etc.)
    will be kept and you won’t have to manually recreate all the
    formatting.
-   It is also possible to import data from other sources automatically
    (ORCID, Google Scholar, etc.), but I have not personally
    experimented with this yet. More info can be found
    [here](https://pkg.mitchelloharawild.com/vitae/articles/data.html).

## Notes

-   This template uses the North-American `8.5in x 11in` (letter) size
    instead of the original European `8.27 x 11.69` (A4) size.
-   Special characters (e.g., `$`, `&`, `%`) need to be escaped with a
    backslash or a double backslash.
-   The template uses some LaTeX language at times for specific
    customization (commands starting with backslashes `\`). Some useful
    commands include:
    -   `\pagebreak` to create a page break between sections
    -   `\\textit{yourtext}` to italicize `yourtext` (within dataframes,
        outside of regular rmarkdown)
    -   `\\hspace{0.5cm}` to create some left space (e.g., between
        columns in tables)
    -   `\\dotfill` to create dotted lines (e.g., for the award section)
    -   `\setlength{\parindent}{-0.2in}` and
        `\setlength{\leftskip}{0.2in}` to indent publications, “hanging”
        style.
    -   `\scriptsize` to make text smaller (typically for publications
        sections)
    -   `\normalsize` to make text normal (for regular sections)
    -   For specific font sizes, consider the following reference: 

<img width="40%" height="531" alt="image" src="https://github.com/user-attachments/assets/6561307a-e7eb-432f-8bb3-8156987f1c31" />

-   For questions or difficulties, feel free to open a GitHub issue
    here.

## Demos

My long CV is available for demonstration here:
<https://remi-theriault.com/cv>

My short CV is available for demonstration here:
<https://remi-theriault.com/cv_short.pdf>
