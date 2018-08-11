# helper ====
#' @title
#' Store item content as character strings
#'
#' @description
#' Helper function to append and validate [`psItemContent`][psItemContent] class.
# TODO link to psItems class here, once available
#'
#' @details
#' Store **full items** along with metadata (language, design) to enable deployment and convenience functions, as well as enhance the presentation of results.
#'
#' @param items `[character()]` giving the *participant-facing* **item content**.
#' Can be named to provide short, *researcher-facing* **item handles**.
#' - if `dir_bin` is `NULL` (default), `items` must be text.
#'   Items can be marked up using [Pandoc Markdown](https://rmarkdown.rstudio.com/authoring_pandoc_markdown.html).
#'   An additional subclass `psItemContentText` is prepended and validated.
#' - if `dir_bin` is given, `items` must be file paths, relative from `dir_bin`.
#'   An additional subclass `psItemContentBin`` is prepended and validated.
#'   `lang`, `fontsize_global`, `alignment` and `linestretch` are ignored.
#'
#' @param dir_bin `[character(1)]` giving the root from which `items` can be found, when `items` are paths.
#' Defaults to `NULL`, in which case `items` are expected to be texts.
#' Must be relative path *from the working directory*.
#'
#' @inheritParams declare_pandoc_var
#' @inheritParams wrap_in_latex_env
#'
#' @example tests/testthat/helper_psItemContent.R
#'
#' @family S3 classes from `pensieve`
#'
#' @return
#' `[character()]` with class [`psItemContent`][psItemContent].
#'
#' @export
psItemContent <- function(items,  # for all items
                          dir_bin = NULL,  # only for bin items
                          lang = NULL,  # only for text items
                          fontsize_global = NULL,
                          alignment = "left",
                          linestretch = 2,
                          paperwidth = 8.5,  # for all items
                          paperheight = 5.4,
                          top = 0.5,
                          bottom = 0.5,
                          left = 0.5,
                          right = 0.5,
                          unit = "cm",
                          vcentering = TRUE,
                          hcentering = TRUE) {
  assert_string(x = dir_bin, na.ok = FALSE, null.ok = TRUE)

  # construction
  if (is.null(dir_bin)) {
    items <- new_psItemContentText(
      items = items,
      all_items = items,
      # necessary for finding highest possible fontsize for all items after subsetting
      lang = lang,
      fontsize_global = fontsize_global,
      alignment = alignment,
      linestretch = linestretch,
      paperwidth = paperwidth,
      paperheight = paperheight,
      top = top,
      bottom = bottom,
      left = left,
      right = right,
      unit = unit,
      vcentering = vcentering,
      hcentering = hcentering
    )
  } else {
    items <- new_psItemContentBin(
      items = items,
      dir_bin = dir_bin,
      paperwidth = paperwidth,
      paperheight = paperheight,
      top = top,
      bottom = bottom,
      left = left,
      right = right,
      unit = unit,
      vcentering = vcentering,
      hcentering = hcentering
    )
  }

  assert_S3(x = items)

  return(items)
}

new_psItemContent <- function(items, ..., subclass = NULL) {
  assert_character(
    x = items,
    any.missing = TRUE,
    all.missing = TRUE,
    null.ok = FALSE
  )
  structure(
    .Data = items,
    ...,
    class = c(subclass, "psItemContent", "character")
  )
}

#' @describeIn psItemContent Validation
#' @inheritParams validate_S3
#' @export
validate_S3.psItemContent <- function(x, ps_coll = NULL, ...) {
  assert_character(
    x = x,
    any.missing = TRUE,
    all.missing = TRUE,
    unique = TRUE,
    null.ok = FALSE,
    add = ps_coll,
    .var.name = "items"
  )

  assert_names2(
    x = names(x),
    type = "strict",
    add = ps_coll,
    .var.name = "items"
  )

  NextMethod(ps_coll = ps_coll)
}

# coercion ====
#' @rdname psItemContent
#' @param obj
#' An object which can be coerced to [psItemContent][psItemContent], currently one of:
#' - a (named) character vector.
#' @export
as_psItemContent <- function(obj, ...) {
  UseMethod("as_psItemContent")
}
as_psItemContent.default <- function(obj, ...) {
  stop_coercion(obj = obj, target_class = "psItemContent")
}
as_psItemContent.psItemContent <- function(obj, ...) {
  assert_S3(x = obj)
  obj
}
as_psItemContent.character <- function(obj, ...) {
  psItemContent(items = obj, ...)
}


# subsetting ====
#' @title Subsetting method for psItemContents
#' @description Replacement method for subsetting to retain attributes
#' @inheritParams psItemContent
#' @inheritParams base::Extract
#' @export
#' @noRd
`[.psItemContent` <- function(x, i) {
  invoke(
    .f = new_psItemContent,
    .x = get_attributes_but(x = x, not_attrs = c("class", "names")),
    # class is added again by the constructor
    # names is retained automatically by subsetting
    subclass = attr(x = x, which = "class")[1],
    items = NextMethod(x)
  )
}

#' @title Get all attributes except some
#' @description Helper to get some attributes
#' @param x `[any]` object with attributes
#' @param not_attrs `[character()]` giving names of attributes *to omit*
#' @noRd
get_attributes_but <- function(x, not_attrs) {
  attributes(x)[!names(attributes(x)) %in% not_attrs]
}

# subclass text ====
new_psItemContentText <- function(items, all_items, lang, fontsize_global, alignment, linestretch, paperwidth, paperheight, top, bottom, left, right, unit, vcentering, hcentering) {
  new_psItemContent(
    items = items,
    all_items = all_items,
    lang = lang,
    fontsize_global = fontsize_global,
    alignment = alignment,
    linestretch = linestretch,
    paperwidth = paperwidth,
    paperheight = paperheight,
    top = top,
    bottom = bottom,
    left = left,
    right = right,
    unit = unit,
    vcentering = vcentering,
    hcentering = hcentering,
    subclass = "psItemContentText"
  )
}

#' @describeIn psItemContent Validation
#' @noRd
#' @export
validate_S3.psItemContentText <- function(x, ...) {
  if (test_sysdep(x = "pandoc")) {
    invoke(
      .f = partial(...f = assert_fun_args, x = md2tex_mem, y = "foo"),
      .x = get_attributes_but(x = x, not_attrs = c("class", "all_items", "names"))
      # all other arguments are design args which need testing
    )
  }
  NextMethod(ps_coll = ps_coll)
}

# print ====
#' @describeIn psItemContent Printing to the console
#' @inheritParams base::print
#' @export
print.psItemContent <- function(x, ...) {
  attributes(x)[!names(attributes(x)) %in% c("names")] <- NULL
  NextMethod()
}

# knit_print ====
#' @describeIn psItemContent Printing inside knitr chunks
#' @template knit_print
#' @export
knit_print.psItemContentText <- function(x,
                                         inline = FALSE,
                                         ...) {
  res <- imap_chr(
    .x = x,
    .f = function(wording, handle) {
      if (is.integer(handle)) {
        handle <- NULL  # do not print indeces as title
      }
      if (inline) {
        glue_collapse(
          x = c(
            glue("`{handle}`"),  # this goes to "" if there is name
            glue("^['{wording}']")
          )
        )
      } else {
        glue_collapse(
          x = c(
            glue("`{handle}`", ":    "),  # this goes to "" if there is name
            glue("> {wording}


                 ")  # looks ugly but adds the required newline at the end to get into the next environment
          ),
          sep = "\n \n"
        )
      }
    }
  )
  knitr::asis_output(res)
}


# rendering ====
#' @title Render items to desired format
#' @description
#' Worker function that renders items to desired format according to their design attributes.
#' Thin wrapper around `render_chain()`.
#' @inheritParams psItemContent
#' @inheritParams render_chain
#' @inheritSection render_chain return
#' @noRd
render_items <- function(x, format) {
  assert_S3(x = x)
  assert_choice(x = format, choices = names(render_chain_formats), null.ok = FALSE)

  design_args <- get_attributes_but(x = x, not_attrs = c("class", "all_items", "names"))

  # now we figure out what exactly fontsize local should be
  fontsize_local <- invoke(
    .f = find_fontsize,
    .x = design_args,
    l = as.list(attr(x = x, which = "all_items"))
  )

  invoke(
    .f = render_chain,
    .x = design_args,
    l = as.list(x),
    format = format,
    fontsize_local = fontsize_local
  )
}


# export method ====
#' @describeIn psItemContent Export rendered text items to pdf or svg.
#' @eval document_choice_arg(arg_name = "format", choices = names(render_chain_formats)[-4], before = "giving the output format to render items in.", default = "pdf")
#' @inheritParams export_ps
#' @inheritParams render_chain
#'
#' @section Rendering items:
#' It is often helpful to have a canonical, typeset version of text items, ready for for printing, web publishing or interpretation.
#' Rendered text items should meet several criteria:
#' - They should always fit on the *same* card size, both for practical reasons and to emphasize the equal "significance" of items, even if they are of different length.
#'     Card sizes can be arbitrary, specified via `paperwidth`,`paperheight`.
#' - They should always *look identical*, no matter where a participant or researcher encounters them.
#'     Even slight variations in, for example, line wrapping, might give an item a slightly different emphasis.
#' - Given the central status of text items in the methodology and the package, they should by *typeset professionally*.
#'
#' To meet these criteria, text items are
#' 1. converted to **LaTeX** via [md2tex_mem()], then
#' 2. compiled to **PDF** via [texi2pdf2_mem()], then
#' 3. converted to **SVG** via [pdf2svg_mem()], then
#' 4. imported to **R Graphics** via [svg2grob_mem()].
#'     Items are now fully available to the R Graphics system and can be used wherever [graphics::plot()] (or, to be precise, [grid::grid.draw()]) works.
#'
#' At each step of this necessary, but rather long conversion pipeline more (system) dependencies are required and asserted.
#' Additionally, because some of the intermediary formats cannot be easily or fully converted, downstream outputs may be faulty.
#'
#' Always use the *earliest* possible output from the above conversion pipeline to maximize fidelity to the original PDF.
# TODO mention that user-facing functions such as plot and knit_print automatically do this as far as possible.
# TODO explain how you can write stuff to disc
# TODO explain caching
#'
#' Because items always have to fit on one page, this function errors out when the rendered item would fill more than one page.
export_ps.psItemContentText <- function(x, dir = ".", overwrite = FALSE, format = "pdf") {
  assert_choice(x = format, choices = names(render_chain_formats)[-4], null.ok = FALSE)

  res <- render_items(x = x, format = format)

  imap_chr(
    .x = res,
    .f = function(x, y) {
      out_path <- fs::path(dir, y, ext = format)
      if (!overwrite) {
        assert_no_file(x = out_path)
      }
      if (format == "tex") {
        readr::write_lines(
          x = x,
          path = out_path,
          append = FALSE
        )
      } else {
        readr::write_file(
          x = x,
          path = out_path,
          append = FALSE
        )
      }
      out_path
    }
  )
}


# plot method ====
#' @describeIn psItemContent Plot rendered item. Defaults to first item.
#' @section Plotting items:
#' Plotting items to the R graphics system has some limitations:
#' - You can only plot one item at a time.
#'   The function defaults to the *first* item.
#' - The item is placed in the aspect ratio *given by* `psItemContent()` in the middle of the plotting area.
#'   There may be additional white space around the item.
#'   This is because R graphics must offer arbitrary aspect ratios, but items have a fixed aspect ratio.
#'   For good-looking results, you should set the aspect ratio of the plotting area to *equal* that of the items.
plot.psItemContentText <- function(x) {
  grid::grid.newpage()
  grid::grid.draw(render_items(x = x[1], format = "grob")[[1]])
}


# subclass binary files ====
new_psItemContentBin <- function(items, dir_bin, paperwidth, paperheight, top, bottom, left, right, unit, vcentering, hcentering) {
  new_psItemContent(
    items = items,
    dir_bin = dir_bin,
    paperwidth = paperwidth,
    paperheight = paperheight,
    top = top,
    bottom = bottom,
    left = left,
    right = right,
    unit = unit,
    vcentering = vcentering,
    hcentering = hcentering,
    subclass = "psItemContentBin"
  )
}


#' @describeIn psItemContent Validation
#' @noRd
#' @export
validate_S3.psItemContentBin <- function(x, ...) {
  dir_bin <- attr(x = x, which = "dir_bin")

  assert_directory_exists(x = dir_bin, access = "r", add = ps_coll)
  files <- file.path(dir_bin, as.vector(x))
  assert_file_exists(
    x = files,
    access = "r",
    .var.name = "file names",
    add = ps_coll)

  NextMethod(ps_coll = ps_coll)
}
