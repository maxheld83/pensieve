# helper to append, and only if class not already in there
# this is to avoid duplicate classes, which is just confusing
classify_clever <- function(x, classname) {
  if (!inherits(x = x, what = classname)) {
    class(x) <- append(classname, class(x))
  }
  return(x)
}

# helper function to deal with validation stuff in class construction ====
assert_class2 <- function(x, validate) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)
  if (validate) {
    assert(x)
  }
}

produce_class_constructor <- function(classname, fun) {
  # input validation
  assert_string(x = classname,
                na.ok = FALSE)
  assert_function(x = fun,
                  args = NULL)

  # all the GENERAL construction magic from above now happens here
  class_constructor_fun <- function(...) {
    args <- as.list(environment())  # fun trick from https://stackoverflow.com/questions/42695298/how-can-i-pass-argument-names-to-a-function-factory
    out_obj <- do.call(what = fun, args = args[names(args) != "validate"])  # this must be excluded for now, because provided fun does not know validate
    out_obj <- classify_clever(x = out_obj, classname = classname)
    assert_class2(x = out_obj, validate = args$validate)
    return(out_obj)
  }
  formals(class_constructor_fun) <- c(formals(fun), validate = TRUE)  # this is necessary to carry over argument names
  return(class_constructor_fun)
}
