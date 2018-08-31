if (Sys.getenv()["LOGNAME"] %in% c("max")) {
  message("I'm running some preparatory steps because this is a dev machine.")
  imports <- devtools::parse_ns_file()$imports  # capture all imports from namespace file
  imports <- purrr::discard(.x = imports, .p = is.list)  # only take the full imports
  suppressMessages(
    purrr::walk(.x = c(imports, "devtools", "testthat"), .f = library, character.only = TRUE)
  )

  # below load_all causes a vexing problem:
  # htmlwidget::createWidget() will no longer find dependencies because of the altered search path
  # see https://community.rstudio.com/t/testbed-for-developing-htmlwidgets-how-to-iterate-quickly/10297
  # that's why we need below hack from winston chang from https://gist.github.com/wch/c942335660dc6c96322f

  library(inline)

  inc <- '
  /* This is taken from envir.c in the R 2.15.1 source
  https://github.com/SurajGupta/r-source/blob/master/src/main/envir.c
  */
  #define FRAME_LOCK_MASK (1<<14)
  #define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
  #define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))
  '

  src <- '
  if (TYPEOF(env) == NILSXP)
  error("use of NULL environment is defunct");
  if (TYPEOF(env) != ENVSXP)
  error("not an environment");
  UNLOCK_FRAME(env);
  // Return TRUE if unlocked; FALSE otherwise
  SEXP result = PROTECT( Rf_allocVector(LGLSXP, 1) );
  LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0;
  UNPROTECT(1);
  return result;
  '

  unlockEnvironment <- cfunction(signature(env = "environment"),
                                 includes = inc,
                                 body = src)

  imports <- parent.env(asNamespace("htmlwidgets"))
  unlockEnvironment(imports)
  imports$system.file <- pkgload:::shim_system.file


  # After the code above has been run, you can load an in-development package
  # that uses htmlwidgets (like dygraphs or leaflet). After being loaded this way,
  # When the JS or CSS resources of the package are edited, they'll immediately
  # be available, without having to build and install the package.
  devtools::load_all()
}


