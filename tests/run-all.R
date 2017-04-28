library(testthat)

# disable custom templates until relevant test section
Sys.setenv("PT_ROOT_TEMPLATE_DIR"="NULL")
on.exit(Sys.unsetenv("PT_ROOT_TEMPLATE_DIR"))

test_check("ProjectTemplate")
