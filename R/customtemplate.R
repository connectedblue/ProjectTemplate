# This file has the main infrastructure for the custom template subsystem.  There 
# are no functions exported to users from here - instead the user facing functions make
# calls here

# Custom templates can be configured for each different installation of ProjectTemplate
# There can be multiple templates, and one of them is a default.  They are invoked by
#     create.project("project-name", "template-name")
# or
#     create.project("project-name") to invoke the default template
#
# First of all, the standard ProjectTemplate structure is created.  Then the structure
# defined for each template name is copied into the standard structure, overwriting 
# anything already there, and adding any new files.
#
# The location of the templates is a single location on the local file system or github.
# This is called the root template location.  Each sub directory under the root 
# location is the template-name used in the call to create.project().
#
# A user function configure.templates() provides a management interface to manage the
# installed templates on a system.
#
# Template designers can simply create directory structures for any functionality they 
# want to, e.g. knitr templates, shinyapps, corporate analysis standards etc.
# There is an advanced template design feature, whereby the template is described in
# a template definition file.  This allows for re-use of functionalility between templates
# (e.g. a lib function that you'd like to make available in many templates).  It also
# allows more fine grained control than just plain copy-and-overwrite.  For example, it
# can be specifed for a .gitignore segment on a template to be appended to an existing
# .gitignore in the target project template directory.
#
# Template definitions are stored in a DCF format file, and loaded into a dataframe for 
# processing.  There can be multiple DCF records defined in a single file.  Each record
# can the following fields (depending on template_type):
#
#
#      template_type:           type of template definition:
#                                       root - template root file for a site installation
#                                       project - templates for create.project()
#
#      content_location:        where template content can be found:
#                                       local:/path/to/content/dir.or.file
#                                       github:username/repo@branch:path/to/content.or.file
#
#
#      merge:                   determines how file content is handled when it clashes with
#                               files of the same name in the target project:
#                                       overwrite/append/duplicate 
#
#      template_name:           Name of template that the content relates to
#
#      target_dir:              directory relative to project where content is placed 
#
#       default:                whether this record is the default template
#


# Root template definition file uses this format with some restrictions:  
#  if the field no_templates_defined: is present, the template definition is not defined
#       merge:     not used
# target_dir:      not used
#
# The file is stored in the ProjectTemplate package under the directory:
#           inst/defaults/customtemplates 
# and is named RootConfig.dcf.  A backup is kept in the etc folder of the R installation.
# This is used to restore configuration if a new version of ProjectTemplate has been 
# installed and the RootConfig.dcf got written over.
#
# General template definition files reside directly under the template subdirectory
# with the name template-definition.dcf.  If this file exists, it is used to build the
# template structure.  If not, any files or folders are copied directly (with over-write)
# to the target project directory.  

#
# Custom template functions start here .....
#

# First, Some short cut definitions to aid readability

# Where is the root template location defined - put in the R/etc folder to preserve config
# between installs of ProjectTemplate
.root.template.dir <- file.path(R.home(), "etc")
.root.template.file <- file.path(.root.template.dir, "ProjectTemplateRootConfig.dcf")

# Types of template configuration allowed
.available.template.types <- c("root", "project")
.template.field.names <- c("template_type", "content_location", "template_name")
.root.template.field.names <- c("target_dir", "default")
.project.template.field.names <- c("merge", "target_dir")
.template.merge.types <- c("overwrite", "append", "duplicate")
.no.templates <- "no_templates_defined"


# Helper function to remove the first item in a list
.remove.first <- function (x) rev(head(rev(x), -1))


# take a template definition dataframe, parse the content_location field into
# the valid components and return the definition frame with the new columns
.parse.content.location <- function(definition) {
        
        # Parse the content_location field to extract the embedded information
        content_location <- strsplit(definition$content_location, ":")
        
        location_type <- sapply(content_location, function (x) x[1])
        file_location <- sapply(content_location, function (x) x[3])
        github_repo <- sapply(content_location, function (x) x[2])
        
        # Validate the location_type
        valid_locations <- c("local", "github")        
        invalid_types <- setdiff(location_type, valid_locations)
        if(length(invalid_types)>0) {
                stop(paste0("Invalid location types: ", invalid_types))
        }
        definition <- cbind(definition, data.frame(location_type=location_type,
                                                   file_location=file_location,
                                                   github_repo=github_repo) )
        definition
}

# enforce some rules about all template types:
#       check field names are correct
#       template_type of right format
#       merge types of the right format
#       content_location in the correct format
# stop if any validation breaks
.validate.template.definition <- function(definition) {
        
        # Mandatory fields should be present
        missing_names <- setdiff(.template.field.names, names(definition))
        if (length(missing_names) != 0) {
                stop(paste0("Missing template field: ", missing_names, "\n"))
        }
        
        # Check that the template type field is valid
        invalid_types <- setdiff(definition$template_type, .available.template.types)
        if(length(invalid_types)>0) {
                stop(paste0("Invalid template type: ", invalid_types, "\n"))
        }
        
        # Make sure merge types are valid
        invalid_mergetypes <- setdiff(definition$merge,.template.merge.types)
        if (length(invalid_mergetypes) != 0) {
                stop(paste0("Invalid values found in merge field: ", invalid_mergetypes))
        }
        
        # Parse the content_location field to extract the embedded information
        definition <- .parse.content.location(definition)
        
        definition
}

# Check the root template has the right format
.validate.root.template <- function(definition) {
        
        # Make sure only root items are included in the definition
        definition <- definition[definition$template_type == "root",]
        
        # Mandatory fields for root templates should be present
        missing_names <- setdiff(.root.template.field.names, names(definition))
        if (length(missing_names) != 0) {
                stop(paste0("Missing template field: ", missing_names, "\n"))
        }
        
        # make the default column a logical value
        definition$default <- as.logical(definition$default)
        
        # Make the first item a default if another one isn't, or if there are more
        # than one default
        if (sum(definition$default) != 1) {
                definition$default <- c(TRUE, rep(FALSE, length(definition$default)-1))
        }
        
        # Make sure there are no duplicate template_name
        duplicates <- definition$template_name[duplicated(definition$template_name)]
        if (length(duplicates) > 0) {
                stop(paste0("Duplicate template name found in template_name field: ", duplicates, "\n"))
        }
        
        
        definition
}

# Read a template definition file, validate it and return the contents as a dataframe
.read.template.definition <- function (template.file) {
        definition <- as.data.frame(read.dcf(template.file), 
                                    stringsAsFactors = FALSE)
        
        # return NULL if no templates defined
        if (.no.templates %in% names(definition)) return(NULL)
        
        definition <- .validate.template.definition(definition)
        
        definition
}

# Read a template definition file, validate it and return the contents as a dataframe
.read.root.template <- function () {
        
        .require.root.template()
        
        # read the file and perform basic validation
        definition <- .read.template.definition(.root.template.file)
        
        # if no root template defined return false
        if (is.null(definition)) return(definition)
        
        # apply additional checks for root template files
        definition <- .validate.root.template(definition)
        
        # Save any validation fixes back
        .save.root.template(definition)
        
        definition
}

# Save a validated template definition file as the root template
.save.root.template <- function (definition) {
        
        root_template_fields <- c(.template.field.names, .root.template.field.names)
        
        # only save relevant columns from the definition
        definition <- definition[,root_template_fields]
        write.dcf(definition, .root.template.field.names)
}

# Clear all root template definitions
.clear.root.template <- function () {
        unlink(.root.template.file)
        .require.root.template()
}


# Check if the root template file exists, if it doesn't create an empty one
.require.root.template <- function() {
        if(!file.exists(.root.template.file)) {
                no_templates <- data.frame(x="")
                colnames(no_templates) <- .no.templates
                write.dcf(no_templates, .root.template.file)
        }
}



# Set a new location for the root template for the current ProjectTemplate installation
.set.root.location <- function (location, type) {
        if (!(type %in% .available.types)) {
                message(paste0("Invalid type: ", type))
                return(invisible(NULL))
        }
        if (is.null(location)) {
                location <- "NULL"
        }
        else if (!.is.dir(location)) {
                message(paste0("Invalid template location: ", location))
                return(invisible(NULL))
        }
        
        location <- data.frame(location=location, type=type)
        write.dcf(location, .root.template.file)
        
        # Create a backup of the root location
        if(!.is.dir(.root.templatebackup.dir)) dir.create(.root.templatebackup.dir)
        write.dcf(location, .root.templatebackup.file)
}


# Extract info about the root template file 
.extract.roottemplate.info <- function () {
        template.root <- .get.root.location()
        if (is.null(template.root)) return(NULL)
        
        # read raw template information
        if (template.root$type == "github") {
                templates <- .download.github(template.root$location)
        }
        else if (template.root$type == "local") {
                templates <- template.root$location
                sub.dirs <- list.dirs(templates)
                template.names <- basename(.remove.first(sub.dirs))
                template.info <- data.frame(
                        clean.names = sub("(.*)_default$", "\\1", 
                                          template.names),
                        default = grepl("_default$", template.names),
                        path = file.path(templates, template.names)
                )        
        }
        else {
                template.info <- NULL
        }
        if (nrow(template.info)>0){
                # sort into order - default first
                template.info <- template.info[with(template.info, 
                                                    order(-default, clean.names)),]
                # and make sure there is only one default
                template.info$default <- c(TRUE, rep(FALSE, nrow(template.info)-1))
        }
        template.info
}

# Provide the status of templates defined under the template root
.root.template.status <- function () {
        template.info <- .get.template.names()
        if (is.null(template.info)) {
                message <- paste0(c("Custom Templates not configured for this installation.",
                                    "Run configure.template() to set up where ProjectTemplate should look for your custom templates"),
                                  collapse = "\n")
#root <- "no_root"
        }
        else if (nrow(template.info)==0) {
                return(
                        message(paste0(c(paste0("No templates are located at ", .get.root.location()$location),
                                         "Add sub directories there to start using custom templates"),
                                       collapse = "\n"))
                )
        }
        else {
                templates <- ifelse(template.info$default, 
                                    paste0("(*) ", template.info$clean.names),
                                    paste0("    ", template.info$clean.names))
                message(paste0(c("The following templates are available:", 
                                 templates,
                                 "If no template specified in create.project(), the default (*) will be used"),
                               collapse = "\n"))
        }
        
}

.download.github <- function (location) {
        #.require.package(devtools)
        library(devtools)
        gh_remote <- devtools:::github_remote(location)
        file_location <- devtools:::remote_download.github_remote(gh_remote)
        file_location
}


#
# This function is cut and pasted from devtools.  It would be better if it were
# exported from that package to allow it to be called from this package
# A request has been raised against that package to add this function to be 
# exported - when it is, this function can be deleted
#
github_remote <- function(repo, username = NULL, ref = NULL, subdir = NULL,
                          auth_token = github_pat(), sha = NULL,
                          host = "https://api.github.com") {
        
        meta <- devtools:::parse_git_repo(repo)
        meta <- devtools:::github_resolve_ref(meta$ref %||% ref, meta)
        
        if (is.null(meta$username)) {
                meta$username <- username %||% getOption("github.user") %||%
                        stop("Unknown username.")
                warning("Username parameter is deprecated. Please use ",
                        username, "/", repo, call. = FALSE)
        }
        
        devtools::remote("github",
               host = host,
               repo = meta$repo,
               subdir = meta$subdir %||% subdir,
               username = meta$username,
               ref = meta$ref,
               sha = sha,
               auth_token = auth_token
        )
}
