#' Build a Dockerfile
#'
#' `Dockerfile$new()` will create an object that can be used to
#' build a Dockerfile using a series of commands.
#' Once the Dockerfile is completed, you can save it to the disk.
#'
#' @return A dockerfile template

#' @importFrom R6 R6Class
#' @export
#'
#' @examples
#' my_dock <- Dockerfile$new()
Dockerfile <- R6::R6Class(
  "Dockerfile",
  public = list(
    #' @field Dockerfile The content of the Dockerfile
    Dockerfile = character(),
    ## Either from a file, or from a character vector
    #' @description Initialize a Dockerfile
    #' @param FROM base image
    #' @param AS of the image
    initialize = function(
      FROM = "rocker/r-base",
      AS = NULL
    ) {
      self$Dockerfile <- create_dockerfile(FROM, AS)
    },
    #' @description Add a RUN command
    #' @param cmd The command to add
    RUN = function(cmd) {
      self$Dockerfile <- c(self$Dockerfile, add_run(cmd))
    },
    #' @description Add ADD command
    #' @param from Base file
    #' @param to Dest file
    #' @param force Boolean, should the ADD be forced?
    ADD = function(from, to, force = TRUE) {
      self$Dockerfile <- c(self$Dockerfile, add_add(from, to, force))
    },
    #' @description Add a COPY command
    #' @param from Base file
    #' @param to Dest file
    #' @param force Boolean, should the ADD be forced?
    COPY = function(from, to, force = TRUE) {
      self$Dockerfile <- c(self$Dockerfile, add_copy(from, to, force))
    },
    #' @description Add a WORKDIR command
    #' @param where Location of the WORKDIR
    WORKDIR = function(where) {
      self$Dockerfile <- c(self$Dockerfile, add_workdir(where))
    },
    #' @description Add a EXPOSE command
    #' @param port The port to expose.
    EXPOSE = function(port) {
      self$Dockerfile <- c(self$Dockerfile, add_expose(port))
    },
    #' @description Add a VOLUME command
    #' @param volume The volume to add.
    VOLUME = function(volume) {
      self$Dockerfile <- c(self$Dockerfile, add_volume(volume))
    },
    #' @description Add a CMD command
    #' @param cmd The command to add
    CMD = function(cmd) {
      self$Dockerfile <- c(self$Dockerfile, add_cmd(cmd))
    },
    #' @description Add a LABEL command
    #' @param key,value The key value pair of the Label.
    LABEL = function(key, value) {
      self$Dockerfile <- c(self$Dockerfile, add_label(key, value))
    },
    #' @description Add a ENV command
    #' @param key,value The key value pair of the Env.
    ENV = function(key, value) {
      self$Dockerfile <- c(self$Dockerfile, add_env(key, value))
    },
    #' @description Add a ENTRYPOINT command
    #' @param cmd The command to launch as an entrypoint.
    ENTRYPOINT = function(cmd) {
      self$Dockerfile <- c(self$Dockerfile, add_entrypoint(cmd))
    },
    #' @description Add a USER command
    #' @param user The user name
    USER = function(user) {
      self$Dockerfile <- c(self$Dockerfile, add_user(user))
    },
    #' @description Add a ARG command.
    #' @param arg The ARG to add.
    #' @param ahead Should the arg be put  ahead of the Dockerfile?
    ARG = function(arg, ahead = FALSE) {
      if (ahead) {
        self$Dockerfile <- c(add_arg(arg), self$Dockerfile)
      } else {
        self$Dockerfile <- c(self$Dockerfile, add_arg(arg))
      }
    },
    #' @description Add a ONBUILD command
    #' @param cmd The command to launch onbuild.
    ONBUILD = function(cmd) {
      self$Dockerfile <- c(self$Dockerfile, add_onbuild(cmd))
    },
    #' @description Add a STOPSIGNAL command
    #' @param signal The signal.
    STOPSIGNAL = function(signal) {
      self$Dockerfile <- c(self$Dockerfile, add_stopsignal(signal))
    },
    #' @description Add a HEALTHCHECK command
    #' @param check The check.
    HEALTHCHECK = function(check) {
      self$Dockerfile <- c(self$Dockerfile, add_healthcheck(check))
    },
    #' @description Add a SHELL command
    #' @param shell The shell.
    SHELL = function(shell) {
      self$Dockerfile <- c(self$Dockerfile, add_shell(shell))
    },
    #' @description Add a MAINTAINER command
    #' @param name,email The maintainer mail and email.
    MAINTAINER = function(name, email) {
      self$Dockerfile <- c(self$Dockerfile, add_maintainer(name, email))
    },
    #' @description Add a custom command (you need to provide the verb)
    #' @param base The verb.
    #' @param cmd The content of the command.
    custom = function(base, cmd) {
      self$Dockerfile <- c(self$Dockerfile, add_custom(base, cmd))
    },
    #' @description Print the Dockerfile.
    print = function() {
      cat(self$Dockerfile, sep = "\n")
    },
    #' @description Print the Dockerfile.
    #' @param as The full path of the Dockerfile.
    write = function(as = "Dockerfile") {
      base::write(self$Dockerfile, file = as)
    },
    #' @description Switch two lines.
    #' @param a,b The two lines to switch.
    switch_cmd = function(a, b) {
      self$Dockerfile <- switch_them(self$Dockerfile, a, b)
    },
    #' @description Remove a line.
    #' @param where The line number.
    remove_cmd = function(where) {
      self$Dockerfile <- remove_from(self$Dockerfile, where)
    },
    #' @description Add a cmd after a given line.
    #' @param cmd The cmd to add.
    #' @param after The line number.
    add_after = function(cmd, after) {
      self$Dockerfile <- add_to(self$Dockerfile, cmd, after)
    }
  )
)
