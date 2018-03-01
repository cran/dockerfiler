#' A Dockerfile template
#'
#' This R6 object contains a series of methods designed to create a Dockerfile.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{RUN}}{add a RUN command to your Dockerfile}
#'   \item{\code{ADD}}{add an ADD command to your Dockerfile}
#'   \item{\code{COPY}}{add a COPY command to your Dockerfile}
#'   \item{\code{WORKDIR}}{add a WORKDIR command to your Dockerfile}
#'   \item{\code{EXPOSE}}{add an EXPOSE command to your Dockerfile}
#'   \item{\code{VOLUME}}{add a VOLUME command to your Dockerfile}
#'   \item{\code{CMD}}{add a CMD command to your Dockerfile}
#'   \item{\code{ENV}}{add an ENV command to your Dockerfile}
#'   \item{\code{ENTRYPOINT}}{add an ENTRYPOINT command to your Dockerfile}
#'   \item{\code{USER}}{add a USER command to your Dockerfile}
#'   \item{\code{ARG}}{add an ARG command to your Dockerfile}
#'   \item{\code{ONBUILD}}{add an ONBUILD command to your Dockerfile}
#'   \item{\code{STOPSIGNAL}}{add a STOPSIGNAL command to your Dockerfile}
#'   \item{\code{HEALTHCHECK}}{add an HEALTHCHECK command to your Dockerfile}
#'   \item{\code{SHELL}}{add a SHELL command to your Dockerfile}
#'   \item{\code{MAINTAINER}}{add a MAINTAINER command to your Dockerfile}
#'   \item{\code{print}}{print your Dockerfile}
#'   \item{\code{write}}{write your Dockerfile}
#'   \item{\code{switch_cmd}}{switch two commands in your Dockerfile}
#'   \item{\code{remove_cmd}}{remove a command in your Dockerfile}
#' }
#'
#' @return A dockerfile template
#'
#' @importFrom R6 R6Class
#' @export
#'
#' @examples
#' my_dock <- Dockerfile$new()
#' my_dock$RUN(r(install.packages("attempt", repo = "http://cran.irsn.fr/")))
#' my_dock$RUN("mkdir /usr/scripts")
#' my_dock$COPY("plumberfile.R", "/usr/scripts/plumber.R")
#' my_dock$EXPOSE(8000)
#' my_dock$CMD("Rscript /usr/scripts/torun.R ")

Dockerfile <- R6::R6Class("Dockerfile",
                      public = list(
                        Dockerfile = character(),
                        ## Either from a file, or from a character vector
                        initialize = function(FROM = "rocker/r-base", AS = NULL){
                          self$Dockerfile <- create_dockerfile("rocker/r-base", AS)
                        },
                        RUN = function(cmd){
                          self$Dockerfile <- c(self$Dockerfile, add_run(cmd))
                        },
                        ADD = function(from, to, force = TRUE){
                          self$Dockerfile <- c(self$Dockerfile,add_add(from, to, force))
                        },
                        COPY = function(from, to, force = TRUE){
                          self$Dockerfile <- c(self$Dockerfile,add_copy(from, to, force))
                        },
                        WORKDIR = function(where){
                          self$Dockerfile <- c(self$Dockerfile, add_workdir(where))
                        },
                        EXPOSE = function(port){
                          self$Dockerfile <- c(self$Dockerfile, add_expose(port))
                        },
                        VOLUME = function(volume){
                          self$Dockerfile <- c(self$Dockerfile, add_volume(volume))
                        },
                        CMD = function(cmd){
                          self$Dockerfile <- c(self$Dockerfile, add_cmd(cmd))
                        },
                        LABEL = function(key, value){
                          self$Dockerfile <- c(self$Dockerfile, add_label(key, value))
                        },
                        ENV = function(key, value){
                          self$Dockerfile <- c(self$Dockerfile, add_env(key, value))
                        },
                        ENTRYPOINT = function(cmd){
                          self$Dockerfile <- c(self$Dockerfile, add_entrypoint(cmd))
                        },
                        USER = function(user){
                          self$Dockerfile <- c(self$Dockerfile, add_user(user))
                        },
                        ARG = function(arg, ahead = FALSE){
                          if (ahead) {
                            self$Dockerfile <- c(add_arg(arg), self$Dockerfile)
                          } else {
                            self$Dockerfile <- c(self$Dockerfile,add_arg(arg))
                          }
                        },
                        ONBUILD = function(cmd){
                          self$Dockerfile <- c(self$Dockerfile,add_onbuild(cmd))
                        },
                        STOPSIGNAL = function(signal){
                          self$Dockerfile <- c(self$Dockerfile,add_stopsignal(signal))
                        },
                        HEALTHCHECK = function(check){
                          self$Dockerfile <- c(self$Dockerfile,add_healthcheck(check))
                        },
                        SHELL = function(shell){
                          self$Dockerfile <- c(self$Dockerfile,add_shell(shell))
                        },
                        MAINTAINER = function(name, email){
                          self$Dockerfile <- c(self$Dockerfile,add_maintainer(name, email))
                        },
                        print = function(){
                          cat(self$Dockerfile, sep = '\n')
                        },
                        write = function(as = "Dockerfile"){
                          base::write(self$Dockerfile, file = as)
                        },
                        switch_cmd = function(a,b){
                          self$Dockerfile <- switch_them(self$Dockerfile, a, b)
                        },
                        remove_cmd = function(where){
                          self$Dockerfile <- remove_from(self$Dockerfile, where)
                        }
                      ))
