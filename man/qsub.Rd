% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qsub.R
\name{qsub}
\alias{qsub}
\title{Run expressions on a gridengine system}
\usage{
qsub(
  ...,
  node = NULL,
  name = NULL,
  wd = getwd(),
  resources = character(),
  wait = FALSE,
  globals = TRUE,
  global_on_missing = c("ignore", "error"),
  packages = NULL
)
}
\arguments{
\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}>, Expressions to defuse.
Multiple expressions are run sequentially in the order the exist in \code{...}.}

\item{node}{Node to run the expressions.}

\item{name}{A string define the job name.}

\item{wd}{Path define the working directory.}

\item{resources}{A named characters define the given resources.}

\item{wait}{A logical (not \code{NA}) indicating whether the R interpreter should
wait for the command to finish, or run it asynchronously.}

\item{globals}{(optional) a scalar logical, a character vector, or a named
list to control how globals are handled. For details, see section 'Globals
used by expressions'}

\item{global_on_missing}{(character string) Action to take when non-existing
global variables ("globals" or "unknowns") are identified when the
expressions are created. If "error", an error is generated immediately. If
"ignore", no action is taken and an attempt to evaluate the expression will
be made. The latter is useful when there is a risk for false-positive globals
being identified, e.g. when expressions contain non-standard evaluation
(NSE). (Default: "ignore").}

\item{packages}{(optional) a character vector specifying packages
to be attached in the qsub R session.}
}
\value{
If \code{wait = TRUE} the value is the exit status returned by the
command, and if \code{wait = FALSE} it is \code{0} (the conventional success value). An
error code (\code{0} for success), given the \link{invisible} attribute (so needs to be
printed explicitly). If the command could not be run for any reason, the
value is \code{127} and a warning is issued (as from R 3.5.0).
}
\description{
Run expressions on a gridengine system
}
\section{Globals used by expressions}{

Global objects (short \emph{globals}) are objects (e.g. variables and
functions) that are needed in order for the expressions to be
evaluated while not being local objects. For example, in
\preformatted{
  a <- 42
  f <- qsub(b <- 2, a * b)
}
variable \code{a} is a global of assignment \code{f} whereas
\code{b} is a local variable.
In order for the expresssions to be resolved successfully (and correctly),
all globals need to be gathered when the expresssions are created such that
they are available whenever and wherever the expresssions is resolved in the
qsub R session.

The default behavior (\code{globals = TRUE}), is that globals are automatically
identified and gathered.  More precisely, globals are identified via code
inspection of the expressions and their values are retrieved with environment
\code{envir} as the starting point (basically via \code{get(global, envir = envir, inherits = TRUE)}).
\emph{In most cases, such automatic collection of globals is sufficient
and less tedious and error prone than if they are manually specified}.

However, for full control, it is also possible to explicitly specify
exactly which the globals are by providing their names as a character
vector.
In the above example, we could use
\preformatted{
  a <- 42
  f <- qsub(b <- 2, a * b, globals = "a")
}

Yet another alternative is to explicitly specify also their values
using a named list as in
\preformatted{
  a <- 42
  f <- qsub(b <- 2, a * b, globals = list(a = a))
}
or
\preformatted{
  f <- qsub(b <- 2, a * b, globals = list(a = 42))
}

Specifying globals explicitly avoids the overhead added from automatically
identifying the globals and gathering their values.  Furthermore, if we know
that the expressions does not make use of any global variables, we can
disable the automatic search for globals by using
\preformatted{
  f <- qsub({ a <- 42; b <- 2; a * b }, globals = FALSE)
}

Expressions often make use of functions from one or more packages.  As
long as these functions are part of the set of globals, the package will make
sure that those packages are attached when the expressions is resolved in the
qsub R session.  Because there is no need for such globals to be frozen or
exported, the package will not export them, which reduces the amount of
transferred objects.
For example, in
\preformatted{
  x <- rnorm(1000)
  f <- qsub({ median(x) })
}
variable \code{x} and \code{median()} are globals, but only \code{x}
is exported whereas \code{median()}, which is part of the \pkg{stats}
package, is not exported.  Instead it is made sure that the \pkg{stats}
package is on the search path when the expressions are evaluated in the qsub
R session.
Effectively, the above becomes
\preformatted{
  x <- rnorm(1000)
  f <- qsub({
    library("stats")
    median(x)
  })
}
To manually specify this, one can either do
\preformatted{
  x <- rnorm(1000)
  f <- qsub({
    median(x)
  }, globals = list(x = x, median = stats::median)
}
or
\preformatted{
  x <- rnorm(1000)
  f <- qsub({
    library("stats")
    median(x)
  }, globals = list(x = x))
}
Both are effectively the same.

Although rarely needed, a combination of automatic identification and manual
specification of globals is supported via attributes \code{add} (to add
false negatives) and \code{ignore} (to ignore false positives) on value
\code{TRUE}.  For example, with
\code{globals = structure(TRUE, ignore = "b", add = "a")} any globals
automatically identified except \code{b} will be used in addition to
global \code{a}.
}

