
On Tue, 2022-08-30 at 16:59 +0200, Victoria Wimmer wrote:
> Thanks,
> 
> You need to call on.exit() directly after oldpar <- par(no.readonly =
> TRUE) in code line 30 in R/plot_points_on_atpol.R:

Thanks, fixed.

Regards,
Grzegorz
===
On Tue, 2022-08-16 at 11:12 +0200, Beni Altmann wrote:

Thanks Benjamin,

> Thanks,
> 
> I think you missed one change of par() in R/plot_points_on_atpol.R
> line 30.

Yeah, however it's internal function, called by other with oldpar restored. Nevertheless, corrected.

> There is also still a print() in R/atpol_grids.R line 17 and one in 
> R/miscRscript.r line 7.

Fixed.

> Please fix and resubmit.
> 
> Best,
> Benjamin Altmann

=======

On Fri, 2022-08-12 at 14:30 +0200, Victoria Wimmer wrote:

First of all, thank you Victoria for your input. As that's my first package, I really appreciate it.

> Please remove the redundant "in R" from your title.

Done

> Please always write package names, software names and API
> (application programming interface) names in single quotes in title and
> description. e.g: --> 'atpolR '
> Please note that package names are case sensitive.

Single quotes added

> Please rather use the Authors@R field and declare Maintainer, Authors
> and Contributors with their appropriate roles with person() calls.
> e.g. something like: [...]

Changed.

> If there are references describing the methods in your package,
> please add these in the description field of your DESCRIPTION 
> file in the form [...]

Added.

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the 
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g. 
> \value{No return value, called for side effects} or similar)
> Missing Rd-tags:
>       grid_to_latlon.Rd: \value
>       plotPoitsOnAtpol.Rd: \value

Added.

> \dontrun{} should only be used if the example really cannot be
> executed  (e.g. because of missing additional software, missing 
> API keys, ...) by the user. That's why wrapping examples in 
> \dontrun{} adds the comment ("# Not run:") as a warning for the user.

Removed and changed to \donttest{} in one case.

> You write information messages to the console that cannot be easily 
> suppressed. It is more R like to generate objects that can be used to
> extract the information a user is interested in, and then print()
> that 
> object.
> Instead of print() rather use message()/warning()

Changed to message()/warning()

> Please make sure that you do not change the user's options, par or 
> working directory. If you really have to do so within functions,
> please ensure with an *immediate* call of on.exit() that the settings are
> reset when the function is exited. e.g.:

Done. I wasn't aware it changes the option globally, not only within the function. 
Added oldpar/definition and recall in vignette as well.

Thanks for checking and hints.
Regards,
Grzegorz

=====
On Sun, 2022-07-24 at 23:43 +0200, Uwe Ligges wrote:
> Thanks, we see:
> 
>    License components with restrictions and base license permitting
> such:
>      GPL-3 + file LICENSE
>    File 'LICENSE':
>                          GNU GENERAL PUBLIC LICENSE
>                             Version 3, 29 June 2007
> 
>       Copyright (C) 2007 Free Software Foundation, Inc.
> <https://fsf.org/>
>       Everyone is  ..
> 
> Please omit "+ file LICENSE" and the file itself which is part of R 
> anyway. It is only used to specify additional restrictions to the GPL
> such as attribution requirements.
> 
> 
> Pls resubmit after the CRAN vacations.

License file removed.

=======
## R CMD check results

Duration: 1m 35.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
