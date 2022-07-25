## R CMD check results

Duration: 1m 35.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

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
