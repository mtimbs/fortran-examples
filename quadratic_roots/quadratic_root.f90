program quadratic_roots
    ! (-b + sqrt(b2 - 4ac)) / 2a
    implicit none

    real(8) :: a,b,c ! user input
    real(8) :: x1, x2 ! user output
    real(8) :: discrim ! intermediate variable

    write(*,*) "This program solves for the roots of a quadratic equation in the form ax2 + bx +c."
    write(*,*) "Please tell me the values of a,b,c"

    read(*,*) a, b, c

    discrim = b**2 - 4*a*c

    if (discrim > 0) then
        x1 = (-b - sqrt(discrim)) / (2*a)
        x2 = (-b + sqrt(discrim)) / (2*a)
        write(*,*) "The roots of the given quadratic are: ", x1, x2
    elseif (discrim == 0) then
        x1 = -b / (2*a)
        write(*,*) "The singular root of the given quadratic is: ", x1
    else
        write(*,*) "The roots are imaginary. Burn"
    end if


end program quadratic_roots