program bisection
    implicit none

    real(8) :: x_low, x_up
    real(8) :: x_mid
    real(8) :: f_low, f_up, f_mid

    integer :: i, max_iterations

    max_iterations = 20

    write(*,*) "Provide intial guesses for lower and upper bracket"
    read(*,*) x_low, x_up

    f_low = exp(-1.0 * x_low) - x_low
    f_up = exp(-1.0 * x_up) - x_up

    if(f_low * f_up > 0) then
        write(*,*) "The initial guesses do not bracket the root. Try again"
        stop
    end if

    ! function is x = e**-x ==> e**-x - x = 0
    do i = 1, max_iterations
        x_mid = (x_low + x_up) / 2.0
        f_mid = exp(-1.0 * x_mid) - x_mid

        write(*,*) "iteration: ", i
        write(*,*) "x_low, x_mid, x_up: ", x_low, x_mid, x_up
        write(*,*) "f_low, f_mid, f_up: ", f_low, f_mid, f_up
        write(*,*)

        if (f_low * f_mid < 0) then
            x_up = x_mid
            f_up = f_mid
        else
            x_low = x_mid
            f_low = f_mid
        end if
    end do



end program bisection