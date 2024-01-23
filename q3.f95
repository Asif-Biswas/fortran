program main
  implicit none
  real :: x, y, h, actual_solution
  integer :: i, n

  ! Define the initial values
  x = 0.0
  y = 2.0
  h = 0.1
  n = 2 / h  ! Number of steps

  ! Modified Euler's method
  do i = 1, n
    call modified_euler(x, y, h)
    x = x + h
    actual_solution = sqrt(2.0 * x**2 + 2.0)
    print*, x, y, actual_solution
  end do

contains

  subroutine modified_euler(x, y, h)
    real, intent(inout) :: x, y, h
    real :: k1, k2

    k1 = h * (2.0 * x - exp(2.0 * x) * y**2) / (y * exp(2.0 * x))
    k2 = h * (2.0 * (x + h) - exp(2.0 * (x + h)) * (y + k1)**2) / ((y + k1) * exp(2.0 * (x + h)))

    y = y + 0.5 * (k1 + k2)
  end subroutine modified_euler

end program main
