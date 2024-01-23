

program main
  implicit none
  real, dimension(3,3) :: A
  real, dimension(3) :: B
  real, dimension(3) :: X
  integer :: i, j, k, n
  real :: sum, factor

  ! Initialize the coefficient matrix A and the right-hand side vector B
  A = reshape([2.0, 4.0, 6.0, 3.0, 8.0, 5.0, -1.0, 1.0, 2.0], shape(A))
  B = [22.0, 27.0, 2.0]

  ! Perform LU-factorization
  n = size(A, 1)
  do k = 1, n-1
    do i = k+1, n
      factor = A(i,k) / A(k,k)
      A(i,k+1:n) = A(i,k+1:n) - factor * A(k,k+1:n)
      A(i,k) = factor
    end do
  end do

  ! Solve the system using forward and backward substitution
  X(1) = B(1) / A(1,1)
  do i = 2, n
    sum = B(i)
    do j = 1, i-1
      sum = sum - A(i,j) * X(j)
    end do
    X(i) = sum / A(i,i)
  end do

  do i = n-1, 1, -1
    sum = X(i)
    do j = i+1, n
      sum = sum - A(i,j) * X(j)
    end do
    X(i) = sum
  end do

  ! Display the solution
  print*, "Solution:"
  print*, "X" , X(1)
  print*, "Y" , X(2)
  print*, "Z" , X(3)

end program main
