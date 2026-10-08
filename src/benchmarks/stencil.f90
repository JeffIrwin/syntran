program bench_stencil
integer, parameter :: n = 1000000
real(8), allocatable :: u(:)
integer :: i, k
allocate(u(0:n-1)); u = [(1.0d-6*i, i=0,n-1)]; u = u*(1.0d0-u)
do k = 1, 200
  u(1:n-2) = u(1:n-2) + 0.25d0*(u(0:n-3) - 2.0d0*u(1:n-2) + u(2:n-1))
end do
print *, sum(u)
end program bench_stencil
