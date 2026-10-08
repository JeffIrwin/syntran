program bench_reduce
integer, parameter :: n = 10000000
real(8), allocatable :: a(:), b(:)
real(8) :: s
integer :: i, k
allocate(a(0:n-1)); a = [(1.0d-7*i, i=0,n-1)]; b = 1.0d0 - a
s = 0
do k = 1, 50; s = s + sum(a*b) + dot_product(a, b) + maxval(a - b); end do
print *, s
end program bench_reduce
