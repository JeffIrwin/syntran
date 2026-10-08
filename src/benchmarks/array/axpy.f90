program bench_axpy
integer, parameter :: n = 10000000
real(8), allocatable :: a(:), b(:)
integer :: i, k
allocate(a(0:n-1)); a = [(1.0d-7*i, i=0,n-1)]; b = 0.5d0*a
do k = 1, 50; a = a*0.999d0 + b; end do
print *, sum(a)
end program bench_axpy
