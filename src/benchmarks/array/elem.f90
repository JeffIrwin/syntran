program bench_elem
integer, parameter :: n = 5000000
real(8), allocatable :: a(:), b(:)
integer :: i, k
allocate(a(0:n-1)); a = [(1.0d-6*i, i=0,n-1)]; allocate(b(0:n-1)); b = 0
do k = 1, 20; b = b + sqrt(abs(a)) + exp(-a*a); end do
print *, sum(b)
end program bench_elem
