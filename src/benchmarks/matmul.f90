program bench_matmul
	implicit none
	integer, parameter :: n = 300
	real(8), allocatable :: m(:,:), c(:,:)
	integer :: i, j, k
	allocate(m(0:n-1, 0:n-1))
	do j = 0, n-1
		do i = 0, n-1
			m(i,j) = 1.0d-3 * (i + n*j)
		end do
	end do
	c = m
	do k = 1, 20
		c = 1.0d-3 * matmul(m, c)
	end do
	print *, sum(c)
end program bench_matmul
