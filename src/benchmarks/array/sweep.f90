program bench_sweep
	implicit none
	real(8), parameter :: dt = 1.0d-6
	real(8), allocatable :: x(:), v(:), g(:)
	character(len = 32) :: arg
	integer :: n, k
	call get_command_argument(1, arg)
	read(arg, *) n
	allocate(x(n), v(n), g(n))
	x = 0
	v = 1
	g = -9.8d0
	do k = 1, 60000000 / n
		v = v + dt * g
		x = x + dt * v
	end do
	print *, sum(x)
end program bench_sweep
