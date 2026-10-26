program bench_small
real(8) :: x(0:2), v(0:2), g(0:2), dt
integer :: k
x = 0; v = [1d0, 2d0, 3d0]; g = [0d0, -9.8d0, 0d0]; dt = 1d-6
do k = 1, 2000000; v = v + dt*g; x = x + dt*v; end do
print *, sum(x)
end program bench_small
