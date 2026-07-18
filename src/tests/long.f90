
module long_m

	use syntran__test_core_m

	implicit none

	! Shard selector for splitting the long/AOC suite across parallel
	! processes in CI (see `program long` below for `--shard I/N` parsing).
	! Defaults run everything in a single process, same as before sharding
	! was added.
	integer :: g_shard_i = 0, g_shard_n = 1

	! Global test counter, advanced for every candidate test regardless of
	! whether this shard runs it, so indices stay stable across shards run in
	! different processes
	integer :: g_itest = 0

contains

!===============================================================================

subroutine chk(path, expected, npass, nfail)

	! Run one long/AOC test if it belongs to this shard (round-robin by
	! global test index) and tally pass/fail.  Path is relative to the repo
	! root; chdir_ = .true. lets the syntran program's relative input.txt
	! reads resolve inside its own day directory

	implicit none

	character(len = *), intent(in) :: path, expected
	integer, intent(inout) :: npass, nfail

	!********

	logical :: ok

	g_itest = g_itest + 1
	if (mod(g_itest - 1, g_shard_n) /= g_shard_i) return

	ok = interpret_file(path, quiet = .true., chdir_ = .true.) == expected

	if (ok) then
		npass = npass + 1
	else
		call console_color(fg_bright_red)
		write(*, '(a,i0,a)') '     Error: test ', g_itest, ' failed: '//path
		call console_color_reset()
		nfail = nfail + 1
	end if

end subroutine chk

!===============================================================================

subroutine parse_shard(str_, i, n)

	! Parse a "I/N" shard spec like "0/4" into i (0-based shard index) and n
	! (total shard count).  Exits the process with a nonzero status on any
	! malformed input

	implicit none

	character(len = *), intent(in) :: str_
	integer, intent(out) :: i, n

	!********

	integer :: islash, ios

	islash = index(str_, '/')
	if (islash < 2 .or. islash == len(str_)) then
		write(*,*) 'Error: malformed --shard arg "'//str_//'", expected "I/N"'
		call exit(1)
	end if

	read(str_(1: islash - 1), *, iostat = ios) i
	if (ios /= 0) then
		write(*,*) 'Error: malformed --shard index in "'//str_//'"'
		call exit(1)
	end if

	read(str_(islash + 1:), *, iostat = ios) n
	if (ios /= 0) then
		write(*,*) 'Error: malformed --shard count in "'//str_//'"'
		call exit(1)
	end if

	if (n < 1 .or. i < 0 .or. i >= n) then
		write(*,*) 'Error: --shard I/N must satisfy 0 <= I < N and N >= 1, got "'//str_//'"'
		call exit(1)
	end if

end subroutine parse_shard

!===============================================================================

subroutine unit_test_aoc_2017(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'aoc 2017'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: &
		path = 'src/tests/long/aoc/2017/'

	write(*,*) 'Unit testing '//label//' ...'

	! This one-off is included because it's the first AOC problem where I've used recursion
	call chk(path//"6/main.syntran" , '6681:2392', npass, nfail)
	call chk(path//"12/main.syntran", '141:171',   npass, nfail)

end subroutine unit_test_aoc_2017

!===============================================================================

subroutine unit_test_aoc_2019(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'aoc 2019'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: &
		path = 'src/tests/long/aoc/2019/'

	write(*,*) 'Unit testing '//label//' ...'

	call chk(path//"20/main.syntran", '514:6208', npass, nfail)

end subroutine unit_test_aoc_2019

!===============================================================================

subroutine unit_test_aoc_2023(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'aoc 2023'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: &
		path = 'src/tests/long/aoc/2023/'

	write(*,*) 'Unit testing '//label//' ...'

	call chk(path//"01/main.syntran",        '107443',             npass, nfail)
	call chk(path//"02/main.syntran",        '76485',              npass, nfail)
	call chk(path//"02/main-struct.syntran", '76485',              npass, nfail)
	call chk(path//"03/main.syntran",        '88145909',           npass, nfail)
	call chk(path//"04/main.syntran",        '6311320',            npass, nfail)
	call chk(path//"05/main.syntran",        '261668924',          npass, nfail)
	call chk(path//"06/main.syntran",        '34675170',           npass, nfail)
	call chk(path//"07/main.syntran",        '499106636',          npass, nfail)
	call chk(path//"08/main.syntran",        '13289612829906',     npass, nfail)
	call chk(path//"09/main.syntran",        '1819127106',         npass, nfail)
	call chk(path//"10/main.syntran",        '7220',                npass, nfail)
	call chk(path//"11/main.syntran",        '827019473638',       npass, nfail)
	call chk(path//"12/main.syntran",        '21',                  npass, nfail)
	call chk(path//"13/main.syntran",        '57259',               npass, nfail)
	call chk(path//"14/main.syntran",        '191157',              npass, nfail)
	call chk(path//"15/main.syntran",        '768500',              npass, nfail)
	call chk(path//"16/main.syntran",        '14553',               npass, nfail)
	call chk(path//"17/main.syntran",        '196',                 npass, nfail)
	call chk(path//"18/main.syntran",        '111131797001390',    npass, nfail)
	call chk(path//"19/main.syntran",        '125317462035060',    npass, nfail)
	call chk(path//"20/main.syntran",        '818723272',           npass, nfail)
	call chk(path//"21/main.syntran",        '3770',                npass, nfail)
	call chk(path//"22/main.syntran",        '66981',               npass, nfail)
	call chk(path//"23/main.syntran",        '248',                 npass, nfail)
	call chk(path//"24/main.syntran",        '25261',               npass, nfail)
	call chk(path//"25/main.syntran",        '555856',              npass, nfail)

end subroutine unit_test_aoc_2023

!===============================================================================

subroutine unit_test_aoc_2024(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'aoc 2024'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: &
		path = 'src/tests/long/aoc/2024/'

	write(*,*) 'Unit testing '//label//' ...'

	call chk(path//"1/main.syntran" , '32625824',        npass, nfail)
	call chk(path//"2/main.syntran" , '920',              npass, nfail)
	call chk(path//"3/main.syntran" , '266050255',        npass, nfail)
	call chk(path//"4/main.syntran" , '4596',             npass, nfail)
	call chk(path//"5/main.syntran" , '10050',            npass, nfail)
	call chk(path//"6/main.syntran" , '47',                npass, nfail) !'7180'
	call chk(path//"7/main.syntran" , '15136',             npass, nfail) !'42686972627683'
	call chk(path//"8/main.syntran" , '1731',              npass, nfail)
	call chk(path//"9/main.syntran" , '12764730558978',    npass, nfail)
	call chk(path//"10/main.syntran", '1493',              npass, nfail)
	call chk(path//"11/main.syntran", '218279375892027',   npass, nfail)
	call chk(path//"12/main.syntran", '2202472',           npass, nfail)
	call chk(path//"13/main.syntran", '93866170426408',    npass, nfail)
	call chk(path//"14/main.syntran", '232253028',         npass, nfail)
	call chk(path//"15/main.syntran", '19113',             npass, nfail)
	call chk(path//"16/main.syntran", '7081',              npass, nfail)
	call chk(path//"17/main.syntran", '1,6,7,4,3,0,5,0,6:216148338630253', npass, nfail)
	call chk(path//"18/main.syntran", '374:30,12',         npass, nfail)
	call chk(path//"19/main.syntran", '22',                npass, nfail)
	call chk(path//"20/main.syntran", '1521',              npass, nfail)
	call chk(path//"21/main.syntran", '170279148797334',   npass, nfail)
	call chk(path//"22/main.syntran", '37327646',          npass, nfail)
	call chk(path//"23/main.syntran", '893:cw,dy,ef,iw,ji,jv,ka,ob,qv,ry,ua,wt,xz', npass, nfail)
	call chk(path//"24/main.syntran", '60714423975686:',   npass, nfail)
	call chk(path//"25/main.syntran", '3356',              npass, nfail)

end subroutine unit_test_aoc_2024

!===============================================================================

subroutine unit_test_misc(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'long misc'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: &
		path = 'src/tests/long/aoc/'

	write(*,*) 'Unit testing '//label//' ...'

	call chk(path//"poople_test.syntran", 'true', npass, nfail)

end subroutine unit_test_misc

!===============================================================================

subroutine unit_tests_long(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	write(*,*) repeat('=', 60)
	write(*,*) 'Running long syntran unit tests ...'
	if (g_shard_n > 1) then
		write(*, '(a,i0,a,i0,a)') ' Running shard ', g_shard_i, ' of ', g_shard_n, ' ...'
	end if
	write(*,*)

	npass = 0
	nfail = 0

	call unit_test_aoc_2017(npass, nfail)
	call unit_test_aoc_2019(npass, nfail)
	call unit_test_aoc_2023(npass, nfail)
	call unit_test_aoc_2024(npass, nfail)
	call unit_test_misc    (npass, nfail)

	call log_test_summary(npass, nfail)

	iostat = nfail

end subroutine unit_tests_long

!===============================================================================

end module long_m

!===============================================================================

program long

	! Run with `fpm test long`, or pass `--shard I/N` (0-based I, e.g.
	! `--shard 0/4`) to run only every Nth test starting at I, splitting the
	! suite across N parallel processes for faster CI wall-time.  Omitting
	! the flag runs the full suite in one process, as before sharding existed.

	use syntran__app_m
	use long_m
	implicit none

	integer :: io
	integer :: nargs, iarg, arglen
	character(len = :), allocatable :: arg

	call set_ansi_colors(.true.)

	nargs = command_argument_count()
	iarg = 1
	do while (iarg <= nargs)

		call get_command_argument(iarg, length = arglen)
		allocate(character(len = arglen) :: arg)
		call get_command_argument(iarg, arg)

		if (arg == '--shard') then

			if (iarg == nargs) then
				write(*,*) 'Error: --shard requires an I/N argument'
				call exit(1)
			end if

			iarg = iarg + 1
			deallocate(arg)
			call get_command_argument(iarg, length = arglen)
			allocate(character(len = arglen) :: arg)
			call get_command_argument(iarg, arg)

			call parse_shard(arg, g_shard_i, g_shard_n)

		else
			write(*,*) 'Error: unrecognized arg "'//arg//'"'
			call exit(1)
		end if

		deallocate(arg)
		iarg = iarg + 1

	end do

	call unit_tests_long(io)
	call exit(io)

end program long

!===============================================================================

