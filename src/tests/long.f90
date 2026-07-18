
module long_m

	use syntran__test_core_m

	implicit none

	! Single-test selector for running one long/AOC test at a time from CI
	! (see `program long` below for `--test I` / `--count` parsing).  -1 (the
	! default) means run every test in this process, same as before this
	! selection mode existed
	integer :: g_target = -1

	! Counting mode: walk every test's index without running it, so `--count`
	! can report how many tests exist without paying to run them
	logical :: g_dry_run = .false.

	! Suppress section banners and the final summary.  Used by --count and
	! --test so a CI work queue running many single-test processes in
	! parallel doesn't spam per-process banners into the shared log
	logical :: g_quiet = .false.

	! Global test counter, advanced for every candidate test regardless of
	! whether it's the one selected by g_target, so indices stay stable
	! across single-test invocations run in different processes
	integer :: g_itest = 0

contains

!===============================================================================

subroutine chk(path, expected, npass, nfail)

	! Run one long/AOC test if it's selected (either g_target is unset and
	! every test runs, or this is the g_target'th test) and tally pass/fail.
	! In g_dry_run mode, just advance the counter without running anything, so
	! --count can report the total.  Path is relative to the repo root;
	! chdir_ = .true. lets the syntran program's relative input.txt reads
	! resolve inside its own day directory

	implicit none

	character(len = *), intent(in) :: path, expected
	integer, intent(inout) :: npass, nfail

	!********

	logical :: ok

	g_itest = g_itest + 1
	if (g_dry_run) return
	if (g_target >= 0 .and. g_itest - 1 /= g_target) return

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

subroutine unit_test_aoc_2017(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'aoc 2017'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: &
		path = 'src/tests/long/aoc/2017/'

	if (.not. g_quiet) write(*,*) 'Unit testing '//label//' ...'

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

	if (.not. g_quiet) write(*,*) 'Unit testing '//label//' ...'

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

	if (.not. g_quiet) write(*,*) 'Unit testing '//label//' ...'

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

	if (.not. g_quiet) write(*,*) 'Unit testing '//label//' ...'

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

	if (.not. g_quiet) write(*,*) 'Unit testing '//label//' ...'

	call chk(path//"poople_test.syntran", 'true', npass, nfail)

end subroutine unit_test_misc

!===============================================================================

subroutine unit_tests_long(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	if (.not. g_quiet) then
		write(*,*) repeat('=', 60)
		write(*,*) 'Running long syntran unit tests ...'
		write(*,*)
	end if

	npass = 0
	nfail = 0

	call unit_test_aoc_2017(npass, nfail)
	call unit_test_aoc_2019(npass, nfail)
	call unit_test_aoc_2023(npass, nfail)
	call unit_test_aoc_2024(npass, nfail)
	call unit_test_misc    (npass, nfail)

	if (.not. g_quiet) call log_test_summary(npass, nfail)

	iostat = nfail

end subroutine unit_tests_long

!===============================================================================

end module long_m

!===============================================================================

program long

	! Run with `fpm test long` to run the whole suite, `--count` to print the
	! number of tests without running them, or `--test I` (0-based) to run
	! only test I.  A CI work queue combines these: query `--count` once,
	! then dispatch one `--test I` process per index across as many parallel
	! workers as there are cores (e.g. `xargs -P`), which dynamically hands
	! each worker the next index as soon as it finishes, so slow AOC problems
	! don't bottleneck a fixed shard the way a static split would.

	use syntran__app_m
	use long_m
	implicit none

	integer :: io
	integer :: nargs, iarg, arglen, ios
	logical :: count_mode
	character(len = :), allocatable :: arg

	call set_ansi_colors(.true.)

	count_mode = .false.

	nargs = command_argument_count()
	iarg = 1
	do while (iarg <= nargs)

		call get_command_argument(iarg, length = arglen)
		allocate(character(len = arglen) :: arg)
		call get_command_argument(iarg, arg)

		if (arg == '--count') then

			count_mode = .true.
			g_dry_run  = .true.
			g_quiet    = .true.

		else if (arg == '--test') then

			if (iarg == nargs) then
				write(*,*) 'Error: --test requires an I argument'
				call exit(1)
			end if

			iarg = iarg + 1
			deallocate(arg)
			call get_command_argument(iarg, length = arglen)
			allocate(character(len = arglen) :: arg)
			call get_command_argument(iarg, arg)

			read(arg, *, iostat = ios) g_target
			if (ios /= 0 .or. g_target < 0) then
				write(*,*) 'Error: malformed --test index "'//arg//'", expected a non-negative integer'
				call exit(1)
			end if
			g_quiet = .true.

		else
			write(*,*) 'Error: unrecognized arg "'//arg//'"'
			call exit(1)
		end if

		deallocate(arg)
		iarg = iarg + 1

	end do

	call unit_tests_long(io)

	if (count_mode) then
		write(*, '(i0)') g_itest
		call exit(0)
	end if

	if (g_target >= 0 .and. g_target >= g_itest) then
		write(*, '(a,i0,a,i0)') &
			'Error: --test index ', g_target, ' is out of range, expected 0 to ', &
			g_itest - 1
		call exit(1)
	end if

	call exit(io)

end program long

!===============================================================================

