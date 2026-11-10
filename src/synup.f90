
!===============================================================================

module syntran__synup_m

	! `synup`, the toolchain manager.  It installs several versions of syntran
	! side by side under $SYNUP_HOME (default ~/.syntran), and links one of them
	! as bin/syntran.  The system `curl` does the downloading, so there's no
	! libcurl to link, and every download is checked against the release's
	! SHA256SUMS before anything is unpacked.
	!
	!     $SYNUP_HOME/
	!         bin/synup, bin/syntran    syntran is a link (a copy on Windows)
	!         versions/1.7.0/           the contents of a release zip, plus .sha256
	!         versions/dev/             the rolling `dev-latest` prerelease
	!         default                   `stable`, `dev`, or a version
	!
	! The release URLs and asset names are a lasting contract with every synup
	! that has shipped, so don't change them.  See the "Toolchain manager"
	! section of CLAUDE.md.

	use, intrinsic :: iso_c_binding
	use syntran__errors_m, only: err_prefix, warn_prefix
	use syntran__line_edit_m, only: enable_vt_processing, is_stdout_tty
	use syntran__utils_m

	implicit none

	! Everything else is a helper, and names like `exists` or `fail` shouldn't
	! leak to whoever uses this
	private

	public :: synup_cli, synup_colors, syntran_update, synup_home, sha256_of, sums_hash, ver_parse, ver_cmp, &
		parse_version, newest_of, tag_names

	character(len = *), parameter :: &
		SYNUP_RELEASES_URL = 'https://github.com/JeffIrwin/syntran/releases', &
		SYNUP_API_URL      = 'https://api.github.com/repos/JeffIrwin/syntran/releases', &
		SYNUP_DEV_TAG      = 'dev-latest', &
		SYNUP_SUMS         = 'SHA256SUMS', &
		SYNUP_STABLE       = 'stable', &
		SYNUP_DEV          = 'dev'

	! src/c/synup.c
	interface
		function c_asset(buf, n) bind(c, name = "synup_asset")
			import :: c_char, c_int
			character(kind = c_char), intent(out) :: buf(*)
			integer(c_int), value :: n
			integer(c_int) :: c_asset
		end function c_asset

		function c_self_exe(buf, n) bind(c, name = "synup_self_exe")
			import :: c_char, c_int
			character(kind = c_char), intent(out) :: buf(*)
			integer(c_int), value :: n
			integer(c_int) :: c_self_exe
		end function c_self_exe

		function c_exists(path) bind(c, name = "synup_exists")
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: path(*)
			integer(c_int) :: c_exists
		end function c_exists

		function c_mkdir(path) bind(c, name = "synup_mkdir")
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: path(*)
			integer(c_int) :: c_mkdir
		end function c_mkdir

		function c_rename(from, to) bind(c, name = "synup_rename")
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: from(*), to(*)
			integer(c_int) :: c_rename
		end function c_rename

		function c_remove(path) bind(c, name = "synup_remove")
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: path(*)
			integer(c_int) :: c_remove
		end function c_remove

		function c_symlink(target, link) bind(c, name = "synup_symlink")
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: target(*), link(*)
			integer(c_int) :: c_symlink
		end function c_symlink

		function c_chmod_x(path) bind(c, name = "synup_chmod_x")
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: path(*)
			integer(c_int) :: c_chmod_x
		end function c_chmod_x

		function c_sha256_file(path, hex) bind(c, name = "synup_sha256_file")
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: path(*)
			character(kind = c_char), intent(out) :: hex(*)
			integer(c_int) :: c_sha256_file
		end function c_sha256_file
	end interface

	private :: c_asset, c_self_exe, c_exists, c_mkdir, c_rename, c_remove, &
		c_symlink, c_chmod_x, c_sha256_file

!===============================================================================

contains

!===============================================================================

function cs(s) result(r)
	! Null-terminated for C
	character(len = *), intent(in) :: s
	character(len = :), allocatable :: r
	r = s//c_null_char
end function cs

!===============================================================================

function env_str(name) result(v)

	character(len = *), intent(in) :: name
	character(len = :), allocatable :: v

	integer :: n

	call get_environment_variable(name, length = n)
	if (n <= 0) then
		v = ''
		return
	end if
	allocate(character(len = n) :: v)
	call get_environment_variable(name, v)

end function env_str

!===============================================================================

function npath(p) result(r)
	! Backslashes for cmd.exe, which doesn't take `/` in every command
	character(len = *), intent(in) :: p
	character(len = :), allocatable :: r
	if (is_windows()) then
		r = replace_all(p, '/', '\')
	else
		r = p
	end if
end function npath

!===============================================================================

function sq(p) result(r)
	! A path, quoted for the shell
	character(len = *), intent(in) :: p
	character(len = :), allocatable :: r
	r = sh_quote(npath(p))
end function sq

!===============================================================================

function exe_name(name) result(r)
	character(len = *), intent(in) :: name
	character(len = :), allocatable :: r
	r = name
	if (is_windows()) r = name//'.exe'
end function exe_name

!===============================================================================

function null_dev() result(r)
	character(len = :), allocatable :: r
	r = '/dev/null'
	if (is_windows()) r = 'NUL'
end function null_dev

!===============================================================================

function run_cmd(cmd) result(ok)

	! Run a shell command and say whether it succeeded

	character(len = *), intent(in) :: cmd
	logical :: ok

	character(len = :), allocatable :: c
	integer :: es, cstat

	c = cmd
	! cmd.exe /c strips the outer quotes of a command that starts with one, so
	! it needs another pair around the whole thing
	if (is_windows()) c = '"'//cmd//'"'

	flush(output_unit)
	call execute_command_line(c, wait = .true., exitstat = es, cmdstat = cstat)
	ok = cstat == 0 .and. es == 0

end function run_cmd

!===============================================================================

function fail(msg) result(status)
	character(len = *), intent(in) :: msg
	integer :: status
	write(*,'(a)') err_prefix//msg//color_reset
	status = exit_failure
end function fail

!===============================================================================

function path_kind(path) result(r)
	! 0 for nothing, 1 for a file, 2 for a dir
	character(len = *), intent(in) :: path
	integer :: r
	r = int(c_exists(cs(path)))
end function path_kind

!===============================================================================

function make_dir(path) result(ok)
	character(len = *), intent(in) :: path
	logical :: ok
	ok = c_mkdir(cs(path)) == 0
end function make_dir

!===============================================================================

function make_tree(home) result(ok)

	! $SYNUP_HOME and its bin and versions.  One at a time, because the order
	! of the operands of `.or.` isn't defined, and a dir needs its parent

	character(len = *), intent(in) :: home
	logical :: ok

	ok = .false.
	if (.not. make_dir(home)) return
	if (.not. make_dir(home//'/bin')) return
	if (.not. make_dir(versions_dir(home))) return
	ok = .true.

end function make_tree

!===============================================================================

subroutine remove_tree(path)

	! `rm -rf`, and no complaint if it's not there

	character(len = *), intent(in) :: path

	logical :: ok

	if (path_kind(path) == 0) return
	if (is_windows()) then
		if (path_kind(path) == 2) then
			ok = run_cmd('rmdir /s /q '//sq(path))
		else
			ok = run_cmd('del /f /q '//sq(path))
		end if
	else
		ok = run_cmd('rm -rf '//sq(path))
	end if

end subroutine remove_tree

!===============================================================================

function write_text(path, text) result(ok)

	character(len = *), intent(in) :: path, text
	logical :: ok

	integer :: io, iu

	open(newunit = iu, file = path, status = 'replace', action = 'write', &
		access = 'stream', form = 'unformatted', iostat = io)
	if (io /= 0) then
		ok = .false.
		return
	end if
	write(iu, iostat = io) text
	close(iu)
	ok = io == 0

end function write_text

!===============================================================================

function copy_bin(src, dst) result(ok)

	! Copy a file's bytes.  Fortran stream i/o rather than `cp`/`copy`, which
	! differ per OS

	character(len = *), intent(in) :: src, dst
	logical :: ok

	character(len = :), allocatable :: s

	integer :: io

	ok = .false.
	s = read_file(src, io)
	if (io /= 0) return
	ok = write_text(dst, s)

end function copy_bin

!===============================================================================

function sha256_of(path, hex) result(ok)

	! Lowercase hex SHA-256 of a file

	character(len = *), intent(in) :: path
	character(len = 64), intent(out) :: hex
	logical :: ok

	character(kind = c_char) :: buf(65)

	integer :: i

	hex = ''
	ok = c_sha256_file(cs(path), buf) == 0
	if (.not. ok) return
	do i = 1, 64
		hex(i:i) = buf(i)
	end do

end function sha256_of

!===============================================================================

function sums_hash(text, name) result(hash)

	! The hash of asset `name` from the text of a SHA256SUMS file, in the format
	! of `sha256sum`: `<hex>  <name>` or `<hex> *<name>`.  Empty if there's no
	! such line

	character(len = *), intent(in) :: text, name
	character(len = :), allocatable :: hash

	character(len = :), allocatable :: line, rest

	integer :: i, j, n

	hash = ''
	n = len(text)
	i = 1
	do while (i <= n)
		j = index(text(i:), line_feed)
		if (j == 0) then
			j = n + 1
		else
			j = i + j - 1
		end if
		line = trim(text(i: j - 1))
		i = j + 1

		! A CRLF file
		if (len(line) > 0) then
			if (line(len(line):len(line)) == carriage_return) line = line(:len(line) - 1)
		end if

		if (len(line) < 66) cycle
		if (line(65:65) /= ' ') cycle
		rest = adjustl(line(66:))
		if (len(rest) > 0) then
			if (rest(1:1) == '*') rest = rest(2:)
		end if
		if (rest == name) then
			hash = to_lower(line(1:64))
			return
		end if
	end do

end function sums_hash

!===============================================================================

function ver_parse(s, v) result(ok)

	! "X.Y.Z" into 3 ints

	character(len = *), intent(in) :: s
	integer, intent(out) :: v(3)
	logical :: ok

	integer :: i, k, ndig

	v = 0
	ok = .false.
	k = 1
	ndig = 0
	do i = 1, len(s)
		if (s(i:i) >= '0' .and. s(i:i) <= '9') then
			ndig = ndig + 1
			if (ndig > 6) return
			v(k) = v(k) * 10 + (ichar(s(i:i)) - ichar('0'))
		else if (s(i:i) == '.' .and. ndig > 0 .and. k < 3) then
			k = k + 1
			ndig = 0
		else
			return
		end if
	end do
	ok = k == 3 .and. ndig > 0

end function ver_parse

!===============================================================================

function ver_cmp(a, b) result(c)

	! -1, 0, or 1 for a < b, a == b, a > b.  Strings that aren't versions sort
	! before ones that are

	character(len = *), intent(in) :: a, b
	integer :: c

	integer :: va(3), vb(3), i
	logical :: oka, okb

	oka = ver_parse(a, va)
	okb = ver_parse(b, vb)
	c = 0
	if (.not. oka .or. .not. okb) then
		if (oka .neqv. okb) c = merge(1, -1, oka)
		return
	end if
	do i = 1, 3
		if (va(i) /= vb(i)) then
			c = merge(1, -1, va(i) > vb(i))
			return
		end if
	end do

end function ver_cmp

!===============================================================================

function parse_version(text) result(ver)

	! The version out of the output of `syntran --version`, which is
	! "syntran X.Y.Z" somewhere in there.  Empty if it can't be found

	character(len = *), intent(in) :: text
	character(len = :), allocatable :: ver

	integer :: i, j, v(3)

	ver = ''
	i = index(text, 'syntran ')
	if (i == 0) return
	i = i + len('syntran ')
	j = i
	do while (j <= len(text))
		if (.not. (text(j:j) == '.' .or. (text(j:j) >= '0' .and. text(j:j) <= '9'))) exit
		j = j + 1
	end do
	if (ver_parse(text(i: j - 1), v)) ver = text(i: j - 1)

end function parse_version

!===============================================================================

function newest_of(names) result(newest)

	! The newest version in a list, ignoring anything that isn't one (e.g. `dev`)

	type(string_vector_t), intent(in) :: names
	character(len = :), allocatable :: newest

	integer :: i, v(3)

	newest = ''
	do i = 1, names%len_
		if (.not. ver_parse(names%v(i)%s, v)) cycle
		if (len(newest) == 0) then
			newest = names%v(i)%s
		else if (ver_cmp(names%v(i)%s, newest) > 0) then
			newest = names%v(i)%s
		end if
	end do

end function newest_of

!===============================================================================

function tag_names(json) result(tags)

	! The tags in a GitHub releases API response, by a plain scan for
	! `"tag_name": "`.  That's enough without a JSON parser, since a tag has no
	! escapes

	character(len = *), intent(in) :: json
	type(string_vector_t) :: tags

	character(len = *), parameter :: key = '"tag_name"'

	integer :: i, j, k, n

	tags = new_string_vector()
	n = len(json)
	i = 1
	do while (i <= n)
		j = index(json(i:), key)
		if (j == 0) exit
		j = i + j - 1 + len(key)
		i = j

		! Skip to the opening quote of the value
		k = index(json(j:), '"')
		if (k == 0) exit
		j = j + k
		k = index(json(j:), '"')
		if (k == 0) exit
		call tags%push(json(j: j + k - 2))
		i = j + k
	end do

end function tag_names

!===============================================================================

function synup_home() result(home)

	character(len = :), allocatable :: home, h

	home = env_str('SYNUP_HOME')
	if (len(home) > 0) return

	if (is_windows()) then
		h = env_str('USERPROFILE')
	else
		h = env_str('HOME')
	end if
	home = h//'/.syntran'

end function synup_home

!===============================================================================

function releases_url() result(url)
	! An override is for tests, which serve a fake release tree with file://
	character(len = :), allocatable :: url
	url = env_str('SYNUP_RELEASES_URL')
	if (len(url) == 0) url = SYNUP_RELEASES_URL
end function releases_url

!===============================================================================

function have_curl() result(ok)
	logical :: ok
	ok = run_cmd('curl --version > '//null_dev()//' 2>&1')
end function have_curl

!===============================================================================

function curl_get(url, out) result(ok)

	character(len = *), intent(in) :: url, out
	logical :: ok

	! -f makes an HTTP error a failure instead of saving the error page.  A few
	! retries, because a download can flake out
	ok = run_cmd('curl -fsSL --retry 3 -o '//sq(out)//' '//sh_quote(url))

end function curl_get

!===============================================================================

function list_dir(path) result(names)

	! The names in a dir, without the hidden ones.  Fortran has no way to list a
	! dir, so the shell does it, into a file

	character(len = *), intent(in) :: path
	type(string_vector_t) :: names

	character(len = :), allocatable :: tmp, text, cmd
	integer :: i, j, n, io

	names = new_string_vector()
	if (path_kind(path) /= 2) return

	tmp = path//'/.list.tmp'
	if (is_windows()) then
		cmd = 'dir /b /ad '//sq(path)//' > '//sq(tmp)
	else
		cmd = 'ls -1 '//sq(path)//' > '//sq(tmp)
	end if
	if (.not. run_cmd(cmd)) return

	text = read_file(tmp, io)
	call remove_tree(tmp)
	n = len(text)
	i = 1
	do while (i <= n)
		j = index(text(i:), line_feed)
		if (j == 0) then
			j = n + 1
		else
			j = i + j - 1
		end if
		if (j > i) then
			cmd = text(i: j - 1)
			if (cmd(len(cmd):len(cmd)) == carriage_return) cmd = cmd(:len(cmd) - 1)
			if (len(cmd) > 0) then
				if (cmd(1:1) /= '.') call names%push(cmd)
			end if
		end if
		i = j + 1
	end do

end function list_dir

!===============================================================================

function versions_dir(home) result(d)
	character(len = *), intent(in) :: home
	character(len = :), allocatable :: d
	d = home//'/versions'
end function versions_dir

!===============================================================================

function installed(home) result(names)
	! Every installed version, including `dev`
	character(len = *), intent(in) :: home
	type(string_vector_t) :: names
	names = list_dir(versions_dir(home))
end function installed

!===============================================================================

function default_spec(home) result(spec)

	! What `default` says, or `stable` if it's missing

	character(len = *), intent(in) :: home
	character(len = :), allocatable :: spec

	integer :: io

	spec = read_file(home//'/default', io)
	if (io /= 0) spec = ''
	spec = trim(adjustl(replace_all(replace_all(spec, line_feed, ''), carriage_return, '')))
	if (len(spec) == 0) spec = SYNUP_STABLE

end function default_spec

!===============================================================================

function resolve(home, spec) result(name)

	! The installed version that a spec means: `stable` is the newest release
	! that's installed.  Empty if there's no such version.  A spec is only ever
	! `stable`, `dev`, or a version, never a path, because callers remove the
	! dir that it names, and `x/../..` must not get that far

	character(len = *), intent(in) :: home, spec
	character(len = :), allocatable :: name

	integer :: v(3)

	name = ''
	if (spec == SYNUP_STABLE) then
		name = newest_of(installed(home))
	else if (spec == SYNUP_DEV .or. ver_parse(spec, v)) then
		if (path_kind(versions_dir(home)//'/'//spec) == 2) name = spec
	end if

end function resolve

!===============================================================================

function replace_exe(src, dst) result(ok)

	! Put a copy of `src` at `dst` while `dst` may be running.  POSIX can rename
	! over a running exe, and the process keeps its old inode.  Windows can't,
	! but it can rename a running exe out of the way.  The `.new` file is next
	! to `dst` so that the rename stays on one file system

	character(len = *), intent(in) :: src, dst
	logical :: ok

	character(len = :), allocatable :: new, old

	new = dst//'.new'
	old = dst//'.old'
	ok = .false.

	if (.not. copy_bin(src, new)) return
	if (c_chmod_x(cs(new)) /= 0) then
		call remove_tree(new)
		return
	end if

	if (is_windows()) then
		call remove_tree(old)
		if (path_kind(dst) /= 0) then
			if (c_rename(cs(dst), cs(old)) /= 0) then
				call remove_tree(new)
				return
			end if
		end if
		if (c_rename(cs(new), cs(dst)) /= 0) then
			! Put it back
			if (path_kind(old) /= 0) ok = c_rename(cs(old), cs(dst)) == 0
			ok = .false.
			call remove_tree(new)
			return
		end if
	else
		if (c_rename(cs(new), cs(dst)) /= 0) then
			call remove_tree(new)
			return
		end if
	end if

	ok = .true.

end function replace_exe

!===============================================================================

subroutine clean_old(home)
	! A Windows update leaves the exe that was running as `.old`
	character(len = *), intent(in) :: home
	call remove_tree(home//'/bin/'//exe_name('syntran')//'.old')
	call remove_tree(home//'/bin/'//exe_name('synup')//'.old')
end subroutine clean_old

!===============================================================================

function relink(home) result(status)

	! Point bin/syntran at the default version

	character(len = *), intent(in) :: home
	integer :: status

	character(len = :), allocatable :: spec, name, src, dst, tmp, lib

	status = exit_success
	spec = default_spec(home)
	name = resolve(home, spec)
	if (len(name) == 0) then
		if (spec == SYNUP_STABLE) then
			status = fail('no version is installed.  Try `synup install`')
		else
			status = fail('the default version `'//spec//'` is not installed')
		end if
		return
	end if

	src = versions_dir(home)//'/'//name//'/'//exe_name('syntran')
	dst = home//'/bin/'//exe_name('syntran')
	if (path_kind(src) /= 1) then
		status = fail('`'//src//'` is missing')
		return
	end if

	if (is_windows()) then
		if (.not. replace_exe(src, dst)) then
			status = fail('cannot replace `'//dst//'`')
		end if
		return
	end if

	! A relative target, so that the whole $SYNUP_HOME can move
	tmp = home//'/bin/.syntran.tmp'
	call remove_tree(tmp)
	if (c_symlink(cs('../versions/'//name//'/syntran'), cs(tmp)) /= 0 .or. &
		c_rename(cs(tmp), cs(dst)) /= 0) then
		call remove_tree(tmp)
		status = fail('cannot link `'//dst//'`')
		return
	end if

	! macOS builds load this from next to the exe
	lib = versions_dir(home)//'/'//name//'/libgomp.1.dylib'
	if (path_kind(lib) == 1) then
		tmp = home//'/bin/.libgomp.tmp'
		call remove_tree(tmp)
		if (c_symlink(cs('../versions/'//name//'/libgomp.1.dylib'), cs(tmp)) == 0) then
			if (c_rename(cs(tmp), cs(home//'/bin/libgomp.1.dylib')) /= 0) call remove_tree(tmp)
		end if
	end if

end function relink

!===============================================================================

function fetch_release(base, home, label, hash, tmp) result(status)

	! Download and check a release's zip, and unpack it to tmp/x.  `base` is the
	! URL dir that holds the assets.  The hash comes back for the caller to store

	character(len = *), intent(in) :: base, home, label
	character(len = 64), intent(out) :: hash
	character(len = :), allocatable, intent(out) :: tmp
	integer :: status

	character(len = :), allocatable :: asset, sums, want, zip, x
	character(len = 256) :: buf
	character(len = 64) :: got
	character(kind = c_char) :: cbuf(256)

	integer :: io, i

	status = exit_failure
	hash = ''

	if (c_asset(cbuf, 256) /= 0) then
		status = fail('there is no prebuilt syntran for this platform.  Build it from source')
		return
	end if
	buf = ''
	do i = 1, 256
		if (cbuf(i) == c_null_char) exit
		buf(i:i) = cbuf(i)
	end do
	asset = trim(buf)

	if (.not. have_curl()) then
		status = fail('`curl` was not found.  synup uses it to download')
		return
	end if

	tmp = versions_dir(home)//'/.tmp'
	call remove_tree(tmp)
	if (.not. make_dir(tmp)) then
		status = fail('cannot write to `'//tmp//'`')
		return
	end if

	write(*,'(a)') 'synup: downloading '//label//' ('//asset//') ...'

	sums = tmp//'/'//SYNUP_SUMS
	if (.not. curl_get(base//'/'//SYNUP_SUMS, sums)) then
		call remove_tree(tmp)
		status = fail('cannot download '//SYNUP_SUMS//' from `'//base//'`')
		return
	end if
	want = sums_hash(read_file(sums, io), asset)
	if (len(want) == 0) then
		call remove_tree(tmp)
		status = fail(SYNUP_SUMS//' has no checksum for '//asset//', so it is not installed')
		return
	end if

	zip = tmp//'/'//asset
	if (.not. curl_get(base//'/'//asset, zip)) then
		call remove_tree(tmp)
		status = fail('cannot download `'//base//'/'//asset//'`')
		return
	end if

	if (.not. sha256_of(zip, got)) then
		call remove_tree(tmp)
		status = fail('cannot read the download')
		return
	end if
	if (got /= want) then
		call remove_tree(tmp)
		status = fail('checksum mismatch for '//asset//', so it is not installed' &
			//line_feed//'  expected '//want//line_feed//'  got      '//got)
		return
	end if
	hash = got

	x = tmp//'/x'
	if (.not. make_dir(x)) then
		call remove_tree(tmp)
		status = fail('cannot write to `'//x//'`')
		return
	end if
	if (is_windows()) then
		! bsdtar has shipped with Windows since 10
		io = merge(0, 1, run_cmd('tar -xf '//sq(zip)//' -C '//sq(x)))
	else if (run_cmd('unzip -o -q '//sq(zip)//' -d '//sq(x))) then
		io = 0
	else if (run_cmd('tar -xf '//sq(zip)//' -C '//sq(x))) then
		io = 0
	else
		io = 1
	end if
	if (io /= 0) then
		call remove_tree(tmp)
		status = fail('cannot unpack '//asset//'.  Is `unzip` installed?')
		return
	end if

	status = exit_success

end function fetch_release

!===============================================================================

function find_unpacked(x) result(dir)

	! The dir of an unpacked release that holds syntran.  It's flat now, but the
	! packaging may nest it in `bin` some day

	character(len = *), intent(in) :: x
	character(len = :), allocatable :: dir

	dir = ''
	if (path_kind(x//'/'//exe_name('syntran')) == 1) then
		dir = x
	else if (path_kind(x//'/bin/'//exe_name('syntran')) == 1) then
		dir = x//'/bin'
	end if

end function find_unpacked

!===============================================================================

function synup_install(home, spec, quiet) result(status)

	! Install `latest`, `dev`, or a version X.Y.Z, and say what it was called
	! through the file $SYNUP_HOME/versions/.last

	character(len = *), intent(in) :: home, spec
	logical, intent(in) :: quiet
	integer :: status

	character(len = :), allocatable :: base, label, tmp, dir, name, vtext, dest
	character(len = 64) :: hash, old_hash
	integer :: io, v(3)
	logical :: ok

	status = exit_failure
	base = releases_url()

	if (spec == 'latest') then
		base = base//'/latest/download'
		label = 'the latest release'
	else if (spec == SYNUP_DEV) then
		base = base//'/download/'//SYNUP_DEV_TAG
		label = 'the dev build'
	else if (ver_parse(spec, v)) then
		base = base//'/download/'//spec
		label = 'syntran '//spec
	else
		status = fail('`'//spec//'` is not `latest`, `dev`, or a version like 1.7.0')
		return
	end if

	if (.not. make_tree(home)) then
		status = fail('cannot write to `'//home//'`.  Set SYNUP_HOME to somewhere you can')
		return
	end if

	status = fetch_release(base, home, label, hash, tmp)
	if (status /= exit_success) return
	status = exit_failure

	dir = find_unpacked(tmp//'/x')
	if (len(dir) == 0) then
		call remove_tree(tmp)
		status = fail('the download has no '//exe_name('syntran'))
		return
	end if
	! The zips lose the exec bits.  Releases before synup have no synup in them
	ok = c_chmod_x(cs(dir//'/'//exe_name('syntran'))) == 0
	if (path_kind(dir//'/'//exe_name('synup')) == 1) then
		ok = ok .and. c_chmod_x(cs(dir//'/'//exe_name('synup'))) == 0
	end if
	if (.not. ok) then
		call remove_tree(tmp)
		status = fail('cannot make syntran executable')
		return
	end if

	! Run it before it's kept, to know that it works on this machine, and to
	! learn which version `latest` is
	ok = run_cmd(sq(dir//'/'//exe_name('syntran'))//' --version > '// &
		sq(tmp//'/version.txt')//' 2>&1')
	vtext = read_file(tmp//'/version.txt', io)
	name = parse_version(vtext)
	if (.not. ok .or. len(name) == 0) then
		call remove_tree(tmp)
		status = fail('the downloaded syntran does not run on this machine')
		return
	end if

	if (spec == SYNUP_DEV) then
		dest = SYNUP_DEV
	else if (spec == 'latest') then
		dest = name
	else
		dest = spec
	end if

	io = 0
	old_hash = ''
	if (path_kind(versions_dir(home)//'/'//dest//'/.sha256') == 1) then
		old_hash = trim(read_file(versions_dir(home)//'/'//dest//'/.sha256', io))
	end if
	if (old_hash == hash) then
		call remove_tree(tmp)
		if (.not. quiet) write(*,'(a)') 'synup: '//dest//' is already installed and up to date'
		ok = write_text(versions_dir(home)//'/.last', dest)
		status = exit_success
		return
	end if

	if (.not. write_text(dir//'/.sha256', hash)) then
		call remove_tree(tmp)
		status = fail('cannot write to `'//dir//'`')
		return
	end if

	! The new one is good, so the old one can go
	call remove_tree(versions_dir(home)//'/'//dest)
	if (c_rename(cs(dir), cs(versions_dir(home)//'/'//dest)) /= 0) then
		call remove_tree(tmp)
		status = fail('cannot install into `'//versions_dir(home)//'/'//dest//'`')
		return
	end if
	call remove_tree(tmp)

	ok = write_text(versions_dir(home)//'/.last', dest)
	write(*,'(a)') 'synup: installed '//dest//' in '//versions_dir(home)//'/'//dest
	status = exit_success

end function synup_install

!===============================================================================

function last_installed(home) result(name)
	! What synup_install() just put in place
	character(len = *), intent(in) :: home
	character(len = :), allocatable :: name
	integer :: io
	name = trim(read_file(versions_dir(home)//'/.last', io))
	if (io /= 0) name = ''
end function last_installed

!===============================================================================

function synup_set_default(home, spec) result(status)

	character(len = *), intent(in) :: home, spec
	integer :: status

	character(len = :), allocatable :: name

	integer :: v(3)

	if (spec /= SYNUP_STABLE .and. spec /= SYNUP_DEV .and. .not. ver_parse(spec, v)) then
		status = fail('`'//spec//'` is not `stable`, `dev`, or a version like 1.7.0')
		return
	end if
	if (len(resolve(home, spec)) == 0) then
		if (spec == SYNUP_STABLE) then
			status = fail('no version is installed.  Try `synup install`')
		else
			status = fail(spec//' is not installed.  Try `synup install '//spec//'`')
		end if
		return
	end if

	if (.not. write_text(home//'/default', spec)) then
		status = fail('cannot write to `'//home//'`')
		return
	end if

	status = relink(home)
	if (status == exit_success) then
		! Not inside the write, because resolve() runs a command, which flushes
		! stdout, and a flush inside a write to the same unit deadlocks
		name = resolve(home, spec)
		write(*,'(a)') 'synup: the default is now '//spec//' ('//name//')'
	end if

end function synup_set_default

!===============================================================================

function synup_update(home) result(status)

	! Install the newest release if it's new, and refresh `dev` if it's installed

	character(len = *), intent(in) :: home
	integer :: status

	character(len = :), allocatable :: name, src, dst
	character(len = 64) :: h1, h2
	type(string_vector_t) :: have

	call clean_old(home)

	status = synup_install(home, 'latest', .false.)
	if (status /= exit_success) return
	name = last_installed(home)

	have = installed(home)
	if (any_dev(have)) then
		status = synup_install(home, SYNUP_DEV, .false.)
		if (status /= exit_success) return
	end if

	status = relink(home)
	if (status /= exit_success) return

	! A newer release may come with a newer synup.  Only if this is the newest
	! release, so that nothing is replaced by an older one
	if (len(name) > 0 .and. name == newest_of(installed(home))) then
		src = versions_dir(home)//'/'//name//'/'//exe_name('synup')
		dst = home//'/bin/'//exe_name('synup')
		if (path_kind(src) == 1) then
			if (sha256_of(src, h1)) then
				if (.not. sha256_of(dst, h2)) h2 = ''
				if (h1 /= h2) then
					if (replace_exe(src, dst)) then
						write(*,'(a)') 'synup: updated '//dst
					else
						write(*,'(a)') warn_prefix//'cannot update '//dst//color_reset
					end if
				end if
			end if
		end if
	end if

end function synup_update

!===============================================================================

function any_dev(names) result(r)
	type(string_vector_t), intent(in) :: names
	logical :: r
	integer :: i
	r = .false.
	do i = 1, names%len_
		if (names%v(i)%s == SYNUP_DEV) r = .true.
	end do
end function any_dev

!===============================================================================

function synup_init(home) result(status)

	! Make a new $SYNUP_HOME with the latest release in it, and a copy of this
	! synup

	character(len = *), intent(in) :: home
	integer :: status

	character(len = :), allocatable :: self, dst, other
	character(kind = c_char) :: cbuf(4096)
	character(len = 4096) :: buf

	integer :: i
	logical :: ok

	if (.not. make_tree(home)) then
		status = fail('cannot make `'//home//'`.  Set SYNUP_HOME to somewhere you can write')
		return
	end if

	dst = home//'/bin/'//exe_name('synup')
	if (c_self_exe(cbuf, 4096) == 0) then
		buf = ''
		do i = 1, 4096
			if (cbuf(i) == c_null_char) exit
			buf(i:i) = cbuf(i)
		end do
		self = trim(buf)
		if (.not. replace_exe(self, dst)) then
			status = fail('cannot copy synup to `'//dst//'`')
			return
		end if
	end if

	status = synup_install(home, 'latest', .false.)
	if (status /= exit_success) return

	if (.not. write_text(home//'/default', SYNUP_STABLE)) then
		status = fail('cannot write to `'//home//'`')
		return
	end if
	status = relink(home)
	if (status /= exit_success) return

	write(*,'(a)')
	write(*,'(a)') 'synup: done.  Put this in your PATH:'
	write(*,'(a)')
	write(*,'(a)') '    '//npath(home//'/bin')
	write(*,'(a)')

	! An older syntran elsewhere would be found first
	other = which_syntran()
	if (len(other) > 0 .and. npath(other) /= npath(home//'/bin/'//exe_name('syntran'))) then
		write(*,'(a)') warn_prefix//'`'//other//'` is already in your PATH.  It will be used '// &
			'until `'//npath(home//'/bin')//'` comes before it'//color_reset
	end if
	ok = .true.

end function synup_init

!===============================================================================

function which_syntran() result(path)

	! The syntran that the shell finds, or empty

	character(len = :), allocatable :: path

	character(len = :), allocatable :: tmp, cmd
	integer :: io

	path = ''
	tmp = synup_home()//'/.which.tmp'
	if (path_kind(synup_home()) /= 2) return
	if (is_windows()) then
		cmd = 'where syntran > '//sq(tmp)//' 2> '//null_dev()
	else
		cmd = 'command -v syntran > '//sq(tmp)//' 2> '//null_dev()
	end if
	if (.not. run_cmd(cmd)) then
		call remove_tree(tmp)
		return
	end if
	path = read_file(tmp, io)
	call remove_tree(tmp)
	! The first line only
	io = index(path, line_feed)
	if (io > 0) path = path(:io - 1)
	path = trim(path)
	if (len(path) > 0) then
		if (path(len(path):len(path)) == carriage_return) path = path(:len(path) - 1)
	end if

end function which_syntran

!===============================================================================

function synup_list(home, available) result(status)

	character(len = *), intent(in) :: home
	logical, intent(in) :: available
	integer :: status

	character(len = :), allocatable :: spec, name, json, mark
	type(string_vector_t) :: names, tags
	integer :: i, io
	logical :: ok

	status = exit_success

	if (available) then
		if (.not. have_curl()) then
			status = fail('`curl` was not found.  synup uses it to download')
			return
		end if
		! The dir for a temp file, if it can be made
		ok = make_dir(home)
		json = home//'/.releases.tmp'
		if (.not. curl_get(SYNUP_API_URL//'?per_page=100', json)) then
			status = fail('cannot list the releases from `'//SYNUP_API_URL//'`')
			return
		end if
		tags = tag_names(read_file(json, io))
		call remove_tree(json)
		write(*,'(a)') 'Available:'
		do i = 1, tags%len_
			write(*,'(a)') '    '//tags%v(i)%s
		end do
		return
	end if

	spec = default_spec(home)
	name = resolve(home, spec)
	names = installed(home)
	if (names%len_ == 0) then
		write(*,'(a)') 'Nothing is installed.  Try `synup init`'
		return
	end if

	write(*,'(a)') 'Installed in '//npath(versions_dir(home))//':'
	do i = 1, names%len_
		mark = ''
		if (names%v(i)%s == name) mark = '  (default: '//spec//')'
		write(*,'(a)') '    '//names%v(i)%s//mark
	end do

end function synup_list

!===============================================================================

function synup_remove(home, spec) result(status)

	character(len = *), intent(in) :: home, spec
	integer :: status

	character(len = :), allocatable :: name

	if (spec == SYNUP_STABLE) then
		status = fail('`stable` is not a version.  Say which one to remove')
		return
	end if
	if (len(resolve(home, spec)) == 0) then
		status = fail(spec//' is not installed')
		return
	end if

	name = resolve(home, default_spec(home))
	if (name == spec) then
		status = fail(spec//' is the default.  Pick another with `synup default` first')
		return
	end if

	call remove_tree(versions_dir(home)//'/'//spec)
	write(*,'(a)') 'synup: removed '//spec
	status = exit_success

end function synup_remove

!===============================================================================

function synup_run(home, spec, args) result(status)

	! Run an installed version without changing the default

	character(len = *), intent(in) :: home, spec
	type(string_vector_t), intent(in) :: args
	integer :: status

	character(len = :), allocatable :: name, cmd, c
	integer :: i, es, cstat

	name = resolve(home, spec)
	if (len(name) == 0) then
		status = fail(spec//' is not installed')
		return
	end if

	cmd = sq(versions_dir(home)//'/'//name//'/'//exe_name('syntran'))
	do i = 1, args%len_
		cmd = cmd//' '//sh_quote(args%v(i)%s)
	end do

	c = cmd
	if (is_windows()) c = '"'//cmd//'"'
	flush(output_unit)
	call execute_command_line(c, wait = .true., exitstat = es, cmdstat = cstat)
	if (cstat /= 0) then
		status = exit_failure
	else
		status = es
	end if

end function synup_run

!===============================================================================

function syntran_update() result(status)

	! `syntran --update`.  The work is synup's, since a syntran can't replace
	! itself while it runs, and synup manages every version, not just this one

	integer :: status

	logical :: ok

	if (is_windows()) then
		ok = run_cmd('where synup > NUL 2>&1')
	else
		ok = run_cmd('command -v synup > /dev/null 2>&1')
	end if
	if (.not. ok) then
		status = fail('`synup` was not found in your PATH.  It comes in the release zip, '// &
			'next to syntran.  Run `synup init` to set up syntran and synup together')
		return
	end if

	status = merge(exit_success, exit_failure, run_cmd('synup update'))

end function syntran_update

!===============================================================================

subroutine synup_colors()

	! Color the output like syntran does, when stdout is a terminal.  This is
	! the `--color auto` of set_ansi_colors() in src/app.f90, which can't be
	! used here: it's in a module that pulls in the whole interpreter, and
	! synup is a small exe

	if (is_stdout_tty()) then
		call enable_vt_processing()
		fg_bold              = FG_BOLD_
		fg_bold_bright_red   = FG_BOLD_BRIGHT_RED_
		fg_bold_bright_green = FG_BOLD_BRIGHT_GREEN_
		fg_bright_cyan       = FG_BRIGHT_CYAN_
		fg_bold_bright_cyan  = FG_BOLD_BRIGHT_CYAN_
		fg_bold_yellow       = FG_BOLD_YELLOW_
		color_reset          = COLOR_RESET_
	else
		fg_bold              = ''
		fg_bold_bright_red   = ''
		fg_bold_bright_green = ''
		fg_bright_cyan       = ''
		fg_bold_bright_cyan  = ''
		fg_bold_yellow       = ''
		color_reset          = ''
	end if

	! These end in fg_bold, so a message after one must end with color_reset
	err_prefix  = fg_bold_bright_red//'Error'//fg_bold//': '
	warn_prefix = fg_bold_yellow//'Warning'//fg_bold//': '

end subroutine synup_colors

!===============================================================================

subroutine print_help_item(first, name, arg, desc, desc2)

	! One command or option of `--help`, laid out like print_help_opt() in
	! src/app.f90: the name on one line, and the description indented below

	character(len = *), intent(in) :: first, name, arg, desc
	character(len = *), intent(in), optional :: desc2

	character(len = :), allocatable :: line

	if (len(first) > 0) then
		line = "  "//fg_bold_bright_cyan//first//color_reset//", "
	else
		line = "  "
	end if
	line = line//fg_bold_bright_cyan//name//color_reset
	if (len(arg) > 0) line = line//" "//fg_bright_cyan//arg//color_reset

	write(*,*) line
	write(*,*) repeat(" ", 10)//desc
	if (present(desc2)) write(*,*) repeat(" ", 10)//desc2

end subroutine print_help_item

!===============================================================================

subroutine synup_help()

	write(*,*) "The syntran toolchain installer"
	write(*,*)
	write(*,*) fg_bold_bright_green//"Usage:"//color_reset//" "// &
		fg_bold_bright_cyan//"synup"//color_reset//" "// &
		fg_bright_cyan//"<COMMAND>"//color_reset
	write(*,*)
	write(*,*) fg_bold_bright_green//"Commands:"//color_reset
	call print_help_item("", "init", "", &
		"Set up ~/.syntran with the latest release and this synup")
	call print_help_item("", "install", "[VERSION]", &
		"Install `latest` [default], `dev`, or a version like 1.7.0")
	call print_help_item("", "default", "<VERSION>", &
		"Run this version as `syntran`: `stable` (the newest release), `dev`,", &
		"or a version")
	call print_help_item("", "update", "", &
		"Install the newest release if there is a new one, and refresh `dev`", &
		"if it is installed")
	call print_help_item("", "list", "[--available]", &
		"Show what is installed, or the releases that could be")
	call print_help_item("", "remove", "<VERSION>", &
		"Delete an installed version, unless it is the default")
	call print_help_item("", "run", "<VERSION> [ARGS]...", &
		"Run an installed version once, without changing the default")
	write(*,*)
	write(*,*) fg_bold_bright_green//"Options:"//color_reset
	call print_help_item("-h", "--help", "", "Show this help")
	call print_help_item("", "--version", "", "Show the version of synup")
	write(*,*)
	write(*,*) "Everything is in $SYNUP_HOME, which is ~/.syntran by default"
	write(*,*)

end subroutine synup_help

!===============================================================================

function synup_cli(argv, version) result(status)

	! The whole of the `synup` program.  argv doesn't have the program name.
	! `version` is this synup's, for --version

	type(string_vector_t), intent(in) :: argv
	character(len = *), intent(in) :: version
	integer :: status

	character(len = :), allocatable :: home, cmd, spec
	type(string_vector_t) :: rest
	integer :: i

	if (argv%len_ == 0) then
		call synup_help()
		status = exit_failure
		return
	end if

	cmd = argv%v(1)%s
	select case (cmd)
	case ('-h', '--help', 'help')
		call synup_help()
		status = exit_success
		return
	case ('--version', '-V')
		write(*,'(a)') 'synup '//version
		status = exit_success
		return
	end select

	home = synup_home()
	call clean_old(home)

	select case (cmd)
	case ('init')
		status = synup_init(home)

	case ('install')
		spec = 'latest'
		if (argv%len_ >= 2) spec = argv%v(2)%s
		status = synup_install(home, spec, .false.)
		if (status == exit_success .and. len(read_default(home)) == 0) then
			! The first install is the default, so that `synup install` is enough
			status = synup_set_default(home, SYNUP_STABLE)
		else if (status == exit_success) then
			status = relink(home)
		end if

	case ('default')
		if (argv%len_ < 2) then
			status = fail('`synup default` needs a version')
		else
			status = synup_set_default(home, argv%v(2)%s)
		end if

	case ('update')
		status = synup_update(home)

	case ('list')
		status = synup_list(home, argv%len_ >= 2 .and. any_arg(argv, '--available'))

	case ('remove')
		if (argv%len_ < 2) then
			status = fail('`synup remove` needs a version')
		else
			status = synup_remove(home, argv%v(2)%s)
		end if

	case ('run')
		if (argv%len_ < 2) then
			status = fail('`synup run` needs a version')
		else
			rest = new_string_vector()
			do i = 3, argv%len_
				if (i == 3 .and. argv%v(i)%s == '--') cycle
				call rest%push(argv%v(i)%s)
			end do
			status = synup_run(home, argv%v(2)%s, rest)
		end if

	case default
		write(*,'(a)') err_prefix//'unknown command `'//cmd//'`'//color_reset
		write(*,'(a)')
		call synup_help()
		status = exit_failure
	end select

end function synup_cli

!===============================================================================

function read_default(home) result(r)
	! The text of `default`, or empty if it's not there yet
	character(len = *), intent(in) :: home
	character(len = :), allocatable :: r
	integer :: io
	r = read_file(home//'/default', io)
	if (io /= 0) r = ''
end function read_default

!===============================================================================

function any_arg(argv, s) result(r)
	type(string_vector_t), intent(in) :: argv
	character(len = *), intent(in) :: s
	logical :: r
	integer :: i
	r = .false.
	do i = 2, argv%len_
		if (argv%v(i)%s == s) r = .true.
	end do
end function any_arg

!===============================================================================

end module syntran__synup_m

!===============================================================================
