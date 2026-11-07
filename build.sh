#!/bin/bash

set -eu

usage() {
	cat <<'EOF'
Usage: build.sh [debug|release] [options] [-- extra args]

Builds syntran with cmake, or with fpm if --fpm is given.

  debug, release       Build type (default: debug).  --debug/--release also work
  --fpm                Build with fpm instead of cmake.  The binary is installed
                       to build/fpm/<Debug|Release>/bin/
  --llvm, --no-llvm    Build the native backend (`syntran --native`), which needs
                       LLVM 17+.  Default: on if llvm-config is found, on the
                       PATH or in the usual install dirs.  $LLVM_CONFIG names one
  --blas, --no-blas    Link BLAS/LAPACK for `@`, std::solve(), etc., or use the
                       portable Fortran fallbacks.  Default: on if the libraries
                       are found and link.  SYNTRAN_NO_BLAS=1 means --no-blas
  --blas-dir DIR       Look for BLAS/LAPACK in DIR instead of the usual places
  --test               Run the short tests after building
  -h, --help           Show this help

Anything after `--` is passed on to the cmake configure step, or to `fpm install`
EOF
}

die() {
	echo "Error: $*" >&2
	exit 1
}

# Echo a command, then run it
run() {
	printf '+' >&2
	printf ' %q' "$@" >&2
	echo >&2
	"$@"
}

config=Debug
use_fpm=
llvm=auto
blas=auto
blas_dir_arg=
run_tests=
passthru=()

# I used to use ${1,,} for lowercase conversion, but that's bash 4 which isn't
# available on poor little macos :(
while [[ $# -gt 0 ]]; do
	case "$1" in
		release|Release|--release) config=Release;;
		debug|Debug|--debug)       config=Debug;;
		--fpm)                     use_fpm=1;;
		--llvm)                    llvm=on;;
		--no-llvm)                 llvm=off;;
		--blas)                    blas=on;;
		--no-blas)                 blas=off;;
		--blas-dir)
			[[ $# -gt 1 ]] || die "--blas-dir needs a directory"
			blas_dir_arg="$2"
			shift
			;;
		--blas-dir=*)              blas_dir_arg="${1#--blas-dir=}";;
		--test)                    run_tests=1;;
		-h|--help)                 usage; exit 0;;
		--)
			shift
			passthru=("$@")
			break
			;;
		*) echo "Error: unrecognized cmd arg \"$1\"" >&2; usage >&2; exit 1;;
	esac
	shift
done

if [[ "$blas" == auto && -n "${SYNTRAN_NO_BLAS:-}" ]]; then
	blas=off
fi

# Lowercase, for fpm's profiles
config_lc=debug
[[ "$config" == Release ]] && config_lc=release

## Uncomment to tell cmake to use ifort instead of gfortran
#export SYNTRAN_INTEL=true

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
echo "machine = ${machine}"

generator=()
if [[ "$machine" == "MinGw" ]]; then
	generator=(-G 'MSYS Makefiles')
	if [[ -z "${SYNTRAN_INTEL:-}" ]]; then
		export FC=gfortran
	fi
	echo "FC = $FC"
fi

if [[ "$machine" == "Linux" ]]; then

	## ninja works, similar performance as gnu make
	#generator=(-G 'Ninja')

	if [[ -z "${SYNTRAN_INTEL:-}" ]]; then
		export FC=$(which gfortran-14)
	elif [[ -z "${FC:-}" ]]; then
		# Caller wants Intel but hasn't already exported FC (e.g. via
		# setup-fortran in CI) -- default to ifx.
		export FC=ifx
	fi
	echo "FC = $FC"
fi

if [[ "$machine" == "Mac" ]]; then

	# If you don't have it:  brew install gcc@14
	#
	# The gfortran included with gcc 13.2.0 seems to have a bug on macos.  Might
	# be worth trying a later 13 version when it's available

	#which gcc
	#which gfortran
	which gfortran-14

	## CMake doesn't like this hack, renamed gfortran won't pass its tests
	#mkdir ~/bin/
	#cp $(which gfortran-14) /usr/local/bin/gfortran
	#export PATH=$PATH:~/bin/
	#which gfortran

	export FC=$(which gfortran-14)
	echo "FC = $FC"

fi

#===============================================================================
# LLVM, for the native backend (`syntran --native`, see doc/native.md)

# Print the major version of llvm-config $1, or nothing if it doesn't run
llvm_major() {
	local v
	v="$("$1" --version 2>/dev/null | cut -d. -f1)"
	[[ "$v" =~ ^[0-9]+$ ]] && echo "$v"
	return 0
}

# Print the path of an llvm-config that is version 17 or later.  $LLVM_CONFIG
# and then the PATH win.  Otherwise look in the places that a package manager
# puts it when it's not on the PATH (Homebrew's llvm is keg-only, MSYS2's is
# outside of Git Bash's PATH, and Debian versions its directories), and take
# the newest
find_llvm_config() {
	local c v best= best_v=0 cands=()
	for c in "${LLVM_CONFIG:-}" "$(command -v llvm-config || true)"; do
		[[ -n "$c" && -x "$c" ]] || continue
		v="$(llvm_major "$c")"
		if [[ -n "$v" ]] && (( v >= 17 )); then
			echo "$c"
			return 0
		fi
	done

	case "$machine" in
		Mac)
			cands=(/opt/homebrew/opt/llvm/bin/llvm-config /usr/local/opt/llvm/bin/llvm-config
				/opt/homebrew/opt/llvm@*/bin/llvm-config /usr/local/opt/llvm@*/bin/llvm-config)
			;;
		Linux)
			cands=(/usr/lib/llvm-*/bin/llvm-config /usr/lib64/llvm*/bin/llvm-config
				/usr/bin/llvm-config-* /usr/local/bin/llvm-config-*)
			;;
		MinGw)
			cands=(/c/msys64/ucrt64/bin/llvm-config.exe /ucrt64/bin/llvm-config.exe)
			;;
	esac
	for c in ${cands[@]+"${cands[@]}"}; do
		[[ -x "$c" ]] || continue
		v="$(llvm_major "$c")"
		if [[ -n "$v" ]] && (( v >= 17 && v > best_v )); then
			best="$c"
			best_v="$v"
		fi
	done
	[[ -n "$best" ]] || return 1
	echo "$best"
}

# llvm-config prints C:/... paths on Windows, and bash wants /c/...
llvm_path() {
	if [[ "$machine" == MinGw ]]; then
		cygpath -u "$("$llvm_config" "$1")"
	else
		"$llvm_config" "$1"
	fi
}

llvm_config=
if [[ "$llvm" != off ]]; then
	llvm_config="$(find_llvm_config || true)"
	if [[ -n "$llvm_config" ]]; then
		llvm=on
	elif [[ "$llvm" == on ]]; then
		case "$machine" in
			Mac)   hint="brew install llvm";;
			Linux) hint="sudo apt install llvm-dev";;
			MinGw) hint="pacman -S mingw-w64-ucrt-x86_64-llvm, in MSYS2";;
			*)     hint="see doc/native.md";;
		esac
		die "--llvm needs llvm-config from LLVM 17 or later.  Try \`$hint\`, or set LLVM_CONFIG to the path of llvm-config"
	else
		llvm=off
		llvm_desc="off (llvm-config 17+ not found)"
	fi
fi
llvm_bindir=
if [[ "$llvm" == on ]]; then
	llvm_desc="on ($llvm_config, $("$llvm_config" --version))"
	# libLLVM-*.dll is loaded when syntran starts, so its dir must be on the PATH
	[[ "$machine" == MinGw ]] && llvm_bindir="$(llvm_path --bindir)"
elif [[ "$llvm" == off && -z "${llvm_desc:-}" ]]; then
	llvm_desc="off (--no-llvm)"
fi

# On Windows syntran loads libLLVM-*.dll when it starts, and Windows looks for
# DLLs next to the exe before the PATH.  So put libLLVM and the DLLs that it needs
# (libstdc++, zlib, etc.) next to the binaries in the dirs "$@", so that they run
# without MSYS2 on the PATH.  They're hard links where possible, because libLLVM
# is ~150 MB
copy_llvm_dlls() {
	local queue seen=" " f d dir
	command -v objdump >/dev/null 2>&1 || {
		echo "Warning: no objdump, so not copying libLLVM-*.dll next to the binaries." \
			"Put $llvm_bindir on your PATH to run them" >&2
		return 0
	}

	# The closure of the DLLs that libLLVM imports and that MSYS2 provides.  The
	# rest are Windows system DLLs
	queue="$(cd "$llvm_bindir" && ls libLLVM-*.dll 2>/dev/null | tail -n 1)"
	[[ -n "$queue" ]] || die "no libLLVM-*.dll in $llvm_bindir"
	while [[ -n "$queue" ]]; do
		f="${queue%% *}"
		[[ "$queue" == *" "* ]] && queue="${queue#* }" || queue=
		[[ "$seen" == *" $f "* || ! -e "$llvm_bindir/$f" ]] && continue
		seen="$seen$f "
		for d in $(objdump -p "$llvm_bindir/$f" | awk '/DLL Name/ {print $3}'); do
			queue="${queue:+$queue }$d"
		done
	done

	for dir in "$@"; do
		mkdir -p "$dir"
		for f in $seen; do
			[[ "$llvm_bindir/$f" -ef "$dir/$f" ]] && continue
			cmp -s "$llvm_bindir/$f" "$dir/$f" && continue
			rm -f "$dir/$f"
			ln "$llvm_bindir/$f" "$dir/$f" 2>/dev/null || cp -f "$llvm_bindir/$f" "$dir/$f"
		done
	done
}

#===============================================================================
# BLAS/LAPACK for `@`, std::solve(), and std::lu_factor()/std::lu_solve()
#
# Looks for the libraries in the usual places, creates a shim dir if they're only
# available as libopenblas, and then checks that a program can really link them
# the same way that fpm.toml and CMakeLists.txt do (-llapack -lblas)

blas_on=0
blas_desc=
blas_L=              # dir for -L (fpm) or CMAKE_LIBRARY_PATH (cmake), if needed

# Is there a lib<name> in dir $1?  $2 is the name
has_lib() {
	local e
	for e in so dylib a dll.a; do
		[[ -e "$1/lib$2.$e" ]] && return 0
	done
	return 1
}

# Print the first existing lib<name> in dir $1, preferring a static library on
# Windows where we link with -static
first_lib() {
	local e exts="so dylib a dll.a"
	[[ "$machine" == MinGw ]] && exts="a dll.a so dylib"
	for e in $exts; do
		if [[ -e "$1/lib$2.$e" ]]; then
			echo "$1/lib$2.$e"
			return 0
		fi
	done
	return 1
}

# Does dir $1 have OpenBLAS, or both reference BLAS and LAPACK?
dir_has_blas() {
	has_lib "$1" openblas || { has_lib "$1" blas && has_lib "$1" lapack; }
}

# Print the dirs to look in, in order of preference
blas_candidates() {
	local d
	case "$machine" in
		Mac)
			echo /opt/homebrew/opt/openblas/lib
			echo /usr/local/opt/openblas/lib
			;;
		Linux)
			if command -v pkg-config >/dev/null 2>&1; then
				pkg-config --variable=libdir openblas 2>/dev/null || true
			fi
			for d in \
				/usr/lib/x86_64-linux-gnu/openblas-pthread /usr/lib/x86_64-linux-gnu \
				/usr/lib/aarch64-linux-gnu/openblas-pthread /usr/lib/aarch64-linux-gnu \
				/usr/lib64/openblas /usr/lib64 /usr/lib /usr/local/lib \
				/opt/OpenBLAS/lib
			do
				echo "$d"
			done
			;;
		MinGw)
			# The real install goes first, so that the copies in build/openblas
			# get refreshed from it.  They're only a fallback (e.g. in CI)
			echo /c/msys64/ucrt64/lib
			echo /ucrt64/lib
			echo build/openblas
			;;
	esac
}

# Try to link a tiny program against BLAS and LAPACK with the link flags in "$@".
# Leaves the linker output in $probe_log
probe_log=
probe_blas() {
	local d rc=0 extra=()
	# The same link flags as the builds below.  Without them OpenBLAS's static
	# library has an undefined reference to omp_get_max_threads()
	[[ "$machine" == MinGw ]] && extra=(-static -fopenmp)
	d="$(mktemp -d)"
	cat > "$d/probe.f90" <<'EOF'
program probe
	implicit none
	external :: dgemm, dgesv
	double precision :: a(1,1), b(1,1), c(1,1)
	integer :: ipiv(1), info
	a = 1.d0
	b = 1.d0
	c = 0.d0
	call dgemm('N', 'N', 1, 1, 1, 1.d0, a, 1, b, 1, 0.d0, c, 1)
	call dgesv(1, 1, a, 1, ipiv, b, 1, info)
	print *, c, info
end program probe
EOF
	"$FC" -o "$d/probe" "$d/probe.f90" ${extra[@]+"${extra[@]}"} "$@" -llapack -lblas > "$d/log" 2>&1 || rc=$?
	probe_log="$(tail -n 5 "$d/log")"
	rm -rf "$d"
	return $rc
}

# Make build/openblas/lib{blas,lapack} from the libopenblas in dir $1, since
# fpm.toml links -llapack -lblas but brew, MSYS2 and /opt/OpenBLAS only ship
# libopenblas.  Windows has to copy because it can't symlink.  Sets blas_L
make_blas_shim() {
	local src ext shim=build/openblas
	src="$(first_lib "$1" openblas)"
	ext="${src##*/libopenblas.}"
	mkdir -p "$shim"
	if [[ "$machine" == MinGw ]]; then
		# Only if changed, since cmake relinks when a library's timestamp does
		cmp -s "$src" "$shim/libblas.$ext" || cp -f "$src" "$shim/libblas.$ext"
		cmp -s "$src" "$shim/liblapack.$ext" || cp -f "$src" "$shim/liblapack.$ext"
	else
		ln -sf "$src" "$shim/libblas.$ext"
		ln -sf "$src" "$shim/liblapack.$ext"
	fi
	blas_L="$shim"
}

detect_blas() {
	local d found= flags
	flags=()

	if [[ "$blas" == off ]]; then
		blas_desc="off (--no-blas)"
		return
	fi
	if [[ -n "$blas_dir_arg" ]]; then
		dir_has_blas "$blas_dir_arg" || die "no libopenblas, or libblas and liblapack, in $blas_dir_arg"
		found="$blas_dir_arg"
	elif [[ "$machine" == Mac && -d /System/Library/Frameworks/Accelerate.framework ]]; then
		# `-llapack -lblas` resolves to Accelerate with no -L
		probe_blas && { blas_on=1; blas_desc="on (Accelerate)"; return; }
	else
		for d in $(blas_candidates); do
			if dir_has_blas "$d"; then
				found="$d"
				break
			fi
		done
	fi

	# Only link as -llapack -lblas if the dir has those names, otherwise shim them
	if [[ -n "$found" ]]; then
		if ! has_lib "$found" openblas; then
			echo "Note: reference BLAS in $found is much slower than OpenBLAS.  See README.md" >&2
		fi
		if has_lib "$found" blas && has_lib "$found" lapack; then
			[[ -n "$blas_dir_arg" || "$found" == build/openblas ]] && blas_L="$found"
		else
			make_blas_shim "$found"
			[[ "$machine" == Linux ]] && flags+=(-Wl,-rpath,"$found")
		fi
	fi

	# First with nothing but the default search path, unless the dir is needed
	if [[ -z "$blas_L" ]] && probe_blas; then
		blas_on=1
		blas_desc="on (default library path)"
		[[ -n "$found" ]] && blas_desc="on ($found)"
		return
	elif [[ -n "$blas_L" ]] && probe_blas "-L$blas_L" ${flags[@]+"${flags[@]}"}; then
		blas_on=1
		blas_desc="on ($found)"
		[[ "$found" != "$blas_L" ]] && blas_desc="on ($found, via $blas_L)"
		blas_rpath=(${flags[@]+"${flags[@]}"})
		return
	fi

	# Found a dir but the default path didn't work: try it explicitly.  Never put
	# MSYS2's own lib dir on -L, because its libgfortran.a would shadow ours
	if [[ "$machine" != MinGw && -z "$blas_L" && -n "$found" ]] && probe_blas "-L$found"; then
		blas_on=1
		blas_L="$found"
		blas_desc="on ($found)"
		return
	fi

	if [[ "$blas" == on ]]; then
		# Forced.  Let the build fail loudly if the libraries are really missing
		blas_on=1
		blas_desc="on (forced by --blas, but a test link failed)"
		return
	fi

	blas_L=
	blas_rpath=()
	if [[ -n "$found" ]]; then
		echo "Warning: found BLAS/LAPACK in $found but a test program wouldn't link:" >&2
		echo "$probe_log" | sed 's/^/    /' >&2
		blas_desc="off (found $found, but it doesn't link.  Using the slower portable fallbacks)"
	else
		case "$machine" in
			Mac)   hint="brew install openblas";;
			Linux) hint="sudo apt-get install libopenblas-dev";;
			*)     hint="see the BLAS section of README.md";;
		esac
		echo "Warning: no BLAS/LAPACK found.  Using the slower portable fallbacks.  Try: $hint" >&2
		blas_desc="off (not found)"
	fi
}

blas_rpath=()
detect_blas

#===============================================================================

tool=cmake
[[ -n "$use_fpm" ]] && tool=fpm
echo "build: tool=$tool config=$config blas=$blas_desc llvm=$llvm_desc"

if [[ -z "$use_fpm" ]]; then

	build="build/$config"

	# Always spell out the options.  Otherwise a value cached by an earlier
	# `build.sh` in the same build dir would stick
	extra=()
	if [[ "$blas_on" == 1 ]]; then
		extra+=(-DSYNTRAN_NO_BLAS=OFF)
		[[ -n "$blas_L" ]] && extra+=(-DCMAKE_LIBRARY_PATH="$PWD/$blas_L")
		if [[ "$machine" == MinGw ]]; then
			# FindBLAS tests its candidates with a link, which fails without
			# OpenMP, and it would otherwise take libopenblas.dll.a from the
			# MSYS2 prefix that LLVM puts on CMAKE_PREFIX_PATH, over our static
			# libblas.a and liblapack.a
			extra+=(-DBLA_VENDOR=Generic "-DCMAKE_REQUIRED_LINK_OPTIONS=-fopenmp;-static")
		fi
	else
		extra+=(-DSYNTRAN_NO_BLAS=ON)
	fi
	if [[ "$llvm" == on ]]; then
		extra+=(-DSYNTRAN_LLVM=ON -DCMAKE_PREFIX_PATH="$("$llvm_config" --prefix)")
	else
		extra+=(-DSYNTRAN_LLVM=OFF)
	fi

	# The `${extra[@]+...}` form is for bash 3.2 on macOS, where expanding an
	# empty array under `set -u` is an "unbound variable" error
	run cmake -S . -B "$build" ${generator[@]+"${generator[@]}"} ${extra[@]+"${extra[@]}"} \
		-DCMAKE_BUILD_TYPE=$config -DCMAKE_Fortran_COMPILER=$FC \
		${passthru[@]+"${passthru[@]}"}
	run cmake --build "$build" --config $config --parallel

	[[ -n "$llvm_bindir" ]] && copy_llvm_dlls "$build"

	if [[ -n "$run_tests" ]]; then
		run "./$build/test"
	fi

else

	# Features take the place of --profile, and only one feature can link
	# libraries, so BLAS + LLVM is its own feature.  See fpm.toml
	features=()
	if [[ "$blas_on" == 1 && "$llvm" == on ]]; then
		features=(--features "$config_lc,llvm-blas")
	elif [[ "$llvm" == on ]]; then
		features=(--features "$config_lc,noblas,llvm")
	elif [[ "$blas_on" == 1 ]]; then
		features=(--profile "$config_lc")
	else
		features=(--features "$config_lc,noblas")
	fi

	# Same warning flags as cmake and utils/build.rsp
	flags="-Wno-tabs"
	[[ "$config" == Debug ]] && flags="$flags -DSYNTRAN_BOUNDS_CHECK"
	[[ -n "$blas_L" ]] && flags="$flags -L$blas_L"
	[[ ${#blas_rpath[@]} -gt 0 ]] && flags="$flags ${blas_rpath[*]}"
	if [[ "$machine" == MinGw ]]; then
		# See CLAUDE.md for why these are required on Windows
		flags="$flags -static -fopenmp -Wl,--wrap=setlocale -Wl,--stack,67108864"
	fi

	if [[ "$llvm" == on ]]; then
		if [[ "$machine" == MinGw ]]; then
			# Link the import library through a private dir, because the MSYS2
			# lib dir itself on the path would shadow our libgfortran.a.  Copy
			# it only if changed, and since MSYS2 bumps LLVM's version, glob for
			# its name
			imp="$(ls "$(llvm_path --libdir)"/libLLVM-*.dll.a 2>/dev/null | tail -n 1)"
			[[ -n "$imp" ]] || die "no libLLVM-*.dll.a import library in $(llvm_path --libdir)"
			mkdir -p build/llvm-win
			cmp -s "$imp" build/llvm-win/libLLVM.a || cp -f "$imp" build/llvm-win/libLLVM.a
			flags="$flags -Lbuild/llvm-win"
		else
			# fpm can't find the LLVM libraries by itself
			export LIBRARY_PATH="$("$llvm_config" --libdir)${LIBRARY_PATH:+:$LIBRARY_PATH}"
		fi
	fi
	export FPM_FC="$FC"

	# `fpm install` instead of `fpm build` gives a stable path to the binaries,
	# instead of a build/gfortran_<hash>/ directory that changes with the flags
	prefix="build/fpm/$config"
	run fpm install --test "${features[@]}" --flag "$flags" --prefix "$prefix" \
		${passthru[@]+"${passthru[@]}"}

	[[ -n "$llvm_bindir" ]] && copy_llvm_dlls "$prefix/bin" "$prefix/test"

	if [[ -n "$run_tests" ]]; then
		run "./$prefix/test/test"
	fi

fi
